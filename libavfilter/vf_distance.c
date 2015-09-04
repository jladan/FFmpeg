/*
 * Copyright (c) 2015 John Ladan
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * Compute the distance from a reference colour for all pixels in YUV colourspace
 */

#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "avfilter.h"
#include "formats.h"
#include "internal.h"
#include "video.h"

typedef struct DistanceContext {
    const AVClass *class;
    uint8_t yuv[4];         /* reference colour */
    double factor;          /* parameters for distance function */
    double gamma;
} DistanceContext;

typedef struct ThreadData {
    AVFrame *dst;
    const AVFrame *src;
} ThreadData;

#define OFFSET(x) offsetof(DistanceContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM

static const AVOption distance_options[] = {
    { "yuv",  "The reference YUV value coded in hex: YYUUVV",            OFFSET(yuv),  AV_OPT_TYPE_COLOR, { .str = "black" }, .flags = FLAGS },
    { "factor", "the multiplicative factor (brightness)", OFFSET(factor), AV_OPT_TYPE_DOUBLE, { .dbl = 1.0 }, 0.01, 10.0, FLAGS }, /* The max and min are basically arbitrary */
    { "gamma", "gamma factor to be applied (for contrast)", OFFSET(gamma), AV_OPT_TYPE_DOUBLE, { .dbl = 1.0 }, 0.0, 5.0, FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(distance);

// XXX Only 8bit formats for now
#define YUV_FORMATS                                         \
    AV_PIX_FMT_YUV444P,  AV_PIX_FMT_YUV422P,  AV_PIX_FMT_YUV420P,    \
    AV_PIX_FMT_YUVA420P, AV_PIX_FMT_YUVA422P, AV_PIX_FMT_YUVA444P,   \
    /* Note that the yuvj formats are deprecated internally */       \
    AV_PIX_FMT_YUVJ444P, AV_PIX_FMT_YUVJ422P, AV_PIX_FMT_YUVJ420P,   \
    AV_PIX_FMT_UYVY422

/* unsupported formats (as of yet)
AV_PIX_FMT_YUV410P,   ///< planar YUV 4:1:0,  9bpp, (1 Cr & Cb sample per 4x4 Y samples)
AV_PIX_FMT_YUV411P,   ///< planar YUV 4:1:1, 12bpp, (1 Cr & Cb sample per 4x1 Y samples)
AV_PIX_FMT_UYYVYY411, ///< packed YUV 4:1:1, 12bpp, Cb Y0 Y1 Cr Y2 Y3
AV_PIX_FMT_YUV440P,   ///< planar YUV 4:4:0 (1 Cr & Cb sample per 1x2 Y samples)
AV_PIX_FMT_YUVJ440P,  ///< planar YUV 4:4:0 full scale (JPEG), deprecated in favor of AV_PIX_FMT_YUV440P and setting color_range
AV_PIX_FMT_YVYU422,   ///< packed YUV 4:2:2, 16bpp, Y0 Cr Y1 Cb

(May work with an adjustment to process_slices_uyuv422
AV_PIX_FMT_YUYV422,   ///< packed YUV 4:2:2, 16bpp, Y0 Cb Y1 Cr
*/


static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat yuv_pixfmts[] = { YUV_FORMATS, AV_PIX_FMT_NONE };
    static const enum AVPixelFormat out8_pixfmts[] = { AV_PIX_FMT_GRAY8, AV_PIX_FMT_NONE };

    // No matter what, we only take in yuv, and only output greyscale
    ff_formats_ref(ff_make_format_list(yuv_pixfmts), &ctx->inputs[0]->out_formats);
    ff_formats_ref(ff_make_format_list(out8_pixfmts), &ctx->outputs[0]->in_formats);


    return 0;
}

static int config_props(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    DistanceContext *s = ctx->priv;
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);

    switch (inlink->format) {
    case AV_PIX_FMT_YUV444P:
    case AV_PIX_FMT_YUVA444P:
    case AV_PIX_FMT_YUVJ444P:
        break;
    case AV_PIX_FMT_YUV422P:
    case AV_PIX_FMT_YUVA422P:
    case AV_PIX_FMT_YUVJ422P:
        break;
    case AV_PIX_FMT_YUV420P:
    case AV_PIX_FMT_YUVA420P:
    case AV_PIX_FMT_YUVJ420P:
        break;
    case AV_PIX_FMT_UYVY422:
        break;
    default:
        return AVERROR(ENOMEM); // TODO find better error
    }

    return 0;
}

#define DISTANCE sqrt(y*y + u*u + v*v) / 255.0
#define CONVERT(diff) av_clipd(s->factor * pow(diff, s->gamma ), 0.0, 1.0) * 255.0

static int process_slice_uyvy422(AVFilterContext *ctx, void *arg, int jobnr, int nb_jobs)
{
    DistanceContext *s = ctx->priv;
    const ThreadData *td = arg;
    const AVFrame *src = td->src;
    AVFrame *dst = td->dst;
    const int height = src->height;
    const int width = src->width*2;
    const int src_pitch = src->linesize[0];
    const int dst_pitch = dst->linesize[0];
    const int slice_start = (height *  jobnr   ) / nb_jobs;
    const int slice_end   = (height * (jobnr+1)) / nb_jobs;
    const unsigned char *srcp = src->data[0] + slice_start * src_pitch;
    unsigned char *dstp = dst->data[0] + slice_start * dst_pitch;
    int x, y, dx;

    for (y = slice_start; y < slice_end; y++) {
        for (x = dx = 0; x < width; x += 4, dx += 2) {
            const int u = srcp[x + 0] - s->yuv[1];
            const int v = srcp[x + 2] - s->yuv[2];
            int y = srcp[x + 1] - s->yuv[0];
            dstp[dx + 0] = CONVERT(DISTANCE);
            y = srcp[x + 3] - s->yuv[0];
            dstp[dx + 1] = CONVERT(DISTANCE);
        }
        srcp += src_pitch;
        dstp += dst_pitch;
    }

    return 0;
}

static int process_slice_yuv444p(AVFilterContext *ctx, void *arg, int jobnr, int nb_jobs)
{
    DistanceContext *s = ctx->priv;
    const ThreadData *td = arg;
    const AVFrame *src = td->src;
    AVFrame *dst = td->dst;
    const int height = src->height;
    const int width = src->width;
    const int slice_start = (height *  jobnr   ) / nb_jobs;
    const int slice_end   = (height * (jobnr+1)) / nb_jobs;
    const int src_pitchY  = src->linesize[0];
    const int src_pitchUV = src->linesize[1];
    const unsigned char *srcpU = src->data[1] + slice_start * src_pitchUV;
    const unsigned char *srcpV = src->data[2] + slice_start * src_pitchUV;
    const unsigned char *srcpY = src->data[0] + slice_start * src_pitchY;
    const int dst_pitch  = dst->linesize[0];
    unsigned char *dstp = dst->data[0] + slice_start * dst_pitch;
    int x, y;

    for (y = slice_start; y < slice_end; y++) {
        for (x = 0; x < width; x++) {
            const int y = srcpY[x] - s->yuv[0];
            const int u = srcpU[x] - s->yuv[1];
            const int v = srcpV[x] - s->yuv[2];
            dstp[x] = CONVERT(DISTANCE);
        }
        srcpY += src_pitchY;
        srcpU += src_pitchUV;
        srcpV += src_pitchUV;
        dstp += dst_pitch;
    }

    return 0;
}

static int process_slice_yuv422p(AVFilterContext *ctx, void *arg, int jobnr, int nb_jobs)
{
    DistanceContext *s = ctx->priv;
    const ThreadData *td = arg;
    const AVFrame *src = td->src;
    AVFrame *dst = td->dst;
    const int height = src->height;
    const int width = src->width;
    const int slice_start = (height *  jobnr   ) / nb_jobs;
    const int slice_end   = (height * (jobnr+1)) / nb_jobs;
    const int src_pitchY  = src->linesize[0];
    const int src_pitchUV = src->linesize[1];
    const unsigned char *srcpU = src->data[1] + slice_start * src_pitchUV;
    const unsigned char *srcpV = src->data[2] + slice_start * src_pitchUV;
    const unsigned char *srcpY = src->data[0] + slice_start * src_pitchY;
    const int dst_pitch  = dst->linesize[0];
    unsigned char *dstp = dst->data[0] + slice_start * dst_pitch;
    int x, y;

    for (y = slice_start; y < slice_end; y++) {
        for (x = 0; x < width; x += 2) {
            int y = srcpY[x] - s->yuv[0];
            const int u = srcpU[x >> 1] - s->yuv[1];
            const int v = srcpV[x >> 1] - s->yuv[2];
            dstp[x] = CONVERT(DISTANCE);
            y = srcpY[x + 1] - s->yuv[0];
            dstp[x + 1] = CONVERT(DISTANCE);
        }
        srcpY += src_pitchY;
        srcpU += src_pitchUV;
        srcpV += src_pitchUV;
        dstp += dst_pitch;
    }

    return 0;
}

static int process_slice_yuv420p(AVFilterContext *ctx, void *arg, int jobnr, int nb_jobs)
{
    DistanceContext *s = ctx->priv;
    const ThreadData *td = arg;
    const AVFrame *src = td->src;
    AVFrame *dst = td->dst;
    const int height = FFALIGN(src->height, 2) >> 1;
    const int width = src->width;
    const int slice_start = ((height *  jobnr   ) / nb_jobs) << 1;
    const int slice_end   = ((height * (jobnr+1)) / nb_jobs) << 1;
    const int src_pitchY  = src->linesize[0];
    const int src_pitchUV = src->linesize[1];
    const int dst_pitch  = dst->linesize[0];
    const unsigned char *srcpY = src->data[0] + src_pitchY * slice_start;
    const unsigned char *srcpU = src->data[1] + src_pitchUV * (slice_start >> 1);
    const unsigned char *srcpV = src->data[2] + src_pitchUV * (slice_start >> 1);
    unsigned char *dstp = dst->data[0] + dst_pitch * slice_start;
    int x, y;

    for (y = slice_start; y < slice_end; y += 2) {
        for (x = 0; x < width; x += 2) {
            const int u = srcpU[x >> 1] - s->yuv[1];
            const int v = srcpV[x >> 1] - s->yuv[2];
            int y = srcpY[x + 0] - s->yuv[0];
            dstp[x] = CONVERT(DISTANCE);
            y = srcpY[x + 1] - s->yuv[0];
            dstp[x + 1] = CONVERT(DISTANCE);
            y = srcpY[x + src_pitchY] - s->yuv[0];
            dstp[x + dst_pitch] = CONVERT(DISTANCE);
            y = srcpY[x + src_pitchY + 1] - s->yuv[0];
            dstp[x + dst_pitch + 1] = CONVERT(DISTANCE);
        }
        srcpY += src_pitchY << 1;
        dstp += dst_pitch << 1;
        srcpU += src_pitchUV;
        srcpV += src_pitchUV;
    }

    return 0;
}

static int filter_frame(AVFilterLink *link, AVFrame *in)
{
    AVFilterContext *ctx = link->dst;
    AVFilterLink *outlink = ctx->outputs[0];
    AVFrame *out;
    ThreadData td = {0};

    out = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    if (!out) {
        av_frame_free(&in);
        return AVERROR(ENOMEM);
    }
    // This is kosher according to vf_extractplanes
    av_frame_copy_props(out, in);

    td.src = in;
    td.dst = out;

    switch (in->format) {
    case AV_PIX_FMT_YUV444P:
    case AV_PIX_FMT_YUVA444P:
    case AV_PIX_FMT_YUVJ444P:
        ctx->internal->execute(ctx, process_slice_yuv444p, &td, NULL,
            FFMIN(in->height, ctx->graph->nb_threads));
        break;
    case AV_PIX_FMT_YUV422P:
    case AV_PIX_FMT_YUVA422P:
    case AV_PIX_FMT_YUVJ422P:
        ctx->internal->execute(ctx, process_slice_yuv422p, &td, NULL,
            FFMIN(in->height, ctx->graph->nb_threads));
        break;
    case AV_PIX_FMT_YUV420P:
    case AV_PIX_FMT_YUVA420P:
    case AV_PIX_FMT_YUVJ420P:
        ctx->internal->execute(ctx, process_slice_yuv420p, &td, NULL,
            FFMIN(in->height / 2, ctx->graph->nb_threads));
        break;
    case AV_PIX_FMT_UYVY422:
        ctx->internal->execute(ctx, process_slice_uyvy422, &td, NULL,
            FFMIN(in->height, ctx->graph->nb_threads));
        break;
    default:
        return AVERROR(ENOMEM); // TODO find better error
    }

    av_frame_free(&in);
    return ff_filter_frame(outlink, out);
}

static const AVFilterPad inputs[] = {
    { .name         = "default",
      .type         = AVMEDIA_TYPE_VIDEO,
      .filter_frame = filter_frame,
      .config_props = config_props,
    },
    { NULL }
};
static const AVFilterPad outputs[] = {
    { .name = "default",
      .type = AVMEDIA_TYPE_VIDEO,
    },
    { NULL }
};

AVFilter ff_vf_distance = {
    .name = "distance",
    .description = NULL_IF_CONFIG_SMALL("Outputs the distance from a reference colour in YUV colour space"),
    .priv_size = sizeof(DistanceContext),
    .priv_class = &distance_class,
    .query_formats = query_formats,
    .inputs = inputs,
    .outputs = outputs,
    .flags = AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC | AVFILTER_FLAG_SLICE_THREADS,
};

