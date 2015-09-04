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
 * Apply a colormap to a greyscale video/image
 */

#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "avfilter.h"
#include "formats.h"
#include "internal.h"
#include "video.h"
#include "drawutils.h"

#include <stdio.h>

typedef struct ColormapContext {
    const AVClass *class;
    char   *map_name;
    // Only 8bit depth for now
    uint8_t map[3][256];        /* lookup table for colour map */
    int zero_value;             /* zero_value < 0 ? 128 : zero_value */
    int step;
} ColormapContext;

typedef struct ThreadData {
    AVFrame *dst;
    const AVFrame *src;
    const unsigned int *map[3];
} ThreadData;

#define OFFSET(x) offsetof(ColormapContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM

static const AVOption colormap_options[] = {
    { "map", "Name of the colormap", OFFSET(map_name),  AV_OPT_TYPE_STRING, { .str = "bone" }, .flags = FLAGS },
    { "zero", "The value of zero in the greyscale image", OFFSET(zero_value), AV_OPT_TYPE_INT, { .i64 = 0 }, INT_MIN, INT_MAX, .flags = FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(colormap);

// XXX Only 8bit formats for now
#define IN_FORMATS AV_PIX_FMT_GRAY8

#define RGB_FORMATS                             \
    AV_PIX_FMT_ARGB,         AV_PIX_FMT_RGBA,         \
    AV_PIX_FMT_ABGR,         AV_PIX_FMT_BGRA,         \
    AV_PIX_FMT_RGB24,        AV_PIX_FMT_BGR24

static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat in_pixfmts[] = { IN_FORMATS, AV_PIX_FMT_NONE };
    static const enum AVPixelFormat out8_pixfmts[] = { RGB_FORMATS, AV_PIX_FMT_NONE };

    // No matter what, we only take in yuv, and only output greyscale
    ff_formats_ref(ff_make_format_list(in_pixfmts), &ctx->inputs[0]->out_formats);
    ff_formats_ref(ff_make_format_list(out8_pixfmts), &ctx->outputs[0]->in_formats);

    return 0;
}

static void fill_autumn_map(uint8_t lut[3][256], uint8_t *rgba_map, int min, int max, int zero) {

    int val;

    const uint8_t R = rgba_map[0];
    const uint8_t G = rgba_map[1];
    const uint8_t B = rgba_map[2];

    printf("autumn colour scheme, %d, %d, %d, %d, %d\n", R, G, B, min, max);
    for (val = min; val < zero; val++) {
        lut[R][val] = 0;
        lut[G][val] = 0;
        lut[B][val] = 0;
    }
    // autumn goes from 0.0 -> 1.0 in g, but r and b stay at 0 and 1 respectively
    for (val = zero; val <= max; val++) {
        double res = (1.0 * val) / max;
        lut[R][val] = 255;
        lut[G][val] = av_clipd(res, 0.0, 1.0) * 255.0;
        lut[B][val] = 0;
    }
}


static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    ColormapContext *s = ctx->priv;
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(outlink->format);
    uint8_t rgba_map[4]; /* component index -> RGBA color index map */

    ff_fill_rgba_map(rgba_map, outlink->format);
    s->step = av_get_bits_per_pixel(desc) >> 3;

    printf("setting up lookup table\n");
    // TODO allow for a choice of colourmaps
    fill_autumn_map(s->map, rgba_map, 0, 255, 0);

    return 0;
}

static int process_slice(AVFilterContext *ctx, void *arg, int jobnr, int nb_jobs)
{
    ColormapContext *s = ctx->priv;
    const ThreadData *td = arg;
    const AVFrame *src = td->src;
    AVFrame *dst = td->dst;
    const int height = src->height;
    const int width = src->width;
    const int slice_start = (height *  jobnr   ) / nb_jobs;
    const int slice_end   = (height * (jobnr+1)) / nb_jobs;
    const int src_pitch  = src->linesize[0];
    const unsigned char *srcp = src->data[0] + slice_start * src_pitch;
    const int dst_pitch  = dst->linesize[0];
    unsigned char *dstp = dst->data[0] + slice_start * dst_pitch;
    const int step = s->step;

    int x, dx, y;
    for (y = slice_start; y < slice_end; y++, srcp += src_pitch, dstp += dst_pitch) {
        for (x = 0, dx = 0; x < width; x++, dx += step) {
            dstp[dx + 0] = s->map[0][srcp[x]];
            dstp[dx + 1] = s->map[1][srcp[x]];
            dstp[dx + 2] = s->map[2][srcp[x]];
        }
   }

    return 0;
}


static int filter_frame(AVFilterLink *link, AVFrame *in)
{
    AVFilterContext *ctx = link->dst;
    AVFilterLink *outlink = ctx->outputs[0];
    AVFrame *out;
    ThreadData td = {0};
    int ret;

    out = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    if (!out) {
        av_frame_free(&in);
        return AVERROR(ENOMEM);
    }
    // This is kosher according to vf_extractplanes
    av_frame_copy_props(out, in);

    td.src = in;
    td.dst = out;

    if (ret = ctx->internal->execute(ctx, process_slice, &td, NULL, FFMIN(in->height, ctx->graph->nb_threads)))
        return ret;

    av_frame_free(&in);
    return ff_filter_frame(outlink, out);
}

static const AVFilterPad inputs[] = {
    { .name         = "default",
      .type         = AVMEDIA_TYPE_VIDEO,
      .filter_frame = filter_frame,
    },
    { NULL }
};
static const AVFilterPad outputs[] = {
    { .name = "default",
      .type = AVMEDIA_TYPE_VIDEO,
      .config_props = config_output,
    },
    { NULL }
};

AVFilter ff_vf_colormap = {
    .name = "colormap",
    .description = NULL_IF_CONFIG_SMALL("Applies a colourmap to a greyscale video"),
    .priv_size = sizeof(ColormapContext),
    .priv_class = &colormap_class,
    .query_formats = query_formats,
    .inputs = inputs,
    .outputs = outputs,
    .flags = AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC | AVFILTER_FLAG_SLICE_THREADS,
};

