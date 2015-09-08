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

#include "libavutil/log.h"
#include <stdio.h>

enum map_name {
    MAP_BONE,
    MAP_AUTUMN,
    NB_COLOR_MAP
};

typedef struct ColormapContext {
    const AVClass *class;
    int   name;                 ///<map_name
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
    { "zero", "The value of zero in the greyscale image", OFFSET(zero_value), AV_OPT_TYPE_INT, { .i64 = 0 }, INT_MIN, INT_MAX, .flags = FLAGS },
    { "map", "select color map", OFFSET(name), AV_OPT_TYPE_INT, { .i64 = MAP_BONE }, 0, NB_COLOR_MAP - 1, FLAGS, "map_name" },
    { "bone", "bone colour map", 0, AV_OPT_TYPE_CONST, { .i64 = MAP_BONE }, INT_MIN, INT_MAX, FLAGS, "map_name" },
    { "autumn", "autumn colour map", 0, AV_OPT_TYPE_CONST, { .i64 = MAP_AUTUMN }, INT_MIN, INT_MAX, FLAGS, "map_name" },
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

static void linear_gradient(uint8_t vec[256], const double *pts, const int len, const int min, const int zero, const int max)
{
    double x, dx, x0, x1, y0, y1;
    int val, i = 0; 

    // Negative values
    x = -1.0;
    dx = 1.0 / (zero - min);
    for (val = min; val < zero; val++, x += dx) {
        x0 = pts[i + 0]; x1 = pts[i + 2];
        y0 = pts[i + 1]; y1 = pts[i + 3];
        vec[val] = av_clipd((y1 - y0) / (x1 - x0) * (x - x0) + y0, 0.0, 1.0) * 255.0;
        // Using the while loop gives room for left and right values of points
        while (x >= pts[i + 2] && i < len)
            i++;
    }
    // Positive values
    x = 0.0;
    dx = 1.0 / (max - zero);
    printf("Transition point: %d,\n\t%f\t%f\n\t%f\t%f\n", val, pts[i], pts[i+1], pts[i+2], pts[i+3]);
    for (val = zero; val <= max; val++, x += dx) {
        x0 = pts[i + 0]; x1 = pts[i + 2];
        y0 = pts[i + 1]; y1 = pts[i + 3];
        vec[val] = av_clipd((y1 - y0) / (x1 - x0) * (x - x0) + y0, 0.0, 1.0) * 255.0;
        // printf("%f\t%f\t%f\n", y0 * 255, vec[val], y1 * 255);
        // Using the while loop gives room for left and right values of points
        while (x >= pts[i + 2] && i/2 < len - 1) {
            i += 2;
            printf("inner Transition point: %d,\n\t%f\t%f\n\t%f\t%f\n", val, pts[i], pts[i+1], pts[i+2], pts[i+3]);
        }
    }
    
}

static double bone_data[3][8] = {
    {       // red
        0.0,        0.0,
        0.746032,   0.652778,
        1.0,        1.0,
        1.0,        1.0
    }, {    // green
        0.0,        0.0,
        0.365079,   0.319444,
        0.746032,   0.777778,
        1.0,        1.0
    }, {    // blue
        0.0,       0.0,
        0.365079,  0.444444,
        1.0,        1.0,
        1.0,       1.0
    }
};

static double fall_data[3][4] = {
    {       // red
        0.0, 1.0,
        1.0, 1.0
    }, {    // green
        0.0,        0.0,
        1.0,        1.0
    }, {    // blue
        0.0,       0.0,
        1.0,       0.0
    }
};

static void fill_bone_map(uint8_t lut[3][256], uint8_t *rgba_map, int min, int max, int zero) 
{
    const uint8_t R = rgba_map[0];
    const uint8_t G = rgba_map[1];
    const uint8_t B = rgba_map[2];

    linear_gradient(lut[R], bone_data[0], 3, 0, 0, 255);
    linear_gradient(lut[G], bone_data[1], 4, 0, 0, 255);
    linear_gradient(lut[B], bone_data[2], 3, 0, 0, 255);
}

static void fill_autumn_map(uint8_t lut[3][256], uint8_t *rgba_map, int min, int max, int zero) {

    int val;

    const uint8_t R = rgba_map[0];
    const uint8_t G = rgba_map[1];
    const uint8_t B = rgba_map[2];

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

    linear_gradient(lut[R], fall_data[0], 2, 0, 0, 255);
    linear_gradient(lut[G], fall_data[1], 2, 0, 0, 255);
    linear_gradient(lut[B], fall_data[2], 2, 0, 0, 255);
}


static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    ColormapContext *s = ctx->priv;
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(outlink->format);
    uint8_t rgba_map[4]; /* component index -> RGBA color index map */

    ff_fill_rgba_map(rgba_map, outlink->format);
    s->step = av_get_bits_per_pixel(desc) >> 3;

    // TODO allow for a choice of colourmaps
    switch (s->name){
    case MAP_BONE:
        fill_bone_map(s->map, rgba_map, 0, 255, 0);
        break;
    case MAP_AUTUMN:
        fill_autumn_map(s->map, rgba_map, 0, 255, 0);
        break;
    default:
        return AVERROR(ENOMEM);
    }

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

