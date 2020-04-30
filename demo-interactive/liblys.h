// Copyright (c) 2019-2020. DIKU, University of Copenhagen
// ISC License

#ifndef LIBLYS_HEADER
#define LIBLYS_HEADER

#include "tracer.h"
#include <stdio.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <stdbool.h>
#include <inttypes.h>

void load_obj_data(
    char* obj_path,
    size_t* num_tris, size_t* num_mat_components,
    float** tri_data, uint32_t** tri_mats, float** mat_data);
void free_obj_data(float* tri_data, uint32_t* tri_mats, float* mat_data);

struct lys_context {
    struct futhark_context *fut;
    struct futhark_opaque_state *state;
    SDL_Window *wnd;
    SDL_Surface *wnd_surface;
    SDL_Surface *surface;
    uint32_t width;
    uint32_t height;
    int32_t *data;
    int64_t last_time;
    bool running;
};

#define FUT_CHECK(ctx, x) _fut_check(ctx, x, __FILE__, __LINE__)
static inline void _fut_check(struct futhark_context *ctx, int res,
                              const char *file, int line) {
    if (res != 0) {
        fprintf(stderr, "%s:%d: Futhark error %d: %s\n",
                file, line, res, futhark_context_get_error(ctx));
        exit(EXIT_FAILURE);
    }
}

#endif
