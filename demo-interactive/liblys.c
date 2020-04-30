// Copyright (c) 2019-2020. DIKU, University of Copenhagen
// ISC License

// Convenience framework for writing visualisations with Futhark and
// C/SDL.
//
// Based on initial SDL wrapper code by Jakob Stokholm Bertelsen.

#define _XOPEN_SOURCE
#include "liblys.h"

#include <inttypes.h>
#include <assert.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

#define INITIAL_WIDTH 800
#define INITIAL_HEIGHT 600

#define SDL_ASSERT(x) _sdl_assert(x, __FILE__, __LINE__)
static inline void _sdl_assert(int res, const char *file, int line) {
    if (res == 0) {
        fprintf(stderr, "%s:%d: SDL error %d: %s\n",
                file, line, res, SDL_GetError());
        exit(EXIT_FAILURE);
    }
}

void window_size_updated(struct lys_context *ctx, uint32_t newx, uint32_t newy) {
    // https://stackoverflow.com/a/40122002
    ctx->wnd_surface = SDL_GetWindowSurface(ctx->wnd);
    SDL_ASSERT(ctx->wnd_surface != NULL);

    ctx->width = newx;
    ctx->height = newy;

    struct futhark_opaque_state *new_state;
    FUT_CHECK(ctx->fut, futhark_entry_resize(ctx->fut, &new_state, ctx->height, ctx->width, ctx->state));
    futhark_free_opaque_state(ctx->fut, ctx->state);
    ctx->state = new_state;

    ctx->wnd_surface = SDL_GetWindowSurface(ctx->wnd);
    SDL_ASSERT(ctx->wnd_surface != NULL);

    if (ctx->data != NULL) {
        free(ctx->data);
    }
    ctx->data = malloc(ctx->width * ctx->height * sizeof(uint32_t));
    assert(ctx->data != NULL);

    if (ctx->surface != NULL) {
        SDL_FreeSurface(ctx->surface);
    }
    ctx->surface = SDL_CreateRGBSurfaceFrom(
        ctx->data,
        (int)ctx->width, (int)ctx->height,
        32, (int)((size_t)ctx->width * sizeof(uint32_t)),
        0xFF0000, 0xFF00, 0xFF, 0x00000000);
    SDL_ASSERT(ctx->surface != NULL);
}

void handle_sdl_events(struct lys_context *ctx) {
    SDL_Event event;

    while (SDL_PollEvent(&event) == 1) {
        switch (event.type) {
        case SDL_WINDOWEVENT:
            switch (event.window.event) {
            case SDL_WINDOWEVENT_RESIZED:
            {
                uint32_t newx = (uint32_t)event.window.data1;
                uint32_t newy = (uint32_t)event.window.data2;
                window_size_updated(ctx, newx, newy);
                break;
            }
            }
            break;
        case SDL_QUIT:
            ctx->running = 0;
            break;
        case SDL_KEYDOWN:
        case SDL_KEYUP:
            switch (event.key.keysym.sym) {
            case SDLK_ESCAPE:
                if (event.key.type == SDL_KEYDOWN) {
                    ctx->running = 0;
                }
                break;
            default:
            {
                struct futhark_opaque_state *new_state;
                int e = event.key.type == SDL_KEYDOWN ? 0 : 1;
                FUT_CHECK(ctx->fut, futhark_entry_key(ctx->fut, &new_state,
                                                      e, event.key.keysym.sym, ctx->state));
                futhark_free_opaque_state(ctx->fut, ctx->state);
                ctx->state = new_state;
            }
            }
        }
    }
}

void sdl_loop(struct lys_context *ctx) {
    struct futhark_i32_2d *out_arr;

    while (ctx->running) {
        struct futhark_opaque_state *new_state;
        FUT_CHECK(ctx->fut, futhark_entry_step(ctx->fut, &new_state, ctx->state));
        futhark_free_opaque_state(ctx->fut, ctx->state);
        ctx->state = new_state;

        FUT_CHECK(ctx->fut, futhark_entry_render(ctx->fut, &out_arr, ctx->state));
        FUT_CHECK(ctx->fut, futhark_values_i32_2d(ctx->fut, out_arr, ctx->data));
        FUT_CHECK(ctx->fut, futhark_free_i32_2d(ctx->fut, out_arr));

        SDL_ASSERT(SDL_BlitSurface(ctx->surface, NULL, ctx->wnd_surface, NULL)==0);

        SDL_ASSERT(SDL_UpdateWindowSurface(ctx->wnd) == 0);

        handle_sdl_events(ctx);
    }
}

void do_sdl(
    struct lys_context *ctx,
    struct futhark_f32_3d* triangle_data,
    struct futhark_u32_1d* triangle_mats,
    struct futhark_f32_2d* mat_data)
{
    struct futhark_context *fut = ctx->fut;

    float cam_pitch = 0.0;
    float cam_yaw = 0.0;
    float cam_origin_[3] = { 0.0f, 0.8f, 1.8f };
    uint32_t cam_conf_id = 0;
    struct futhark_f32_1d* cam_origin =
        futhark_new_f32_1d(ctx->fut, cam_origin_, 3);
    futhark_entry_init(
        fut, &ctx->state, 0,
        ctx->height, ctx->width,
        cam_conf_id,
        triangle_data, triangle_mats, mat_data,
        cam_pitch, cam_yaw, cam_origin);

    ctx->wnd =
        SDL_CreateWindow("Gotta go fast! -- PROPERTTY OF VOLOVO VCARS DONT COPY THAT FLOPPY",
                         SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                         (int)ctx->width, (int)ctx->height, SDL_WINDOW_RESIZABLE);
    SDL_ASSERT(ctx->wnd != NULL);

    window_size_updated(ctx, ctx->width, ctx->height);

    ctx->running = 1;

    sdl_loop(ctx);
    FUT_CHECK(fut, futhark_free_opaque_state(fut, ctx->state));

    free(ctx->data);
    SDL_FreeSurface(ctx->surface);
    // do not free wnd_surface (see SDL_GetWindowSurface)
    SDL_DestroyWindow(ctx->wnd);
    SDL_Quit();
}

void create_futhark_context(const char *deviceopt,
                            int interactive,
                            struct futhark_context_config **cfg,
                            struct futhark_context **ctx) {
    *cfg = futhark_context_config_new();
    assert(*cfg != NULL);

#if defined(LYS_BACKEND_opencl) || defined(LYS_BACKEND_cuda)
    if (deviceopt != NULL) {
        futhark_context_config_set_device(*cfg, deviceopt);
    }
#else
    (void)deviceopt;
#endif

#ifdef LYS_BACKEND_opencl
    if (interactive) {
        futhark_context_config_select_device_interactively(*cfg);
    }
#else
    (void)interactive;
#endif

    *ctx = futhark_context_new(*cfg);
    assert(*ctx != NULL);

#ifdef LYS_BACKEND_opencl
    cl_device_id device;
    assert(clGetCommandQueueInfo(futhark_context_get_command_queue(*ctx),
                                 CL_QUEUE_DEVICE, sizeof(cl_device_id), &device, NULL)
           == CL_SUCCESS);

    size_t dev_name_size;
    assert(clGetDeviceInfo(device, CL_DEVICE_NAME, 0, NULL, &dev_name_size)
           == CL_SUCCESS);
    char *dev_name = malloc(dev_name_size);
    assert(clGetDeviceInfo(device, CL_DEVICE_NAME, dev_name_size, dev_name, NULL)
           == CL_SUCCESS);

    printf("Using OpenCL device: %s\n", dev_name);
    printf("Use -d or -i to change this.\n");
    free(dev_name);
#endif
}

char* stradd(const char* a, const char* b) {
    char* c = malloc(sizeof(char) * (strlen(a) + strlen(b) + 1));
    strcpy(c, a);
    strcat(c, b);
    return c;
}

int main(int argc, char* argv[]) {
    char *deviceopt = NULL;
    int interactive = 0;

    char* parent_dir;
    {
	char* executable_path = argv[0];
        char* last_slash = strrchr(executable_path, '/');
        if (!last_slash) {
            // Windows
            last_slash = strrchr(executable_path, '\\');
        }
        if (last_slash) {
            size_t after_slash_i = (size_t)(last_slash + 1 - executable_path);
            parent_dir = malloc(sizeof(char) * (after_slash_i + 1));
            strncpy(parent_dir, executable_path, after_slash_i);
            parent_dir[after_slash_i] = '\0';
        } else {
            parent_dir = "";
        }
    }

    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
        printf("Usage: %s options...\n", argv[0]);
        puts("Options:");
        puts("  -d DEV  Set the computation device.");
        puts("  -i      Select execution device interactively.");
        puts("  -o OBJ  The object scene to render.");
        puts("  --help  Print this help and exit.");
        return 0;
    }

    int c;
    char* obj_path = stradd(parent_dir, "assets/CornellBox-Original.obj");
    while ( (c = getopt(argc, argv, "w:h:Rd:io:")) != -1) {
        switch (c) {
        case 'd':
            deviceopt = optarg;
            break;
        case 'i':
            interactive = 1;
            break;
        case 'o': {
            size_t l = strlen(optarg);
            obj_path = (char*)malloc(l + 1);
            strcpy(obj_path, optarg);
        } break;
        default:
            fprintf(stderr, "unknown option: %c\n", c);
            exit(EXIT_FAILURE);
        }
    }

    if (optind < argc) {
        fprintf(stderr, "Excess non-options: ");
        while (optind < argc)
            fprintf(stderr, "%s ", argv[optind++]);
        fprintf(stderr, "\n");
        exit(EXIT_FAILURE);
    }

    struct lys_context ctx;
    memset(&ctx, 0, sizeof(struct lys_context));
    ctx.width = INITIAL_WIDTH;
    ctx.height = INITIAL_HEIGHT;
    SDL_ASSERT(SDL_Init(SDL_INIT_EVERYTHING) == 0);

    struct futhark_context_config* futcfg;
    create_futhark_context(deviceopt, interactive, &futcfg, &ctx.fut);

    size_t num_tris, num_mat_components;
    float* triangle_data;
    uint32_t* triangle_mats;
    float* mat_data;
    load_obj_data(
        obj_path,
        &num_tris, &num_mat_components,
        &triangle_data, &triangle_mats, &mat_data);

    struct futhark_f32_3d* fut_triangle_data =
        futhark_new_f32_3d(ctx.fut, triangle_data, (int64_t)num_tris, 3, 3);
    struct futhark_u32_1d* fut_triangle_mats =
        futhark_new_u32_1d(ctx.fut, triangle_mats, (int64_t)num_tris);
    struct futhark_f32_2d* fut_mat_data =
        futhark_new_f32_2d(ctx.fut, mat_data, (int64_t)num_mat_components / 28, 28);

    do_sdl(&ctx, fut_triangle_data, fut_triangle_mats, fut_mat_data);

    futhark_context_free(ctx.fut);
    futhark_context_config_free(futcfg);
    free_obj_data(triangle_data, triangle_mats, mat_data);
    return 0;
}
