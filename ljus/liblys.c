// Copyright (c) 2019-2020. DIKU, University of Copenhagen
// ISC License

// Convenience framework for writing visualisations with Futhark and
// C/SDL.
//
// Based on initial SDL wrapper code by Jakob Stokholm Bertelsen.

#define _XOPEN_SOURCE
#include "liblys.h"
#include "main_printf.h"

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

static int64_t get_wall_time(void) {
    struct timeval time;
    assert(gettimeofday(&time,NULL) == 0);
    return time.tv_sec * 1000000 + time.tv_usec;
}

static uint32_t font_size_from_dimensions(uint32_t width, uint32_t height) {
    uint32_t size, font_size;
    if (height < width) {
        size = height;
    } else {
        size = width;
    }
    font_size = size / 45;
    if (font_size < 14) {
        font_size = 14;
    } else if (font_size > 32) {
        font_size = 32;
    }
    return font_size;
}

void window_size_updated(struct lys_context *ctx, uint32_t newx, uint32_t newy) {
    // https://stackoverflow.com/a/40122002
    ctx->wnd_surface = SDL_GetWindowSurface(ctx->wnd);
    SDL_ASSERT(ctx->wnd_surface != NULL);

    ctx->width = newx;
    ctx->height = newy;

    ctx->font_size = font_size_from_dimensions(ctx->width, ctx->height);
    TTF_CloseFont(ctx->font);
    ctx->font = TTF_OpenFont(ctx->font_path, (int)ctx->font_size);
    SDL_ASSERT(ctx->font != NULL);

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
                if (ctx->grab_mouse && ctx->mouse_grabbed) {
                    assert(SDL_SetRelativeMouseMode(0) == 0);
                    ctx->mouse_grabbed = 0;
                } else if (event.key.type == SDL_KEYDOWN) {
                    ctx->running = 0;
                }
                break;
            case SDLK_F1:
                if (event.key.type == SDL_KEYDOWN) {
                    ctx->show_text = !ctx->show_text;
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
        int64_t now = get_wall_time();
        float delta = ((float)(now - ctx->last_time))/1000000;
        ctx->fps = (ctx->fps*0.9f + (1.0f/delta)*0.1f);
        ctx->last_time = now;
        struct futhark_opaque_state *new_state;
        FUT_CHECK(ctx->fut, futhark_entry_step(ctx->fut, &new_state, delta, ctx->state));
        futhark_free_opaque_state(ctx->fut, ctx->state);
        ctx->state = new_state;

        FUT_CHECK(ctx->fut, futhark_entry_render(ctx->fut, &out_arr, ctx->state));
        FUT_CHECK(ctx->fut, futhark_values_i32_2d(ctx->fut, out_arr, ctx->data));
        FUT_CHECK(ctx->fut, futhark_free_i32_2d(ctx->fut, out_arr));

        SDL_ASSERT(SDL_BlitSurface(ctx->surface, NULL, ctx->wnd_surface, NULL)==0);

        if (ctx->show_text) {
            build_text(ctx, ctx->text_buffer, ctx->text_buffer_len, ctx->text_format,
                       ctx->fps, ctx->sum_names);
            if (*(ctx->text_buffer) != '\0') {
                uint32_t text_colour;
                FUT_CHECK(ctx->fut,
                          futhark_entry_text_colour(ctx->fut, (int32_t*) &text_colour,
                                                    ctx->state));
                SDL_Color sdl_text_colour =
                    { .a = (uint8_t)(text_colour >> 24) & 0xff,
                      .r = (uint8_t)(text_colour >> 16) & 0xff,
                      .g = (uint8_t)(text_colour >> 8) & 0xff,
                      .b = text_colour & 0xff };

                SDL_Surface *text_surface;
                SDL_Rect offset_rect;
                offset_rect.x = 10;
                uint32_t y = 10;
                char* buffer = ctx->text_buffer;
                while (true) {
                    char* buffer_start = buffer;

                    bool no_more_text = false;
                    while (true) {
                        if (*buffer == '\n') {
                            *buffer = '\0';
                            break;
                        } else if (*buffer == '\0') {
                            no_more_text = true;
                            break;
                        }
                        buffer++;
                    }

                    if (*buffer_start != '\0') {
                        text_surface = TTF_RenderUTF8_Blended(ctx->font, buffer_start, sdl_text_colour);
                        SDL_ASSERT(text_surface != NULL);
                        offset_rect.y = (int)y;
                        offset_rect.w = text_surface->w;
                        offset_rect.h = text_surface->h;
                        SDL_ASSERT(SDL_BlitSurface(text_surface, NULL,
                                                   ctx->wnd_surface, &offset_rect) == 0);
                        SDL_FreeSurface(text_surface);
                    }

                    if (no_more_text) {
                        break;
                    } else {
                        buffer++;
                        y += ctx->font_size;
                    }
                }
            }
        }

        SDL_ASSERT(SDL_UpdateWindowSurface(ctx->wnd) == 0);

        handle_sdl_events(ctx);
    }
}

void do_sdl(
    struct lys_context *ctx,
    bool allow_resize,
    struct futhark_f32_3d* triangle_data,
    struct futhark_u32_1d* triangle_mats,
    struct futhark_f32_2d* mat_data)
{
    struct futhark_context *fut = ctx->fut;

    ctx->last_time = get_wall_time();
    futhark_entry_init(
        fut, &ctx->state, (uint32_t)get_wall_time(),
        ctx->height, ctx->width,
        triangle_data, triangle_mats, mat_data);

    int flags = 0;
    if (allow_resize) {
        flags |= SDL_WINDOW_RESIZABLE;
    }
    ctx->wnd =
        SDL_CreateWindow("Gotta go fast! -- PROPERTTY OF VOLOVO VCARS DONT COPY THAT FLOPPY",
                         SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                         (int)ctx->width, (int)ctx->height, (uint32_t)flags);
    SDL_ASSERT(ctx->wnd != NULL);

    window_size_updated(ctx, ctx->width, ctx->height);

    ctx->running = 1;
    ctx->mouse_grabbed = 0;

    FUT_CHECK(ctx->fut, futhark_entry_grab_mouse(ctx->fut, &ctx->grab_mouse));
    if (ctx->grab_mouse) {
        assert(SDL_SetRelativeMouseMode(1) == 0);
        ctx->mouse_grabbed = 1;
    }

    struct futhark_u8_1d *text_format_array;
    FUT_CHECK(ctx->fut, futhark_entry_text_format(ctx->fut, &text_format_array));
    size_t text_format_len = (size_t)futhark_shape_u8_1d(ctx->fut, text_format_array)[0];
    ctx->text_format = malloc(sizeof(char) * (text_format_len + 1));
    assert(ctx->text_format != NULL);
    FUT_CHECK(ctx->fut, futhark_values_u8_1d(ctx->fut, text_format_array, (unsigned char*) ctx->text_format));
    ctx->text_format[text_format_len] = '\0';
    FUT_CHECK(ctx->fut, futhark_free_u8_1d(ctx->fut, text_format_array));

    ctx->sum_names = (char* **) malloc(sizeof(char* *) * n_printf_arguments());
    assert(ctx->sum_names != NULL);

    ctx->text_buffer_len = text_format_len;
    size_t i_arg = (size_t)(-1);
    for (size_t i = 0; i < text_format_len; i++) {
        if (ctx->text_format[i] == '%' &&
            i + 1 < text_format_len && ctx->text_format[i + 1] != '%') {
            i_arg++;
            if (ctx->text_format[i + 1] == '[') {
                ctx->text_format[i + 1] = 's';
                size_t end_pos;
                size_t n_choices = 1;
                bool found_end = false;
                for (end_pos = i + 2; end_pos < text_format_len; end_pos++) {
                    if (ctx->text_format[end_pos] == '|') {
                        n_choices++;
                    } else if (ctx->text_format[end_pos] == ']') {
                        found_end = true;
                        break;
                    }
                }
                assert(found_end);
                ctx->sum_names[i_arg] = (char* *) malloc(sizeof(char*) * (n_choices + 1));
                assert(ctx->sum_names[i_arg] != NULL);
                ctx->sum_names[i_arg][n_choices] = NULL;
                char* temp_choice = (char*) malloc(sizeof(char) * (end_pos - i - n_choices));
                assert(temp_choice != NULL);
                size_t choice_cur = 0;
                size_t i_choice = 0;
                for (size_t j = i + 2; j < end_pos + 1; j++) {
                    if (ctx->text_format[j] == '|' || ctx->text_format[j] == ']') {
                        temp_choice[choice_cur] = '\0';
                        ctx->sum_names[i_arg][i_choice] = (char*) malloc(sizeof(char) * (choice_cur + 1));
                        assert(ctx->sum_names[i_arg][i_choice] != NULL);
                        strncpy(ctx->sum_names[i_arg][i_choice], temp_choice, choice_cur + 1);
                        choice_cur = 0;
                        i_choice++;
                    } else {
                        temp_choice[choice_cur] = ctx->text_format[j];
                        choice_cur++;
                    }
                }
                free(temp_choice);
                size_t shift_left = end_pos - i - 1;
                for (size_t j = end_pos + 1; j < text_format_len; j++) {
                    ctx->text_format[j - shift_left] = ctx->text_format[j];
                }
                text_format_len -= shift_left;
                ctx->text_format[text_format_len] = '\0';
                i++;
            } else {
                ctx->sum_names[i_arg] = NULL;
                ctx->text_buffer_len += 20; // estimate
            }
        }
    }

    ctx->text_buffer = malloc(sizeof(char) * ctx->text_buffer_len);
    assert(ctx->text_buffer != NULL);
    ctx->text_buffer[0] = '\0';

    ctx->show_text = 1;

    sdl_loop(ctx);
    FUT_CHECK(fut, futhark_free_opaque_state(fut, ctx->state));

    free(ctx->text_format);
    free(ctx->text_buffer);

    for (size_t i = 0; i < n_printf_arguments(); i++) {
        if (ctx->sum_names[i] != NULL) {
            size_t j = 0;
            while (ctx->sum_names[i][j] != NULL) {
                free(ctx->sum_names[i][j]);
                j++;
            }
            free(ctx->sum_names[i]);
        }
    }
    free(ctx->sum_names);

    free(ctx->data);
    SDL_FreeSurface(ctx->surface);
    TTF_CloseFont(ctx->font);
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

void init_sdl(struct lys_context *ctx, char* font_path) {
    SDL_ASSERT(SDL_Init(SDL_INIT_EVERYTHING) == 0);
    SDL_ASSERT(TTF_Init() == 0);

    ctx->font_path = font_path;
    ctx->font_size = font_size_from_dimensions(ctx->width, ctx->height);
    ctx->font = TTF_OpenFont(ctx->font_path, (int)ctx->font_size);
    SDL_ASSERT(ctx->font != NULL);
}

char* stradd(const char* a, const char* b) {
    char* c = malloc(sizeof(char) * (strlen(a) + strlen(b) + 1));
    strcpy(c, a);
    strcat(c, b);
    return c;
}

int main(int argc, char* argv[]) {
    uint32_t width = INITIAL_WIDTH, height = INITIAL_HEIGHT;
    bool allow_resize = true;
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
        puts("  -w INT  Set the initial width of the window.");
        puts("  -h INT  Set the initial height of the window.");
        puts("  -R      Disallow resizing the window.");
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
        case 'w':
            width = (uint32_t)atoi(optarg);
            if (width <= 0) {
                fprintf(stderr, "'%s' is not a valid width.\n", optarg);
                exit(EXIT_FAILURE);
            }
            break;
        case 'h':
            height = (uint32_t)atoi(optarg);
            if (height <= 0) {
                fprintf(stderr, "'%s' is not a valid width.\n", optarg);
                exit(EXIT_FAILURE);
            }
            break;
        case 'R':
            allow_resize = false;
            break;
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

    char* font_path = stradd(parent_dir, "Fira/FiraMono-Medium.ttf");

    struct lys_context ctx;
    memset(&ctx, 0, sizeof(struct lys_context));
    ctx.width = width;
    ctx.height = height;
    ctx.fps = 0;
    init_sdl(&ctx, font_path);

    struct futhark_context_config* futcfg;
    struct futhark_context* futctx;
    create_futhark_context(deviceopt, interactive, &futcfg, &futctx);
    ctx.fut = futctx;


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

    do_sdl(&ctx, allow_resize, fut_triangle_data, fut_triangle_mats, fut_mat_data);

    free(font_path);

    futhark_context_free(futctx);
    futhark_context_config_free(futcfg);
    free_obj_data(triangle_data, triangle_mats, mat_data);
    return 0;
}
