BACKEND=opencl
CC=clang

PROG_FUT_DEPS:=$(shell find {lib,src} -name \*.fut)

NOWARN_CFLAGS=-std=c11 -O2 -no-pie
CFLAGS=$(NOWARN_CFLAGS)  -Wall -Wextra -Wconversion -pedantic -DLYS_BACKEND_$(BACKEND)
LDFLAGS=-lm -lfreetype -lpthread
INCLUDE=-I./build

ifeq ($(OS),Windows_NT)
OPENCL_LDFLAGS = -L"${OCL_ROOT}\lib\x86_64" -lOpenCL
OPENCL_INCLUDE = -I"${OCL_ROOT}\include"
else
OPENCL_LDFLAGS = -lOpenCL
endif

ifeq ($(BACKEND),opencl)
	ifeq ($(OS),Windows_NT)
		LDFLAGS += -L"${OCL_ROOT}\lib\x86_64" -lOpenCL
		INCLUDE += -I"${OCL_ROOT}\include"
	else
		LDFLAGS += -lOpenCL
	endif
else ifeq ($(BACKEND),cuda)
	LDFLAGS += -lcuda -lnvrtc
else ifeq ($(BACKEND),c)
# nothing
else
	$(error Unknown BACKEND: $(BACKEND).  Must be 'opencl', 'cuda', or 'c')
endif

ifeq ($(OS),Windows_NT)
	LDFLAGS += -lmingw32 -lSDL2main -lSDL2 -lSDL2_ttf -lWs2_32 -lUserenv
	MAIN=main.exe
	TRACER=tracer.lib
	LJUS=ljus.lib
else
	LDFLAGS += -L./deps/SDL2/lib -ldl -lSDL2 -lSDL2_ttf
	INCLUDE += -I./deps/SDL2/include
	MAIN=main
	TRACER=libtracer.a
	LJUS=libljus.a
endif

.PHONY: all
all: demo-interactive/$(MAIN) $(TRACER)

.PHONY: lib
lib: $(TRACER)

demo-interactive/$(MAIN): build/tracer_printf.h demo-interactive/liblys.c demo-interactive/liblys.h $(TRACER)
	$(CC) demo-interactive/liblys.c $(TRACER) -o $@ $(CFLAGS) -I./demo-interactive $(INCLUDE) $(LDFLAGS)

$(TRACER): build/tracer.o build/$(LJUS)
	cd build && ar x $(LJUS) && ar rcs ../libtracer.a *.o

# We do not want warnings and such for the generated code.
build/tracer.o: build/tracer.c
	$(CC) -o $@ -c $< $(NOWARN_CFLAGS)

build/tracer.c: src/lib.fut futhark.pkg $(PROG_FUT_DEPS)
	test futhark.pkg -nt lib && futhark pkg sync; \
	mkdir -p build && futhark $(BACKEND) -o build/tracer --library src/lib.fut

build/tracer.h: build/tracer.c

build/tracer_printf.h: build/tracer.c demo-interactive/gen_printf.py
	python3 demo-interactive/gen_printf.py $@ $<

build/$(LJUS): $(shell find ljus/src -name \*.rs) ljus/Cargo.toml
	cd ljus && cargo build --lib --release && cp target/release/$(LJUS) ../build/$(LJUS)

run: demo-interactive/$(MAIN)
	./demo-interactive/$(MAIN) -o assets/SpectrumSphere.obj

clean:
	rm -rf demo-interactive/$(MAIN) build

clean-full: clean
	cd ljus && cargo clean
