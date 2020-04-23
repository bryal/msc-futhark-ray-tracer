BACKEND=opencl
CC=clang

PROG_FUT_DEPS:=$(shell find {lib,src} -name \*.fut)

NOWARN_CFLAGS=-std=c11 -O2 -fPIC
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
	EXE=.exe
else
	LDFLAGS += -L./deps/SDL2/lib -ldl -lSDL2 -lSDL2_ttf
	INCLUDE += -I./deps/SDL2/include
	EXE=
endif

.PHONY: all
all: main-interactive$(EXE) main-save$(EXE) libtracer.a

.PHONY: lib
lib: libtracer.a

main-interactive$(EXE): build/tracer_printf.h demo-interactive/liblys.c demo-interactive/liblys.h libtracer.a build/libljus.a
	$(CC) demo-interactive/liblys.c build/libljus.a libtracer.a -o $@ $(CFLAGS) -I./demo-interactive $(INCLUDE) $(LDFLAGS)

main-save$(EXE): $(shell find demo-save/src -name \*.rs) demo-save/Cargo.toml demo-save/.cargo/config libtracer.a
	cd demo-save && cargo build --release && cp target/release/demo-save$(EXE) ../main-save$(EXE)

libtracer.a: build/tracer.o
	ar rcs libtracer.a build/tracer.o

# We do not want warnings and such for the generated code.
build/tracer.o: build/tracer.c
	$(CC) -o $@ -c $< $(NOWARN_CFLAGS)

build/tracer.c: src/lib.fut futhark.pkg $(PROG_FUT_DEPS)
	test futhark.pkg -nt lib && futhark pkg sync; \
	mkdir -p build && futhark $(BACKEND) -o build/tracer --library src/lib.fut

build/tracer.h: build/tracer.c

build/tracer_printf.h: build/tracer.c demo-interactive/gen_printf.py
	python3 demo-interactive/gen_printf.py $@ $<

build/libljus.a: $(shell find ljus/src -name \*.rs) ljus/Cargo.toml
	cd ljus && cargo build --lib --release && cp target/release/libljus.a ../build/libljus.a

run: main-interactive$(EXE)
	./main-interactive$(EXE) -o assets/SpectrumSphere.obj

clean:
	rm -rf libtracer.a main-interactive$(EXE) main-save$(EXE) build

clean-full: clean
	cd ljus && cargo clean
