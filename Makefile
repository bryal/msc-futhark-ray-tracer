LYS_BACKEND?=opencl
CC=clang

PROG_FUT_DEPS:=$(shell find {lib,src} -name \*.fut)

NOWARN_CFLAGS=-std=c11 -O2 -DCL_TARGET_OPENCL_VERSION='220' -no-pie
CFLAGS=$(NOWARN_CFLAGS)  -Wall -Wextra -Wconversion -pedantic -DLYS_BACKEND_$(LYS_BACKEND)
BASE_LDFLAGS=-L./ljus/target/release -lljus_rs -lm -lfreetype -lpthread
INCLUDE=-I./ljus -I./build

ifeq ($(OS),Windows_NT)
OPENCL_LDFLAGS = -L"${OCL_ROOT}\lib\x86_64" -lOpenCL
OPENCL_INCLUDE = -I"${OCL_ROOT}\include"
else
OPENCL_LDFLAGS = -lOpenCL
endif

LDFLAGS = $(BASE_LDFLAGS)

ifeq ($(LYS_BACKEND),opencl)
	ifeq ($(OS),Windows_NT)
		LDFLAGS += -L"${OCL_ROOT}\lib\x86_64" -lOpenCL
		INCLUDE += -I"${OCL_ROOT}\include"
	else
		LDFLAGS += -lOpenCL
	endif
else ifeq ($(LYS_BACKEND),cuda)
	LDFLAGS += -lcuda -lnvrtc
else ifeq ($(LYS_BACKEND),c)
# nothing
else
	$(error Unknown LYS_BACKEND: $(LYS_BACKEND).  Must be 'opencl', 'cuda', or 'c')
endif

ifeq ($(OS),Windows_NT)
	LDFLAGS += -lmingw32 -lSDL2main -lSDL2 -lSDL2_ttf -lWs2_32 -lUserenv
	MAIN = main.exe
else
	LDFLAGS += -L./deps/SDL2/lib -ldl -lSDL2 -lSDL2_ttf
	INCLUDE += -I./deps/SDL2/include
	MAIN = main
endif

all: $(MAIN)

ifeq ($(shell test futhark.pkg -nt lib; echo $$?),0)
$(MAIN):
	futhark pkg sync
	@make # The sync might have resulted in a new Makefile.
else
$(MAIN): build/main.o build/main.h build/main_printf.h ljus/liblys.c ljus/liblys.h libljus_rs
	$(CC) ljus/liblys.c build/main.o -o $@ $(CFLAGS) $(INCLUDE) $(LDFLAGS)
endif

# We do not want warnings and such for the generated code.
build/main.o: build/main.c
	$(CC) -o $@ -c $< $(NOWARN_CFLAGS)

build/main.c: src/main.fut $(PROG_FUT_DEPS)
	mkdir -p build && futhark $(LYS_BACKEND) -o build/main --library src/main.fut

build/main.h: build/main.c

build/main_printf.h: build/main.c
	python3 ljus/gen_printf.py $@ $<

.PHONY: libljus_rs
libljus_rs: $(shell find ljus/src -name \*.rs)
	cd ljus; cargo build --lib --release

run: $(MAIN)
	./$(MAIN)

clean:
	rm -rf $(MAIN) build

clean-full: clean
	cd ljus && cargo clean
