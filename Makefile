LYS_BACKEND?=opencl
CC=clang

PROG_FUT_DEPS:=$(shell ls *.fut; find lib -name \*.fut)

NOWARN_CFLAGS=-std=c11 -O2 -DCL_TARGET_OPENCL_VERSION='220' -no-pie
CFLAGS=$(NOWARN_CFLAGS)  -Wall -Wextra -Wconversion -pedantic -DLYS_BACKEND_$(LYS_BACKEND)
BASE_LDFLAGS=-L./rust-stuff/target/release -lrust_stuff -lm -lfreetype -lpthread
INCLUDE=-I. -I./rust-stuff

OPENCL_LDFLAGS = -lOpenCL

ifeq ($(LYS_BACKEND),opencl)
	LDFLAGS=$(OPENCL_LDFLAGS) $(BASE_LDFLAGS)
else ifeq ($(LYS_BACKEND),cuda)
	LDFLAGS=$(BASE_LDFLAGS) -lcuda -lnvrtc
else ifeq ($(LYS_BACKEND),c)
	LDFLAGS=$(BASE_LDFLAGS)
else
	$(error Unknown LYS_BACKEND: $(LYS_BACKEND).  Must be 'opencl', 'cuda', or 'c')
endif

ifeq ($(OS),Windows_NT)
OPENCL_LDFLAGS += -L"${OCL_ROOT}\lib\x86_64"
LDFLAGS += -lmingw32 -lSDL2main -lSDL2 -lSDL2_ttf -lWs2_32 -lUserenv
INCLUDE += -I"${OCL_ROOT}\include"
MAIN=main.exe
else # assume Linux
LDFLAGS += -L./deps/SDL2/lib -ldl -lSDL2 -lSDL2_ttf
INCLUDE += -I./deps/SDL2/include
MAIN=main
endif

all: $(MAIN)

ifeq ($(shell test futhark.pkg -nt lib; echo $$?),0)
$(MAIN):
	futhark pkg sync
	@make # The sync might have resulted in a new Makefile.
else
$(MAIN): main_wrapper.o main_wrapper.h main_printf.h lys/liblys.c lys/liblys.h librust_stuff
	$(CC) lys/liblys.c main_wrapper.o -o $@ $(CFLAGS) $(INCLUDE) $(LDFLAGS)
endif

main_printf.h: main_wrapper.c
	python3 lys/gen_printf.py $@ $<

# We do not want warnings and such for the generated code.
main_wrapper.o: main_wrapper.c
	$(CC) -o $@ -c $< $(NOWARN_CFLAGS)

%.c: %.fut
	futhark $(LYS_BACKEND) --library $<

%_wrapper.fut: lys/genlys.fut $(PROG_FUT_DEPS)
	cat $< | sed 's/"lys"/"main"/' > $@

.PHONY: librust_stuff
librust_stuff: $(shell find rust-stuff/src -name \*.rs)
	cd rust-stuff; cargo build --lib --release

run: $(MAIN)
	./$(MAIN)

clean:
	rm -f $(MAIN) main.c main.h main_wrapper.* main_printf.h *.o librust_stuff.a
