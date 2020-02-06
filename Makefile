LYS_BACKEND?=opencl
PROG_FUT_DEPS:=$(shell ls *.fut; find lib -name \*.fut)

all: main

PKG_CFLAGS=$(shell pkg-config --cflags sdl2 SDL2_ttf)
NOWARN_CFLAGS=-std=c11 -O
CFLAGS?=$(NOWARN_CFLAGS) $(PKG_CFLAGS) -Wall -Wextra -Wconversion -pedantic -DLYS_BACKEND_$(LYS_BACKEND)
BASE_LDFLAGS=-lm -lSDL2 -lSDL2_ttf -ldl -lpthread

OPENCL_LDFLAGS?=-lOpenCL

ifeq ($(LYS_BACKEND),opencl)
	LDFLAGS?=$(OPENCL_LDFLAGS) $(BASE_LDFLAGS)
else ifeq ($(LYS_BACKEND),cuda)
	LDFLAGS?=$(BASE_LDFLAGS) -lcuda -lnvrtc
else ifeq ($(LYS_BACKEND),c)
	LDFLAGS?=$(BASE_LDFLAGS)
else
	$(error Unknown LYS_BACKEND: $(LYS_BACKEND).  Must be 'opencl', 'cuda', or 'c')
endif

ifeq ($(shell test futhark.pkg -nt lib; echo $$?),0)
main:
	futhark pkg sync
	@make # The sync might have resulted in a new Makefile.
else
main: main_wrapper.o main_wrapper.h main_printf.h lys/liblys.c lys/liblys.h libload_obj.a
	gcc lys/liblys.c main_wrapper.o libload_obj.a -o $@ $(CFLAGS) -I. $(LDFLAGS)
endif

libload_obj.a: load_obj.rs
	rustc load_obj.rs --crate-type staticlib --edition 2018

main_printf.h: main_wrapper.c
	python3 lys/gen_printf.py $@ $<

# We do not want warnings and such for the generated code.
main_wrapper.o: main_wrapper.c
	gcc -o $@ -c $< $(NOWARN_CFLAGS)

%.c: %.fut
	futhark $(LYS_BACKEND) --library $<

%_wrapper.fut: lys/genlys.fut $(PROG_FUT_DEPS)
	cat $< | sed 's/"lys"/"main"/' > $@

run: main
	./main -r 100000

clean:
	rm -f main main.c main.h main_wrapper.* main_printf.h *.o libload_obj.a
