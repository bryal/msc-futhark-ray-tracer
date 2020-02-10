LYS_BACKEND?=opencl

PROG_FUT_DEPS:=$(shell ls *.fut; find lib -name \*.fut)

all: main

NOWARN_CFLAGS=-std=c11 -O2 -DCL_TARGET_OPENCL_VERSION='220' -no-pie
CFLAGS?=$(NOWARN_CFLAGS)  -Wall -Wextra -Wconversion -pedantic -DLYS_BACKEND_$(LYS_BACKEND)
BASE_LDFLAGS=-L./deps/SDL2/lib -lm -l:libSDL2.a -l:libSDL2_ttf.a -lfreetype -ldl -lpthread
INCLUDE=-I. -I./rust-stuff -I./deps/SDL2/include

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
main: main_wrapper.o main_wrapper.h main_printf.h lys/liblys.c lys/liblys.h librust_stuff.a
	gcc lys/liblys.c main_wrapper.o librust_stuff.a -o $@ $(CFLAGS) $(INCLUDE) $(LDFLAGS)
endif

librust_stuff.a: $(shell find rust-stuff/src -name \*.rs)
	cd rust-stuff; cargo build --lib --release
	cp rust-stuff/target/release/librust_stuff.a ./

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
	./main

clean:
	rm -f main main.c main.h main_wrapper.* main_printf.h *.o librust_stuff.a
