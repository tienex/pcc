# Makefile for x86asm library
#
# Copyright (c) 2025
# BSD Licensed - See COPYING

CC = gcc
AR = ar
CFLAGS = -Wall -Wextra -O2 -fPIC -I.
LDFLAGS =

# Library sources
LIB_SRCS = x86asm.c x86asm_util.c \
           emit_gnu_as.c emit_apple_as.c \
           emit_nasm.c emit_yasm.c emit_fasm.c \
           emit_masm.c emit_tasm.c \
           emit_wasm.c

# Object files
LIB_OBJS = $(LIB_SRCS:.c=.o)

# Library name
LIB_STATIC = libx86asm.a
LIB_SHARED = libx86asm.so

# Test program
TEST_PROG = test_x86asm
TEST_SRCS = test_x86asm.c
TEST_OBJS = $(TEST_SRCS:.c=.o)

# Example program
EXAMPLE_PROG = example
EXAMPLE_SRCS = example.c
EXAMPLE_OBJS = $(EXAMPLE_SRCS:.c=.o)

# Section demo program
SECTION_DEMO = section_demo
SECTION_DEMO_SRCS = section_demo.c
SECTION_DEMO_OBJS = $(SECTION_DEMO_SRCS:.c=.o)

# Default target
all: $(LIB_STATIC) $(LIB_SHARED) $(TEST_PROG) $(EXAMPLE_PROG) $(SECTION_DEMO)

# Static library
$(LIB_STATIC): $(LIB_OBJS)
	$(AR) rcs $@ $^

# Shared library
$(LIB_SHARED): $(LIB_OBJS)
	$(CC) -shared -o $@ $^ $(LDFLAGS)

# Test program
$(TEST_PROG): $(TEST_OBJS) $(LIB_STATIC)
	$(CC) -o $@ $^ $(LDFLAGS)

# Example program
$(EXAMPLE_PROG): $(EXAMPLE_OBJS) $(LIB_STATIC)
	$(CC) -o $@ $^ $(LDFLAGS)

# Section demo program
$(SECTION_DEMO): $(SECTION_DEMO_OBJS) $(LIB_STATIC)
	$(CC) -o $@ $^ $(LDFLAGS)

# Compile C source files
%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

# Dependencies
x86asm.o: x86asm.c x86asm.h x86asm_internal.h
x86asm_util.o: x86asm_util.c x86asm_internal.h x86asm.h
emit_gnu_as.o: emit_gnu_as.c x86asm_internal.h x86asm.h
emit_apple_as.o: emit_apple_as.c x86asm_internal.h x86asm.h
emit_nasm.o: emit_nasm.c x86asm_internal.h x86asm.h
emit_yasm.o: emit_yasm.c x86asm_internal.h x86asm.h
emit_fasm.o: emit_fasm.c x86asm_internal.h x86asm.h
emit_masm.o: emit_masm.c x86asm_internal.h x86asm.h
emit_tasm.o: emit_tasm.c x86asm_internal.h x86asm.h
emit_wasm.o: emit_wasm.c x86asm_internal.h x86asm.h
test_x86asm.o: test_x86asm.c x86asm.h
example.o: example.c x86asm.h
section_demo.o: section_demo.c x86asm.h

# Run tests
test: $(TEST_PROG)
	./$(TEST_PROG)

# Clean build artifacts
clean:
	rm -f $(LIB_OBJS) $(TEST_OBJS) $(EXAMPLE_OBJS) $(SECTION_DEMO_OBJS)
	rm -f $(LIB_STATIC) $(LIB_SHARED) $(TEST_PROG) $(EXAMPLE_PROG) $(SECTION_DEMO)
	rm -f test_output_*.asm test_addressing_*.asm test_simd_*.asm
	rm -f strlen_*.asm vec_add_*.asm
	rm -f sections_*.asm

# Install library (optional)
install: $(LIB_STATIC) $(LIB_SHARED)
	install -d $(DESTDIR)/usr/local/lib
	install -m 644 $(LIB_STATIC) $(DESTDIR)/usr/local/lib/
	install -m 755 $(LIB_SHARED) $(DESTDIR)/usr/local/lib/
	install -d $(DESTDIR)/usr/local/include
	install -m 644 x86asm.h $(DESTDIR)/usr/local/include/

.PHONY: all test clean install
