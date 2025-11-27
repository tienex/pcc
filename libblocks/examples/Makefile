# Makefile for libblocks examples

CC = gcc
CFLAGS = -I.. -Wall -Wextra -std=c99
LDFLAGS = -L.. -lblocks

EXAMPLES = simple_blocks_example

all: $(EXAMPLES)

simple_blocks_example: simple_blocks_example.c ../libblocks.a
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

clean:
	rm -f $(EXAMPLES) *.o

test: all
	@echo "Running simple blocks example..."
	./simple_blocks_example

.PHONY: all clean test
