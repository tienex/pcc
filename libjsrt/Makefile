# Makefile for JavaScript Runtime Library (libjsrt)

CC = gcc
AR = ar
CFLAGS = -Wall -Wextra -O2 -g -std=c99 -fPIC
INCLUDES = -I.
ARFLAGS = rcs

# Library name
LIB = libjsrt.a
SOLIB = libjsrt.so

# Source files
SRCS = jsrt.c jsgc.c jsops.c jscoerce.c jsbuiltin.c jsstdlib.c

# Object files
OBJS = $(SRCS:.c=.o)

# Headers
HDRS = jsrt.h jsgc.h

# Default target
all: $(LIB) $(SOLIB)

# Static library
$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $@ $^
	@echo "Built static library: $@"

# Shared library
$(SOLIB): $(OBJS)
	$(CC) -shared -o $@ $^
	@echo "Built shared library: $@"

# Compile object files
%.o: %.c $(HDRS)
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

# Stub source files (to be implemented)
jsops.c:
	@echo "Creating stub: $@"
	@echo '/* JavaScript Operators - TO BE IMPLEMENTED */' > $@
	@echo '#include "jsrt.h"' >> $@
	@echo 'js_value_t *js_op_add(js_value_t *a, js_value_t *b) { return js_value_number(0); }' >> $@

jscoerce.c:
	@echo "Creating stub: $@"
	@echo '/* JavaScript Type Coercion - TO BE IMPLEMENTED */' > $@
	@echo '#include "jsrt.h"' >> $@
	@echo 'js_value_t *js_to_primitive(js_value_t *v, const char *h) { return v; }' >> $@
	@echo 'js_value_t *js_to_boolean(js_value_t *v) { return JS_FALSE(); }' >> $@
	@echo 'js_value_t *js_to_number(js_value_t *v) { return js_value_number(0); }' >> $@
	@echo 'js_value_t *js_to_bigint(js_value_t *v) { return js_value_bigint(0); }' >> $@
	@echo 'js_value_t *js_to_string(js_value_t *v) { return js_value_string(""); }' >> $@
	@echo 'js_value_t *js_to_object(js_value_t *v) { return js_value_object(); }' >> $@

jsbuiltin.c:
	@echo "Creating stub: $@"
	@echo '/* JavaScript Built-in Objects - TO BE IMPLEMENTED */' > $@
	@echo '#include "jsrt.h"' >> $@
	@echo 'void js_init_builtins(void) { /* Initialize built-ins */ }' >> $@
	@echo 'void js_function_free(js_function_t *fn) { if (fn) free(fn); }' >> $@

jsstdlib.c:
	@echo "Creating stub: $@"
	@echo '/* JavaScript Standard Library - TO BE IMPLEMENTED */' > $@
	@echo '#include "jsrt.h"' >> $@
	@echo '#include <stdio.h>' >> $@
	@echo 'js_value_t *js_console_log(js_value_t *this_val, js_value_t **args, int argc) {' >> $@
	@echo '    for (int i = 0; i < argc; i++) {' >> $@
	@echo '        js_value_print(args[i], stdout);' >> $@
	@echo '        if (i < argc - 1) printf(" ");' >> $@
	@echo '    }' >> $@
	@echo '    printf("\\n");' >> $@
	@echo '    return js_value_undefined();' >> $@
	@echo '}' >> $@

# Clean
clean:
	rm -f $(OBJS) $(LIB) $(SOLIB)
	rm -f jsops.c jscoerce.c jsbuiltin.c jsstdlib.c

# Install
PREFIX ?= /usr/local
install: $(LIB) $(SOLIB)
	install -d $(PREFIX)/lib
	install -m 644 $(LIB) $(PREFIX)/lib/
	install -m 755 $(SOLIB) $(PREFIX)/lib/
	install -d $(PREFIX)/include/jsrt
	install -m 644 $(HDRS) $(PREFIX)/include/jsrt/

uninstall:
	rm -f $(PREFIX)/lib/$(LIB)
	rm -f $(PREFIX)/lib/$(SOLIB)
	rm -rf $(PREFIX)/include/jsrt

# Test
test: all
	@echo "Running tests..."
	$(CC) $(CFLAGS) $(INCLUDES) -o test_jsrt test_jsrt.c -L. -ljsrt -lm
	./test_jsrt

# Create test file
test_jsrt.c:
	@echo "Creating test file..."
	@echo '#include "jsrt.h"' > $@
	@echo '#include <stdio.h>' >> $@
	@echo '#include <assert.h>' >> $@
	@echo 'int main() {' >> $@
	@echo '    js_runtime_init();' >> $@
	@echo '    js_value_t *v = js_value_number(42);' >> $@
	@echo '    assert(js_is_number(v));' >> $@
	@echo '    assert(js_get_number(v) == 42);' >> $@
	@echo '    js_value_print(v, stdout);' >> $@
	@echo '    printf("\\nAll tests passed!\\n");' >> $@
	@echo '    js_runtime_cleanup();' >> $@
	@echo '    return 0;' >> $@
	@echo '}' >> $@

.PHONY: all clean install uninstall test
