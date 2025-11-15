/*
 * Copyright (c) 2025 PCC Project
 *
 * Universal Virtual Table and Messaging Library - Implementation
 */

#include "vtable.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>

/*
 * Internal Structures
 */

struct vtable_builder {
	vtable_type_t type;
	char *class_name;
	size_t instance_size;
	vfunc_entry_t **methods;
	size_t method_count;
	size_t method_capacity;
	vtable_t **bases;
	size_t base_count;
	size_t base_capacity;
};

/* Objective-C method cache entry */
typedef struct cache_entry {
	const char *selector;
	void *implementation;
	struct cache_entry *next;
} cache_entry_t;

#define CACHE_SIZE 16

typedef struct {
	cache_entry_t *buckets[CACHE_SIZE];
} method_cache_t;

/* Global Objective-C class registry */
static objc_class_t **class_registry = NULL;
static size_t class_registry_size = 0;
static size_t class_registry_capacity = 0;

/*
 * Utility Functions
 */

static char *strdup_safe(const char *str) {
	if (!str) return NULL;
	size_t len = strlen(str);
	char *dup = (char *)malloc(len + 1);
	if (dup) memcpy(dup, str, len + 1);
	return dup;
}

static unsigned int hash_string(const char *str) {
	unsigned int hash = 5381;
	int c;
	while ((c = *str++))
		hash = ((hash << 5) + hash) + c;
	return hash;
}

/*
 * Vtable Builder Implementation
 */

vtable_builder_t *vtable_builder_create(vtable_type_t type, const char *class_name) {
	vtable_builder_t *builder = (vtable_builder_t *)calloc(1, sizeof(vtable_builder_t));
	if (!builder) return NULL;

	builder->type = type;
	builder->class_name = strdup_safe(class_name);
	builder->method_capacity = 8;
	builder->methods = (vfunc_entry_t **)calloc(builder->method_capacity, sizeof(vfunc_entry_t *));
	builder->base_capacity = 4;
	builder->bases = (vtable_t **)calloc(builder->base_capacity, sizeof(vtable_t *));

	return builder;
}

int vtable_builder_add_method(vtable_builder_t *builder, const vfunc_entry_t *entry) {
	if (!builder || !entry) return -1;

	if (builder->method_count >= builder->method_capacity) {
		builder->method_capacity *= 2;
		vfunc_entry_t **new_methods = (vfunc_entry_t **)realloc(builder->methods,
			builder->method_capacity * sizeof(vfunc_entry_t *));
		if (!new_methods) return -1;
		builder->methods = new_methods;
	}

	vfunc_entry_t *new_entry = (vfunc_entry_t *)malloc(sizeof(vfunc_entry_t));
	if (!new_entry) return -1;

	*new_entry = *entry;
	new_entry->name = strdup_safe(entry->name);
	new_entry->mangled_name = strdup_safe(entry->mangled_name);
	new_entry->offset = builder->method_count;

	builder->methods[builder->method_count++] = new_entry;
	return 0;
}

int vtable_builder_add_base(vtable_builder_t *builder, const vtable_t *base_vtable) {
	if (!builder || !base_vtable) return -1;

	if (builder->base_count >= builder->base_capacity) {
		builder->base_capacity *= 2;
		vtable_t **new_bases = (vtable_t **)realloc(builder->bases,
			builder->base_capacity * sizeof(vtable_t *));
		if (!new_bases) return -1;
		builder->bases = new_bases;
	}

	builder->bases[builder->base_count++] = (vtable_t *)base_vtable;
	return 0;
}

void vtable_builder_set_size(vtable_builder_t *builder, size_t size) {
	if (builder) builder->instance_size = size;
}

vtable_t *vtable_builder_build(vtable_builder_t *builder) {
	if (!builder) return NULL;

	vtable_t *vtable = (vtable_t *)calloc(1, sizeof(vtable_t));
	if (!vtable) return NULL;

	vtable->type = builder->type;
	vtable->class_name = strdup_safe(builder->class_name);
	vtable->instance_size = builder->instance_size;

	switch (builder->type) {
	case VTABLE_CXX_SINGLE:
	case VTABLE_CXX_MULTIPLE:
	case VTABLE_CXX_VIRTUAL: {
		vtable_cxx_t *cxx = (vtable_cxx_t *)calloc(1, sizeof(vtable_cxx_t));
		cxx->num_entries = builder->method_count;
		cxx->entries = (vfunc_entry_t **)malloc(builder->method_count * sizeof(vfunc_entry_t *));
		memcpy(cxx->entries, builder->methods, builder->method_count * sizeof(vfunc_entry_t *));

		if (builder->base_count > 0) {
			cxx->num_bases = builder->base_count;
			cxx->base_vtables = (vtable_cxx_t **)malloc(builder->base_count * sizeof(vtable_cxx_t *));
			for (size_t i = 0; i < builder->base_count; i++) {
				cxx->base_vtables[i] = builder->bases[i]->impl.cxx;
			}
		}

		vtable->impl.cxx = cxx;
		break;
	}
	case VTABLE_OBERON2:
	case VTABLE_COMPONENT_PASCAL: {
		vtable_oberon_t *oberon = (vtable_oberon_t *)calloc(1, sizeof(vtable_oberon_t));
		oberon->type_name = strdup_safe(builder->class_name);
		oberon->size = builder->instance_size;
		oberon->num_methods = builder->method_count;
		oberon->methods = (vfunc_entry_t **)malloc(builder->method_count * sizeof(vfunc_entry_t *));
		memcpy(oberon->methods, builder->methods, builder->method_count * sizeof(vfunc_entry_t *));

		if (builder->base_count > 0) {
			oberon->base = builder->bases[0]->impl.oberon;
			oberon->extension_level = oberon->base->extension_level + 1;
		}

		vtable->impl.oberon = oberon;
		break;
	}
	default:
		break;
	}

	return vtable;
}

void vtable_builder_free(vtable_builder_t *builder) {
	if (!builder) return;
	free(builder->class_name);
	free(builder->methods);
	free(builder->bases);
	free(builder);
}

/*
 * C++ Vtable Operations
 */

vtable_cxx_t *vtable_cxx_create(const char *class_name, size_t num_methods) {
	vtable_cxx_t *vtable = (vtable_cxx_t *)calloc(1, sizeof(vtable_cxx_t));
	if (!vtable) return NULL;

	vtable->num_entries = num_methods;
	vtable->entries = (vfunc_entry_t **)calloc(num_methods, sizeof(vfunc_entry_t *));

	return vtable;
}

int vtable_cxx_add_function(vtable_cxx_t *vtable, const vfunc_entry_t *func) {
	if (!vtable || !func || func->offset >= vtable->num_entries) return -1;

	vfunc_entry_t *entry = (vfunc_entry_t *)malloc(sizeof(vfunc_entry_t));
	if (!entry) return -1;

	*entry = *func;
	entry->name = strdup_safe(func->name);
	entry->mangled_name = strdup_safe(func->mangled_name);

	vtable->entries[func->offset] = entry;
	return 0;
}

int vtable_cxx_add_base(vtable_cxx_t *vtable, vtable_cxx_t *base, ptrdiff_t offset) {
	if (!vtable || !base) return -1;

	size_t new_count = vtable->num_bases + 1;
	vtable_cxx_t **new_bases = (vtable_cxx_t **)realloc(vtable->base_vtables,
		new_count * sizeof(vtable_cxx_t *));
	if (!new_bases) return -1;

	vtable->base_vtables = new_bases;
	vtable->base_vtables[vtable->num_bases] = base;
	vtable->num_bases = new_count;

	return 0;
}

void *vtable_cxx_lookup(const vtable_cxx_t *vtable, size_t index) {
	if (!vtable || index >= vtable->num_entries) return NULL;
	if (!vtable->entries[index]) return NULL;
	return vtable->entries[index]->function_ptr;
}

vfunc_entry_t *vtable_cxx_find_method(const vtable_cxx_t *vtable, const char *name) {
	if (!vtable || !name) return NULL;

	for (size_t i = 0; i < vtable->num_entries; i++) {
		if (vtable->entries[i] && vtable->entries[i]->name &&
		    strcmp(vtable->entries[i]->name, name) == 0) {
			return vtable->entries[i];
		}
	}

	/* Search in base classes */
	for (size_t i = 0; i < vtable->num_bases; i++) {
		vfunc_entry_t *found = vtable_cxx_find_method(vtable->base_vtables[i], name);
		if (found) return found;
	}

	return NULL;
}

ptrdiff_t vtable_cxx_base_offset(const vtable_cxx_t *vtable, const char *base_class) {
	if (!vtable || !base_class) return -1;

	/* For now, return simple offset - would need class layout info */
	return 0;
}

void *vtable_cxx_get_rtti(const vtable_cxx_t *vtable) {
	return vtable ? vtable->type_info : NULL;
}

/*
 * Objective-C Messaging Implementation
 */

static method_cache_t *cache_create(void) {
	method_cache_t *cache = (method_cache_t *)calloc(1, sizeof(method_cache_t));
	return cache;
}

static void cache_free(method_cache_t *cache) {
	if (!cache) return;

	for (int i = 0; i < CACHE_SIZE; i++) {
		cache_entry_t *entry = cache->buckets[i];
		while (entry) {
			cache_entry_t *next = entry->next;
			free(entry);
			entry = next;
		}
	}
	free(cache);
}

static void *cache_lookup(method_cache_t *cache, const char *selector) {
	if (!cache || !selector) return NULL;

	unsigned int hash = hash_string(selector) % CACHE_SIZE;
	cache_entry_t *entry = cache->buckets[hash];

	while (entry) {
		if (strcmp(entry->selector, selector) == 0) {
			return entry->implementation;
		}
		entry = entry->next;
	}

	return NULL;
}

static void cache_insert(method_cache_t *cache, const char *selector, void *implementation) {
	if (!cache || !selector) return;

	unsigned int hash = hash_string(selector) % CACHE_SIZE;

	cache_entry_t *entry = (cache_entry_t *)malloc(sizeof(cache_entry_t));
	entry->selector = selector;
	entry->implementation = implementation;
	entry->next = cache->buckets[hash];
	cache->buckets[hash] = entry;
}

objc_class_t *objc_class_create(const char *name, objc_class_t *super, size_t instance_size) {
	objc_class_t *cls = (objc_class_t *)calloc(1, sizeof(objc_class_t));
	if (!cls) return NULL;

	cls->name = strdup_safe(name);
	cls->super = super;
	cls->instance_size = instance_size;
	cls->cache = cache_create();

	return cls;
}

int objc_class_add_method(objc_class_t *cls, const char *selector,
                          void *implementation, const char *types) {
	if (!cls || !selector || !implementation) return -1;

	objc_method_t *method = (objc_method_t *)malloc(sizeof(objc_method_t));
	if (!method) return -1;

	method->selector = strdup_safe(selector);
	method->types = strdup_safe(types);
	method->implementation = implementation;

	size_t new_count = cls->num_methods + 1;
	objc_method_t **new_methods = (objc_method_t **)realloc(cls->methods,
		new_count * sizeof(objc_method_t *));
	if (!new_methods) {
		free(method);
		return -1;
	}

	cls->methods = new_methods;
	cls->methods[cls->num_methods] = method;
	cls->num_methods = new_count;

	return 0;
}

int objc_class_add_ivar(objc_class_t *cls, const char *name, size_t size, size_t alignment) {
	if (!cls || !name) return -1;

	/* Simple ivar storage - would need layout calculation */
	size_t new_count = cls->num_ivars + 1;
	void **new_ivars = (void **)realloc(cls->ivars, new_count * sizeof(void *));
	if (!new_ivars) return -1;

	cls->ivars = new_ivars;
	cls->num_ivars = new_count;

	return 0;
}

void objc_class_register(objc_class_t *cls) {
	if (!cls) return;

	if (class_registry_size >= class_registry_capacity) {
		size_t new_capacity = class_registry_capacity == 0 ? 16 : class_registry_capacity * 2;
		objc_class_t **new_registry = (objc_class_t **)realloc(class_registry,
			new_capacity * sizeof(objc_class_t *));
		if (!new_registry) return;
		class_registry = new_registry;
		class_registry_capacity = new_capacity;
	}

	class_registry[class_registry_size++] = cls;
}

objc_method_t *objc_class_get_method(objc_class_t *cls, const char *selector) {
	if (!cls || !selector) return NULL;

	/* Search own methods */
	for (size_t i = 0; i < cls->num_methods; i++) {
		if (strcmp(cls->methods[i]->selector, selector) == 0) {
			return cls->methods[i];
		}
	}

	/* Search superclass */
	if (cls->super) {
		return objc_class_get_method(cls->super, selector);
	}

	return NULL;
}

void objc_cache_method(objc_class_t *cls, const char *selector, void *implementation) {
	if (!cls || !selector || !implementation) return;
	cache_insert((method_cache_t *)cls->cache, selector, implementation);
}

void *objc_msg_send(void *obj, const char *selector, ...) {
	if (!obj || !selector) return NULL;

	/* Get object's class */
	objc_class_t *cls = *(objc_class_t **)obj; /* isa pointer */

	/* Check cache */
	method_cache_t *cache = (method_cache_t *)cls->cache;
	void *cached = cache_lookup(cache, selector);
	if (cached) {
		/* Call cached implementation - simplified, would need varargs forwarding */
		typedef void *(*method_imp_t)(void *, const char *, ...);
		method_imp_t imp = (method_imp_t)cached;
		va_list args;
		va_start(args, selector);
		void *result = imp(obj, selector, args);
		va_end(args);
		return result;
	}

	/* Lookup method */
	objc_method_t *method = objc_class_get_method(cls, selector);
	if (!method) {
		fprintf(stderr, "Selector '%s' not found\n", selector);
		return NULL;
	}

	/* Cache it */
	cache_insert(cache, selector, method->implementation);

	/* Call implementation */
	typedef void *(*method_imp_t)(void *, const char *, ...);
	method_imp_t imp = (method_imp_t)method->implementation;
	va_list args;
	va_start(args, selector);
	void *result = imp(obj, selector, args);
	va_end(args);

	return result;
}

void *objc_msg_send_super(void *obj, const char *selector, ...) {
	if (!obj || !selector) return NULL;

	objc_class_t *cls = *(objc_class_t **)obj;
	if (!cls->super) return NULL;

	objc_method_t *method = objc_class_get_method(cls->super, selector);
	if (!method) return NULL;

	typedef void *(*method_imp_t)(void *, const char *, ...);
	method_imp_t imp = (method_imp_t)method->implementation;
	va_list args;
	va_start(args, selector);
	void *result = imp(obj, selector, args);
	va_end(args);

	return result;
}

objc_class_t *objc_get_class(const char *name) {
	if (!name) return NULL;

	for (size_t i = 0; i < class_registry_size; i++) {
		if (strcmp(class_registry[i]->name, name) == 0) {
			return class_registry[i];
		}
	}

	return NULL;
}

objc_class_t *objc_get_object_class(void *obj) {
	if (!obj) return NULL;
	return *(objc_class_t **)obj;
}

/*
 * Oberon-2 Type-Bound Procedures
 */

vtable_oberon_t *vtable_oberon_create(const char *type_name, size_t size) {
	vtable_oberon_t *vtable = (vtable_oberon_t *)calloc(1, sizeof(vtable_oberon_t));
	if (!vtable) return NULL;

	vtable->type_name = strdup_safe(type_name);
	vtable->size = size;
	vtable->extension_level = 0;

	return vtable;
}

vtable_oberon_t *vtable_oberon_extend(vtable_oberon_t *base, const char *derived_name) {
	if (!base) return NULL;

	vtable_oberon_t *derived = (vtable_oberon_t *)calloc(1, sizeof(vtable_oberon_t));
	if (!derived) return NULL;

	derived->type_name = strdup_safe(derived_name);
	derived->size = base->size;
	derived->base = base;
	derived->extension_level = base->extension_level + 1;

	/* Copy base methods */
	derived->num_methods = base->num_methods;
	if (base->num_methods > 0) {
		derived->methods = (vfunc_entry_t **)malloc(base->num_methods * sizeof(vfunc_entry_t *));
		memcpy(derived->methods, base->methods, base->num_methods * sizeof(vfunc_entry_t *));
	}

	return derived;
}

int vtable_oberon_add_method(vtable_oberon_t *vtable, const vfunc_entry_t *method) {
	if (!vtable || !method) return -1;

	/* Check if overriding existing method */
	for (size_t i = 0; i < vtable->num_methods; i++) {
		if (strcmp(vtable->methods[i]->name, method->name) == 0) {
			/* Override */
			vfunc_entry_t *new_entry = (vfunc_entry_t *)malloc(sizeof(vfunc_entry_t));
			*new_entry = *method;
			new_entry->name = strdup_safe(method->name);
			new_entry->offset = i;
			vtable->methods[i] = new_entry;
			return 0;
		}
	}

	/* Add new method */
	size_t new_count = vtable->num_methods + 1;
	vfunc_entry_t **new_methods = (vfunc_entry_t **)realloc(vtable->methods,
		new_count * sizeof(vfunc_entry_t *));
	if (!new_methods) return -1;

	vfunc_entry_t *new_entry = (vfunc_entry_t *)malloc(sizeof(vfunc_entry_t));
	*new_entry = *method;
	new_entry->name = strdup_safe(method->name);
	new_entry->offset = vtable->num_methods;

	vtable->methods = new_methods;
	vtable->methods[vtable->num_methods] = new_entry;
	vtable->num_methods = new_count;

	return 0;
}

void *vtable_oberon_dispatch(vtable_oberon_t *vtable, const char *method_name) {
	if (!vtable || !method_name) return NULL;

	for (size_t i = 0; i < vtable->num_methods; i++) {
		if (strcmp(vtable->methods[i]->name, method_name) == 0) {
			return vtable->methods[i]->function_ptr;
		}
	}

	/* Search base */
	if (vtable->base) {
		return vtable_oberon_dispatch(vtable->base, method_name);
	}

	return NULL;
}

int vtable_oberon_is_extension(vtable_oberon_t *type, vtable_oberon_t *base) {
	if (!type || !base) return 0;

	vtable_oberon_t *current = type;
	while (current) {
		if (current == base) return 1;
		current = current->base;
	}

	return 0;
}

/*
 * Multiple Inheritance Support
 */

mi_layout_t *vtable_mi_calculate_layout(vtable_cxx_t **bases, size_t num_bases) {
	if (!bases || num_bases == 0) return NULL;

	mi_layout_t *layout = (mi_layout_t *)calloc(1, sizeof(mi_layout_t));
	if (!layout) return NULL;

	layout->num_vtables = num_bases;
	layout->offsets = (ptrdiff_t *)calloc(num_bases, sizeof(ptrdiff_t));
	layout->vtables = (vtable_cxx_t **)malloc(num_bases * sizeof(vtable_cxx_t *));

	/* Simple layout - first base at offset 0, others follow */
	ptrdiff_t current_offset = 0;
	for (size_t i = 0; i < num_bases; i++) {
		layout->offsets[i] = current_offset;
		layout->vtables[i] = bases[i];
		current_offset += sizeof(void *); /* vtable pointer size */
	}

	return layout;
}

void *vtable_mi_adjust_this(void *obj, ptrdiff_t offset) {
	if (!obj) return NULL;
	return (char *)obj + offset;
}

vtable_cxx_t *vtable_mi_get_base_vtable(void *obj, size_t base_index) {
	if (!obj) return NULL;
	/* Get vtable pointer at base_index */
	vtable_cxx_t ***vtable_array = (vtable_cxx_t ***)obj;
	return vtable_array[base_index] ? *vtable_array[base_index] : NULL;
}

void vtable_mi_free_layout(mi_layout_t *layout) {
	if (!layout) return;
	free(layout->offsets);
	free(layout->vtables);
	free(layout);
}

/*
 * Virtual Inheritance Support
 */

vbtable_t *vtable_create_vbtable(size_t num_virtual_bases) {
	vbtable_t *vbt = (vbtable_t *)calloc(1, sizeof(vbtable_t));
	if (!vbt) return NULL;

	vbt->num_virtual_bases = num_virtual_bases;
	vbt->offsets = (ptrdiff_t *)calloc(num_virtual_bases, sizeof(ptrdiff_t));
	vbt->base_names = (const char **)calloc(num_virtual_bases, sizeof(char *));

	return vbt;
}

int vtable_vbtable_add_base(vbtable_t *vbt, const char *base_name, ptrdiff_t offset) {
	if (!vbt || !base_name) return -1;

	for (size_t i = 0; i < vbt->num_virtual_bases; i++) {
		if (!vbt->base_names[i]) {
			vbt->base_names[i] = strdup_safe(base_name);
			vbt->offsets[i] = offset;
			return 0;
		}
	}

	return -1;
}

ptrdiff_t vtable_vbtable_get_offset(const vbtable_t *vbt, const char *base_name) {
	if (!vbt || !base_name) return -1;

	for (size_t i = 0; i < vbt->num_virtual_bases; i++) {
		if (vbt->base_names[i] && strcmp(vbt->base_names[i], base_name) == 0) {
			return vbt->offsets[i];
		}
	}

	return -1;
}

void vtable_vbtable_free(vbtable_t *vbt) {
	if (!vbt) return;
	for (size_t i = 0; i < vbt->num_virtual_bases; i++) {
		free((void *)vbt->base_names[i]);
	}
	free(vbt->offsets);
	free(vbt->base_names);
	free(vbt);
}

/*
 * Cleanup Functions
 */

void vtable_free(vtable_t *vtable) {
	if (!vtable) return;

	free((void *)vtable->class_name);

	switch (vtable->type) {
	case VTABLE_CXX_SINGLE:
	case VTABLE_CXX_MULTIPLE:
	case VTABLE_CXX_VIRTUAL:
		vtable_cxx_free(vtable->impl.cxx);
		break;
	case VTABLE_OBERON2:
	case VTABLE_COMPONENT_PASCAL:
		vtable_oberon_free(vtable->impl.oberon);
		break;
	case VTABLE_OBJC:
		objc_class_free(vtable->impl.objc);
		break;
	default:
		break;
	}

	free(vtable);
}

void vtable_cxx_free(vtable_cxx_t *vtable) {
	if (!vtable) return;

	for (size_t i = 0; i < vtable->num_entries; i++) {
		if (vtable->entries[i]) {
			free((void *)vtable->entries[i]->name);
			free((void *)vtable->entries[i]->mangled_name);
			free(vtable->entries[i]);
		}
	}

	free(vtable->entries);
	free(vtable->base_vtables);
	free(vtable);
}

void vtable_oberon_free(vtable_oberon_t *vtable) {
	if (!vtable) return;

	free((void *)vtable->type_name);

	for (size_t i = 0; i < vtable->num_methods; i++) {
		if (vtable->methods[i]) {
			free((void *)vtable->methods[i]->name);
			free(vtable->methods[i]);
		}
	}

	free(vtable->methods);
	free(vtable);
}

void objc_class_free(objc_class_t *cls) {
	if (!cls) return;

	free((void *)cls->name);

	for (size_t i = 0; i < cls->num_methods; i++) {
		if (cls->methods[i]) {
			free((void *)cls->methods[i]->selector);
			free((void *)cls->methods[i]->types);
			free(cls->methods[i]);
		}
	}

	free(cls->methods);
	free(cls->ivars);
	cache_free((method_cache_t *)cls->cache);
	free(cls);
}

void vtable_print(const vtable_t *vtable) {
	if (!vtable) return;

	printf("Vtable: %s\n", vtable->class_name);
	printf("Type: %d\n", vtable->type);
	printf("Instance size: %zu\n", vtable->instance_size);

	if (vtable->type == VTABLE_CXX_SINGLE || vtable->type == VTABLE_CXX_MULTIPLE) {
		vtable_cxx_t *cxx = vtable->impl.cxx;
		printf("Methods: %zu\n", cxx->num_entries);
		for (size_t i = 0; i < cxx->num_entries; i++) {
			if (cxx->entries[i]) {
				printf("  [%zu] %s (%s)\n", i, cxx->entries[i]->name,
					cxx->entries[i]->mangled_name);
			}
		}
	}
}

size_t vtable_method_count(const vtable_t *vtable) {
	if (!vtable) return 0;

	switch (vtable->type) {
	case VTABLE_CXX_SINGLE:
	case VTABLE_CXX_MULTIPLE:
	case VTABLE_CXX_VIRTUAL:
		return vtable->impl.cxx->num_entries;
	case VTABLE_OBERON2:
	case VTABLE_COMPONENT_PASCAL:
		return vtable->impl.oberon->num_methods;
	case VTABLE_OBJC:
		return vtable->impl.objc->num_methods;
	default:
		return 0;
	}
}
