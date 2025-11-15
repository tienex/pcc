/*
 * Copyright (c) 2025 PCC Project
 *
 * Universal Virtual Table and Messaging Library
 *
 * Supports:
 * - C++ virtual functions and vtables
 * - Objective-C messaging and method dispatch
 * - Multiple inheritance (C++)
 * - Virtual inheritance
 * - Method resolution order (MRO)
 * - Cross-language virtual dispatch
 * - Runtime type information (RTTI)
 */

#ifndef _PCC_VTABLE_H_
#define _PCC_VTABLE_H_

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Vtable Types
 */
typedef enum {
	VTABLE_CXX_SINGLE = 0,    /* C++ single inheritance */
	VTABLE_CXX_MULTIPLE,      /* C++ multiple inheritance */
	VTABLE_CXX_VIRTUAL,       /* C++ virtual inheritance */
	VTABLE_OBJC,              /* Objective-C */
	VTABLE_OBERON2,           /* Oberon-2 type-bound procedures */
	VTABLE_COMPONENT_PASCAL,  /* Component Pascal */
	VTABLE_ADA_TAGGED,        /* Ada tagged types */
	VTABLE_MODULA3_OBJECT     /* Modula-3 objects */
} vtable_type_t;

/*
 * Method Calling Conventions
 */
typedef enum {
	CALL_CDECL = 0,
	CALL_STDCALL,
	CALL_THISCALL,
	CALL_FASTCALL,
	CALL_VECTORCALL,
	CALL_OBJC_MSGEND,
	CALL_PASCAL
} calling_convention_t;

/*
 * Virtual Function Entry
 */
typedef struct {
	const char *name;
	const char *mangled_name;
	void *function_ptr;
	size_t offset;            /* Offset in vtable */
	calling_convention_t convention;
	void *type_info;          /* Return type and parameter types */
	int is_pure_virtual;
	int is_override;
	int is_final;
} vfunc_entry_t;

/*
 * C++ Vtable Layout
 */
typedef struct vtable_cxx {
	ptrdiff_t offset_to_top;  /* For multiple inheritance */
	void *type_info;          /* RTTI pointer */
	size_t num_entries;
	vfunc_entry_t **entries;
	struct vtable_cxx **base_vtables;  /* For multiple inheritance */
	size_t num_bases;
} vtable_cxx_t;

/*
 * Objective-C Method
 */
typedef struct objc_method {
	const char *selector;     /* SEL */
	const char *types;        /* Type encoding */
	void *implementation;     /* IMP */
} objc_method_t;

/*
 * Objective-C Class
 */
typedef struct objc_class {
	struct objc_class *isa;   /* Metaclass */
	struct objc_class *super; /* Superclass */
	const char *name;
	size_t instance_size;
	size_t num_ivars;
	void **ivars;             /* Instance variables */
	size_t num_methods;
	objc_method_t **methods;
	void *cache;              /* Method cache */
	void *protocols;
} objc_class_t;

/*
 * Oberon-2 Type Descriptor
 */
typedef struct vtable_oberon {
	const char *type_name;
	size_t size;
	struct vtable_oberon *base;
	size_t num_methods;
	vfunc_entry_t **methods;
	int extension_level;      /* 0 for base type */
} vtable_oberon_t;

/*
 * Universal Vtable (works across languages)
 */
typedef struct {
	vtable_type_t type;
	const char *class_name;
	size_t instance_size;
	void *type_descriptor;
	union {
		vtable_cxx_t *cxx;
		objc_class_t *objc;
		vtable_oberon_t *oberon;
	} impl;
} vtable_t;

/*
 * Vtable Builder
 */
typedef struct vtable_builder vtable_builder_t;

/* Create vtable builder */
vtable_builder_t *vtable_builder_create(vtable_type_t type, const char *class_name);

/* Add virtual function */
int vtable_builder_add_method(vtable_builder_t *builder, const vfunc_entry_t *entry);

/* Add base class (for inheritance) */
int vtable_builder_add_base(vtable_builder_t *builder, const vtable_t *base_vtable);

/* Set instance size */
void vtable_builder_set_size(vtable_builder_t *builder, size_t size);

/* Build final vtable */
vtable_t *vtable_builder_build(vtable_builder_t *builder);

/* Free builder */
void vtable_builder_free(vtable_builder_t *builder);

/*
 * C++ Vtable Operations
 */

/* Create C++ vtable */
vtable_cxx_t *vtable_cxx_create(const char *class_name, size_t num_methods);

/* Add virtual function */
int vtable_cxx_add_function(vtable_cxx_t *vtable, const vfunc_entry_t *func);

/* Add base class vtable (multiple inheritance) */
int vtable_cxx_add_base(vtable_cxx_t *vtable, vtable_cxx_t *base, ptrdiff_t offset);

/* Lookup virtual function by index */
void *vtable_cxx_lookup(const vtable_cxx_t *vtable, size_t index);

/* Lookup by name */
vfunc_entry_t *vtable_cxx_find_method(const vtable_cxx_t *vtable, const char *name);

/* Calculate offset for base class cast */
ptrdiff_t vtable_cxx_base_offset(const vtable_cxx_t *vtable, const char *base_class);

/* Get RTTI */
void *vtable_cxx_get_rtti(const vtable_cxx_t *vtable);

/*
 * Objective-C Messaging
 */

/* Create Objective-C class */
objc_class_t *objc_class_create(const char *name, objc_class_t *super, size_t instance_size);

/* Add method to class */
int objc_class_add_method(objc_class_t *cls, const char *selector,
                          void *implementation, const char *types);

/* Add instance variable */
int objc_class_add_ivar(objc_class_t *cls, const char *name, size_t size, size_t alignment);

/* Register class */
void objc_class_register(objc_class_t *cls);

/* Message send (like objc_msgSend) */
void *objc_msg_send(void *obj, const char *selector, ...);

/* Message send to super */
void *objc_msg_send_super(void *obj, const char *selector, ...);

/* Lookup method */
objc_method_t *objc_class_get_method(objc_class_t *cls, const char *selector);

/* Method caching */
void objc_cache_method(objc_class_t *cls, const char *selector, void *implementation);

/* Get class by name */
objc_class_t *objc_get_class(const char *name);

/* Get object's class */
objc_class_t *objc_get_object_class(void *obj);

/*
 * Oberon-2 Type-Bound Procedures
 */

/* Create Oberon type descriptor */
vtable_oberon_t *vtable_oberon_create(const char *type_name, size_t size);

/* Extend type (inheritance) */
vtable_oberon_t *vtable_oberon_extend(vtable_oberon_t *base, const char *derived_name);

/* Add type-bound procedure */
int vtable_oberon_add_method(vtable_oberon_t *vtable, const vfunc_entry_t *method);

/* Dispatch method */
void *vtable_oberon_dispatch(vtable_oberon_t *vtable, const char *method_name);

/* Type test (IS) */
int vtable_oberon_is_extension(vtable_oberon_t *type, vtable_oberon_t *base);

/*
 * Multiple Inheritance Support
 */

/* Calculate vtable layout for multiple inheritance */
typedef struct {
	size_t num_vtables;       /* Number of vtable pointers needed */
	ptrdiff_t *offsets;       /* Offset of each vtable in object */
	vtable_cxx_t **vtables;   /* Vtables for each base */
} mi_layout_t;

mi_layout_t *vtable_mi_calculate_layout(vtable_cxx_t **bases, size_t num_bases);

/* Adjust 'this' pointer for multiple inheritance */
void *vtable_mi_adjust_this(void *obj, ptrdiff_t offset);

/* Get vtable for specific base class */
vtable_cxx_t *vtable_mi_get_base_vtable(void *obj, size_t base_index);

/* Free MI layout */
void vtable_mi_free_layout(mi_layout_t *layout);

/*
 * Virtual Inheritance Support
 */

/* Virtual base table (VBT) */
typedef struct {
	size_t num_virtual_bases;
	ptrdiff_t *offsets;       /* Offsets to virtual bases */
	const char **base_names;
} vbtable_t;

vbtable_t *vtable_create_vbtable(size_t num_virtual_bases);

int vtable_vbtable_add_base(vbtable_t *vbt, const char *base_name, ptrdiff_t offset);

ptrdiff_t vtable_vbtable_get_offset(const vbtable_t *vbt, const char *base_name);

void vtable_vbtable_free(vbtable_t *vbt);

/*
 * Method Resolution Order (MRO)
 */

/* Calculate MRO (C3 linearization) */
typedef struct {
	size_t num_classes;
	const char **class_names;
	vtable_t **vtables;
} mro_t;

mro_t *vtable_calculate_mro(const char *class_name, vtable_t **bases, size_t num_bases);

/* Lookup method using MRO */
vfunc_entry_t *vtable_mro_find_method(const mro_t *mro, const char *method_name);

void vtable_mro_free(mro_t *mro);

/*
 * Cross-Language Dispatch
 */

/* Universal method dispatch */
void *vtable_dispatch(vtable_t *vtable, const char *method_name, void *obj, ...);

/* Get method from any vtable type */
void *vtable_get_method(const vtable_t *vtable, const char *method_name);

/* Convert between vtable types */
vtable_t *vtable_convert(const vtable_t *src, vtable_type_t target_type);

/*
 * Runtime Type Information (RTTI)
 */

typedef struct {
	const char *type_name;
	const char *mangled_name;
	vtable_type_t vtable_type;
	size_t size;
	size_t num_bases;
	void **base_types;        /* Base type_info structures */
} type_info_t;

/* Create type info */
type_info_t *vtable_create_type_info(const char *type_name, vtable_type_t type);

/* Add base class type info */
int vtable_type_info_add_base(type_info_t *ti, type_info_t *base);

/* Dynamic cast (like dynamic_cast<>) */
void *vtable_dynamic_cast(void *obj, type_info_t *target_type);

/* Type check (like typeid) */
type_info_t *vtable_get_type_info(void *obj);

/* Check if types are compatible */
int vtable_is_base_of(type_info_t *base, type_info_t *derived);

/* Free type info */
void vtable_type_info_free(type_info_t *ti);

/*
 * Vtable Serialization (for precompiled headers)
 */

/* Serialize vtable to buffer */
size_t vtable_serialize(const vtable_t *vtable, void *buffer, size_t buffer_size);

/* Deserialize vtable from buffer */
vtable_t *vtable_deserialize(const void *buffer, size_t buffer_size);

/* Write vtable to file */
int vtable_write_file(const vtable_t *vtable, const char *filename);

/* Read vtable from file */
vtable_t *vtable_read_file(const char *filename);

/*
 * Debugging and Introspection
 */

/* Print vtable layout */
void vtable_print(const vtable_t *vtable);

/* Get method count */
size_t vtable_method_count(const vtable_t *vtable);

/* Iterate over methods */
typedef void (*vtable_method_callback_t)(const vfunc_entry_t *method, void *user_data);
void vtable_foreach_method(const vtable_t *vtable, vtable_method_callback_t callback, void *user_data);

/* Verify vtable consistency */
int vtable_verify(const vtable_t *vtable);

/* Get memory size of vtable */
size_t vtable_memory_size(const vtable_t *vtable);

/*
 * Cleanup
 */

void vtable_free(vtable_t *vtable);
void vtable_cxx_free(vtable_cxx_t *vtable);
void vtable_oberon_free(vtable_oberon_t *vtable);
void objc_class_free(objc_class_t *cls);

#ifdef __cplusplus
}
#endif

#endif /* _PCC_VTABLE_H_ */
