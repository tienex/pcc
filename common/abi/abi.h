/*
 * Copyright (c) 2025 PCC Project
 *
 * Generic ABI (Application Binary Interface) Library
 *
 * Provides uniform interface for:
 * - Class/struct layout (field ordering, padding, vtable placement)
 * - Name mangling (C++, Pascal Object, etc.)
 * - Calling conventions
 * - Virtual table (vtable) layout
 * - Exception handling tables
 *
 * Supports multiple ABIs:
 * - Itanium C++ ABI (GCC, Clang on Unix/Linux/macOS)
 * - Visual C++ ABI (MSVC on Windows)
 * - Watcom C++ ABI
 *
 * This library can be used by:
 * - C++ compiler (cxxcom)
 * - Pascal compiler (pcom) for Object Pascal/Delphi classes
 * - C compiler (ccom) for struct layout
 * - Future language frontends
 */

#ifndef ABI_H
#define ABI_H

#include <stddef.h>
#include <stdint.h>

/* Forward declarations */
struct abi_context;
struct abi_type;
struct abi_class;
struct abi_function;

/* ABI types supported */
typedef enum {
	/* C++ ABIs */
	ABI_ITANIUM,        /* Itanium C++ ABI (GCC 3+, Clang, most Unix) */
	ABI_MSVC,           /* Microsoft Visual C++ ABI */
	ABI_WATCOM,         /* Watcom C++ ABI */
	ABI_ARM,            /* ARM C++ ABI (variant of Itanium) */
	ABI_BORLAND,        /* Borland C++/C++Builder/Delphi */
	ABI_GNU_OLD,        /* Old GNU C++ ABI (GCC 2.x, ARM/CFront) */
	ABI_DMC,            /* Digital Mars C++ (Zortech/Symantec) */
	ABI_CFRONT,         /* AT&T Cfront C++ ABI */
	ABI_EDG,            /* Edison Design Group C++ frontend */
	ABI_SUN,            /* Sun/Oracle C++ ABI */
	ABI_INTEL,          /* Intel C++ Compiler ABI */
	ABI_IBM,            /* IBM XL C++ ABI */
	ABI_HP,             /* HP aCC ABI */

	/* Objective-C ABIs */
	ABI_APPLE_OBJC1,    /* Apple/NeXT Objective-C 1.0 (Fragile) */
	ABI_APPLE_OBJC2,    /* Apple/NeXT Objective-C 2.0 (Non-fragile) */
	ABI_GNU_OBJC,       /* GNU Objective-C runtime */
	ABI_COREFOUNDATION, /* Apple CoreFoundation */

	/* Modern System Languages */
	ABI_DLANG,          /* D Language ABI */
	ABI_SWIFT,          /* Swift ABI */
	ABI_RUST,           /* Rust ABI */
	ABI_GO,             /* Go (golang) ABI */
	ABI_ZIG,            /* Zig ABI */
	ABI_CRYSTAL,        /* Crystal ABI */
	ABI_NIM,            /* Nim ABI */
	ABI_VLANG,          /* V Language ABI */
	ABI_JULIA,          /* Julia ABI */

	/* VM/Managed Languages */
	ABI_JAVA,           /* Java JNI ABI */
	ABI_CLR,            /* .NET Common Language Runtime */
	ABI_DART,           /* Dart Native ABI */
	ABI_KOTLIN,         /* Kotlin/Native ABI */

	/* Functional Languages */
	ABI_GHC,            /* Glasgow Haskell Compiler (GHC) */
	ABI_OCAML,          /* OCaml ABI */
	ABI_FSHARP,         /* F# ABI */

	/* Pascal ABIs */
	ABI_FREEPASCAL,     /* FreePascal ABI */
	ABI_GNU_PASCAL,     /* GNU Pascal (GPC) ABI */

	/* Fortran ABIs */
	ABI_GFORTRAN,       /* GNU Fortran (gfortran) */
	ABI_IFORT,          /* Intel Fortran (ifort/ifx) */
	ABI_NAG_FORTRAN,    /* NAG Fortran Compiler */

	/* Ada ABI */
	ABI_GNAT,           /* GNAT Ada ABI */

	/* Framework ABIs */
	ABI_ECERE,          /* eCere SDK ABI */

	/* IR/Bytecode ABIs */
	ABI_LLVM_IR,        /* LLVM Intermediate Representation */
	ABI_WASM,           /* WebAssembly */

	ABI_AUTO            /* Auto-detect based on platform */
} abi_kind_t;

/* Type classifications for ABI purposes */
typedef enum {
	ABI_TYPE_VOID,
	ABI_TYPE_BOOL,
	ABI_TYPE_CHAR,
	ABI_TYPE_SCHAR,
	ABI_TYPE_UCHAR,
	ABI_TYPE_SHORT,
	ABI_TYPE_USHORT,
	ABI_TYPE_INT,
	ABI_TYPE_UINT,
	ABI_TYPE_LONG,
	ABI_TYPE_ULONG,
	ABI_TYPE_LONGLONG,
	ABI_TYPE_ULONGLONG,
	ABI_TYPE_INT128,
	ABI_TYPE_UINT128,
	ABI_TYPE_FLOAT,
	ABI_TYPE_DOUBLE,
	ABI_TYPE_LONGDOUBLE,
	ABI_TYPE_FLOAT128,
	ABI_TYPE_POINTER,
	ABI_TYPE_REFERENCE,
	ABI_TYPE_RVALUE_REFERENCE,
	ABI_TYPE_MEMBERPOINTER,
	ABI_TYPE_ARRAY,
	ABI_TYPE_FUNCTION,
	ABI_TYPE_STRUCT,
	ABI_TYPE_CLASS,
	ABI_TYPE_UNION,
	ABI_TYPE_ENUM
} abi_type_kind_t;

/* Member visibility */
typedef enum {
	ABI_VISIBILITY_DEFAULT,
	ABI_VISIBILITY_HIDDEN,
	ABI_VISIBILITY_PROTECTED,
	ABI_VISIBILITY_INTERNAL
} abi_visibility_t;

/* Calling conventions */
typedef enum {
	ABI_CC_C,           /* C calling convention */
	ABI_CC_CDECL,       /* __cdecl (caller cleans stack) */
	ABI_CC_STDCALL,     /* __stdcall (callee cleans stack) */
	ABI_CC_FASTCALL,    /* __fastcall (args in registers) */
	ABI_CC_THISCALL,    /* __thiscall (this in ECX on x86) */
	ABI_CC_VECTORCALL,  /* __vectorcall (SIMD optimization) */
	ABI_CC_PASCAL,      /* Pascal calling convention */
	ABI_CC_SYSCALL,     /* System call */
	ABI_CC_WATCOM       /* Watcom register calling convention */
} abi_calling_conv_t;

/* Virtual table (vtable) entry types */
typedef enum {
	ABI_VTABLE_VCALL_OFFSET,      /* Virtual call offset */
	ABI_VTABLE_VBASE_OFFSET,      /* Virtual base offset */
	ABI_VTABLE_OFFSET_TO_TOP,     /* Offset to top of object */
	ABI_VTABLE_RTTI,              /* RTTI type info pointer */
	ABI_VTABLE_FUNCTION,          /* Virtual function pointer */
	ABI_VTABLE_DELETING_DTOR,     /* Deleting destructor */
	ABI_VTABLE_COMPLETE_DTOR,     /* Complete object destructor */
	ABI_VTABLE_THUNK              /* Thunk for covariant returns */
} abi_vtable_component_kind_t;

/* Class field descriptor */
typedef struct abi_field {
	const char *name;             /* Field name */
	struct abi_type *type;        /* Field type */
	size_t offset;                /* Offset from class start (bytes) */
	size_t bit_offset;            /* Bit offset for bitfields */
	size_t bit_width;             /* Bit width (0 = not a bitfield) */
	unsigned int is_static : 1;   /* Static member */
	unsigned int is_mutable : 1;  /* Mutable member */
	unsigned int access : 2;      /* 0=private, 1=protected, 2=public */
	struct abi_field *next;       /* Next field */
} abi_field_t;

/* Base class descriptor */
typedef struct abi_base {
	struct abi_class *base_class; /* Base class */
	size_t offset;                /* Offset from derived class start */
	unsigned int is_virtual : 1;  /* Virtual inheritance */
	unsigned int is_primary : 1;  /* Primary base class */
	unsigned int access : 2;      /* 0=private, 1=protected, 2=public */
	struct abi_base *next;        /* Next base class */
} abi_base_t;

/* Virtual function descriptor */
typedef struct abi_virtual_func {
	const char *name;             /* Function name */
	struct abi_function *func;    /* Function descriptor */
	size_t vtable_index;          /* Index in vtable */
	size_t vbase_offset;          /* Virtual base offset */
	int is_pure : 1;              /* Pure virtual */
	int is_deleted : 1;           /* Deleted function */
	struct abi_virtual_func *next;
} abi_virtual_func_t;

/* Class descriptor */
typedef struct abi_class {
	const char *name;             /* Class name */
	const char *mangled_name;     /* Mangled name (cached) */
	size_t size;                  /* Total size in bytes */
	size_t alignment;             /* Alignment requirement */
	size_t vtable_offset;         /* Offset to vtable pointer */
	size_t vbase_offset;          /* Virtual base offset */

	abi_field_t *fields;          /* Non-static data members */
	abi_base_t *bases;            /* Base classes */
	abi_virtual_func_t *virtuals; /* Virtual functions */

	unsigned int has_vtable : 1;  /* Has virtual functions */
	unsigned int has_vbases : 1;  /* Has virtual bases */
	unsigned int is_pod : 1;      /* Plain Old Data */
	unsigned int is_empty : 1;    /* Empty class */
	unsigned int is_polymorphic : 1;  /* Has virtual functions */
	unsigned int is_abstract : 1; /* Has pure virtual functions */
	unsigned int is_final : 1;    /* Cannot be derived from */

	/* ABI-specific data */
	void *abi_data;               /* Opaque ABI-specific info */
} abi_class_t;

/* Function parameter descriptor */
typedef struct abi_param {
	const char *name;             /* Parameter name */
	struct abi_type *type;        /* Parameter type */
	unsigned int is_const : 1;
	unsigned int is_volatile : 1;
	unsigned int is_restrict : 1;
	struct abi_param *next;
} abi_param_t;

/* Function descriptor */
typedef struct abi_function {
	const char *name;             /* Unmangled name */
	const char *mangled_name;     /* Mangled name (cached) */
	struct abi_type *return_type; /* Return type */
	abi_param_t *params;          /* Parameters */
	abi_calling_conv_t calling_conv;  /* Calling convention */

	/* Class membership */
	struct abi_class *parent_class;  /* NULL if not a method */

	/* Method qualifiers */
	unsigned int is_const : 1;    /* const method */
	unsigned int is_volatile : 1; /* volatile method */
	unsigned int is_virtual : 1;  /* Virtual method */
	unsigned int is_static : 1;   /* Static method */
	unsigned int is_constructor : 1;
	unsigned int is_destructor : 1;
	unsigned int is_inline : 1;
	unsigned int is_deleted : 1;  /* = delete */
	unsigned int is_defaulted : 1;  /* = default */

	/* Template info */
	unsigned int is_template : 1;
	void *template_args;          /* Template arguments (opaque) */
} abi_function_t;

/* Type descriptor */
typedef struct abi_type {
	abi_type_kind_t kind;         /* Type classification */
	size_t size;                  /* Size in bytes */
	size_t alignment;             /* Alignment requirement */

	/* Qualifiers */
	unsigned int is_const : 1;
	unsigned int is_volatile : 1;
	unsigned int is_restrict : 1;
	unsigned int is_unsigned : 1;

	/* Type-specific data */
	union {
		struct abi_class *class_type;  /* For CLASS/STRUCT */
		struct abi_type *pointee;      /* For POINTER/REFERENCE */
		struct abi_type *element;      /* For ARRAY */
		struct abi_function *function; /* For FUNCTION */
		struct {
			struct abi_type *member_type;
			struct abi_class *class_type;
		} member_pointer;              /* For MEMBERPOINTER */
	} u;

	size_t array_size;            /* For arrays */
	const char *name;             /* Type name (for debugging) */
} abi_type_t;

/* Name mangling context */
typedef struct abi_mangle_ctx {
	abi_kind_t abi;               /* ABI being used */
	char *buffer;                 /* Output buffer */
	size_t bufsize;               /* Buffer size */
	size_t pos;                   /* Current position */

	/* Substitution table for Itanium ABI */
	void *substitutions;
	int subst_count;

	/* Namespace stack */
	const char **namespaces;
	int namespace_depth;
} abi_mangle_ctx_t;

/* Class layout context */
typedef struct abi_layout_ctx {
	abi_kind_t abi;               /* ABI being used */
	size_t pointer_size;          /* Target pointer size */
	size_t pointer_align;         /* Target pointer alignment */

	/* Layout constraints */
	unsigned int pack_alignment;  /* #pragma pack value */
	unsigned int use_ms_bitfield_layout : 1;
	unsigned int is_union : 1;
} abi_layout_ctx_t;

/* ABI context - holds all ABI-related state */
typedef struct abi_context {
	abi_kind_t kind;              /* Current ABI */
	size_t pointer_size;          /* Target pointer size */
	size_t pointer_align;         /* Target pointer alignment */

	/* Function pointers for ABI operations */
	const struct abi_ops *ops;

	/* Cache of mangled names */
	void *mangle_cache;

	/* Cache of class layouts */
	void *layout_cache;
} abi_context_t;

/* ABI operations vtable */
typedef struct abi_ops {
	/* Name mangling */
	char *(*mangle_function)(abi_context_t *ctx, const abi_function_t *func);
	char *(*mangle_variable)(abi_context_t *ctx, const char *name, const abi_type_t *type);
	char *(*mangle_type)(abi_context_t *ctx, const abi_type_t *type);
	char *(*mangle_vtable)(abi_context_t *ctx, const abi_class_t *cls);
	char *(*mangle_rtti)(abi_context_t *ctx, const abi_class_t *cls);

	/* Class layout */
	void (*layout_class)(abi_context_t *ctx, abi_class_t *cls);
	void (*layout_bases)(abi_context_t *ctx, abi_class_t *cls);
	void (*layout_fields)(abi_context_t *ctx, abi_class_t *cls);
	void (*layout_vtable)(abi_context_t *ctx, abi_class_t *cls);

	/* Vtable construction */
	size_t (*compute_vtable_size)(abi_context_t *ctx, const abi_class_t *cls);
	void (*build_vtable)(abi_context_t *ctx, abi_class_t *cls, void **vtable);

	/* Type size/alignment */
	size_t (*sizeof_type)(abi_context_t *ctx, const abi_type_t *type);
	size_t (*alignof_type)(abi_context_t *ctx, const abi_type_t *type);

	/* Calling conventions */
	void (*setup_call)(abi_context_t *ctx, const abi_function_t *func,
	                   void *args, size_t nargs);
	void (*classify_return)(abi_context_t *ctx, const abi_type_t *type,
	                        int *in_memory, size_t *reg_count);
} abi_ops_t;

/*
 * Public API
 */

/* Initialize ABI context */
abi_context_t *abi_init(abi_kind_t kind);
void abi_destroy(abi_context_t *ctx);

/* Auto-detect ABI based on target platform */
abi_kind_t abi_detect(const char *target_triple);

/* Name mangling */
char *abi_mangle_function(abi_context_t *ctx, const abi_function_t *func);
char *abi_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type);
char *abi_mangle_type(abi_context_t *ctx, const abi_type_t *type);
char *abi_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls);
char *abi_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls);

/* Demangling (for debugging/error messages) */
char *abi_demangle(const char *mangled_name);

/* Class layout */
void abi_layout_class(abi_context_t *ctx, abi_class_t *cls);
size_t abi_sizeof_class(abi_context_t *ctx, const abi_class_t *cls);
size_t abi_alignof_class(abi_context_t *ctx, const abi_class_t *cls);
size_t abi_offsetof_field(abi_context_t *ctx, const abi_class_t *cls, const char *field);

/* Type operations */
abi_type_t *abi_create_type(abi_type_kind_t kind);
void abi_destroy_type(abi_type_t *type);
size_t abi_sizeof_type(abi_context_t *ctx, const abi_type_t *type);
size_t abi_alignof_type(abi_context_t *ctx, const abi_type_t *type);

/* Class operations */
abi_class_t *abi_create_class(const char *name);
void abi_destroy_class(abi_class_t *cls);
void abi_add_field(abi_class_t *cls, abi_field_t *field);
void abi_add_base(abi_class_t *cls, abi_base_t *base);
void abi_add_virtual(abi_class_t *cls, abi_virtual_func_t *vfunc);

/* Function operations */
abi_function_t *abi_create_function(const char *name);
void abi_destroy_function(abi_function_t *func);
void abi_add_param(abi_function_t *func, abi_param_t *param);

/* Vtable operations */
void **abi_build_vtable(abi_context_t *ctx, abi_class_t *cls);
size_t abi_vtable_size(abi_context_t *ctx, const abi_class_t *cls);
void *abi_vtable_lookup(void **vtable, size_t index);

/* Compatibility checking */
int abi_is_compatible(abi_kind_t abi1, abi_kind_t abi2);
int abi_can_interoperate(abi_kind_t abi1, abi_kind_t abi2, const abi_function_t *func);

/* Get ABI name as string */
const char *abi_name(abi_kind_t abi);

/* Debug output */
void abi_dump_class(abi_context_t *ctx, const abi_class_t *cls, FILE *out);
void abi_dump_function(abi_context_t *ctx, const abi_function_t *func, FILE *out);
void abi_dump_vtable(abi_context_t *ctx, const abi_class_t *cls, FILE *out);

#endif /* ABI_H */
