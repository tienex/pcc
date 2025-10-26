/*
 * C# Language Version Support
 * Compatibility with C# 1.0 through C# 12.0
 */

#ifndef _CS_VERSION_H_
#define _CS_VERSION_H_

/* C# Language Versions */
#define CS_VERSION_10   10   /* C# 1.0 (2002) - Initial release */
#define CS_VERSION_11   11   /* C# 1.1 */
#define CS_VERSION_20   20   /* C# 2.0 (2005) - Generics, iterators */
#define CS_VERSION_30   30   /* C# 3.0 (2007) - LINQ, lambdas */
#define CS_VERSION_40   40   /* C# 4.0 (2010) - Dynamic binding */
#define CS_VERSION_50   50   /* C# 5.0 (2012) - async/await */
#define CS_VERSION_60   60   /* C# 6.0 (2015) - Roslyn features */
#define CS_VERSION_70   70   /* C# 7.0 (2017) - Tuples, pattern matching */
#define CS_VERSION_71   71   /* C# 7.1 */
#define CS_VERSION_72   72   /* C# 7.2 */
#define CS_VERSION_73   73   /* C# 7.3 */
#define CS_VERSION_80   80   /* C# 8.0 (2019) - Nullable reference types */
#define CS_VERSION_90   90   /* C# 9.0 (2020) - Records */
#define CS_VERSION_100  100  /* C# 10.0 (2021) - Global usings */
#define CS_VERSION_110  110  /* C# 11.0 (2022) - Generic attributes */
#define CS_VERSION_120  120  /* C# 12.0 (2023) - Primary constructors */

#define CS_VERSION_LATEST CS_VERSION_120
#define CS_VERSION_DEFAULT CS_VERSION_120

/* Feature flags for version-specific features */
enum cs_feature {
	/* C# 1.0 Features */
	CS_FEAT_BASIC_TYPES      = (1 << 0),
	CS_FEAT_CLASSES          = (1 << 1),
	CS_FEAT_INTERFACES       = (1 << 2),
	CS_FEAT_DELEGATES        = (1 << 3),
	CS_FEAT_EVENTS           = (1 << 4),
	CS_FEAT_PROPERTIES       = (1 << 5),
	CS_FEAT_INDEXERS         = (1 << 6),
	CS_FEAT_ATTRIBUTES       = (1 << 7),

	/* C# 2.0 Features */
	CS_FEAT_GENERICS         = (1 << 8),
	CS_FEAT_NULLABLE_TYPES   = (1 << 9),
	CS_FEAT_ITERATORS        = (1 << 10),
	CS_FEAT_ANONYMOUS_METHODS = (1 << 11),
	CS_FEAT_PARTIAL_TYPES    = (1 << 12),
	CS_FEAT_STATIC_CLASSES   = (1 << 13),
	CS_FEAT_COVARIANCE       = (1 << 14),

	/* C# 3.0 Features */
	CS_FEAT_LINQ             = (1 << 15),
	CS_FEAT_LAMBDA           = (1 << 16),
	CS_FEAT_EXTENSION_METHODS = (1 << 17),
	CS_FEAT_ANONYMOUS_TYPES  = (1 << 18),
	CS_FEAT_AUTO_PROPERTIES  = (1 << 19),
	CS_FEAT_OBJECT_INIT      = (1 << 20),
	CS_FEAT_COLLECTION_INIT  = (1 << 21),
	CS_FEAT_EXPRESSION_TREES = (1 << 22),

	/* C# 4.0 Features */
	CS_FEAT_DYNAMIC          = (1 << 23),
	CS_FEAT_NAMED_ARGS       = (1 << 24),
	CS_FEAT_OPTIONAL_ARGS    = (1 << 25),
	CS_FEAT_GENERIC_VARIANCE = (1 << 26),

	/* C# 5.0 Features */
	CS_FEAT_ASYNC_AWAIT      = (1 << 27),
	CS_FEAT_CALLER_INFO      = (1 << 28),

	/* C# 6.0 Features */
	CS_FEAT_EXPRESSION_BODIED = (1 << 29),
	CS_FEAT_NULL_CONDITIONAL = (1 << 30),
	CS_FEAT_STRING_INTERPOLATION = (1ULL << 31),
	CS_FEAT_NAMEOF           = (1ULL << 32),
	CS_FEAT_USING_STATIC     = (1ULL << 33),
	CS_FEAT_EXCEPTION_FILTERS = (1ULL << 34),
	CS_FEAT_INDEX_INIT       = (1ULL << 35),

	/* C# 7.0-7.3 Features */
	CS_FEAT_TUPLES           = (1ULL << 36),
	CS_FEAT_PATTERN_MATCHING = (1ULL << 37),
	CS_FEAT_OUT_VARS         = (1ULL << 38),
	CS_FEAT_LOCAL_FUNCTIONS  = (1ULL << 39),
	CS_FEAT_REF_RETURNS      = (1ULL << 40),
	CS_FEAT_DISCARDS         = (1ULL << 41),
	CS_FEAT_BINARY_LITERALS  = (1ULL << 42),
	CS_FEAT_DIGIT_SEPARATORS = (1ULL << 43),
	CS_FEAT_DEFAULT_LITERAL  = (1ULL << 44),

	/* C# 8.0 Features */
	CS_FEAT_NULLABLE_REFS    = (1ULL << 45),
	CS_FEAT_ASYNC_STREAMS    = (1ULL << 46),
	CS_FEAT_RANGES_INDICES   = (1ULL << 47),
	CS_FEAT_SWITCH_EXPRESSIONS = (1ULL << 48),
	CS_FEAT_DEFAULT_INTERFACE = (1ULL << 49),
	CS_FEAT_USING_DECLARATIONS = (1ULL << 50),

	/* C# 9.0 Features */
	CS_FEAT_RECORDS          = (1ULL << 51),
	CS_FEAT_INIT_ONLY        = (1ULL << 52),
	CS_FEAT_TOP_LEVEL_STMTS  = (1ULL << 53),
	CS_FEAT_TARGET_TYPED_NEW = (1ULL << 54),

	/* C# 10.0 Features */
	CS_FEAT_RECORD_STRUCTS   = (1ULL << 55),
	CS_FEAT_GLOBAL_USING     = (1ULL << 56),
	CS_FEAT_FILE_SCOPED_NS   = (1ULL << 57),
	CS_FEAT_LAMBDA_IMPROVEMENTS = (1ULL << 58),

	/* C# 11.0 Features */
	CS_FEAT_RAW_STRINGS      = (1ULL << 59),
	CS_FEAT_GENERIC_ATTRS    = (1ULL << 60),
	CS_FEAT_LIST_PATTERNS    = (1ULL << 61),
	CS_FEAT_REQUIRED_MEMBERS = (1ULL << 62),

	/* C# 12.0 Features */
	CS_FEAT_PRIMARY_CTORS    = (1ULL << 63),
};

/* Version feature sets */
struct cs_version_features {
	int version;
	const char *name;
	unsigned long long features;
};

/* Function prototypes */
int cs_version_parse(const char *version_str);
const char *cs_version_to_string(int version);
int cs_version_supports_feature(int version, enum cs_feature feature);
unsigned long long cs_version_get_features(int version);
void cs_version_set_current(int version);
int cs_version_get_current(void);
int cs_version_check_feature(enum cs_feature feature);

/* Version-specific feature checking macros */
#define CS_HAS_GENERICS()        cs_version_check_feature(CS_FEAT_GENERICS)
#define CS_HAS_NULLABLE()        cs_version_check_feature(CS_FEAT_NULLABLE_TYPES)
#define CS_HAS_LINQ()            cs_version_check_feature(CS_FEAT_LINQ)
#define CS_HAS_LAMBDA()          cs_version_check_feature(CS_FEAT_LAMBDA)
#define CS_HAS_DYNAMIC()         cs_version_check_feature(CS_FEAT_DYNAMIC)
#define CS_HAS_ASYNC()           cs_version_check_feature(CS_FEAT_ASYNC_AWAIT)
#define CS_HAS_TUPLES()          cs_version_check_feature(CS_FEAT_TUPLES)
#define CS_HAS_PATTERN_MATCH()   cs_version_check_feature(CS_FEAT_PATTERN_MATCHING)
#define CS_HAS_RECORDS()         cs_version_check_feature(CS_FEAT_RECORDS)
#define CS_HAS_NULL_CONDITIONAL() cs_version_check_feature(CS_FEAT_NULL_CONDITIONAL)

/* External language version variable */
extern int cs_language_version;

#endif /* _CS_VERSION_H_ */
