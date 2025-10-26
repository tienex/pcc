/*
 * C# Language Version Management
 * Implementation of version-specific feature support
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cs_version.h"

/* Current language version */
int cs_language_version = CS_VERSION_DEFAULT;

/* Version feature table */
static const struct cs_version_features version_table[] = {
	/* C# 1.0 */
	{
		.version = CS_VERSION_10,
		.name = "C# 1.0",
		.features = CS_FEAT_BASIC_TYPES | CS_FEAT_CLASSES |
		           CS_FEAT_INTERFACES | CS_FEAT_DELEGATES |
		           CS_FEAT_EVENTS | CS_FEAT_PROPERTIES |
		           CS_FEAT_INDEXERS | CS_FEAT_ATTRIBUTES,
	},

	/* C# 2.0 */
	{
		.version = CS_VERSION_20,
		.name = "C# 2.0",
		.features = CS_FEAT_BASIC_TYPES | CS_FEAT_CLASSES |
		           CS_FEAT_INTERFACES | CS_FEAT_DELEGATES |
		           CS_FEAT_EVENTS | CS_FEAT_PROPERTIES |
		           CS_FEAT_INDEXERS | CS_FEAT_ATTRIBUTES |
		           CS_FEAT_GENERICS | CS_FEAT_NULLABLE_TYPES |
		           CS_FEAT_ITERATORS | CS_FEAT_ANONYMOUS_METHODS |
		           CS_FEAT_PARTIAL_TYPES | CS_FEAT_STATIC_CLASSES |
		           CS_FEAT_COVARIANCE,
	},

	/* C# 3.0 */
	{
		.version = CS_VERSION_30,
		.name = "C# 3.0",
		.features = CS_FEAT_BASIC_TYPES | CS_FEAT_CLASSES |
		           CS_FEAT_INTERFACES | CS_FEAT_DELEGATES |
		           CS_FEAT_EVENTS | CS_FEAT_PROPERTIES |
		           CS_FEAT_INDEXERS | CS_FEAT_ATTRIBUTES |
		           CS_FEAT_GENERICS | CS_FEAT_NULLABLE_TYPES |
		           CS_FEAT_ITERATORS | CS_FEAT_ANONYMOUS_METHODS |
		           CS_FEAT_PARTIAL_TYPES | CS_FEAT_STATIC_CLASSES |
		           CS_FEAT_COVARIANCE |
		           CS_FEAT_LINQ | CS_FEAT_LAMBDA |
		           CS_FEAT_EXTENSION_METHODS | CS_FEAT_ANONYMOUS_TYPES |
		           CS_FEAT_AUTO_PROPERTIES | CS_FEAT_OBJECT_INIT |
		           CS_FEAT_COLLECTION_INIT | CS_FEAT_EXPRESSION_TREES,
	},

	/* C# 4.0 */
	{
		.version = CS_VERSION_40,
		.name = "C# 4.0",
		.features = CS_FEAT_BASIC_TYPES | CS_FEAT_CLASSES |
		           CS_FEAT_INTERFACES | CS_FEAT_DELEGATES |
		           CS_FEAT_EVENTS | CS_FEAT_PROPERTIES |
		           CS_FEAT_INDEXERS | CS_FEAT_ATTRIBUTES |
		           CS_FEAT_GENERICS | CS_FEAT_NULLABLE_TYPES |
		           CS_FEAT_ITERATORS | CS_FEAT_ANONYMOUS_METHODS |
		           CS_FEAT_PARTIAL_TYPES | CS_FEAT_STATIC_CLASSES |
		           CS_FEAT_COVARIANCE |
		           CS_FEAT_LINQ | CS_FEAT_LAMBDA |
		           CS_FEAT_EXTENSION_METHODS | CS_FEAT_ANONYMOUS_TYPES |
		           CS_FEAT_AUTO_PROPERTIES | CS_FEAT_OBJECT_INIT |
		           CS_FEAT_COLLECTION_INIT | CS_FEAT_EXPRESSION_TREES |
		           CS_FEAT_DYNAMIC | CS_FEAT_NAMED_ARGS |
		           CS_FEAT_OPTIONAL_ARGS | CS_FEAT_GENERIC_VARIANCE,
	},

	/* C# 5.0 */
	{
		.version = CS_VERSION_50,
		.name = "C# 5.0",
		.features = CS_FEAT_BASIC_TYPES | CS_FEAT_CLASSES |
		           CS_FEAT_INTERFACES | CS_FEAT_DELEGATES |
		           CS_FEAT_EVENTS | CS_FEAT_PROPERTIES |
		           CS_FEAT_INDEXERS | CS_FEAT_ATTRIBUTES |
		           CS_FEAT_GENERICS | CS_FEAT_NULLABLE_TYPES |
		           CS_FEAT_ITERATORS | CS_FEAT_ANONYMOUS_METHODS |
		           CS_FEAT_PARTIAL_TYPES | CS_FEAT_STATIC_CLASSES |
		           CS_FEAT_COVARIANCE |
		           CS_FEAT_LINQ | CS_FEAT_LAMBDA |
		           CS_FEAT_EXTENSION_METHODS | CS_FEAT_ANONYMOUS_TYPES |
		           CS_FEAT_AUTO_PROPERTIES | CS_FEAT_OBJECT_INIT |
		           CS_FEAT_COLLECTION_INIT | CS_FEAT_EXPRESSION_TREES |
		           CS_FEAT_DYNAMIC | CS_FEAT_NAMED_ARGS |
		           CS_FEAT_OPTIONAL_ARGS | CS_FEAT_GENERIC_VARIANCE |
		           CS_FEAT_ASYNC_AWAIT | CS_FEAT_CALLER_INFO,
	},

	/* C# 6.0 */
	{
		.version = CS_VERSION_60,
		.name = "C# 6.0",
		.features = CS_FEAT_BASIC_TYPES | CS_FEAT_CLASSES |
		           CS_FEAT_INTERFACES | CS_FEAT_DELEGATES |
		           CS_FEAT_EVENTS | CS_FEAT_PROPERTIES |
		           CS_FEAT_INDEXERS | CS_FEAT_ATTRIBUTES |
		           CS_FEAT_GENERICS | CS_FEAT_NULLABLE_TYPES |
		           CS_FEAT_ITERATORS | CS_FEAT_ANONYMOUS_METHODS |
		           CS_FEAT_PARTIAL_TYPES | CS_FEAT_STATIC_CLASSES |
		           CS_FEAT_COVARIANCE |
		           CS_FEAT_LINQ | CS_FEAT_LAMBDA |
		           CS_FEAT_EXTENSION_METHODS | CS_FEAT_ANONYMOUS_TYPES |
		           CS_FEAT_AUTO_PROPERTIES | CS_FEAT_OBJECT_INIT |
		           CS_FEAT_COLLECTION_INIT | CS_FEAT_EXPRESSION_TREES |
		           CS_FEAT_DYNAMIC | CS_FEAT_NAMED_ARGS |
		           CS_FEAT_OPTIONAL_ARGS | CS_FEAT_GENERIC_VARIANCE |
		           CS_FEAT_ASYNC_AWAIT | CS_FEAT_CALLER_INFO |
		           CS_FEAT_EXPRESSION_BODIED | CS_FEAT_NULL_CONDITIONAL |
		           CS_FEAT_STRING_INTERPOLATION | CS_FEAT_NAMEOF |
		           CS_FEAT_USING_STATIC | CS_FEAT_EXCEPTION_FILTERS |
		           CS_FEAT_INDEX_INIT,
	},

	/* C# 7.0 */
	{
		.version = CS_VERSION_70,
		.name = "C# 7.0",
		.features = CS_FEAT_BASIC_TYPES | CS_FEAT_CLASSES |
		           CS_FEAT_INTERFACES | CS_FEAT_DELEGATES |
		           CS_FEAT_EVENTS | CS_FEAT_PROPERTIES |
		           CS_FEAT_INDEXERS | CS_FEAT_ATTRIBUTES |
		           CS_FEAT_GENERICS | CS_FEAT_NULLABLE_TYPES |
		           CS_FEAT_ITERATORS | CS_FEAT_ANONYMOUS_METHODS |
		           CS_FEAT_PARTIAL_TYPES | CS_FEAT_STATIC_CLASSES |
		           CS_FEAT_COVARIANCE |
		           CS_FEAT_LINQ | CS_FEAT_LAMBDA |
		           CS_FEAT_EXTENSION_METHODS | CS_FEAT_ANONYMOUS_TYPES |
		           CS_FEAT_AUTO_PROPERTIES | CS_FEAT_OBJECT_INIT |
		           CS_FEAT_COLLECTION_INIT | CS_FEAT_EXPRESSION_TREES |
		           CS_FEAT_DYNAMIC | CS_FEAT_NAMED_ARGS |
		           CS_FEAT_OPTIONAL_ARGS | CS_FEAT_GENERIC_VARIANCE |
		           CS_FEAT_ASYNC_AWAIT | CS_FEAT_CALLER_INFO |
		           CS_FEAT_EXPRESSION_BODIED | CS_FEAT_NULL_CONDITIONAL |
		           CS_FEAT_STRING_INTERPOLATION | CS_FEAT_NAMEOF |
		           CS_FEAT_USING_STATIC | CS_FEAT_EXCEPTION_FILTERS |
		           CS_FEAT_INDEX_INIT |
		           CS_FEAT_TUPLES | CS_FEAT_PATTERN_MATCHING |
		           CS_FEAT_OUT_VARS | CS_FEAT_LOCAL_FUNCTIONS |
		           CS_FEAT_REF_RETURNS | CS_FEAT_DISCARDS |
		           CS_FEAT_BINARY_LITERALS | CS_FEAT_DIGIT_SEPARATORS,
	},

	/* C# 7.3 */
	{
		.version = CS_VERSION_73,
		.name = "C# 7.3",
		.features = CS_FEAT_BASIC_TYPES | CS_FEAT_CLASSES |
		           CS_FEAT_INTERFACES | CS_FEAT_DELEGATES |
		           CS_FEAT_EVENTS | CS_FEAT_PROPERTIES |
		           CS_FEAT_INDEXERS | CS_FEAT_ATTRIBUTES |
		           CS_FEAT_GENERICS | CS_FEAT_NULLABLE_TYPES |
		           CS_FEAT_ITERATORS | CS_FEAT_ANONYMOUS_METHODS |
		           CS_FEAT_PARTIAL_TYPES | CS_FEAT_STATIC_CLASSES |
		           CS_FEAT_COVARIANCE |
		           CS_FEAT_LINQ | CS_FEAT_LAMBDA |
		           CS_FEAT_EXTENSION_METHODS | CS_FEAT_ANONYMOUS_TYPES |
		           CS_FEAT_AUTO_PROPERTIES | CS_FEAT_OBJECT_INIT |
		           CS_FEAT_COLLECTION_INIT | CS_FEAT_EXPRESSION_TREES |
		           CS_FEAT_DYNAMIC | CS_FEAT_NAMED_ARGS |
		           CS_FEAT_OPTIONAL_ARGS | CS_FEAT_GENERIC_VARIANCE |
		           CS_FEAT_ASYNC_AWAIT | CS_FEAT_CALLER_INFO |
		           CS_FEAT_EXPRESSION_BODIED | CS_FEAT_NULL_CONDITIONAL |
		           CS_FEAT_STRING_INTERPOLATION | CS_FEAT_NAMEOF |
		           CS_FEAT_USING_STATIC | CS_FEAT_EXCEPTION_FILTERS |
		           CS_FEAT_INDEX_INIT |
		           CS_FEAT_TUPLES | CS_FEAT_PATTERN_MATCHING |
		           CS_FEAT_OUT_VARS | CS_FEAT_LOCAL_FUNCTIONS |
		           CS_FEAT_REF_RETURNS | CS_FEAT_DISCARDS |
		           CS_FEAT_BINARY_LITERALS | CS_FEAT_DIGIT_SEPARATORS |
		           CS_FEAT_DEFAULT_LITERAL,
	},

	/* C# 8.0 */
	{
		.version = CS_VERSION_80,
		.name = "C# 8.0",
		.features = (unsigned long long)-1 & ~(CS_FEAT_RECORDS | CS_FEAT_INIT_ONLY |
		           CS_FEAT_TOP_LEVEL_STMTS | CS_FEAT_TARGET_TYPED_NEW |
		           CS_FEAT_RECORD_STRUCTS | CS_FEAT_GLOBAL_USING |
		           CS_FEAT_FILE_SCOPED_NS | CS_FEAT_LAMBDA_IMPROVEMENTS |
		           CS_FEAT_RAW_STRINGS | CS_FEAT_GENERIC_ATTRS |
		           CS_FEAT_LIST_PATTERNS | CS_FEAT_REQUIRED_MEMBERS |
		           CS_FEAT_PRIMARY_CTORS),
	},

	/* C# 9.0 */
	{
		.version = CS_VERSION_90,
		.name = "C# 9.0",
		.features = (unsigned long long)-1 & ~(
		           CS_FEAT_RECORD_STRUCTS | CS_FEAT_GLOBAL_USING |
		           CS_FEAT_FILE_SCOPED_NS | CS_FEAT_LAMBDA_IMPROVEMENTS |
		           CS_FEAT_RAW_STRINGS | CS_FEAT_GENERIC_ATTRS |
		           CS_FEAT_LIST_PATTERNS | CS_FEAT_REQUIRED_MEMBERS |
		           CS_FEAT_PRIMARY_CTORS),
	},

	/* C# 10.0 */
	{
		.version = CS_VERSION_100,
		.name = "C# 10.0",
		.features = (unsigned long long)-1 & ~(
		           CS_FEAT_RAW_STRINGS | CS_FEAT_GENERIC_ATTRS |
		           CS_FEAT_LIST_PATTERNS | CS_FEAT_REQUIRED_MEMBERS |
		           CS_FEAT_PRIMARY_CTORS),
	},

	/* C# 11.0 */
	{
		.version = CS_VERSION_110,
		.name = "C# 11.0",
		.features = (unsigned long long)-1 & ~(CS_FEAT_PRIMARY_CTORS),
	},

	/* C# 12.0 */
	{
		.version = CS_VERSION_120,
		.name = "C# 12.0",
		.features = (unsigned long long)-1,  /* All features */
	},
};

static const int version_table_size = sizeof(version_table) / sizeof(version_table[0]);

/* Parse version string to version number */
int cs_version_parse(const char *version_str) {
	if (!version_str)
		return CS_VERSION_DEFAULT;

	/* Handle "latest" keyword */
	if (strcmp(version_str, "latest") == 0)
		return CS_VERSION_LATEST;

	/* Parse version numbers */
	if (strcmp(version_str, "1") == 0 || strcmp(version_str, "1.0") == 0)
		return CS_VERSION_10;
	else if (strcmp(version_str, "2") == 0 || strcmp(version_str, "2.0") == 0)
		return CS_VERSION_20;
	else if (strcmp(version_str, "3") == 0 || strcmp(version_str, "3.0") == 0)
		return CS_VERSION_30;
	else if (strcmp(version_str, "4") == 0 || strcmp(version_str, "4.0") == 0)
		return CS_VERSION_40;
	else if (strcmp(version_str, "5") == 0 || strcmp(version_str, "5.0") == 0)
		return CS_VERSION_50;
	else if (strcmp(version_str, "6") == 0 || strcmp(version_str, "6.0") == 0)
		return CS_VERSION_60;
	else if (strcmp(version_str, "7") == 0 || strcmp(version_str, "7.0") == 0)
		return CS_VERSION_70;
	else if (strcmp(version_str, "7.1") == 0)
		return CS_VERSION_71;
	else if (strcmp(version_str, "7.2") == 0)
		return CS_VERSION_72;
	else if (strcmp(version_str, "7.3") == 0)
		return CS_VERSION_73;
	else if (strcmp(version_str, "8") == 0 || strcmp(version_str, "8.0") == 0)
		return CS_VERSION_80;
	else if (strcmp(version_str, "9") == 0 || strcmp(version_str, "9.0") == 0)
		return CS_VERSION_90;
	else if (strcmp(version_str, "10") == 0 || strcmp(version_str, "10.0") == 0)
		return CS_VERSION_100;
	else if (strcmp(version_str, "11") == 0 || strcmp(version_str, "11.0") == 0)
		return CS_VERSION_110;
	else if (strcmp(version_str, "12") == 0 || strcmp(version_str, "12.0") == 0)
		return CS_VERSION_120;

	/* Default to latest if unrecognized */
	fprintf(stderr, "Warning: Unrecognized C# version '%s', using latest\n",
	        version_str);
	return CS_VERSION_LATEST;
}

/* Convert version number to string */
const char *cs_version_to_string(int version) {
	for (int i = 0; i < version_table_size; i++) {
		if (version_table[i].version == version)
			return version_table[i].name;
	}
	return "Unknown";
}

/* Get feature set for a version */
unsigned long long cs_version_get_features(int version) {
	/* Find exact version or closest lower version */
	unsigned long long features = 0;

	for (int i = 0; i < version_table_size; i++) {
		if (version_table[i].version <= version)
			features = version_table[i].features;
		else
			break;
	}

	return features;
}

/* Check if a version supports a feature */
int cs_version_supports_feature(int version, enum cs_feature feature) {
	unsigned long long features = cs_version_get_features(version);
	return (features & feature) != 0;
}

/* Set current language version */
void cs_version_set_current(int version) {
	cs_language_version = version;
}

/* Get current language version */
int cs_version_get_current(void) {
	return cs_language_version;
}

/* Check if current version supports a feature */
int cs_version_check_feature(enum cs_feature feature) {
	return cs_version_supports_feature(cs_language_version, feature);
}

/* Print version information */
void cs_version_print_info(void) {
	printf("Current C# version: %s\n",
	       cs_version_to_string(cs_language_version));
	printf("Available features:\n");

	unsigned long long features = cs_version_get_features(cs_language_version);

	if (features & CS_FEAT_GENERICS)
		printf("  - Generics\n");
	if (features & CS_FEAT_NULLABLE_TYPES)
		printf("  - Nullable types\n");
	if (features & CS_FEAT_ITERATORS)
		printf("  - Iterators (yield)\n");
	if (features & CS_FEAT_LINQ)
		printf("  - LINQ\n");
	if (features & CS_FEAT_LAMBDA)
		printf("  - Lambda expressions\n");
	if (features & CS_FEAT_EXTENSION_METHODS)
		printf("  - Extension methods\n");
	if (features & CS_FEAT_ANONYMOUS_TYPES)
		printf("  - Anonymous types\n");
	if (features & CS_FEAT_DYNAMIC)
		printf("  - Dynamic binding\n");
	if (features & CS_FEAT_ASYNC_AWAIT)
		printf("  - async/await\n");
	if (features & CS_FEAT_NULL_CONDITIONAL)
		printf("  - Null-conditional operators (?.)\n");
	if (features & CS_FEAT_STRING_INTERPOLATION)
		printf("  - String interpolation\n");
	if (features & CS_FEAT_TUPLES)
		printf("  - Tuples\n");
	if (features & CS_FEAT_PATTERN_MATCHING)
		printf("  - Pattern matching\n");
	if (features & CS_FEAT_RECORDS)
		printf("  - Records\n");
	if (features & CS_FEAT_NULLABLE_REFS)
		printf("  - Nullable reference types\n");
	if (features & CS_FEAT_TOP_LEVEL_STMTS)
		printf("  - Top-level statements\n");
	if (features & CS_FEAT_GLOBAL_USING)
		printf("  - Global using directives\n");
	if (features & CS_FEAT_RAW_STRINGS)
		printf("  - Raw string literals\n");
	if (features & CS_FEAT_PRIMARY_CTORS)
		printf("  - Primary constructors\n");
}
