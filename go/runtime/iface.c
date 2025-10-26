/*
 * Copyright (c) 2025 PCC Go Runtime Library
 *
 * Interface operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

/*
 * Create new interface value
 */
go_interface
go_iface_new(go_type *typ, void *data)
{
	go_interface iface;

	iface.type = typ;

	if (data != NULL && typ != NULL) {
		/* Copy data */
		iface.data = go_malloc(typ->size);
		memcpy(iface.data, data, typ->size);
	} else {
		iface.data = NULL;
	}

	return iface;
}

/*
 * Check if interface is nil
 */
bool
go_iface_is_nil(go_interface iface)
{
	return (iface.type == NULL && iface.data == NULL);
}

/*
 * Check if interface contains specific type
 */
bool
go_iface_type_assert(go_interface iface, go_type *typ)
{
	if (iface.type == NULL || typ == NULL)
		return false;

	/* Simple pointer comparison */
	if (iface.type == typ)
		return true;

	/* Compare type properties */
	if (iface.type->kind == typ->kind &&
	    iface.type->size == typ->size &&
	    strcmp(iface.type->name, typ->name) == 0)
		return true;

	return false;
}

/*
 * Get interface data
 */
void *
go_iface_data(go_interface iface)
{
	return iface.data;
}

/*
 * Get interface type
 */
go_type *
go_iface_type(go_interface iface)
{
	return (go_type *)iface.type;
}

/*
 * Type assertion with result
 */
bool
go_type_assert(go_interface iface, go_type *typ, void *result)
{
	if (!go_iface_type_assert(iface, typ))
		return false;

	/* Copy data to result */
	if (result != NULL && iface.data != NULL && typ != NULL)
		memcpy(result, iface.data, typ->size);

	return true;
}

/*
 * Type assertion (comma-ok idiom)
 */
bool
go_type_assert_ok(go_interface iface, go_type *typ)
{
	return go_iface_type_assert(iface, typ);
}
