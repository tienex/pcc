/*
 * COBOL type system - PICTURE clause parsing
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "pass1.h"

struct picture *
parse_picture(const char *pic)
{
	struct picture *p;
	const char *s = pic;
	int digits = 0, scale = 0, sign = 0;
	int type = COB_TYPE_ALPHANUMERIC;
	int after_v = 0;

	p = xmalloc(sizeof(*p));
	memset(p, 0, sizeof(*p));
	p->pic_string = xstrdup(pic);

	/* Parse PICTURE string */
	while (*s) {
		char ch = toupper(*s);
		int count = 1;

		/* Check for repeat count like 9(5) */
		if (s[1] == '(') {
			s += 2;
			count = 0;
			while (isdigit(*s)) {
				count = count * 10 + (*s - '0');
				s++;
			}
			if (*s == ')')
				s++;
		}

		switch (ch) {
		case '9':
			type = COB_TYPE_NUMERIC;
			if (after_v)
				scale += count;
			else
				digits += count;
			break;

		case 'X':
			type = COB_TYPE_ALPHANUMERIC;
			digits += count;
			break;

		case 'A':
			type = COB_TYPE_ALPHABETIC;
			digits += count;
			break;

		case 'S':
			sign = 1;
			break;

		case 'V':
			after_v = 1;
			break;

		case 'Z':
		case '*':
		case '+':
		case '-':
		case ',':
		case '.':
		case '/':
			type = COB_TYPE_EDITED;
			digits += count;
			break;

		default:
			s++;
			continue;
		}

		if (ch != 'S' && ch != 'V')
			s++;
	}

	p->pic_type = type;
	p->pic_digits = digits;
	p->pic_scale = scale;
	p->pic_sign = sign;
	p->pic_size = digits + (sign ? 1 : 0);

	return p;
}

int
get_cobol_size(struct picture *pic)
{
	if (!pic)
		return 4; /* Default */
	return pic->pic_size;
}

TWORD
picture_to_type(struct picture *pic)
{
	if (!pic)
		return INT;

	switch (pic->pic_type) {
	case COB_TYPE_NUMERIC:
		if (pic->pic_scale > 0)
			return DOUBLE;
		if (pic->pic_digits <= 4)
			return INT;
		if (pic->pic_digits <= 9)
			return LONG;
		return LONGLONG;

	case COB_TYPE_ALPHABETIC:
	case COB_TYPE_ALPHANUMERIC:
		return CHAR | PTR;

	case COB_TYPE_POINTER:
		return PTR;

	case COB_TYPE_OBJECT:
		return PTR;

	default:
		return INT;
	}
}
