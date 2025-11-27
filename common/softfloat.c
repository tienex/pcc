/*	$Id$	*/

/*
 * Copyright (c) 2008 Anders Magnusson. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "manifest.h"
#include "softfloat.h"

#include <stdint.h>
#include <stdlib.h>

#ifndef PCC_DEBUG
#define assert(e) ((void)0)
#else
#define assert(e) (!(e)?cerror("assertion failed " #e " at softfloat:%d",__LINE__):(void)0)
#endif
/*
 * Floating point emulation, to not depend on the characteristics (and bugs)
 * of the host floating-point implementation when compiling.
 *
 * XXX - assumes that:
 *	- long long is (at least) 64 bits
 *	- int is at least 32 bits.
 *	- short is 16 bits.
 */

#if defined(FDFLOAT) || defined(PDP10FLOAT)

#ifdef FDFLOAT
/*
 * Supports F- and D-float, used in DEC VAX machines.
 *
 * XXX - assumes that:
 *	- long long is (at least) 64 bits
 *	- int is at least 32 bits.
 *	- short is 16 bits.
 */
#endif

#ifdef PDP10FLOAT
/*
 * Supports PDP-10 36-bit floating point format.
 *
 * Single precision (36-bit): sign(1) + exponent(8, excess-128) + fraction(27)
 * Double precision (72-bit): sign(1) + exponent(8, excess-1024) + fraction(62)
 *
 * The fraction has an implied binary point between bit 8 and bit 9.
 * Floating point zero is represented as all zeros.
 * Negatives are represented in two's complement.
 *
 * XXX - assumes that:
 *	- long long is (at least) 64 bits
 *	- int is at least 32 bits.
 *	- short is 16 bits.
 */
#endif

/*
 * Map fd1-fd4 to the fp[] array elements.
 */
#define fd1	fp[0]
#define fd2	fp[1]
#define fd3	fp[2]
#define fd4	fp[3]

/*
 * Useful macros to manipulate the float.
 */
#define DSIGN(w)	(((w).fd1 >> 15) & 1)
#define DSIGNSET(w,s)	((w).fd1 = (s << 15) | ((w).fd1 & 077777))
#define DEXP(w)		(((w).fd1 >> 7) & 0377)
#define DEXPSET(w,e)	((w).fd1 = (((e) & 0377) << 7) | ((w).fd1 & 0100177))
#define DMANTH(w)	((w).fd1 & 0177)
#define DMANTHSET(w,m)	((w).fd1 = ((m) & 0177) | ((w).fd1 & 0177600))
#define EXPBIAS		128

typedef unsigned int lword;
typedef unsigned long long dword;

#define MAXMANT 0x100000000000000LL

/*
 * Returns a zero dfloat.
 */
static SF
nulldf(void)
{
	SF rv;

	rv.fd1 = rv.fd2 = rv.fd3 = rv.fd4 = 0;
	return rv;
}

/*
 * Convert a (u)longlong to dfloat.
 * XXX - fails on too large (> 55 bits) numbers.
 * For FDFLOAT/PDP10FLOAT, we ignore the type parameter and always
 * return double precision.
 */
static SF
soft_cast_internal(CONSZ ll, TWORD t)
{
	int i;
	SF rv;

	rv = nulldf();
	if (ll == 0)
		return rv;  /* fp is zero */
	if (ll < 0)
		DSIGNSET(rv,1), ll = -ll;
	for (i = 0; ll > 0; i++, ll <<= 1)
		;
	DEXPSET(rv, (64+EXPBIAS)-i);
	DMANTHSET(rv, ll >> 56);
	rv.fd2 = ll >> 40;
	rv.fd3 = ll >> 24;
	rv.fd4 = ll >> 8;
	return rv;
}

/*
 * multiply two dfloat. Use chop, not round.
 */
static SF
soft_mul_internal(SF p1, SF p2)
{
	SF rv;
	lword a1[2], a2[2], res[4];
	dword sum;

	res[0] = res[1] = res[2] = res[3] = 0;

	/* move mantissa into lwords */
	a1[0] = p1.fd4 | (p1.fd3 << 16);
	a1[1] = p1.fd2 | DMANTH(p1) << 16 | 0x800000;

	a2[0] = p2.fd4 | (p2.fd3 << 16);
	a2[1] = p2.fd2 | DMANTH(p2) << 16 | 0x800000;

#define MULONE(x,y,r) sum += (dword)a1[x] * (dword)a2[y]; sum += res[r]; \
	res[r] = sum; sum >>= 32;

	sum = 0;
	MULONE(0, 0, 0);
	MULONE(1, 0, 1);
	res[2] = sum;
	sum = 0;
	MULONE(0, 1, 1);
	MULONE(1, 1, 2);
	res[3] = sum;

	rv.fd1 = 0;
	DSIGNSET(rv, DSIGN(p1) ^ DSIGN(p2));
	DEXPSET(rv, DEXP(p1) + DEXP(p2) - EXPBIAS);
	if (res[3] & 0x8000) {
		res[3] = (res[3] << 8) | (res[2] >> 24);
		res[2] = (res[2] << 8) | (res[1] >> 24);
	} else {
		DEXPSET(rv, DEXP(rv) - 1);
		res[3] = (res[3] << 9) | (res[2] >> 23);
		res[2] = (res[2] << 9) | (res[1] >> 23);
	}
	DMANTHSET(rv, res[3] >> 16);
	rv.fd2 = res[3];
	rv.fd3 = res[2] >> 16;
	rv.fd4 = res[2];
	return rv;
}

static SF
soft_div_internal(SF t, SF n)
{
	SF rv;
	dword T, N, K;
	int c;

#define SHL(x,b) ((dword)(x) << b)
	T = SHL(1,55) | SHL(DMANTH(t), 48) |
	    SHL(t.fd2, 32) | SHL(t.fd3, 16) | t.fd4;
	N = SHL(1,55) | SHL(DMANTH(n), 48) |
	    SHL(n.fd2, 32) | SHL(n.fd3, 16) | n.fd4;

	c = T > N;
	for (K = 0; (K & 0x80000000000000ULL) == 0; ) {
		if (T >= N) {
			T -= N;
			K |= 1;
		}
		T <<= 1;
		K <<= 1;
	}
	rv.fd1 = 0;
	DSIGNSET(rv, DSIGN(t) ^ DSIGN(n));
	DEXPSET(rv, DEXP(t) - DEXP(n) + EXPBIAS + c);
	DMANTHSET(rv, K >> 48);
	rv.fd2 = K >> 32;
	rv.fd3 = K >> 16;
	rv.fd4 = K;
	return rv;
}

/*
 * Negate a float number. Easy.
 */
SF
soft_neg(SF sf)
{
	int sign = DSIGN(sf) == 0;
	DSIGNSET(sf, sign);
	return sf;
}

/*
 * Return true if fp number is zero.
 */
int
soft_isz(SF sf)
{
	return (DEXP(sf) == 0);
}

int
soft_cmp_eq(SF x1, SF x2)
{
	/* Check if both are zero */
	if (DEXP(x1) == 0 && DEXP(x2) == 0)
		return 1;

	/* Compare all fields */
	return (x1.fd1 == x2.fd1 && x1.fd2 == x2.fd2 &&
	        x1.fd3 == x2.fd3 && x1.fd4 == x2.fd4);
}

int
soft_cmp_ne(SF x1, SF x2)
{
	return !soft_cmp_eq(x1, x2);
}

int
soft_cmp_le(SF x1, SF x2)
{
	int s1 = DSIGN(x1), s2 = DSIGN(x2);

	/* Handle zeros */
	if (DEXP(x1) == 0 && DEXP(x2) == 0)
		return 1;

	/* Different signs: negative < positive */
	if (s1 && !s2) return 1;
	if (!s1 && s2) return 0;

	/* Same sign: compare magnitudes */
	if (s1) {  /* both negative */
		return !(x1.fd1 < x2.fd1 || (x1.fd1 == x2.fd1 &&
		         (x1.fd2 < x2.fd2 || (x1.fd2 == x2.fd2 &&
		         (x1.fd3 < x2.fd3 || (x1.fd3 == x2.fd3 && x1.fd4 < x2.fd4))))));
	} else {  /* both positive */
		return (x1.fd1 < x2.fd1 || (x1.fd1 == x2.fd1 &&
		        (x1.fd2 < x2.fd2 || (x1.fd2 == x2.fd2 &&
		        (x1.fd3 < x2.fd3 || (x1.fd3 == x2.fd3 && x1.fd4 <= x2.fd4))))));
	}
}

int
soft_cmp_lt(SF x1, SF x2)
{
	return soft_cmp_le(x1, x2) && !soft_cmp_eq(x1, x2);
}

int
soft_cmp_ge(SF x1, SF x2)
{
	return !soft_cmp_lt(x1, x2);
}

int
soft_cmp_gt(SF x1, SF x2)
{
	return !soft_cmp_le(x1, x2);
}

/*
 * Convert a fp number to a CONSZ.
 */
CONSZ
soft_val(SF sf)
{
	CONSZ mant;
	int exp = DEXP(sf) - EXPBIAS;

	mant = SHL(1,55) | SHL(DMANTH(sf), 48) |
            SHL(sf.fd2, 32) | SHL(sf.fd3, 16) | sf.fd4;

	while (exp < 0)
		mant >>= 1, exp++;
	while (exp > 0)
		mant <<= 1, exp--;
	return mant;
}

static SF
soft_plus_internal(SF x1, SF x2)
{
	/*
	 * Basic floating-point addition for FDFLOAT/PDP10FLOAT.
	 * This is a simplified implementation that handles common cases.
	 * For full IEEE compliance, would need proper rounding, denormals, etc.
	 */
	SF result;
	int exp1, exp2, exp_diff;
	long long mant1, mant2;
	int sign1, sign2;

	/* Handle zeros */
	if (DEXP(x1) == 0) return x2;
	if (DEXP(x2) == 0) return x1;

	exp1 = DEXP(x1);
	exp2 = DEXP(x2);
	sign1 = DSIGN(x1);
	sign2 = DSIGN(x2);

	/* Extract mantissas with implied leading bit */
	mant1 = (1LL << 55) | (((long long)DMANTH(x1)) << 48) |
	        (((long long)x1.fd2) << 32) | (((long long)x1.fd3) << 16) | x1.fd4;
	mant2 = (1LL << 55) | (((long long)DMANTH(x2)) << 48) |
	        (((long long)x2.fd2) << 32) | (((long long)x2.fd3) << 16) | x2.fd4;

	/* Align exponents */
	exp_diff = exp1 - exp2;
	if (exp_diff > 0) {
		mant2 >>= exp_diff;
		result = x1;
	} else if (exp_diff < 0) {
		mant1 >>= -exp_diff;
		result = x2;
	} else {
		result = x1;
	}

	/* Add or subtract based on signs */
	if (sign1 == sign2) {
		mant1 += mant2;
	} else {
		if (mant1 >= mant2) {
			mant1 -= mant2;
		} else {
			mant1 = mant2 - mant1;
			DSIGNSET(result, sign2);
		}
	}

	/* Normalize result */
	while (mant1 >= (1LL << 56)) {
		mant1 >>= 1;
		DEXPSET(result, DEXP(result) + 1);
	}
	while (mant1 < (1LL << 55) && mant1 != 0) {
		mant1 <<= 1;
		DEXPSET(result, DEXP(result) - 1);
	}

	/* Check for zero result */
	if (mant1 == 0) {
		result.fd1 = result.fd2 = result.fd3 = result.fd4 = 0;
		return result;
	}

	/* Pack result */
	DMANTHSET(result, (mant1 >> 48) & 0177);
	result.fd2 = (mant1 >> 32) & 0xffff;
	result.fd3 = (mant1 >> 16) & 0xffff;
	result.fd4 = mant1 & 0xffff;

	return result;
}

static SF
soft_minus_internal(SF x1, SF x2)
{
	/* Negate x2 and add */
	DSIGNSET(x2, !DSIGN(x2));
	return soft_plus_internal(x1, x2);
}

/*
 * Convert a hex constant to floating point number.
 * Uses strtosf() which already handles hex float format via gdtoa.
 */
NODE *
fhexcon(char *s)
{
	NODE *p;
	SF sf;
	TWORD t;

	/* Determine the type from suffix (f/F/l/L) */
	t = DOUBLE;  /* default */
	for (char *c = s; *c; c++) {
		if (*c == 'f' || *c == 'F') {
			t = FLOAT;
			break;
		} else if (*c == 'l' || *c == 'L') {
			t = LDOUBLE;
			break;
		}
	}

	/* Parse the hex float using strtosf() */
	sf = strtosf(s, t);

	/* Create FCON node */
	p = block(FCON, NIL, NIL, t, 0, 0);
	p->n_dcon = sf;

	return p;
}

/*
 * Convert a floating-point constant to D-float and store it in a NODE.
 */
NODE *
floatcon(char *s)
{
	NODE *p;
	dword mant;
	SF fl, flexp, exp5;
	int exp, negexp, bexp;

	exp = 0;
	mant = 0;
#define ADDTO(sum, val) sum = sum * 10 + val - '0'
	for (; *s >= '0' && *s <= '9'; s++) {
		if (mant<MAXMANT)
			ADDTO(mant, *s);
		else
			exp++;
	}
	if (*s == '.') {
		for (s++; *s >= '0' && *s <= '9'; s++) {
			if (mant<MAXMANT) {
				ADDTO(mant, *s);
				exp--;
			}
		}
	}

	if ((*s == 'E') || (*s == 'e')) {
		int eexp = 0, sign = 0;
		s++;
		if (*s == '+')
			s++;
		else if (*s=='-')
			sign = 1, s++;

		for (; *s >= '0' && *s <= '9'; s++)
			ADDTO(eexp, *s);
		if (sign)
			eexp = -eexp;
		exp = exp + eexp;
	}

	negexp = 1;
	if (exp<0) {
		negexp = -1;
		exp = -exp;
	}


	flexp = soft_cast_internal(1, INT);
	exp5 = soft_cast_internal(5, INT);
	bexp = exp;
	fl = soft_cast_internal(mant, INT);

	for (; exp; exp >>= 1) {
		if (exp&01)
			flexp = soft_mul_internal(flexp, exp5);
		exp5 = soft_mul_internal(exp5, exp5);
	}
	if (negexp<0)
		fl = soft_div_internal(fl, flexp);
	else
		fl = soft_mul_internal(fl, flexp);

	DEXPSET(fl, DEXP(fl) + negexp*bexp);
	p = block(FCON, NIL, NIL, DOUBLE, 0, 0); /* XXX type */
	p->n_dcon = fl;
	return p;
}

/*
 * Wrapper functions to match the common interface defined in softfloat.h
 */
SFP
soft_cast(CONSZ v, TWORD t)
{
	static SF result;
	result = soft_cast_internal(v, t);
	return &result;
}

SF
soft_mul(SF x1, SF x2, TWORD t)
{
	return soft_mul_internal(x1, x2);
}

SF
soft_div(SF x1, SF x2, TWORD t)
{
	return soft_div_internal(x1, x2);
}

SF
soft_plus(SF x1, SF x2, TWORD t)
{
	return soft_plus_internal(x1, x2);
}

SF
soft_minus(SF x1, SF x2, TWORD t)
{
	return soft_minus_internal(x1, x2);
}

#else

/*
 * Use parametric floating-point representation, as used in the package gdtoa
 * published by David M. Gay and generally available as gdtoa.tgz at
 * http://www.netlib.org/fp/ ; see also strtodg.c introduction.
 *
 * Arithmetic characteristics are described in struct FPI (explained below);
 * the actual numbers are represented (stored) in struct SF.
 * Floating-point numbers have fpi->nbits bits.
 * These numbers are regarded as integers multiplied by 2^e
 * (i.e., 2 to the power of the exponent e), where e is stored in
 * *exp by strtodg.  The minimum and maximum exponent values fpi->emin
 * and fpi->emax for normalized floating-point numbers reflect this
 * arrangement.  For example, the IEEE 754 standard for binary arithmetic
 * specifies doubles (also known as binary64) as having 53 bits, with
 * normalized values of the form 1.xxxxx... times 2^(b-1023), with 52 bits
 * (the x's) and the biased exponent b represented explicitly;
 * b is an unsigned integer in the range 1 <= b <= 2046 for normalized
 * finite doubles, b = 0 for denormals, and b = 2047 for Infinities and NaNs.
 * To turn an IEEE double into the representation used here, we multiply
 * 1.xxxx... by 2^52 (to make it an integer) and reduce the exponent
 * e = (b-1023) by 52:
 *	fpi->emin = 1 - 1023 - 52
 *	fpi->emax = 1046 - 1023 - 52
 * For fpi_binary64 initialization, we actually write -53+1 rather than -52,
 * to emphasize that there are 53 bits including one implicit bit at the
 * left of the binary point.
 * Field fpi->rounding indicates the desired rounding direction, with
 * possible values
 *	FPI_Round_zero = toward 0,
 *	FPI_Round_near = unbiased rounding -- the IEEE default,
 *	FPI_Round_up = toward +Infinity, and
 *	FPI_Round_down = toward -Infinity
 *	FPI_Round_near_from0 = to nearest, ties always away from 0
 * given in pass1.h.
 *
 * Field fpi->sudden_underflow indicates whether computations should return
 * denormals or flush them to zero.  Normal floating-point numbers have
 * bit fpi->nbits in the significand on.  Denormals have it off, with
 * exponent = fpi->emin.
 *
 * Fields fpi->explicit_one, fpi->storage, and fpi->exp_bias are only
 * relevant when the numbers are finally packed into interchange format.
 * If bit 1 is the lowest in the significand, bit fpi->storage has the sign.
 *
 * Some architectures do not use IEEE arithmetic but can nevertheless use
 * the same parametrization. They should provide their own FPI objects.
 * Fields fpi->has_inf_nan and fpi->has_neg_zero cover the non-IEEE cases
 * of lacking respectively the use of infinities and NaN, and negative zero.
 * Field fpi->has_radix_16 is for architectures (IBM, DG Nova) with base 16.
 *
 * In this implementation, the bits are stored in one large integer
 * (unsigned long long); this limits the number of bits to 64.
 * Because of the storage representation (but not the implementation),
 * exponents are restricted to 15 bits.
 *
 * Furthermore, this implementation assumes that:
 *	- integers are (obviously) stored as 2's complement
 *	- long long is (at least) 64 bits
 *	- CONSZ is (at least) 64 bits
 * There are possible issues if int is 16 bits, with divisions.
 *
 * As a result, this is also a slightly restricted software implementation of
 * IEEE 754:1985 standard. Missing parts, useless in a compiler, are:
 * - no soft_unpack(), to convert from external binary{32,64} formats
 * - no precision control (not required, dropped in :2008)
 * - no soft_sqrt() operation
 * - no soft_remainder() operation
 * - no soft_rint(), _ceil, or _floor rounding operations
 * - no binary-to-decimal conversion (can use D. Gay's dgtoa.c)
 * - signaling NaNs (SF_NoNumber; should cause SFEXCP_Invalid on every op)
 * - XXX fenv-support is pending
 * - no soft_scalb(), _logb, or _nextafter optional functions
 * It should be easy to expand it into a complete implementation.
 *
 * The operations are rounded according to the indicated type.
 * If because of FLT_EVAL_METHOD, the precision has to be greater,
 * this should be handled by the calling code.
 */

/*
 * API restrictions:
 *	- type information should be between FLOAT and LDOUBLE
 */

#ifndef Long
#define Long int
#endif
#ifndef ULong
typedef unsigned Long ULong;
#endif
#ifndef ULLong
typedef unsigned long long ULLong;
#endif

#define ONEZEROES(n)	(1ull << (n))
#define ONES(n) 	(ONEZEROES(n) | (ONEZEROES(n)-1))

#define WORKBITS	64
#define NORMALMANT	ONEZEROES(WORKBITS-1)

#define SFNORMALIZE(sf)					\
	while ((sf).significand < NORMALMANT)		\
		(sf).significand <<= 1, (sf).exponent--;\
	if (((sf).kind & SF_kmask) == SF_Denormal)	\
		(sf).kind -= SF_Denormal - SF_Normal;

#define SFNEG(sf)	((sf).kind ^= SF_Neg, sf)
#define SFCOPYSIGN(sf, src)	\
	((sf).kind ^= ((sf).kind & SF_Neg) ^ ((src).kind & SF_Neg), (sf))
#define SFISZ(sf)	(((sf).kind & SF_kmask) == SF_Zero)
#define SFISINF(sf)	(((sf).kind & SF_kmask) == SF_Infinite)
#define SFISNAN(sf)	(((sf).kind & SF_kmask) >= SF_NaN)

typedef struct DULLong {
	ULLong hi, lo;
} DULLong;

static DULLong rshiftdro(ULLong, ULLong, int);
static DULLong lshiftd(ULLong, ULLong, int);
static SF sfround(SF, ULLong, TWORD);
#define SFROUND(sf,t)	(sfround(sf, 0, t))
static SF sfadd(SF, SF, TWORD);
static SF sfsub(SF, SF, TWORD);

#ifndef NO_COMPLEX
static SF finitemma(SF, SF, int, SF, SF, ULLong *);
static SF sfddiv(SF, ULLong, SF, ULLong, TWORD);
#endif

extern int strtodg (const char*, char**, FPI*, Long*, ULong*);

/* IEEE binary formats, and their interchange format encodings */
#ifdef USE_IEEEFP_16
FPI fpi_binary16 = { 11, 1-15-11+1,
                        30-15-11+1, 1, 0,
        0, 1, 1, 0,  16,   15+11-1 };
#endif
#ifdef USE_IEEEFP_128
FPI fpi_binary128 = { 113,   1-16383-113+1,
                         32766-16383-113+1, 1, 0,
        0, 1, 1, 0,   128,     16383+113-1 };

/* VAX G-float - 64-bit, extended range compared to D-float */
/* sign(1) + exp(11, excess-1024) + frac(52, hidden) */
/* Same mantissa precision as IEEE binary64 but different format */
/* No INF/NaN, has negative zero, uses VAX rounding (ties away from zero) */
FPI fpi_vax_g = { 53,   1-1024-53+1,
                        2046-1024-53+1, FPI_Round_near_from0, 0,
        0, 0, 1, 0,   64,     1024+53-1 };

/* VAX H-float - 128-bit, quad precision */
/* sign(1) + exp(15, excess-16384) + frac(112, hidden) */
/* Extended range and precision compared to G-float */
/* No INF/NaN, has negative zero, uses VAX rounding */
FPI fpi_vax_h = { 113,   1-16384-113+1,
                         32766-16384-113+1, FPI_Round_near_from0, 0,
        0, 0, 1, 0,   128,     16384+113-1 };

/*
 * FP8 formats - 8-bit floating point for ML/AI accelerators
 * Defined in "FP8 Formats for Deep Learning" (NVIDIA/Graphcore, 2022)
 * Used in NVIDIA H100, AMD MI300, and other AI accelerators
 */

/* FP8 E4M3 - Optimized for training, no infinities */
/* sign(1) + exp(4, bias=7) + mantissa(3, explicit leading bit) */
/* Range: ~2^-6 to 448 (approx 2^8.8) */
/* NOTE: Uses explicit leading bit (explicit_one=1), not hidden bit */
/* Max exponent value (15) represents NaN, not infinity */
/* Common in ML training workloads (weights, gradients) */
FPI fpi_fp8_e4m3 = { 3,  1-7-3+1,
                          14-7-3+1, 1, 0,
        1, 0, 1, 0,   8,      7+3-1 };

/* FP8 E5M2 - Optimized for inference, has infinities */
/* sign(1) + exp(5, bias=15) + mantissa(2, explicit leading bit) */
/* Range: ~2^-14 to 57344 (2^15 × 1.75) */
/* NOTE: Uses explicit leading bit (explicit_one=1), not hidden bit */
/* Exponent=31, mantissa=0 → infinity; mantissa≠0 → NaN (IEEE-like) */
/* Common in ML inference workloads (activations) */
FPI fpi_fp8_e5m2 = { 2,  1-15-2+1,
                          30-15-2+1, 1, 0,
        1, 1, 1, 0,   8,     15+2-1 };

#ifdef USE_IEEEFP_32
#define FPI_FLOAT	fpi_binary32
FPI fpi_binary32 = { 24,  1-127-24+1,
                        254-127-24+1, 1, 0,
        0, 1, 1, 0,  32,    127+24-1 };
#elif defined(USE_IEEEFP_16)
#define FPI_FLOAT	fpi_binary16
#elif defined(USE_MSBFP_32)
#define FPI_FLOAT	fpi_msbf32
#elif defined(USE_IBMFP_32)
#define FPI_FLOAT	fpi_ibm32
#elif defined(USE_BFLOAT16)
#define FPI_FLOAT	fpi_bfloat16
#elif defined(USE_VAXFP_F)
#define FPI_FLOAT	fpi_vax_f
#elif defined(USE_PXR24)
#define FPI_FLOAT	fpi_pxr24
#elif defined(USE_AMD24)
#define FPI_FLOAT	fpi_amd24
#elif defined(USE_DECIMAL32)
#define FPI_FLOAT	fpi_decimal32
#elif defined(USE_AMD_3DNOW)
#define FPI_FLOAT	fpi_amd_3dnow
#elif defined(USE_FLEXPOINT16)
#define FPI_FLOAT	fpi_flexpoint16
#elif defined(USE_HEXAGON_HALF)
#define FPI_FLOAT	fpi_hexagon_half
#elif defined(USE_ARM_ALT_HALF)
#define FPI_FLOAT	fpi_arm_alt_half
#else
#error need float definition
#endif
#ifdef USE_IEEEFP_64
#define FPI_DOUBLE	fpi_binary64
FPI fpi_binary64 = { 53,   1-1023-53+1,
                        2046-1023-53+1, 1, 0,
        0, 1, 1, 0,  64,     1023+53-1 };
#elif defined(USE_MSBFP_64)
#define FPI_DOUBLE	fpi_msbf64
#elif defined(USE_IBMFP_64)
#define FPI_DOUBLE	fpi_ibm64
#elif defined(USE_CRAYFP_64)
#define FPI_DOUBLE	fpi_cray64
#elif defined(USE_VAXFP_D)
#define FPI_DOUBLE	fpi_vax_d
#elif defined(USE_VAXFP_G)
#define FPI_DOUBLE	fpi_vax_g
#elif defined(USE_DECIMAL64)
#define FPI_DOUBLE	fpi_decimal64
#else
#error need double definition
#endif
#ifdef USE_IEEEFP_X80
#define FPI_LDOUBLE	fpi_binaryx80
/* IEEE double extended in its usual form, for example Intel 387 */
FPI fpi_binaryx80 = { 64,   1-16383-64+1,
                        32766-16383-64+1, 1, 0,
        1, 1, 1, 0,   80,     16383+64-1 };
#define FPI_LDOUBLE_NAN		{ 0, 0, 0, 0xc000, 0x7fff }
#define FPI_LDOUBLE_INF		{ 0, 0, 0, 0x8000, 0x7fff }
#define FPI_LDOUBLE_ZERO	{ 0, 0, 0, 0, 0 }
#define	FPI_LDOUBLE_ISZ(sf)	(sf.fp[0] == 0 && sf.fp[1] == 0 && \
	sf.fp[2] == 0 && sf.fp[3] == 0 && (sf.fp[4] & 0x7fff) == 0)
#define	FPI_LDOUBLE_NEG(sf)	sf.fp[4] ^= 0x8000
#elif defined(USE_IEEEFP_128)
#define FPI_LDOUBLE	fpi_binary128
#elif defined(USE_IBMFP_128)
#define FPI_LDOUBLE	fpi_ibm128
#elif defined(USE_CRAYFP_64)
#define FPI_LDOUBLE	fpi_cray64
#elif defined(USE_VAXFP_H)
#define FPI_LDOUBLE	fpi_vax_h
#elif defined(USE_DECIMAL128)
#define FPI_LDOUBLE	fpi_decimal128
#elif defined(USE_DOUBLE_DOUBLE)
#define FPI_LDOUBLE	fpi_double_double
#elif defined(USE_M68K_EXT96)
#define FPI_LDOUBLE	fpi_m68k_ext96
#elif defined(USE_HPPA_QUAD)
#define FPI_LDOUBLE	fpi_hppa_quad
#else
#error need long double definition
#endif

/*
 * Microsoft Binary Format (MBF) - Used in Microsoft BASIC and early MS products
 * MBF uses bias of 129, no implicit bit, no INF/NaN support
 */
#ifdef USE_MSBFP_32
#define FPI_MSBF_SINGLE	fpi_msbf32
/* MBF Single: 1 sign + 8 exponent (bias 129) + 23 mantissa */
FPI fpi_msbf32 = { 24, 1-129-24+1,
                      254-129-24+1, 1, 0,
        0, 0, 0, 0,  32,    129 };
#endif
#ifdef USE_MSBFP_64
#define FPI_MSBF_DOUBLE	fpi_msbf64
/* MBF Double: 1 sign + 8 exponent (bias 129) + 55 mantissa */
FPI fpi_msbf64 = { 56, 1-129-56+1,
                      254-129-56+1, 1, 0,
        0, 0, 0, 0,  64,    129 };
#endif

/*
 * IBM Hexadecimal Floating Point - Used on IBM System/360, 370, and successors
 * Radix 16 (hexadecimal), 7-bit exponent with bias 64
 * Has negative zero but no INF/NaN
 */
#ifdef USE_IBMFP_32
#define FPI_IBM_SINGLE	fpi_ibm32
/* IBM Single: 1 sign + 7 exponent (bias 64) + 24 mantissa (6 hex digits) */
FPI fpi_ibm32 = { 24, 0-64,
                     127-64, 1, 0,
        0, 0, 1, 1,  32,    64 };
#endif
#ifdef USE_IBMFP_64
#define FPI_IBM_DOUBLE	fpi_ibm64
/* IBM Double: 1 sign + 7 exponent (bias 64) + 56 mantissa (14 hex digits) */
FPI fpi_ibm64 = { 56, 0-64,
                     127-64, 1, 0,
        0, 0, 1, 1,  64,    64 };
#endif
#ifdef USE_IBMFP_128
#define FPI_IBM_EXTENDED fpi_ibm128
/* IBM Extended: 1 sign + 7 exponent (bias 64) + 112 mantissa (28 hex digits) */
FPI fpi_ibm128 = { 112, 0-64,
                      127-64, 1, 0,
        0, 0, 1, 1,  128,   64 };
#endif

/*
 * Cray Floating Point - Used on Cray supercomputers (Cray-1, X-MP, Y-MP, etc.)
 * 64-bit format: 1 sign + 15 exponent (bias 16384) + 48 mantissa
 * Has negative zero but no INF/NaN, no denormals
 */
#ifdef USE_CRAYFP_64
#define FPI_CRAY_DOUBLE	fpi_cray64
FPI fpi_cray64 = { 48, 1-16384-48+1,
                      65534-16384-48+1, 1, 0,
        0, 0, 1, 0,  64,    16384 };
#endif

/*
 * bfloat16 (Brain Floating Point) - Google, Intel, ARM
 * Used extensively in machine learning and AI
 * 1 sign + 8 exponent (bias 127, same as float32) + 7 mantissa
 * Truncated version of IEEE 754 float32, same dynamic range
 */
#ifdef USE_BFLOAT16
#define FPI_BFLOAT16	fpi_bfloat16
FPI fpi_bfloat16 = { 8, 1-127-8+1,
                        254-127-8+1, 1, 0,
        0, 1, 1, 0,  16,    127+8-1 };
#endif

/*
 * TensorFloat-32 (TF32) - NVIDIA Ampere and later GPUs
 * 1 sign + 8 exponent (bias 127) + 10 mantissa
 * Intermediate format between float16 and float32 for AI training
 */
#ifdef USE_TENSORFLOAT32
#define FPI_TF32	fpi_tf32
FPI fpi_tf32 = { 11, 1-127-11+1,
                    254-127-11+1, 1, 0,
        0, 1, 1, 0,  19,    127+11-1 };
#endif

/*
 * FP8 E4M3 - 8-bit float used in NVIDIA Hopper, AMD MI300, Intel Habana
 * 1 sign + 4 exponent (bias 7) + 3 mantissa
 * Optimized for AI training with higher precision, smaller range
 */
#ifdef USE_FP8_E4M3
#define FPI_FP8_E4M3	fpi_fp8_e4m3
FPI fpi_fp8_e4m3 = { 4, 1-7-4+1,
                       14-7-4+1, 1, 0,
        0, 1, 0, 0,  8,     7+4-1 };
#endif

/*
 * FP8 E5M2 - 8-bit float used in NVIDIA Hopper, AMD MI300, Intel Habana
 * 1 sign + 5 exponent (bias 15) + 2 mantissa
 * Optimized for AI inference with larger range, lower precision
 */
#ifdef USE_FP8_E5M2
#define FPI_FP8_E5M2	fpi_fp8_e5m2
FPI fpi_fp8_e5m2 = { 3, 1-15-3+1,
                       30-15-3+1, 1, 0,
        0, 1, 1, 0,  8,     15+3-1 };
#endif

/*
 * DEC VAX F_floating - 32-bit
 * Used on VAX computers (1970s-1990s)
 * 1 sign + 8 exponent (bias 128) + 23 mantissa
 * No INF/NaN, hidden bit, different byte order than IEEE
 */
#ifdef USE_VAXFP_F
#define FPI_VAX_F	fpi_vax_f
FPI fpi_vax_f = { 24, 1-128-24+1,
                     254-128-24+1, 1, 0,
        0, 0, 0, 1,  32,    128 };
#endif

/*
 * DEC VAX D_floating - 64-bit
 * 1 sign + 8 exponent (bias 128) + 55 mantissa
 */
#ifdef USE_VAXFP_D
#define FPI_VAX_D	fpi_vax_d
FPI fpi_vax_d = { 56, 1-128-56+1,
                     254-128-56+1, 1, 0,
        0, 0, 0, 1,  64,    128 };
#endif

/*
 * DEC VAX G_floating - 64-bit
 * 1 sign + 11 exponent (bias 1024) + 52 mantissa
 * Similar range to IEEE double but different encoding
 */
#ifdef USE_VAXFP_G
#define FPI_VAX_G	fpi_vax_g
FPI fpi_vax_g = { 53, 1-1024-53+1,
                     2046-1024-53+1, 1, 0,
        0, 0, 0, 1,  64,    1024 };
#endif

/*
 * DEC VAX H_floating - 128-bit
 * 1 sign + 15 exponent (bias 16384) + 112 mantissa
 */
#ifdef USE_VAXFP_H
#define FPI_VAX_H	fpi_vax_h
FPI fpi_vax_h = { 113, 1-16384-113+1,
                      32766-16384-113+1, 1, 0,
        0, 0, 0, 1,  128,   16384 };
#endif

/*
 * ARM Alternative Half Precision - 16-bit
 * 1 sign + 5 exponent (bias 15) + 10 mantissa
 * No INF/NaN support (all exponent bits set = normal number)
 */
#ifdef USE_ARM_ALT_HALF
#define FPI_ARM_ALT_HALF	fpi_arm_alt_half
FPI fpi_arm_alt_half = { 11, 1-15-11+1,
                            31-15-11+1, 1, 0,
        0, 1, 0, 1,  16,    15+11-1 };
#endif

/*
 * Posit formats - Modern alternative to IEEE 754 (John Gustafson)
 * Variable exponent and fraction based on regime encoding
 * No INF (all bits set = NaR - Not a Real), better accuracy distribution
 * Note: Simplified FPI representation - actual posit arithmetic differs significantly
 */
#ifdef USE_POSIT8
#define FPI_POSIT8	fpi_posit8
/* Posit<8,0>: ~6-bit effective precision, variable exponent */
FPI fpi_posit8 = { 6, -24,
                     24, 1, 0,
        0, 0, 0, 1,  8,     0 };
#endif

#ifdef USE_POSIT16
#define FPI_POSIT16	fpi_posit16
/* Posit<16,1>: ~13-bit effective precision, variable exponent */
FPI fpi_posit16 = { 13, -120,
                       120, 1, 0,
        0, 0, 0, 1,  16,    0 };
#endif

#ifdef USE_POSIT32
#define FPI_POSIT32	fpi_posit32
/* Posit<32,2>: ~28-bit effective precision, variable exponent */
FPI fpi_posit32 = { 28, -504,
                       504, 1, 0,
        0, 0, 0, 1,  32,    0 };
#endif

#ifdef USE_POSIT64
#define FPI_POSIT64	fpi_posit64
/* Posit<64,3>: ~59-bit effective precision, variable exponent */
FPI fpi_posit64 = { 59, -2040,
                       2040, 1, 0,
        0, 0, 0, 1,  64,    0 };
#endif

/*
 * Pixar PXR24 - 24-bit floating point used in image processing
 * 1 sign + 8 exponent (bias 127) + 15 mantissa
 * Used in OpenEXR and film production
 */
#ifdef USE_PXR24
#define FPI_PXR24	fpi_pxr24
FPI fpi_pxr24 = { 16, 1-127-16+1,
                     254-127-16+1, 1, 0,
        0, 1, 1, 0,  24,    127+16-1 };
#endif

/*
 * AMD 24-bit float (historic) - Used in some AMD 3D graphics
 * 1 sign + 8 exponent (bias 127) + 15 mantissa
 */
#ifdef USE_AMD24
#define FPI_AMD24	fpi_amd24
FPI fpi_amd24 = { 16, 1-127-16+1,
                     254-127-16+1, 1, 0,
        0, 1, 1, 0,  24,    127+16-1 };
#endif

/*
 * Minifloat formats - Used in research, education, and specialized hardware
 */

/* E3M2 - 6-bit float: 1 sign + 3 exponent + 2 mantissa */
#ifdef USE_MINIFLOAT_E3M2
#define FPI_MINI_E3M2	fpi_mini_e3m2
FPI fpi_mini_e3m2 = { 3, 1-3-3+1,
                         6-3-3+1, 1, 0,
        0, 1, 0, 0,  6,     3+3-1 };
#endif

/* E2M3 - 6-bit float: 1 sign + 2 exponent + 3 mantissa */
#ifdef USE_MINIFLOAT_E2M3
#define FPI_MINI_E2M3	fpi_mini_e2m3
FPI fpi_mini_e2m3 = { 4, 1-1-4+1,
                         2-1-4+1, 1, 0,
        0, 1, 0, 0,  6,     1+4-1 };
#endif

/*
 * IEEE 754-2008 Decimal Floating Point - DPD (Densely Packed Decimal) encoding
 * Used in financial applications, scientific computing requiring exact decimal
 * Note: Radix 10, not binary - requires special decimal arithmetic
 */
#ifdef USE_DECIMAL32
#define FPI_DECIMAL32	fpi_decimal32
/* decimal32: 7 decimal digits, exponent range -95 to +96 */
FPI fpi_decimal32 = { 24, -95,
                         96, 1, 0,
        0, 0, 1, 1,  32,    101 };
#endif

#ifdef USE_DECIMAL64
#define FPI_DECIMAL64	fpi_decimal64
/* decimal64: 16 decimal digits, exponent range -383 to +384 */
FPI fpi_decimal64 = { 54, -383,
                         384, 1, 0,
        0, 0, 1, 1,  64,    398 };
#endif

#ifdef USE_DECIMAL128
#define FPI_DECIMAL128	fpi_decimal128
/* decimal128: 34 decimal digits, exponent range -6143 to +6144 */
FPI fpi_decimal128 = { 114, -6143,
                          6144, 1, 0,
        0, 0, 1, 1,  128,   6176 };
#endif

/*
 * Double-Double (DD) - PowerPC, SPARC, and high-precision computing
 * Two IEEE 754 doubles combined for ~106 bits of precision
 * Used when hardware quad precision unavailable
 * Note: Non-standard exponent range, variable precision
 */
#ifdef USE_DOUBLE_DOUBLE
#define FPI_DOUBLE_DOUBLE	fpi_double_double
/* ~106 mantissa bits, same exponent range as double but extended */
FPI fpi_double_double = { 106, 1-1023-106+1,
                             2046-1023-106+1, 1, 0,
        0, 1, 1, 1,  128,   1023+106-1 };
#endif

/*
 * OCP (Open Compute Project) FP8 Format Variants
 * Standardized ML formats with different special value handling
 */

/* OCP E4M3FN - Finite only, no INF, NaN encoding */
#ifdef USE_OCP_E4M3FN
#define FPI_OCP_E4M3FN	fpi_ocp_e4m3fn
FPI fpi_ocp_e4m3fn = { 4, 1-7-4+1,
                          14-7-4+1, 1, 0,
        0, 1, 0, 0,  8,     7+4-1 };
#endif

/* OCP E5M2FNUZ - Finite, no negative zero, no INF */
#ifdef USE_OCP_E5M2FNUZ
#define FPI_OCP_E5M2FNUZ	fpi_ocp_e5m2fnuz
FPI fpi_ocp_e5m2fnuz = { 3, 1-15-3+1,
                            30-15-3+1, 1, 0,
        0, 1, 0, 0,  8,     15+3-1 };
#endif

/* OCP E4M3FNUZ - Finite, no negative zero */
#ifdef USE_OCP_E4M3FNUZ
#define FPI_OCP_E4M3FNUZ	fpi_ocp_e4m3fnuz
FPI fpi_ocp_e4m3fnuz = { 4, 1-7-4+1,
                            14-7-4+1, 1, 0,
        0, 1, 0, 0,  8,     7+4-1 };
#endif

/*
 * Motorola 68881/68882 Extended Precision
 * 96-bit packed format used in 68k FPU
 * Same as x87 extended but stored in 96 bits instead of 80
 */
#ifdef USE_M68K_EXT96
#define FPI_M68K_EXT96	fpi_m68k_ext96
FPI fpi_m68k_ext96 = { 64, 1-16383-64+1,
                          32766-16383-64+1, 1, 0,
        1, 1, 1, 0,  96,    16383+64-1 };
#endif

/*
 * HP-PA (Precision Architecture) Quad Precision
 * 128-bit format from HP workstations (HP 9000 series)
 * Similar to IEEE quad but different encoding
 */
#ifdef USE_HPPA_QUAD
#define FPI_HPPA_QUAD	fpi_hppa_quad
FPI fpi_hppa_quad = { 113, 1-16383-113+1,
                         32766-16383-113+1, 1, 0,
        0, 1, 1, 1,  128,   16383+113-1 };
#endif

/*
 * Intel Flexpoint - Earlier Intel format for embedded/DSP
 * 16-bit with shared exponent across multiple values (block floating point)
 * Simplified representation: individual format
 */
#ifdef USE_FLEXPOINT16
#define FPI_FLEXPOINT16	fpi_flexpoint16
FPI fpi_flexpoint16 = { 10, 1-15-10+1,
                           30-15-10+1, 1, 0,
        0, 1, 0, 0,  16,    15+10-1 };
#endif

/*
 * Unums Type I - John Gustafson's earlier work (before Posits)
 * Variable-size unum with explicit uncertainty bit
 * Note: Simplified FPI representation - actual unums are more complex
 */
#ifdef USE_UNUM16
#define FPI_UNUM16	fpi_unum16
/* Simplified 16-bit unum representation */
FPI fpi_unum16 = { 10, -120,
                      120, 1, 0,
        0, 0, 0, 1,  16,    0 };
#endif

#ifdef USE_UNUM32
#define FPI_UNUM32	fpi_unum32
/* Simplified 32-bit unum representation */
FPI fpi_unum32 = { 24, -504,
                      504, 1, 0,
        0, 0, 0, 1,  32,    0 };
#endif

/*
 * AMD 3DNow! Float - Historic AMD multimedia extension
 * Standard IEEE 32-bit, included for completeness
 */
#ifdef USE_AMD_3DNOW
#define FPI_AMD_3DNOW	fpi_amd_3dnow
FPI fpi_amd_3dnow = { 24, 1-127-24+1,
                         254-127-24+1, 1, 0,
        0, 1, 1, 0,  32,    127+24-1 };
#endif

/*
 * NVIDIA MX formats - Mixed precision training formats
 */

/* MX9 E4M3 - NVIDIA Hopper mixed precision */
#ifdef USE_NVIDIA_MX9_E4M3
#define FPI_NVIDIA_MX9_E4M3	fpi_nvidia_mx9_e4m3
FPI fpi_nvidia_mx9_e4m3 = { 4, 1-7-4+1,
                               14-7-4+1, 1, 0,
        0, 1, 0, 0,  9,     7+4-1 };
#endif

/* MX9 E5M2 - NVIDIA Hopper mixed precision */
#ifdef USE_NVIDIA_MX9_E5M2
#define FPI_NVIDIA_MX9_E5M2	fpi_nvidia_mx9_e5m2
FPI fpi_nvidia_mx9_e5m2 = { 3, 1-15-3+1,
                               30-15-3+1, 1, 0,
        0, 1, 1, 0,  9,     15+3-1 };
#endif

/*
 * Google TPU formats - Custom formats for Tensor Processing Units
 */

/* BF8 - Google Brain 8-bit float variant */
#ifdef USE_GOOGLE_BF8
#define FPI_GOOGLE_BF8	fpi_google_bf8
FPI fpi_google_bf8 = { 3, 1-15-3+1,
                          30-15-3+1, 1, 0,
        0, 1, 0, 0,  8,     15+3-1 };
#endif

/*
 * Qualcomm Hexagon DSP formats - Signal processing optimized
 * Half precision variant for DSP operations
 */
#ifdef USE_HEXAGON_HALF
#define FPI_HEXAGON_HALF	fpi_hexagon_half
FPI fpi_hexagon_half = { 11, 1-15-11+1,
                            30-15-11+1, 1, 0,
        0, 1, 1, 0,  16,    15+11-1 };
#endif

/*
 * Logarithmic Number System - Alternative to floating point
 * Value = sign * 2^exponent (no mantissa)
 * Efficient for multiplication/division, harder for add/subtract
 */
#ifdef USE_LNS16
#define FPI_LNS16	fpi_lns16
/* 16-bit LNS: 1 sign + 15 exponent */
FPI fpi_lns16 = { 1, -16384,
                    16383, 1, 0,
        0, 0, 0, 1,  16,    0 };
#endif

#ifdef USE_LNS32
#define FPI_LNS32	fpi_lns32
/* 32-bit LNS: 1 sign + 31 exponent */
FPI fpi_lns32 = { 1, -1073741824,
                    1073741823, 1, 0,
        0, 0, 0, 1,  32,    0 };
#endif

FPI * fpis[3] = {
	&FPI_FLOAT,
	&FPI_DOUBLE,
	&FPI_LDOUBLE
};

/*
 * Format-specific helper functions for partially implemented formats
 */

#ifdef USE_VAXFP_F
/*
 * VAX F_floating byte order conversion
 * VAX format: [exp+sign(8)][frac_hi(8)][frac_mid(8)][frac_lo(8)]
 * Memory layout is different from IEEE
 */
static inline uint32_t vax_f_to_ieee32(uint32_t vax) {
	/* VAX has exponent bias 128, IEEE has 127 */
	/* TODO: Full conversion including byte swapping */
	return vax; /* Placeholder - needs implementation */
}

static inline uint32_t ieee32_to_vax_f(uint32_t ieee) {
	/* TODO: Full conversion including byte swapping */
	return ieee; /* Placeholder - needs implementation */
}
#endif

#ifdef USE_VAXFP_D
/* VAX D_floating conversion helpers */
static inline uint64_t vax_d_to_ieee64(uint64_t vax) {
	return vax; /* Placeholder - needs implementation */
}

static inline uint64_t ieee64_to_vax_d(uint64_t ieee) {
	return ieee; /* Placeholder - needs implementation */
}
#endif

#ifdef USE_OCP_E4M3FN
/*
 * OCP E4M3FN special value handling
 * This format has no INF - max exponent values are used for NaN only
 * All values are finite numbers except NaN (0xFF for positive, 0xFF for negative)
 */
static inline int ocp_e4m3fn_is_nan(uint8_t val) {
	return (val & 0x7F) == 0x7F; /* All exponent bits set */
}

static inline uint8_t ocp_e4m3fn_make_nan(int sign) {
	return sign ? 0xFF : 0x7F; /* NaN encoding */
}
#endif

#ifdef USE_OCP_E5M2FNUZ
/*
 * OCP E5M2FNUZ special value handling
 * Finite only, No negative zero, Unsigned Zero
 * No INF, no negative zero (-0 is treated as +0)
 */
static inline int ocp_e5m2fnuz_is_nan(uint8_t val) {
	return (val & 0x7F) == 0x7C; /* Special NaN encoding */
}

static inline uint8_t ocp_e5m2fnuz_make_nan(void) {
	return 0x7C; /* NaN encoding (no sign distinction) */
}

static inline uint8_t ocp_e5m2fnuz_normalize_zero(uint8_t val) {
	/* Convert -0 to +0 */
	if ((val & 0x7F) == 0)
		return 0; /* Always positive zero */
	return val;
}
#endif

#ifdef USE_OCP_E4M3FNUZ
/* OCP E4M3FNUZ special value handling */
static inline int ocp_e4m3fnuz_is_nan(uint8_t val) {
	return (val & 0x7F) == 0x7F;
}

static inline uint8_t ocp_e4m3fnuz_make_nan(void) {
	return 0x7F; /* NaN encoding (no sign distinction) */
}

static inline uint8_t ocp_e4m3fnuz_normalize_zero(uint8_t val) {
	if ((val & 0x7F) == 0)
		return 0; /* Always positive zero */
	return val;
}
#endif

#ifdef USE_DOUBLE_DOUBLE
/*
 * Double-Double arithmetic helpers
 * Double-double uses two IEEE doubles (hi, lo) where:
 * - hi contains the high-order bits (main value)
 * - lo contains the low-order bits (correction term)
 * - Value = hi + lo, where |lo| <= 0.5 * ulp(hi)
 *
 * This provides ~106 bits of precision using two 53-bit mantissas
 */

typedef struct {
	double hi;
	double lo;
} dd_float;

/* Quick two-sum: Assumes |a| >= |b| */
static inline dd_float dd_quick_two_sum(double a, double b) {
	dd_float result;
	double s = a + b;
	result.hi = s;
	result.lo = b - (s - a);
	return result;
}

/* Two-sum: No assumption on relative magnitude */
static inline dd_float dd_two_sum(double a, double b) {
	double s = a + b;
	double v = s - a;
	dd_float result;
	result.hi = s;
	result.lo = (a - (s - v)) + (b - v);
	return result;
}

/* Two-product: Exact product of two doubles */
static inline dd_float dd_two_prod(double a, double b) {
	dd_float result;
	double p = a * b;
	result.hi = p;
	/* Assuming FMA (fused multiply-add) available, otherwise needs Split */
#ifdef FP_FAST_FMA
	result.lo = fma(a, b, -p);
#else
	/* Dekker's algorithm for exact product (needs splitting) */
	/* TODO: Implement Split algorithm */
	result.lo = 0; /* Placeholder */
#endif
	return result;
}

/* Add: (a_hi, a_lo) + (b_hi, b_lo) */
static inline dd_float dd_add(dd_float a, dd_float b) {
	dd_float s = dd_two_sum(a.hi, b.hi);
	dd_float t = dd_two_sum(a.lo, b.lo);
	s.lo += t.hi;
	s = dd_quick_two_sum(s.hi, s.lo);
	s.lo += t.lo;
	s = dd_quick_two_sum(s.hi, s.lo);
	return s;
}

/* Multiply: (a_hi, a_lo) * (b_hi, b_lo) */
static inline dd_float dd_mul(dd_float a, dd_float b) {
	dd_float p = dd_two_prod(a.hi, b.hi);
	p.lo += a.hi * b.lo + a.lo * b.hi;
	p = dd_quick_two_sum(p.hi, p.lo);
	return p;
}

/* Negate */
static inline dd_float dd_neg(dd_float a) {
	dd_float result;
	result.hi = -a.hi;
	result.lo = -a.lo;
	return result;
}

/* Subtract */
static inline dd_float dd_sub(dd_float a, dd_float b) {
	return dd_add(a, dd_neg(b));
}
#endif

/*
 * Constant rounding control mode (cf. TS 18661 clause 11).
 * Default is to have no effect. Set through #pragma STDC FENV_ROUND
 */
int sf_constrounding = FPI_RoundNotSet;
/*
 * Exceptions raised by functions which do not return a SF; behave like errno
 */
int sf_exceptions = 0;

/*
 * Returns a (signed) zero softfloat.
 * "kind" (sign and exceptions) is merged into.
 */
SF
zerosf(int kind)
{
	SF rv;

	/*static_*/assert(SF_Zero == 0);
#if 0
	rv.kind &= ~SF_kmask;
#else
	rv.kind = SF_Zero | (kind & ~SF_kmask);
#endif
	rv.significand = rv.exponent = rv.kind = 0;
	return rv;
}

/*
 * Returns a (signed) infinite softfloat.
 * If the target does not support infinites, the "infinite" will propagate
 * until sfround() below, where it will be transformed to DBL_MAX and exception
 */
SF
infsf(int kind)
{
	SF rv;

	rv.kind = SF_Infinite | (kind & ~SF_kmask);
	rv.significand = NORMALMANT;
	rv.exponent = fpis[LDOUBLE-FLOAT]->emax + 1;
	return rv;
}

/*
 * Returns a NaN softfloat.
 */
SF
nansf(int kind)
{
	SF rv;

	rv.kind = SF_NaN | (kind & ~SF_kmask);
	rv.significand = 3 * ONEZEROES(WORKBITS-2);
	rv.exponent = fpis[LDOUBLE-FLOAT]->emax + 1;
	return rv;
}

/*
 * Returns a (signed) huge, perhaps infinite, softfloat.
 */
SF
hugesf(int kind, TWORD t)
{
	FPI *fpi = fpis[t-FLOAT];
	SF rv;

	if (fpi->has_inf_nan)
		return infsf(kind);
	rv.kind = (kind & ~SF_kmask) | SF_Normal;
	rv.significand = ONES(fpi->nbits - 1);
	rv.exponent = fpi->emax;
	return rv;
}

/*
 * The algorithms for the operations were derived from John Hauser's 
 * SoftFloat package (which is also used in libpcc/libsoftfloat.) 
 *
 * Results are rounded to odd ("jamming" in John Hauser's code)
 * in order to avoid double rounding errors. See
 *	Sylvie Boldo, Guillaume Melquiond, "When double rounding is odd",
 *	Research report No 2004-48, Normale Sup', Lyon, Nov 2004;
 *	http://www.ens-lyon.fr/LIP/Pub/Rapports/RR/RR2004/RR2004-48.pdf
 *	17th IMACS World Congress, Paris, Jul 2005; pp.11;
 *	http://hal.inria.fr/inria-00070603/document
 */

/*
 * Shift right double-sized, rounding to odd.
 */
static DULLong
rshiftdro(ULLong a, ULLong b, int count)
{
	struct DULLong z;
	assert(count >= 0);
	if ((unsigned)count >= 2*WORKBITS) {
		z.hi = 0;
		z.lo = (a != 0) | (b != 0);
	}
	else if (count >= WORKBITS) {
		z.hi = 0;
		z.lo = (a >> (count - WORKBITS)) | (b != 0);
	}
	else {
		z.hi = a >> count;
		z.lo = (a << (WORKBITS - count)) | (b >> count) | (b != 0);
	}
	return z;
}

/*
 * Shift left double-sized.
 */
static DULLong
lshiftd(ULLong a, ULLong b, int count)
{
	struct DULLong z;
	assert((unsigned)count < WORKBITS);
	z.hi = a << count;
	z.lo = (a >> (WORKBITS-count)) | (b << count);
	return z;
}

/*
 * Full-range multiply, result to double-sized.
 */
static DULLong
muld(ULLong a, ULLong b)
{
	struct DULLong z;
	ULong ahi, bhi;
	ULLong mid;

#define HALFWORKBITS	(WORKBITS/2)
#define LOWHALFMASK	ONES(WORKBITS-HALFWORKBITS)
	ahi = (ULong)(a >> HALFWORKBITS);
	a &= LOWHALFMASK;
	bhi = (ULong)(b >> HALFWORKBITS);
	b &= LOWHALFMASK;
	z.lo = a * b;
	a *= bhi;
	b *= ahi;
	mid = (z.lo >> HALFWORKBITS) + (a & LOWHALFMASK) + (b & LOWHALFMASK);
	z.lo &= LOWHALFMASK;
	z.lo |= (mid & LOWHALFMASK) << HALFWORKBITS;
	z.hi = (ULLong) ahi * bhi + (a >> HALFWORKBITS) +
		(b >> HALFWORKBITS) + (mid >> HALFWORKBITS);
	return z;
}

/*
 * Explicit cast into some floating-point format, and assigments.
 * Drop precision (rounding correctly) and clamp exponent in range.
 */
typedef int bool;

static SF
sfround(SF sf, ULLong extra, TWORD t)
{
	FPI *fpi;
	ULLong minmant, mant, maxmant;
	DULLong z;
	int exp = sf.exponent, excess, doinc, rd;

	assert(t>=FLOAT && t<=LDOUBLE);
	fpi = fpis[t-FLOAT];
	switch(sf.kind & SF_kmask) {
	  case SF_Zero:
		if (! fpi->has_neg_zero)
			sf.kind &= ~SF_Neg;
		/* FALLTHROUGH */
	  default:
/* XXX revise: SF_NaN escapes here, but what to do if !fpi->has_inf_nan */
		return sf;
	  case SF_Infinite:
		return fpi->has_inf_nan ? sf :
/* XXX revise: IEEE says overflow and invalid cannot be both, but this is for VAX... */
			hugesf(sf.kind | SFEXCP_Overflow | SFEXCP_Invalid, t);
	  case SF_NaNbits:
		cerror("Unexpected softfloat NaNbits");
		sf.kind -= SF_NaNbits - SF_NaN;
		return sf;
	  case SF_Denormal:
		if (exp != fpi->emin || extra) {
			/*static_*/assert(SF_Denormal > SF_Normal);
			sf.kind -= SF_Denormal - SF_Normal;
		}
		break;
	  case SF_Normal:
		break;
	}

	maxmant = ONES(fpi->nbits - 1);
	if (fpi->has_radix_16)
		minmant = ONEZEROES(fpi->nbits - 4);
	else
		minmant = ONEZEROES(fpi->nbits - 1);
	assert(sf.significand);
	excess = 0;
	if (sf.significand < minmant) {
		/* Not normalized */
		if ((sf.kind & SF_kmask) != SF_Denormal) {
			mant = sf.significand;
			for (; mant < minmant; --excess)
				mant <<= 1;
			if (fpi->has_radix_16 && (exp + excess) & 3)
				excess -= (exp + excess) & 3;
		}
	}
	else {
		if ((sf.kind & SF_kmask) == SF_Denormal)
			sf.kind -= SF_Denormal - SF_Normal;
		if ((sf.significand & ~maxmant) != 0) {
			/* More bits than allowed in significand */
			if (sf.significand & NORMALMANT)
				excess = WORKBITS - fpi->nbits;
			else {
				mant = sf.significand;
				for (; mant > maxmant; ++excess)
					mant >>= 1;
			}
		}
	}
	if (fpi->has_radix_16 && (exp + excess) & 3)
		excess += 4 - ((exp + excess) & 3);
	if (excess) {
		if (excess < 0)
			z = lshiftd(sf.significand, extra, -excess);
		else
			z = rshiftdro(sf.significand, extra, excess);
		sf.significand = z.hi;
		extra = z.lo;
		exp += excess;
	}
	assert((sf.kind & SF_kmask) == SF_Denormal ? sf.significand < minmant
	    : (sf.significand & ~(minmant-1)) == minmant || fpi->has_radix_16);

/* XXX check limit cases (emin, emin-1, emin-2) to avoid double rounding... */

	rd = fpi->rounding;
	if (sf_constrounding != FPI_RoundNotSet)
		rd = sf_constrounding;
	if (sf.kind & SF_Neg && rd == FPI_Round_down)
		rd = FPI_Round_up;
	else if (sf.kind & SF_Neg && rd == FPI_Round_up)
		rd = FPI_Round_down;
	if (extra != 0) {
		doinc = rd == FPI_Round_up;
		if (rd == FPI_Round_near && extra == NORMALMANT)
			doinc = (int)sf.significand & 1;
		else if ((rd & 3) == FPI_Round_near && extra >= NORMALMANT)
			doinc = 1;
/* XXX set SFEXCP_Inex(hi|lo) ? later ? */
		if (doinc) {
			if (sf.significand == maxmant) {
				sf.significand = minmant;
				++exp;
			}
			else
				sf.significand++;
		}
	}
	else doinc = 0;

	if (exp < fpi->emin) {
/* XXX NO! IEEE754 says that if result is less than DBL_MIN but can be
 * represented exactly (as denormal), Underflow exception is NOT signaled.
 */
		sf.kind |= SFEXCP_Underflow;
		if (fpi->sudden_underflow || exp < fpi->emin - fpi->nbits)
			return zerosf(sf.kind | SFEXCP_Inexlo);
		if (doinc) {
			if (sf.significand == minmant) {
				sf.significand = maxmant;
				--exp;
			}
			else
				sf.significand--;
		}
		excess = fpi->emin - exp;
		z = rshiftdro(sf.significand, extra, excess);
/* XXX need to consider extra here... */
		doinc = rd == FPI_Round_up;
		if ((rd & 3) == FPI_Round_near) {
			if (z.lo > NORMALMANT)
				doinc = 1;
			else if (rd == FPI_Round_near && z.lo == NORMALMANT)
				doinc = (int)z.hi & 1;
			else if (rd == FPI_Round_near_from0 && z.lo == NORMALMANT)
				doinc = 1;
		}
		if (doinc) z.hi++;
/* XXX revise: it seems there should be a missed case where the delivered result
 * was DBL_MIN-DBL_EPSILON, thus just below the bar, but when rounded to the lower
 * precision of denormals it should be rounded up to normal...
 * (in the exact half case, note extra==0)
 */
		if (z.hi) {
			sf.significand = z.hi;
			sf.kind += SF_Denormal - SF_Normal;
			sf.kind |= doinc ? SFEXCP_Inexhi : SFEXCP_Inexlo;
			exp = fpi->emin;
		}
		else {
			sf = zerosf(sf.kind | SFEXCP_Inexlo);
			exp = 0;
		}
	}
	else if (exp > fpi->emax) {
		sf.kind |= SFEXCP_Overflow | SFEXCP_Inexhi;
		if (! fpi->has_inf_nan || rd == FPI_Round_zero || 
		    rd == FPI_Round_down) {
			sf.significand = maxmant;
			exp = fpi->emax;
		}
		else {
			sf.kind = SF_Infinite | (sf.kind & ~SF_kmask);
			sf.significand = minmant;
			exp = fpi->emax+1;
		}
	}
	else if (extra)
		sf.kind = doinc ? SFEXCP_Inexhi : SFEXCP_Inexlo;
	sf.exponent = exp;
	return sf;
}

/*
 * Conversions.
 */

/*
 * Convert from integer type f to floating-point type t.
 * Rounds correctly to the target type.
 */
SF
soft_int2fp(CONSZ ll, TWORD f, TWORD t)
{
	SF rv;

	rv = zerosf(0);
	if (ll == 0)
		return rv;  /* fp is zero */
	rv.kind = SF_Normal;
	if (!ISUNSIGNED(f) && ll < 0)
		rv.kind |= SF_Neg, ll = -ll;
	rv.significand = ll; /* rv.exponent already 0 */
	return SFROUND(rv, t);
}

/*
 * Explicit cast into some floating-point format, and assigments.
 * Drop precision (rounding correctly) and clamp exponent in range.
 */
SF
soft_fp2fp(SF sf, TWORD t)
{
	return SFROUND(sf, t);
}

/*
 * Convert a fp number to a CONSZ. Always chop toward zero.
 * XXX Should warns somehow if out-of-range: in sf_exceptions
 */
CONSZ
soft_fp2int(SF sf, TWORD t)
{
	ULLong mant;
	int exp = sf.exponent;

	switch(sf.kind & SF_kmask) {
	  case SF_Zero:
	  case SF_Denormal:
		return 0;

	  case SF_Normal:
		if (exp < - WORKBITS - 1)
			return 0;
		if (exp < WORKBITS)
			break;
		/* FALLTHROUGH */
	  case SF_Infinite:
/* XXX revise */
		/* Officially entering undefined behaviour! */
		uerror("Conversion of huge FP constant into integer");
		/* FALLTHROUGH */

	  case SF_NoNumber:
	  default:
		/* Can it happen? Debug_Warns? ICE? */
		/* FALLTHROUGH */
	  case SF_NaN:
	  case SF_NaNbits:
		uerror("Undefined FP conversion into an integer, replaced with 0");
		return 0;
	}
	mant = sf.significand;
	while (exp < 0)
		mant >>= 1, exp++;
/* XXX if (exp > WORKBITS) useless (really SZ of CONSZ) */
	while (exp > 0)
/* XXX check overflow */
		mant <<= 1, exp--;
	if (sf.kind & SF_Neg)
		mant = -(long long)mant;
/* XXX sf_exceptions */
/* XXX check overflow for target type */
	return mant;
}

/*
 * Operations.
 */

/*
 * Negate a softfloat.
 */
SF
soft_neg(SF sf)
{
	FPI_LDOUBLE_NEG(sf);
	return sf;
}

/*
 * IEEE754 operations on sign bit. Always well defined, even with NaN.
 */
CONSZ
soft_signbit(SF sf)
{
	return sf.kind & SF_Neg;
}

SF
soft_copysign(SF sf, SF src)
{
	SFCOPYSIGN(sf, src);
	return sf;
}

/*
 * Add two numbers of same sign.
 * The devil is in the details, like those negative zeroes...
 */
static SF
sfadd(SF x1, SF x2, TWORD t)
{
	SF rv;
	DULLong z;
	int diff;

	if (SFISINF(x1))
		return x1;
	if (SFISINF(x2))
		return x2;
	if (SFISZ(x2)) /* catches all signed zeroes, delivering x1 */
		return x1;
	if (SFISZ(x1))
		return x2;
	assert(x1.significand && x2.significand);
	SFNORMALIZE(x1);
	SFNORMALIZE(x2);
	if (x1.exponent - WORKBITS > x2.exponent) {
		x1.kind |= SFEXCP_Inexlo;
		return x1;
	}
	if (x2.exponent - WORKBITS > x1.exponent) {
		x2.kind |= SFEXCP_Inexlo;
		return x2;
	}
	diff = x1.exponent - x2.exponent;
	if (diff < 0) {
		rv = x2;
		z = rshiftdro(x1.significand, 0, -diff );
	}
	else {
		rv = x1;
		z = rshiftdro(x2.significand, 0, diff );
	}
	rv.significand += z.hi;
	if (rv.significand < NORMALMANT) {
		/* target mantissa overflows */
		z = rshiftdro(rv.significand, z.lo, 1);
		rv.significand = z.hi | NORMALMANT;
		++rv.exponent;
	}
	return sfround(rv, z.lo, t);
}

/*
 * Substract two positive numbers.
 * Special hack when the type number t being negative, to indicate
 * that rounding rules should be reversed (because the operands were.)
 */
static SF
sfsub(SF x1, SF x2, TWORD t)
{
	SF rv;
	DULLong z;
	int diff;

	if (SFISINF(x1) && SFISINF(x2))
		return nansf(x1.kind | SFEXCP_Invalid);
	if (SFISINF(x1))
		return x1;
	if (SFISINF(x2))
		return SFNEG(x2);
	if (SFISZ(x2)) /* catches 0 - 0, delivering +0 */
		return x1;
	if (SFISZ(x1))
		return SFNEG(x2);
	assert(x1.significand && x2.significand);
	SFNORMALIZE(x1);
	SFNORMALIZE(x2);
	if (x1.exponent - WORKBITS > x2.exponent) {
		x1.kind |= (int)t < 0 ? SFEXCP_Inexlo : SFEXCP_Inexhi;
		return x1;
	}
	if (x2.exponent - WORKBITS > x1.exponent) {
		x2.kind |= (int)t < 0 ? SFEXCP_Inexlo : SFEXCP_Inexhi;
		return SFNEG(x2);
	}
	diff = x1.exponent - x2.exponent;
	if (diff == 0 && x1.significand == x2.significand) {
		if ((int)t < 0)
			t = -(int)t;
/* XXX sf_constrounding */
		if ((fpis[t-FLOAT]->rounding & 3) == FPI_Round_down)
			return zerosf(SF_Neg | (x1.kind & ~SF_Neg));
		return zerosf(x1.kind & ~SF_Neg); /* x1==x2==-0 done elsewhere */
	}
	if (diff < 0 || (diff == 0 && x1.significand < x2.significand)) {
		rv = SFNEG(x2);
		z = rshiftdro(x1.significand, 0, -diff );
	}
	else {
		rv = x1;
		z = rshiftdro(x2.significand, 0, diff );
	}
	if ((rv.kind & SF_kmask) == SF_Denormal)
		rv.kind -= SF_Denormal - SF_Normal;
	rv.significand -= z.hi;
	if ((int)t < 0) {
		int rd;

		t = -(int)t;
		rd = fpis[t-FLOAT]->rounding;
/* XXX sf_constrounding */
		if ((rd & 3) == FPI_Round_up || (rd & 3) == FPI_Round_down) {
			rv = sfround(SFNEG(rv), z.lo, t);
			return SFNEG(rv);
		}
	}
	return sfround(rv, z.lo, t);
}

SF
soft_plus(SF x1, SF x2, TWORD t)
{
	x1.kind |= x2.kind & SFEXCP_ALLmask;
	x2.kind |= x1.kind & SFEXCP_ALLmask;
	if (SFISNAN(x1))
		return x1;
	else if (SFISNAN(x2))
		return x2;
	switch ((x1.kind & SF_Neg) - (x2.kind & SF_Neg)) {
	  case SF_Neg - 0:
		return sfsub(x2, SFNEG(x1), - (int)t);
	  case 0 - SF_Neg:
		return sfsub(x1, SFNEG(x2), t);
	}
	return sfadd(x1, x2, t);
}

SF
soft_minus(SF x1, SF x2, TWORD t)
{
	x1.kind |= x2.kind & SFEXCP_ALLmask;
	x2.kind |= x1.kind & SFEXCP_ALLmask;
	if (SFISNAN(x1))
		return x1;
	else if (SFISNAN(x2))
		return x2;
	if ((x1.kind & SF_Neg) != (x2.kind & SF_Neg))
		return sfadd(x1, SFNEG(x2), t);
	else if (SFISZ(x1) && SFISZ(x2))
		return x1; /* special case -0 - -0, should return -0 */
	else if (x1.kind & SF_Neg)
		return sfsub(SFNEG(x2), SFNEG(x1), - (int)t);
	else
		return sfsub(x1, x2, t);
}

/*
 * Multiply two softfloats.
 */
SF
soft_mul(SF x1, SF x2, TWORD t)
{
#if 0
	ULong x1hi, x2hi;
	ULLong mid1, mid, extra;
#else
	DULLong z;
#endif

	x1.kind |= x2.kind & SFEXCP_ALLmask;
	x2.kind |= x1.kind & SFEXCP_ALLmask;
	x1.kind ^= x2.kind & SF_Neg;
	SFCOPYSIGN(x2, x1);
	if (SFISNAN(x1))
		return x1;
	else if (SFISNAN(x2))
		return x2;
	if ((SFISINF(x1) && SFISZ(x2)) || (SFISZ(x1) && SFISINF(x2)))
		return nansf(x1.kind | SFEXCP_Invalid);
	if (SFISINF(x1) || SFISZ(x1))
		return x1;
	if (SFISINF(x2) || SFISZ(x2))
		return x2;
	assert(x1.significand && x2.significand);
	SFNORMALIZE(x1);
	SFNORMALIZE(x2);
	x1.exponent += x2.exponent + WORKBITS;
#if 0
	x1hi = x1.significand >> 32;
	x1.significand &= ONES(32);
	x2hi = x2.significand >> 32;
	x2.significand &= ONES(32);
	extra = x1.significand * x2.significand;
	mid1 = x1hi * x2.significand;
	mid = mid1 + x1.significand * x2hi;
	x1.significand = (ULLong) x1hi * x2hi;
	x1.significand += ((ULLong)(mid < mid1) << 32) | (mid >> 32);
	mid <<= 32;
	extra += mid;
#ifdef LONGLONG_WIDER_THAN_WORKBITS
	if (extra < mid || (extra>>WORKBITS)) {
		x1.significand++;
		extra &= ONES(WORKBITS);
	}
#else
	x1.significand += (extra < mid);
#endif
	if (x1.significand < NORMALMANT) {
		x1.exponent--;
		x1.significand <<= 1;
		if (extra >= NORMALMANT) {
			x1.significand++;
			extra -= NORMALMANT;
		}
		extra <<= 1;
	}
	return sfround(x1, extra, t);
#else
	z = muld(x1.significand, x2.significand);
	if (z.hi < NORMALMANT) {
		x1.exponent--;
		z = lshiftd(z.hi, z.lo, 1);
	}
	x1.significand = z.hi;
	return sfround(x1, z.lo, t);
#endif
}

SF
soft_div(SF x1, SF x2, TWORD t)
{
	ULLong q, r, oppx2;
	int exp;

	x1.kind |= x2.kind & SFEXCP_ALLmask;
	x2.kind |= x1.kind & SFEXCP_ALLmask;
	x1.kind ^= x2.kind & SF_Neg;
	SFCOPYSIGN(x2, x1);
	if (SFISNAN(x1))
		return x1;
	else if (SFISNAN(x2))
		return x2;
	if ((SFISINF(x1) && SFISINF(x2)) || (SFISZ(x1) && SFISZ(x2)))
		return nansf(x1.kind | SFEXCP_Invalid);
	if (SFISINF(x1) || SFISZ(x1))
		return x1;
	else if (SFISINF(x2))
		return zerosf(x1.kind);
	else if (SFISZ(x2))
		return infsf(x2.kind | SFEXCP_DivByZero);
	assert(x1.significand && x2.significand);
	SFNORMALIZE(x1);
	SFNORMALIZE(x2);
	exp = x1.exponent - x2.exponent - WORKBITS;
	if (exp < -32767)
		/* huge underflow, flush to 0 to avoid issues */
		return zerosf(x1.kind | SFEXCP_Inexlo | SFEXCP_Underflow);
	q = 0;
	if (x1.significand >= x2.significand) {
		++exp;
		++q;
		x1.significand -= x2.significand;
	}
	r = x1.significand;
	oppx2 = (ONES(WORKBITS-1) - x2.significand) + 1;
	do {
		q <<= 1;
		if (r & NORMALMANT) {
			r &= ~NORMALMANT;
			r <<= 1;
			r += oppx2;
			++q;
		}
		else {
			r <<= 1;
			if (r >= x2.significand) {
				r -= x2.significand;		
				++q;
			}
		}
	} while((q & NORMALMANT) == 0);
	x1.significand = q;
	x1.exponent = exp;
	if (r) {
		/* be sure to set correctly highest bit of extra */
		r += oppx2 / 2;
		r |= 1; /* rounds to odd */
/* XXX can remainder be power-of-2? doesn't seem it may happen... */
	}
	return sfround(x1, r, t);
}

#ifndef NO_COMPLEX
/*
 * Perform the addition or subtraction of two products of finite numbers.
 * Keep the extra bits (do not round).
 * Note that maximum return exponent is 2*emax+WORKBITS.
 */
#define FMMPLUS 	0
#define FMMMINUS	SF_Neg

static SF
finitemma(SF a, SF b, int op, SF c, SF d, ULLong * extrap)
{
	SF rv;
	DULLong z, z1, z2;
	int diff, z1k, z1exp, z2k, z2exp, excess;
	ULLong x;

	z1k = z2k = SF_Normal |
		(a.kind & SFEXCP_ALLmask) | (b.kind & SFEXCP_ALLmask) |
		(c.kind & SFEXCP_ALLmask) | (d.kind & SFEXCP_ALLmask);
	z1k |= (a.kind & SF_Neg) ^ (b.kind & SF_Neg);
	z2k |= (c.kind & SF_Neg) ^ (d.kind & SF_Neg) ^ op;
	assert(a.significand && b.significand && c.significand && d.significand);
	SFNORMALIZE(a);
	SFNORMALIZE(b);
	SFNORMALIZE(c);
	SFNORMALIZE(d);
	z1exp = a.exponent + b.exponent + WORKBITS;
	z2exp = c.exponent + d.exponent + WORKBITS;
#if 0
	if (z1exp - WORKBITS > z2exp) {
		x1.kind |= (int)t < 0 ? SFEXCP_Inexlo : SFEXCP_Inexhi;
		return x1;
	}
	if (z2exp - WORKBITS > z1exp) {
		x2.kind |= (int)t < 0 ? SFEXCP_Inexlo : SFEXCP_Inexhi;
		return SFNEG(x2);
	}
#endif
	z1 = muld(a.significand, b.significand);
	if (z1.hi < NORMALMANT) {
		z1exp--;
		z1 = lshiftd(z1.hi, z1.lo, 1);
	}
	z2 = muld(c.significand, d.significand);
	if (z2.hi < NORMALMANT) {
		z2exp--;
		z2 = lshiftd(z2.hi, z2.lo, 1);
	}
	diff = z1exp - z2exp;
	if (z1k == z2k) { /* same sign, add them; easier */
		rv.kind = z1k;
		if (diff < 0) {
			z = z2, z2 = z1, z1 = z;
			rv.exponent = z2exp;
		}
		else
			rv.exponent = z1exp;
		z2 = rshiftdro(z2.hi, z2.lo, diff );
		rv.significand = z1.hi + z2.hi;
		z1.lo += z2.lo;
		if (z1.lo < z2.lo)
			++rv.significand;
		if (rv.significand < NORMALMANT) {
			/* target mantissa overflows */
			z = rshiftdro(rv.significand, z1.lo, 1);
			rv.significand = z.hi | NORMALMANT;
			++rv.exponent;
		}
		else
			z.lo = z1.lo;
		*extrap = z.lo;
	}
	else { /* opposite sign, substraction, and possible cancellation */
		if (diff == 0 && z1.hi == z2.hi && z1.lo == z2.lo) {
			*extrap = 0;
/* XXX compute sign of 0 if rounding, merge exceptions... */
			return zerosf(z1k & ~SF_Neg);
		}
		else if (diff == 0 && z1.hi == z2.hi) {
			if (z1.lo > z2.lo) {
				rv.kind = z1k;
				rv.significand = z1.lo - z2.lo;
			}
			else {
				rv.kind = z2k;
				rv.significand = z2.lo - z1.lo;
			}
			rv.exponent = z1exp - WORKBITS;
			z.lo = 0;
		}
		else {
			if (diff < 0 || (diff == 0 && z1.hi < z2.hi)) {
				rv.kind = z2k ^ SF_Neg;
				rv.exponent = z2exp;
				if (diff != 0) {
					z = z2;
					z2 = rshiftdro(z1.hi, z1.lo, -diff );
					z1 = z;
				}
				else {
					z = z2, z2 = z1, z1 = z;
				}
			}
			else {
				rv.kind = z1k;
				rv.exponent = z1exp;
				if (diff != 0)
					z2 = rshiftdro(z2.hi, z2.lo, diff );
			}
			z.hi = z1.hi - z2.hi;
			if (z2.lo > z1.lo)
				--z.hi;
			z.lo = z1.lo - z2.lo;
			x = z.hi;
			for (excess = 0; x < NORMALMANT; ++excess)
				x <<= 1;
			z = lshiftd(z.hi, z.lo, excess);
			rv.exponent -= excess;
			rv.significand = z.hi;
		}
		*extrap = z.lo;
	}
	return rv;
}

#define CXSFISZ(r,i)	(SFISZ(r)   && SFISZ(i))
#define CXSFISINF(r,i)	(SFISINF(r) || SFISINF(i))
#define CXSFISNAN(r,i)	(!CXSFISINF(r,i) && (SFISNAN(r) || SFISNAN(i)))

/*
 * Multiply two complexes (pairs of softfloats)
 */
void
soft_cxmul(SF r1, SF i1, SF r2, SF i2, SF *rrv, SF *irv, TWORD t)
{
	SF sf;
	ULLong extra;

# if 0
	x1.kind |= x2.kind & SFEXCP_ALLmask;
	x2.kind |= x1.kind & SFEXCP_ALLmask;
	x1.kind ^= x2.kind & SF_Neg;
	SFCOPYSIGN(x2, x1);
#endif
	if (CXSFISINF(r1, i1)) {
		if (CXSFISZ(r2, i2)) {
			return;
		}
		else if (SFISNAN(r2) && SFISNAN(i2)) {
			return;
		}
		/* result is an infinity */
	}
	else if (CXSFISINF(r2, i2)) {
		if (CXSFISZ(r1, i1)) {
			return;
		}
		else if (SFISNAN(r1) && SFISNAN(i1)) {
			return;
		}
		/* result is an infinity */
	}

/* XXX missing many special cases with NaN or zeroes... */

	sf = finitemma(r1, r2, FMMMINUS, i1, i2, &extra);
	*rrv = sfround(sf, extra, t);
	sf = finitemma(r1, i2, FMMPLUS,  i1, r2, &extra);
	*irv = sfround(sf, extra, t);
}

/*
 * Divide two complexes (pairs of softfloats.) Indeed complex.
 */
/*
 * Helper function: division double by double
 * Result is correctly rounded.
 */
static SF
sfddiv(SF num, ULLong extran, SF den, ULLong extrad, TWORD t)
{
	DULLong zn, zd, r, oppden;
	ULLong q;
	int exp, excess;

#if 0
	num.kind |= den.kind & SFEXCP_ALLmask;
	den.kind |= num.kind & SFEXCP_ALLmask;
	num.kind ^= den.kind & SF_Neg;
	SFCOPYSIGN(den, num);
#endif
	assert(num.significand && den.significand);
	q = num.significand;
	for (excess = 0; q < NORMALMANT; ++excess)
		q <<= 1;
	zn = lshiftd(num.significand, extran, excess);
	num.exponent -= excess;
	q = den.significand;
	for (excess = 0; q < NORMALMANT; ++excess)
		q <<= 1;
	zd = lshiftd(den.significand, extrad, excess);
	den.exponent -= excess;
	exp = num.exponent - den.exponent - WORKBITS;
	if (exp < -32767) {
		/* huge underflow, flush to 0 to avoid issues */
		return zerosf(num.kind | SFEXCP_Inexlo | SFEXCP_Underflow);
	}
	q = 0;
	if (zn.hi >= zd.hi) {
		++exp;
		++q;
		zn.hi -= zd.hi;
	}
	r = zn;
	if (zd.lo) {
		oppden.hi = (ONES(WORKBITS-1) - zd.hi);
		oppden.lo = (ONES(WORKBITS-1) - zd.lo) + 1;
	}
	else {
		oppden.hi = (ONES(WORKBITS-1) - zd.hi) + 1;
		oppden.lo = 0;
	}
	do {
		q <<= 1;
		if (r.hi & NORMALMANT) {
			r.hi &= ~NORMALMANT;
			r = lshiftd(r.hi, r.lo, 1);
			r.hi += oppden.hi;
			r.lo += oppden.lo;
			if (r.lo < oppden.lo)
				++r.hi;
			++q;
		}
		else {
			r = lshiftd(r.hi, r.lo, 1);
			if (r.hi > zd.hi || (r.hi == zd.hi && r.lo >= zd.lo)) {
				r.hi -= zd.hi;
				if (zd.lo > r.lo)
					--r.hi;
				r.lo -= zd.lo;
				++q;
			}
		}
	} while((q & NORMALMANT) == 0);
	num.significand = q;
	num.exponent = exp;
	if (r.hi) {
		/* be sure to set correctly highest bit of extra */
		r.hi += oppden.hi / 2;
		r.hi |= 1; /* rounds to odd */
/* XXX is there special case if remainder is power-of-2? can it happen? */
	}
	return sfround(num, r.hi, t);
}

void
soft_cxdiv(SF r1, SF i1, SF r2, SF i2, SF *rrv, SF *irv, TWORD t)
{
	SF den, sf;
	ULLong extrad, extra;

	if (CXSFISINF(r1, i1)) {
		if (CXSFISINF(r2, i2)) {
			return;
		}
		else if (SFISNAN(r2) && SFISNAN(i2)) {
			return;
		}
		/* result is an infinity */
	}
	else if (CXSFISINF(r2, i2)) {
		if (SFISNAN(r1) && SFISNAN(i1)) {
			return;
		}
		/* result is zero */
	}
	else if (CXSFISZ(r2, i2)) {
		if (SFISNAN(r2) && SFISNAN(i2)) {
			return;
		}
		/* result is an infinity */
	}

/* XXX missing many special cases with NaN or zeroes... */

	den= finitemma(r2, r2, FMMPLUS,  i2, i2, &extrad);
	sf = finitemma(r1, r2, FMMPLUS,  i1, i2, &extra);
	*rrv = sfddiv(sf, extra, den, extrad, t);
	sf = finitemma(i1, r2, FMMMINUS, r1, i2, &extra);
	*irv = sfddiv(sf, extra, den, extrad, t);
}
#endif

/*
 * Classifications and comparisons.
 */

/*
 * Return true if fp number is zero. Easy.
 */
int
soft_isz(SF sf)
{
	int r = FPI_LDOUBLE_ISZ(sf);
#ifdef DEBUGFP
	if ((sf.debugfp == 0.0 && r == 0) || (sf.debugfp != 0.0 && r == 1))
		fpwarn("soft_isz", sf.debugfp, (long double)r);
#endif
	return r;
}

int
soft_isnan(SF sf)
{
	return (sf.kind & SF_kmask) >= SF_NaN;
}

/*
 * Return IEEE754 class of fp number, as SF_xxx enum member.
 * First clamp the number into its original (semantic) type, cf. 7.12.3.
 * Can be used to implement isinf, isfinite, isnormal.
 */
int
soft_fpclassify(SF sf, TWORD t)
{
	return SFROUND(sf, t).kind & SF_kmask;
}


/*
 * Return true if either operand is NaN.
 */
static int
soft_cmp_unord(SF x1, SF x2)
{
	return SFISNAN(x1) || SFISNAN(x2);
}

static int
soft_cmp_eq(SF x1, SF x2)
{
	int exp1, exp2;

	if (soft_cmp_unord(x1, x2))
		return 0; /* IEEE says "quiet" */
	if (SFISZ(x1))
		/* special case: +0 == -0 (discard sign) */
		return SFISZ(x2);
	if ((x1.kind & SF_Neg) != (x2.kind & SF_Neg))
		return 0;
	if (SFISINF(x1))
		return SFISINF(x2);
	/* Both operands are finite, nonzero, same sign. */
	exp1 = x1.exponent, exp2 = x2.exponent;
	assert(x1.significand && x2.significand);
	while (x1.significand < NORMALMANT)
		x1.significand <<= 1, exp1--;
	while (x2.significand < NORMALMANT)
		x2.significand <<= 1, exp2--;
	return exp1 == exp2 && x1.significand == x2.significand;
}

/*
 * IEEE754 states that between any two floating-point numbers,
 * four mutually exclusive relations are possible:
 * less than, equal, greater than, or unordered.
 */

static int
soft_cmp_ne(SF x1, SF x2)
{
	return soft_cmp_unord(x1, x2) ? 0 : ! soft_cmp_eq(x1, x2);
}

static int
soft_cmp_lt(SF x1, SF x2)
{
	int exp1, exp2;

	if (soft_cmp_unord(x1, x2))
/* XXX "invalid"; warns? use sf_exceptions */
		return 0;
	if (SFISZ(x1) && SFISZ(x2))
		return 0; /* special case: -0 !> +0 */
	switch ((x1.kind & SF_Neg) - (x2.kind & SF_Neg)) {
	  case SF_Neg - 0:
		return 1; /* x1 < 0 < x2 */
	  case 0 - SF_Neg:
		return 0; /* x1 > 0 > x2 */
	  case 0:
		break;
	}
	if (x1.kind & SF_Neg) {
		SF tmp = x1;
		x1 = x2;
		x2 = tmp;
	}
	if (SFISINF(x2))
		return ! SFISINF(x1);
	if (SFISZ(x1))
		return 1;
	if (SFISZ(x2) || SFISINF(x1))
		return 0;
	/* Both operands are finite, nonzero, same sign. Test |x1| < |x2|*/
	exp1 = x1.exponent, exp2 = x2.exponent;
	assert(x1.significand && x2.significand);
	while (x1.significand < NORMALMANT)
		x1.significand <<= 1, exp1--;
	while (x2.significand < NORMALMANT)
		x2.significand <<= 1, exp2--;
	return exp1 < exp2 || x1.significand < x2.significand;
}

static int
soft_cmp_gt(SF x1, SF x2)
{
	int exp1, exp2;

	if (soft_cmp_unord(x1, x2))
/* XXX "invalid"; warns? use sf_exceptions */
		return 0;
	if (SFISZ(x1) && SFISZ(x2))
		return 0; /* special case: -0 !< +0 */
	switch ((x1.kind & SF_Neg) - (x2.kind & SF_Neg)) {
	  case SF_Neg - 0:
		return 1; /* x1 > 0 > x2 */
	  case 0 - SF_Neg:
		return 0; /* x1 < 0 < x2 */
	  case 0:
		break;
	}
	if (x1.kind & SF_Neg) {
		SF tmp = x1;
		x1 = x2;
		x2 = tmp;
	}
	if (SFISINF(x1))
		return ! SFISINF(x2);
	if (SFISZ(x1) || SFISINF(x2))
		return 0;
	if (SFISZ(x2))
		return 1;
	/* Both operands are finite, nonzero, same sign. Test |x1| > |x2|*/
	exp1 = x1.exponent, exp2 = x2.exponent;
	assert(x1.significand && x2.significand);
	while (x1.significand < NORMALMANT)
		x1.significand <<= 1, exp1--;
	while (x2.significand < NORMALMANT)
		x2.significand <<= 1, exp2--;
	return exp1 > exp2 || x1.significand > x2.significand;
}

/*
 * Note: for _le and _ge, having NaN operand is "invalid" in IEEE754;
 * but we cannot return SFEXCP_Invalid as done for the operations.
 */
static int
soft_cmp_le(SF x1, SF x2)
{
	return soft_cmp_unord(x1, x2) ? 0 : ! soft_cmp_gt(x1, x2);
}

static int
soft_cmp_ge(SF x1, SF x2)
{
	return soft_cmp_unord(x1, x2) ? 0 : ! soft_cmp_lt(x1, x2);
}

int
soft_cmp(SF v1, SF v2, int v)
{
	int rv = 0;

	switch (v) {
	case GT:
		rv = soft_cmp_gt(v1, v2);
		break;
	case LT:
		rv = soft_cmp_lt(v1, v2);
		break;
	case GE:
		rv = soft_cmp_ge(v1, v2);
		break;
	case LE:
		rv = soft_cmp_le(v1, v2);
		break;
	case EQ:
		rv = soft_cmp_eq(v1, v2);
		break;
	case NE:
		rv = soft_cmp_ne(v1, v2);
		break;
	}
	return rv;
}

#if 0
/*
 * Prepare a SF value for use in packed (interchange) format.
 * Correctly rounds for the target type representation.
 * Returns the biased exponent, ready to be shifted.
 *
 * The .significand bits are left in place, ready to be used by
 * the endian-aware code. The MSB one is still there.
 * Sign is indicated with psf->kind & SF_neg, as usual.
 *
 * SF_NaN is expanded into the IEEE754:2008 default representation
 * (11 as most significant bits, rest all zeroes); if not appropriate
 * for the target, the calling code should replace it with SF_NaNbits
 * with the adequate bits into the .significand member.
 */
/* XXX TODO: implement the NaNbits stuff in sfround() above... */
static int
soft_pack(SF *psf, TWORD t)
{
	FPI *fpi;
	int biasedexp;

	assert(t>=FLOAT && t<=LDOUBLE);
	fpi = fpis[t-FLOAT];
	*psf = SFROUND(*psf, t);
	switch(psf->kind & SF_kmask) {
	  case SF_Zero:
		psf->significand = 0;
		biasedexp = 0;
		if (! fpi->has_neg_zero)
			psf->kind &= ~SF_Neg;
		break;

	  case SF_Normal:
		if (fpi->has_radix_16) {
			assert((psf->significand >> (fpi->nbits-4)) >= 1);
			assert((psf->significand >> (fpi->nbits-4)) <= 15);
		}
		else
			assert((psf->significand >> (fpi->nbits-1)) == 1);
		assert(psf->exponent >= fpi->emin);
		assert(psf->exponent <= fpi->emax);
		biasedexp = psf->exponent + fpi->exp_bias;
		break;

	  case SF_Denormal:
		assert(! fpi->sudden_underflow);
		assert(! fpi->has_radix_16);
		assert((psf->significand >> (fpi->nbits-1)) == 0);
		assert(psf->exponent == fpi->emin);
		biasedexp = 0;
		break;

	  case SF_Infinite:
		assert(fpi->has_inf_nan);
		psf->significand = ONEZEROES(fpi->nbits-1);
		biasedexp = fpi->emax - fpi->exp_bias + 1;
		break;

	  case SF_NoNumber:
	  default:
		/* Can it happen? Debug_Warns? ICE? */
		/* Let decay as quiet NaN */
	  case SF_NaN:
		psf->significand = 3 * ONEZEROES(fpi->nbits-2);
		/* FALLTHROUGH */
	  case SF_NaNbits:
/* XXX 0./0. on a Vax is likely to end here... as an ICE :-( */
		assert(fpi->has_inf_nan);
		biasedexp = fpi->emax - fpi->exp_bias + 1;
		break;
	}
	if (fpi->has_radix_16) {
		assert((biasedexp & 3) == 0);
		biasedexp >>= 2;
	}
	return biasedexp;
}


/*
 * Convert an internal floating-point constant into its external representation.
 * isf is floating point number, dt is resulting type and 
 */
CONSZ
soft_fp2ext(SF isf, TWORD dt)
{
#ifdef FLT_IS_NOT_SIGN_EXP_MAGNITUDE
	return NULL;
#else
	SF sf;
	FPI *fpi;
	int exp, fracbits, t, ti;

#ifdef notyet
	fpi = fpis[t - FLOAT];
	if (fpi->storage == 0)
		return NULL; /* not stored as sign+exponent+significand */
#endif
	if (~(U_CONSZ)0 >> (fpi->storage-1) == 0)
		return NULL; /* too large */
	sf = p->n_dcon;
	exp = soft_pack(&sf, t);
	fracbits = fpi->nbits - 1;
	if (fpi->explicit_one) ++fracbits;
	q = ccopy(p);
	q->n_lval = sf.significand & (((U_CONSZ)1 << fracbits) - 1);
	q->n_lval |= ((U_CONSZ)exp) << fracbits;
	if (sf.kind & SF_Neg)
		q->n_lval |= ((U_CONSZ)1 << (fpi->storage-1));
	q->n_op = ICON;
	q->n_type = ti;
	q->n_sp = NULL;
	return q;
#endif
}
#endif
#endif

/*
 * encode (sign,exp,mantissa) as long double.
 */
static void
vals2fp(unsigned short *fp, int k, int exp, uint32_t *mant)
{
	fp[4] = fp[3] = fp[2] = fp[1] = fp[0] = 0;
#ifdef USE_IEEEFP_X80
	switch (k & SF_kmask) {
	case SF_Zero:
		break; /* already 0 */

	case SF_Normal:
		fp[4] = exp + FPI_LDOUBLE.exp_bias;
		fp[3] = mant[1] >> 16;
		fp[2] = mant[1];
		fp[1] = mant[0] >> 16;
		fp[0] = mant[0];
		break;
	default:
		fprintf(stderr, "vals2fp: unhandled %x\n", k);
		break;
	}
	if (k & SF_Neg)
		fp[4] |= 0x8000;

	if (k & (SFEXCP_ALLmask & ~(SFEXCP_Inexlo|SFEXCP_Inexhi)))
		fprintf(stderr, "vals2fp: unhandled2 %x\n", k);
#elif defined(FDFLOAT) || defined(PDP10FLOAT)
	/*
	 * PDP-10/VAX format: sign(1) + exp(8) + mantissa(55 with implied 1)
	 * Stored in 4 shorts: fd1 contains sign|exp|mant[6:0], fd2-fd4 have rest
	 */
	unsigned long long m;

	switch (k & SF_kmask) {
	case SF_Zero:
		break; /* already 0 */

	case SF_Normal:
		/* Combine mantissa from two 32-bit words into 64-bit value */
		m = ((unsigned long long)mant[1] << 32) | mant[0];

		/* Adjust exponent (gdtoa gives biased exponent) */
		exp += EXPBIAS;

		/* Pack into fd1-fd4 format */
		fp[0] = ((exp & 0377) << 7) | ((m >> 57) & 0177);  /* fd1: exp + high 7 bits */
		fp[1] = (m >> 41) & 0xffff;  /* fd2: next 16 bits */
		fp[2] = (m >> 25) & 0xffff;  /* fd3: next 16 bits */
		fp[3] = (m >> 9) & 0xffff;   /* fd4: next 16 bits */
		break;

	default:
		fprintf(stderr, "vals2fp: unhandled %x\n", k);
		break;
	}

	if (k & SF_Neg)
		fp[0] |= 0x8000;  /* Set sign bit in fd1 */

	if (k & (SFEXCP_ALLmask & ~(SFEXCP_Inexlo|SFEXCP_Inexhi)))
		fprintf(stderr, "vals2fp: unhandled2 %x\n", k);
#else
	cerror("fixme floating point");
#endif
}

/*
 * Conversions from decimal and hexadecimal strings.
 * Rounds correctly to the target type (subject to FLT_EVAL_METHOD.)
 * dt is resulting type.
 */
SF
strtosf(char *str, TWORD tw)
{
	SF sf;
	char *eptr;
	ULong bits[2] = { 0, 0 };
	Long expt;
	int k;

	k = strtodg(str, &eptr, &FPI_LDOUBLE, &expt, bits);

	if (k & SFEXCP_Overflow)
		werror("Overflow in floating-point constant");
	if (k & SFEXCP_Inexact && (str[1] == 'x' || str[1] == 'X'))
		werror("Hexadecimal floating-point constant not exactly");
//fprintf(stderr, "vals: k %x expt %d bits %x %x\n", k, expt, bits[0], bits[1]);
	vals2fp(sf.fp, k, expt, bits);

#ifdef DEBUGFP
	{
		long double ld = strtold(str, NULL);
		if (ld != sf.debugfp)
			fpwarn("strtosf", sf.debugfp, ld);
	}
#endif

	return sf;
}

/*
 * return INF/NAN.
 */
SF
soft_huge_val(void)
{
	SF sf;
	static short val[] = FPI_LDOUBLE_INF;

	memcpy(&sf, val, sizeof val);
#ifdef DEBUGFP
	if (sf.debugfp != __builtin_huge_vall())
		fpwarn("soft_huge_val", sf.debugfp, __builtin_huge_vall());
#endif
	return sf;
}

SF
soft_nan(char *c)
{
	SF sf;
	static short val[] = FPI_LDOUBLE_NAN;

	memcpy(&sf, val, sizeof val);
	return sf;
}

/*
 * Return a LDOUBLE zero.
 */
SF
soft_zero()
{
	SF sf;
	static short val[] = FPI_LDOUBLE_ZERO;

	memcpy(&sf, val, sizeof val);
	return sf;
}

#ifdef DEBUGFP
void
fpwarn(char *s, long double soft, long double hard)
{
	extern int nerrors;

	union { long double ld; int i[3]; } X;
	fprintf(stderr, "WARNING: In function %s: soft=%La hard=%La\n",
	    s, soft, hard);
	fprintf(stderr, "WARNING: soft=%Lf hard=%Lf\n", soft, hard);
	X.ld=soft;
	fprintf(stderr, "WARNING: s[0]=%x s[1]=%x s[2]=%x ",
	    X.i[0], X.i[1], X.i[2]);
	X.ld=hard;
	fprintf(stderr, "h[0]=%x h[1]=%x h[2]=%x\n", X.i[0], X.i[1], X.i[2]);
	nerrors++;
}
#endif
