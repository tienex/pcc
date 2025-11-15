/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Compiler Abstraction Layer for DOS Memory Manager
 *
 * Supports: PCC, Watcom C/C++, Microsoft C/C++, Borland C/C++, DJGPP
 * Works with: DOS16 real mode, DOS16 protected mode, DOS32
 */

#ifndef DOSMEM_COMPILER_H
#define DOSMEM_COMPILER_H

#include <stdint.h>

/*
 * Compiler Detection
 */

/* Watcom C/C++ */
#if defined(__WATCOMC__)
  #define COMPILER_WATCOM
  #if defined(__386__) || defined(__FLAT__)
    #define DOS32
  #else
    #define DOS16
  #endif
#endif

/* Microsoft C/C++ */
#if defined(_MSC_VER) || defined(_QC)
  #define COMPILER_MSC
  #if defined(_M_I86)
    #define DOS16
  #elif defined(_M_IX86) || defined(_M_AMD64)
    #define DOS32
  #endif
#endif

/* Borland C/C++ / Turbo C/C++ */
#if defined(__BORLANDC__) || defined(__TURBOC__)
  #define COMPILER_BORLAND
  #if defined(__MSDOS__)
    #if defined(__DPMI__) || defined(__DPMI16__) || defined(__DPMI32__)
      /* DPMI mode */
      #if defined(__DPMI32__)
        #define DOS32
      #else
        #define DOS16
      #endif
    #else
      #define DOS16
    #endif
  #endif
#endif

/* DJGPP (always 32-bit DPMI) */
#if defined(__DJGPP__)
  #define COMPILER_DJGPP
  #define DOS32
#endif

/* PCC (Portable C Compiler) */
#if defined(__PCC__)
  #define COMPILER_PCC
  /* Detect mode from predefined macros */
  #if defined(__i386__) || defined(__x86_64__)
    #define DOS32
  #else
    #define DOS16
  #endif
#endif

/*
 * Far Pointer Support
 */

#ifdef DOS16
  /* 16-bit: use far pointers */
  #if defined(COMPILER_WATCOM)
    #define FAR __far
    #define NEAR __near
    #define HUGE __huge
  #elif defined(COMPILER_MSC)
    #define FAR __far
    #define NEAR __near
    #define HUGE __huge
  #elif defined(COMPILER_BORLAND)
    #define FAR far
    #define NEAR near
    #define HUGE huge
  #else
    #define FAR
    #define NEAR
    #define HUGE
  #endif

  /* Make far pointer from segment:offset */
  #if defined(COMPILER_WATCOM) || defined(COMPILER_MSC)
    #include <i86.h>
    #define MK_FP(seg, off) ((void FAR *)((unsigned long)(seg) << 16 | (unsigned)(off)))
  #elif defined(COMPILER_BORLAND)
    #include <dos.h>
    /* Borland provides MK_FP macro */
  #else
    #define MK_FP(seg, off) ((void FAR *)((unsigned long)(seg) << 16 | (unsigned)(off)))
  #endif
#else
  /* 32-bit: no far pointers */
  #define FAR
  #define NEAR
  #define HUGE
  #define MK_FP(seg, off) ((void *)((unsigned long)(seg) << 4 | (unsigned)(off)))
#endif

/*
 * Interrupt Calling
 */

#if defined(COMPILER_WATCOM)
  #include <i86.h>
  #define INT86(num, in, out) int86(num, in, out)
  #define INT86X(num, in, out, segregs) int86x(num, in, out, segregs)
#elif defined(COMPILER_MSC)
  #include <dos.h>
  #define INT86(num, in, out) int86(num, in, out)
  #define INT86X(num, in, out, segregs) int86x(num, in, out, segregs)
#elif defined(COMPILER_BORLAND)
  #include <dos.h>
  #define INT86(num, in, out) int86(num, in, out)
  #define INT86X(num, in, out, segregs) int86x(num, in, out, segregs)
#elif defined(COMPILER_DJGPP)
  #include <dpmi.h>
  #include <go32.h>
  /* DJGPP uses different interrupt calling mechanism */
  #define INT86(num, in, out) _go32_dpmi_simulate_int(num, (_go32_dpmi_registers *)(in))
  #define INT86X(num, in, out, segregs) _go32_dpmi_simulate_int(num, (_go32_dpmi_registers *)(in))
#else
  /* PCC or unknown compiler */
  #include <dos.h>
  #define INT86(num, in, out) int86(num, in, out)
  #define INT86X(num, in, out, segregs) int86x(num, in, out, segregs)
#endif

/*
 * Inline Assembly Macros
 */

/* Function pointer type for XMS/EMS entry points */
#ifdef DOS16
  typedef void (FAR *xms_entry_t)(void);
#else
  typedef void (*xms_entry_t)(void);
#endif

/*
 * Call XMS driver entry point
 * All compilers handle this differently
 */
#if defined(COMPILER_WATCOM)
  /* Watcom inline assembly */
  #define CALL_XMS(func_num, dx_val, result_ax, result_dx, result_bx) \
    do { \
      _asm { \
        mov ah, func_num \
        mov dx, dx_val \
        call dword ptr [xms_entry] \
        mov result_ax, ax \
        mov result_dx, dx \
        mov result_bx, bx \
      } \
    } while(0)

#elif defined(COMPILER_MSC)
  /* Microsoft inline assembly */
  #define CALL_XMS(func_num, dx_val, result_ax, result_dx, result_bx) \
    do { \
      _asm mov ah, func_num \
      _asm mov dx, dx_val \
      _asm call dword ptr [xms_entry] \
      _asm mov result_ax, ax \
      _asm mov result_dx, dx \
      _asm mov result_bx, bx \
    } while(0)

#elif defined(COMPILER_BORLAND)
  /* Borland inline assembly */
  #define CALL_XMS(func_num, dx_val, result_ax, result_dx, result_bx) \
    do { \
      asm mov ah, func_num; \
      asm mov dx, dx_val; \
      asm call dword ptr [xms_entry]; \
      asm mov result_ax, ax; \
      asm mov result_dx, dx; \
      asm mov result_bx, bx; \
    } while(0)

#elif defined(COMPILER_DJGPP)
  /* DJGPP uses DPMI to call real-mode code */
  extern int dosmem_call_xms_djgpp(unsigned char func, unsigned short dx,
                                   unsigned short *ax, unsigned short *dx_out,
                                   unsigned short *bx);
  #define CALL_XMS(func_num, dx_val, result_ax, result_dx, result_bx) \
    dosmem_call_xms_djgpp(func_num, dx_val, &result_ax, &result_dx, &result_bx)

#else
  /* Generic - may not work */
  #define CALL_XMS(func_num, dx_val, result_ax, result_dx, result_bx) \
    do { result_ax = 0; result_dx = 0; result_bx = 0; } while(0)
#endif

/*
 * Memory Model Detection
 */
#ifdef DOS16
  #if defined(__TINY__) || defined(_MT)
    #define MEMORY_MODEL_TINY
  #elif defined(__SMALL__) || defined(_MS)
    #define MEMORY_MODEL_SMALL
  #elif defined(__MEDIUM__) || defined(_MM)
    #define MEMORY_MODEL_MEDIUM
  #elif defined(__COMPACT__) || defined(_MC)
    #define MEMORY_MODEL_COMPACT
  #elif defined(__LARGE__) || defined(_ML)
    #define MEMORY_MODEL_LARGE
  #elif defined(__HUGE__) || defined(_MH)
    #define MEMORY_MODEL_HUGE
  #else
    #define MEMORY_MODEL_SMALL  /* Default */
  #endif
#else
  #define MEMORY_MODEL_FLAT
#endif

/*
 * Pragma Pack Support
 */
#if defined(COMPILER_WATCOM) || defined(COMPILER_MSC)
  #define PACK_PUSH() __pragma(pack(push, 1))
  #define PACK_POP() __pragma(pack(pop))
#elif defined(COMPILER_BORLAND)
  #define PACK_PUSH() _Pragma("pack(push, 1)")
  #define PACK_POP() _Pragma("pack(pop)")
#else
  #define PACK_PUSH()
  #define PACK_POP()
#endif

/*
 * DOS Mode Detection at Runtime
 */
static inline int dosmem_is_dpmi_mode(void)
{
#if defined(DOS32)
  return 1;  /* DOS32 is always DPMI */
#else
  /* Check for DPMI in DOS16 */
  union REGS regs;
  struct SREGS sregs;

  regs.x.ax = 0x1687;  /* DPMI installation check */
  INT86X(0x2F, &regs, &regs, &sregs);

  return (regs.x.ax == 0);  /* AX=0 if DPMI available */
#endif
}

static inline int dosmem_is_real_mode(void)
{
  return !dosmem_is_dpmi_mode();
}

#endif /* DOSMEM_COMPILER_H */
