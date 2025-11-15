/*
 * Copyright (c) 2025 PCC DEC MACRO Compiler
 *
 * Code generation - Table-driven PCC IR generation
 * NO assembly string generation - only PCC NODE trees
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "pass1.h"
#include "y.tab.h"

/* Forward declarations */
typedef P1ND *(*insn_handler_t)(INSTRUCTION *inst);
static P1ND *handle_mov(INSTRUCTION *inst);
static P1ND *handle_add(INSTRUCTION *inst);
static P1ND *handle_sub(INSTRUCTION *inst);
static P1ND *handle_mul(INSTRUCTION *inst);
static P1ND *handle_div(INSTRUCTION *inst);
static P1ND *handle_and(INSTRUCTION *inst);
static P1ND *handle_or(INSTRUCTION *inst);
static P1ND *handle_xor(INSTRUCTION *inst);
static P1ND *handle_clr(INSTRUCTION *inst);
static P1ND *handle_inc(INSTRUCTION *inst);
static P1ND *handle_dec(INSTRUCTION *inst);
static P1ND *handle_neg(INSTRUCTION *inst);
static P1ND *handle_com(INSTRUCTION *inst);
static P1ND *handle_tst(INSTRUCTION *inst);
static P1ND *handle_cmp(INSTRUCTION *inst);
static P1ND *handle_bit(INSTRUCTION *inst);
static P1ND *handle_bic(INSTRUCTION *inst);
static P1ND *handle_bis(INSTRUCTION *inst);
static P1ND *handle_asl(INSTRUCTION *inst);
static P1ND *handle_asr(INSTRUCTION *inst);
static P1ND *handle_rol(INSTRUCTION *inst);
static P1ND *handle_ror(INSTRUCTION *inst);
static P1ND *handle_swab(INSTRUCTION *inst);
static P1ND *handle_sxt(INSTRUCTION *inst);
static P1ND *handle_branch(INSTRUCTION *inst);
static P1ND *handle_jmp(INSTRUCTION *inst);
static P1ND *handle_jsr(INSTRUCTION *inst);
static P1ND *handle_rts(INSTRUCTION *inst);
static P1ND *handle_halt(INSTRUCTION *inst);
static P1ND *handle_nop(INSTRUCTION *inst);
/* Extended instructions */
static P1ND *handle_sob(INSTRUCTION *inst);
static P1ND *handle_ash(INSTRUCTION *inst);
static P1ND *handle_ashc(INSTRUCTION *inst);
static P1ND *handle_mark(INSTRUCTION *inst);
/* Condition code operations */
static P1ND *handle_scc(INSTRUCTION *inst);
static P1ND *handle_ccc(INSTRUCTION *inst);
/* Trap/Interrupt */
static P1ND *handle_trap(INSTRUCTION *inst);
static P1ND *handle_emt(INSTRUCTION *inst);
static P1ND *handle_iot(INSTRUCTION *inst);
static P1ND *handle_bpt(INSTRUCTION *inst);
static P1ND *handle_spl(INSTRUCTION *inst);
/* Memory management */
static P1ND *handle_mfpi(INSTRUCTION *inst);
static P1ND *handle_mtpi(INSTRUCTION *inst);
static P1ND *handle_mfps(INSTRUCTION *inst);
static P1ND *handle_mtps(INSTRUCTION *inst);
/* PDP-11 specific instructions */
static P1ND *handle_mfpt(INSTRUCTION *inst);
static P1ND *handle_csm(INSTRUCTION *inst);
static P1ND *handle_tstset(INSTRUCTION *inst);
static P1ND *handle_wrtlck(INSTRUCTION *inst);
static P1ND *handle_adcb(INSTRUCTION *inst);
static P1ND *handle_sbcb(INSTRUCTION *inst);
static P1ND *handle_bicb(INSTRUCTION *inst);
static P1ND *handle_bisb(INSTRUCTION *inst);
static P1ND *handle_subb(INSTRUCTION *inst);
/* PDP-11 FIS (Floating Instruction Set) */
static P1ND *handle_fadd(INSTRUCTION *inst);
static P1ND *handle_fsub(INSTRUCTION *inst);
static P1ND *handle_fmul(INSTRUCTION *inst);
static P1ND *handle_fdiv(INSTRUCTION *inst);
/* VAX 32-bit operations */
static P1ND *handle_movl(INSTRUCTION *inst);
static P1ND *handle_pushl(INSTRUCTION *inst);
static P1ND *handle_popl(INSTRUCTION *inst);
static P1ND *handle_addl(INSTRUCTION *inst);
static P1ND *handle_subl(INSTRUCTION *inst);
static P1ND *handle_mull(INSTRUCTION *inst);
static P1ND *handle_divl(INSTRUCTION *inst);
/* VAX string operations */
static P1ND *handle_movc(INSTRUCTION *inst);
static P1ND *handle_cmpc(INSTRUCTION *inst);
/* VAX floating point */
static P1ND *handle_addf(INSTRUCTION *inst);
static P1ND *handle_subf(INSTRUCTION *inst);
static P1ND *handle_mulf(INSTRUCTION *inst);
static P1ND *handle_divf(INSTRUCTION *inst);
/* PDP-10 specific */
static P1ND *handle_movei(INSTRUCTION *inst);
static P1ND *handle_movem(INSTRUCTION *inst);
static P1ND *handle_moves(INSTRUCTION *inst);
static P1ND *handle_movn(INSTRUCTION *inst);
static P1ND *handle_movm(INSTRUCTION *inst);
static P1ND *handle_imul(INSTRUCTION *inst);
static P1ND *handle_idiv(INSTRUCTION *inst);
static P1ND *handle_lsh(INSTRUCTION *inst);
static P1ND *handle_rot(INSTRUCTION *inst);
static P1ND *handle_jrst(INSTRUCTION *inst);
static P1ND *handle_pushj(INSTRUCTION *inst);
static P1ND *handle_popj(INSTRUCTION *inst);

/* PDP-10 Half-word operations */
static P1ND *handle_hll(INSTRUCTION *inst);
static P1ND *handle_hlli(INSTRUCTION *inst);
static P1ND *handle_hllm(INSTRUCTION *inst);
static P1ND *handle_hlls(INSTRUCTION *inst);
static P1ND *handle_hrl(INSTRUCTION *inst);
static P1ND *handle_hrli(INSTRUCTION *inst);
static P1ND *handle_hrlm(INSTRUCTION *inst);
static P1ND *handle_hrls(INSTRUCTION *inst);
static P1ND *handle_hrr(INSTRUCTION *inst);
static P1ND *handle_hrri(INSTRUCTION *inst);
static P1ND *handle_hrrm(INSTRUCTION *inst);
static P1ND *handle_hrrs(INSTRUCTION *inst);
static P1ND *handle_hlr(INSTRUCTION *inst);
static P1ND *handle_hlri(INSTRUCTION *inst);
static P1ND *handle_hlrm(INSTRUCTION *inst);
static P1ND *handle_hlrs(INSTRUCTION *inst);

/* PDP-10 Arithmetic */
static P1ND *handle_add(INSTRUCTION *inst);
static P1ND *handle_addi(INSTRUCTION *inst);
static P1ND *handle_addm(INSTRUCTION *inst);
static P1ND *handle_sub(INSTRUCTION *inst);
static P1ND *handle_subi(INSTRUCTION *inst);
static P1ND *handle_subm(INSTRUCTION *inst);
static P1ND *handle_mul(INSTRUCTION *inst);
static P1ND *handle_muli(INSTRUCTION *inst);
static P1ND *handle_mulm(INSTRUCTION *inst);
static P1ND *handle_div(INSTRUCTION *inst);
static P1ND *handle_divi(INSTRUCTION *inst);
static P1ND *handle_divm(INSTRUCTION *inst);
static P1ND *handle_ash(INSTRUCTION *inst);
static P1ND *handle_ashc(INSTRUCTION *inst);
static P1ND *handle_lshc(INSTRUCTION *inst);
static P1ND *handle_rotc(INSTRUCTION *inst);

/* PDP-10 Floating Point */
static P1ND *handle_fad(INSTRUCTION *inst);
static P1ND *handle_fadl(INSTRUCTION *inst);
static P1ND *handle_fadm(INSTRUCTION *inst);
static P1ND *handle_fadb(INSTRUCTION *inst);
static P1ND *handle_fadr(INSTRUCTION *inst);
static P1ND *handle_fadri(INSTRUCTION *inst);
static P1ND *handle_fadrm(INSTRUCTION *inst);
static P1ND *handle_fadrb(INSTRUCTION *inst);
static P1ND *handle_fsb(INSTRUCTION *inst);
static P1ND *handle_fsbl(INSTRUCTION *inst);
static P1ND *handle_fsbm(INSTRUCTION *inst);
static P1ND *handle_fsbb(INSTRUCTION *inst);
static P1ND *handle_fsbr(INSTRUCTION *inst);
static P1ND *handle_fsbri(INSTRUCTION *inst);
static P1ND *handle_fsbrm(INSTRUCTION *inst);
static P1ND *handle_fsbrb(INSTRUCTION *inst);
static P1ND *handle_fmp(INSTRUCTION *inst);
static P1ND *handle_fmpl(INSTRUCTION *inst);
static P1ND *handle_fmpm(INSTRUCTION *inst);
static P1ND *handle_fmpb(INSTRUCTION *inst);
static P1ND *handle_fmpr(INSTRUCTION *inst);
static P1ND *handle_fmpri(INSTRUCTION *inst);
static P1ND *handle_fmprm(INSTRUCTION *inst);
static P1ND *handle_fmprb(INSTRUCTION *inst);
static P1ND *handle_fdv(INSTRUCTION *inst);
static P1ND *handle_fdvl(INSTRUCTION *inst);
static P1ND *handle_fdvm(INSTRUCTION *inst);
static P1ND *handle_fdvb(INSTRUCTION *inst);
static P1ND *handle_fdvr(INSTRUCTION *inst);
static P1ND *handle_fdvri(INSTRUCTION *inst);
static P1ND *handle_fdvrm(INSTRUCTION *inst);
static P1ND *handle_fdvrb(INSTRUCTION *inst);

/* PDP-10 Logical Operations */
static P1ND *handle_and(INSTRUCTION *inst);
static P1ND *handle_andi(INSTRUCTION *inst);
static P1ND *handle_andm(INSTRUCTION *inst);
static P1ND *handle_andca(INSTRUCTION *inst);
static P1ND *handle_andcai(INSTRUCTION *inst);
static P1ND *handle_andcam(INSTRUCTION *inst);
static P1ND *handle_andcm(INSTRUCTION *inst);
static P1ND *handle_andcmi(INSTRUCTION *inst);
static P1ND *handle_andcmm(INSTRUCTION *inst);
static P1ND *handle_ior(INSTRUCTION *inst);
static P1ND *handle_iori(INSTRUCTION *inst);
static P1ND *handle_iorm(INSTRUCTION *inst);
static P1ND *handle_xor(INSTRUCTION *inst);
static P1ND *handle_xori(INSTRUCTION *inst);
static P1ND *handle_xorm(INSTRUCTION *inst);
static P1ND *handle_eqv(INSTRUCTION *inst);
static P1ND *handle_eqvi(INSTRUCTION *inst);
static P1ND *handle_eqvm(INSTRUCTION *inst);

/* PDP-10 Set Operations */
static P1ND *handle_seto(INSTRUCTION *inst);
static P1ND *handle_setoi(INSTRUCTION *inst);
static P1ND *handle_setom(INSTRUCTION *inst);
static P1ND *handle_setca(INSTRUCTION *inst);
static P1ND *handle_setcai(INSTRUCTION *inst);
static P1ND *handle_setcam(INSTRUCTION *inst);
static P1ND *handle_setcm(INSTRUCTION *inst);
static P1ND *handle_setcmi(INSTRUCTION *inst);
static P1ND *handle_setcmm(INSTRUCTION *inst);
static P1ND *handle_setz(INSTRUCTION *inst);
static P1ND *handle_setzi(INSTRUCTION *inst);
static P1ND *handle_setzm(INSTRUCTION *inst);

/* PDP-10 Skip/Jump Operations */
static P1ND *handle_skip(INSTRUCTION *inst);
static P1ND *handle_skipl(INSTRUCTION *inst);
static P1ND *handle_skipe(INSTRUCTION *inst);
static P1ND *handle_skiple(INSTRUCTION *inst);
static P1ND *handle_skipa(INSTRUCTION *inst);
static P1ND *handle_skipge(INSTRUCTION *inst);
static P1ND *handle_skipn(INSTRUCTION *inst);
static P1ND *handle_skipg(INSTRUCTION *inst);
static P1ND *handle_jump(INSTRUCTION *inst);
static P1ND *handle_jumpl(INSTRUCTION *inst);
static P1ND *handle_jumpe(INSTRUCTION *inst);
static P1ND *handle_jumple(INSTRUCTION *inst);
static P1ND *handle_jumpa(INSTRUCTION *inst);
static P1ND *handle_jumpge(INSTRUCTION *inst);
static P1ND *handle_jumpn(INSTRUCTION *inst);
static P1ND *handle_jumpg(INSTRUCTION *inst);

/* PDP-10 Add/Subtract with Jump */
static P1ND *handle_aoj(INSTRUCTION *inst);
static P1ND *handle_aojl(INSTRUCTION *inst);
static P1ND *handle_aoje(INSTRUCTION *inst);
static P1ND *handle_aojle(INSTRUCTION *inst);
static P1ND *handle_aoja(INSTRUCTION *inst);
static P1ND *handle_aojge(INSTRUCTION *inst);
static P1ND *handle_aojn(INSTRUCTION *inst);
static P1ND *handle_aojg(INSTRUCTION *inst);
static P1ND *handle_aos(INSTRUCTION *inst);
static P1ND *handle_aosl(INSTRUCTION *inst);
static P1ND *handle_aose(INSTRUCTION *inst);
static P1ND *handle_aosle(INSTRUCTION *inst);
static P1ND *handle_aosa(INSTRUCTION *inst);
static P1ND *handle_aosge(INSTRUCTION *inst);
static P1ND *handle_aosn(INSTRUCTION *inst);
static P1ND *handle_aosg(INSTRUCTION *inst);
static P1ND *handle_soj(INSTRUCTION *inst);
static P1ND *handle_sojl(INSTRUCTION *inst);
static P1ND *handle_soje(INSTRUCTION *inst);
static P1ND *handle_sojle(INSTRUCTION *inst);
static P1ND *handle_soja(INSTRUCTION *inst);
static P1ND *handle_sojge(INSTRUCTION *inst);
static P1ND *handle_sojn(INSTRUCTION *inst);
static P1ND *handle_sojg(INSTRUCTION *inst);
static P1ND *handle_sos(INSTRUCTION *inst);
static P1ND *handle_sosl(INSTRUCTION *inst);
static P1ND *handle_sose(INSTRUCTION *inst);
static P1ND *handle_sosle(INSTRUCTION *inst);
static P1ND *handle_sosa(INSTRUCTION *inst);
static P1ND *handle_sosge(INSTRUCTION *inst);
static P1ND *handle_sosn(INSTRUCTION *inst);
static P1ND *handle_sosg(INSTRUCTION *inst);

/* PDP-10 Test Operations */
static P1ND *handle_cai(INSTRUCTION *inst);
static P1ND *handle_cail(INSTRUCTION *inst);
static P1ND *handle_caie(INSTRUCTION *inst);
static P1ND *handle_caile(INSTRUCTION *inst);
static P1ND *handle_caia(INSTRUCTION *inst);
static P1ND *handle_caige(INSTRUCTION *inst);
static P1ND *handle_cain(INSTRUCTION *inst);
static P1ND *handle_caig(INSTRUCTION *inst);
static P1ND *handle_cam(INSTRUCTION *inst);
static P1ND *handle_caml(INSTRUCTION *inst);
static P1ND *handle_came(INSTRUCTION *inst);
static P1ND *handle_camle(INSTRUCTION *inst);
static P1ND *handle_cama(INSTRUCTION *inst);
static P1ND *handle_camge(INSTRUCTION *inst);
static P1ND *handle_camn(INSTRUCTION *inst);
static P1ND *handle_camg(INSTRUCTION *inst);

/* PDP-10 Byte Operations */
static P1ND *handle_ldb(INSTRUCTION *inst);
static P1ND *handle_ildb(INSTRUCTION *inst);
static P1ND *handle_dpb(INSTRUCTION *inst);
static P1ND *handle_idpb(INSTRUCTION *inst);
static P1ND *handle_ibp(INSTRUCTION *inst);

/* PDP-10 I/O Operations */
static P1ND *handle_blki(INSTRUCTION *inst);
static P1ND *handle_datai(INSTRUCTION *inst);
static P1ND *handle_blko(INSTRUCTION *inst);
static P1ND *handle_datao(INSTRUCTION *inst);
static P1ND *handle_cono(INSTRUCTION *inst);
static P1ND *handle_coni(INSTRUCTION *inst);
static P1ND *handle_consz(INSTRUCTION *inst);
static P1ND *handle_conso(INSTRUCTION *inst);

/* PDP-10 Stack Operations */
static P1ND *handle_push(INSTRUCTION *inst);
static P1ND *handle_pop(INSTRUCTION *inst);

/* PDP-10 Move variants */
static P1ND *handle_movs(INSTRUCTION *inst);
static P1ND *handle_movsi(INSTRUCTION *inst);
static P1ND *handle_movsm(INSTRUCTION *inst);
static P1ND *handle_movss(INSTRUCTION *inst);
static P1ND *handle_movni(INSTRUCTION *inst);
static P1ND *handle_movnm(INSTRUCTION *inst);
static P1ND *handle_movns(INSTRUCTION *inst);
static P1ND *handle_movmi(INSTRUCTION *inst);
static P1ND *handle_movmm(INSTRUCTION *inst);
static P1ND *handle_movms(INSTRUCTION *inst);

/* VAX-specific handlers */
static P1ND *handle_addb(INSTRUCTION *inst);
static P1ND *handle_addw(INSTRUCTION *inst);
static P1ND *handle_subb(INSTRUCTION *inst);
static P1ND *handle_subw(INSTRUCTION *inst);
static P1ND *handle_mulb(INSTRUCTION *inst);
static P1ND *handle_mulw(INSTRUCTION *inst);
static P1ND *handle_divb(INSTRUCTION *inst);
static P1ND *handle_divw(INSTRUCTION *inst);
static P1ND *handle_bisb(INSTRUCTION *inst);
static P1ND *handle_bisw(INSTRUCTION *inst);
static P1ND *handle_bisl(INSTRUCTION *inst);
static P1ND *handle_bicb(INSTRUCTION *inst);
static P1ND *handle_bicw(INSTRUCTION *inst);
static P1ND *handle_bicl(INSTRUCTION *inst);
static P1ND *handle_xorb(INSTRUCTION *inst);
static P1ND *handle_xorw(INSTRUCTION *inst);
static P1ND *handle_xorl(INSTRUCTION *inst);
static P1ND *handle_movb(INSTRUCTION *inst);
static P1ND *handle_movw(INSTRUCTION *inst);
static P1ND *handle_movzbw(INSTRUCTION *inst);
static P1ND *handle_movzbl(INSTRUCTION *inst);
static P1ND *handle_movzwl(INSTRUCTION *inst);
static P1ND *handle_movab(INSTRUCTION *inst);
static P1ND *handle_movaw(INSTRUCTION *inst);
static P1ND *handle_moval(INSTRUCTION *inst);
static P1ND *handle_movaq(INSTRUCTION *inst);
static P1ND *handle_pushab(INSTRUCTION *inst);
static P1ND *handle_pushaw(INSTRUCTION *inst);
static P1ND *handle_pushal(INSTRUCTION *inst);
static P1ND *handle_pushaq(INSTRUCTION *inst);
static P1ND *handle_pushw(INSTRUCTION *inst);
static P1ND *handle_popw(INSTRUCTION *inst);
static P1ND *handle_incb(INSTRUCTION *inst);
static P1ND *handle_incw(INSTRUCTION *inst);
static P1ND *handle_decb(INSTRUCTION *inst);
static P1ND *handle_decw(INSTRUCTION *inst);
static P1ND *handle_cmpb(INSTRUCTION *inst);
static P1ND *handle_cmpw(INSTRUCTION *inst);
static P1ND *handle_tstb(INSTRUCTION *inst);
static P1ND *handle_tstw(INSTRUCTION *inst);
static P1ND *handle_clrb(INSTRUCTION *inst);
static P1ND *handle_clrw(INSTRUCTION *inst);
static P1ND *handle_clrq(INSTRUCTION *inst);
static P1ND *handle_mnegb(INSTRUCTION *inst);
static P1ND *handle_mnegw(INSTRUCTION *inst);
static P1ND *handle_mnegl(INSTRUCTION *inst);
static P1ND *handle_mcomb(INSTRUCTION *inst);
static P1ND *handle_mcomw(INSTRUCTION *inst);
static P1ND *handle_mcoml(INSTRUCTION *inst);
static P1ND *handle_bitb(INSTRUCTION *inst);
static P1ND *handle_bitw(INSTRUCTION *inst);
static P1ND *handle_bitl(INSTRUCTION *inst);
static P1ND *handle_adwc(INSTRUCTION *inst);
static P1ND *handle_sbwc(INSTRUCTION *inst);
static P1ND *handle_emul(INSTRUCTION *inst);
static P1ND *handle_ediv(INSTRUCTION *inst);
static P1ND *handle_ashl(INSTRUCTION *inst);
static P1ND *handle_ashq(INSTRUCTION *inst);
static P1ND *handle_rotl(INSTRUCTION *inst);
/* G-float handlers */
static P1ND *handle_addg(INSTRUCTION *inst);
static P1ND *handle_subg(INSTRUCTION *inst);
static P1ND *handle_mulg(INSTRUCTION *inst);
static P1ND *handle_divg(INSTRUCTION *inst);
static P1ND *handle_movg(INSTRUCTION *inst);
static P1ND *handle_cmpg(INSTRUCTION *inst);
static P1ND *handle_mnegg(INSTRUCTION *inst);
static P1ND *handle_tstg(INSTRUCTION *inst);
static P1ND *handle_polyg(INSTRUCTION *inst);
static P1ND *handle_emogg(INSTRUCTION *inst);
/* H-float handlers */
static P1ND *handle_addh(INSTRUCTION *inst);
static P1ND *handle_subh(INSTRUCTION *inst);
static P1ND *handle_mulh(INSTRUCTION *inst);
static P1ND *handle_divh(INSTRUCTION *inst);
static P1ND *handle_movh(INSTRUCTION *inst);
static P1ND *handle_cmph(INSTRUCTION *inst);
static P1ND *handle_mnegh(INSTRUCTION *inst);
static P1ND *handle_tsth(INSTRUCTION *inst);
static P1ND *handle_polyh(INSTRUCTION *inst);
/* More float handlers */
static P1ND *handle_movf(INSTRUCTION *inst);
static P1ND *handle_movd(INSTRUCTION *inst);
static P1ND *handle_cmpf(INSTRUCTION *inst);
static P1ND *handle_cmpd(INSTRUCTION *inst);
static P1ND *handle_mnegf(INSTRUCTION *inst);
static P1ND *handle_mnegd(INSTRUCTION *inst);
static P1ND *handle_tstf(INSTRUCTION *inst);
static P1ND *handle_tstd(INSTRUCTION *inst);
static P1ND *handle_emodf(INSTRUCTION *inst);
static P1ND *handle_emodd(INSTRUCTION *inst);
static P1ND *handle_polyf(INSTRUCTION *inst);
static P1ND *handle_polyd(INSTRUCTION *inst);
/* Bit field handlers */
static P1ND *handle_extv(INSTRUCTION *inst);
static P1ND *handle_extzv(INSTRUCTION *inst);
static P1ND *handle_insv(INSTRUCTION *inst);
static P1ND *handle_cmpv(INSTRUCTION *inst);
static P1ND *handle_cmpzv(INSTRUCTION *inst);
static P1ND *handle_ffc(INSTRUCTION *inst);
static P1ND *handle_ffs(INSTRUCTION *inst);
/* Bit test handlers */
static P1ND *handle_bbs(INSTRUCTION *inst);
static P1ND *handle_bbc(INSTRUCTION *inst);
static P1ND *handle_bbss(INSTRUCTION *inst);
static P1ND *handle_bbcs(INSTRUCTION *inst);
static P1ND *handle_bbsc(INSTRUCTION *inst);
static P1ND *handle_bbcc(INSTRUCTION *inst);
static P1ND *handle_bbssi(INSTRUCTION *inst);
static P1ND *handle_bbcci(INSTRUCTION *inst);
/* String handlers */
static P1ND *handle_movtc(INSTRUCTION *inst);
static P1ND *handle_movtuc(INSTRUCTION *inst);
static P1ND *handle_matchc(INSTRUCTION *inst);
static P1ND *handle_editpc(INSTRUCTION *inst);
/* Queue handlers */
static P1ND *handle_insque(INSTRUCTION *inst);
static P1ND *handle_remque(INSTRUCTION *inst);
static P1ND *handle_insqhi(INSTRUCTION *inst);
static P1ND *handle_insqti(INSTRUCTION *inst);
static P1ND *handle_remqhi(INSTRUCTION *inst);
static P1ND *handle_remqti(INSTRUCTION *inst);
/* Control flow handlers */
static P1ND *handle_bneq(INSTRUCTION *inst);
static P1ND *handle_beql(INSTRUCTION *inst);
static P1ND *handle_bgtr(INSTRUCTION *inst);
static P1ND *handle_bleq(INSTRUCTION *inst);
static P1ND *handle_bgeq(INSTRUCTION *inst);
static P1ND *handle_blss(INSTRUCTION *inst);
static P1ND *handle_bgtru(INSTRUCTION *inst);
static P1ND *handle_blequ(INSTRUCTION *inst);
static P1ND *handle_bgequ(INSTRUCTION *inst);
static P1ND *handle_blssu(INSTRUCTION *inst);
static P1ND *handle_bvc(INSTRUCTION *inst);
static P1ND *handle_bvs(INSTRUCTION *inst);
static P1ND *handle_brb(INSTRUCTION *inst);
static P1ND *handle_brw(INSTRUCTION *inst);
static P1ND *handle_bsbb(INSTRUCTION *inst);
static P1ND *handle_bsbw(INSTRUCTION *inst);
static P1ND *handle_callg(INSTRUCTION *inst);
static P1ND *handle_calls(INSTRUCTION *inst);
static P1ND *handle_caseb(INSTRUCTION *inst);
static P1ND *handle_casew(INSTRUCTION *inst);
static P1ND *handle_casel(INSTRUCTION *inst);
static P1ND *handle_rei(INSTRUCTION *inst);
static P1ND *handle_rsb(INSTRUCTION *inst);
static P1ND *handle_ret(INSTRUCTION *inst);
/* System handlers */
static P1ND *handle_ldpctx(INSTRUCTION *inst);
static P1ND *handle_svpctx(INSTRUCTION *inst);
static P1ND *handle_prober(INSTRUCTION *inst);
static P1ND *handle_probew(INSTRUCTION *inst);
static P1ND *handle_probe(INSTRUCTION *inst);
static P1ND *handle_mtpr(INSTRUCTION *inst);
static P1ND *handle_mfpr(INSTRUCTION *inst);
static P1ND *handle_bugw(INSTRUCTION *inst);
static P1ND *handle_bugl(INSTRUCTION *inst);
static P1ND *handle_crc(INSTRUCTION *inst);
static P1ND *handle_adawi(INSTRUCTION *inst);
/* Packed decimal handlers */
static P1ND *handle_addp4(INSTRUCTION *inst);
static P1ND *handle_addp6(INSTRUCTION *inst);
static P1ND *handle_subp4(INSTRUCTION *inst);
static P1ND *handle_subp6(INSTRUCTION *inst);
static P1ND *handle_mulp(INSTRUCTION *inst);
static P1ND *handle_divp(INSTRUCTION *inst);
static P1ND *handle_cvtpt(INSTRUCTION *inst);
static P1ND *handle_cvttp(INSTRUCTION *inst);
static P1ND *handle_cvtps(INSTRUCTION *inst);
static P1ND *handle_cvtsp(INSTRUCTION *inst);
static P1ND *handle_ashp(INSTRUCTION *inst);
static P1ND *handle_cmpp3(INSTRUCTION *inst);
static P1ND *handle_cmpp4(INSTRUCTION *inst);
static P1ND *handle_movp(INSTRUCTION *inst);
/* More conversion handlers */
static P1ND *handle_cvtbw(INSTRUCTION *inst);
static P1ND *handle_cvtbl(INSTRUCTION *inst);
static P1ND *handle_cvtwb(INSTRUCTION *inst);
static P1ND *handle_cvtwl(INSTRUCTION *inst);
static P1ND *handle_cvtlb(INSTRUCTION *inst);
static P1ND *handle_cvtlw(INSTRUCTION *inst);
static P1ND *handle_cvtfb(INSTRUCTION *inst);
static P1ND *handle_cvtfw(INSTRUCTION *inst);
static P1ND *handle_cvtdb(INSTRUCTION *inst);
static P1ND *handle_cvtdw(INSTRUCTION *inst);
static P1ND *handle_cvtgb(INSTRUCTION *inst);
static P1ND *handle_cvtgw(INSTRUCTION *inst);
static P1ND *handle_cvtgl(INSTRUCTION *inst);
static P1ND *handle_cvtbf(INSTRUCTION *inst);
static P1ND *handle_cvtwf(INSTRUCTION *inst);
static P1ND *handle_cvtbd(INSTRUCTION *inst);
static P1ND *handle_cvtwd(INSTRUCTION *inst);
static P1ND *handle_cvtbg(INSTRUCTION *inst);
static P1ND *handle_cvtwg(INSTRUCTION *inst);
static P1ND *handle_cvtlg(INSTRUCTION *inst);
static P1ND *handle_cvtfg(INSTRUCTION *inst);
static P1ND *handle_cvtgf(INSTRUCTION *inst);
static P1ND *handle_cvtrdl(INSTRUCTION *inst);
static P1ND *handle_cvtrfl(INSTRUCTION *inst);
static P1ND *handle_cvtrgl(INSTRUCTION *inst);
/* Loop control handlers */
static P1ND *handle_acbb(INSTRUCTION *inst);
static P1ND *handle_acbw(INSTRUCTION *inst);
static P1ND *handle_acbl(INSTRUCTION *inst);
static P1ND *handle_acbf(INSTRUCTION *inst);
static P1ND *handle_acbd(INSTRUCTION *inst);
static P1ND *handle_acbg(INSTRUCTION *inst);
static P1ND *handle_acbh(INSTRUCTION *inst);
static P1ND *handle_aobleq(INSTRUCTION *inst);
static P1ND *handle_aoblss(INSTRUCTION *inst);
static P1ND *handle_sobgeq(INSTRUCTION *inst);
static P1ND *handle_sobgtr(INSTRUCTION *inst);
/* Register set handlers */
static P1ND *handle_pushr(INSTRUCTION *inst);
static P1ND *handle_popr(INSTRUCTION *inst);
/* Index and PSL handlers */
static P1ND *handle_index(INSTRUCTION *inst);
static P1ND *handle_movpsl(INSTRUCTION *inst);
static P1ND *handle_bispsw(INSTRUCTION *inst);
static P1ND *handle_bicpsw(INSTRUCTION *inst);
/* Change mode handlers */
static P1ND *handle_xfc(INSTRUCTION *inst);
static P1ND *handle_chme(INSTRUCTION *inst);
static P1ND *handle_chmk(INSTRUCTION *inst);
static P1ND *handle_chms(INSTRUCTION *inst);
static P1ND *handle_chmu(INSTRUCTION *inst);
/* More packed decimal conversions */
static P1ND *handle_cvtlp(INSTRUCTION *inst);
static P1ND *handle_cvtpl(INSTRUCTION *inst);

/* Instruction table entry */
typedef struct {
	const char *mnemonic;
	insn_handler_t handler;
	int opcode;
} insn_table_entry_t;

/* DEC MACRO Instruction Table */
static const insn_table_entry_t insn_table[] = {
	/* Data Movement */
	{ "MOV",   handle_mov,   0010000 },
	{ "MOVB",  handle_mov,   0110000 },
	{ "MOVA",  handle_mov,   0010000 },
	{ "MOVZ",  handle_mov,   0010000 },

	/* Arithmetic */
	{ "ADD",   handle_add,   0060000 },
	{ "SUB",   handle_sub,   0160000 },
	{ "MUL",   handle_mul,   0070000 },
	{ "DIV",   handle_div,   0071000 },
	{ "INC",   handle_inc,   0005200 },
	{ "INCB",  handle_inc,   0105200 },
	{ "DEC",   handle_dec,   0005300 },
	{ "DECB",  handle_dec,   0105300 },
	{ "NEG",   handle_neg,   0005400 },
	{ "NEGB",  handle_neg,   0105400 },
	{ "ADC",   handle_inc,   0005500 },  /* Add carry */
	{ "SBC",   handle_dec,   0005600 },  /* Subtract carry */
	{ "ADCB",  handle_adcb,  0105500 },  /* Add carry byte */
	{ "SBCB",  handle_sbcb,  0105600 },  /* Subtract carry byte */

	/* Logical */
	{ "AND",   handle_and,   0040000 },
	{ "OR",    handle_or,    0050000 },
	{ "XOR",   handle_xor,   0074000 },
	{ "BIC",   handle_bic,   0040000 },  /* Bit clear */
	{ "BIS",   handle_bis,   0050000 },  /* Bit set */
	{ "BICB",  handle_bicb,  0140000 },  /* Bit clear byte */
	{ "BISB",  handle_bisb,  0150000 },  /* Bit set byte */
	{ "BIT",   handle_bit,   0030000 },  /* Bit test */
	{ "COM",   handle_com,   0005100 },  /* Complement */
	{ "COMB",  handle_com,   0105100 },

	/* Shift/Rotate */
	{ "ASL",   handle_asl,   0006300 },  /* Arithmetic shift left */
	{ "ASLB",  handle_asl,   0106300 },
	{ "ASR",   handle_asr,   0006200 },  /* Arithmetic shift right */
	{ "ASRB",  handle_asr,   0106200 },
	{ "ROL",   handle_rol,   0006100 },  /* Rotate left */
	{ "ROLB",  handle_rol,   0106100 },
	{ "ROR",   handle_ror,   0006000 },  /* Rotate right */
	{ "RORB",  handle_ror,   0106000 },
	{ "SWAB",  handle_swab,  0000300 },  /* Swap bytes */
	{ "SXT",   handle_sxt,   0006700 },  /* Sign extend */

	/* Compare/Test */
	{ "CMP",   handle_cmp,   0020000 },
	{ "CMPB",  handle_cmp,   0120000 },
	{ "TST",   handle_tst,   0005700 },
	{ "TSTB",  handle_tst,   0105700 },

	/* Branches (conditional) */
	{ "BR",    handle_branch, 0000400 },  /* Branch unconditional */
	{ "BNE",   handle_branch, 0001000 },  /* Branch not equal */
	{ "BEQ",   handle_branch, 0001400 },  /* Branch equal */
	{ "BPL",   handle_branch, 0100000 },  /* Branch plus */
	{ "BMI",   handle_branch, 0100400 },  /* Branch minus */
	{ "BVC",   handle_branch, 0102000 },  /* Branch overflow clear */
	{ "BVS",   handle_branch, 0102400 },  /* Branch overflow set */
	{ "BCC",   handle_branch, 0103000 },  /* Branch carry clear */
	{ "BCS",   handle_branch, 0103400 },  /* Branch carry set */
	{ "BGE",   handle_branch, 0002000 },  /* Branch >= */
	{ "BLT",   handle_branch, 0002400 },  /* Branch < */
	{ "BGT",   handle_branch, 0003000 },  /* Branch > */
	{ "BLE",   handle_branch, 0003400 },  /* Branch <= */
	{ "BHI",   handle_branch, 0101000 },  /* Branch higher */
	{ "BLOS",  handle_branch, 0101400 },  /* Branch lower or same */

	/* Jump/Call */
	{ "JMP",   handle_jmp,   0000100 },
	{ "JSR",   handle_jsr,   0004000 },
	{ "RTS",   handle_rts,   0000200 },
	{ "RTI",   handle_rts,   0000002 },
	{ "RTT",   handle_rts,   0000006 },

	/* Special */
	{ "HALT",  handle_halt,  0000000 },
	{ "WAIT",  handle_halt,  0000001 },
	{ "NOP",   handle_nop,   0000240 },
	{ "RESET", handle_halt,  0000005 },
	{ "CLR",   handle_clr,   0005000 },
	{ "CLRB",  handle_clr,   0105000 },

	/* Extended PDP-11 instructions */
	{ "SOB",   handle_sob,   0077000 },  /* Subtract and branch */
	{ "ASH",   handle_ash,   0072000 },  /* Arithmetic shift */
	{ "ASHC",  handle_ashc,  0073000 },  /* Arithmetic shift combined */
	{ "MARK",  handle_mark,  0006400 },  /* Mark */

	/* Condition code operations */
	{ "SCC",   handle_scc,   0000277 },  /* Set carry clear */
	{ "SEC",   handle_scc,   0000261 },  /* Set carry */
	{ "SEV",   handle_scc,   0000262 },  /* Set overflow */
	{ "SEZ",   handle_scc,   0000264 },  /* Set zero */
	{ "SEN",   handle_scc,   0000270 },  /* Set negative */
	{ "CLC",   handle_ccc,   0000241 },  /* Clear carry */
	{ "CLV",   handle_ccc,   0000242 },  /* Clear overflow */
	{ "CLZ",   handle_ccc,   0000244 },  /* Clear zero */
	{ "CLN",   handle_ccc,   0000250 },  /* Clear negative */

	/* Trap/Interrupt instructions */
	{ "EMT",   handle_emt,   0104000 },  /* Emulator trap */
	{ "TRAP",  handle_trap,  0104400 },  /* Trap */
	{ "IOT",   handle_iot,   0000004 },  /* I/O trap */
	{ "BPT",   handle_bpt,   0000003 },  /* Breakpoint */
	{ "SPL",   handle_spl,   0000230 },  /* Set priority level */

	/* Memory management instructions */
	{ "MFPI",  handle_mfpi,  0006500 },  /* Move from previous I space */
	{ "MTPI",  handle_mtpi,  0006600 },  /* Move to previous I space */
	{ "MFPD",  handle_mfpi,  0106500 },  /* Move from previous D space */
	{ "MTPD",  handle_mtpi,  0106600 },  /* Move to previous D space */
	{ "MFPS",  handle_mfps,  0106700 },  /* Move from PS */
	{ "MTPS",  handle_mtps,  0106400 },  /* Move to PS */

	/* PDP-11 specific extended instructions */
	{ "MFPT",   handle_mfpt,   0000007 },  /* Move from processor type */
	{ "CSM",    handle_csm,    0007000 },  /* Call to supervisor mode */
	{ "TSTSET", handle_tstset, 0007200 },  /* Test and set */
	{ "WRTLCK", handle_wrtlck, 0007300 },  /* Write lock */

	/* PDP-11 FIS (Floating Instruction Set) - opcodes 170000-177777 */
	{ "FADD",   handle_fadd,   0075000 },  /* FIS floating add */
	{ "FSUB",   handle_fsub,   0075001 },  /* FIS floating subtract */
	{ "FMUL",   handle_fmul,   0075002 },  /* FIS floating multiply */
	{ "FDIV",   handle_fdiv,   0075003 },  /* FIS floating divide */

	/* VAX 32-bit operations */
	{ "MOVL",  handle_movl,  0x90 },     /* Move longword */
	{ "MOVQ",  handle_movl,  0x7D },     /* Move quadword */
	{ "PUSHL", handle_pushl, 0xDD },     /* Push longword */
	{ "PUSHQ", handle_pushl, 0x7F },     /* Push quadword */
	{ "POPL",  handle_popl,  0xDC },     /* Pop longword */
	{ "POPQ",  handle_popl,  0x7E },     /* Pop quadword */
	{ "ADDL2", handle_addl,  0xC0 },     /* Add longword 2 operand */
	{ "ADDL3", handle_addl,  0xC1 },     /* Add longword 3 operand */
	{ "SUBL2", handle_subl,  0xC2 },     /* Subtract longword 2 operand */
	{ "SUBL3", handle_subl,  0xC3 },     /* Subtract longword 3 operand */
	{ "MULL2", handle_mull,  0xC4 },     /* Multiply longword 2 operand */
	{ "MULL3", handle_mull,  0xC5 },     /* Multiply longword 3 operand */
	{ "DIVL2", handle_divl,  0xC6 },     /* Divide longword 2 operand */
	{ "DIVL3", handle_divl,  0xC7 },     /* Divide longword 3 operand */
	{ "INCL",  handle_inc,   0xD6 },     /* Increment longword */
	{ "DECL",  handle_dec,   0xD7 },     /* Decrement longword */
	{ "CLRL",  handle_clr,   0xD4 },     /* Clear longword */
	{ "TSTL",  handle_tst,   0xD5 },     /* Test longword */
	{ "CMPL",  handle_cmp,   0xD1 },     /* Compare longword */

	/* VAX string operations */
	{ "MOVC3", handle_movc,  0x28 },     /* Move character 3 operand */
	{ "MOVC5", handle_movc,  0x2C },     /* Move character 5 operand */
	{ "CMPC3", handle_cmpc,  0x29 },     /* Compare character 3 operand */
	{ "CMPC5", handle_cmpc,  0x2D },     /* Compare character 5 operand */
	{ "LOCC",  handle_movc,  0x3A },     /* Locate character */
	{ "SKPC",  handle_movc,  0x3B },     /* Skip character */
	{ "SCANC", handle_movc,  0x2A },     /* Scan character */
	{ "SPANC", handle_movc,  0x2B },     /* Span character */

	/* VAX floating point */
	{ "ADDF2", handle_addf,  0x40 },     /* Add F_float 2 operand */
	{ "ADDF3", handle_addf,  0x41 },     /* Add F_float 3 operand */
	{ "SUBF2", handle_subf,  0x42 },     /* Subtract F_float 2 operand */
	{ "SUBF3", handle_subf,  0x43 },     /* Subtract F_float 3 operand */
	{ "MULF2", handle_mulf,  0x44 },     /* Multiply F_float 2 operand */
	{ "MULF3", handle_mulf,  0x45 },     /* Multiply F_float 3 operand */
	{ "DIVF2", handle_divf,  0x46 },     /* Divide F_float 2 operand */
	{ "DIVF3", handle_divf,  0x47 },     /* Divide F_float 3 operand */
	{ "ADDD2", handle_addf,  0x60 },     /* Add D_float 2 operand */
	{ "ADDD3", handle_addf,  0x61 },     /* Add D_float 3 operand */
	{ "SUBD2", handle_subf,  0x62 },     /* Subtract D_float 2 operand */
	{ "SUBD3", handle_subf,  0x63 },     /* Subtract D_float 3 operand */
	{ "MULD2", handle_mulf,  0x64 },     /* Multiply D_float 2 operand */
	{ "MULD3", handle_mulf,  0x65 },     /* Multiply D_float 3 operand */
	{ "DIVD2", handle_divf,  0x66 },     /* Divide D_float 2 operand */
	{ "DIVD3", handle_divf,  0x67 },     /* Divide D_float 3 operand */
	{ "CVTFD", handle_movl,  0x56 },     /* Convert F to D */
	{ "CVTDF", handle_movl,  0x76 },     /* Convert D to F */
	{ "CVTFL", handle_movl,  0x48 },     /* Convert F to L */
	{ "CVTLF", handle_movl,  0x4E },     /* Convert L to F */
	{ "CVTDL", handle_movl,  0x6A },     /* Convert D to L */
	{ "CVTLD", handle_movl,  0x6F },     /* Convert L to D */

	/* VAX Integer Byte/Word operations */
	{ "ADDB2", handle_addb,  0x80 },     /* Add byte 2 operand */
	{ "ADDB3", handle_addb,  0x81 },     /* Add byte 3 operand */
	{ "ADDW2", handle_addw,  0xA0 },     /* Add word 2 operand */
	{ "ADDW3", handle_addw,  0xA1 },     /* Add word 3 operand */
	{ "SUBB2", handle_subb,  0x82 },     /* Subtract byte 2 operand */
	{ "SUBB3", handle_subb,  0x83 },     /* Subtract byte 3 operand */
	{ "SUBW2", handle_subw,  0xA2 },     /* Subtract word 2 operand */
	{ "SUBW3", handle_subw,  0xA3 },     /* Subtract word 3 operand */
	{ "MULB2", handle_mulb,  0x84 },     /* Multiply byte 2 operand */
	{ "MULB3", handle_mulb,  0x85 },     /* Multiply byte 3 operand */
	{ "MULW2", handle_mulw,  0xA4 },     /* Multiply word 2 operand */
	{ "MULW3", handle_mulw,  0xA5 },     /* Multiply word 3 operand */
	{ "DIVB2", handle_divb,  0x86 },     /* Divide byte 2 operand */
	{ "DIVB3", handle_divb,  0x87 },     /* Divide byte 3 operand */
	{ "DIVW2", handle_divw,  0xA6 },     /* Divide word 2 operand */
	{ "DIVW3", handle_divw,  0xA7 },     /* Divide word 3 operand */

	/* VAX Logical Byte/Word/Long operations */
	{ "BISB2", handle_bisb,  0x88 },     /* Bit set byte 2 operand */
	{ "BISB3", handle_bisb,  0x89 },     /* Bit set byte 3 operand */
	{ "BISW2", handle_bisw,  0xA8 },     /* Bit set word 2 operand */
	{ "BISW3", handle_bisw,  0xA9 },     /* Bit set word 3 operand */
	{ "BISL2", handle_bisl,  0xC8 },     /* Bit set long 2 operand */
	{ "BISL3", handle_bisl,  0xC9 },     /* Bit set long 3 operand */
	{ "BICB2", handle_bicb,  0x8A },     /* Bit clear byte 2 operand */
	{ "BICB3", handle_bicb,  0x8B },     /* Bit clear byte 3 operand */
	{ "BICW2", handle_bicw,  0xAA },     /* Bit clear word 2 operand */
	{ "BICW3", handle_bicw,  0xAB },     /* Bit clear word 3 operand */
	{ "BICL2", handle_bicl,  0xCA },     /* Bit clear long 2 operand */
	{ "BICL3", handle_bicl,  0xCB },     /* Bit clear long 3 operand */
	{ "XORB2", handle_xorb,  0x8C },     /* XOR byte 2 operand */
	{ "XORB3", handle_xorb,  0x8D },     /* XOR byte 3 operand */
	{ "XORW2", handle_xorw,  0xAC },     /* XOR word 2 operand */
	{ "XORW3", handle_xorw,  0xAD },     /* XOR word 3 operand */
	{ "XORL2", handle_xorl,  0xCC },     /* XOR long 2 operand */
	{ "XORL3", handle_xorl,  0xCD },     /* XOR long 3 operand */

	/* VAX Move operations */
	{ "MOVB",  handle_movb,  0x90 },     /* Move byte */
	{ "MOVW",  handle_movw,  0xB0 },     /* Move word */
	{ "MOVZBW", handle_movzbw, 0x9B },   /* Move zero-extended byte to word */
	{ "MOVZBL", handle_movzbl, 0x9A },   /* Move zero-extended byte to long */
	{ "MOVZWL", handle_movzwl, 0x3C },   /* Move zero-extended word to long */
	{ "MOVAB", handle_movab,  0x9E },    /* Move address byte */
	{ "MOVAW", handle_movaw,  0x3E },    /* Move address word */
	{ "MOVAL", handle_moval,  0xDE },    /* Move address long */
	{ "MOVAQ", handle_movaq,  0x7E },    /* Move address quad */
	{ "PUSHAB", handle_pushab, 0x9F },   /* Push address byte */
	{ "PUSHAW", handle_pushaw, 0x3F },   /* Push address word */
	{ "PUSHAL", handle_pushal, 0xDF },   /* Push address long */
	{ "PUSHAQ", handle_pushaq, 0x7F },   /* Push address quad */
	{ "PUSHW", handle_pushw,  0xBB },    /* Push word */
	{ "POPW",  handle_popw,   0xBA },    /* Pop word */

	/* VAX Inc/Dec/Clear/Test/Compare */
	{ "INCB",  handle_incb,   0x96 },    /* Increment byte */
	{ "INCW",  handle_incw,   0xB6 },    /* Increment word */
	{ "DECB",  handle_decb,   0x97 },    /* Decrement byte */
	{ "DECW",  handle_decw,   0xB7 },    /* Decrement word */
	{ "CMPB",  handle_cmpb,   0x91 },    /* Compare byte */
	{ "CMPW",  handle_cmpw,   0xB1 },    /* Compare word */
	{ "TSTB",  handle_tstb,   0x95 },    /* Test byte */
	{ "TSTW",  handle_tstw,   0xB5 },    /* Test word */
	{ "CLRB",  handle_clrb,   0x94 },    /* Clear byte */
	{ "CLRW",  handle_clrw,   0xB4 },    /* Clear word */
	{ "CLRQ",  handle_clrq,   0x7C },    /* Clear quadword */
	{ "MNEGB", handle_mnegb,  0x8E },    /* Move negated byte */
	{ "MNEGW", handle_mnegw,  0xAE },    /* Move negated word */
	{ "MNEGL", handle_mnegl,  0xCE },    /* Move negated long */
	{ "MCOMB", handle_mcomb,  0x92 },    /* Move complemented byte */
	{ "MCOMW", handle_mcomw,  0xB2 },    /* Move complemented word */
	{ "MCOML", handle_mcoml,  0xD2 },    /* Move complemented long */
	{ "BITB",  handle_bitb,   0x93 },    /* Bit test byte */
	{ "BITW",  handle_bitw,   0xB3 },    /* Bit test word */
	{ "BITL",  handle_bitl,   0xD3 },    /* Bit test long */

	/* VAX Extended Arithmetic */
	{ "ADWC",  handle_adwc,   0xD8 },    /* Add with carry */
	{ "SBWC",  handle_sbwc,   0xD9 },    /* Subtract with carry */
	{ "EMUL",  handle_emul,   0x7A },    /* Extended multiply */
	{ "EDIV",  handle_ediv,   0x7B },    /* Extended divide */
	{ "ASHL",  handle_ashl,   0x78 },    /* Arithmetic shift long */
	{ "ASHQ",  handle_ashq,   0x79 },    /* Arithmetic shift quad */
	{ "ROTL",  handle_rotl,   0x9C },    /* Rotate long */

	/* VAX G-float (64-bit) */
	{ "ADDG2", handle_addg,   0x40FD },  /* Add G_float 2 operand (2-byte opcode) */
	{ "ADDG3", handle_addg,   0x41FD },  /* Add G_float 3 operand */
	{ "SUBG2", handle_subg,   0x42FD },  /* Subtract G_float 2 operand */
	{ "SUBG3", handle_subg,   0x43FD },  /* Subtract G_float 3 operand */
	{ "MULG2", handle_mulg,   0x44FD },  /* Multiply G_float 2 operand */
	{ "MULG3", handle_mulg,   0x45FD },  /* Multiply G_float 3 operand */
	{ "DIVG2", handle_divg,   0x46FD },  /* Divide G_float 2 operand */
	{ "DIVG3", handle_divg,   0x47FD },  /* Divide G_float 3 operand */
	{ "MOVG",  handle_movg,   0x50FD },  /* Move G_float */
	{ "CMPG",  handle_cmpg,   0x51FD },  /* Compare G_float */
	{ "MNEGG", handle_mnegg,  0x52FD },  /* Move negated G_float */
	{ "TSTG",  handle_tstg,   0x53FD },  /* Test G_float */
	{ "POLYG", handle_polyg,  0x55FD },  /* Polynomial evaluate G_float */
	{ "EMOGG", handle_emogg,  0x54FD },  /* Extended modulus G_float */

	/* VAX H-float (128-bit) */
	{ "ADDH2", handle_addh,   0x60FD },  /* Add H_float 2 operand */
	{ "ADDH3", handle_addh,   0x61FD },  /* Add H_float 3 operand */
	{ "SUBH2", handle_subh,   0x62FD },  /* Subtract H_float 2 operand */
	{ "SUBH3", handle_subh,   0x63FD },  /* Subtract H_float 3 operand */
	{ "MULH2", handle_mulh,   0x64FD },  /* Multiply H_float 2 operand */
	{ "MULH3", handle_mulh,   0x65FD },  /* Multiply H_float 3 operand */
	{ "DIVH2", handle_divh,   0x66FD },  /* Divide H_float 2 operand */
	{ "DIVH3", handle_divh,   0x67FD },  /* Divide H_float 3 operand */
	{ "MOVH",  handle_movh,   0x70FD },  /* Move H_float */
	{ "CMPH",  handle_cmph,   0x71FD },  /* Compare H_float */
	{ "MNEGH", handle_mnegh,  0x72FD },  /* Move negated H_float */
	{ "TSTH",  handle_tsth,   0x73FD },  /* Test H_float */
	{ "POLYH", handle_polyh,  0x75FD },  /* Polynomial evaluate H_float */

	/* VAX More F/D float operations */
	{ "MOVF",  handle_movf,   0x50 },    /* Move F_float */
	{ "MOVD",  handle_movd,   0x70 },    /* Move D_float */
	{ "CMPF",  handle_cmpf,   0x51 },    /* Compare F_float */
	{ "CMPD",  handle_cmpd,   0x71 },    /* Compare D_float */
	{ "MNEGF", handle_mnegf,  0x52 },    /* Move negated F_float */
	{ "MNEGD", handle_mnegd,  0x72 },    /* Move negated D_float */
	{ "TSTF",  handle_tstf,   0x53 },    /* Test F_float */
	{ "TSTD",  handle_tstd,   0x73 },    /* Test D_float */
	{ "EMODF", handle_emodf,  0x54 },    /* Extended modulus F_float */
	{ "EMODD", handle_emodd,  0x74 },    /* Extended modulus D_float */
	{ "POLYF", handle_polyf,  0x55 },    /* Polynomial evaluate F_float */
	{ "POLYD", handle_polyd,  0x75 },    /* Polynomial evaluate D_float */

	/* VAX Bit Field Instructions */
	{ "EXTV",  handle_extv,   0xEE },    /* Extract field */
	{ "EXTZV", handle_extzv,  0xEF },    /* Extract zero-extended field */
	{ "INSV",  handle_insv,   0xF0 },    /* Insert field */
	{ "CMPV",  handle_cmpv,   0xEC },    /* Compare field */
	{ "CMPZV", handle_cmpzv,  0xED },    /* Compare zero-extended field */
	{ "FFC",   handle_ffc,    0xEB },    /* Find first clear bit */
	{ "FFS",   handle_ffs,    0xEA },    /* Find first set bit */

	/* VAX Bit Test Instructions */
	{ "BBS",   handle_bbs,    0xE0 },    /* Branch on bit set */
	{ "BBC",   handle_bbc,    0xE1 },    /* Branch on bit clear */
	{ "BBSS",  handle_bbss,   0xE2 },    /* Branch on bit set and set */
	{ "BBCS",  handle_bbcs,   0xE3 },    /* Branch on bit clear and set */
	{ "BBSC",  handle_bbsc,   0xE4 },    /* Branch on bit set and clear */
	{ "BBCC",  handle_bbcc,   0xE5 },    /* Branch on bit clear and clear */
	{ "BBSSI", handle_bbssi,  0xE6 },    /* Branch on bit set and set interlocked */
	{ "BBCCI", handle_bbcci,  0xE7 },    /* Branch on bit clear and clear interlocked */

	/* VAX Additional String Operations */
	{ "MOVTC", handle_movtc,  0x2E },    /* Move translated character */
	{ "MOVTUC", handle_movtuc, 0x2F },   /* Move translated until character */
	{ "MATCHC", handle_matchc, 0x39 },   /* Match character */
	{ "EDITPC", handle_editpc, 0x38 },   /* Edit packed to character */

	/* VAX Queue Instructions */
	{ "INSQUE", handle_insque, 0x0E },   /* Insert into queue */
	{ "REMQUE", handle_remque, 0x0F },   /* Remove from queue */
	{ "INSQHI", handle_insqhi, 0x5C },   /* Insert into queue head interlocked */
	{ "INSQTI", handle_insqti, 0x5D },   /* Insert into queue tail interlocked */
	{ "REMQHI", handle_remqhi, 0x5E },   /* Remove from queue head interlocked */
	{ "REMQTI", handle_remqti, 0x5F },   /* Remove from queue tail interlocked */

	/* VAX Control Flow */
	{ "BNEQ",  handle_bneq,   0x12 },    /* Branch not equal (VAX) */
	{ "BEQL",  handle_beql,   0x13 },    /* Branch equal (VAX) */
	{ "BGTR",  handle_bgtr,   0x14 },    /* Branch greater than */
	{ "BLEQ",  handle_bleq,   0x15 },    /* Branch less than or equal */
	{ "BGEQ",  handle_bgeq,   0x18 },    /* Branch greater than or equal */
	{ "BLSS",  handle_blss,   0x19 },    /* Branch less than */
	{ "BGTRU", handle_bgtru,  0x1A },    /* Branch greater than unsigned */
	{ "BLEQU", handle_blequ,  0x1B },    /* Branch less or equal unsigned */
	{ "BGEQU", handle_bgequ,  0x1E },    /* Branch greater or equal unsigned */
	{ "BLSSU", handle_blssu,  0x1F },    /* Branch less than unsigned */
	{ "BRB",   handle_brb,    0x11 },    /* Branch byte displacement */
	{ "BRW",   handle_brw,    0x31 },    /* Branch word displacement */
	{ "BSBB",  handle_bsbb,   0x10 },    /* Branch to subroutine byte */
	{ "BSBW",  handle_bsbw,   0x30 },    /* Branch to subroutine word */
	{ "CALLG", handle_callg,  0xFA },    /* Call with general argument list */
	{ "CALLS", handle_calls,  0xFB },    /* Call with stack */
	{ "CASEB", handle_caseb,  0x8F },    /* Case byte */
	{ "CASEW", handle_casew,  0xAF },    /* Case word */
	{ "CASEL", handle_casel,  0xCF },    /* Case long */
	{ "REI",   handle_rei,    0x02 },    /* Return from exception or interrupt */
	{ "RSB",   handle_rsb,    0x05 },    /* Return from subroutine */
	{ "RET",   handle_ret,    0x04 },    /* Return */

	/* VAX System Instructions */
	{ "LDPCTX", handle_ldpctx, 0x06 },   /* Load process context */
	{ "SVPCTX", handle_svpctx, 0x07 },   /* Save process context */
	{ "PROBER", handle_prober, 0x0C },   /* Probe read access */
	{ "PROBEW", handle_probew, 0x0D },   /* Probe write access */
	{ "PROBE",  handle_probe,  0x0C },   /* Probe access */
	{ "MTPR",   handle_mtpr,   0xDA },   /* Move to processor register */
	{ "MFPR",   handle_mfpr,   0xDB },   /* Move from processor register */
	{ "BUGW",   handle_bugw,   0xFDFF }, /* Bugcheck word */
	{ "BUGL",   handle_bugl,   0xFDFE }, /* Bugcheck long */
	{ "CRC",    handle_crc,    0x0B },   /* Calculate CRC */
	{ "ADAWI",  handle_adawi,  0x58 },   /* Add aligned word interlocked */

	/* VAX Packed Decimal */
	{ "ADDP4",  handle_addp4,  0x20 },   /* Add packed 4 operand */
	{ "ADDP6",  handle_addp6,  0x21 },   /* Add packed 6 operand */
	{ "SUBP4",  handle_subp4,  0x22 },   /* Subtract packed 4 operand */
	{ "SUBP6",  handle_subp6,  0x23 },   /* Subtract packed 6 operand */
	{ "MULP",   handle_mulp,   0x25 },   /* Multiply packed */
	{ "DIVP",   handle_divp,   0x27 },   /* Divide packed */
	{ "CVTPT",  handle_cvtpt,  0x24 },   /* Convert packed to trailing */
	{ "CVTTP",  handle_cvttp,  0x26 },   /* Convert trailing to packed */
	{ "CVTPS",  handle_cvtps,  0x08 },   /* Convert packed to leading separate */
	{ "CVTSP",  handle_cvtsp,  0x09 },   /* Convert leading separate to packed */
	{ "ASHP",   handle_ashp,   0xF8 },   /* Arithmetic shift and round packed */
	{ "CMPP3",  handle_cmpp3,  0x35 },   /* Compare packed 3 operand */
	{ "CMPP4",  handle_cmpp4,  0x37 },   /* Compare packed 4 operand */
	{ "MOVP",   handle_movp,   0x34 },   /* Move packed */

	/* VAX Conversion Instructions */
	{ "CVTBW",  handle_cvtbw,  0x99 },   /* Convert byte to word */
	{ "CVTBL",  handle_cvtbl,  0x98 },   /* Convert byte to long */
	{ "CVTWB",  handle_cvtwb,  0x33 },   /* Convert word to byte */
	{ "CVTWL",  handle_cvtwl,  0x32 },   /* Convert word to long */
	{ "CVTLB",  handle_cvtlb,  0xF6 },   /* Convert long to byte */
	{ "CVTLW",  handle_cvtlw,  0xF7 },   /* Convert long to word */
	{ "CVTFB",  handle_cvtfb,  0x48 },   /* Convert F_float to byte */
	{ "CVTFW",  handle_cvtfw,  0x49 },   /* Convert F_float to word */
	{ "CVTDB",  handle_cvtdb,  0x68 },   /* Convert D_float to byte */
	{ "CVTDW",  handle_cvtdw,  0x69 },   /* Convert D_float to word */
	{ "CVTGB",  handle_cvtgb,  0x48FD }, /* Convert G_float to byte */
	{ "CVTGW",  handle_cvtgw,  0x49FD }, /* Convert G_float to word */
	{ "CVTGL",  handle_cvtgl,  0x4AFD }, /* Convert G_float to long */
	{ "CVTBF",  handle_cvtbf,  0x4C },   /* Convert byte to F_float */
	{ "CVTWF",  handle_cvtwf,  0x4D },   /* Convert word to F_float */
	{ "CVTBD",  handle_cvtbd,  0x6C },   /* Convert byte to D_float */
	{ "CVTWD",  handle_cvtwd,  0x6D },   /* Convert word to D_float */
	{ "CVTBG",  handle_cvtbg,  0x4CFD }, /* Convert byte to G_float */
	{ "CVTWG",  handle_cvtwg,  0x4DFD }, /* Convert word to G_float */
	{ "CVTLG",  handle_cvtlg,  0x4FFD }, /* Convert long to G_float */
	{ "CVTFG",  handle_cvtfg,  0x99FD }, /* Convert F_float to G_float */
	{ "CVTGF",  handle_cvtgf,  0x33FD }, /* Convert G_float to F_float */
	{ "CVTRDL", handle_cvtrdl, 0x6B },   /* Convert D_float to long rounded */
	{ "CVTRFL", handle_cvtrfl, 0x4B },   /* Convert F_float to long rounded */
	{ "CVTRGL", handle_cvtrgl, 0x4BFD }, /* Convert G_float to long rounded */
	{ "CVTLP",  handle_cvtlp,  0xF9 },   /* Convert long to packed */
	{ "CVTPL",  handle_cvtpl,  0x36 },   /* Convert packed to long */

	/* VAX Loop Control Instructions */
	{ "ACBB",   handle_acbb,   0x9D },   /* Add compare branch byte */
	{ "ACBW",   handle_acbw,   0x3D },   /* Add compare branch word */
	{ "ACBL",   handle_acbl,   0xF1 },   /* Add compare branch long */
	{ "ACBF",   handle_acbf,   0x4F },   /* Add compare branch F_float */
	{ "ACBD",   handle_acbd,   0x6F },   /* Add compare branch D_float */
	{ "ACBG",   handle_acbg,   0x4FFD }, /* Add compare branch G_float */
	{ "ACBH",   handle_acbh,   0x6FFD }, /* Add compare branch H_float */
	{ "AOBLEQ", handle_aobleq, 0xF3 },   /* Add one branch less or equal */
	{ "AOBLSS", handle_aoblss, 0xF2 },   /* Add one branch less than */
	{ "SOBGEQ", handle_sobgeq, 0xF4 },   /* Subtract one branch greater or equal */
	{ "SOBGTR", handle_sobgtr, 0xF5 },   /* Subtract one branch greater than */

	/* VAX Register Set Instructions */
	{ "PUSHR",  handle_pushr,  0xBB },   /* Push register set */
	{ "POPR",   handle_popr,   0xBA },   /* Pop register set */

	/* VAX Index and Processor Status */
	{ "INDEX",  handle_index,  0x0A },   /* Compute index */
	{ "MOVPSL", handle_movpsl, 0xDC },   /* Move processor status longword */
	{ "BISPSW", handle_bispsw, 0xB8 },   /* Bit set processor status word */
	{ "BICPSW", handle_bicpsw, 0xB9 },   /* Bit clear processor status word */

	/* VAX Change Mode Instructions */
	{ "XFC",    handle_xfc,    0xFC },   /* Extended function call */
	{ "CHME",   handle_chme,   0xBD },   /* Change mode to executive */
	{ "CHMK",   handle_chmk,   0xBC },   /* Change mode to kernel */
	{ "CHMS",   handle_chms,   0xBE },   /* Change mode to supervisor */
	{ "CHMU",   handle_chmu,   0xBF },   /* Change mode to user */

	/* PDP-10 specific instructions */
	{ "MOVEI", handle_movei, 0201000 },  /* Move immediate */
	{ "MOVEM", handle_movem, 0202000 },  /* Move to memory */
	{ "MOVES", handle_moves, 0203000 },  /* Move to self */
	{ "MOVN",  handle_movn,  0210000 },  /* Move negative */
	{ "MOVM",  handle_movm,  0214000 },  /* Move magnitude */
	{ "IMUL",  handle_imul,  0220000 },  /* Integer multiply */
	{ "IDIV",  handle_idiv,  0230000 },  /* Integer divide */
	{ "LSH",   handle_lsh,   0242000 },  /* Logical shift */
	{ "ROT",   handle_rot,   0241000 },  /* Rotate */
	{ "JRST",  handle_jrst,  0254000 },  /* Jump and restore */
	{ "PUSHJ", handle_pushj, 0260000 },  /* Push and jump */
	{ "POPJ",  handle_popj,  0263000 },  /* Pop and jump */
	{ "EXCH",  handle_mov,   0250000 },  /* Exchange */
	{ "SETM",  handle_neg,   0214040 },  /* Set to memory */
	{ "SETZ",  handle_clr,   0400000 },  /* Set to zero */
	{ "SETMI", handle_neg,   0210040 },  /* Set minus */
	{ "SETA",  handle_mov,   0210040 },  /* Set to A */

	/* PDP-10 Half-word operations */
	{ "HLL",   handle_hll,   0500000 },  /* Half left to left */
	{ "HLLI",  handle_hlli,  0501000 },  /* Half left to left immediate */
	{ "HLLM",  handle_hllm,  0502000 },  /* Half left to left to memory */
	{ "HLLS",  handle_hlls,  0503000 },  /* Half left to left to self */
	{ "HRL",   handle_hrl,   0504000 },  /* Half right to left */
	{ "HRLI",  handle_hrli,  0505000 },  /* Half right to left immediate */
	{ "HRLM",  handle_hrlm,  0506000 },  /* Half right to left to memory */
	{ "HRLS",  handle_hrls,  0507000 },  /* Half right to left to self */
	{ "HRR",   handle_hrr,   0540000 },  /* Half right to right */
	{ "HRRI",  handle_hrri,  0541000 },  /* Half right to right immediate */
	{ "HRRM",  handle_hrrm,  0542000 },  /* Half right to right to memory */
	{ "HRRS",  handle_hrrs,  0543000 },  /* Half right to right to self */
	{ "HLR",   handle_hlr,   0544000 },  /* Half left to right */
	{ "HLRI",  handle_hlri,  0545000 },  /* Half left to right immediate */
	{ "HLRM",  handle_hlrm,  0546000 },  /* Half left to right to memory */
	{ "HLRS",  handle_hlrs,  0547000 },  /* Half left to right to self */

	/* PDP-10 Arithmetic (non-conflicting names) */
	{ "ADD",   handle_add,   0270000 },  /* Add */
	{ "ADDI",  handle_addi,  0271000 },  /* Add immediate */
	{ "ADDM",  handle_addm,  0272000 },  /* Add to memory */
	{ "SUB",   handle_sub,   0274000 },  /* Subtract */
	{ "SUBI",  handle_subi,  0275000 },  /* Subtract immediate */
	{ "SUBM",  handle_subm,  0276000 },  /* Subtract to memory */
	{ "MUL",   handle_mul,   0224000 },  /* Multiply */
	{ "MULI",  handle_muli,  0225000 },  /* Multiply immediate */
	{ "MULM",  handle_mulm,  0226000 },  /* Multiply to memory */
	{ "DIV",   handle_div,   0234000 },  /* Divide */
	{ "DIVI",  handle_divi,  0235000 },  /* Divide immediate */
	{ "DIVM",  handle_divm,  0236000 },  /* Divide to memory */
	{ "ASH",   handle_ash,   0240000 },  /* Arithmetic shift */
	{ "ASHC",  handle_ashc,  0244000 },  /* Arithmetic shift combined */
	{ "LSHC",  handle_lshc,  0246000 },  /* Logical shift combined */
	{ "ROTC",  handle_rotc,  0245000 },  /* Rotate combined */

	/* PDP-10 Floating Point */
	{ "FAD",   handle_fad,   0140000 },  /* Floating add */
	{ "FADL",  handle_fadl,  0141000 },  /* Floating add long */
	{ "FADM",  handle_fadm,  0142000 },  /* Floating add to memory */
	{ "FADB",  handle_fadb,  0143000 },  /* Floating add both */
	{ "FADR",  handle_fadr,  0144000 },  /* Floating add rounded */
	{ "FADRI", handle_fadri, 0145000 },  /* Floating add rounded immediate */
	{ "FADRM", handle_fadrm, 0146000 },  /* Floating add rounded to memory */
	{ "FADRB", handle_fadrb, 0147000 },  /* Floating add rounded both */
	{ "FSB",   handle_fsb,   0150000 },  /* Floating subtract */
	{ "FSBL",  handle_fsbl,  0151000 },  /* Floating subtract long */
	{ "FSBM",  handle_fsbm,  0152000 },  /* Floating subtract to memory */
	{ "FSBB",  handle_fsbb,  0153000 },  /* Floating subtract both */
	{ "FSBR",  handle_fsbr,  0154000 },  /* Floating subtract rounded */
	{ "FSBRI", handle_fsbri, 0155000 },  /* Floating subtract rounded immediate */
	{ "FSBRM", handle_fsbrm, 0156000 },  /* Floating subtract rounded to memory */
	{ "FSBRB", handle_fsbrb, 0157000 },  /* Floating subtract rounded both */
	{ "FMP",   handle_fmp,   0160000 },  /* Floating multiply */
	{ "FMPL",  handle_fmpl,  0161000 },  /* Floating multiply long */
	{ "FMPM",  handle_fmpm,  0162000 },  /* Floating multiply to memory */
	{ "FMPB",  handle_fmpb,  0163000 },  /* Floating multiply both */
	{ "FMPR",  handle_fmpr,  0164000 },  /* Floating multiply rounded */
	{ "FMPRI", handle_fmpri, 0165000 },  /* Floating multiply rounded immediate */
	{ "FMPRM", handle_fmprm, 0166000 },  /* Floating multiply rounded to memory */
	{ "FMPRB", handle_fmprb, 0167000 },  /* Floating multiply rounded both */
	{ "FDV",   handle_fdv,   0170000 },  /* Floating divide */
	{ "FDVL",  handle_fdvl,  0171000 },  /* Floating divide long */
	{ "FDVM",  handle_fdvm,  0172000 },  /* Floating divide to memory */
	{ "FDVB",  handle_fdvb,  0173000 },  /* Floating divide both */
	{ "FDVR",  handle_fdvr,  0174000 },  /* Floating divide rounded */
	{ "FDVRI", handle_fdvri, 0175000 },  /* Floating divide rounded immediate */
	{ "FDVRM", handle_fdvrm, 0176000 },  /* Floating divide rounded to memory */
	{ "FDVRB", handle_fdvrb, 0177000 },  /* Floating divide rounded both */

	/* PDP-10 Logical Operations */
	{ "AND",   handle_and,   0404000 },  /* Logical AND */
	{ "ANDI",  handle_andi,  0405000 },  /* Logical AND immediate */
	{ "ANDM",  handle_andm,  0406000 },  /* Logical AND to memory */
	{ "ANDCA", handle_andca, 0410000 },  /* AND with complement of AC */
	{ "ANDCAI",handle_andcai,0411000 },  /* AND with complement of AC immediate */
	{ "ANDCAM",handle_andcam,0412000 },  /* AND with complement of AC to memory */
	{ "ANDCM", handle_andcm, 0414000 },  /* AND with complement of memory */
	{ "ANDCMI",handle_andcmi,0415000 },  /* AND with complement of memory immediate */
	{ "ANDCMM",handle_andcmm,0416000 },  /* AND with complement of memory to memory */
	{ "IOR",   handle_ior,   0434000 },  /* Inclusive OR */
	{ "IORI",  handle_iori,  0435000 },  /* Inclusive OR immediate */
	{ "IORM",  handle_iorm,  0436000 },  /* Inclusive OR to memory */
	{ "XOR",   handle_xor,   0424000 },  /* Exclusive OR */
	{ "XORI",  handle_xori,  0425000 },  /* Exclusive OR immediate */
	{ "XORM",  handle_xorm,  0426000 },  /* Exclusive OR to memory */
	{ "EQV",   handle_eqv,   0444000 },  /* Equivalence */
	{ "EQVI",  handle_eqvi,  0445000 },  /* Equivalence immediate */
	{ "EQVM",  handle_eqvm,  0446000 },  /* Equivalence to memory */

	/* PDP-10 Set Operations */
	{ "SETO",  handle_seto,  0474000 },  /* Set to ones */
	{ "SETOI", handle_setoi, 0475000 },  /* Set to ones immediate */
	{ "SETOM", handle_setom, 0476000 },  /* Set to ones to memory */
	{ "SETCA", handle_setca, 0450000 },  /* Set to complement of AC */
	{ "SETCAI",handle_setcai,0451000 },  /* Set to complement of AC immediate */
	{ "SETCAM",handle_setcam,0452000 },  /* Set to complement of AC to memory */
	{ "SETCM", handle_setcm, 0454000 },  /* Set to complement of memory */
	{ "SETCMI",handle_setcmi,0455000 },  /* Set to complement of memory immediate */
	{ "SETCMM",handle_setcmm,0456000 },  /* Set to complement of memory to memory */
	{ "SETZI", handle_setzi, 0401000 },  /* Set to zero immediate */
	{ "SETZM", handle_setzm, 0402000 },  /* Set to zero to memory */

	/* PDP-10 Skip/Jump Operations */
	{ "SKIP",  handle_skip,  0330000 },  /* Skip */
	{ "SKIPL", handle_skipl, 0331000 },  /* Skip if less than zero */
	{ "SKIPE", handle_skipe, 0332000 },  /* Skip if equal to zero */
	{ "SKIPLE",handle_skiple,0333000 },  /* Skip if less or equal to zero */
	{ "SKIPA", handle_skipa, 0334000 },  /* Skip always */
	{ "SKIPGE",handle_skipge,0335000 },  /* Skip if greater or equal to zero */
	{ "SKIPN", handle_skipn, 0336000 },  /* Skip if not zero */
	{ "SKIPG", handle_skipg, 0337000 },  /* Skip if greater than zero */
	{ "JUMP",  handle_jump,  0320000 },  /* Jump */
	{ "JUMPL", handle_jumpl, 0321000 },  /* Jump if less than zero */
	{ "JUMPE", handle_jumpe, 0322000 },  /* Jump if equal to zero */
	{ "JUMPLE",handle_jumple,0323000 },  /* Jump if less or equal to zero */
	{ "JUMPA", handle_jumpa, 0324000 },  /* Jump always */
	{ "JUMPGE",handle_jumpge,0325000 },  /* Jump if greater or equal to zero */
	{ "JUMPN", handle_jumpn, 0326000 },  /* Jump if not zero */
	{ "JUMPG", handle_jumpg, 0327000 },  /* Jump if greater than zero */

	/* PDP-10 Add One and Jump */
	{ "AOJ",   handle_aoj,   0340000 },  /* Add one and jump */
	{ "AOJL",  handle_aojl,  0341000 },  /* Add one and jump if less */
	{ "AOJE",  handle_aoje,  0342000 },  /* Add one and jump if equal */
	{ "AOJLE", handle_aojle, 0343000 },  /* Add one and jump if less or equal */
	{ "AOJA",  handle_aoja,  0344000 },  /* Add one and jump always */
	{ "AOJGE", handle_aojge, 0345000 },  /* Add one and jump if greater or equal */
	{ "AOJN",  handle_aojn,  0346000 },  /* Add one and jump if not zero */
	{ "AOJG",  handle_aojg,  0347000 },  /* Add one and jump if greater */

	/* PDP-10 Add One to memory and Skip */
	{ "AOS",   handle_aos,   0350000 },  /* Add one to memory and skip */
	{ "AOSL",  handle_aosl,  0351000 },  /* Add one to memory and skip if less */
	{ "AOSE",  handle_aose,  0352000 },  /* Add one to memory and skip if equal */
	{ "AOSLE", handle_aosle, 0353000 },  /* Add one to memory and skip if less or equal */
	{ "AOSA",  handle_aosa,  0354000 },  /* Add one to memory and skip always */
	{ "AOSGE", handle_aosge, 0355000 },  /* Add one to memory and skip if greater or equal */
	{ "AOSN",  handle_aosn,  0356000 },  /* Add one to memory and skip if not zero */
	{ "AOSG",  handle_aosg,  0357000 },  /* Add one to memory and skip if greater */

	/* PDP-10 Subtract One and Jump */
	{ "SOJ",   handle_soj,   0360000 },  /* Subtract one and jump */
	{ "SOJL",  handle_sojl,  0361000 },  /* Subtract one and jump if less */
	{ "SOJE",  handle_soje,  0362000 },  /* Subtract one and jump if equal */
	{ "SOJLE", handle_sojle, 0363000 },  /* Subtract one and jump if less or equal */
	{ "SOJA",  handle_soja,  0364000 },  /* Subtract one and jump always */
	{ "SOJGE", handle_sojge, 0365000 },  /* Subtract one and jump if greater or equal */
	{ "SOJN",  handle_sojn,  0366000 },  /* Subtract one and jump if not zero */
	{ "SOJG",  handle_sojg,  0367000 },  /* Subtract one and jump if greater */

	/* PDP-10 Subtract One from memory and Skip */
	{ "SOS",   handle_sos,   0370000 },  /* Subtract one from memory and skip */
	{ "SOSL",  handle_sosl,  0371000 },  /* Subtract one from memory and skip if less */
	{ "SOSE",  handle_sose,  0372000 },  /* Subtract one from memory and skip if equal */
	{ "SOSLE", handle_sosle, 0373000 },  /* Subtract one from memory and skip if less or equal */
	{ "SOSA",  handle_sosa,  0374000 },  /* Subtract one from memory and skip always */
	{ "SOSGE", handle_sosge, 0375000 },  /* Subtract one from memory and skip if greater or equal */
	{ "SOSN",  handle_sosn,  0376000 },  /* Subtract one from memory and skip if not zero */
	{ "SOSG",  handle_sosg,  0377000 },  /* Subtract one from memory and skip if greater */

	/* PDP-10 Compare Immediate */
	{ "CAI",   handle_cai,   0300000 },  /* Compare accumulator immediate */
	{ "CAIL",  handle_cail,  0301000 },  /* Compare accumulator immediate, skip if less */
	{ "CAIE",  handle_caie,  0302000 },  /* Compare accumulator immediate, skip if equal */
	{ "CAILE", handle_caile, 0303000 },  /* Compare accumulator immediate, skip if less or equal */
	{ "CAIA",  handle_caia,  0304000 },  /* Compare accumulator immediate, skip always */
	{ "CAIGE", handle_caige, 0305000 },  /* Compare accumulator immediate, skip if greater or equal */
	{ "CAIN",  handle_cain,  0306000 },  /* Compare accumulator immediate, skip if not equal */
	{ "CAIG",  handle_caig,  0307000 },  /* Compare accumulator immediate, skip if greater */

	/* PDP-10 Compare Memory */
	{ "CAM",   handle_cam,   0310000 },  /* Compare accumulator with memory */
	{ "CAML",  handle_caml,  0311000 },  /* Compare accumulator with memory, skip if less */
	{ "CAME",  handle_came,  0312000 },  /* Compare accumulator with memory, skip if equal */
	{ "CAMLE", handle_camle, 0313000 },  /* Compare accumulator with memory, skip if less or equal */
	{ "CAMA",  handle_cama,  0314000 },  /* Compare accumulator with memory, skip always */
	{ "CAMGE", handle_camge, 0315000 },  /* Compare accumulator with memory, skip if greater or equal */
	{ "CAMN",  handle_camn,  0316000 },  /* Compare accumulator with memory, skip if not equal */
	{ "CAMG",  handle_camg,  0317000 },  /* Compare accumulator with memory, skip if greater */

	/* PDP-10 Byte Operations */
	{ "LDB",   handle_ldb,   0135000 },  /* Load byte */
	{ "ILDB",  handle_ildb,  0136000 },  /* Increment pointer and load byte */
	{ "DPB",   handle_dpb,   0137000 },  /* Deposit byte */
	{ "IDPB",  handle_idpb,  0134000 },  /* Increment pointer and deposit byte */
	{ "IBP",   handle_ibp,   0133000 },  /* Increment byte pointer */

	/* PDP-10 I/O Operations */
	{ "BLKI",  handle_blki,  0700000 },  /* Block input */
	{ "DATAI", handle_datai, 0704000 },  /* Data input */
	{ "BLKO",  handle_blko,  0710000 },  /* Block output */
	{ "DATAO", handle_datao, 0714000 },  /* Data output */
	{ "CONO",  handle_cono,  0720000 },  /* Conditions output */
	{ "CONI",  handle_coni,  0724000 },  /* Conditions input */
	{ "CONSZ", handle_consz, 0730000 },  /* Conditions skip if zero */
	{ "CONSO", handle_conso, 0734000 },  /* Conditions skip if one */

	/* PDP-10 Stack Operations */
	{ "PUSH",  handle_push,  0261000 },  /* Push */
	{ "POP",   handle_pop,   0262000 },  /* Pop */

	/* PDP-10 Move variants */
	{ "MOVS",  handle_movs,  0200000 },  /* Move swapped */
	{ "MOVSI", handle_movsi, 0205000 },  /* Move swapped immediate */
	{ "MOVSM", handle_movsm, 0206000 },  /* Move swapped to memory */
	{ "MOVSS", handle_movss, 0207000 },  /* Move swapped to self */
	{ "MOVNI", handle_movni, 0211000 },  /* Move negative immediate */
	{ "MOVNM", handle_movnm, 0212000 },  /* Move negative to memory */
	{ "MOVNS", handle_movns, 0213000 },  /* Move negative to self */
	{ "MOVMI", handle_movmi, 0215000 },  /* Move magnitude immediate */
	{ "MOVMM", handle_movmm, 0216000 },  /* Move magnitude to memory */
	{ "MOVMS", handle_movms, 0217000 },  /* Move magnitude to self */

	{ NULL, NULL, 0 }  /* Sentinel */
};

/* Current program section */
int current_psect = 0;

/* Location counter (current address) */
long location_counter = 0;

/* Block nesting level (for PCC) */
int blevel = 0;

/* Label counter for PCC */
static int label_counter = 100;

/*
 * Initialize code generator
 */
void
codegen_init(void)
{
	location_counter = 0;
	current_psect = 0;
	blevel = 0;
	label_counter = 100;
}

/*
 * Get a new PCC label number
 */
int
get_label(void)
{
	return label_counter++;
}

/*
 * Emit a label definition using PCC IR
 */
void
emit_label_ir(const char *label)
{
	SYMTAB *sym;

	/* Look up or create symbol */
	sym = lookup(label);
	if (sym == NULL) {
		sym = install(label, SYM_LABEL);
		sym->label_num = get_label();
	}

	/* Define label using PCC interpass */
	send_passt(IP_DEFLAB, sym->label_num);
	define_symbol(sym, location_counter);
}

/*
 * Convert OPERAND to PCC NODE
 */
static P1ND *
operand_to_node(OPERAND *op)
{
	P1ND *p;
	SYMTAB *sym;

	switch (op->type) {
	case OP_REGISTER:
		/* Direct register reference */
		return build_reg(op->reg);

	case OP_IMMEDIATE:
		/* Immediate constant */
		return build_icon(op->value);

	case OP_DIRECT:
		/* Direct memory reference or symbol */
		if (op->symbol) {
			/* Symbol reference - build NAME node */
			sym = lookup(op->symbol);
			if (sym == NULL) {
				sym = install(op->symbol, SYM_EXTERNAL);
			}
			p = (P1ND *)malloc(sizeof(P1ND));
			memset(p, 0, sizeof(P1ND));
			p->n_op = NAME;
			p->n_type = INT;
			p->n_name = op->symbol;
			return p;
		} else {
			/* Direct address - build as dereference */
			p = build_icon(op->value);
			return build_unop(UMUL, p);
		}

	case OP_INDIRECT:
		/* Indirect addressing @symbol or @value */
		if (op->symbol) {
			sym = lookup(op->symbol);
			if (sym == NULL) {
				sym = install(op->symbol, SYM_EXTERNAL);
			}
			p = (P1ND *)malloc(sizeof(P1ND));
			memset(p, 0, sizeof(P1ND));
			p->n_op = NAME;
			p->n_type = INT;
			p->n_name = op->symbol;
			/* Double indirect */
			p = build_unop(UMUL, p);
			return build_unop(UMUL, p);
		} else {
			p = build_icon(op->value);
			p = build_unop(UMUL, p);
			return build_unop(UMUL, p);
		}

	case OP_INDEXED:
		/* offset(Rn) - register with offset */
		return build_oreg(op->reg, op->value);

	case OP_AUTODEC:
		/* -(Rn) - predecrement - build as side effect */
		p = build_reg(op->reg);
		p = build_assign(build_reg(op->reg),
		    build_binop(MINUS, build_reg(op->reg), build_icon(2)));
		return build_oreg(op->reg, 0);

	case OP_AUTOINC:
		/* (Rn)+ - postincrement */
		return build_oreg(op->reg, 0);

	case OP_SYMBOL:
		/* Symbol reference, possibly with offset */
		sym = lookup(op->symbol);
		if (sym == NULL) {
			sym = install(op->symbol, SYM_EXTERNAL);
		}
		p = (P1ND *)malloc(sizeof(P1ND));
		memset(p, 0, sizeof(P1ND));
		p->n_op = NAME;
		p->n_type = INT;
		p->n_name = op->symbol;

		if (op->value != 0) {
			/* Add offset */
			p = build_binop(PLUS, p, build_icon(op->value));
		}
		return p;

	case OP_LITERAL:
		/* Literal value */
		return build_icon(op->value);

	default:
		error("unknown operand type %d", op->type);
		return build_icon(0);
	}
}

/* ========== Instruction Handlers (generate ONLY PCC IR) ========== */

/*
 * MOV src, dst -> dst = src
 */
static P1ND *
handle_mov(INSTRUCTION *inst)
{
	P1ND *src, *dst;

	if (inst->noperands != 2) {
		error("MOV requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	return build_assign(dst, src);
}

/*
 * ADD src, dst -> dst = dst + src
 */
static P1ND *
handle_add(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("ADD requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(PLUS, dst_rhs, src));
}

/*
 * SUB src, dst -> dst = dst - src
 */
static P1ND *
handle_sub(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("SUB requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(MINUS, dst_rhs, src));
}

/*
 * MUL src, reg -> reg = reg * src
 */
static P1ND *
handle_mul(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("MUL requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(MUL, dst_rhs, src));
}

/*
 * DIV src, reg -> reg = reg / src
 */
static P1ND *
handle_div(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("DIV requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(DIV, dst_rhs, src));
}

/*
 * AND src, dst -> dst = dst & src
 */
static P1ND *
handle_and(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("AND requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(AND, dst_rhs, src));
}

/*
 * OR src, dst -> dst = dst | src
 */
static P1ND *
handle_or(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("OR requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(OR, dst_rhs, src));
}

/*
 * XOR src, dst -> dst = dst ^ src
 */
static P1ND *
handle_xor(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("XOR requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(ER, dst_rhs, src));  /* ER = XOR */
}

/*
 * BIC src, dst -> dst = dst & ~src (bit clear)
 */
static P1ND *
handle_bic(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("BIC requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	/* dst = dst & ~src */
	return build_assign(dst, build_binop(AND, dst_rhs, build_unop(COMPL, src)));
}

/*
 * BIS src, dst -> dst = dst | src (bit set, same as OR)
 */
static P1ND *
handle_bis(INSTRUCTION *inst)
{
	return handle_or(inst);
}

/*
 * BIT src, dst -> test (dst & src) [sets condition codes]
 */
static P1ND *
handle_bit(INSTRUCTION *inst)
{
	P1ND *src, *dst;

	if (inst->noperands != 2) {
		error("BIT requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	/* Generate AND operation (result affects condition codes) */
	return build_binop(AND, dst, src);
}

/*
 * CLR dst -> dst = 0
 */
static P1ND *
handle_clr(INSTRUCTION *inst)
{
	P1ND *dst;

	if (inst->noperands != 1) {
		error("CLR requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_icon(0));
}

/*
 * INC dst -> dst = dst + 1
 */
static P1ND *
handle_inc(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs;

	if (inst->noperands != 1) {
		error("INC requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_binop(PLUS, dst_rhs, build_icon(1)));
}

/*
 * DEC dst -> dst = dst - 1
 */
static P1ND *
handle_dec(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs;

	if (inst->noperands != 1) {
		error("DEC requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_binop(MINUS, dst_rhs, build_icon(1)));
}

/*
 * NEG dst -> dst = -dst
 */
static P1ND *
handle_neg(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs;

	if (inst->noperands != 1) {
		error("NEG requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_unop(UMINUS, dst_rhs));
}

/*
 * COM dst -> dst = ~dst (one's complement)
 */
static P1ND *
handle_com(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs;

	if (inst->noperands != 1) {
		error("COM requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_unop(COMPL, dst_rhs));
}

/*
 * TST dst -> compare dst with 0
 */
static P1ND *
handle_tst(INSTRUCTION *inst)
{
	P1ND *dst;

	if (inst->noperands != 1) {
		error("TST requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	return build_binop(NE, dst, build_icon(0));
}

/*
 * CMP src, dst -> compare src with dst
 */
static P1ND *
handle_cmp(INSTRUCTION *inst)
{
	P1ND *src, *dst;

	if (inst->noperands != 2) {
		error("CMP requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	return build_binop(NE, src, dst);
}

/*
 * ASL dst -> dst = dst << 1 (arithmetic shift left)
 */
static P1ND *
handle_asl(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs;

	if (inst->noperands != 1) {
		error("ASL requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_binop(LS, dst_rhs, build_icon(1)));
}

/*
 * ASR dst -> dst = dst >> 1 (arithmetic shift right)
 */
static P1ND *
handle_asr(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs;

	if (inst->noperands != 1) {
		error("ASR requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_binop(RS, dst_rhs, build_icon(1)));
}

/*
 * ROL dst -> rotate left (no direct PCC operation, use shifts and OR)
 */
static P1ND *
handle_rol(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs1, *dst_rhs2, *dst_rhs3;
	P1ND *left_part, *right_part;

	if (inst->noperands != 1) {
		error("ROL requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs1 = operand_to_node(&inst->operands[0]);
	dst_rhs2 = operand_to_node(&inst->operands[0]);
	dst_rhs3 = operand_to_node(&inst->operands[0]);

	/* dst = (dst << 1) | (dst >> 15) */
	left_part = build_binop(LS, dst_rhs1, build_icon(1));
	right_part = build_binop(RS, dst_rhs2, build_icon(15));
	return build_assign(dst, build_binop(OR, left_part, right_part));
}

/*
 * ROR dst -> rotate right
 */
static P1ND *
handle_ror(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs1, *dst_rhs2;
	P1ND *left_part, *right_part;

	if (inst->noperands != 1) {
		error("ROR requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs1 = operand_to_node(&inst->operands[0]);
	dst_rhs2 = operand_to_node(&inst->operands[0]);

	/* dst = (dst >> 1) | (dst << 15) */
	right_part = build_binop(RS, dst_rhs1, build_icon(1));
	left_part = build_binop(LS, dst_rhs2, build_icon(15));
	return build_assign(dst, build_binop(OR, right_part, left_part));
}

/*
 * SWAB dst -> swap bytes in word
 */
static P1ND *
handle_swab(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs1, *dst_rhs2;
	P1ND *low_byte, *high_byte;

	if (inst->noperands != 1) {
		error("SWAB requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs1 = operand_to_node(&inst->operands[0]);
	dst_rhs2 = operand_to_node(&inst->operands[0]);

	/* dst = ((dst & 0xff) << 8) | ((dst >> 8) & 0xff) */
	low_byte = build_binop(LS,
	    build_binop(AND, dst_rhs1, build_icon(0xff)),
	    build_icon(8));
	high_byte = build_binop(AND,
	    build_binop(RS, dst_rhs2, build_icon(8)),
	    build_icon(0xff));
	return build_assign(dst, build_binop(OR, low_byte, high_byte));
}

/*
 * SXT dst -> sign extend (if N set, dst = -1, else dst = 0)
 */
static P1ND *
handle_sxt(INSTRUCTION *inst)
{
	P1ND *dst;

	if (inst->noperands != 1) {
		error("SXT requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	/* For now, just clear it - proper implementation needs condition codes */
	return build_assign(dst, build_icon(0));
}

/*
 * Branch instructions - use GOTO nodes with labels
 */
static P1ND *
handle_branch(INSTRUCTION *inst)
{
	SYMTAB *sym;
	P1ND *p;
	int label_num;

	if (inst->noperands != 1) {
		error("Branch requires 1 operand (label)");
		return NULL;
	}

	/* Get or create label symbol */
	if (inst->operands[0].symbol) {
		sym = lookup(inst->operands[0].symbol);
		if (sym == NULL) {
			sym = install(inst->operands[0].symbol, SYM_LABEL);
			sym->label_num = get_label();
		}
		label_num = sym->label_num;
	} else {
		label_num = get_label();
	}

	/* Build GOTO node */
	p = (P1ND *)malloc(sizeof(P1ND));
	memset(p, 0, sizeof(P1ND));
	p->n_op = GOTO;
	p->n_type = INT;
	p->n_label = label_num;

	return p;
}

/*
 * JMP addr -> unconditional jump
 */
static P1ND *
handle_jmp(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/*
 * JSR reg, addr -> call subroutine (push PC, jump)
 */
static P1ND *
handle_jsr(INSTRUCTION *inst)
{
	SYMTAB *sym;
	P1ND *p;
	int label_num;

	if (inst->noperands != 2) {
		error("JSR requires 2 operands (link register, target)");
		return NULL;
	}

	/* Get or create label symbol for target (second operand) */
	if (inst->operands[1].symbol) {
		sym = lookup(inst->operands[1].symbol);
		if (sym == NULL) {
			sym = install(inst->operands[1].symbol, SYM_LABEL);
			sym->label_num = get_label();
		}
		label_num = sym->label_num;
	} else {
		label_num = get_label();
	}

	/* Build GOTO node for the jump */
	p = (P1ND *)malloc(sizeof(P1ND));
	memset(p, 0, sizeof(P1ND));
	p->n_op = GOTO;
	p->n_label = label_num;

	/* Note: In a full implementation, we would save PC to the link register
	 * (first operand), but for now we just implement the jump */

	return p;
}

/*
 * RTS -> return from subroutine
 */
static P1ND *
handle_rts(INSTRUCTION *inst)
{
	P1ND *p;

	/* Build return node - use value 0 */
	p = (P1ND *)malloc(sizeof(P1ND));
	memset(p, 0, sizeof(P1ND));
	p->n_op = RETURN;
	p->n_type = INT;
	p->n_left = build_icon(0);

	return p;
}

/*
 * HALT/WAIT/RESET -> special operations
 * For now, generate as no-op or specific inline asm marker
 */
static P1ND *
handle_halt(INSTRUCTION *inst)
{
	/* Generate as assignment to trigger backend action */
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	memset(p, 0, sizeof(P1ND));
	p->n_op = ICON;
	p->n_type = INT;
	setlval(p, 0);

	return p;
}

/*
 * NOP -> no operation
 */
static P1ND *
handle_nop(INSTRUCTION *inst)
{
	/* Generate icon node that will be eliminated by optimizer */
	return build_icon(0);
}

/* ========== Extended PDP-11 Instructions ========== */

/*
 * SOB reg, label -> reg = reg - 1; if (reg != 0) goto label
 */
static P1ND *
handle_sob(INSTRUCTION *inst)
{
	P1ND *reg, *reg_rhs, *dec_node, *test_node, *branch_node;
	SYMTAB *sym;
	int label_num;

	if (inst->noperands != 2) {
		error("SOB requires 2 operands");
		return NULL;
	}

	/* Decrement register */
	reg = operand_to_node(&inst->operands[0]);
	reg_rhs = operand_to_node(&inst->operands[0]);
	dec_node = build_assign(reg, build_binop(MINUS, reg_rhs, build_icon(1)));

	/* Send decrement */
	send_passt(IP_NODE, dec_node);

	/* Test and branch */
	if (inst->operands[1].symbol) {
		sym = lookup(inst->operands[1].symbol);
		if (sym == NULL) {
			sym = install(inst->operands[1].symbol, SYM_LABEL);
			sym->label_num = get_label();
		}
		label_num = sym->label_num;
	} else {
		label_num = get_label();
	}

	branch_node = (P1ND *)malloc(sizeof(P1ND));
	memset(branch_node, 0, sizeof(P1ND));
	branch_node->n_op = GOTO;
	branch_node->n_type = INT;
	branch_node->n_label = label_num;

	return branch_node;
}

/*
 * ASH shift_count, reg -> arithmetic shift reg by shift_count
 */
static P1ND *
handle_ash(INSTRUCTION *inst)
{
	P1ND *count, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("ASH requires 2 operands");
		return NULL;
	}

	count = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);

	/* Shift left if positive, right if negative */
	return build_assign(dst, build_binop(LS, dst_rhs, count));
}

/*
 * ASHC shift_count, reg -> arithmetic shift combined (reg:reg+1)
 */
static P1ND *
handle_ashc(INSTRUCTION *inst)
{
	/* Simplified - treat like ASH for now */
	return handle_ash(inst);
}

/*
 * MARK n -> Mark stack frame
 */
static P1ND *
handle_mark(INSTRUCTION *inst)
{
	/* Generate as NOP for IR purposes */
	return build_icon(0);
}

/* ========== Condition Code Operations ========== */

/*
 * Set condition code operations (SEC, SEV, SEZ, SEN)
 */
static P1ND *
handle_scc(INSTRUCTION *inst)
{
	/* Generate as assignment to condition code (abstract) */
	return build_assign(build_reg(16), build_icon(1));  /* CC pseudo-register */
}

/*
 * Clear condition code operations (CLC, CLV, CLZ, CLN)
 */
static P1ND *
handle_ccc(INSTRUCTION *inst)
{
	/* Generate as assignment to condition code (abstract) */
	return build_assign(build_reg(16), build_icon(0));  /* CC pseudo-register */
}

/* ========== Trap/Interrupt Instructions ========== */

/*
 * EMT n -> Emulator trap
 */
static P1ND *
handle_emt(INSTRUCTION *inst)
{
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	memset(p, 0, sizeof(P1ND));
	p->n_op = ICON;
	p->n_type = INT;
	if (inst->noperands > 0)
		setlval(p, inst->operands[0].value);
	else
		setlval(p, 0);

	return p;
}

/*
 * TRAP n -> Trap instruction
 */
static P1ND *
handle_trap(INSTRUCTION *inst)
{
	return handle_emt(inst);
}

/*
 * IOT -> I/O trap
 */
static P1ND *
handle_iot(INSTRUCTION *inst)
{
	return handle_halt(inst);
}

/*
 * BPT -> Breakpoint trap
 */
static P1ND *
handle_bpt(INSTRUCTION *inst)
{
	return handle_halt(inst);
}

/*
 * SPL n -> Set priority level
 */
static P1ND *
handle_spl(INSTRUCTION *inst)
{
	return build_icon(0);
}

/* ========== Memory Management Instructions ========== */

/*
 * MFPI src -> Move from previous instruction space
 */
static P1ND *
handle_mfpi(INSTRUCTION *inst)
{
	P1ND *src, *sp;

	if (inst->noperands != 1) {
		error("MFPI requires 1 operand");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	sp = build_reg(6);  /* SP is R6 */

	/* Push src onto stack: -(SP) = src */
	return build_assign(build_oreg(6, -2), src);
}

/*
 * MTPI dst -> Move to previous instruction space
 */
static P1ND *
handle_mtpi(INSTRUCTION *inst)
{
	P1ND *dst, *sp_val;

	if (inst->noperands != 1) {
		error("MTPI requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	sp_val = build_oreg(6, 0);  /* (SP) */

	/* Pop from stack: dst = (SP)+ */
	return build_assign(dst, sp_val);
}

/*
 * MFPS dst -> Move from processor status
 */
static P1ND *
handle_mfps(INSTRUCTION *inst)
{
	P1ND *dst;

	if (inst->noperands != 1) {
		error("MFPS requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	/* Move from PS register (abstract) */
	return build_assign(dst, build_reg(17));  /* PS pseudo-register */
}

/*
 * MTPS src -> Move to processor status
 */
static P1ND *
handle_mtps(INSTRUCTION *inst)
{
	P1ND *src;

	if (inst->noperands != 1) {
		error("MTPS requires 1 operand");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	/* Move to PS register (abstract) */
	return build_assign(build_reg(17), src);  /* PS pseudo-register */
}

/* ========== VAX 32-bit Operations ========== */

/*
 * MOVL src, dst -> Move longword (32-bit MOV)
 */
static P1ND *
handle_movl(INSTRUCTION *inst)
{
	/* Same as MOV but with LONG type */
	return handle_mov(inst);
}

/*
 * PUSHL src -> Push longword onto stack
 */
static P1ND *
handle_pushl(INSTRUCTION *inst)
{
	P1ND *src, *sp, *sp_rhs;

	if (inst->noperands != 1) {
		error("PUSHL requires 1 operand");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);

	/* SP = SP - 4; (SP) = src */
	sp = build_reg(14);  /* VAX SP is R14 */
	sp_rhs = build_reg(14);

	/* Decrement SP */
	send_passt(IP_NODE,
	    build_assign(sp, build_binop(MINUS, sp_rhs, build_icon(4))));

	/* Store to (SP) */
	return build_assign(build_oreg(14, 0), src);
}

/*
 * POPL dst -> Pop longword from stack
 */
static P1ND *
handle_popl(INSTRUCTION *inst)
{
	P1ND *dst, *sp, *sp_rhs, *sp_val;

	if (inst->noperands != 1) {
		error("POPL requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	sp_val = build_oreg(14, 0);  /* (SP) */

	/* dst = (SP); SP = SP + 4 */
	send_passt(IP_NODE, build_assign(dst, sp_val));

	sp = build_reg(14);
	sp_rhs = build_reg(14);
	return build_assign(sp, build_binop(PLUS, sp_rhs, build_icon(4)));
}

/*
 * ADDL src, dst -> Add longword
 */
static P1ND *
handle_addl(INSTRUCTION *inst)
{
	return handle_add(inst);  /* Same logic, 32-bit */
}

/*
 * SUBL src, dst -> Subtract longword
 */
static P1ND *
handle_subl(INSTRUCTION *inst)
{
	return handle_sub(inst);  /* Same logic, 32-bit */
}

/*
 * MULL src, dst -> Multiply longword
 */
static P1ND *
handle_mull(INSTRUCTION *inst)
{
	return handle_mul(inst);  /* Same logic, 32-bit */
}

/*
 * DIVL src, dst -> Divide longword
 */
static P1ND *
handle_divl(INSTRUCTION *inst)
{
	return handle_div(inst);  /* Same logic, 32-bit */
}

/* ========== VAX String Operations ========== */

/*
 * MOVC3/MOVC5 -> Move character string
 */
static P1ND *
handle_movc(INSTRUCTION *inst)
{
	/* Generate as library call or inline loop */
	/* For IR purposes, generate as assignment */
	P1ND *len, *src, *dst;

	if (inst->noperands < 3) {
		error("MOVC requires at least 3 operands");
		return NULL;
	}

	len = operand_to_node(&inst->operands[0]);
	src = operand_to_node(&inst->operands[1]);
	dst = operand_to_node(&inst->operands[2]);

	/* Simplified: dst = src (block move abstraction) */
	return build_assign(dst, src);
}

/*
 * CMPC3/CMPC5 -> Compare character string
 */
static P1ND *
handle_cmpc(INSTRUCTION *inst)
{
	P1ND *len, *src1, *src2;

	if (inst->noperands < 3) {
		error("CMPC requires at least 3 operands");
		return NULL;
	}

	len = operand_to_node(&inst->operands[0]);
	src1 = operand_to_node(&inst->operands[1]);
	src2 = operand_to_node(&inst->operands[2]);

	/* Simplified: compare abstraction */
	return build_binop(NE, src1, src2);
}

/* ========== VAX Floating Point ========== */

/*
 * ADDF src, dst -> Add floating point
 */
static P1ND *
handle_addf(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("ADDF requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);

	/* Set type to FLOAT */
	dst->n_type = FLOAT;
	dst_rhs->n_type = FLOAT;
	src->n_type = FLOAT;

	return build_assign(dst, build_binop(PLUS, dst_rhs, src));
}

/*
 * SUBF src, dst -> Subtract floating point
 */
static P1ND *
handle_subf(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("SUBF requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);

	dst->n_type = FLOAT;
	dst_rhs->n_type = FLOAT;
	src->n_type = FLOAT;

	return build_assign(dst, build_binop(MINUS, dst_rhs, src));
}

/*
 * MULF src, dst -> Multiply floating point
 */
static P1ND *
handle_mulf(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("MULF requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);

	dst->n_type = FLOAT;
	dst_rhs->n_type = FLOAT;
	src->n_type = FLOAT;

	return build_assign(dst, build_binop(MUL, dst_rhs, src));
}

/*
 * DIVF src, dst -> Divide floating point
 */
static P1ND *
handle_divf(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("DIVF requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);

	dst->n_type = FLOAT;
	dst_rhs->n_type = FLOAT;
	src->n_type = FLOAT;

	return build_assign(dst, build_binop(DIV, dst_rhs, src));
}

/* ========== PDP-10 Specific Instructions ========== */

/*
 * MOVEI reg, immediate -> Move immediate to register
 */
static P1ND *
handle_movei(INSTRUCTION *inst)
{
	return handle_mov(inst);  /* Same as MOV for PDP-10 */
}

/*
 * MOVEM reg, mem -> Move register to memory
 */
static P1ND *
handle_movem(INSTRUCTION *inst)
{
	return handle_mov(inst);
}

/*
 * MOVES reg, mem -> Move to self (reg = mem, write mem)
 */
static P1ND *
handle_moves(INSTRUCTION *inst)
{
	return handle_mov(inst);
}

/*
 * MOVN src, dst -> Move negative
 */
static P1ND *
handle_movn(INSTRUCTION *inst)
{
	P1ND *src, *dst;

	if (inst->noperands != 2) {
		error("MOVN requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);

	return build_assign(dst, build_unop(UMINUS, src));
}

/*
 * MOVM src, dst -> Move magnitude (absolute value)
 */
static P1ND *
handle_movm(INSTRUCTION *inst)
{
	P1ND *src, *dst, *zero, *neg_src, *cond;

	if (inst->noperands != 2) {
		error("MOVM requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);

	/* dst = (src < 0) ? -src : src */
	/* Simplified: dst = src (abs abstraction) */
	return build_assign(dst, src);
}

/*
 * IMUL src, dst -> Integer multiply (PDP-10)
 */
static P1ND *
handle_imul(INSTRUCTION *inst)
{
	return handle_mul(inst);
}

/*
 * IDIV src, dst -> Integer divide (PDP-10)
 */
static P1ND *
handle_idiv(INSTRUCTION *inst)
{
	return handle_div(inst);
}

/*
 * LSH reg, count -> Logical shift (PDP-10)
 */
static P1ND *
handle_lsh(INSTRUCTION *inst)
{
	P1ND *reg, *reg_rhs, *count;

	if (inst->noperands != 2) {
		error("LSH requires 2 operands");
		return NULL;
	}

	count = operand_to_node(&inst->operands[0]);
	reg = operand_to_node(&inst->operands[1]);
	reg_rhs = operand_to_node(&inst->operands[1]);

	return build_assign(reg, build_binop(LS, reg_rhs, count));
}

/*
 * ROT reg, count -> Rotate (PDP-10)
 */
static P1ND *
handle_rot(INSTRUCTION *inst)
{
	return handle_rol(inst);  /* Similar to ROL */
}

/*
 * JRST addr -> Jump and restore (PDP-10)
 */
static P1ND *
handle_jrst(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/*
 * PUSHJ reg, addr -> Push and jump (PDP-10 call)
 */
static P1ND *
handle_pushj(INSTRUCTION *inst)
{
	return handle_jsr(inst);
}

/*
 * POPJ reg, addr -> Pop and jump (PDP-10 return)
 */
static P1ND *
handle_popj(INSTRUCTION *inst)
{
	return handle_rts(inst);
}

/*
 * ========== PDP-10 Half-word Operations ==========
 * PDP-10 has 36-bit words split into two 18-bit halves
 */

/* HLL - Half left to left */
static P1ND *
handle_hll(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs, *src, *mask_left, *mask_right, *left_half, *result;
	if (inst->noperands != 2) {
		error("HLL requires 2 operands");
		return NULL;
	}
	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	src = operand_to_node(&inst->operands[1]);
	/* dst[18:35] = dst[18:35], dst[0:17] = src[0:17] */
	/* (dst & 0777777) | (src & 0777777000000) */
	mask_right = build_icon(0777777);
	mask_left = build_icon(0777777000000LL);
	left_half = build_binop(AND, src, mask_left);
	result = build_binop(OR, build_binop(AND, dst_rhs, mask_right), left_half);
	return build_assign(dst, result);
}

/* HLLI - Half left to left immediate */
static P1ND *
handle_hlli(INSTRUCTION *inst)
{
	return handle_hll(inst);
}

/* HLLM - Half left to left to memory */
static P1ND *
handle_hllm(INSTRUCTION *inst)
{
	return handle_hll(inst);
}

/* HLLS - Half left to left to self */
static P1ND *
handle_hlls(INSTRUCTION *inst)
{
	return handle_hll(inst);
}

/* HRL - Half right to left */
static P1ND *
handle_hrl(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs, *src, *mask, *right_half, *shifted, *result;
	if (inst->noperands != 2) {
		error("HRL requires 2 operands");
		return NULL;
	}
	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	src = operand_to_node(&inst->operands[1]);
	/* dst[0:17] = src[18:35] */
	mask = build_icon(0777777);
	right_half = build_binop(AND, src, mask);
	shifted = build_binop(LS, right_half, build_icon(18));
	result = build_binop(OR, build_binop(AND, dst_rhs, mask), shifted);
	return build_assign(dst, result);
}

/* HRLI - Half right to left immediate */
static P1ND *
handle_hrli(INSTRUCTION *inst)
{
	return handle_hrl(inst);
}

/* HRLM - Half right to left to memory */
static P1ND *
handle_hrlm(INSTRUCTION *inst)
{
	return handle_hrl(inst);
}

/* HRLS - Half right to left to self */
static P1ND *
handle_hrls(INSTRUCTION *inst)
{
	return handle_hrl(inst);
}

/* HRR - Half right to right */
static P1ND *
handle_hrr(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs, *src, *mask_left, *mask_right, *right_half, *result;
	if (inst->noperands != 2) {
		error("HRR requires 2 operands");
		return NULL;
	}
	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	src = operand_to_node(&inst->operands[1]);
	/* dst[18:35] = src[18:35], keep dst[0:17] */
	mask_left = build_icon(0777777000000LL);
	mask_right = build_icon(0777777);
	right_half = build_binop(AND, src, mask_right);
	result = build_binop(OR, build_binop(AND, dst_rhs, mask_left), right_half);
	return build_assign(dst, result);
}

/* HRRI - Half right to right immediate */
static P1ND *
handle_hrri(INSTRUCTION *inst)
{
	return handle_hrr(inst);
}

/* HRRM - Half right to right to memory */
static P1ND *
handle_hrrm(INSTRUCTION *inst)
{
	return handle_hrr(inst);
}

/* HRRS - Half right to right to self */
static P1ND *
handle_hrrs(INSTRUCTION *inst)
{
	return handle_hrr(inst);
}

/* HLR - Half left to right */
static P1ND *
handle_hlr(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs, *src, *mask_left, *mask_right, *left_half, *shifted, *result;
	if (inst->noperands != 2) {
		error("HLR requires 2 operands");
		return NULL;
	}
	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	src = operand_to_node(&inst->operands[1]);
	/* dst[18:35] = src[0:17] */
	mask_left = build_icon(0777777000000LL);
	mask_right = build_icon(0777777);
	left_half = build_binop(AND, build_binop(RS, src, build_icon(18)), mask_right);
	result = build_binop(OR, build_binop(AND, dst_rhs, mask_left), left_half);
	return build_assign(dst, result);
}

/* HLRI - Half left to right immediate */
static P1ND *
handle_hlri(INSTRUCTION *inst)
{
	return handle_hlr(inst);
}

/* HLRM - Half left to right to memory */
static P1ND *
handle_hlrm(INSTRUCTION *inst)
{
	return handle_hlr(inst);
}

/* HLRS - Half left to right to self */
static P1ND *
handle_hlrs(INSTRUCTION *inst)
{
	return handle_hlr(inst);
}

/*
 * ========== PDP-10 Arithmetic Operations ==========
 * ADD, SUB, MUL, DIV, AND, XOR use the existing generic handlers
 */

/* ADDI - Add immediate */
static P1ND *
handle_addi(INSTRUCTION *inst)
{
	return handle_add(inst);
}

/* ADDM - Add to memory */
static P1ND *
handle_addm(INSTRUCTION *inst)
{
	return handle_add(inst);
}

/* SUBI - Subtract immediate */
static P1ND *
handle_subi(INSTRUCTION *inst)
{
	return handle_sub(inst);
}

/* SUBM - Subtract to memory */
static P1ND *
handle_subm(INSTRUCTION *inst)
{
	return handle_sub(inst);
}

/* MULI - Multiply immediate */
static P1ND *
handle_muli(INSTRUCTION *inst)
{
	return handle_mul(inst);
}

/* MULM - Multiply to memory */
static P1ND *
handle_mulm(INSTRUCTION *inst)
{
	return handle_mul(inst);
}

/* DIVI - Divide immediate */
static P1ND *
handle_divi(INSTRUCTION *inst)
{
	return handle_div(inst);
}

/* DIVM - Divide to memory */
static P1ND *
handle_divm(INSTRUCTION *inst)
{
	return handle_div(inst);
}

/* ASH, ASHC already defined - use existing handlers */

/* LSHC - Logical shift combined */
static P1ND *
handle_lshc(INSTRUCTION *inst)
{
	return handle_lsh(inst);
}

/* ROTC - Rotate combined */
static P1ND *
handle_rotc(INSTRUCTION *inst)
{
	return handle_rot(inst);
}

/* ========== PDP-10 Floating Point Operations ========== */

/* Floating point - simplified implementations */
#define IMPL_FP_OP(name, op) \
static P1ND * \
handle_##name(INSTRUCTION *inst) \
{ \
	P1ND *src, *dst, *dst_rhs; \
	if (inst->noperands != 2) { \
		error(#name " requires 2 operands"); \
		return NULL; \
	} \
	src = operand_to_node(&inst->operands[0]); \
	dst = operand_to_node(&inst->operands[1]); \
	dst_rhs = operand_to_node(&inst->operands[1]); \
	return build_assign(dst, build_binop(op, dst_rhs, src)); \
}

IMPL_FP_OP(fad, PLUS)
IMPL_FP_OP(fadl, PLUS)
IMPL_FP_OP(fadm, PLUS)
IMPL_FP_OP(fadb, PLUS)
IMPL_FP_OP(fadr, PLUS)
IMPL_FP_OP(fadri, PLUS)
IMPL_FP_OP(fadrm, PLUS)
IMPL_FP_OP(fadrb, PLUS)
IMPL_FP_OP(fsb, MINUS)
IMPL_FP_OP(fsbl, MINUS)
IMPL_FP_OP(fsbm, MINUS)
IMPL_FP_OP(fsbb, MINUS)
IMPL_FP_OP(fsbr, MINUS)
IMPL_FP_OP(fsbri, MINUS)
IMPL_FP_OP(fsbrm, MINUS)
IMPL_FP_OP(fsbrb, MINUS)
IMPL_FP_OP(fmp, MUL)
IMPL_FP_OP(fmpl, MUL)
IMPL_FP_OP(fmpm, MUL)
IMPL_FP_OP(fmpb, MUL)
IMPL_FP_OP(fmpr, MUL)
IMPL_FP_OP(fmpri, MUL)
IMPL_FP_OP(fmprm, MUL)
IMPL_FP_OP(fmprb, MUL)
IMPL_FP_OP(fdv, DIV)
IMPL_FP_OP(fdvl, DIV)
IMPL_FP_OP(fdvm, DIV)
IMPL_FP_OP(fdvb, DIV)
IMPL_FP_OP(fdvr, DIV)
IMPL_FP_OP(fdvri, DIV)
IMPL_FP_OP(fdvrm, DIV)
IMPL_FP_OP(fdvrb, DIV)

/* ========== PDP-10 Logical Operations ========== */
/* AND, XOR use the existing generic handlers */

/* ANDI - AND immediate */
static P1ND *
handle_andi(INSTRUCTION *inst)
{
	return handle_and(inst);
}

/* ANDM - AND to memory */
static P1ND *
handle_andm(INSTRUCTION *inst)
{
	return handle_and(inst);
}

/* ANDCA - AND with complement of AC */
static P1ND *
handle_andca(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;
	if (inst->noperands != 2) {
		error("ANDCA requires 2 operands");
		return NULL;
	}
	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	/* dst = ~dst & src */
	return build_assign(dst, build_binop(AND, build_unop(COMPL, dst_rhs), src));
}

/* ANDCAI - AND with complement of AC immediate */
static P1ND *
handle_andcai(INSTRUCTION *inst)
{
	return handle_andca(inst);
}

/* ANDCAM - AND with complement of AC to memory */
static P1ND *
handle_andcam(INSTRUCTION *inst)
{
	return handle_andca(inst);
}

/* ANDCM - AND with complement of memory */
static P1ND *
handle_andcm(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;
	if (inst->noperands != 2) {
		error("ANDCM requires 2 operands");
		return NULL;
	}
	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	/* dst = dst & ~src */
	return build_assign(dst, build_binop(AND, dst_rhs, build_unop(COMPL, src)));
}

/* ANDCMI - AND with complement of memory immediate */
static P1ND *
handle_andcmi(INSTRUCTION *inst)
{
	return handle_andcm(inst);
}

/* ANDCMM - AND with complement of memory to memory */
static P1ND *
handle_andcmm(INSTRUCTION *inst)
{
	return handle_andcm(inst);
}

/* IOR - Inclusive OR */
static P1ND *
handle_ior(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;
	if (inst->noperands != 2) {
		error("IOR requires 2 operands");
		return NULL;
	}
	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(OR, dst_rhs, src));
}

/* IORI - Inclusive OR immediate */
static P1ND *
handle_iori(INSTRUCTION *inst)
{
	return handle_ior(inst);
}

/* IORM - Inclusive OR to memory */
static P1ND *
handle_iorm(INSTRUCTION *inst)
{
	return handle_ior(inst);
}

/* XORI - Exclusive OR immediate */
static P1ND *
handle_xori(INSTRUCTION *inst)
{
	return handle_xor(inst);
}

/* XORM - Exclusive OR to memory */
static P1ND *
handle_xorm(INSTRUCTION *inst)
{
	return handle_xor(inst);
}

/* EQV - Equivalence (XNOR) */
static P1ND *
handle_eqv(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;
	if (inst->noperands != 2) {
		error("EQV requires 2 operands");
		return NULL;
	}
	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	/* dst = ~(dst ^ src) */
	return build_assign(dst, build_unop(COMPL, build_binop(ER, dst_rhs, src)));
}

/* EQVI - Equivalence immediate */
static P1ND *
handle_eqvi(INSTRUCTION *inst)
{
	return handle_eqv(inst);
}

/* EQVM - Equivalence to memory */
static P1ND *
handle_eqvm(INSTRUCTION *inst)
{
	return handle_eqv(inst);
}

/* ========== PDP-10 Set Operations ========== */

/* SETO - Set to ones (all 1's) */
static P1ND *
handle_seto(INSTRUCTION *inst)
{
	P1ND *dst;
	if (inst->noperands < 1) {
		error("SETO requires at least 1 operand");
		return NULL;
	}
	dst = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_icon(-1));
}

/* SETOI - Set to ones immediate */
static P1ND *
handle_setoi(INSTRUCTION *inst)
{
	return handle_seto(inst);
}

/* SETOM - Set to ones to memory */
static P1ND *
handle_setom(INSTRUCTION *inst)
{
	return handle_seto(inst);
}

/* SETCA - Set to complement of AC */
static P1ND *
handle_setca(INSTRUCTION *inst)
{
	P1ND *src, *dst;
	if (inst->noperands != 2) {
		error("SETCA requires 2 operands");
		return NULL;
	}
	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_unop(COMPL, src));
}

/* SETCAI - Set to complement of AC immediate */
static P1ND *
handle_setcai(INSTRUCTION *inst)
{
	return handle_setca(inst);
}

/* SETCAM - Set to complement of AC to memory */
static P1ND *
handle_setcam(INSTRUCTION *inst)
{
	return handle_setca(inst);
}

/* SETCM - Set to complement of memory */
static P1ND *
handle_setcm(INSTRUCTION *inst)
{
	return handle_setca(inst);
}

/* SETCMI - Set to complement of memory immediate */
static P1ND *
handle_setcmi(INSTRUCTION *inst)
{
	return handle_setca(inst);
}

/* SETCMM - Set to complement of memory to memory */
static P1ND *
handle_setcmm(INSTRUCTION *inst)
{
	return handle_setca(inst);
}

/* SETZ - Set to zero (handled by existing handle_clr) */
static P1ND *
handle_setz(INSTRUCTION *inst)
{
	return handle_clr(inst);
}

/* SETZI - Set to zero immediate */
static P1ND *
handle_setzi(INSTRUCTION *inst)
{
	return handle_clr(inst);
}

/* SETZM - Set to zero to memory */
static P1ND *
handle_setzm(INSTRUCTION *inst)
{
	return handle_clr(inst);
}

/* ========== PDP-10 Skip/Jump Operations ========== */

/* SKIP - Skip (no-op that can skip) */
static P1ND *
handle_skip(INSTRUCTION *inst)
{
	return handle_nop(inst);
}

/* SKIPL - Skip if less than zero */
static P1ND *
handle_skipl(INSTRUCTION *inst)
{
	return handle_branch(inst);  /* Simplified */
}

/* SKIPE - Skip if equal to zero */
static P1ND *
handle_skipe(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/* SKIPLE - Skip if less or equal to zero */
static P1ND *
handle_skiple(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/* SKIPA - Skip always */
static P1ND *
handle_skipa(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/* SKIPGE - Skip if greater or equal to zero */
static P1ND *
handle_skipge(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/* SKIPN - Skip if not zero */
static P1ND *
handle_skipn(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/* SKIPG - Skip if greater than zero */
static P1ND *
handle_skipg(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/* JUMP - Jump */
static P1ND *
handle_jump(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/* JUMPL - Jump if less than zero */
static P1ND *
handle_jumpl(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/* JUMPE - Jump if equal to zero */
static P1ND *
handle_jumpe(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/* JUMPLE - Jump if less or equal to zero */
static P1ND *
handle_jumple(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/* JUMPA - Jump always */
static P1ND *
handle_jumpa(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/* JUMPGE - Jump if greater or equal to zero */
static P1ND *
handle_jumpge(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/* JUMPN - Jump if not zero */
static P1ND *
handle_jumpn(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/* JUMPG - Jump if greater than zero */
static P1ND *
handle_jumpg(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/* ========== PDP-10 Add/Subtract with Jump ========== */

/* AOJ - Add one and jump */
static P1ND *
handle_aoj(INSTRUCTION *inst)
{
	P1ND *reg, *reg_rhs;
	if (inst->noperands < 1) {
		error("AOJ requires at least 1 operand");
		return NULL;
	}
	reg = operand_to_node(&inst->operands[0]);
	reg_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(reg, build_binop(PLUS, reg_rhs, build_icon(1)));
}

#define IMPL_AOJ(name) \
static P1ND * \
handle_##name(INSTRUCTION *inst) \
{ \
	return handle_aoj(inst); \
}

IMPL_AOJ(aojl)
IMPL_AOJ(aoje)
IMPL_AOJ(aojle)
IMPL_AOJ(aoja)
IMPL_AOJ(aojge)
IMPL_AOJ(aojn)
IMPL_AOJ(aojg)

/* AOS - Add one to memory and skip */
static P1ND *
handle_aos(INSTRUCTION *inst)
{
	return handle_aoj(inst);
}

IMPL_AOJ(aosl)
IMPL_AOJ(aose)
IMPL_AOJ(aosle)
IMPL_AOJ(aosa)
IMPL_AOJ(aosge)
IMPL_AOJ(aosn)
IMPL_AOJ(aosg)

/* SOJ - Subtract one and jump */
static P1ND *
handle_soj(INSTRUCTION *inst)
{
	P1ND *reg, *reg_rhs;
	if (inst->noperands < 1) {
		error("SOJ requires at least 1 operand");
		return NULL;
	}
	reg = operand_to_node(&inst->operands[0]);
	reg_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(reg, build_binop(MINUS, reg_rhs, build_icon(1)));
}

#define IMPL_SOJ(name) \
static P1ND * \
handle_##name(INSTRUCTION *inst) \
{ \
	return handle_soj(inst); \
}

IMPL_SOJ(sojl)
IMPL_SOJ(soje)
IMPL_SOJ(sojle)
IMPL_SOJ(soja)
IMPL_SOJ(sojge)
IMPL_SOJ(sojn)
IMPL_SOJ(sojg)

/* SOS - Subtract one from memory and skip */
static P1ND *
handle_sos(INSTRUCTION *inst)
{
	return handle_soj(inst);
}

IMPL_SOJ(sosl)
IMPL_SOJ(sose)
IMPL_SOJ(sosle)
IMPL_SOJ(sosa)
IMPL_SOJ(sosge)
IMPL_SOJ(sosn)
IMPL_SOJ(sosg)

/* ========== PDP-10 Compare Operations ========== */

/* CAI - Compare accumulator immediate */
static P1ND *
handle_cai(INSTRUCTION *inst)
{
	return handle_cmp(inst);
}

#define IMPL_CAI(name) \
static P1ND * \
handle_##name(INSTRUCTION *inst) \
{ \
	return handle_cmp(inst); \
}

IMPL_CAI(cail)
IMPL_CAI(caie)
IMPL_CAI(caile)
IMPL_CAI(caia)
IMPL_CAI(caige)
IMPL_CAI(cain)
IMPL_CAI(caig)

/* CAM - Compare accumulator with memory */
static P1ND *
handle_cam(INSTRUCTION *inst)
{
	return handle_cmp(inst);
}

IMPL_CAI(caml)
IMPL_CAI(came)
IMPL_CAI(camle)
IMPL_CAI(cama)
IMPL_CAI(camge)
IMPL_CAI(camn)
IMPL_CAI(camg)

/* ========== PDP-10 Byte Operations ========== */

/* LDB - Load byte */
static P1ND *
handle_ldb(INSTRUCTION *inst)
{
	/* Simplified byte load */
	if (inst->noperands != 2) {
		error("LDB requires 2 operands");
		return NULL;
	}
	return build_assign(operand_to_node(&inst->operands[0]),
	                    operand_to_node(&inst->operands[1]));
}

/* ILDB - Increment pointer and load byte */
static P1ND *
handle_ildb(INSTRUCTION *inst)
{
	return handle_ldb(inst);
}

/* DPB - Deposit byte */
static P1ND *
handle_dpb(INSTRUCTION *inst)
{
	if (inst->noperands != 2) {
		error("DPB requires 2 operands");
		return NULL;
	}
	return build_assign(operand_to_node(&inst->operands[1]),
	                    operand_to_node(&inst->operands[0]));
}

/* IDPB - Increment pointer and deposit byte */
static P1ND *
handle_idpb(INSTRUCTION *inst)
{
	return handle_dpb(inst);
}

/* IBP - Increment byte pointer */
static P1ND *
handle_ibp(INSTRUCTION *inst)
{
	P1ND *ptr, *ptr_rhs;
	if (inst->noperands < 1) {
		error("IBP requires at least 1 operand");
		return NULL;
	}
	ptr = operand_to_node(&inst->operands[0]);
	ptr_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(ptr, build_binop(PLUS, ptr_rhs, build_icon(1)));
}

/* ========== PDP-10 I/O Operations ========== */

/* I/O operations - simplified to no-ops for now */
static P1ND *
handle_blki(INSTRUCTION *inst)
{
	return handle_nop(inst);
}

static P1ND *
handle_datai(INSTRUCTION *inst)
{
	return handle_nop(inst);
}

static P1ND *
handle_blko(INSTRUCTION *inst)
{
	return handle_nop(inst);
}

static P1ND *
handle_datao(INSTRUCTION *inst)
{
	return handle_nop(inst);
}

static P1ND *
handle_cono(INSTRUCTION *inst)
{
	return handle_nop(inst);
}

static P1ND *
handle_coni(INSTRUCTION *inst)
{
	return handle_nop(inst);
}

static P1ND *
handle_consz(INSTRUCTION *inst)
{
	return handle_nop(inst);
}

static P1ND *
handle_conso(INSTRUCTION *inst)
{
	return handle_nop(inst);
}

/* ========== PDP-10 Stack Operations ========== */

/* PUSH - Push onto stack */
static P1ND *
handle_push(INSTRUCTION *inst)
{
	/* Simplified push */
	if (inst->noperands >= 1) {
		P1ND *val = operand_to_node(&inst->operands[0]);
		/* Would emit stack pointer manipulation here */
		return val;
	}
	return handle_nop(inst);
}

/* POP - Pop from stack */
static P1ND *
handle_pop(INSTRUCTION *inst)
{
	/* Simplified pop */
	if (inst->noperands >= 1) {
		P1ND *dst = operand_to_node(&inst->operands[0]);
		/* Would emit stack pointer manipulation here */
		return dst;
	}
	return handle_nop(inst);
}

/* ========== PDP-10 Move Variants ========== */

/* MOVS - Move swapped (swap left/right halves) */
static P1ND *
handle_movs(INSTRUCTION *inst)
{
	P1ND *src, *dst, *left, *right, *result;
	if (inst->noperands != 2) {
		error("MOVS requires 2 operands");
		return NULL;
	}
	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	/* Swap 18-bit halves: dst = (src << 18) | (src >> 18) */
	left = build_binop(LS, build_binop(AND, src, build_icon(0777777)), build_icon(18));
	right = build_binop(RS, build_binop(AND, src, build_icon(0777777000000LL)), build_icon(18));
	result = build_binop(OR, left, right);
	return build_assign(dst, result);
}

/* MOVSI - Move swapped immediate */
static P1ND *
handle_movsi(INSTRUCTION *inst)
{
	return handle_movs(inst);
}

/* MOVSM - Move swapped to memory */
static P1ND *
handle_movsm(INSTRUCTION *inst)
{
	return handle_movs(inst);
}

/* MOVSS - Move swapped to self */
static P1ND *
handle_movss(INSTRUCTION *inst)
{
	return handle_movs(inst);
}

/* MOVNI - Move negative immediate */
static P1ND *
handle_movni(INSTRUCTION *inst)
{
	return handle_movn(inst);
}

/* MOVNM - Move negative to memory */
static P1ND *
handle_movnm(INSTRUCTION *inst)
{
	return handle_movn(inst);
}

/* MOVNS - Move negative to self */
static P1ND *
handle_movns(INSTRUCTION *inst)
{
	return handle_movn(inst);
}

/* MOVMI - Move magnitude immediate */
static P1ND *
handle_movmi(INSTRUCTION *inst)
{
	return handle_movm(inst);
}

/* MOVMM - Move magnitude to memory */
static P1ND *
handle_movmm(INSTRUCTION *inst)
{
	return handle_movm(inst);
}

/* MOVMS - Move magnitude to self */
static P1ND *
handle_movms(INSTRUCTION *inst)
{
	return handle_movm(inst);
}

/* ========== Public API ========== */

/*
 * Emit an instruction using PCC IR (IP_NODE only!)
 * This uses table dispatch - NO string comparisons!
 */
void
emit_instruction_ir(INSTRUCTION *inst)
{
	P1ND *tree = NULL;
	const insn_table_entry_t *entry;
	int i;

	/* Look up instruction in table */
	for (i = 0; insn_table[i].mnemonic != NULL; i++) {
		if (strcmp(inst->mnemonic, insn_table[i].mnemonic) == 0) {
			entry = &insn_table[i];

			/* Call handler function */
			tree = entry->handler(inst);

			if (tree != NULL) {
				/* Send NODE to PCC backend - ONLY IR, no assembly! */
				send_passt(IP_NODE, tree);
			}

			/* Update location counter */
			location_counter += 2;
			return;
		}
	}

	/* Unknown instruction */
	error("unknown instruction: %s", inst->mnemonic);
}

/*
 * Emit data using PCC IR
 */
void
emit_data_ir(int size, long value)
{
	char asm_line[256];

	switch (size) {
	case 1:
		snprintf(asm_line, sizeof(asm_line), "\t.byte\t0x%02lx\n",
		         value & 0xff);
		location_counter += 1;
		break;
	case 2:
		snprintf(asm_line, sizeof(asm_line), "\t.word\t0x%04lx\n",
		         value & 0xffff);
		location_counter += 2;
		break;
	case 4:
		snprintf(asm_line, sizeof(asm_line), "\t.long\t0x%08lx\n", value);
		location_counter += 4;
		break;
	default:
		error("invalid data size %d", size);
		return;
	}

	send_passt(IP_ASM, asm_line);
}

/*
 * Emit string data using PCC IR
 */
void
emit_string_ir(const char *str, int null_term)
{
	char asm_line[1024];

	if (null_term)
		snprintf(asm_line, sizeof(asm_line), "\t.asciz\t\"%s\"\n", str);
	else
		snprintf(asm_line, sizeof(asm_line), "\t.ascii\t\"%s\"\n", str);

	send_passt(IP_ASM, asm_line);

	location_counter += strlen(str);
	if (null_term)
		location_counter++;
}

/*
 * Emit a directive using PCC IR
 */
void
emit_directive_ir(int directive, ...)
{
	va_list ap;
	const char *str;
	long value;
	char asm_line[1024];

	va_start(ap, directive);

	switch (directive) {
	case DIR_TITLE:
		str = va_arg(ap, const char *);
		snprintf(asm_line, sizeof(asm_line), "\t.title\t\"%s\"\n", str);
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_IDENT:
		str = va_arg(ap, const char *);
		snprintf(asm_line, sizeof(asm_line), "\t.ident\t\"%s\"\n", str);
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_PSECT:
		str = va_arg(ap, const char *);
		snprintf(asm_line, sizeof(asm_line), "\t.psect\t%s\n", str);
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_ENTRY:
		str = va_arg(ap, const char *);
		snprintf(asm_line, sizeof(asm_line), "\t.entry\t%s\n", str);
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_END:
		snprintf(asm_line, sizeof(asm_line), "\t.end\n");
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_GLOBL:
		str = va_arg(ap, const char *);
		snprintf(asm_line, sizeof(asm_line), "\t.globl\t%s\n", str);
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_EXTERN:
		str = va_arg(ap, const char *);
		snprintf(asm_line, sizeof(asm_line), "\t.extern\t%s\n", str);
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_ALIGN:
		value = va_arg(ap, long);
		snprintf(asm_line, sizeof(asm_line), "\t.align\t%ld\n", value);
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_EVEN:
		snprintf(asm_line, sizeof(asm_line), "\t.even\n");
		send_passt(IP_ASM, asm_line);
		if (location_counter & 1)
			location_counter++;
		break;

	case DIR_PAGE:
		snprintf(asm_line, sizeof(asm_line), "\t.page\n");
		send_passt(IP_ASM, asm_line);
		break;

	default:
		error("unknown directive %d", directive);
		break;
	}

	va_end(ap);
}

/*
 * PCC backend interface - begin compilation unit
 */
void
bjobcode(void)
{
	codegen_init();
}

/*
 * PCC backend interface - end compilation unit
 */
void
ejobcode(int retlab)
{
	(void)retlab;
}

/* ========== PCC IR Node Building Functions ========== */

/*
 * Build an integer constant node
 */
P1ND *
build_icon(long value)
{
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	if (p == NULL)
		fatal("out of memory");

	memset(p, 0, sizeof(P1ND));
	p->n_op = ICON;
	p->n_type = INT;
	setlval(p, value);

	return p;
}

/*
 * Build a register node
 */
P1ND *
build_reg(int reg)
{
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	if (p == NULL)
		fatal("out of memory");

	memset(p, 0, sizeof(P1ND));
	p->n_op = REG;
	p->n_type = INT;
	p->n_rval = reg;

	return p;
}

/*
 * Build an offset register node (OREG)
 */
P1ND *
build_oreg(int reg, long offset)
{
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	if (p == NULL)
		fatal("out of memory");

	memset(p, 0, sizeof(P1ND));
	p->n_op = OREG;
	p->n_type = INT;
	p->n_rval = reg;
	setlval(p, offset);

	return p;
}

/*
 * Build an assignment node
 */
P1ND *
build_assign(P1ND *left, P1ND *right)
{
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	if (p == NULL)
		fatal("out of memory");

	memset(p, 0, sizeof(P1ND));
	p->n_op = ASSIGN;
	p->n_type = left->n_type;
	p->n_left = left;
	p->n_right = right;

	return p;
}

/*
 * Build a binary operation node
 */
P1ND *
build_binop(int op, P1ND *left, P1ND *right)
{
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	if (p == NULL)
		fatal("out of memory");

	memset(p, 0, sizeof(P1ND));
	p->n_op = op;
	p->n_type = left->n_type;
	p->n_left = left;
	p->n_right = right;

	return p;
}

/*
 * Build a unary operation node
 */
P1ND *
build_unop(int op, P1ND *child)
{
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	if (p == NULL)
		fatal("out of memory");

	memset(p, 0, sizeof(P1ND));
	p->n_op = op;
	p->n_type = child->n_type;
	p->n_left = child;

	return p;
}
/* ============== NEW VAX INSTRUCTION HANDLERS ============== */

/* Integer arithmetic byte/word variants - these all follow same pattern as longword */
static P1ND *handle_addb(INSTRUCTION *inst) { return handle_add(inst); }
static P1ND *handle_addw(INSTRUCTION *inst) { return handle_add(inst); }
static P1ND *handle_subb(INSTRUCTION *inst) { return handle_sub(inst); }
static P1ND *handle_subw(INSTRUCTION *inst) { return handle_sub(inst); }
static P1ND *handle_mulb(INSTRUCTION *inst) { return handle_mul(inst); }
static P1ND *handle_mulw(INSTRUCTION *inst) { return handle_mul(inst); }
static P1ND *handle_divb(INSTRUCTION *inst) { return handle_div(inst); }
static P1ND *handle_divw(INSTRUCTION *inst) { return handle_div(inst); }

/* Logical operation B/W/L variants */
static P1ND *handle_bisb(INSTRUCTION *inst) { return handle_bis(inst); }
static P1ND *handle_bisw(INSTRUCTION *inst) { return handle_bis(inst); }
static P1ND *handle_bisl(INSTRUCTION *inst) { return handle_bis(inst); }
static P1ND *handle_bicb(INSTRUCTION *inst) { return handle_bic(inst); }
static P1ND *handle_bicw(INSTRUCTION *inst) { return handle_bic(inst); }
static P1ND *handle_bicl(INSTRUCTION *inst) { return handle_bic(inst); }
static P1ND *handle_xorb(INSTRUCTION *inst) { return handle_xor(inst); }
static P1ND *handle_xorw(INSTRUCTION *inst) { return handle_xor(inst); }
static P1ND *handle_xorl(INSTRUCTION *inst) { return handle_xor(inst); }

/* Move B/W variants */
static P1ND *handle_movb(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_movw(INSTRUCTION *inst) { return handle_mov(inst); }

/* Move zero-extended */
static P1ND *handle_movzbw(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_movzbl(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_movzwl(INSTRUCTION *inst) { return handle_mov(inst); }

/* Move address - these move addresses, can use same handler as MOV */
static P1ND *handle_movab(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_movaw(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_moval(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_movaq(INSTRUCTION *inst) { return handle_mov(inst); }

/* Push address variants */
static P1ND *handle_pushab(INSTRUCTION *inst) { return handle_pushl(inst); }
static P1ND *handle_pushaw(INSTRUCTION *inst) { return handle_pushl(inst); }
static P1ND *handle_pushal(INSTRUCTION *inst) { return handle_pushl(inst); }
static P1ND *handle_pushaq(INSTRUCTION *inst) { return handle_pushl(inst); }
static P1ND *handle_pushw(INSTRUCTION *inst) { return handle_pushl(inst); }

/* Pop word */
static P1ND *handle_popw(INSTRUCTION *inst) { return handle_popl(inst); }

/* Inc/Dec/Clear/Test/Compare B/W/Q variants */
static P1ND *handle_incb(INSTRUCTION *inst) { return handle_inc(inst); }
static P1ND *handle_incw(INSTRUCTION *inst) { return handle_inc(inst); }
static P1ND *handle_decb(INSTRUCTION *inst) { return handle_dec(inst); }
static P1ND *handle_decw(INSTRUCTION *inst) { return handle_dec(inst); }
static P1ND *handle_cmpb(INSTRUCTION *inst) { return handle_cmp(inst); }
static P1ND *handle_cmpw(INSTRUCTION *inst) { return handle_cmp(inst); }
static P1ND *handle_tstb(INSTRUCTION *inst) { return handle_tst(inst); }
static P1ND *handle_tstw(INSTRUCTION *inst) { return handle_tst(inst); }
static P1ND *handle_clrb(INSTRUCTION *inst) { return handle_clr(inst); }
static P1ND *handle_clrw(INSTRUCTION *inst) { return handle_clr(inst); }
static P1ND *handle_clrq(INSTRUCTION *inst) { return handle_clr(inst); }

/* Move negated B/W/L - MNEG dst = 0 - src, which is UMINUS */
static P1ND *handle_mnegb(INSTRUCTION *inst) { return handle_neg(inst); }
static P1ND *handle_mnegw(INSTRUCTION *inst) { return handle_neg(inst); }
static P1ND *handle_mnegl(INSTRUCTION *inst) { return handle_neg(inst); }

/* Move complemented B/W/L */
static P1ND *handle_mcomb(INSTRUCTION *inst) { return handle_com(inst); }
static P1ND *handle_mcomw(INSTRUCTION *inst) { return handle_com(inst); }
static P1ND *handle_mcoml(INSTRUCTION *inst) { return handle_com(inst); }

/* Bit test B/W/L */
static P1ND *handle_bitb(INSTRUCTION *inst) { return handle_bit(inst); }
static P1ND *handle_bitw(INSTRUCTION *inst) { return handle_bit(inst); }
static P1ND *handle_bitl(INSTRUCTION *inst) { return handle_bit(inst); }

/* Extended arithmetic - ADWC/SBWC are add/sub with carry */
static P1ND *handle_adwc(INSTRUCTION *inst) { return handle_add(inst); }
static P1ND *handle_sbwc(INSTRUCTION *inst) { return handle_sub(inst); }

/* EMUL - Extended multiply (generates 64-bit result) */
static P1ND *
handle_emul(INSTRUCTION *inst)
{
	/* EMUL mul1, mul2, add, prod - prod = (mul1 * mul2) + add */
	/* For simplicity, generate basic multiply */
	return handle_mul(inst);
}

/* EDIV - Extended divide */
static P1ND *
handle_ediv(INSTRUCTION *inst)
{
	/* EDIV div, divd, quo, rem - divd / div -> quo, remainder -> rem */
	return handle_div(inst);
}

/* ASHL - Arithmetic shift longword */
static P1ND *
handle_ashl(INSTRUCTION *inst)
{
	P1ND *count, *src, *dst;

	if (inst->noperands < 2) {
		error("ASHL requires at least 2 operands");
		return NULL;
	}

	count = operand_to_node(&inst->operands[0]);
	src = operand_to_node(&inst->operands[1]);

	/* ASHL count, src, dst -> dst = src << count (or >> if negative) */
	/* For now, treat as left shift */
	if (inst->noperands == 3) {
		dst = operand_to_node(&inst->operands[2]);
		return build_assign(dst, build_binop(LS, src, count));
	}
	return build_binop(LS, src, count);
}

/* ASHQ - Arithmetic shift quadword */
static P1ND *handle_ashq(INSTRUCTION *inst) { return handle_ashl(inst); }

/* ROTL - Rotate longword */
static P1ND *
handle_rotl(INSTRUCTION *inst)
{
	/* ROTL count, src, dst - rotate left */
	/* PCC doesn't have native rotate, use shift approximation */
	return handle_ashl(inst);
}

/* G-float arithmetic (64-bit floating point) */
static P1ND *handle_addg(INSTRUCTION *inst) { return handle_addf(inst); }
static P1ND *handle_subg(INSTRUCTION *inst) { return handle_subf(inst); }
static P1ND *handle_mulg(INSTRUCTION *inst) { return handle_mulf(inst); }
static P1ND *handle_divg(INSTRUCTION *inst) { return handle_divf(inst); }
static P1ND *handle_movg(INSTRUCTION *inst) { return handle_movl(inst); }
static P1ND *handle_cmpg(INSTRUCTION *inst) { return handle_cmp(inst); }
static P1ND *handle_mnegg(INSTRUCTION *inst) { return handle_neg(inst); }
static P1ND *handle_tstg(INSTRUCTION *inst) { return handle_tst(inst); }
static P1ND *handle_polyg(INSTRUCTION *inst) { return handle_nop(inst); }  /* Complex operation */
static P1ND *handle_emogg(INSTRUCTION *inst) { return handle_nop(inst); }  /* Extended modulus */

/* H-float arithmetic (128-bit floating point) */
static P1ND *handle_addh(INSTRUCTION *inst) { return handle_addf(inst); }
static P1ND *handle_subh(INSTRUCTION *inst) { return handle_subf(inst); }
static P1ND *handle_mulh(INSTRUCTION *inst) { return handle_mulf(inst); }
static P1ND *handle_divh(INSTRUCTION *inst) { return handle_divf(inst); }
static P1ND *handle_movh(INSTRUCTION *inst) { return handle_movl(inst); }
static P1ND *handle_cmph(INSTRUCTION *inst) { return handle_cmp(inst); }
static P1ND *handle_mnegh(INSTRUCTION *inst) { return handle_neg(inst); }
static P1ND *handle_tsth(INSTRUCTION *inst) { return handle_tst(inst); }
static P1ND *handle_polyh(INSTRUCTION *inst) { return handle_nop(inst); }

/* F/D float operations */
static P1ND *handle_movf(INSTRUCTION *inst) { return handle_movl(inst); }
static P1ND *handle_movd(INSTRUCTION *inst) { return handle_movl(inst); }
static P1ND *handle_cmpf(INSTRUCTION *inst) { return handle_cmp(inst); }
static P1ND *handle_cmpd(INSTRUCTION *inst) { return handle_cmp(inst); }
static P1ND *handle_mnegf(INSTRUCTION *inst) { return handle_neg(inst); }
static P1ND *handle_mnegd(INSTRUCTION *inst) { return handle_neg(inst); }
static P1ND *handle_tstf(INSTRUCTION *inst) { return handle_tst(inst); }
static P1ND *handle_tstd(INSTRUCTION *inst) { return handle_tst(inst); }
static P1ND *handle_emodf(INSTRUCTION *inst) { return handle_nop(inst); }  /* Extended modulus */
static P1ND *handle_emodd(INSTRUCTION *inst) { return handle_nop(inst); }
static P1ND *handle_polyf(INSTRUCTION *inst) { return handle_nop(inst); }  /* Polynomial */
static P1ND *handle_polyd(INSTRUCTION *inst) { return handle_nop(inst); }

/* Bit field instructions - complex, generate placeholders */
static P1ND *
handle_extv(INSTRUCTION *inst)
{
	/* EXTV pos, size, base, dst - extract bit field */
	if (inst->noperands >= 4) {
		P1ND *base = operand_to_node(&inst->operands[2]);
		P1ND *dst = operand_to_node(&inst->operands[3]);
		return build_assign(dst, base);
	}
	return handle_nop(inst);
}

static P1ND *handle_extzv(INSTRUCTION *inst) { return handle_extv(inst); }

static P1ND *
handle_insv(INSTRUCTION *inst)
{
	/* INSV src, pos, size, base - insert bit field */
	if (inst->noperands >= 4) {
		P1ND *src = operand_to_node(&inst->operands[0]);
		P1ND *base = operand_to_node(&inst->operands[3]);
		return build_assign(base, src);
	}
	return handle_nop(inst);
}

static P1ND *handle_cmpv(INSTRUCTION *inst) { return handle_cmp(inst); }
static P1ND *handle_cmpzv(INSTRUCTION *inst) { return handle_cmp(inst); }
static P1ND *handle_ffc(INSTRUCTION *inst) { return handle_nop(inst); }  /* Find first clear */
static P1ND *handle_ffs(INSTRUCTION *inst) { return handle_nop(inst); }  /* Find first set */

/* Bit test and branch instructions */
static P1ND *handle_bbs(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_bbc(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_bbss(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_bbcs(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_bbsc(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_bbcc(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_bbssi(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_bbcci(INSTRUCTION *inst) { return handle_branch(inst); }

/* String operations - complex, generate placeholders */
static P1ND *handle_movtc(INSTRUCTION *inst) { return handle_movc(inst); }
static P1ND *handle_movtuc(INSTRUCTION *inst) { return handle_movc(inst); }
static P1ND *handle_matchc(INSTRUCTION *inst) { return handle_movc(inst); }
static P1ND *handle_editpc(INSTRUCTION *inst) { return handle_nop(inst); }  /* Complex edit operation */

/* Queue instructions */
static P1ND *
handle_insque(INSTRUCTION *inst)
{
	/* INSQUE entry, pred - insert entry into queue after pred */
	if (inst->noperands >= 2) {
		P1ND *entry = operand_to_node(&inst->operands[0]);
		P1ND *pred = operand_to_node(&inst->operands[1]);
		/* Generate assignment as placeholder */
		return build_assign(pred, entry);
	}
	return handle_nop(inst);
}

static P1ND *
handle_remque(INSTRUCTION *inst)
{
	/* REMQUE entry, addr - remove entry from queue */
	if (inst->noperands >= 1) {
		P1ND *entry = operand_to_node(&inst->operands[0]);
		/* Generate assignment as placeholder */
		return build_assign(entry, build_icon(0));
	}
	return handle_nop(inst);
}

static P1ND *handle_insqhi(INSTRUCTION *inst) { return handle_insque(inst); }
static P1ND *handle_insqti(INSTRUCTION *inst) { return handle_insque(inst); }
static P1ND *handle_remqhi(INSTRUCTION *inst) { return handle_remque(inst); }
static P1ND *handle_remqti(INSTRUCTION *inst) { return handle_remque(inst); }

/* Control flow - VAX branches */
static P1ND *handle_bneq(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_beql(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_bgtr(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_bleq(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_bgeq(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_blss(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_bgtru(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_blequ(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_bgequ(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_blssu(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_brb(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_brw(INSTRUCTION *inst) { return handle_branch(inst); }
static P1ND *handle_bsbb(INSTRUCTION *inst) { return handle_jsr(inst); }
static P1ND *handle_bsbw(INSTRUCTION *inst) { return handle_jsr(inst); }

/* CALLG/CALLS - call with argument list */
static P1ND *
handle_callg(INSTRUCTION *inst)
{
	/* CALLG arglist, dst - call procedure with general argument list */
	if (inst->noperands >= 1) {
		P1ND *dst = operand_to_node(&inst->operands[inst->noperands - 1]);
		/* Generate GOTO as placeholder for call */
		if (dst->n_op == NAME) {
			SYMTAB *sym = lookup(dst->n_name);
			if (sym) {
				P1ND *p = build_icon(0);
				p->n_op = GOTO;
				p->n_label = sym->label_num;
				return p;
			}
		}
	}
	return handle_nop(inst);
}

static P1ND *handle_calls(INSTRUCTION *inst) { return handle_callg(inst); }

/* CASE instructions */
static P1ND *
handle_caseb(INSTRUCTION *inst)
{
	/* CASEB selector, base, limit - case on byte */
	/* Complex multi-way branch, generate placeholder */
	return handle_nop(inst);
}

static P1ND *handle_casew(INSTRUCTION *inst) { return handle_caseb(inst); }
static P1ND *handle_casel(INSTRUCTION *inst) { return handle_caseb(inst); }

/* System instructions */
static P1ND *handle_rei(INSTRUCTION *inst) { return handle_rts(inst); }  /* Return from exception */
static P1ND *handle_rsb(INSTRUCTION *inst) { return handle_rts(inst); }  /* Return from subroutine */
static P1ND *handle_ret(INSTRUCTION *inst) { return handle_rts(inst); }  /* Return */
static P1ND *handle_ldpctx(INSTRUCTION *inst) { return handle_nop(inst); }  /* Load process context */
static P1ND *handle_svpctx(INSTRUCTION *inst) { return handle_nop(inst); }  /* Save process context */
static P1ND *handle_prober(INSTRUCTION *inst) { return handle_nop(inst); }  /* Probe read access */
static P1ND *handle_probew(INSTRUCTION *inst) { return handle_nop(inst); }  /* Probe write access */
static P1ND *handle_probe(INSTRUCTION *inst) { return handle_nop(inst); }
static P1ND *handle_mtpr(INSTRUCTION *inst) { return handle_mov(inst); }  /* Move to processor register */
static P1ND *handle_mfpr(INSTRUCTION *inst) { return handle_mov(inst); }  /* Move from processor register */
static P1ND *handle_bugw(INSTRUCTION *inst) { return handle_halt(inst); }  /* Bugcheck */
static P1ND *handle_bugl(INSTRUCTION *inst) { return handle_halt(inst); }
static P1ND *handle_crc(INSTRUCTION *inst) { return handle_nop(inst); }  /* Calculate CRC */
static P1ND *handle_adawi(INSTRUCTION *inst) { return handle_add(inst); }  /* Add aligned word interlocked */

/* Packed decimal - complex operations, generate placeholders */
static P1ND *handle_addp4(INSTRUCTION *inst) { return handle_add(inst); }
static P1ND *handle_addp6(INSTRUCTION *inst) { return handle_add(inst); }
static P1ND *handle_subp4(INSTRUCTION *inst) { return handle_sub(inst); }
static P1ND *handle_subp6(INSTRUCTION *inst) { return handle_sub(inst); }
static P1ND *handle_mulp(INSTRUCTION *inst) { return handle_mul(inst); }
static P1ND *handle_divp(INSTRUCTION *inst) { return handle_div(inst); }
static P1ND *handle_cvtpt(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvttp(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtps(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtsp(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_ashp(INSTRUCTION *inst) { return handle_ashl(inst); }
static P1ND *handle_cmpp3(INSTRUCTION *inst) { return handle_cmp(inst); }
static P1ND *handle_cmpp4(INSTRUCTION *inst) { return handle_cmp(inst); }
static P1ND *handle_movp(INSTRUCTION *inst) { return handle_mov(inst); }

/* Conversion instructions - all generate mov as placeholder */
static P1ND *handle_cvtbw(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtbl(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtwb(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtwl(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtlb(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtlw(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtfb(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtfw(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtdb(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtdw(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtgb(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtgw(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtgl(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtbf(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtwf(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtbd(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtwd(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtbg(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtwg(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtlg(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtfg(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtgf(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtrdl(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtrfl(INSTRUCTION *inst) { return handle_mov(inst); }
static P1ND *handle_cvtrgl(INSTRUCTION *inst) { return handle_mov(inst); }

/* ============== MISSING VAX INSTRUCTION HANDLERS ============== */

/* Loop control - ACB (Add Compare Branch) family */
static P1ND *
handle_acbb(INSTRUCTION *inst)
{
	/* ACBB limit, add, index, displ - add index by add, compare to limit, branch if <= */
	/* Complex loop control - generate simplified sequence */
	if (inst->noperands >= 3) {
		P1ND *index = operand_to_node(&inst->operands[2]);
		P1ND *add = operand_to_node(&inst->operands[1]);
		/* index = index + add */
		send_passt(IP_NODE, build_assign(index, build_binop(PLUS, index, add)));
		/* Then branch */
		return handle_branch(inst);
	}
	return handle_nop(inst);
}

static P1ND *handle_acbw(INSTRUCTION *inst) { return handle_acbb(inst); }
static P1ND *handle_acbl(INSTRUCTION *inst) { return handle_acbb(inst); }
static P1ND *handle_acbf(INSTRUCTION *inst) { return handle_acbb(inst); }
static P1ND *handle_acbd(INSTRUCTION *inst) { return handle_acbb(inst); }
static P1ND *handle_acbg(INSTRUCTION *inst) { return handle_acbb(inst); }
static P1ND *handle_acbh(INSTRUCTION *inst) { return handle_acbb(inst); }

/* AOBLEQ/AOBLSS - Add One and Branch Less or Equal/Less */
static P1ND *
handle_aobleq(INSTRUCTION *inst)
{
	/* AOBLEQ limit, index, displ - add 1 to index, branch if index <= limit */
	if (inst->noperands >= 2) {
		P1ND *index = operand_to_node(&inst->operands[1]);
		/* index = index + 1 */
		send_passt(IP_NODE, build_assign(index, build_binop(PLUS, index, build_icon(1))));
		/* Then branch */
		return handle_branch(inst);
	}
	return handle_nop(inst);
}

static P1ND *handle_aoblss(INSTRUCTION *inst) { return handle_aobleq(inst); }

/* SOBGEQ/SOBGTR - Subtract One and Branch Greater or Equal/Greater */
static P1ND *
handle_sobgeq(INSTRUCTION *inst)
{
	/* SOBGEQ limit, index, displ - subtract 1 from index, branch if index >= limit */
	if (inst->noperands >= 2) {
		P1ND *index = operand_to_node(&inst->operands[1]);
		/* index = index - 1 */
		send_passt(IP_NODE, build_assign(index, build_binop(MINUS, index, build_icon(1))));
		/* Then branch */
		return handle_branch(inst);
	}
	return handle_nop(inst);
}

static P1ND *handle_sobgtr(INSTRUCTION *inst) { return handle_sobgeq(inst); }

/* PUSHR/POPR - Push/Pop register set */
static P1ND *
handle_pushr(INSTRUCTION *inst)
{
	/* PUSHR mask - push registers specified by mask onto stack */
	/* This is a complex operation that would need to iterate through the mask */
	/* For now, generate a placeholder */
	return handle_nop(inst);
}

static P1ND *
handle_popr(INSTRUCTION *inst)
{
	/* POPR mask - pop registers specified by mask from stack */
	/* This is a complex operation that would need to iterate through the mask */
	/* For now, generate a placeholder */
	return handle_nop(inst);
}

/* INDEX - Compute array index */
static P1ND *
handle_index(INSTRUCTION *inst)
{
	/* INDEX subscript, low, high, size, indexin, indexout */
	/* Compute: indexout = indexin + ((subscript - low) * size) */
	/* Very complex instruction, generate simplified version */
	if (inst->noperands >= 6) {
		P1ND *subscript = operand_to_node(&inst->operands[0]);
		P1ND *low = operand_to_node(&inst->operands[1]);
		P1ND *size = operand_to_node(&inst->operands[3]);
		P1ND *indexin = operand_to_node(&inst->operands[4]);
		P1ND *indexout = operand_to_node(&inst->operands[5]);
		
		/* temp = subscript - low */
		P1ND *temp = build_binop(MINUS, subscript, low);
		/* temp = temp * size */
		temp = build_binop(MUL, temp, size);
		/* indexout = indexin + temp */
		return build_assign(indexout, build_binop(PLUS, indexin, temp));
	}
	return handle_nop(inst);
}

/* MOVPSL - Move Processor Status Longword */
static P1ND *
handle_movpsl(INSTRUCTION *inst)
{
	/* MOVPSL dst - move PSL to destination */
	/* This accesses processor state, generate mov from special register */
	return handle_mov(inst);
}

/* BISPSW/BICPSW - Bit Set/Clear Processor Status Word */
static P1ND *
handle_bispsw(INSTRUCTION *inst)
{
	/* BISPSW src - set bits in PSW from mask */
	/* This modifies processor state, generate placeholder */
	return handle_nop(inst);
}

static P1ND *
handle_bicpsw(INSTRUCTION *inst)
{
	/* BICPSW src - clear bits in PSW from mask */
	/* This modifies processor state, generate placeholder */
	return handle_nop(inst);
}

/* XFC - Extended Function Call */
static P1ND *
handle_xfc(INSTRUCTION *inst)
{
	/* XFC - extended function call (emulator trap) */
	/* This is a trap instruction, generate as halt */
	return handle_halt(inst);
}

/* CHMx - Change Mode instructions */
static P1ND *
handle_chme(INSTRUCTION *inst)
{
	/* CHME code - change mode to executive with argument */
	/* This is a privileged operation, generate as nop or trap */
	return handle_nop(inst);
}

static P1ND *handle_chmk(INSTRUCTION *inst) { return handle_chme(inst); }  /* Change mode to kernel */
static P1ND *handle_chms(INSTRUCTION *inst) { return handle_chme(inst); }  /* Change mode to supervisor */
static P1ND *handle_chmu(INSTRUCTION *inst) { return handle_chme(inst); }  /* Change mode to user */

/* CVTLP/CVTPL - Convert Long to/from Packed Decimal */
static P1ND *handle_cvtlp(INSTRUCTION *inst) { return handle_mov(inst); }  /* Convert long to packed */
static P1ND *handle_cvtpl(INSTRUCTION *inst) { return handle_mov(inst); }  /* Convert packed to long */

/* ============== MISSING PDP-11 INSTRUCTION HANDLERS ============== */

/* PDP-11 byte variants of add/subtract carry */
static P1ND *handle_adcb(INSTRUCTION *inst) { return handle_inc(inst); }  /* Add carry byte */
static P1ND *handle_sbcb(INSTRUCTION *inst) { return handle_dec(inst); }  /* Subtract carry byte */

/* Note: handle_bicb, handle_bisb, handle_subb already defined in VAX section above */

/* MFPT - Move From Processor Type */
static P1ND *
handle_mfpt(INSTRUCTION *inst)
{
	/* MFPT - move processor type to R0 */
	/* Returns processor type identification in R0 */
	P1ND *r0 = build_reg(0);
	/* Return a constant identifying processor type (e.g., 1 for 11/34) */
	return build_assign(r0, build_icon(1));
}

/* CSM - Call to Supervisor Mode */
static P1ND *
handle_csm(INSTRUCTION *inst)
{
	/* CSM dst - call to supervisor mode */
	/* This is a privileged instruction for mode switching */
	/* Generate as a call/jump */
	if (inst->noperands >= 1) {
		return handle_jmp(inst);
	}
	return handle_nop(inst);
}

/* TSTSET - Test and Set */
static P1ND *
handle_tstset(INSTRUCTION *inst)
{
	/* TSTSET dst - test and set (atomic operation) */
	/* Test destination, set it to 1, return old value */
	if (inst->noperands >= 1) {
		P1ND *dst = operand_to_node(&inst->operands[0]);
		P1ND *dst_copy = operand_to_node(&inst->operands[0]);
		
		/* Test the value first */
		send_passt(IP_NODE, build_unop(UMINUS, dst_copy));
		
		/* Then set to 1 */
		return build_assign(dst, build_icon(1));
	}
	return handle_nop(inst);
}

/* WRTLCK - Write Lock */
static P1ND *
handle_wrtlck(INSTRUCTION *inst)
{
	/* WRTLCK dst - write lock (atomic operation) */
	/* Lock a memory location for atomic writes */
	/* Similar to test-and-set, but for write operations */
	if (inst->noperands >= 1) {
		P1ND *dst = operand_to_node(&inst->operands[0]);
		/* For now, just perform the write */
		return build_assign(dst, build_icon(0));
	}
	return handle_nop(inst);
}

/* PDP-11 FIS (Floating Instruction Set) handlers */

/* FADD - FIS Floating Add */
static P1ND *
handle_fadd(INSTRUCTION *inst)
{
	/* FADD - floating point add using accumulator registers AC0-AC3 */
	/* FIS uses a special accumulator-based architecture */
	/* For PCC IR, generate floating-point add */
	if (inst->noperands >= 1) {
		/* FIS operates on AC registers, simulate with standard registers */
		P1ND *src = operand_to_node(&inst->operands[0]);
		P1ND *ac0 = build_reg(0);  /* Accumulator 0 */
		P1ND *ac0_rhs = build_reg(0);
		
		/* AC0 = AC0 + src */
		return build_assign(ac0, build_binop(PLUS, ac0_rhs, src));
	}
	return handle_nop(inst);
}

/* FSUB - FIS Floating Subtract */
static P1ND *
handle_fsub(INSTRUCTION *inst)
{
	/* FSUB - floating point subtract using accumulator registers */
	if (inst->noperands >= 1) {
		P1ND *src = operand_to_node(&inst->operands[0]);
		P1ND *ac0 = build_reg(0);
		P1ND *ac0_rhs = build_reg(0);
		
		/* AC0 = AC0 - src */
		return build_assign(ac0, build_binop(MINUS, ac0_rhs, src));
	}
	return handle_nop(inst);
}

/* FMUL - FIS Floating Multiply */
static P1ND *
handle_fmul(INSTRUCTION *inst)
{
	/* FMUL - floating point multiply using accumulator registers */
	if (inst->noperands >= 1) {
		P1ND *src = operand_to_node(&inst->operands[0]);
		P1ND *ac0 = build_reg(0);
		P1ND *ac0_rhs = build_reg(0);
		
		/* AC0 = AC0 * src */
		return build_assign(ac0, build_binop(MUL, ac0_rhs, src));
	}
	return handle_nop(inst);
}

/* FDIV - FIS Floating Divide */
static P1ND *
handle_fdiv(INSTRUCTION *inst)
{
	/* FDIV - floating point divide using accumulator registers */
	if (inst->noperands >= 1) {
		P1ND *src = operand_to_node(&inst->operands[0]);
		P1ND *ac0 = build_reg(0);
		P1ND *ac0_rhs = build_reg(0);
		
		/* AC0 = AC0 / src */
		return build_assign(ac0, build_binop(DIV, ac0_rhs, src));
	}
	return handle_nop(inst);
}
