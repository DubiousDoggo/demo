;
; File generated by cc65 v 2.19 - Git 16258d812
;
	.fopt		compiler,"cc65 v 2.19 - Git 16258d812"
	.setcpu		"6502"
	.smart		on
	.autoimport	on
	.case		on
	.debuginfo	off
	.importzp	sp, sreg, regsave, regbank
	.importzp	tmp1, tmp2, tmp3, tmp4, ptr1, ptr2, ptr3, ptr4
	.macpack	longbranch
	.forceimport	__STARTUP__
	.forceimport	initmainargs
	.import		_puts
	.export		_main

.segment	"RODATA"

S0001:
	.byte	$48,$45,$4C,$4C,$4F,$2C,$20,$57,$4F,$52,$4C,$44,$00

; ---------------------------------------------------------------
; int __near__ __cdecl__ main (int argc, char **argv)
; ---------------------------------------------------------------

.segment	"CODE"

.proc	_main: near

.segment	"CODE"

	lda     #<(S0001)
	ldx     #>(S0001)
	jsr     _puts
	ldx     #$00
	txa
	rts

.endproc

