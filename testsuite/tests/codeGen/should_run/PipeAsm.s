.section	__TEXT,__cstring,cstring_literals
	.balign 8
_Lr1aD_bytes:
	.string "Main"
.section	__TEXT,__cstring,cstring_literals
	.balign 8
_Lr1aB_bytes:
	.string "main"
.data
	.balign 8
_Lr1aC_closure:
	.quad	_ghczminternal_GHCziInternalziTypes_TrNameS_con_info
	.quad	_Lr1aB_bytes
.data
	.balign 8
_Lr1aE_closure:
	.quad	_ghczminternal_GHCziInternalziTypes_TrNameS_con_info
	.quad	_Lr1aD_bytes
.data
	.balign 8
	.globl _Main_zdtrModule_closure
_Main_zdtrModule_closure:
	.quad	_ghczminternal_GHCziInternalziTypes_Module_con_info
	.quad	_Lr1aC_closure+1
	.quad	_Lr1aE_closure+1
	.quad	3
.data
	.balign 8
_Lu0_srt:
	.quad	_stg_SRT_1_info
	.quad	_ghczminternal_GHCziInternalziEnum_zdfEnumInt_closure
	.quad	0
.data
	.balign 8
_Lu1_srt:
	.quad	_stg_SRT_3_info
	.quad	_ghczminternal_GHCziInternalziDataziFoldable_zdfFoldableList_closure
	.quad	_ghczminternal_GHCziInternalziNum_zdfNumInt_closure
	.quad	_Lu0_srt
	.quad	0
.data
	.balign 8
_Lu2_srt:
	.quad	_stg_SRT_2_info
	.quad	_ghczminternal_GHCziInternalziShow_zdfShowInt_closure
	.quad	_Lu1_srt
	.quad	0
.text
	.balign 8
	.long	0
	.long	0
	.long	15
	.long	_Lu0_srt-(_Lt1aH_info)+0
_Lt1aH_info:
Lc1b0:
	mov x17, x22
	sub x15, x20, #40
	cmp x15, x28
	b.lo Lc1b1
Lc1b2:
	adrp x15, _stg_upd_frame_info@page
	add x15, x15, _stg_upd_frame_info@pageoff
	str x15, [ x20, -16 ]
	str x17, [ x20, -8 ]
	adrp x23, _ghczminternal_GHCziInternalziEnum_zdfEnumInt_closure@page
	add x23, x23, _ghczminternal_GHCziInternalziEnum_zdfEnumInt_closure@pageoff
	adrp x17, _stg_ap_pp_info@page
	add x17, x17, _stg_ap_pp_info@pageoff
	str x17, [ x20, -40 ]
	adrp x17, _stg_INTLIKE_closure@page
	add x17, x17, _stg_INTLIKE_closure@pageoff
	add x17, x17, #273
	str x17, [ x20, -32 ]
	adrp x17, _stg_INTLIKE_closure@page
	add x17, x17, _stg_INTLIKE_closure@pageoff
	add x17, x17, #1857
	str x17, [ x20, -24 ]
	sub x20, x20, #40
	b _ghczminternal_GHCziInternalziEnum_enumFromTo_info
Lc1b1:
	mov x22, x17
	ldr x17, [ x19, -16 ]
	br x17
.text
	.balign 8
	.long	0
	.long	0
	.long	15
	.long	_Lu1_srt-(_Lt1aI_info)+0
_Lt1aI_info:
Lc1b3:
	mov x17, x22
	sub x15, x20, #40
	cmp x15, x28
	b.lo Lc1b4
Lc1b5:
	add x21, x21, #16
	ldr x15, [ x19, 856 ]
	cmp x21, x15
	b.hi Lc1b7
Lc1b6:
	adrp x15, _stg_upd_frame_info@page
	add x15, x15, _stg_upd_frame_info@pageoff
	str x15, [ x20, -16 ]
	str x17, [ x20, -8 ]
	adrp x17, _Lt1aH_info@page
	add x17, x17, _Lt1aH_info@pageoff
	str x17, [ x21, -8 ]
	sub x17, x21, #8
	adrp x23, _ghczminternal_GHCziInternalziDataziFoldable_zdfFoldableList_closure@page
	add x23, x23, _ghczminternal_GHCziInternalziDataziFoldable_zdfFoldableList_closure@pageoff
	adrp x15, _stg_ap_pp_info@page
	add x15, x15, _stg_ap_pp_info@pageoff
	str x15, [ x20, -40 ]
	adrp x15, _ghczminternal_GHCziInternalziNum_zdfNumInt_closure@page
	add x15, x15, _ghczminternal_GHCziInternalziNum_zdfNumInt_closure@pageoff
	str x15, [ x20, -32 ]
	str x17, [ x20, -24 ]
	sub x20, x20, #40
	b _ghczminternal_GHCziInternalziDataziFoldable_sum_info
Lc1b7:
	movz x15, #16
	str x15, [ x19, 904 ]
Lc1b4:
	mov x22, x17
	ldr x17, [ x19, -16 ]
	br x17
.text
	.balign 8
	.long	0
	.long	0
	.long	21
	.long	_Lu2_srt-(_Lt1aJ_info)+0
_Lt1aJ_info:
Lc1b8:
	mov x17, x22
	sub x15, x20, #32
	cmp x15, x28
	b.lo Lc1b9
Lc1ba:
	add x21, x21, #16
	ldr x15, [ x19, 856 ]
	cmp x21, x15
	b.hi Lc1bc
Lc1bb:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	mov x0, x19
	mov x1, x17
	str x17, [ sp, 32 ]
	bl _newCAF
	ldp x29, x30, [sp], #16
	cbz x0, Lc1aR
Lc1aQ:
	adrp x17, _stg_bh_upd_frame_info@page
	add x17, x17, _stg_bh_upd_frame_info@pageoff
	str x17, [ x20, -16 ]
	str x0, [ x20, -8 ]
	adrp x17, _Lt1aI_info@page
	add x17, x17, _Lt1aI_info@pageoff
	str x17, [ x21, -8 ]
	sub x17, x21, #8
	adrp x23, _ghczminternal_GHCziInternalziShow_zdfShowInt_closure@page
	add x23, x23, _ghczminternal_GHCziInternalziShow_zdfShowInt_closure@pageoff
	adrp x15, _stg_ap_p_info@page
	add x15, x15, _stg_ap_p_info@pageoff
	str x15, [ x20, -32 ]
	str x17, [ x20, -24 ]
	sub x20, x20, #32
	b _ghczminternal_GHCziInternalziShow_show_info
Lc1aR:
	ldr x17, [ sp, 16 ]
	ldr x17, [ x17 ]
	br x17
Lc1bc:
	movz x15, #16
	str x15, [ x19, 904 ]
Lc1b9:
	mov x22, x17
	ldr x17, [ x19, -16 ]
	br x17
.data
	.balign 8
_Lt1aJ_closure:
	.quad	_Lt1aJ_info
	.quad	0
	.quad	0
	.quad	0
.data
	.balign 8
_Lup_srt:
	.quad	_stg_SRT_2_info
	.quad	_Lt1aJ_closure
	.quad	_ghczminternal_GHCziInternalziSystemziIO_putStrLn_closure
	.quad	0
.text
	.balign 8
	.long	0
	.long	0
	.long	21
	.long	_Lup_srt-(_Main_main_info)+0
	.globl _Main_main_info
_Main_main_info:
Lc1bj:
	mov x17, x22
	sub x15, x20, #16
	cmp x15, x28
	b.lo Lc1bk
Lc1bl:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	mov x0, x19
	mov x1, x17
	str x17, [ sp, 32 ]
	bl _newCAF
	ldp x29, x30, [sp], #16
	cbz x0, Lc1bi
Lc1bh:
	adrp x17, _stg_bh_upd_frame_info@page
	add x17, x17, _stg_bh_upd_frame_info@pageoff
	str x17, [ x20, -16 ]
	str x0, [ x20, -8 ]
	adrp x23, _Lt1aJ_closure@page
	add x23, x23, _Lt1aJ_closure@pageoff
	adrp x22, _ghczminternal_GHCziInternalziSystemziIO_putStrLn_closure@page
	add x22, x22, _ghczminternal_GHCziInternalziSystemziIO_putStrLn_closure@pageoff
	sub x20, x20, #16
	b _stg_ap_p_fast
Lc1bi:
	ldr x17, [ sp, 16 ]
	ldr x17, [ x17 ]
	br x17
Lc1bk:
	mov x22, x17
	ldr x17, [ x19, -16 ]
	br x17
.data
	.balign 8
	.globl _Main_main_closure
_Main_main_closure:
	.quad	_Main_main_info
	.quad	0
	.quad	0
	.quad	0
.data
	.balign 8
_Luu_srt:
	.quad	_stg_SRT_2_info
	.quad	_ghczminternal_GHCziInternalziTopHandler_runMainIO_closure
	.quad	_Main_main_closure
	.quad	0
.text
	.balign 8
	.long	0
	.long	0
	.long	21
	.long	_Luu_srt-(_ZCMain_main_info)+0
	.globl _ZCMain_main_info
_ZCMain_main_info:
Lc1bs:
	mov x17, x22
	sub x15, x20, #16
	cmp x15, x28
	b.lo Lc1bt
Lc1bu:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	mov x0, x19
	mov x1, x17
	str x17, [ sp, 32 ]
	bl _newCAF
	ldp x29, x30, [sp], #16
	cbz x0, Lc1br
Lc1bq:
	adrp x17, _stg_bh_upd_frame_info@page
	add x17, x17, _stg_bh_upd_frame_info@pageoff
	str x17, [ x20, -16 ]
	str x0, [ x20, -8 ]
	adrp x23, _Main_main_closure@page
	add x23, x23, _Main_main_closure@pageoff
	adrp x22, _ghczminternal_GHCziInternalziTopHandler_runMainIO_closure@page
	add x22, x22, _ghczminternal_GHCziInternalziTopHandler_runMainIO_closure@pageoff
	sub x20, x20, #16
	b _stg_ap_p_fast
Lc1br:
	ldr x17, [ sp, 16 ]
	ldr x17, [ x17 ]
	br x17
Lc1bt:
	mov x22, x17
	ldr x17, [ x19, -16 ]
	br x17
.data
	.balign 8
	.globl _ZCMain_main_closure
_ZCMain_main_closure:
	.quad	_ZCMain_main_info
	.quad	0
	.quad	0
	.quad	0
.ident "GHC 9.15.20260213"
