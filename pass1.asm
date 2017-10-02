	keep	obj/pass1
	mcopy pass1.mac
****************************************************************
*
*  Pass 1
*
*  This module contains the subroutines used to do pass 1
*  processing of the input files.
*
****************************************************************
	copy	directPage
****************************************************************
*
*  Align - align the code to a byte boundary
*
*  Inputs:
*	sp - pointer to the opcode
*	pc - current program counter
*
*  Outputs:
*	sp - pointer to the next opcode
*	pc - new program counter
*
****************************************************************
*
Align	private

	ldy	#1	get the alignment factor
	lda	[sp],Y
	sta	r0
	iny
	iny
	lda	[sp],Y
	sta	r2
	add4	sp,#5
	jsr	PrepareAlign	do pass 1 prep for the align
	rts
	end

****************************************************************
*
*  Const - constant bytes
*
*  Inputs:
*	sp - pointer to the opcode
*	pc - current program counter
*
*  Outputs:
*	sp - pointer to the next opcode
*	pc - new program counter
*
****************************************************************
*
Const	private

	lda	[sp]
	and	#$00FF
	tax
	clc
	adc	pc
	sta	pc
	bcc	lb1
	inc	pc+2
lb1	txa
	sec
	adc	sp
	sta	sp
	bcc	lb2
	inc	sp+2
lb2	rts
	end

****************************************************************
*
*  DefineSegment - put the segment in the symbol table
*
*  Inputs:
*	segName - pointer to the segment name
*	segType - segment type
*	pc - current pc
*	segEntry - disp to segment entry point
*
****************************************************************
*
DefineSegment private
	using Common

	ph4	segName	push the symbol name ptr
	ph2	#0	length attribute is 0
	ph2	#'N'	push the type attribute
	lda	segType	push the private flag
	and	#$4000
	beq	lb1
	lda	#1
lb1	pha
	ph2	#1	the symbol is global
	ph2	#0	the symbol is not an expression
	clc		push the location
	lda	pc
	adc	segEntry
	tax
	lda	pc+2
	adc	segEntry+2
	pha
	phx
	lda	segType	push the data area flag
	and	#$007F
	cmp	#1
	beq	lb2
	ph2	#0
	bra	lb3
lb2	ph2	#1
lb3	ph2	#1	push the segment flag
	jsr	Define	define the symbol
	rts
	end

****************************************************************
*
*  DoOrg - set the program counter
*
*  Inputs:
*	sp - pointer to the opcode
*	pc - current program counter
*
*  Outputs:
*	sp - pointer to the next opcode
*	pc - new program counter
*
****************************************************************
*
DoOrg	private
	using OutCommon

	ldy	#1	get the value and skip the record
	lda	[sp],Y
	sta	r0
	ldy	#3
	lda	[sp],Y
	sta	r2+2
	add4	sp,#5

	sub4	r0,loadOrg	get the disp from the segment start
	cmpl	pc,r0
	bge	lb1	if the disp is greater than the pc then
	move4 r0,pc	  update the pc
lb1	anop
	rts
	end

****************************************************************
*
*  DoPass1 - Do pass 1 processing
*
*  Outputs:
*	C - set if an error occurred
*
****************************************************************
*
DoPass1	start
	using Common
;
;  Write the pass header
;
	lda	#1	pass = 1
	sta	pass
	lda	list	if list then
	beq	wp1
	puts	#'Segment:',cr=t	  print the general header
	putcr
	bra	wp2	else if progress then
wp1	lda	progress
	beq	wp2
	puts	#'Pass 1: '	  print the dot header
wp2	anop
;
;  Initialize pass dependent variables
;
	jsr	InitPass
;
;  Process segments until there are no more
;
ps1	jsr	NextSegment	get the next segment
	bcc	rt1	branch if there are no more
	jsr	DefineSegment	put the segment in the symbol table
	jsr	ListSeg	list the segname start info
	jsr	DoSegment	process this segment
	add4	pc,segSpace	add in the reserved space (if any)
	bra	ps1	next segment
;
;  Return to main
;
rt1	lda	list	if list or progress then
	bne	rt2
	lda	progress
	beq	rt3
rt2	putcr		  write a cr
rt3	anop		endif
	clc
	rts
	end

****************************************************************
*
*  DoSegment - process the opcodes in this segment
*
*  Inputs:
*	sp - pointer to the first opcode to process
*
****************************************************************
*
DoSegment private

lb1	lda	[sp]
	and	#$00FF
	asl	A
	tax
	jsr	(addr,X)
	bra	lb1

addr	dc	a'End'	$00		End
	dc	15a'Const'	$01..$0F 	Const
	dc	16a'Const'	$10..$1F 	Const
	dc	16a'Const'	$20..$2F 	Const
	dc	16a'Const'	$30..$3F 	Const
	dc	16a'Const'	$40..$4F 	Const
	dc	16a'Const'	$50..$5F 	Const
	dc	16a'Const'	$60..$6F 	Const
	dc	16a'Const'	$70..$7F 	Const
	dc	16a'Const'	$80..$8F 	Const
	dc	16a'Const'	$90..$9F 	Const
	dc	16a'Const'	$A0..$AF 	Const
	dc	16a'Const'	$B0..$BF 	Const
	dc	16a'Const'	$C0..$CF 	Const
	dc	16a'Const'	$D0..$DF 	Const
	dc	a'Align'	$E0		Align
	dc	a'DoOrg'	$E1		Org
	dc	a'Invalid'	$E2		Reloc
	dc	a'Invalid'	$E3		Interseg
	dc	a'Strong'	$E4		Using
	dc	a'Strong'	$E5		Strong
	dc	a'Global'	$E6		Global
	dc	a'Gequ'	$E7		Gequ
	dc	a'Invalid'	$E8		Mem
	dc	a'Invalid'	$E9		unused
	dc	a'Invalid'	$EA		unused
	dc	a'Expr'	$EB		Expr
	dc	a'Expr'	$EC		ZExpr
	dc	a'Expr'	$ED		BExpr
	dc	a'RelExpr'	$EE		RelExpr
	dc	a'Local'	$EF		Local
	dc	a'Equ'	$F0		Equ
	dc	a'DS'	$F1		DS
	dc	a'Lconst'	$F2		LConst
	dc	a'Expr'	$F3		LExpr
	dc	a'Invalid'	$F4		Entry
	dc	a'Invalid'	$F5		cReloc
	dc	a'Invalid'	$F6		cInterseg
	dc	a'Invalid'	$F7		Super
	dc	a'Invalid'	$F8		unused
	dc	a'Invalid'	$F9		unused
	dc	a'Invalid'	$FA		unused
	dc	a'Invalid'	$FB		unused
	dc	a'Invalid'	$FC		unused
	dc	a'Invalid'	$FD		unused
	dc	a'Invalid'	$FE		unused
	dc	a'Invalid'	$FF		unused
	end

****************************************************************
*
*  DS - insert zeros at the PC
*
*  Inputs:
*	sp - pointer to the opcode
*	pc - current program counter
*
*  Outputs:
*	sp - pointer to the next opcode
*	pc - new program counter
*
****************************************************************
*
DS	private

	inc4	sp
	ldy	#2
	clc
	lda	[sp]
	adc	pc
	sta	pc
	lda	[sp],Y
	adc	pc+2
	sta	pc+2
	add4	sp,#4
	rts
	end

****************************************************************
*
*  End - end of the segment
*
****************************************************************
*
End	private

	pla
	rts
	end

****************************************************************
*
*  EndExp - end of the expression
*
*  Inputs:
*	sp - pointer to the opcode
*
*  Outputs:
*	sp - pointer to the next opcode
*
****************************************************************
*
EndExp	private

	inc4	sp
	pla
	rts
	end

****************************************************************
*
*  Equ - define a local equate
*
*  Inputs:
*	sp - pointer to the opcode
*
*  Outputs:
*	sp - pointer to the next opcode
*
****************************************************************
*
Equ	private
	using Common

	inc4	sp	skip the op code
	ph4	sp	push the symbol name ptr
	lda	[sp]	skip the symbol name
	and	#$00FF
	sec
	adc	sp
	sta	sp
	bcc	lb1
	inc	sp+2
lb1	lda	segVersion	if the segment is version 0 or 1 then
	cmp	#2
	beq	lb2
	lda	[sp]	  push the length byte
	and	#$00FF
	pha
	inc4	sp	  ++sp
	bra	lb3	else
lb2	lda	[sp]	  push the length word
	pha
	add4	sp,#2	  sp += 2
lb3	anop		endif
	lda	[sp]	push the type attribute
	and	#$00FF
	pha
	lda	[sp]	push the private flag
	and	#$FF00
	xba
	pha
	add4	sp,#2	sp += 2 {skip the attributes}
	ph2	#0	the symbol is local
	ph2	#1	the symbol is an expression
	ph4	sp	push the address of the expression
	ph2	#0	push the data area flag
	ph2	#0	push the segment flag
	jsr	Define	define the symbol
	jsr	SkipExpression	skip the expression
	rts
	end

****************************************************************
*
*  Expr - evaluate an expression
*
*  Inputs:
*	sp - pointer to the opcode
*	pc - current program counter
*
*  Outputs:
*	sp - pointer to the next opcode
*	pc - new program counter
*
****************************************************************
*
Expr	private

	lda	[sp]	update the PC
	xba
	and	#$00FF
	clc
	adc	pc
	sta	pc
	bcc	lb1
	inc	pc+2
lb1	add4	sp,#2	skip the op code and expression length
	brl	SkipExpression	skip the expression
	end

****************************************************************
*
*  Gequ - define a global equate
*
*  Inputs:
*	sp - pointer to the opcode
*
*  Outputs:
*	sp - pointer to the next opcode
*
****************************************************************
*
Gequ	private
	using Common

	inc4	sp	skip the op code
	ph4	sp	push the symbol name ptr
	lda	[sp]	skip the symbol name
	and	#$00FF
	sec
	adc	sp
	sta	sp
	bcc	lb1
	inc	sp+2
lb1	lda	segVersion	if the segment is version 0 ro 1 then
	cmp	#2
	beq	lb2
	lda	[sp]	  push the length byte
	and	#$00FF
	pha
	inc4	sp	  ++sp
	bra	lb3	else
lb2	lda	[sp]	  push the length word
	pha
	add4	sp,#2	  sp += 2
lb3	anop		endif
	lda	[sp]	push the type attribute
	and	#$00FF
	pha
	lda	[sp]	push the private flag
	and	#$FF00
	xba
	pha
	add4	sp,#2	sp += 2 {skip the attributes}
	ph2	#1	the symbol is global
	ph2	#1	the symbol is an expression
	ph4	sp	push the address of the expression
	ph2	#0	push the data area flag
	ph2	#0	push the segment flag
	jsr	Define	define the symbol
	jsr	SkipExpression	skip the expression
	rts
	end

****************************************************************
*
*  Global - define a global label at the PC
*
*  Inputs:
*	sp - pointer to the opcode
*
*  Outputs:
*	sp - pointer to the next opcode
*
****************************************************************
*
Global	private
	using Common

	inc4	sp	skip the op code
	ph4	sp	push the symbol name ptr
	lda	[sp]	skip the symbol name
	and	#$00FF
	sec
	adc	sp
	sta	sp
	bcc	lb1
	inc	sp+2
lb1	lda	segVersion	if the segment is version 0 ro 1 then
	cmp	#2
	beq	lb2
	lda	[sp]	  push the length byte
	and	#$00FF
	pha
	inc4	sp	  ++sp
	bra	lb3	else
lb2	lda	[sp]	  push the length word
	pha
	add4	sp,#2	  sp += 2
lb3	anop		endif
	lda	[sp]	push the type attribute
	and	#$00FF
	pha
	lda	[sp]	push the private flag
	and	#$FF00
	xba
	pha
	add4	sp,#2	sp += 2 {skip the attributes}
	ph2	#1	the symbol is global
	ph2	#0	the symbol is not an expression
	ph4	pc	push the current pc
	ph2	#0	push the data area flag
	ph2	#0	push the segment flag
	jsr	Define	define the symbol
	rts
	end

****************************************************************
*
*  Invalid - invalid op code
*
*  Notes:
*	An invalid opcode stops the link process with a
*	terminal error.
*
****************************************************************
*
Invalid	private

	lda	#8
	jmp	TermError
	end

****************************************************************
*
*  LConst - long constant bytes
*
*  Inputs:
*	sp - pointer to the opcode
*	pc - current program counter
*
*  Outputs:
*	sp - pointer to the next opcode
*	pc - new program counter
*
****************************************************************
*
LConst	private

	inc4	sp
	ldy	#2
	lda	[sp]
	sta	r0
	lda	[sp],Y
	sta	r0+2
	add4	sp,r0
	add4	sp,#4
	add4	pc,r0
	rts
	end

****************************************************************
*
*  ListSeg - list the segment start info
*
*  Inputs:
*	list - list info flag
*
****************************************************************
*
ListSeg	private
	using Common

	lda	list
	bne	lb1
	lda	progress
	beq	lb1
	putc	#'.'
lb1	jsr	CheckForPause
	rts
	end

****************************************************************
*
*  Local - define a local label at the PC
*
*  Inputs:
*	sp - pointer to the opcode
*
*  Outputs:
*	sp - pointer to the next opcode
*
****************************************************************
*
Local	private
	using Common

	inc4	sp	skip the op code
	ph4	sp	push the symbol name ptr
	lda	[sp]	skip the symbol name
	and	#$00FF
	sec
	adc	sp
	sta	sp
	bcc	lb1
	inc	sp+2
lb1	lda	segVersion	if the segment is version 0 ro 1 then
	cmp	#2
	beq	lb2
	lda	[sp]	  push the length byte
	and	#$00FF
	pha
	inc4	sp	  ++sp
	bra	lb3	else
lb2	lda	[sp]	  push the length word
	pha
	add4	sp,#2	  sp += 2
lb3	anop		endif
	lda	[sp]	push the type attribute
	and	#$00FF
	pha
	lda	[sp]	push the private flag
	and	#$FF00
	xba
	pha
	add4	sp,#2	sp += 2 {skip the attributes}
	ph2	#0	the symbol is local
	ph2	#0	the symbol is not an expression
	ph4	pc	push the current pc
	ph2	#0	push the data area flag
	ph2	#0	push the segment flag
	jsr	Define	define the symbol
	rts
	end

****************************************************************
*
*  Operation - handle an operation in an expression
*
*  Inputs:
*	sp - pointer to the operation
*
*  Outputs:
*	sp - pointer to the next expression term
*
****************************************************************
*
Operation private

	inc4	sp
	rts
	end

****************************************************************
*
*  PrepareAlign - do pass 1 prep for an align on pass 2
*
*  Inputs:
*	r0 - alignmanr factor
*	pc - program counter
*
*  Outputs:
*	pc - program counter
*
****************************************************************
*
PrepareAlign start

	dec4	r0	align the PC

lb1	lda	r0	quit if we are aligned
	and	pc
	bne	lb2
	lda	r2
	and	pc+2
	beq	lb5
lb2	lda	r0	form the remaining bit mask
	and	pc
	sta	r4
	lda	r2
	and	pc+2
	sta	r6
	lda	#1	find the least significant bit
	sta	r8
	stz	r10
lb3	lda	r8
	and	r4
	bne	lb4
	lda	r10
	and	r6
	bne	lb4
	asl	r8
	rol	r10
	bra	lb3
lb4	add4	pc,r8	add this to the pc
	bra	lb1	check the next bit

lb5	rts
	end

****************************************************************
*
*  RelExpr - evaluate a relative expression
*
*  Inputs:
*	sp - pointer to the opcode
*	pc - current program counter
*
*  Outputs:
*	sp - pointer to the next opcode
*	pc - new program counter
*
****************************************************************
*
RelExpr	private

	lda	[sp]	update the PC
	xba
	and	#$00FF
	clc
	adc	pc
	sta	pc
	bcc	lb1
	inc	pc+2
lb1	add4	sp,#6	skip the op code, length and offset
	brl	SkipExpression	skip the expression
	end

****************************************************************
*
*  SkipExpression - skip an expression, noting label uses
*
*  Inputs:
*	sp - pointer to the first opcode in the expression
*
*  Outputs:
*	sp - pointer to the first opcode past the expression
*
****************************************************************
*
SkipExpression private

lb1	lda	[sp]
	and	#$00FF
	asl	A
	tax
	jsr	(addr,X)
	bra	lb1

addr	dc	a'EndExp'	$00		End
	dc	15a'Operation'	$01..$0F 	some form of operation
	dc	6a'Operation'	$10..$15 	some form of operation
	dc	10a'Invalid'	$16..$1F 	unused
	dc	16a'Invalid'	$20..$2F 	unused
	dc	16a'Invalid'	$30..$3F 	unused
	dc	16a'Invalid'	$40..$4F 	unused
	dc	16a'Invalid'	$50..$5F 	unused
	dc	16a'Invalid'	$60..$6F 	unused
	dc	16a'Invalid'	$70..$7F 	unused
	dc	a'Operation'	$80		program counter
	dc	a'Value'	$81		absolute value
	dc	a'WeakReference'	$82		weak label reference
	dc	a'StrongReference'	$83		strong label reference
	dc	a'StrongReference'	$84		length attribute
	dc	a'StrongReference'	$85		type attribute
	dc	a'WeakReference'	$86		count attribute
	dc	a'Value'	$87		disp from start of segment
	dc	8a'Invalid'	$88-8F		unused
	dc	16a'Invalid'	$90..$9F 	unused
	dc	16a'Invalid'	$A0..$AF 	unused
	dc	16a'Invalid'	$B0..$BF 	unused
	dc	16a'Invalid'	$C0..$CF 	unused
	dc	16a'Invalid'	$D0..$DF 	unused
	dc	16a'Invalid'	$E0..$EF 	unused
	dc	16a'Invalid'	$F0..$FF 	unused
	end

****************************************************************
*
*  Strong - Note that we are using a data area
*
*  Inputs:
*	sp - pointer to the opcode
*
*  Outputs:
*	sp - pointer to the next opcode
*
****************************************************************
*
Strong	private

	inc4	sp	skip the op code
	jsr	Reference	make a reference to the name
	lda	[sp]	skip the name in the obj segment
	and	#$00FF
	sec
	adc	sp
	sta	sp
	bcc	lb1
	inc	sp+2
lb1	rts
	end

****************************************************************
*
*  StrongReference - handle a strong label reference in an expression
*
*  Inputs:
*	sp - pointer to the label name
*
*  Outputs:
*	sp - pointer to the next expression term
*
****************************************************************
*
StrongReference private

	inc4	sp	skip the op code
	jsr	Reference	make a reference to the name
	lda	[sp]	skip the name in the segment
	and	#$00FF
	sec
	adc	sp
	sta	sp
	bcc	lb1
	inc	sp+2
lb1	rts
	end

****************************************************************
*
*  Value - handle a value in an expression
*
*  Inputs:
*	sp - pointer to the value
*
*  Outputs:
*	sp - pointer to the next expression term
*
****************************************************************
*
Value	private

	add4	sp,#5
	rts
	end

****************************************************************
*
*  WeakReference - handle a weak label reference in an expression
*
*  Inputs:
*	sp - pointer to the label name
*
*  Outputs:
*	sp - pointer to the next expression term
*
****************************************************************
*
WeakReference private

	lda	[sp]
	and	#$FF00
	xba
	inc	A
	sec
	adc	sp
	sta	sp
	bcc	lb1
	inc	sp+2
lb1	rts
	end
