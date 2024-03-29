	keep	obj/exp
	mcopy exp.mac
****************************************************************
*
*  Expression evaluation
*
*  This module handles evaluation of expressions during pass
*  2.
*
****************************************************************
	copy	directPage
****************************************************************
*
*  ExpCommon - global data for the expression module
*
****************************************************************
*
;
;  Constants
;
maxTerm	gequ	16	max # of STACKED terms in an expression
maxDepth gequ	8	max # of NESTED unresolved labels

ExpCommon data
;
;  External value returned by CopyExpression
;
copiedExpression ds 2	was the expression resolved to a constant?
shiftCount ds	4	shift count (# bits to shift)
shiftFlag ds	2	is the expression shifted?
shiftValue ds	4	expression value before shift
symbolCount ds 2	count attribute
symbolLength ds 2	length attribute
symbolRelocatable ds 2	symbol relocatable flag
symbolType ds 2 	type attribute
symbolValue ds 4	symbol value
symbolData ds	2	symbol data area number
symbolFlag ds	2	symbol flags
symbolFile ds	2	symbol file
expSegment ds	2	segment number for the expression
;
;  Current expression information
;
expValue ds	4	expression value
expLength ds	2	expression length
	end

****************************************************************
*
*  CopyExpression - resolve or copy an expression
*
*  Inputs:
*	ep - pointer to the first opcode in the expression
*
*  Outputs:
*	X-A constant value or ptr to a safe copy of the expression
*	copiedExpression -
*		1 -> the value returned is a copy of the expression
*		0 -> the value returned is a constant
*
****************************************************************
*
CopyExpression start
	using ExpCommon
val	equ	1	value of the expression
oep	equ	5	original copy of ep
length	equ	9	length of the expression, in bytes
done	equ	13	done processing flag
sp	equ	15	expression stack pointer
stack	equ	17	expression stack

	sub	(4:ep),16+maxTerm*4

	stz	copiedExpression	assume we can resolve to a constant
	move4 ep,oep	save a copy of the start of the expression
	stz	done	not done, yet
	stz	sp	nothing on the operand stack

lb1	lda	[ep]	loop over the expression, processing it
	and	#$00FF
	asl	A
	tax
	jsr	(addr,X)
	lda	done
	beq	lb1

	lda	#9	if sp <> 4 then
	ldx	sp
	cpx	#4
	jne	TermError	  flag an expression syntax error
	move4 stack,val	set the value
	lda	copiedExpression	if the expression is not constant then
	beq	lb4
	sub2	ep,oep,length	  get some memory from the symbol table
	ph2	length
	jsr	GetSymbolMemory
	sta	val
	stx	val+2
	sta	ep
	stx	ep+2
	lda	length	  X = # of words to copy
	lsr	A
	tax
	bcc	lb2	  if there are an odd # of bytes then
	short M
	lda	[oep]
	sta	[ep]
	long	M
	inc4	oep
	inc4	ep
	tax
	beq	lb4
lb2	ldy	#0
lb3	lda	[oep],Y
	sta	[ep],Y
	iny
	iny
	dex
	bne	lb3

lb4	ret	4:val	return the expression value
;
;  Add:
;
Add	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	clc		do the operation
	lda	stack-4,X
	adc	stack,X
	sta	stack-4,X
	lda	stack-2,X
	adc	stack+2,X
	sta	stack-2,X
	rts
;
;  And:
;
And	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-4,X	do the operation
	ora	stack-2,X
	beq	and1
	lda	stack,X
	ora	stack+2,X
	beq	and1

	lda	#1	result is true
	bra	and2

and1	lda	#0	result is false
and2	sta	stack-4,X
	lda	#0
	sta	stack-2,X
	rts
;
;  BAnd:
;
BAnd	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-4,X	do the operation
	and	stack,X
	sta	stack-4,X
	lda	stack-2,X
	and	stack+2,X
	sta	stack-2,X
	rts
;
;  BEor:
;
BEor	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-4,X	do the operation
	eor	stack,X
	sta	stack-4,X
	lda	stack-2,X
	eor	stack+2,X
	sta	stack-2,X
	rts
;
;  BNot:
;
BNot	anop

	inc4	ep	update ep
	jsr	Check1	make sure there is at least 1 operand
	lda	stack-4,X	do the operation
	eor	#$FFFF
	sta	stack-4,X
	lda	stack-2,X
	eor	#$FFFF
	sta	stack-2,X
	rts
;
;  BOr:
;
BOr	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-4,X	do the operation
	ora	stack,X
	sta	stack-4,X
	lda	stack-2,X
	ora	stack+2,X
	sta	stack-2,X
	rts
;
;  Check1 - Makes sure there is at least 1 operand.  Returns sp in X.
;
Check1	anop

	ldx	sp
	beq	check2a
	rts
;
;  Check2 - Makes sure there are at least 2 operands.  Removes 1, returns
;	   new sp in X.
;
Check2	anop

	lda	sp
	cmp	#8
	bge	check21
check2a	lda	#9
	jmp	TermError

check21	sec
	sbc	#4
	sta	sp
	tax
	rts
;
;  CheckStack - check for stack overflows
;
CheckStack anop

	lda	#9
	ldx	sp
	cpx	#maxTerm*4
	jeq	TermError
	rts
;
;  Div:
;
Div	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-2,X	do the operation
	pha
	lda	stack-4,X
	pha
	lda	stack+2,X
	pha
	lda	stack,X
	pha
	jsl	~Div4
	ldx	sp
	pla
	sta	stack-4,X
	pla
	sta	stack-2,X
	pla
	pla
	rts
;
;  Eor:
;
Eor	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-4,X	do the operation
	ora	stack-2,X
	bne	eor1
	lda	stack,X
	ora	stack+2,X
	bne	eor2
	bra	eor3

eor1	lda	stack,X
	ora	stack+2,X
	bne	eor3

eor2	lda	#1	result is true
	bra	eor4

eor3	lda	#0	result is false
eor4	sta	stack-4,X
	lda	#0
	sta	stack-2,X
	rts
;
;  EndExp - end of the expression
;
EndExp	anop

	inc4	ep
	inc	done
	rts
;
;  EQ:
;
EQ	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-4,X	do the operation
	cmp	stack,X
	bne	eq1
	lda	stack-2,X
	cmp	stack+2,X
	bne	eq1

	lda	#1	result is true
	bra	eq2

eq1	lda	#0	result is false
eq2	ldx	sp
	sta	stack-4,X
	lda	#0
	sta	stack-2,X
	rts
;
;  Invalid - illegal byte in the expression
;
Invalid	anop

	lda	#8
	jmp	TermError
;
;  LE:
;
LE	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-2,X	do the operation
	pha
	lda	stack-4,X
	pha
	lda	stack+2,X
	pha
	lda	stack,X
	pha
	jsl	SCmp4
	ble	le1

	lda	#0	result is false
	bra	le2

le1	lda	#1	result is true
le2	ldx	sp
	sta	stack-4,X
	lda	#0
	sta	stack-2,X
	rts
;
;  LT:
;
LT	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-2,X	do the operation
	pha
	lda	stack-4,X
	pha
	lda	stack+2,X
	pha
	lda	stack,X
	pha
	jsl	SCmp4
	blt	lt1

	lda	#0	result is false
	bra	lt2

lt1	lda	#1	result is true
lt2	ldx	sp
	sta	stack-4,X
	lda	#0
	sta	stack-2,X
	rts
;
;  GE:
;
GE	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-2,X	do the operation
	pha
	lda	stack-4,X
	pha
	lda	stack+2,X
	pha
	lda	stack,X
	pha
	jsl	SCmp4
	bge	ge1

	lda	#0	result is false
	bra	ge2

ge1	lda	#1	result is true
ge2	ldx	sp
	sta	stack-4,X
	lda	#0
	sta	stack-2,X
	rts
;
;  GT:
;
GT	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-2,X	do the operation
	pha
	lda	stack-4,X
	pha
	lda	stack+2,X
	pha
	lda	stack,X
	pha
	jsl	SCmp4
	bgt	gt1

	lda	#0	result is false
	bra	gt2

gt1	lda	#1	result is true
gt2	ldx	sp
	sta	stack-4,X
	lda	#0
	sta	stack-2,X
	rts
;
;  Mod:
;
Mod	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-2,X	do the operation
	pha
	lda	stack-4,X
	pha
	lda	stack+2,X
	pha
	lda	stack,X
	pha
	jsl	~Div4
	pla
	pla
	ldx	sp
	pla
	sta	stack-4,X
	pla
	sta	stack-2,X
	rts
;
;  Mul:
;
Mul	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-2,X	do the operation
	pha
	lda	stack-4,X
	pha
	lda	stack+2,X
	pha
	lda	stack,X
	pha
	jsl	~Mul4
	ldx	sp
	pla
	sta	stack-4,X
	pla
	sta	stack-2,X
	rts
;
;  NE:
;
NE	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-4,X	do the operation
	cmp	stack,X
	bne	ne1
	lda	stack-2,X
	cmp	stack+2,X
	bne	ne1

	lda	#0	result is false
	bra	ne2

ne1	lda	#1	result is true
ne2	ldx	sp
	sta	stack-4,X
	lda	#0
	sta	stack-2,X
	rts
;
;  Not:
;
Not	anop

	inc4	ep	update ep
	jsr	Check1	make sure there are at least 2 operands
	lda	stack-4,X	do the operation
	ora	stack-2,X
	bne	not1

	lda	#1	result is true
	bra	not2

not1	lda	#0	result is false
not2	sta	stack-4,X
	lda	#0
	sta	stack-2,X
	rts
;
;  Or:
;
Or	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-4,X	do the operation
	ora	stack-2,X
	bne	or1
	lda	stack,X
	ora	stack+2,X
	bne	or1

	lda	#0	result is false
	bra	or2

or1	lda	#1	result is true
or2	sta	stack-4,X
	lda	#0
	sta	stack-2,X
	rts
;
;  PCounter - program counter
;
PCounter anop

	lda	#1	copiedExpression = true
	sta	copiedExpression
	inc4	ep	skip the op code
	jsr	CheckStack	make sure there is room on the stack
	add2	sp,#4	reserve space on the operand stack
	rts
;
;  Reference - a reference to a label
;
Reference anop

	lda	#1	copiedExpression = true
	sta	copiedExpression
	jsr	CheckStack	make sure there is room on the stack
	add2	sp,#4	reserve space on the operand stack
	inc4	ep	skip the op code
	lda	[ep]	skip the name in the segment
	and	#$00FF
	sec
	adc	ep
	sta	ep
	bcc	rf1
	inc	ep+2
rf1	rts
;
;  SegDisp - disp from the start of the segment
;
SegDisp	anop

	lda	#1	copiedExpression = true
	sta	copiedExpression
	jsr	CheckStack	make sure there is room on the stack
	add4	ep,#5	skip the op code and operand
	add2	sp,#4	reserve space on the operand stack
	rts
;
;  Shift:
;
Shift	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands

	lda	stack+2,X	if shift is to the right then
	bpl	shift2
	lda	stack,X	  shift to the right
	tay
shift1	lsr	stack-2,X
	ror	stack-4,X
	iny
	bne	shift1
	rts		  return

shift2	lda	stack,X	shift to the left
	tay
	beq	shift4
shift3	asl	stack-4,X
	rol	stack-2,X
	dey
	bne	shift3
shift4	rts
;
;  Sub:
;
Sub	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	sec		do the operation
	lda	stack-4,X
	sbc	stack,X
	sta	stack-4,X
	lda	stack-2,X
	sbc	stack+2,X
	sta	stack-2,X
	rts
;
;  UMinus:
;
UMinus	anop

	inc4	ep	update ep
	jsr	Check1	make sure there is at least 1 operand
	sec		do the operation
	lda	#0
	sbc	stack-4,X
	sta	stack-4,X
	lda	#0
	sbc	stack-2,X
	sta	stack-2,X
	rts
;
;  Value - constant value
;
Value	anop

	jsr	CheckStack	make sure there is room on the stack
	ldy	#1	place the value on the stack
	ldx	sp
	lda	[ep],Y
	sta	stack,X
	ldy	#3
	lda	[ep],Y
	sta	stack+2,X
	add4	ep,#5	skip the op code and operand
	add2	sp,#4	reserve space on the operand stack
	rts
;
;  Table of expression handling subroutines
;
addr	dc	a'EndExp'	$00		End
	dc	a'Add'	$01		+
	dc	a'Sub'	$02		-
	dc	a'Mul'	$03		*
	dc	a'Div'	$04		div
	dc	a'Mod'	$05		mod
	dc	a'UMinus'	$06		unary -
	dc	a'Shift'	$07		<< or >>
	dc	a'And'	$08		and
	dc	a'Or'	$09		or
	dc	a'Eor'	$0A		eor
	dc	a'Not'	$0B		not
	dc	a'LE'	$0C		<=
	dc	a'GE'	$0D		>=
	dc	a'NE'	$0E		<>
	dc	a'LT'	$0F		<
	dc	a'GT'	$10		>
	dc	a'EQ'	$11		=
	dc	a'BAnd'	$12		&
	dc	a'BOr'	$13		|
	dc	a'BEor'	$14		bitwise eor
	dc	a'BNot'	$15		bitwise not
	dc	10a'Invalid'	$16..$1F 	unused
	dc	16a'Invalid'	$20..$2F 	unused
	dc	16a'Invalid'	$30..$3F 	unused
	dc	16a'Invalid'	$40..$4F 	unused
	dc	16a'Invalid'	$50..$5F 	unused
	dc	16a'Invalid'	$60..$6F 	unused
	dc	16a'Invalid'	$70..$7F 	unused
	dc	a'PCounter'	$80		program counter
	dc	a'Value'	$81		absolute value
	dc	a'Reference'	$82		weak label reference
	dc	a'Reference'	$83		strong label reference
	dc	a'Reference'	$84		length attribute
	dc	a'Reference'	$85		type attribute
	dc	a'Reference'	$86		count attribute
	dc	a'SegDisp'	$87		disp from start of segment
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
*  Evaluate - evaluate an expression
*
*  Inputs:
*	ep - pointer to the expression
*
*  Outputs:
*	shiftFlag - 1 if the value is shifted, else 0
*	shiftValue - expression result before shifting
*	shiftCount - shift counter
*	symbolRelocatable - non-zero if sym is relocatable
*	returns the value of the expression
*
****************************************************************
*
Evaluate start
	using Common
	using ExpCommon
done	equ	1	done processing flag
sp	equ	3	expression stack pointer
stack	equ	7	expression stack

	sub	(4:ep),6+maxTerm*5

	tsc		check for a stack overflow
	sec
	sbc	#$0100
	cmp	dpReg
	bge	lb0
	lda	#9
	jmp	TermError

lb0	stz	shiftFlag	no shift has occurred
	stz	shiftCount
	stz	shiftCount+2
	stz	done	not done, yet
	stz	sp	nothing on the operand stack
	stz	expSegment	no segment-dependent variables, yet

lb1	lda	[ep]	loop over the expression, processing it
	and	#$00FF
	asl	A
	tax
	jsr	(addr,X)
	lda	done
	beq	lb1

	lda	#9	if sp <> 4 then
	ldx	sp
	cpx	#5
	jne	TermError	  flag an expression syntax error
	lda	stack+4	set the relocatable flag
	and	#$00FF
	sta	symbolRelocatable

	ret	4:stack	return the expression value
;
;  Add:
;
Add	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	clc		do the operation
	lda	stack-5,X
	adc	stack,X
	sta	stack-5,X
	lda	stack-3,X
	adc	stack+2,X
	sta	stack-3,X

	lda	stack-1,X	if both operands are relative then
	and	stack+4,X
	and	#$00FF
	beq	ad1
	ph4	#0	  Error(NULL,16)
	ph2	#16
	jsr	Error
	rts

ad1	lda	stack-1,X	if either operand is relative and a
	ora	stack+4,X	  shift has occurred then
	and	#$00FF
	beq	ad3
	lda	shiftFlag
	beq	ad2
	ph4	#0	  Error(NULL,16)
	ph2	#16
	jsr	Error
	rts

ad2	lda	#1	one operand is relative, so the result
	sta	stack-1,X	 is also relative
ad3	rts
;
;  And:
;
And	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-5,X	do the operation
	ora	stack-3,X
	beq	and1
	lda	stack,X
	ora	stack+2,X
	beq	and1

	lda	#1	result is true
	bra	and2

and1	lda	#0	result is false
and2	sta	stack-5,X
	lda	#0
	sta	stack-3,X
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  AttrCount - get the count attribute of a label
;
AttrCount anop

	inc4	ep	skip the op code
	ph4	ep	find the symbol value
	ph2	#0
	jsr	GetSymbolValue
	jsr	CheckStack	make sure there is room on the stack
	lda	symbolCount	save the value
	sta	stack,X
	lda	#0
	sta	stack+2,X
	short M	set the relocation flag
	lda	#0
	sta	stack+4,X
	long	M
	add2	sp,#5	reserve space on the operand stack
	lda	[ep]	skip the name in the segment
	and	#$00FF
	sec
	adc	ep
	sta	ep
	bcc	ac1
	inc	ep+2
ac1	rts
;
;  AttrLength - get the length attribute of a label
;
AttrLength anop

	inc4	ep	skip the op code
	ph4	ep	find the symbol value
	ph2	#1
	jsr	GetSymbolValue
	jsr	CheckStack	make sure there is room on the stack
	lda	symbolLength	save the value
	sta	stack,X
	lda	#0
	sta	stack+2,X
	short M	set the relocation flag
	lda	#0
	sta	stack+4,X
	long	M
	add2	sp,#5	reserve space on the operand stack
	lda	[ep]	skip the name in the segment
	and	#$00FF
	sec
	adc	ep
	sta	ep
	bcc	al1
	inc	ep+2
al1	rts
;
;  AttrType - get the type attribute of a label
;
AttrType anop

	inc4	ep	skip the op code
	ph4	ep	find the symbol value
	ph2	#1
	jsr	GetSymbolValue
	jsr	CheckStack	make sure there is room on the stack
	lda	symbolType	save the value
	sta	stack,X
	lda	#0
	sta	stack+2,X
	short M	set the relocation flag
	lda	#0
	sta	stack+4,X
	long	M
	add2	sp,#5	reserve space on the operand stack
	lda	[ep]	skip the name in the segment
	and	#$00FF
	sec
	adc	ep
	sta	ep
	bcc	at1
	inc	ep+2
at1	rts
;
;  BAnd:
;
BAnd	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-5,X	do the operation
	and	stack,X
	sta	stack-5,X
	lda	stack-3,X
	and	stack+2,X
	sta	stack-3,X
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  BEor:
;
BEor	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-5,X	do the operation
	eor	stack,X
	sta	stack-5,X
	lda	stack-3,X
	eor	stack+2,X
	sta	stack-3,X
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  BNot:
;
BNot	anop

	inc4	ep	update ep
	jsr	Check1	make sure there is at least 1 operand
	lda	stack-5,X	do the operation
	eor	#$FFFF
	sta	stack-5,X
	lda	stack-3,X
	eor	#$FFFF
	sta	stack-3,X
	jsr	NoRelocate1	make sure the operand is not relocatable
	rts
;
;  BOr:
;
BOr	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-5,X	do the operation
	ora	stack,X
	sta	stack-5,X
	lda	stack-3,X
	ora	stack+2,X
	sta	stack-3,X
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  Check1 - Makes sure there is at least 1 operand.  Returns sp in X.
;
Check1	anop

	ldx	sp
	beq	check2a
	rts
;
;  Check2 - Makes sure there are at least 2 operands.  Removes 1, returns
;	   new sp in X.
;
Check2	anop

	lda	sp
	cmp	#10
	bge	check21
check2a	lda	#9
	jmp	TermError

check21	sec
	sbc	#5
	sta	sp
	tax
	rts
;
;  CheckStack - check for stack overflows
;
CheckStack anop

	lda	#9
	ldx	sp
	cpx	#maxTerm*5
	jeq	TermError
	rts
;
;  Div:
;
Div	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-3,X	do the operation
	pha
	lda	stack-5,X
	pha
	lda	stack+2,X
	pha
	lda	stack,X
	pha
	jsl	~Div4
	ldx	sp
	pla
	sta	stack-5,X
	pla
	sta	stack-3,X
	pla
	pla
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  Eor:
;
Eor	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-5,X	do the operation
	ora	stack-3,X
	bne	eor1
	lda	stack,X
	ora	stack+2,X
	bne	eor2
	bra	eor3

eor1	lda	stack,X
	ora	stack+2,X
	bne	eor3

eor2	lda	#1	result is true
	bra	eor4

eor3	lda	#0	result is false
eor4	sta	stack-5,X
	lda	#0
	sta	stack-3,X
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  EndExp - end of the expression
;
EndExp	anop

	inc4	ep
	inc	done
	rts
;
;  EQ:
;
EQ	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-5,X	do the operation
	cmp	stack,X
	bne	eq1
	lda	stack-3,X
	cmp	stack+2,X
	bne	eq1

	lda	#1	result is true
	bra	eq2

eq1	lda	#0	result is false
eq2	ldx	sp
	sta	stack-5,X
	lda	#0
	sta	stack-3,X
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  Invalid - illegal byte in the expression
;
Invalid	anop
	lda	#8
	jmp	TermError
;
;  LE:
;
LE	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-3,X	do the operation
	pha
	lda	stack-5,X
	pha
	lda	stack+2,X
	pha
	lda	stack,X
	pha
	jsl	SCmp4
	ble	le1

	lda	#0	result is false
	bra	le2

le1	lda	#1	result is true
le2	ldx	sp
	sta	stack-5,X
	lda	#0
	sta	stack-3,X
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  LT:
;
LT	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-3,X	do the operation
	pha
	lda	stack-5,X
	pha
	lda	stack+2,X
	pha
	lda	stack,X
	pha
	jsl	SCmp4
	blt	lt1

	lda	#0	result is false
	bra	lt2

lt1	lda	#1	result is true
lt2	ldx	sp
	sta	stack-5,X
	lda	#0
	sta	stack-3,X
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  GE:
;
GE	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-3,X	do the operation
	pha
	lda	stack-5,X
	pha
	lda	stack+2,X
	pha
	lda	stack,X
	pha
	jsl	SCmp4
	bge	ge1

	lda	#0	result is false
	bra	ge2

ge1	lda	#1	result is true
ge2	ldx	sp
	sta	stack-5,X
	lda	#0
	sta	stack-3,X
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  GT:
;
GT	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-3,X	do the operation
	pha
	lda	stack-5,X
	pha
	lda	stack+2,X
	pha
	lda	stack,X
	pha
	jsl	SCmp4
	bgt	gt1

	lda	#0	result is false
	bra	gt2

gt1	lda	#1	result is true
gt2	ldx	sp
	sta	stack-5,X
	lda	#0
	sta	stack-3,X
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  Mod:
;
Mod	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-3,X	do the operation
	pha
	lda	stack-5,X
	pha
	lda	stack+2,X
	pha
	lda	stack,X
	pha
	jsl	~Div4
	pla
	pla
	ldx	sp
	pla
	sta	stack-5,X
	pla
	sta	stack-3,X
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  Mul:
;
Mul	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-3,X	do the operation
	pha
	lda	stack-5,X
	pha
	lda	stack+2,X
	pha
	lda	stack,X
	pha
	jsl	~Mul4
	ldx	sp
	pla
	sta	stack-5,X
	pla
	sta	stack-3,X
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  NE:
;
NE	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-5,X	do the operation
	cmp	stack,X
	bne	ne1
	lda	stack-3,X
	cmp	stack+2,X
	bne	ne1

	lda	#0	result is false
	bra	ne2

ne1	lda	#1	result is true
ne2	ldx	sp
	sta	stack-5,X
	lda	#0
	sta	stack-3,X
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  NoRelocate1 - make sure the top operand is not relocatable
;
NoRelocate1 anop

	lda	stack-1,X
	bra	nr0
;
;  NoRelocate2 - make sure the top two operands are not relocatable
;
NoRelocate2 anop

	lda	stack-1,X
	ora	stack+4,X
nr0	and	#$00FF
	beq	nr1
	ph4	#0
	ph2	#16
	jsr	Error
nr1	rts
;
;  Not:
;
Not	anop

	inc4	ep	update ep
	jsr	Check1	make sure there are at least 2 operands
	lda	stack-5,X	do the operation
	ora	stack-3,X
	bne	not1

	lda	#1	result is true
	bra	not2

not1	lda	#0	result is false
not2	sta	stack-5,X
	lda	#0
	sta	stack-3,X
	jsr	NoRelocate1	make sure the operand is not relocatable
	rts
;
;  Or:
;
Or	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	lda	stack-5,X	do the operation
	ora	stack-3,X
	bne	or1
	lda	stack,X
	ora	stack+2,X
	bne	or1

	lda	#0	result is false
	bra	or2

or1	lda	#1	result is true
or2	sta	stack-5,X
	lda	#0
	sta	stack-3,X
	jsr	NoRelocate2	make sure the operands are not relocatable
	rts
;
;  PCounter - program counter
;
PCounter anop

	inc4	ep	skip the op code
         ldx	dpReg	get the program counter
	lda	>pc+2,X
	pha
	lda	>pc,X
	pha
	jsr	CheckStack	make sure there is room on the stack
	pla		save the program counter
	sta	stack,X
	pla
	sta	stack+2,X
	short M	value is relocatable
	lda	#1
	sta	stack+4,X
	long	M
	add2	sp,#5	reserve space on the operand stack
	rts
;
;  Reference - a reference to a label
;
Reference anop

	inc4	ep	skip the op code
	ph4	ep	find the symbol value
	ph2	#1
	jsr	GetSymbolValue
	jsr	CheckStack	make sure there is room on the stack
	lda	symbolValue	save the value
	sta	stack,X
	lda	symbolValue+2
	sta	stack+2,X
	short M	set the relocation flag
	lda	symbolRelocatable
	sta	stack+4,X
	long	M
	add2	sp,#5	reserve space on the operand stack
	lda	[ep]	skip the name in the segment
	and	#$00FF
	sec
	adc	ep
	sta	ep
	bcc	rf1
	inc	ep+2
rf1	rts
;
;  SegDisp - disp from the start of the segment
;
SegDisp	anop

	jsr	CheckStack	make sure there is room on the stack
	clc		save the value + startpc
	lda	startpc
	ldy	#1
	adc	[ep],Y
	sta	stack,X
	lda	startpc+2
	iny
	iny
	adc	[ep],Y
	sta	stack+2,X
	short M	value is relocatable
	lda	#1
	sta	stack+4,X
	long	M
	add4	ep,#5	skip the op code and operand
	add2	sp,#5	reserve space on the operand stack
	rts
;
;  Shift:
;
Shift	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands

	lda	shiftFlag	a shift can only be done 1 time
	beq	shift0
	phx
	ph4	#0
	ph2	#2
	jsr	Error
	plx
	stz	shiftFlag
shift0	inc	shiftFlag

	lda	stack,X	save the shift count
	sta	shiftCount
	lda	stack+2,X
	sta	shiftCount+2
	bmi	sh2	restrict it to a reasonable range
	bne	sh1
	lda	shiftCount
	cmp	#32
	blt	sh4
sh1	lla	shiftCount,32
	bra	sh4
sh2	inc	A
	bne	sh3
	lda	shiftCount
	cmp	#-32
	bge	sh4
sh3	lla	shiftCount,-32

sh4	lda	stack-5,X	save the shifted value
	sta	shiftValue
	lda	stack-3,X
	sta	shiftValue+2

	lda	stack+2,X	if shift is to the right then
	bpl	shift2
	lda	stack,X	  shift to the right
	tay
shift1	lsr	stack-3,X
	ror	stack-5,X
	iny
	bne	shift1
	rts		  return

shift2	lda	stack,X	shift to the left
	tay
	beq	shift4
shift3	asl	stack-5,X
	rol	stack-3,X
	dey
	bne	shift3
shift4	rts
;
;  Sub:
;
Sub	anop

	inc4	ep	update ep
	jsr	Check2	make sure there are at least 2 operands
	sec		do the operation
	lda	stack-5,X
	sbc	stack,X
	sta	stack-5,X
	lda	stack-3,X
	sbc	stack+2,X
	sta	stack-3,X

	lda	stack-1,X	if both operands are relative then
	and	stack+4,X
	and	#$00FF
	beq	su1
	lda	#0	  result is a constant
	sta	stack-1,X
	rts

su1	lda	stack-1,X	if either operand is relative and a
	ora	stack+4,X	  shift has occurred then
	and	#$00FF
	beq	su3
	lda	shiftFlag
	beq	su2
	ph4	#0	  Error(NULL,16)
	ph2	#16
	jsr	Error
	rts

su2	lda	#1	one operand is relative, so the result
	sta	stack-1,X	 is also relative
su3	rts
;
;  UMinus:
;
UMinus	anop

	inc4	ep	update ep
	jsr	Check1	make sure there is at least 1 operand
	sec		do the operation
	lda	#0
	sbc	stack-5,X
	sta	stack-5,X
	lda	#0
	sbc	stack-3,X
	sta	stack-3,X
	jsr	NoRelocate1	make sure the operand is not relocatable
	rts
;
;  Value - constant value
;
Value	anop

	jsr	CheckStack	make sure there is room on the stack
	ldy	#1	place the value on the stack
	ldx	sp
	lda	[ep],Y
	sta	stack,X
	ldy	#3
	lda	[ep],Y
	sta	stack+2,X
	short M	value is not relocatable
	lda	#0
	sta	stack+4,X
	long	M
	add4	ep,#5	skip the op code and operand
	add2	sp,#5	reserve space on the operand stack
	rts
;
;  Weak - weak reference to a label
;
Weak	anop

	inc4	ep	skip the op code
	ph4	ep	find the symbol value
	ph2	#0
	jsr	GetSymbolValue
	jsr	CheckStack	make sure there is room on the stack
	lda	symbolValue	save the value
	sta	stack,X
	lda	symbolValue+2
	sta	stack+2,X
	short M	set the relocation flag
	lda	symbolRelocatable
	sta	stack+4,X
	long	M
	add2	sp,#5	reserve space on the operand stack
	lda	[ep]	skip the name in the segment
	and	#$00FF
	sec
	adc	ep
	sta	ep
	bcc	wk1
	inc	ep+2
wk1	rts
;
;  Table of expression handling subroutines
;
addr	dc	a'EndExp'	$00		End
	dc	a'Add'	$01		+
	dc	a'Sub'	$02		-
	dc	a'Mul'	$03		*
	dc	a'Div'	$04		div
	dc	a'Mod'	$05		mod
	dc	a'UMinus'	$06		unary -
	dc	a'Shift'	$07		<< or >>
	dc	a'And'	$08		and
	dc	a'Or'	$09		or
	dc	a'Eor'	$0A		eor
	dc	a'Not'	$0B		not
	dc	a'LE'	$0C		<=
	dc	a'GE'	$0D		>=
	dc	a'NE'	$0E		<>
	dc	a'LT'	$0F		<
	dc	a'GT'	$10		>
	dc	a'EQ'	$11		=
	dc	a'BAnd'	$12		&
	dc	a'BOr'	$13		|
	dc	a'BEor'	$14		bitwise eor
	dc	a'BNot'	$15		bitwise not
	dc	10a'Invalid'	$16..$1F 	unused
	dc	16a'Invalid'	$20..$2F 	unused
	dc	16a'Invalid'	$30..$3F 	unused
	dc	16a'Invalid'	$40..$4F 	unused
	dc	16a'Invalid'	$50..$5F 	unused
	dc	16a'Invalid'	$60..$6F 	unused
	dc	16a'Invalid'	$70..$7F 	unused
	dc	a'PCounter'	$80		program counter
	dc	a'Value'	$81		absolute value
	dc	a'Weak'	$82		weak label reference
	dc	a'Reference'	$83		strong label reference
	dc	a'AttrLength'	$84		length attribute
	dc	a'AttrType'	$85		type attribute
	dc	a'AttrCount'	$86		count attribute
	dc	a'SegDisp'	$87		disp from start of segment
	dc	8a'Invalid'	$88-8F		unused
	dc	16a'Invalid'	$90..$9F 	unused
	dc	16a'Invalid'	$A0..$AF 	unused
	dc	16a'Invalid'	$B0..$BF 	unused
	dc	16a'Invalid'	$C0..$CF 	unused
	dc	16a'Invalid'	$D0..$DF 	unused
	dc	16a'Invalid'	$E0..$EF 	unused
	dc	16a'Invalid'	$F0..$FF 	unused
	ret
	end

****************************************************************
*
*  SCmp4 - Four byte signed integer compare
*
*  Inputs:
*	7,S - first argument
*	3,S - second argument
*
*  Outputs:
*	C - set if 7,S >= 3,S, else clear
*	Z - set if 7,S = 3,S, else clear
*
****************************************************************
*
SCmp4	private

	lda	9,S	branch if both numbers have the same
	eor	5,S	 sign
	bpl	cp1
	lda	5,S	do a comparison of oppositely signed
	cmp	9,S	 numbers
	rts

cp1	lda	9,S	do a comparison of numbers with the
	cmp	5,S	 same sign
	bne	cp2
	lda	7,S
	cmp	3,S
cp2	rts
	end
