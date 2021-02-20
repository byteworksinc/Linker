	keep	obj/pass2
	mcopy pass2.mac
****************************************************************
*
*  Pass 2
*
*  This module contains the subroutines used to do pass 2
*  processing of the input files and creation of the output
*  files.
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
	add4	sp,#5	skip the alignment opcode and operand
	jsr	DefineAlign	do the align
	rts
	end

****************************************************************
*
*  BExpr - evaluate a local bank expression
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
BExpr	private
	using ExpCommon
	using OutCommon
	using Common

	stz	saveSegment	saveSegment := false
	lda	[sp]	update the PC
	xba
	and	#$00FF
	sta	expLength
lb1	add4	sp,#2	skip the op code and expression length
	ph4	sp	evaluate the expression
	jsr	Evaluate
	sta	expValue
	stx	expValue+2
	add4	loadOrg,pc,val1	make sure bank bytes match
	lda	symbolRelocatable
	beq	lb2
	add4	loadOrg,expValue,val2
	bra	lb2a
lb2	move4 expValue,val2
lb2a	stz	mask
	stz	mask+2
	short M
	ldx	expLength
	beq	lb4
	cpx	#4
	bge	lb4
	ldx	#3
	lda	#$FF
lb3	sta	mask,X
	dex
	cpx	expLength
	bge	lb3
lb4	long	M
	lda	mask
	and	val1
	sta	val1
	lda	mask+2
	and	val1+2
	sta	val1+2
	lda	mask
	and	val2
	cmp	val1
	bne	lb5
	lda	mask+2
	and	val2+2
	cmp	val1+2
	beq	lb6
lb5	ph4	#0
	ph2	#10
	jsr	Error
	bra	lb10
lb6	lda	expSegment	if the expression uses values in another
	beq	lb10	  segment then
	cmp	loadNumber
	beq	lb10
	lda	expLength	  if the expression is too short for
	cmp	#3	    a legal interseg reference then
	bge	lb7
	ph4	#0	    flag the error
	ph2	#10
	jsr	Error
	bra	lb10	    skip to "normal" processing
lb7	lda	symbolRelocatable	  if the expression is relocatable then
	beq	lb9
	jsr	DictInterseg	    create a dictionary entry
	sta	saveSegment	    save the save segment flag
	lda	shiftFlag	    if the value is shifted then
	beq	lb8
	move4 shiftValue,expValue	      use the unshifted value
lb8	anop
lb9	lda	expSegment	    if the segment is dynamic then
	jsr	IsDynamic
	bcc	lb11
	ph4	#0	      flag the error
	ph2	#17
	jsr	Error
	bra	lb11	  else
lb10	lda	symbolRelocatable	    if the expression is relocatable then
	beq	lb11
	jsr	DictReloc	      create a dictionary entry
lb11	anop		  endif

	jsr	PutValue	write an expression value
	lda	saveSegment	if saveSegment then
	beq	lb12
	sec		  save the segment number
	lda	op
	sbc	saveSegment
	sta	r0
	lda	op+2
	sbc	#0
	sta	r2
	short M
	clc
	lda	expSegment
	sta	[r0]
	long	M
lb12	brl	SkipExpression	skip the expression
;
;  Local data
;
val1	ds	4	first address
val2	ds	4	second address
mask	ds	4	bank mask
saveSegment ds 2	save segment number flag
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

	lda	[sp]	update the program counter
	and	#$00FF
	tay
	clc
	adc	pc
	sta	pc
	bcc	lb1
	inc	pc+2
lb1	inc4	sp	skip the op code
	tyx		save the length

	tya		move the bytes
	lsr	A
	bcc	lb2
	short M
	dey
	lda	[sp],Y
	sta	[op],Y
	long	M
lb2	dey
	dey
	bmi	lb3a
lb3	lda	[sp],Y
	sta	[op],Y
	dey
	dey
	bpl	lb3
lb3a	anop
	txa		update the output pointer
	clc
	adc	op
	sta	op
	bcc	lb4
	inc	op+2
lb4	txa		update sp
	clc
	adc	sp
	sta	sp
	bcc	lb5
	inc	sp+2
lb5	rts
	end

****************************************************************
*
*  DefineAlign - align to a power of 2 boundary
*
*  Inputs:
*	r0 - alignment factor
*	sp - pointer to the opcode
*	pc - current program counter
*
*  Outputs:
*	sp - pointer to the next opcode
*	pc - new program counter
*
****************************************************************
*
DefineAlign start
	using Common

	stz	total	total = 0
	stz	total+2
	move4 pc,tpc	save the pc
	ph4	r0	check the alignment factor
	jsr	CheckAlign
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
lb4	add4	total,r8	update the total
	add4	pc,r8
	bra	lb1	check the next bit

lb5	move4 tpc,pc	reset pc
	lda	total	if total <> 0 then
	ora	total+2
	beq	lb6
	move4 total,r0	  define an appropriate DS record
	jsr	DefineDS
lb6	rts

total	ds	4	total DS space
tpc	ds	4	temp pc
	end

****************************************************************
*
*  DefineDS - reserve space in a segment
*
*  Inputs:
*	r0 - # of bytes to reserve
*	sp - pointer to the opcode
*	pc - current program counter
*
*  Outputs:
*	sp - pointer to the next opcode
*	pc - new program counter
*
****************************************************************
*
DefineDS start
	using Common

	add4	pc,r0	update the program count
	lda	express	if express or (r0 < 10) then
	bne	lb0
	lda	r2
	bne	lb7
	lda	r0
	cmp	#10
	bge	lb7
lb0	ldx	r2	  fill 64K areas
	beq	lb2
	ldy	#0
	tya
lb1	sta	[op],Y
	dey
	dey
	bne	lb1
	inc	op+2
	dex
	bne	lb1
lb2	short M	  fill in remaining bytes
	lda	#0
	ldy	r0
	beq	lb5
	dey
	beq	lb4
lb3	sta	[op],Y
	dey
	bne	lb3
lb4	sta	[op]
lb5	long	M
	clc		  update op
	lda	op
	adc	r0
	sta	op
	bcc	lb6
	inc	op+2
lb6	bra	lb8	else {if not express then}
lb7	ph4	r0	  finish off the current lConst
	jsr	FinishLConst
	pl4	r0
	lda	#$F1	  place a DS record in the segment
	sta	[op]
	ldy	#1
	lda	r0
	sta	[op],Y
	iny
	iny
	lda	r2
	sta	[op],Y
	add4	op,#5,opst	  start a new lConst
	add4	op,#10	  update op
lb8	anop		endif
	rts
	end

****************************************************************
*
*  DefineSegment - put the segment in the symbol table
*
*  Inputs:
*	segName - pointer to the segment name
*	pc - current pc
*	segEntry - disp to segment entry point
*
****************************************************************
*
DefineSegment private
	using Common

	ph4	segName	push the symbol name ptr
	ph2	#1	the symbol is global
	clc		push the location
	lda	pc
	adc	segEntry
	tax
	lda	pc+2
	adc	segEntry+2
	pha
	phx
	jsr	Define2	define the symbol
	rts
	end

****************************************************************
*
*  DictReloc - Create a relocatable dictionary entry (current bank)
*
*  Inputs:
*	bankOrg - is the program bank relative?
*	expLength - expression length
*	expValue - expression value
*	shiftFlag - is the value shifted?
*	shiftValue - value before a shift
*	shiftCount - shift counter
*
****************************************************************
*
DictReloc private
	using Common
	using ExpCommon
	using OutCommon

	lda	bankOrg	if the program is bank relative then
	beq	lb0
	lda	shiftFlag	  if the value is not shifted then
	bne	lb0
	lda	expLength	    if the expression is 1 or 2 bytes then
	cmp	#3
	bge	lb0
	rts		      return

lb0	lda	#11	make sure there is room in the dictionary
	cmp	loadDictSize
	blt	lb1
	jsr	ExpandDictBuffer

lb1	lda	shiftFlag	if the expression is shifted then
	beq	lb1a
	move4 shiftValue,val	  use the unshifted value
	bra	lb1b	else
lb1a	move4 expValue,val	  use the returned value
lb1b	anop		endif
	lda	val+2	short = val and pc < 64K
	ora	pc+2
	sta	short
	short M	if short then
	bne	lb2
	lda	#$F5	  write the cReloc opcode
	sta	[dp]
	lda	expLength	  write the expression length
	cmp	#4
	bne	lb1c
	dec	A
lb1c	ldy	#1
	sta	[dp],Y
	bra	lb3	else
lb2	lda	#$E2	  write the Reloc opcode
	sta	[dp]
	lda	expLength	  write the expression length
	ldy	#1
	sta	[dp],Y
lb3	iny		write the shift count
	lda	shiftCount
	sta	[dp],Y
	long	M
	lda	short	if short then
	bne	lb4
	lda	pc	  save the pc
	iny
	sta	[dp],Y
	iny		  save the value
	iny
	lda	val
	sta	[dp],Y
	add4	dp,#7	  update dp
	sub2	loadDictSize,#7	  update loadDictSize
	rts		  return

lb4	iny		save the pc
	lda	pc
	sta	[dp],Y
	iny
	iny
	lda	pc+2
	sta	[dp],Y
	iny		save the value
	iny
	lda	val
	sta	[dp],Y
	iny
	iny
	lda	val+2
	sta	[dp],Y
	sub2	loadDictSize,#11	update loadDictSize
	add4	dp,#11	update dp
	rts
;
;  Local data
;
short	ds	2	is this a cReloc?
val	ds	4	expression value
	end

****************************************************************
*
*  DictInterseg - Create an interseg dictionary entry (another bank)
*
*  Inputs:
*	expLength - expression length
*	expValue - expression value
*	expSegment - expression segment number
*	shiftFlag - is the value shifted?
*	shiftValue - value before a shift
*	shiftCount - shift counter
*
*  Outputs:
*	A - 1 if the segment should be saved in the expression,
*	    else 0.  (The segment is saved with the expression
*	    for 3-byte cInterseg expressions with 0 shift when
*	    files are being compacted.)
*
****************************************************************
*
DictInterseg private
	using Common
	using ExpCommon
	using OutCommon

	stz	saveSegment	don't save the segment number
	lda	#15	make sure there is room in the dictionary
	cmp	loadDictSize
	blt	lb1
	jsr	ExpandDictBuffer

lb1	lda	shiftFlag	if the expression is shifted then
	beq	lb1a
	move4 shiftValue,val	  use the unshifted value
	bra	lb1b	else
lb1a	move4 expValue,val	  use the returned value
lb1b	anop		endif
	lda	expSegment	short = (val < 64K) and (pc < 64K)
	and	#$FF00	  and (expSegment < 256)
	ora	val+2
	ora	pc+2
	sta	short
	short M	if short then
	bne	lb2
	lda	#$F6	  write the cInterseg opcode
	sta	[dp]
	lda	expLength	  write the expression length
	cmp	#4
	bne	lb1c
	ldx	compact	  if compact then
	beq	ss1
	ldx	shiftFlag	    if not shiftFlag then
	bne	ss1
	ldx	#2	      set saveSegment
	stx	saveSegment
ss1	dec	A	  convert length to 3
lb1c	ldy	#1
	sta	[dp],Y
	bra	lb3	else
lb2	lda	#$E3	  write the interseg opcode
	sta	[dp]
	lda	expLength	  write the expression length
	ldy	#1
	sta	[dp],Y
lb3	ldy	#2	write the shift count
	lda	shiftCount
	sta	[dp],Y
	long	M
	lda	short	if short then
	bne	lb4
	lda	pc	  save the pc
	iny
	sta	[dp],Y
	iny		  save the expression segment
	iny
	lda	expSegment
	sta	[dp],Y
	iny		  save the value
	lda	val
	sta	[dp],Y
	sub2	loadDictSize,#8	  update loadDictSize
	add4	dp,#8	  update dp
	ldx	compact	  if compact then
	beq	lb4a
	ldx	expLength	    if expLength = 3 then
	cpx	#3
	bne	lb4a
	ldx	shiftFlag	      if not shiftFlag then
	bne	lb4a
	lda	#1		we do need to save the segment
	sta	saveSegment
lb4a	lda	saveSegment	  return the save segment code
	rts

lb4	iny		save the pc
	lda	pc
	sta	[dp],Y
	iny
	iny
	lda	pc+2
	sta	[dp],Y
	iny		set the file number to 1
	iny
	lda	#1
	sta	[dp],Y
	iny		save the segment number
	iny
	lda	expSegment
	sta	[dp],Y
	iny		save the value
	iny
	lda	val
	sta	[dp],Y
	iny
	iny
	lda	val+2
	sta	[dp],Y
	sub2	loadDictSize,#15	update loadDictSize
	add4	dp,#15	update dp
	lda	#0	don't save the segment number
	rts
;
;  Local data
;
saveSegment ds 2	save segment code:
!			 0: don't save the segment #
!			 1: save segment # in 3 byte field
!			 2: save segment # in 4 byte field
short	ds	2	is this a cReloc?
val	ds	4	expression value
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

	ldy	#1	get the value
	lda	[sp],Y
	sta	r4
	ldy	#3
	lda	[sp],Y
	sta	r6
	add4	sp,#5	skip the op code & operand
	sub4	pc,r4,r0	calculate the space to insert
	lda	r2	if space < 0 then
	bpl	lb1
	ph4	#0	  Error(NULL,3)
	ph2	#3
	jsr	Error
	rts		  return

lb1	jsr	DefineDS	handle the ORG
	rts
	end

****************************************************************
*
*  DoPass2 - Do pass 1 processing
*
*  Outputs:
*	C - set if an error occurred
*
****************************************************************
*
DoPass2	start
	using Common
;
;  Write the pass header
;
	lda	#2	pass = 2
	sta	pass
	lda	list	if (not list) and progress then
	bne	wp1
	lda	progress
	beq	wp1
	puts	#'Pass 2: '	  print the dot header
wp1	anop
;
;  Initialize pass dependent variables
;
	jsr	InitPass
	jsr	DynamicCheck
;
;  Process segments until there are no more
;
ps1	jsr	NextSegment	get the next segment
	bcc	rt1	branch if there are no more
	move	#0,dataAreas,#256	clear the data area flags
	jsr	DefineSegment	put the segment in the symbol table
	jsr	ListSeg	list the segname start info
	jsr	DoSegment	process the segment
	lda	segSpace	add in the reserved space (if any)
	ora	segSpace+2
	beq	ps1
	move4 segSpace,r0
	jsr	DefineDS
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
	dc	a'Using'	$E4		Using
	dc	a'Strong'	$E5		Strong
	dc	a'Global'	$E6		Global
	dc	a'Gequ'	$E7		Gequ
	dc	a'Invalid'	$E8		Mem
	dc	a'Invalid'	$E9		unused
	dc	a'Invalid'	$EA		unused
	dc	a'Expr'	$EB		Expr
	dc	a'ZExpr'	$EC		ZExpr
	dc	a'BExpr'	$ED		BExpr
	dc	a'RelExpr'	$EE		RelExpr
	dc	a'Local'	$EF		Local
	dc	a'Equ'	$F0		Equ
	dc	a'DS'	$F1		DS
	dc	a'Lconst'	$F2		LConst
	dc	a'LExpr'	$F3		LExpr
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
	using Common

	inc4	sp	skip the opcode
	ldy	#2	get the DS length
	lda	[sp]
	sta	r0
	lda	[sp],Y
	sta	r2
	jsr	DefineDS	handle the DS
	add4	sp,#4	skip the length
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
lb1	lda	segVersion	if the segment is version 0 ro 1 then
	cmp	#2
	beq	lb2
	add4	sp,#3	  skip the attributes (1 byte length)
	bra	lb3	else
lb2	add4	sp,#4	  skip the attributes (2 byte length)
lb3	anop		endif
	ph2	#0	the symbol is local
	ph4	#0	don't check for addressing errors
	jsr	Define2	define the symbol
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
	using ExpCommon
	using Common
	using OutCommon

	stz	saveSegment	saveSegment := false
	lda	[sp]	update the PC
	xba
	and	#$00FF
	sta	expLength
lb1	add4	sp,#2	skip the op code and expression length
	ph4	sp	evaluate the expression
	jsr	Evaluate
	sta	expValue
	stx	expValue+2
	lda	expSegment	if the expression uses values in another
	beq	lb2	  segment then
	cmp	loadNumber
	beq	lb2
	lda	symbolRelocatable	  if the expression is relocatable then
	beq	lb1a
	jsr	DictInterseg	    create a dictionary entry
	sta	saveSegment	    save the save segment flag
	lda	shiftFlag	    if the value is shifted then
	beq	sh1	      use the unshifted value
	move4 shiftValue,expValue
sh1	anop
lb1a	lda	expSegment	  if the segment is dynamic then
	jsr	IsDynamic
	bcc	lb3
	ph4	#0	    flag the error
	ph2	#17
	jsr	Error
	bra	lb3	else
lb2	lda	symbolRelocatable	  if the expression is relocatable then
	beq	lb3
	jsr	DictReloc	    create a dictionary entry
lb3	anop		endif
	jsr	PutValue	write an expression value
	lda	saveSegment	if saveSegment then
	beq	lb4
	sec		  save the segment number
	lda	op
	sbc	saveSegment
	sta	r0
	lda	op+2
	sbc	#0
	sta	r2
	short M
	clc
	lda	expSegment
	sta	[r0]
	long	M
lb4	brl	SkipExpression	skip the expression

saveSegment ds 2	save segment number flag
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
lb1	lda	segVersion	if the segment is version 0 or 1 then
	cmp	#2
	beq	lb2
	add4	sp,#3	  skip the attributes (1 byte length)
	bra	lb3	else
lb2	add4	sp,#4	  skip the attributes (2 byte length)
lb3	anop		endif
	ph2	#1	the symbol is global
	ph4	#0	don't ceck for addressing errors
	jsr	Define2	define the symbol
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
	add4	sp,#3	  skip the attributes (1 byte length)
	bra	lb3	else
lb2	add4	sp,#4	  skip the attributes (2 byte length)
lb3	anop		endif
	ph2	#1	the symbol is global
	ph4	pc	push the pass 2 value
	jsr	Define2	define the symbol
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

	ldy	#1	get the length
	lda	[sp],Y
	sta	r0
	iny
	iny
	lda	[sp],Y
	sta	r2
	add4	sp,#5	skip the op code, length

	ldx	r2	move 64K chunks
	beq	lb3
	ldy	#0
lb2	lda	[sp],Y
	sta	[op],Y
	dey
	dey
	bne	lb2
	inc	op+2
	inc	sp+2
	inc	pc+2
	dec	r2
	bne	lb2
lb3	ldy	r0	move the remaining bytes
	beq	lb6
	short M
	dey
	beq	lb5
lb4	lda	[sp],Y
	sta	[op],Y
	dey
	bne	lb4
lb5	lda	[sp]
	sta	[op]
	long	M

	add4	op,r0	update op for the <64K part
	add4	sp,r0	skip the rest of the record
	add4	pc,r0	update the PC
lb6	rts
	end

****************************************************************
*
*  LExpr - evaluate an expression, allowing references to dynamic segs
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
LExpr	private
	using ExpCommon
	using Common
	using OutCommon

	stz	saveSegment	saveSegment := false
	lda	[sp]	update the PC
	xba
	and	#$00FF
	sta	expLength
lb1	add4	sp,#2	skip the op code and expression length
	ph4	sp	evaluate the expression
	jsr	Evaluate
	sta	expValue
	stx	expValue+2
	lda	expSegment	if the expression uses values in another
	beq	lb2	  segment then
	cmp	loadNumber
	beq	lb2
	lda	expSegment	  if the segment is dynamic then
	jsr	IsDynamic
	bcc	lb1a
	jsr	JumpTable	    create a jump table entry
lb1a	lda	symbolRelocatable	  if the expression is relocatable then
	beq	lb3
	jsr	DictInterseg	    create a dictionary entry
	sta	saveSegment	    save the save segment flag
	lda	shiftFlag	    if the value is shifted then
	beq	lb3	      use the unshifted value
	move4 shiftValue,expValue
	bra	lb3	else
lb2	lda	symbolRelocatable	  if the expression is relocatable then
	beq	lb3
	jsr	DictReloc	    create a dictionary entry
lb3	anop		endif
	jsr	PutValue	write an expression value
	lda	saveSegment	if saveSegment then
	beq	lb4
	sec		  save the segment number
	lda	op
	sbc	saveSegment
	sta	r0
	lda	op+2
	sbc	#0
	sta	r2
	short M
	clc
	lda	expSegment
	sta	[r0]
	long	M
lb4	brl	SkipExpression	skip the expression

saveSegment ds 2	save segment number flag
	end

****************************************************************
*
*  ListSeg - list the segment start info
*
*  Inputs:
*	segName - ptr to name of the segment
*	segType - segment type
*	pc - segment disp
*	segLength - segment length
*	list - list info flag
*
****************************************************************
*
ListSeg	private
	using Common
	using OutCommon

	lda	list	if list then
	jeq	lb3
	ph4	pc	  print the program counter
	ph2	#8
	ph2	#0
	jsr	PrintHex
	putc	#' '
	ph4	segLength	  print the segment length
	ph2	#8
	ph2	#0
	jsr	PrintHex
	putc	#' '	  print the load segment number
	lda	loadNumber
	ldx	kflag
	beq	lb0
	ldx	express
	beq	lb0
	inc	A
lb0	pea	0
	pha
	ph2	#2
	ph2	#0
	jsr	PrintHex
	lda	segType	  print the segment type
	lsr	A
	bcc	lb1
	puts	#' Data: '
	bra	lb2
lb1	puts	#' Code: '
lb2	sub4	segName,#1,r0	  print the segment name
	puts	[r0],cr=t
	jsr	CheckForPause	  check for early exit
	rts

lb3	lda	progress	else if progres then
	beq	lb4
	putc	#'.'	  print a dot
lb4	jsr	CheckForPause	  check for early exit
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
	add4	sp,#3	  skip the attributes (1 byte length)
	bra	lb3	else
lb2	add4	sp,#4	  skip the attributes (2 byte length)
lb3	anop		endif
	ph2	#0	the symbol is local
	ph4	pc	push the pass 2 value
	jsr	Define2	define the symbol
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
*  PutValue - write a value to the file
*
*  Inputs:
*	expValue - expression value
*	expLength - expression length
*
****************************************************************
*
PutValue private
	using ExpCommon

	lda	expLength	write the value
	cmp	#2
	bge	lb4
	short M	write a 1 byte value
	lda	expValue
	sta	[op]
	long	M
	bra	lb7
lb4	bne	lb5
	lda	expValue	write a 2 byte value
	sta	[op]
	bra	lb7
lb5	cmp	#4
	beq	lb6
	lda	expValue	write a 3 byte value
	sta	[op]
	ldy	#1
	lda	expValue+1
	sta	[op],Y
	bra	lb7
lb6	lda	expValue	write a 4 byte value
	sta	[op]
	ldy	#2
	lda	expValue+2
	sta	[op],Y
lb7	clc		update op
	lda	op
	adc	expLength
	sta	op
	bcc	lb8
	inc	op+2
lb8	clc		update pc
	lda	pc
	adc	expLength
	sta	pc
	bcc	lb9
	inc	pc+2
lb9	rts
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
	using ExpCommon
	using Common
	using OutCommon

	lda	[sp]	update the PC
	xba
	and	#$00FF
	sta	expLength
lb1	ldy	#2	add pc, org and value
	clc
	lda	[sp],Y
	adc	pc
	sta	t1
	iny
	iny
	lda	[sp],Y
	adc	pc+2
	sta	t1+2
	add4	t1,loadOrg
	add4	sp,#6	skip the op code, length & value
	ph4	sp	evaluate the expression
	jsr	Evaluate
	sta	expValue
	stx	expValue+2
	sub4	expValue,t1	compute rel displacement

	add4	expValue,loadOrg,t1	t1 = expValue+loadOrg
	short I,M	check t1 for branch out of range
	lda	expLength
	cmp	#4
	bge	lb6
	tay
	tax
	lda	t1,X
	bmi	lb3
lb2	lda	t1,X
	bne	lb5
	inx
	cpx	#4
	blt	lb2
	lda	t1-1,Y
	bpl	lb6
	bra	lb4a
lb3	lda	#$FF
lb4	cmp	t1,X
	bne	lb5
	inx
	cpx	#4
	blt	lb4
	lda	t1-1,Y
	bmi	lb6
lb4a	lda	expLength	let BRL wrap around within program bank
	cmp	#2
	bge	lb6
lb5	long	I,M
	ph4	#0
	ph2	#11
	jsr	Error
lb6	long	I,M

	lda	expSegment	if the expression uses values in another
	beq	lb7	  segment then flag the error
	cmp	loadNumber
	beq	lb7
	ph4	#0
	ph2	#10
	jsr	Error
lb7	jsr	PutValue	write an expression value
	brl	SkipExpression	skip the expression

t1	ds	4	temp value
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
*  Strong - Strong label reference
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
	using ExpCommon

	inc4	sp	skip the op code
	jsr	Reference2	make a reference to the name
	stz	expSegment	find the symbol value (forces error)
	ph4	sp
	ph2	#1
	jsr	GetSymbolValue
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
	jsr	Reference2	make a reference to the name
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
*  Using - Note that we are using a data area
*
*  Inputs:
*	sp - pointer to the opcode
*
*  Outputs:
*	sp - pointer to the next opcode
*
****************************************************************
*
Using	private
	using ExpCommon
	using Common

	inc4	sp	skip the op code
	jsr	Reference2	make a reference to the name
	stz	expSegment	find the symbol
	ph4	sp
	ph2	#1
	jsr	GetSymbolValue
	lda	symbolFlag	if not a data area then
	and	#isDataArea
	bne	lb1
	ph4	sp	  Error(sp,8)
	ph2	#8
	jsr	Error
	bra	lb2	else
lb1	ldx	symbolData	  set the data area flag
	short M
	lda	#1
	sta	dataAreas,X
	long	M
lb2	anop		endif

	lda	[sp]	skip the name in the obj segment
	and	#$00FF
	sec
	adc	sp
	sta	sp
	bcc	lb3
	inc	sp+2
lb3	rts
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

****************************************************************
*
*  ZExpr - evaluate a zero page expression
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
ZExpr	private
	using ExpCommon
	using Common
	using OutCommon

	lda	[sp]	update the PC
	xba
	and	#$00FF
	sta	expLength
lb1	add4	sp,#2	skip the op code and expression length
	ph4	sp	evaluate the expression
	jsr	Evaluate
	sta	expValue
	stx	expValue+2
	ldx	expLength	make sure truncated bytes are 0
	cpx	#4
	bge	lb2
lb1a	lda	expValue,X
	and	#$00FF
	beq	lb1b
	ph4	#0
	ph2	#9
	jsr	Error
	bra	lb2
lb1b	inx
	cpx	#4
	blt	lb1a

lb2	lda	symbolRelocatable	if the expression is relocatable then
	beq	lb3
	ph4	#0	  flag an error
	ph2	#9
	jsr	Error
lb3	jsr	PutValue	write an expression value
	brl	SkipExpression	skip the expression
	end
