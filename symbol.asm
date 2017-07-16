	keep	obj/symbol
	mcopy symbol.mac
****************************************************************
*
*  Symbol Tables
*
*  This module contains the subroutines used to create, search
*  and manipulate the symbol table.
*
****************************************************************
	copy	directPage
****************************************************************
*
*  SymbolCommon - global data for the symbol table module
*
****************************************************************
*
SymbolCommon privdata
;
;  Symbol table entry
;
symNext	equ	0	pointer to the next symbol
symAlpha equ	4	alphabetized list pointer
symVal	equ	8	value of the label (or ptr to expression)
symSeg	equ	12	segment number
symFile	equ	14	file number
symData	equ	16	data area number
symExp	equ	18	is the value an expression?
symFlag	equ	20	pass 1/2 resolved flags
symPriv	equ	22	is the symbol private?
symLength equ	24	length attribute
symType	equ	26	type attribute
symName	equ	28	symbol name (p-string)

symSize	equ	28	size of a symbol, sans symbol name
;
;  Constants
;
hashSize equ	877	number of hash buckets
blockSize equ	4096	symbol table blocking factor
;
;  Symbol table variables
;
alpha	ds	4	head of alphabetized list
hashDisp ds	2	disp in hash table; saved for efficiency

poolPtr	ds	4	ptr to next byte in symbol table pool
poolSize ds	2	# of bytes left in the current pool

table	ds	hashSize*4	symbol table
	end

****************************************************************
*
*  AllocatePool - allocate a new symbol table pool
*
*  Outputs:
*	poolPtr - pointer to the first byte of the pool
*	poolSize - size of the block, in bytes
*
****************************************************************
*
AllocatePool private
	using SymbolCommon

	ph4	#blockSize
	jsr	MLalloc
	sta	poolPtr
	stx	poolPtr+2
	lda	#blockSize
	sta	poolSize
	rts
	end

****************************************************************
*
*  AlphaInsert - insert the symbol in the alphabetized list
*
*  Inputs:
*	sym - pointer to the new symbol
*	alpha - head of the alphabetized list
*
****************************************************************
*
AlphaInsert private
	using SymbolCommon
p1	equ	1	work pointers
p2	equ	5
p3	equ	9

	sub	(4:sym),12

	lda	alpha	if alpha = nil then
	ora	alpha+2
	bne	lb1
	move4 sym,alpha	  alpha = sym
	ldy	#symAlpha	  sym^.alpha = nil
	lda	#0
	sta	[sym],Y
	iny
	iny
	sta	[sym],Y
	brl	lb8	  return

lb1	move4 alpha,p1	p1 = alpha
	stz	p2	p2 = nil
	stz	p2+2
	add4	sym,#symName	while sym^.symName >= p1^.symName do
	lda	[sym]
	and	#$00FF
	sta	len1
lb2	add4	p1,#symName,p3
	lda	len1
	sta	lens
	lda	[p3]
	and	#$00FF
	sta	len2
	cmp	lens
	bge	lb3
	sta	lens
lb3	short M
	ldy	#1
lb4	lda	[sym],Y
	cmp	[p3],Y
	bne	lb5
	iny
	dec	lens
	bne	lb4
	lda	len1
	cmp	len2
lb5	long	M
	blt	lb6
	move4 p1,p2	  p2 = p1
	ldy	#symAlpha	  p1 = p2^.symAlpha
	lda	[p2],Y
	sta	p1
	iny
	iny
	lda	[p2],Y
	sta	p1+2
	ora	p1	  quit if at the end of the list
	bne	lb2	endwhile
lb6	sub4	sym,#symName	fix sym

	ldy	#symAlpha	sym^.symAlpha = p1
	lda	p1
	sta	[sym],Y
	iny
	iny
	lda	p1+2
	sta	[sym],Y
	lda	p2	if p2 = nil then
	ora	p2+2
	bne	lb7
	move4 sym,alpha	  alpha = sym
	bra	lb8	  return

lb7	ldy	#symAlpha	p2^.symAlpha = sym
	lda	sym
	sta	[p2],Y
	iny
	iny
	lda	sym+2
	sta	[p2],Y

lb8	ret
;
;  Local data
;
lens	ds	2	shortest string length
len1	ds	2	length(sym^.symName)
len2	ds	2	length(p1^.symName)
	end

****************************************************************
*
*  CreateSymbol - create a new symbol table entry
*
*  Inputs:
*	name - name of the new entry
*	hashDisp - disp in hash table for the entry
*
*  Outputs:
*	returns a pointer to the new symbol table entry
*
****************************************************************
*
CreateSymbol private
	using Common
	using OutCommon
	using SymbolCommon
entryLength equ 1	length of the symbol table entry
sym	equ	3	ptr to symbol table entry
p1	equ	7	work pointer

	sub	(4:name),10

	lda	[name]	no match - create a symbol table entry
	and	#$00FF
	sec
	adc	#symSize
	sta	entryLength
	cmp	poolSize
	ble	cs2
	jsr	AllocatePool	no room - get a new pool
cs2	sub2	poolSize,entryLength	subtract the space we need
	clc		update the pool pointer and get a copy
	lda	poolPtr
	sta	sym
	adc	entryLength
	sta	poolPtr
	lda	poolPtr+2
	sta	sym+2
	adc	#0
	sta	poolPtr+2

	ldx	hashDisp	place the record in the hash list
	ldy	#2
	lda	table,X
	sta	[sym]
	lda	table+2,X
	sta	[sym],Y
	lda	sym
	sta	table,X
	lda	sym+2
	sta	table+2,X
	ldy	#symSeg	record our load segment number
	lda	loadNumber
	sta	[sym],Y
	ldy	#symFile	record our file number
	lda	fileNumber
	sta	[sym],Y
	ldy	#symFlag	the value is not resolved
	lda	#0
	sta	[sym],Y
	ldy	#symPriv	the symbol is not private
	sta	[sym],Y
	add4	sym,#symName,p1	record the symbol name
	lda	[name]
	and	#$00FF
	tay
	short M
cs3	lda	[name],Y
	sta	[p1],Y
	dey
	bpl	cs3
	long	M
	lda	symbols	if the symbol table will be printed then
	beq	cs4
	ph4	sym	  insert in alphabetical list
	jsr	AlphaInsert
cs4	ret	4:sym
	end

****************************************************************
*
*  Define - define a symbol
*
*  Inputs:
*	name - ptr to the symbol name
*	length - length attribute
*	type - type attribute
*	private - is the symbol private?
*	global - is the symbol global? (or local)
*	expression - is the value an expression? (or a constant)
*	value - symbol value, or pointer to the expression
*	isData - is the symbol a data area name?
*	isSegment - is the symbol a segment name?
*
****************************************************************
*
Define	start
	using Common
	using SymbolCommon
	using ExpCommon
	using OutCommon
sym	equ	1	pointer to symbol table entry

 sub (4:name,2:length,2:type,2:private,2:global,2:expression,4:value,2:isData,2:isSegment),4

	lda	global	if the symbol is local then
	bne	fs1
	lda	dataNumber	  if dataNumber = 0 then
	jeq	lb2	    return {don't define the symbol!}

!			{get a symbol to define}
fs1	ph4	name	if ((sym = FindSymbol(name)) != NULL) {
	jsr	FindSymbol
	sta	sym
	stx	sym+2
	ora	sym+2
	beq	fs5
	ldy	#symFile	  if (sym->symFile == fileNumber)
	lda	[sym],Y
	cmp	fileNumber
	beq	fs6	    goto fs6;
fs2	ldy	#symFile	  while (sym->symFile != fileNumber) {
	lda	[sym],Y
	cmp	fileNumber
	beq	fs4
fs3	ldy	#2	    sym = sym->symNext;
	lda	[sym]
	tax
	lda	[sym],Y
	sta	sym+2
	stx	sym
	ora	sym	    if (sym == NULL)
	beq	fs5	      goto fs5;
	bra	fs2	    }
fs4	clc		  if (!Match(& sym->symName,name)) {
	lda	sym
	adc	#symName
	tax
	lda	sym+2
	adc	#^symName
	pha
	phx
	ph4	name
	jsr	Match
	tax
	beq	fs6
	bra	fs3	    sym = sym->symNext;
!			    if (sym == NULL)
!			      goto fs5;
!			    goto fs2;
!			    }
!			  }
	bra	fs6	else
fs5	ph4	name	  sym = CreateSymbol(name);
	jsr	CreateSymbol
	sta	sym
	stx	sym+2
fs6	anop

	lda	expression	if the value is an expression then
	beq	ex1
	ph4	value	  copy the expression
	jsr	CopyExpression
	ldy	#symVal
	sta	[sym],Y
	ldy	#symVal+2
	txa
	sta	[sym],Y

	ldy	#symExp	  set the expression flag
	lda	copiedExpression
	sta	[sym],Y
	bne	ex2	  if a constant was returned then
	ldy	#symFlag	    set the constant flag
	lda	#isConstant
	ora	[sym],Y
	sta	[sym],Y
	bra	ex2	else
ex1	ldy	#symVal	  save the symbol value
	lda	value
	sta	[sym],Y
	ldy	#symVal+2
	lda	value+2
	sta	[sym],Y
	ldy	#symExp	  clear expression flag
	lda	#0
	sta	[sym],Y
ex2	anop		endif

	ldy	#symLength	set the length attribute
	lda	length
	sta	[sym],Y
	ldy	#symType	set the type attribute
	lda	type
	sta	[sym],Y
	ldy	#symPriv	set the private flag
	lda	private
	sta	[sym],Y
	ldy	#symData	set the data area number
	lda	dataNumber
	ldx	global
	beq	ex3
	ldx	isData
	bne	ex3
	lda	#0
ex3	sta	[sym],Y
	ldy	#symFile	set the file number
	lda	fileNumber
	sta	[sym],Y
	ldy	#symSeg	set the load segment number
	lda	loadNumber
	sta	[sym],Y
	ldy	#symFlag	set the "resolved on pass 1" flag
	lda	[sym],Y
	ora	#pass1Resolved+pass1Requested
	sta	[sym],Y
	lda	isData	if isData then
	beq	lb2
!	ldy	#symFlag	  set the data area flag
	lda	[sym],Y
	ora	#isDataArea
	sta	[sym],Y
lb2	lda	isSegment	if isSegment then
	beq	lb3
!	ldy	#symFlag	  set the segment flag
	lda	[sym],Y
	ora	#isSegmentFlag
	sta	[sym],Y

lb3	ret
	end

****************************************************************
*
*  Define2 - note that the symbol is resolved on pass 2
*
*  Inputs:
*	name - ptr to the symbol name
*	global - is the symbol global?
*	value - value, or 0 if the value does not need to be checked
*
****************************************************************
*
Define2	start
	using Common
	using SymbolCommon
sym	equ	1	pointer to symbol table entry
p1	equ	5	copy of sym; used for duplicate check

	sub	(4:name,2:global,4:value),8

	lda	global	if the symbol is local then
	bne	lb1
	lda	dataNumber	  if dataNumber = 0 then
	jeq	lb10	    return {don't define the symbol!}

!			/* find the correct symbol */
lb1	ph4	name	sym = FindSymbol(name);
	jsr	FindSymbol	p1 = sym;
	sta	sym
	sta	p1
	stx	sym+2
	stx	p1+2
	ldy	#symFile	if (sym->symFile != fileNumber) {
	lda	[sym],Y
	cmp	fileNumber
	beq	lb5
lb2	ldy	#symFile	  while (sym->symFile != fileNumber)
	lda	[sym],Y
	cmp	fileNumber
	beq	lb4
lb3	ldy	#2	    sym = sym->symNext;
	lda	[sym],Y
	tax
	lda	[sym]
	sta	sym
	stx	sym+2
	bra	lb2
lb4	clc		  if (!Match(& sym->symName,name) {
	lda	sym
	adc	#symName
	tax
	lda	sym+2
	adc	#^symName
	pha
	phx
	ph4	name
	jsr	Match
	tax
	bne	lb3
!			    sym = sym->symNext;
!			    goto lb2;
!			    }
!			  }
!			/* check for duplicates in this file */
lb5	ldy	#symFlag	if (sym->symFlag & pass2Resolved)
	lda	[sym],Y
	and	#pass2Resolved
	bne	lb7
!			  Error(DUPLICATE_SYMBOL);
!			/* check for duplicate globals in  */
!			/* two different files		   */
	ldy	#symPriv	else if ((global) && (!sym^.symPriv)){
	lda	[sym],Y
	bne	lb9
lb6	lda	p1	  while (p1 != NULL) {
	ora	p1+2
	beq	lb9
	cmpl	p1,sym	    if (p1 != sym)
	beq	lb8
	clc		      if (Match(p1->symName,name))
	lda	p1
	adc	#symName
	tax
	lda	p1+2
	adc	#^symName
	pha
	phx
	ph4	name
	jsr	Match
	tax
	bne	lb8
	ldy	#symPriv		if (!p1->symPriv) {
	lda	[p1],Y
	bne	lb8
	ldy	#symFlag		  if (p1->symFlag & pass2Resolved)
	lda	[p1],Y
	and	#pass2Resolved
	beq	lb8
lb7	ph4	name		  Error(DUPLICATE_SYMBOL);
	ph2	#1
	jsr	Error
	bra	lb9		  goto lb9;
!				  }
lb8	ldy	#2	    p1 = p1->symNext;
	lda	[p1],Y
	tax
	lda	[p1]
	sta	p1
	stx	p1+2
	bra	lb6	    }
!			  }
lb9	anop

	ldy	#symFlag	set the "resolved on pass 2" flag
	lda	[sym],Y
	ora	#pass2Resolved+pass2Requested
	sta	[sym],Y
	ldy	#symExp	if the symbol is an expression then
	lda	[sym],Y
	beq	lb9a
	ldy	#symVal+2	  make sure all needed symbols are
	lda	[sym],Y	    available

;	and	#$FF00	debug
;	beq	db1	debug
;	brk	$33	debug
;b1	lda	[sym],Y	debug

	pha
	dey
	dey
	lda	[sym],Y
	pha
	jsr	Evaluate
lb9a	lda	value	if value <> 0 then
	ora	value+2
	beq	lb10
	ldy	#symVal	  if value <> sym^.symVal then
	lda	[sym],Y
	cmp	value
	bne	lb9b
	iny
	iny
	lda	[sym],Y
	cmp	value+2
	beq	lb10
lb9b	ph4	name	    addressing error
	ph2	#7
	jsr	Error

lb10	ret
	end

****************************************************************
*
*  FindFirstSymbol - find the alphabetically smallest symbol
*
*  Inputs:
*	r12 - head of the symbol table
*
*  Outputs:
*	r0 - pointer to the first symbol
*	r12 - pointer to the remaining symbols
*	C - set if a symbol was found, else clear
*
*  Notes:
*	Only symbols resolved on pass 2 are returned.
*
****************************************************************
*
FindFirstSymbol private
	using Common
	using SymbolCommon

lb0	lda	r12	if r12 = nil then
	ora	r14
	bne	lb1	  return false
	clc
	rts

lb1	move4 r12,r0	r0 = r12
	ldy	#symAlpha	r12 = r0^.symAlpha
	lda	[r0],Y
	sta	r12
	iny
	iny
	lda	[r0],Y
	sta	r14
	ldy	#symFlag	if the symbol was not resolved then
	lda	[r0],Y
	and	#pass2Resolved
	beq	lb0	  skip this one
	sec		return true
	rts
	end

****************************************************************
*
*  FindSymbol - find a symbol
*
*  Inputs:
*	name - pointer to the symbol name
*
*  Outputs:
*	A-X - address of the symbol table entry; nil for none
*	hashDisp - hash table displacement
*
*  Notes:
*	There may be several symbols with the same name.  In
*	that case, this subroutine returns the first one in
*	the hash bucked.  You can scan forward from sym^.next
*	to find the others.
*
****************************************************************
*
FindSymbol private
	using SymbolCommon
sym	equ	1	symbol table pointer

	sub	(4:name),4

	ph4	name	get the address of the proper hash bucket
	jsr	Hash
	sta	hashDisp
	tax
	lda	table,X
	sta	sym
	lda	table+2,X
	sta	sym+2
	ora	sym	branch if it is empty
	beq	lb2

lb1	clc		if the names match then
	lda	sym
	adc	#symName
	tax
	lda	sym+2
	adc	#0
	pha
	phx
	ph4	name
	jsr	Match
	tax
	beq	lb2	  return
	ldy	#2	next symbol
	lda	[sym],Y
	tax
	lda	[sym]
	sta	sym
	stx	sym+2
	ora	sym+2
	bne	lb1

lb2	ret	4:sym
	end

****************************************************************
*
*  GetSymbolMemory - get memory from the symbol table pool
*
*  Inputs:
*	size - number of bytes to reserve
*
*  Outputs:
*	returns a pointer to the memory
*
****************************************************************
*
GetSymbolMemory start
	using SymbolCommon
ptr	equ	1	pointer to the memory

	sub	(2:size),4

	lda	size	if there isn't enough room then
	cmp	poolSize
	ble	lb2
	cmp	#blockSize	  if the request is bigger than
	blt	lb1	    blockSize then
	pea	0	    get the memory from MLalloc
	pha
	jsr	MLAlloc
	sta	ptr
	stx	ptr+2
	bra	lb3	    return
lb1	jsr	AllocatePool	  no room - get a new pool

lb2	sub2	poolSize,size	subtract the space we need
	clc		update the pool pointer and get a copy
	lda	poolPtr
	sta	ptr
	adc	size
	sta	poolPtr
	lda	poolPtr+2
	sta	ptr+2
	adc	#0
	sta	poolPtr+2

lb3	ret	4:ptr
	end

****************************************************************
*
*  GetSymbolValue - get the value of a symbol; for Evaluate only!
*
*  Inputs:
*	name - name of the symbol
*	strong - strong reference? (or weak)
*	fileNumber - current file number
*	expSegment - segment for the current expression
*
*  Outputs:
*	symbolValue - symbol value
*	symbolRelocatable - is the symbol relocatable?
*	symbolLength - length attribute
*	symbolCount - count attribute
*	symbolType - type attribute
*	symbolFlag - symbol flags
*	symbolData - data area number
*
****************************************************************
*
GetSymbolValue start
	using Common
	using OutCommon
	using SymbolCommon
	using ExpCommon

sym	equ	1	pointer to the symbol
p1	equ	5	work pointer

	sub	(4:name,2:strong),8
;
;  Find the symbol in the hash table
;
	ph4	name	find the symbol
	jsr	FindSymbol
	sta	sym
	sta	p1
	stx	sym+2
	stx	p1+2
	ora	p1+2
	beq	nosym
;
;  Find the private version of the symbol.  Use it if it is resolved.
;
	ldy	#symFile	if p1^.symFile <> fileNumber then
	lda	[p1],Y
	cmp	fileNumber
	beq	lb4
lb1	ldy	#symFile	  while p1^.symFile <> fileNumber do
	lda	[p1],Y
	cmp	fileNumber
	beq	lb3
lb2	ldy	#2	    p1 := p1^.next;
	lda	[p1],Y
	tax
	lda	[p1]
	sta	p1
	stx	p1+2
	ora	p1+2
	beq	gb1
	bra	lb1
lb3	clc		  if not Match(p1^.symName,name) then
	lda	#symName
	adc	p1
	tax
	lda	#^symName
	adc	p1+2
	pha
	phx
	ph4	name
	jsr	Match
	tax
	bne	lb2	    p1 := p1^.next;
!			    goto 1;
!			    endif
lb4	anop		  endif

	ldy	#symFlag	if the private symbol is resolved then
	lda	[p1],Y
	and	#pass1Resolved
	beq	gb1
	move4 p1,sym	  use it
	bra	sv1
;
;  Find the global version of the symbol.
;
gb1	ldy	#symFlag	while not (sym^.symFlag & pass1Resolved) do
	lda	[sym],Y
	and	#pass1Resolved
	bne	gb3
gb2	ldy	#2	  sym := sym^.next;
	lda	[sym],Y
	tax
	lda	[sym]
	sta	sym
	stx	sym+2
	ora	sym+2	  if sym = nil then
	bne	gb1
nosym	stz	symbolValue	    symbolValue = 0
	stz	symbolValue+2
	stz	symbolRelocatable	    symbolRelocatable = false
	stz	symbolLength	    symbolLength = 0
	stz	symbolCount	    symbolCount = 0
	stz	symbolType	    symbolType = 0
	stz	symbolFlag	    symbolFlag = 0
	stz	symbolData	    symbolData = 0
	stz	symbolFile	    symbolFile = 0
	lda	strong	    if the reference is strong then
	jeq	rt1
	ph4	name	      flag the error
	ph2	#6
	jsr	Error
	brl	rt1	    return
gb3	ldy	#symPriv	if sym^.symPriv then
	lda	[sym],Y
	bne	gb2	  sym := sym^.next;
!			  goto 1;
!			  endif
	clc		if not Match(sym^.symName,name) then
	lda	#symName
	adc	sym
	tax
	lda	#^symName
	adc	sym+2
	pha
	phx
	ph4	name
	jsr	Match
	tax
	bne	gb2	  sym := sym^.next;
!			  goto 1;
!			  endif
!			endif
;
;  Set the symbol values
;
sv1	ldy	#symExp	if the value is an expression then
	lda	[sym],Y
	jeq	sv2
	ph2	copiedExpression	  save volitile variables
	ph4	shiftCount
	ph2	shiftFlag
	ph4	shiftValue
	ph2	symbolCount
	ph2	symbolLength
	ph2	symbolRelocatable
	ph2	symbolType
	ph4	symbolValue
	ldy	#symVal+2	  evaluate the expression
	lda	[sym],Y
	pha
	dey
	dey
	lda	[sym],Y
	pha
	jsr	Evaluate
	sta	symbolValue	  save the value
	stx	symbolValue+2
	lda	shiftFlag	  if the value is shifted then
	beq	sv1a
	ph4	name	    flag the error
	ph2	#2
	jsr	Error
sv1a	lda	symbolRelocatable	  if the symbol is relocatable then
	beq	sv1c
	jsr	CheckSegment	    check for errors
	ldy	#symSeg	    set the expression file
	lda	[sym],Y
	sta	expSegment
sv1c	pl4	symbolValue	  restore volitile variables
	pl2	symbolType
	pl2	symbolRelocatable
	pl2	symbolLength
	pl2	symbolCount
	pl4	shiftValue
	pl2	shiftFlag
	pl4	shiftCount
	pl2	copiedExpression
	bra	sv3	else
sv2	ldy	#symVal	  set the value
	lda	[sym],Y
	sta	symbolValue
	iny
	iny
	lda	[sym],Y
	sta	symbolValue+2
	stz	symbolRelocatable	  set the relocation flag
	ldy	#symFlag
	lda	[sym],Y
	and	#isConstant
	bne	sv3
	inc	symbolRelocatable	  if relocatable then
	jsr	CheckSegment	    check for cross-file errors
	ldy	#symSeg	    set the expression file
	lda	[sym],Y
	sta	expSegment
sv3	anop		endif

	lda	#1	count attribute is 1
	sta	symbolCount
	ldy	#symLength	set the length attribute
	lda	[sym],Y
	sta	symbolLength
	ldy	#symType	set the type attribute
	lda	[sym],Y
	sta	symbolType
	ldy	#symFlag	set the flags
	lda	[sym],Y
	sta	symbolFlag
	ldy	#symFile	set the file
	lda	[sym],Y
	sta	symbolFile
	ldy	#symData	set the data area number
	lda	[sym],Y
	sta	symbolData
	beq	rt1	if symbolData <> 0 then
	lda	symbolFlag	  if not symbolFlag & isDataArea then
	and	#isDataArea
	bne	rt1
	ldx	symbolData	    if not dataAreas[symbolData] then
	lda	dataAreas,X
	and	#$00FF
	bne	rt1
	lda	strong	      if the reference is strong then
	beq	rt1
	ph4	name		flag unresolved reference
	ph2	#6
	jsr	Error
rt1	ret
;
;  CheckSegment - check for cross-file expressions
;
CheckSegment anop
	lda	expSegment	    if the expression is file-sensitive
	beq	cf1	      then
	ldy	#symSeg	      verify that the files match
	cmp	[sym],Y
	beq	cf1
	clc		      nope -> flag the error
	lda	sym
	adc	#symName
	tax
	lda	sym+2
	adc	#^symName
	pha
	phx
	ph2	#24
	jsr	Error
cf1	rts
	end

****************************************************************
*
*  Hash - find the hash tabe displacement for a symbol
*
*  Inputs:
*	ptr - pointer to the symbol name
*
*  Outputs:
*	returns the displacement into table
*
****************************************************************
*
Hash	private
	using SymbolCommon
disp	equ	1	hash displacement
temp	equ	3	temp value; for forming char pairs

	sub	(4:ptr),4

!			{get ready for the sum loop}
	lda	[ptr]	get the # of characters
	and	#$00FF
	lsr	A	X = numChars div 2
	tax
	bcc	lb1	if odd(numChars) then
	lda	[ptr]	  disp = ch[1] & $3F
	xba
	and	#$003F
	sta	disp
	ldy	#2	  Y = 2
	bra	lb2	else
lb1	stz	disp	  disp = 0
	ldy	#1	  y = 1
lb2	anop		endif

!			{add pairs of characters to the sum}
	txa		quit now if there was 1 character
	beq	lb4

lb3	lda	[ptr],Y	fetch a character pair
	and	#$003F	compact it to 12 bits
	sta	temp
	lda	[ptr],Y
	and	#$3F00
	lsr	A
	lsr	A
	ora	temp
	adc	disp	add the result to disp
	sta	disp
	iny		next pair
	iny
	dex
	bne	lb3

!			{create a hash table displacement}
lb4	lda	disp	mod result with # of buckets
lb5	cmp	#hashSize
	blt	lb6
	sec
	sbc	#hashSize
	bra	lb5
lb6	asl	A	convert to a displacement
	asl	A
	sta	disp

	ret	2:disp
	end

****************************************************************
*
*  InitSymbol - initialize the symbol table module
*
*  Outputs:
*	poolSize - set to 0
*	table - all pointers set to nil
*
****************************************************************
*
InitSymbol start
	using SymbolCommon

	stz	alpha	alpha = nil
	stz	alpha+2
	stz	poolSize	no bytes in the symbol pool
	move	#0,table,#hashSize*4	zero the hash table
	rts
	end

****************************************************************
*
*  Match - see if two names match
*
*  Inputs:
*	p1,p2 - pointers to the two names
*
*  Outputs:
*	A - 0 if the names match, 1 if they do not
*
*  Notes:
*	This subroutine assumes that the names are not null
*	strings.
*
****************************************************************
*
Match	private
res	equ	1	do the names match?

	sub	(4:p1,4:p2),2

	lda	#1	assume they do not match
	sta	res
	lda	[p1]	check the length & first char
	cmp	[p2]
	bne	mt2
	and	#$00FF	check the characters
	tay
	short M
mt1	lda	[p1],Y
	cmp	[p2],Y
	bne	mt2
	dey
	bne	mt1
	long	M
	stz	res	the strings match

mt2	long	M
	ret	2:res
	end

****************************************************************
*
*  NeedSegment - do we need this symbol from the library?
*
*  Inputs:
*	name - pointer to the symbol name
*	priv - private flag
*	fileNumber - symbol's file number
*	pass - pass number
*
*  Outputs:
*	A - 1 if we need it, 0 if we don't
*
****************************************************************
*
NeedSegment start
	using Common
	using SymbolCommon
need	equ	1	do we need it?
sp	equ	3	symbol pointer
maybe	equ	7	we may need this symbol

	sub	(4:name,2:priv),8

	stz	need	assume we don't need it
	stz	maybe	so far, we have no need
	ph4	name	if the symbol is not found then
	jsr	FindSymbol
	sta	sp
	stx	sp+2
	ora	sp+2
	jeq	lb10	  we don't need it

	lda	priv	if the library symbol is private then
	beq	lb4
	ldy	#symFile	  if (sp->symFile != fileNumber) {
	lda	[sp],Y
	cmp	fileNumber
	beq	lb3
lb1	ldy	#symFile	    while (sp->symFile != fileNumber) {
	lda	[sp],Y
	cmp	fileNumber
	beq	lb2a
lb2	ldy	#2	      sp = sp->symNext;
	lda	[sp],Y
	tax
	lda	[sp]
	sta	sp
	stx	sp+2
	ora	sp+2	      if (sp == NULL)
	jeq	lb10		return false;
	bra	lb1	      }
lb2a	clc		    if (!Match(sp->symName,sp)) {
	lda	sp
	adc	#symName
	tax
	lda	sp+2
	adc	#^symName
	pha
	phx
	ph4	name
	jsr	Match
	tax
	bne	lb2
!			      sp = sp->symNext;
!			      goto lb1;
lb3	anop		      }
	jsr	CheckSymbol	  check the symbol
	bcc	lb10
	inc	need
	bra	lb10
lb4	anop		else {if library symbol is global then}
	lda	sp	  while (sp != NULL)
	ora	sp+2
	beq	lb7
	ldy	#symPriv	    if (!sp->symPriv)
	lda	[sp],Y
	bne	lb6
	clc		      if (Match(sp->symName,name))
	lda	sp
	adc	#symName
	tax
	lda	sp+2
	adc	#^symName
	pha
	phx
	ph4	name
	jsr	Match
	tax
	bne	lb6
	lda	pass		if the symbol is resolved then
	cmp	#1
	bne	lb5
	lda	#pass1Resolved
	bra	lb5a
lb5	lda	#pass2Resolved
lb5a	ldy	#symFlag
	and	[sp],Y
	bne	lb10		  we don't need this segment
	lda	pass		if the symbol is requested then
	cmp	#1
	bne	lb5b
	lda	#pass1Requested
	bra	lb5c
lb5b	lda	#pass2Requested
lb5c	ldy	#symFlag
	and	[sp],Y
	beq	lb6
	inc	maybe		  we may need this segment
lb6	ldy	#2	    sp = sp->symNext;
	lda	[sp],Y
	tax
	lda	[sp]
	sta	sp
	stx	sp+2
	bra	lb4
lb7	lda	maybe	if maybe then
	beq	lb10
	inc	need	  we need this symbol

lb10	ret	2:need
;
;  Check the symbol
;
CheckSymbol anop
	lda	pass	if (pass == 1) {
	cmp	#1
	bne	cs1
	ldy	#symFlag	  if (sp->symFlag & pass1Requested)
	lda	[sp],Y
	bit	#pass1Requested
	beq	cs2
	bit	#pass1Resolved	    if (!(sp->symFlag & pass1Resolved)
	bne	cs2
	sec		      return true;
	rts
!			  }
!			else {pass == 2}
cs1	ldy	#symFlag	  if (sp->symFlag & pass2Requested)
	lda	[sp],Y
	bit	#pass2Requested
	beq	cs2
	bit	#pass2Resolved	    if (!(sp->symFlag & pass2Resolved)
	bne	cs2
	sec		      return true;
	rts
!			  }
cs2	clc		return false
	rts
	end

****************************************************************
*
*  PrintSymbol - print one symbol
*
*  Inputs:
*	r0 - pointer to the symbol table entry
*
****************************************************************
*
PrintSymbol private
	using Common
	using SymbolCommon

	ldy	#symVal+2	push the symbol value
	lda	[r0],Y
	pha
	dey
	dey
	lda	[r0],Y
	pha
	ldy	#symExp	if the symbol is an expression then
	lda	[r0],Y
	beq	lb0
	jsr	Evaluate	  evaluate the expression
	phx
	pha
lb0	ph2	#8	print the symbol value
	ph2	#0
	jsr	PrintHex
	ldy	#symPriv	print the global/private flag
	lda	[r0],Y
	beq	lb1
	puts	#' P '
	bra	lb2
lb1	puts	#' G '
lb2	ldy	#symSeg	print the load segment number
	lda	[r0],Y
	ldx	kflag
	beq	lb3
	ldx	express
	beq	lb3
	inc	A
lb3	pea	0
	pha
	ph2	#2
	ph2	#0
	jsr	PrintHex
	putc	#' '
	ldy	#symData	print the data area number
	lda	[r0],Y
	pea	0
	pha
	ph2	#2
	ph2	#0
	jsr	PrintHex
	putc	#' '
	add4	r0,#symName-1,r4	print the symbol name
	puts	[r4]
	rts
	end

****************************************************************
*
*  PrintSymbols - print the symbol table
*
*  Inputs:
*	symbols - print symbols flag
*
****************************************************************
*
PrintSymbols start
	using SymbolCommon
	using Common
;
;  Write the header
;
	lda	symbols	quit if the symbol flag is off
	bne	lb1
	rts

lb1	lda	list	write the header
	bne	lb2
	putcr
	putcr
lb2	puts	#'Global symbol table:',cr=t
	putcr
;
;  Initialize the symbol list pointer
;
	move4 alpha,r12
;
;  Print the symbols
;
	stz	col2	not doing column 2
ps1	jsr	FindFirstSymbol	find the next symbol to print
	bcc	ps5
	jsr	PrintSymbol	print it
	lda	col2	if this one is in column 1 then
	bne	ps4
	inc	col2	  col2 = true
	ldy	#symName	  get the length of the name
	lda	[r0],Y
	and	#$00FF
	sta	r4
	sec		  print the proper number of spaces
	lda	#27
	sbc	r4
	sta	r4
	beq	ps2
	bpl	ps3
ps2	lda	#1
	sta	r4
ps3	putc	#' '
	dec	r4
	bne	ps3
	bra	ps1	else
ps4	stz	col2	  col2 = false
	putcr		  write a CR
	jsr	CheckForPause	  check for early exit
	bra	ps1	next symbol

ps5	lda	col2	if in column 1 then
	bne	ps6
	putcr		  write the CR

ps6	putcr
	lda	list
	beq	ps7
	putcr
	putcr
ps7	rts
;
;  Local data
;
col2	ds	2
	end

****************************************************************
*
*  Reference - make a reference to a symbol
*
*  Inputs:
*	sp - pointer to the symbol name to reference
*
****************************************************************
*
Reference start
	using Common
	using SymbolCommon

!			{get a symbol to define}
fs1	ph4	sp	if ((r0 = FindSymbol(sp)) != NULL) {
	jsr	FindSymbol
	sta	r0
	stx	r0+2
	ora	r0+2
	beq	fs5
	ldy	#symFile	  if (r0->symFile == fileNumber)
	lda	[r0],Y
	cmp	fileNumber
	beq	fs6	    goto fs6;
fs2	ldy	#symFile	  while (r0->symFile != fileNumber) {
	lda	[r0],Y
	cmp	fileNumber
	beq	fs4
fs3	ldy	#2	    r0 = r0->symNext;
	lda	[r0]
	tax
	lda	[r0],Y
	sta	r2
	stx	r0
	ora	r0	    if (r0 == NULL)
	beq	fs5	      goto fs5;
	bra	fs2	    }
fs4	clc		  if (!Match(r0->symName,sp)) {
	lda	r0
	adc	#symName
	tax
	lda	r2
	adc	#^symName
	pha
	phx
	ph4	sp
	jsr	Match
	tax
	bne	fs3
	rts		    r0 = r0->symNext;
!			    if (r0 == NULL)
!			      goto fs5;
!			    goto fs2;
!			    }
!			  }
!			else
fs5	ph4	sp	  CreateSymbol(sp);
	jsr	CreateSymbol
	sta	r0
	stx	r2
fs6	ldy	#symFlag	set the pass 1 requested flag
	lda	[r0],Y
	ora	#pass1Requested
	sta	[r0],Y
	rts
	end

****************************************************************
*
*  Reference2 - note that pass2 has requested a symbol
*
*  Inputs:
*	sp - pointer to the symbol name to reference
*
****************************************************************
*
Reference2 start
	using Common
	using SymbolCommon

	ph4	sp	r0 = FindSymbol(sp);
	jsr	FindSymbol
	sta	r0
	stx	r2
	ldy	#symFile	if (r0->symFile != fileNumber) {
	lda	[r0],Y
	cmp	fileNumber
	beq	lb3
lb1	ldy	#symFile	  while (r0->symFile != fileNumber) {
	lda	[r0],Y
	cmp	fileNumber
	beq	lb2a
lb2	ldy	#2	    r0 = r0->symNext;
	lda	[r0],Y
	tax
	lda	[r0]
	sta	r0
	stx	r2
	bra	lb1
lb2a	clc		  if (!Match(r0->symName,sp)) {
	lda	r0
	adc	#symName
	tax
	lda	r2
	adc	#^symName
	pha
	phx
	ph4	sp
	jsr	Match
	tax
	bne	lb2
!			    r0 = r0->symNext;
!			    goto lb1;
lb3	anop		    }

	ldy	#symFlag	set the pass 2 requested flag
	lda	[r0],Y
	ora	#pass2Requested
	sta	[r0],Y
	rts
	end

****************************************************************
*
*  Unresolved - are there unresolved references?
*
*  Inputs:
*	pass - pass number
*
*  Outputs:
*	C - set if there are unresolved references, else clear
*
****************************************************************
*
Unresolved start
	using SymbolCommon
	using Common

	lda	pass	if pass1 then
	cmp	#1
	bne	lb1
	lda	#pass1Resolved	  resolved = pass1Resolved
	ldx	#pass1Requested	  requested = pass1Requested
	bra	lb2	else
lb1	lda	#pass2Resolved	  resolved = pass2Resolved
	ldx	#pass2Requested	  requested = pass2Requested
lb2	sta	resolved	endif
	stx	requested

	la	index,hashSize*4-4	for each hash bucket do
lb3	ldx	index	  for each symbol in the bucket do
	lda	table,X
	sta	r0
	sta	r4
	lda	table+2,X
	sta	r2
	sta	r6
	ora	r2
	beq	lb7
lb4	ldy	#symFlag	    if r0^.symFlag & requested then
	lda	[r0],Y
	bit	requested
	beq	lb6
	bit	resolved	      if not (r0^.symFlag & resolved) then
	bne	lb6
	ldy	#symPriv		if r0^.symPriv then
	lda	[r0]
	beq	lb5
	jsr	GlobalExists		  if GlobalExists then
	bcs	lb6		    skip request
lb5	sec			return true
	rts

lb6	ldy	#2	    r0 = r0^.symNext
	lda	[r0],Y
	tax
	lda	[r0]
	sta	r0
	stx	r2
	ora	r2	  next symbol in bucket
	bne	lb4
lb7	sec		next bucket
	lda	index
	sbc	#4
	sta	index
	bpl	lb3
	clc		no symbols needed
	rts
;
;  GlobalExists - see if a global symbol by the name of r0^.symName exists
;
GlobalExists anop

	move4 r4,r8	r8 = first sym in bucket
	add4	r0,#symName,r12	r12 = @r0^.symName
ge1	ldy	#symPriv	for each symbol do
	lda	[r8],Y	  if the symbol is global then
	bne	ge2
	clc		    if Match(r8^.symName,r12) then
	lda	r8
	adc	#symName
	tax
	lda	r10
	adc	#^symName
	pha
	phx
	ph4	r12
	jsr	Match
	tax
	bne	ge2
	sec		      return true
	rts
ge2	ldy	#2	  next symbol
	lda	[r8],Y
	tax
	lda	[r8]
	sta	r8
	stx	r10
	ora	r10
	bne	ge1
	clc		return false
	rts
;
;  Local data
;
index	ds	2	index into the hash table
resolved ds	2	resolved mask for this pass
requested ds	2	requested mask for this pass
	end
