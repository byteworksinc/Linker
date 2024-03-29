	keep	obj/out
	mcopy out.mac
****************************************************************
*
*  Ouput Module
*
*  This module handles writing the output file and locating the
*  proper load segment for code segments.
*
****************************************************************
	copy	directPage
****************************************************************
*
*  OutCommon - global data for the segment module
*
****************************************************************
*
OutCommon data
;
;  Constants
;
nameSize equ	10	size of a load segment name
dictGrowSize equ 4096	grow size for the dictionary buffer
dynGrowSize equ 1024	grow size for synamic segment buffer
;
;  global variables
;
expressSegment ds 2	express segment number
keepRefnum ds	2	keep file reference number
lastLoadNumber ds 4	last load number allocated
loadList ds	4	head of load segment list
loadNamePtr ds 4	pointer to the name of the load segment
;
;  Dynamic segment information
;
dynHandle ds	4	handle of the dynamic segment buffer
dynSize	ds	2	bytes left in the buffer
dynBuffSize ds 4	total memory in the segment buffer
dynStart ds	4	ptr to the start of the buffer
dynSegment ds	2	dynamic segment number
;
;  Current load segment information
;
loadNext ds	4	pointer to the next load segment
loadLast ds	4	pointer to the last load segment
loadPtr	ds	4	pointer to the storage location for this record
loadNumber ds	2	number of this load segment
loadType ds	2	load segment type
loadORG	ds	4	load segment origin
loadAlign ds	4	load segment aligment
loadBankSize ds 4	load segment bank size
loadName ds	nameSize	name of the load segment
loadSeg	ds	4	handle of the segment buffer
loadSegStart ds 4	start of the segment buffer
loadPC	ds	4	size of the load segment
loadOp	ds	4	op disp for the segment buffer
loadOpSt ds	4	op at start of current lConst
loadDict ds	4	handle of the dictionary buffer
loadDictStart ds 4	start of the dictionary buffer
loadDictSize ds 2	bytes left in the dictionary buffer
loadDp	ds	4	dp disp for the dictionary buffer
loadPass2 ds	2	are we ready for pass 2?

loadEnd	anop		end of the record

loadSize equ	loadEnd-loadNext	size of a load module record (even!)
	end

****************************************************************
*
*  AddEnd - add an end record to the dictionary
*
*  Inputs:
*	loadNext... - load segment record
*
****************************************************************
*
AddEnd	private
	using OutCommon

	lda	loadDictSize	make sure there is room in the dictionary
	bne	lb1
	jsr	ExpandDictBuffer
lb1	short M	save the end marker
	lda	#0
	sta	[dp]
	long	M
	inc4	dp	update dp
	dec	loadDictSize	update loadDictSize
	rts
	end

****************************************************************
*
*  AddSegment - add a segment to the expressload table
*
*  Inputs:
*	r8 - pointer to the segment to add
*	r4,r8 - expressload list pointers
*	expOffset - segment offset pointer
*	expMap - segment map pointer
*	expHeader - segment header map
*	expressSegment - express segment number
*
****************************************************************
*
AddSegment private
	using OutCommon

	jsr	MoveSegment	move the segment ot the new list

	sub4	expHeader,expOffset,r12	set the segment offset
	lda	r12
	sta	[expOffset]
	ldy	#6	zero the reserved words
	lda	#0
lb1	sta	[expOffset],Y
	dey
	dey
	bne	lb1
	add4	expOffset,#8	update the offset pointer

	ldy	#loadNumber-loadNext	get the old segment number
	lda	[r4],Y
	dec	A	set it's mapped segment
	asl	A
	tay
	lda	expressSegment
	sta	[expMap],Y
	ldy	#loadNumber-loadNext	set the new segment number
	sta	[r4],Y
	inc	expressSegment	update segment number

	add4	fMark,#69+5	allow for the segment header & lconst
	move4 fMark,mark1	set the lconst mark
	ldy	#loadPC-loadNext	find the segment size
	lda	[r4],Y
	sta	len1
	iny
	iny
	lda	[r4],Y
	sta	len1+2
	add4	fMark,len1	allow for the segment body
	move4 fMark,mark2	set the reloc mark
	ldy	#loadDp-loadNext	find the reloc size
	lda	[r4],Y
	sta	len2
	iny
	iny
	lda	[r4],Y
	sta	len2+2
	ora	len2	if len2 = 0 then
	bne	lb1a
	stz	mark2	  mark2 = 0
	stz	mark2+2
lb1a	add4	fMark,len2	allow for the dictionary
	inc4	fMark	allow for the end mark
	ldy	#loadBankSize-loadNext	set the bank size
	lda	[r4],Y
	sta	bankSize
	iny
	iny
	lda	[r4],Y
	sta	bankSize+2
	ldy	#loadType-loadNext	set the segment type
	lda	[r4],Y
	sta	kind
	ldy	#loadOrg-loadNext	set the origin
	lda	[r4],Y
	sta	org
	iny
	iny
	lda	[r4],Y
	sta	org+2
	ldy	#loadAlign-loadNext	set the alignment factor
	lda	[r4],Y
	sta	align
	iny
	iny
	lda	[r4],Y
	sta	align+2
	ldy	#loadNumber-loadNext	set the segment number
	lda	[r4],Y
	sta	segNum
	ldx	#nameSize-2	set the segment name
	ldy	#loadName-loadNext+nameSize-2
lb2	lda	[r4],Y
	sta	name,X
	dey
	dey
	dex
	dex
	bpl	lb2

	ldy	#segEnd-mark1-1	move the header to the express segment
	short M
lb3	lda	mark1,Y
	sta	[expHeader],Y
	dey
	bpl	lb3
	long	M
	add4	expHeader,#69	update the segment header pointer
	rts

mark1	ds	4	lConst file mark
len1	ds	4	lConst length
mark2	ds	4	reloc file mark
len2	ds	4	reloc length
	dc	i1'0'	undefined
	dc	i1'0'	label length
	dc	i1'4'	number length
	dc	i1'2'	version
bankSize ds	4	bank size
kind	ds	2	segment type
	dc	i'0'	undefined
org	ds	4	origin
align	ds	4	alignment
	dc	i1'0'	numsex
	dc	i1'0'	undefined
segNum	ds	2	segment number
	dc	i4'0'	segment entry
	dc	i'lname-len1'	disp to name
	dc	i'segend-mark1'	disp to body
lname	dc	10c' '	load name
	dc	i1'10'	segment name length
name	ds	10	segment name
segend	anop
	end

****************************************************************
*
*  CheckHeader - check the header parameters
*
*  Inputs:
*	segOrg - origin for this segment
*	segAlign - alignment for this segment
*	segType - type for this segment
*	segBanksize - banksize for this segment
*	loadType - load segment type
*	loadORG - load segment origin
*	loadAlign - load segment aligment
*	loadBankSize - load segment bank size
*
*  Outputs:
*	loadType - load segment type
*	loadORG - load segment origin
*	loadAlign - load segment aligment
*	loadBankSize - load segment bank size
*
****************************************************************
*
CheckHeader private
	using OutCommon
	using Common
;
;  Set the load segment type
;
	lda	loadPC	if at the start of the segment then
	ora	loadPC+2
	bne	st1
	lda	segType	  use segType
	and	#$FEFF
	sta	loadType
	bra	st4	else
st1	lda	segType	  or in the or flags
	and	#$1D00
	ora	loadType
	sta	loadType
	lda	segType	  mask out missing and flags
	and	#$E200
	ora	#$1DFF
	and	loadType
	sta	loadType
	lda	loadType	  get the type without flags
	and	#$007F
	sta	r0
	lda	segType
	and	#$007F
	cmp	r0	  if they do not match then
	beq	st4
	cmp	#1	    if seg is data then
	bne	st2
	lda	r0	      if not load in [code,init,dp] then
	beq	st4
	cmp	#$10
	beq	st4
	cmp	#$12
	beq	st4
err15	ph4	#0		flag segment conflict
	ph2	#15
	jsr	Error
	bra	st4
st2	cmp	#0	    else if seg in [code,init,dp] then
	beq	st3
	cmp	#$10
	beq	st3
	cmp	#$12
	bne	err15
st3	lda	r0	      if load = data then
	cmp	#1
	bne	err15
	lda	loadType		use seg type
	and	#$FF00
	sta	loadType
	lda	segType
	and	#$007F
	ora	loadType
	sta	loadType
!			    else flag the error
st4	anop		endif

	lda	bankOrg	if bankOrg then
	beq	st5
	lda	loadType	  bank org the program
	ora	#$0100
	sta	loadType
st5	anop		endif
;
;  Set the load segment origin (pass 1)
;
	lda	pass	branch if pass 2
	cmp	#2
	beq	or0
	lda	segOrg	skip if org is 0
	ora	segOrg+2
	jeq	or4

	lda	loadPC	if at the start of the segment then
	ora	loadPC+2
	ora	loadOrg
	ora	loadOrg+2
	bne	po1
	move4 segOrg,loadOrg	  loadOrg = segOrg
	bra	po3	else
po1	sub4	segOrg,loadOrg,r0	  update the pc
	lda	r2
	bmi	po3
	cmpl	r0,pc
	blt	po3
	move4 r0,pc
po3	anop		endif
	bra	or4
;
;  Set the load segment origin (pass 2)
;
or0	lda	segOrg	skip if org is 0
	ora	segOrg+2
	beq	or4

	lda	loadPC	if at the start of the segment then
	ora	loadPC+2
	ora	loadOrg
	ora	loadOrg+2
	bne	or1
	move4 segOrg,loadOrg	  loadOrg = segOrg
	bra	or3	else
or1	sub4	segOrg,loadOrg,r0	  update the pc
	sub4	r0,pc
	lda	r2
	bpl	or2	  if disp is negative then
	ph4	#0	    Error(NULL,3)
	ph2	#3
	jsr	Error
	bra	or3	  else
or2	lda	r0	    define DS bytes to fill space
	ora	r2
	beq	or3
	jsr	DefineDS
or3	anop		endif
	jsr	CheckAlignOrg	check for conflicts between align,org
or4	anop
;
;  Set the load segment alignment (pass 1)
;
	lda	pass	branch if pass 2
	cmp	#2
	beq	sa0
	lda	segAlign	skip if alignment is 0
	ora	segAlign+2
	jeq	sa4

	lda	loadPC	if at the start of the segment then
	ora	loadPC+2
	ora	loadAlign
	ora	loadAlign+2
	bne	la1
	move4 segAlign,loadAlign	  loadAlign = segAlign
	bra	la3	else
la1	cmpl	loadAlign,segAlign	  if loadAlign < segAlign then
	bge	la2
	move4	segAlign,loadAlign	    loadAlign = segAlign
la2	move4 segAlign,r0	  PrepareAlign(segAlign)
	jsr	PrepareAlign
la3	anop		endif
	bra	sa4
;
;  Set the load segment alignment (pass 2)
;
sa0	lda	segAlign	skip if alignment is 0
	ora	segAlign+2
	beq	sa4

	ph4	segAlign	make sure the align is a power of 2
	jsr	CheckAlign
	lda	loadPC	if at the start of the segment then
	ora	loadPC+2
	ora	loadAlign
	ora	loadAlign+2
	bne	sa1
	move4 segAlign,loadAlign	  loadAlign = segAlign
	bra	sa3	else
sa1	cmpl	loadAlign,segAlign	  if loadAlign < segAlign then
	bge	sa2
	move4	segAlign,loadAlign	    loadAlign = segAlign
sa2	move4 segAlign,r0	  DefineAlign(segAlign)
	jsr	DefineAlign
sa3	anop		endif
	jsr	CheckAlignOrg	check for conflicts between align,org
sa4	anop
;
;  Set the load segment bank size
;
	lda	pass	branch if pass 2
	cmp	#2
	beq	rt1
	lda	loadBankSize	if loadBanksize = 0
	ora	loadBanksize+2
	beq	bs1
	cmpl	segBanksize,loadBanksize	  or ((segBanksize < loadBanksize)
	bge	bs2	  and (segBanksize <> 0)) then
	lda	segBanksize
	ora	segBanksize+2
	beq	bs2
bs1	move4 segBankSize,loadBankSize	  loadBankSize = segBankSize
bs2	anop

rt1	rts
;
;  CheckAlignOrg - make sure the align and org do not conflict
;
CheckAlignOrg anop

	lda	loadAlign	if loadAlign <> 0 then
	ora	loadAlign+2
	beq	ca2
	sub4	loadAlign,#1,r0	  if loadOrg & (loadAlign-1) then
	lda	r0
	and	loadOrg
	bne	ca1
	lda	r2
	and	loadOrg+2
	beq	ca2
ca1	ph4	#0	    Error(NULL,21)
	ph2	#21
	jsr	Error
ca2	rts
	end

****************************************************************
*
*  CompactCInterseg - compact a cInterseg record
*
*  Inputs:
*	r0 - ptr to the cInterseg record
*	r4 - ptr to the end of the dictionary
*	dp - ptr to the next free byte in the dictionary
*
****************************************************************
*
CompactCInterseg private
	using Common
	using OutCommon
;
;  See if the record can be compacted
;
	lda	[r0]	length must be 2 or 3
	xba
	and	#$00FF
	cmp	#2
	beq	ck1
	cmp	#3
	jne	nc1
ck1	sta	length
	cmp	#3	if length = 3 then
	bne	ck2
	ldy	#2	  shift count must be 0
	lda	[r0],Y
	and	#$00FF
	jne	nc1
	stz	shift
	lda	#2	  set the record type
	sta	recordType
	ldy	#5	  set the segment number
	lda	[r0],Y
	and	#$00FF
	sta	segment
	bra	ck3	else {if length = 2 then}
ck2	ldy	#5	  segment number must be <= 12
	lda	[r0],Y
	and	#$00FF
	sta	segment
	cmp	#13
	jge	nc1
	ldx	express	  if express then
	beq	ck2a
	cmp	#12	    don't use 12, either
	jeq	nc1
ck2a	clc		  set the record type for shift 0
	lda	segment
	adc	#13
	sta	recordType
	ldy	#2	  shift must be 0 or -16
	lda	[r0],Y
	and	#$00FF
	sta	shift
	beq	ck3
	cmp	#$00F0
	jne	nc1
	add2	recordType,#12	  set the record type for shift -16
ck3	anop
;
;  Create a Super record
;
	lda	#13	create the record header
	cmp	loadDictSize	make sure there is room in the
	blt	sp1	 dictionary
	jsr	ExpandDictBuffer
sp1	add4	dp,#1,recordLengthPtr	save a pointer to the length field
	sub4	recordLengthPtr,loadDictStart
	short M	set the op code
	lda	#$F7
	sta	[dp]
	ldy	#5	set the super record type
	lda	recordType
	sta	[dp],Y
	long	M
	add4	dp,#6	skip the super record header
	sub2	loadDictSize,#6

	ldy	#4	set the segment offset page
	lda	[r0],Y
	and	#$00FF
	sta	page
	beq	pg3	if page <> 0 then
	sta	r12	  while (r12 := page) > $7F do
pg1	lda	r12
	cmp	#$80
	blt	pg2
	short M	    write a skip page for $7F pages
	lda	#$FF
	sta	[dp]
	long	M
	inc4	dp
	dec	loadDictSize
	sub2	r12,#$7F	    r12 -= $7F
	bra	pg1	  endwhile
pg2	lda	r12	  if r12 <> 0 then
	beq	pg3
	short M	    write a skip page for r12 pages
	ora	#$80
	sta	[dp]
	long	M
	inc4	dp
	dec	loadDictSize
pg3	anop
	move4 dp,r12	initialize the page counter
	short M
	lda	#$FF
	sta	[r12]
	long	M
	inc4	dp	skip the page counter
	dec	loadDictSize

	move4 r0,r8	for each dictionary record do
sp2	cmpl	r8,r4
	jeq	sp14
	lda	[r8]	  if it is a Reloc then
	and	#$00FF
	cmp	#$E2
	bne	sp3
	add4	r8,#11	    skip the record
	bra	sp2	    loop
sp3	cmp	#$75	  if it is a skipped cReloc or
	beq	sp4	    cReloc then
	cmp	#$F5
	bne	sp5
sp4	add4	r8,#7	    skip the record
	bra	sp2	    loop
sp5	cmp	#$76	  if it is a skipped cInterseg then
	bne	sp6
	add4	r8,#8	    skip the record
	bra	sp2	    loop
sp6	cmp	#$E3	  if it is an Interseg then
	bne	sp7
	add4	r8,#15	    skip the record
	bra	sp2	    loop
sp7	ldy	#2	  if the cInterseg is a different type
	short M	    then
	lda	[r8],Y
	cmp	shift
	bne	sp7a
	dey
	lda	[r8],Y
	cmp	length
	bne	sp7a
	cmp	#3
	beq	sp8
	ldy	#5
	lda	[r8],Y
	cmp	segment
	beq	sp8
sp7a	long	M	    skip the record
	add4	r8,#8
	brl	sp2	    loop

sp8	long	M	  make sure there is room in the
	lda	#5	   dictionary
	cmp	loadDictSize
	blt	sp9
	sub4	r12,loadDictStart
	jsr	ExpandDictBuffer
	add4	r12,loadDictStart
sp9	short M	  if r8^.page <> page then
	sec
	ldy	#4
	lda	[r8],Y
	sbc	page
	beq	sp13
	dec	A	    if r8^.page - page > 1 then
	beq	sp12
sp10	cmp	#$80	      record a page skip
	blt	sp11
	tax
	lda	#$FF
	sta	[dp]
	long	M
	inc4	dp
	dec	loadDictSize
	short M
	txa
	sec
	sbc	#$7F
	bra	sp10
sp11	cmp	#0
	beq	sp12
	ora	#$80
	sta	[dp]
	long	M
	inc4	dp
	dec	loadDictSize
	short M
sp12	ldy	#4	  record the new page
	lda	[r8],Y
	sta	page
	lda	#$FF	  set up a record count
	sta	[dp]
	long	M
	move4 dp,r12
	inc4	dp
	dec	loadDictSize
	short M
sp13	lda	[r12]	  increment the record count
	inc	A
	sta	[r12]
	ldy	#3	  set the offset
	lda	[r8],Y
	sta	[dp]
	long	M
	inc4	dp
	dec	loadDictSize
	short M	  mark the record
	lda	[r8]
	and	#$7F
	sta	[r8]
	long	M
	add4	r8,#8	  skip the record
	brl	sp2	  loop

sp14	anop		set the super record length
	add4	recordLengthPtr,loadDictStart,r12
	sub4	dp,r12,recordLengthPtr
	sub4	recordLengthPtr,#4
	ldy	#2
	lda	recordLengthPtr
	sta	[r12]
	lda	recordLengthPtr+2
	sta	[r12],Y
	rts
;
;  Cannot compact - put the unchanged record in the dictionary
;
nc1	lda	#8	make sure there is room in the
	cmp	loadDictSize	 dictionary
	blt	nc2
	jsr	ExpandDictBuffer
nc2	ldy	#6	move the record
nc3	lda	[r0],Y
	sta	[dp],Y
	dey
	dey
	bpl	nc3
	add4	dp,#8	skip the record
	sub2	loadDictSize,#8
	add4	r0,#8
	rts
;
;  Local data
;
recordLengthPtr ds 4	pointer to the super record length field
page	ds	2	current cReloc page number
length	ds	2	length of the cReloc fields
shift	ds	2	shift count
segment	ds	2	segment number
recordType ds	2	type of the Super record
	end

****************************************************************
*
*  CompactCReloc - compact a cReloc record
*
*  Inputs:
*	r0 - ptr to the cReloc record
*	r4 - ptr to the end of the dictionary
*	dp - ptr to the next free byte in the dictionary
*
****************************************************************
*
CompactCReloc private
	using OutCommon
;
;  See if the record can be compacted
;
	lda	[r0]	length must be 2 or 3
	xba
	and	#$00FF
	cmp	#2
	beq	ck1
	cmp	#3
	jne	nc1
ck1	sta	length
	ldy	#2	shift count must be 0
	lda	[r0],Y
	and	#$00FF
	jne	nc1
;
;  Create a Super record
;
	lda	#13	create the record header
	cmp	loadDictSize	make sure there is room in the
	blt	sp1	 dictionary
	jsr	ExpandDictBuffer
sp1	add4	dp,#1,recordLengthPtr	save a pointer to the length field
	sub4	recordLengthPtr,loadDictStart
	short M	set the op code
	lda	#$F7
	sta	[dp]
	ldy	#5	set the super record type
	lda	length
	and	#$01
	sta	[dp],Y
	long	M
	add4	dp,#6	skip the super record header
	sub2	loadDictSize,#6

	ldy	#4	set the segment offset page
	lda	[r0],Y
	and	#$00FF
	sta	page
	beq	pg3	if page <> 0 then
	sta	r12	  while (r12 := page) > $7F do
pg1	lda	r12
	cmp	#$80
	blt	pg2
	short M	    write a skip page for $7F pages
	lda	#$FF
	sta	[dp]
	long	M
	inc4	dp
	dec	loadDictSize
	sub2	r12,#$7F	    r12 -= $7F
	bra	pg1	  endwhile
pg2	lda	r12	  if r12 <> 0 then
	beq	pg3
	short M	    write a skip page for r12 pages
	ora	#$80
	sta	[dp]
	long	M
	inc4	dp
	dec	loadDictSize
pg3	anop
	move4 dp,r12	initialize the page counter
	short M
	lda	#$FF
	sta	[r12]
	long	M
	inc4	dp	skip the page counter
	dec	loadDictSize

	move4 r0,r8	for each dictionary record do
sp2	cmpl	r8,r4
	jeq	sp14
	lda	[r8]	  if it is a Reloc then
	and	#$00FF
	cmp	#$E2
	bne	sp3
	add4	r8,#11	    skip the record
	bra	sp2	    loop
sp3	cmp	#$75	  if it is a skipped cReloc then
	bne	sp4
	add4	r8,#7	    skip the record
	bra	sp2	    loop
sp4	cmp	#$F6	  if it is a cInterseg or skipped
	beq	sp5	   cInterseg then
	cmp	#$76
	bne	sp6
sp5	add4	r8,#8	    skip the record
	bra	sp2	    loop
sp6	cmp	#$E3	  if it is an Interseg then
	bne	sp7
	add4	r8,#15	    skip the record
	bra	sp2	    loop
sp7	ldy	#1	  if the cReloc is a different type then
	lda	[r8],Y
	cmp	length	    (checks length and shift=0)
	beq	sp8
	add4	r8,#7	    skip the record
	bra	sp2	    loop

sp8	lda	#5	  make sure there is room in the
	cmp	loadDictSize	   dictionary
	blt	sp9
	sub4	r12,loadDictStart
	jsr	ExpandDictBuffer
	add4	r12,loadDictStart
sp9	short M	  if r8^.page <> page then
	sec
	ldy	#4
	lda	[r8],Y
	sbc	page
	beq	sp13
	dec	A	    if r8^.page - page > 1 then
	beq	sp12
sp10	cmp	#$80	      record a page skip
	blt	sp11
	tax
	lda	#$FF
	sta	[dp]
	long	M
	inc4	dp
	dec	loadDictSize
	short M
	txa
	sec
	sbc	#$7F
	bra	sp10
sp11	cmp	#0
	beq	sp12
	ora	#$80
	sta	[dp]
	long	M
	inc4	dp
	dec	loadDictSize
	short M
sp12	ldy	#4	  record the new page
	lda	[r8],Y
	sta	page
	lda	#$FF	  set up a record count
	sta	[dp]
	long	M
	move4 dp,r12
	inc4	dp
	dec	loadDictSize
	short M
sp13	lda	[r12]	  increment the record count
	inc	A
	sta	[r12]
	ldy	#3	  set the offset
	lda	[r8],Y
	sta	[dp]
	long	M
	inc4	dp
	dec	loadDictSize
	short M	  mark the record
	lda	[r8]
	and	#$7F
	sta	[r8]
	long	M
	add4	r8,#7	  skip the record
	brl	sp2	  loop

sp14	anop		set the super record length
	add4	recordLengthPtr,loadDictStart,r12
	sub4	dp,r12,recordLengthPtr
	sub4	recordLengthPtr,#4
	ldy	#2
	lda	recordLengthPtr
	sta	[r12]
	lda	recordLengthPtr+2
	sta	[r12],Y
	rts
;
;  Cannot compact - put the unchanged record in the dictionary
;
nc1	lda	#8	make sure there is room in the
	cmp	loadDictSize	 dictionary
	blt	nc2
	jsr	ExpandDictBuffer
nc2	ldy	#6	move the record
nc3	lda	[r0],Y
	sta	[dp],Y
	dey
	dey
	bpl	nc3
	add4	dp,#7	skip the record
	sub2	loadDictSize,#7
	add4	r0,#7
	rts
;
;  Local data
;
recordLengthPtr ds 4	pointer to the super record length field
page	ds	2	current cReloc page number
length	ds	2	length of the cReloc fields
	end

****************************************************************
*
*  CompactSegment - compact a single segment
*
*  Inputs:
*	LoadNext... - segment record
*
****************************************************************
*
CompactSegment private
	using OutCommon
	using Common

	lda	loadDict	if there is no dictionary then
	ora	loadDict+2
	bne	lb1
	rts		  return

lb1	move4 loadDict,dictHandle	save the dictionary handle
	move4 loadDictStart,r0	get a pointer to the dictionary
	move4 dp,r4	get a pointer to the dictionary end
	stz	loadDictStart	zero the current load dictionary
	stz	loadDictStart+2
	stz	loadDict
	stz	loadDict+2
	stz	loadDp
	stz	loadDp+2
	stz	dp
	stz	dp+2
	stz	loadDictSize
	stz	loadDictSize+2
lb2	cmpl	r0,r4	for each dictionary entry do
	jeq	lb12
	lda	[r0]	  if r0^ = cReloc then
	and	#$00FF
	cmp	#$00F5
	bne	lb3
	jsr	CompactCReloc	    compact the record
	bra	lb2	    loop
lb3	cmp	#$75	  if r0^ = skipped cReloc then
	bne	lb4
	add4	r0,#7	    skip the record
	bra	lb2	    loop
lb4	cmp	#$E2	  if r0^ = Reloc then
	bne	lb7
	lda	#12	    make sure there is room in the
	cmp	loadDictSize	     dictionary
	blt	lb5
	jsr	ExpandDictBuffer
lb5	ldy	#10	    move the record
lb6	lda	[r0],Y
	sta	[dp],Y
	dey
	dey
	bpl	lb6
	add4	dp,#11	    skip the record
	sub2	loadDictSize,#11
	add4	r0,#11
	bra	lb2	    loop
lb7	cmp	#$F6	  if r0^ = cInterseg then
	bne	lb8
	jsr	CompactCInterseg	    compact the record
	bra	lb2	    loop
lb8	cmp	#$76	  if r0^ = skipped cInterseg then
	bne	lb9
	add4	r0,#8	    skip the record
	brl	lb2	    loop
!	(condition must be true)	  if r0^ = Interseg then
lb9	lda	#16	    make sure there is room in the
	cmp	loadDictSize	     dictionary
	blt	lb10
	jsr	ExpandDictBuffer
lb10	ldy	#14	    move the record
lb11	lda	[r0],Y
	sta	[dp],Y
	dey
	dey
	bpl	lb11
	add4	dp,#15	    skip the record
	sub2	loadDictSize,#15
	add4	r0,#15
	brl	lb2	    loop

lb12	ph4	dictHandle	free the old dictionary buffer
	_DisposeHandle
	rts

dictHandle ds	4	old dictionary handle
	end

****************************************************************
*
*  CreateDynamicSegment - create the dynamic segment (if any)
*
*  Inputs:
*	dynSegment - dynmaic segment number; 0 if none
*	dynStart - start of the dynamic segment
*	dy - next byte in the buffer
*
****************************************************************
*
CreateDynamicSegment private
	using OutCommon

	lda	dynSegment	quit if there is no segment
	bne	lb0
	rts
lb0	lda	dynSize	write the trailing 4 bytes
	cmp	#4
	bge	lb1
	jsr	ExpandDynamicBuffer
lb1	ldy	#2
	lda	#0
	sta	[dy]
	sta	[dy],Y
	add4	dy,#4
	sub4	dy,dynStart,r0	set the size of the lConst
	sub4	r0,#5
	move4 dynStart,r4
	lda	#$F2
	sta	[r4]
	ldy	#1
	lda	r0
	sta	[r4],Y
	iny
	iny
	lda	r2
	sta	[r4],Y

	move	#0,loadNext,#loadSize	clear all entries
	lda	dynSegment	set the segment number
	sta	loadNumber
	lda	#2	set the segment type
	sta	loadType
	lla	loadBankSize,$10000	set the load segment bank size
	move	jumpName,loadName,#nameSize set the segment name
	move4 dynHandle,loadSeg	set the segment buffer handle
	move4 dynStart,loadSegStart	set the load segment start
	sub4	dy,dynStart,loadPc	set the segment size
	sub4	loadPc,#5
	move4 loadPc,pc
	move4 dy,op	set the segment pointer
	sub4	dy,dynStart,loadOp	set the segment displacement
	inc	loadPass2	ready for pass 2
	stz	dp	no dictionary pointer
	stz	dp+2
	jsr	SaveSegment
	rts

jumpName dc	c'~JumpTable'
	end

****************************************************************
*
*  CreateFile - make sure an output file exists
*
*  Inputs:
*	kname - keep file name
*
****************************************************************
*
CreateFile private
	using OutCommon

	move4 kname,giPathname	see if a file exists
	OSGet_File_Info giRec
	bcs	lb1
	lda	giFiletype	yes -> make sure it is an OBJ file
	cmp	#$B3
	blt	err7
	cmp	#$BF+1
	bge	err7
	rts

lb1	move4 kname,crPathname	no file exists, so create one
	OSCreate crRec
	bcs	err12
	rts

err12	lda	#12	file write error
	jmp	TermError

err7	lda	#7	Could not overwrite existing file
	jmp	TermError
;
;  Local data
;
giRec	dc	i'3'	GetFileInfo record
giPathname ds	4
	ds	2
giFiletype ds	2

crRec	dc	i'4'
crPathname ds	4
	dc	i'$C3'
	dc	i'EXE'
	dc	i4'$0100'
	end

****************************************************************
*
*  DecimalVariable - convert an OS string to a decimal value
*
*  Inputs:
*	r0 - pointer to the shell variable value
*
*  Outputs:
*	C - set if all characters are digits, else clear
*	X-A - value (if C set); least sig. 32 bits only
*
****************************************************************
*
DecimalVariable private

	lda	[r0]	get a loop counter
	sta	r12
	stz	r8	set the initial value
	stz	r10
	add4	r0,#2,r4	get a character pointer

lb1	lda	[r4]	if the character is not a digit then
	and	#$00FF	  return false
	cmp	#'0'
	blt	lb3
	cmp	#'9'+1
	bge	lb3
	mul4	r8,#10	r8 = r8*10 + value(r4^)
	lda	[r4]
	and	#$000F
	clc
	adc	r8
	sta	r8
	bcc	lb2
	inc	r10
lb2	inc4	r4	loop
	dec	r12
	bne	lb1

	lda	r8	return value
	ldx	r10
	sec
	rts

lb3	clc		return false
	rts
	end

****************************************************************
*
*  DoCompact - compact the dictionaries
*
****************************************************************
*
DoCompact private
	using OutCommon
	using Common

	jsr	SaveSegment	save the current segment
	lda	#1	set the segment number
	sta	segment
lb1	move4 loadList,r0	find the proper segment
lb2	ldy	#loadNumber-loadNext
	lda	[r0],Y
	cmp	segment
	bne	lb3
	jsr	GetSegment	get the segment
	jsr	CompactSegment	compact the segment
	jsr	SaveSegment	save the segment
	inc	segment	next segment
	bra	lb1	start the search over

lb3	ldy	#2	next segment pointer
	lda	[r0],Y
	tax
	lda	[r0]
	sta	r0
	stx	r2
	ora	r2
	bne	lb2

	move4 loadList,r0	done - get a segment back
	jsr	GetSegment
	rts		return

segment	ds	2	segment number
	end

****************************************************************
*
*  DynamicCheck - if any segment is dynamic, set up a jump table
*
*  Inputs:
*	loadList - list of current load segments
*	loadNext... - active load segment
*	lastLoadNumber - last load number allocated
*
*  Outputs: (if there is a dynamic segment)
*	dynHandle - handle of the synamic segment
*	dynStart - start of the dynamic segment
*	dynSize - size of the dynamic segment
*	dy - pointer to the next spot in the dynamic segment
*	dynSegment - dynamic segment number
*
****************************************************************
*
DynamicCheck start
	using OutCommon
;
;  See if any segment is dynamic
;
	lda	loadType	check the current segment
	bmi	pr1
	move4 loadList,r0	check the segments in the segment list
cs1	lda	r0
	ora	r2
	beq	cs2
	ldy	#loadType-loadNext
	lda	[r0],Y
	bmi	pr1
	ldy	#2
	lda	[r0],Y
	tax
	lda	[r0]
	sta	r0
	stx	r2
	bra	cs1

cs2	rts
;
;  Prepare for dynamic segment output
;
pr1	stz	dy	zero the pointer
	stz	dy+2
	stz	dynSize	zero the size
	stz	dynBuffSize	no memory in the buffer
	stz	dynBuffSize+2
	stz	dynHandle	no handle, yet
	stz	dynHandle+2
	stz	dynStart	initialize the field
	stz	dynStart+2
	jsr	ExpandDynamicBuffer	get an initial buffer
	add4	dy,#5	create space for the lConst opcode
	ldy	#6	insert the 8 leading zeros
	lda	#0
pr2	sta	[dy],Y
	dey
	dey
	bpl	pr2
	sub2	dynSize,#13
	add4	dy,#8
	lda	lastLoadNumber	get a segment number
	inc	A
	sta	lastLoadNumber
	sta	dynSegment
	rts
	end

****************************************************************
*
*  ExpandDictBuffer - expand the dictionary buffer
*
*  Inputs:
*	loadDict - handle of the dictionary buffer, nil for none
*	loadDictSize - size of the dictionary buffer
*
*  Outputs:
*	loadDict - handle of the dictionary buffer
*	loadDictStart - start of the dictionary buffer
*	loadDictSize - size of the dictionary buffer
*	dp - start of the dictionary buffer
*
*  Notes:
*	Other than dp, direct page is not disturbed.
*
****************************************************************
*
ExpandDictBuffer start
	using Common
	using OutCommon

	lda	loadDict	if there is no buffer then
	ora	loadDict+2
	bne	lb1
	la	loadDictSize,dictGrowSize  set the dictionary size
	pha		  get a new buffer
	pha
	ph4	#dictGrowSize
	ph2	userID
	ph2	#$8000
	ph4	#0
	_NewHandle
	jcs	oom
	pl4	loadDict
	stz	loadDP	  loadDP = 0
	stz	loadDP+2
	brl	lb2	else
lb1	sub4	dp,loadDictStart,loadDP	  get the displacement
	ph4	loadDict	  unlock the handle
	_HUnlock
	pha		  get the current handle size
	pha
	ph4	loadDict
	_GetHandleSize
	clc		  set the new size
	lda	1,S
	adc	#dictGrowSize
	sta	1,S
	lda	3,S
	adc	#^dictGrowSize
	sta	3,S
	ph4	loadDict	  expand the buffer
	_SetHandleSize
	bcs	oom
	ph4	loadDict	  lock the handle
	_HLock
	add2	loadDictSize,#dictGrowSize update the free space
lb2	anop		endif

	move4 loadDict,dp	dereference the handle
	ldy	#2
	lda	[dp]
	sta	loadDictStart
	lda	[dp],Y
	sta	loadDictStart+2
	add4	loadDictStart,loadDP,dp	set dp
	rts

oom	lda	#5
	jmp	TermError
	end

****************************************************************
*
*  ExpandDynamicBuffer - expand the dynamic segment buffer
*
*  Inputs:
*	dynHandle - handle of the buffer, nil for none
*	dynBuffSize - size of the buffer
*	synSize - bytes left in the current buffer
*	dy - pointer to the current buffer
*	dynStart - start of the buffer
*
*  Outputs:
*	dynHandle - handle of the buffer, nil for none
*	dynBuffSize - size of the buffer
*	synSize - bytes left in the current buffer
*	dy - pointer to the current buffer
*	dynStart - start of the buffer
*
****************************************************************
*
ExpandDynamicBuffer private
	using Common
	using OutCommon

	lda	dynHandle	if there is no buffer then
	ora	dynHandle+2
	bne	lb1
	pha		  get a new buffer
	pha
	ph4	#dynGrowSize
	ph2	userID
	ph2	#$8000
	ph4	#0
	_NewHandle
	jcs	oom
	pl4	dynHandle
	lla	dynBuffSize,dynGrowSize	  set the size
	bra	lb2	else
lb1	ph4	loadDict	  unlock the handle
	_HUnlock
	add4	dynBuffSize,#dynGrowSize	  get the new size
	ph4	dynBuffSize	  set the new size
	ph4	dynHandle	  expand the buffer
	_SetHandleSize
	bcs	oom
	ph4	dynHandle	  lock the handle
	_HLock
lb2	anop		endif

	add2	dynSize,#dynGrowSize	update the free space
	sub4	dy,dynStart	convert dy to a displacement
	move4 dynHandle,r0	dereference the handle
	ldy	#2
	lda	[r0]
	sta	dynStart
	lda	[r0],Y
	sta	dynStart+2
	add4	dy,dynStart	set dy
	rts

oom	lda	#5
	jmp	TermError
	end

****************************************************************
*
*  ExpressLoad - Handle express loading
*
*  Inputs:
*	express - is the program express loaded?
*
****************************************************************
*
ExpressLoad private
	using OutCommon
	using Common
;
;  Sort the segments by number
;
	lda	#1	set the initial segment number
	sta	expressSegment
	stz	r0	nothing in the output list
	stz	r2
sn1	move4 loadList,r8	for each segment number
sn2	lda	r8	  for each segment
	ora	r10
	beq	sn4
	ldy	#loadNumber-loadNext	    if this is the right segment then
	lda	[r8],Y
	cmp	expressSegment
	bne	sn3
	jsr	MoveSegment	      move the segment to the new list
	bra	sn4
sn3	ldy	#2	  next segment
	lda	[r8],Y
	tax
	lda	[r8]
	sta	r8
	stx	r10
	bra	sn2
sn4	inc	expressSegment	next segment number
	lda	loadList
	ora	loadList+2
	bne	sn1
	move4 r0,loadList	replace the list
;
;  Build the expressload segment
;
	pha		get memory for the lConst record
	pha
	mul4	lastLoadNumber,#79,r0
	add4	r0,#7
	ph4	r0
	ph2	userID
	ph2	#$8000
	ph4	#0
	_NewHandle
	bcc	ex1
	lda	#5
	jmp	TermError
ex1	pl4	r0	recover the handle
	ldy	#2	recover the start pointer
	lda	[r0]
	sta	expStart
	lda	[r0],Y
	sta	expStart+2
	add4	expStart,#6,expOffset	set the initial offset pointer
	mul4	lastLoadNumber,#8,r0	set the initial map pointer
	add4	r0,expOffset,expMap
	mul4	lastLoadNumber,#2,r0	set the initial header pointer
	add4	r0,expMap,expHeader
	lda	#2	start with segment 2
	sta	expressSegment
	mul4	lastLoadNumber,#79,fMark set the initial file mark
	add4	fMark,#67+5+4+2+1
;
;  Add all static segments to the expressload segment
;
	stz	r0	nothing in the output list
	stz	r2
st1	move4 loadList,r8	for each segment do
st2	lda	r8
	ora	r10
	beq	dy1
	ldy	#loadType-loadNext	  if it is a static segment then
	lda	[r8],Y
	bmi	st3
	jsr	AddSegment	    add the segment to the express list
	bra	st1	    loop
st3	ldy	#2	next segment
	lda	[r8],Y
	tax
	lda	[r8]
	sta	r8
	stx	r10
	bra	st2
;
;  Add all remaining segments to the expressload segment
;
dy1	lda	loadList	for each segment do
	ora	loadList+2
	beq	dn1
	move4 loadList,r8	  add the segment to the express list
	jsr	AddSegment
	bra	dy1
;
;  Adjust the dynamic segment number
;
dn1	anop
	lda	dynSegment	get the old segment number
	beq	fe1	skip if there isn't one
	dec	A	set its mapped segment
	asl	A
	tay
	lda	[expMap],Y
	sta	dynSegment	set the new segment number
;
;  Finish off the express record
;
fe1	move4 r0,loadList	replace the load list pointer
	lda	#0	put and end mark after the record
	short M
	sta	[expHeader]
	long	M
	ldy	#2	zero the reserved space for the link
	sta	[expStart]
	sta	[expStart],Y
	ldy	#4	set the number of segments
	lda	lastLoadNumber
	dec	A
	sta	[expStart],Y
;
;  Write the expressload segment
;
!			set the size of the segment
	sub4	expHeader,expStart,length
	add4	length,#headerEnd-header+1,byteCnt
	move4 length,length2	set the length of the lCost record

	lda	keepRefnum	write the header
	sta	wrRefnum
	sta	wsRefnum
	OSWrite wrRec
	bcs	lb1
	move4 expStart,wsBuff	write the body
	add4	length,#1,wsLen
	OSWrite wsRec
	bcs	lb1
	rts

lb1	lda	#12
	jmp	TermError
;
;  Local data
;
wsRec	dc	i'4'	write the body
wsRefnum ds	2
wsBuff	ds	4
wsLen	ds	4
	ds	4

wrRec	dc	i'4'	write the segment header
wrRefnum ds	2
	da	'header'
	dc	i4'headerEnd-header'
	ds	4

header	anop		segment header model
byteCnt	ds	4	length of the segment, in bytes
	dc	i4'0'	reserved space at the end of the segment
length	ds	4	length of the segment
	dc	i1'0'	undefined
	dc	i1'0'	label length
	dc	i1'4'	number length
	dc	i1'2'	OMF version
	dc	i4'$10000'	bank size
	dc	i'$8001'	segment kind
	dc	i'0'	undefined
	dc	i4'0'	origin
	dc	i4'0'	alignment factor
	dc	i1'0'	numSex; set to LSB first
	dc	i1'0'	undefined
	dc	i'1'	segment number
	dc	i4'0'	entry point
	dc	i'ldName-header'	disp to the segment name
	dc	i'data-header'	disp to the segment image
ldName	dc	10c' '	load segment name; unused in load segs
	dc	i1'12'	length of the segment name
	dc	c'~ExpressLoad'	segment name
data	dc	i1'$F2'	lConst header
length2	ds	4
headerEnd anop
	end

****************************************************************
*
*  FindLoadSegment - find the proper load segment for a file
*
*  Inputs:
*	loadNamePtr - pointer to the load segment name
*	segOrg - origin for this segment
*	segAlign - alignment for this segment
*	segType - type for this segment
*	segBanksize - banksize for this segment
*
*  Outputs:
*	loadNumber - load segment number
*
*  Additional outputs if pass = 2:
*	op - pointer to the next output segment byte
*	dp - pointer to the next dictionary segment byte
*	dictSize - remaining bytes in the dictionary segment
*	dictStart - pointer to the first byte in the dictionary
*
****************************************************************
*
FindLoadSegment start
	using Common
	using OutCommon

	lda	loadNumber	if there is an existing segment then
	bmi	lb3
	move4 loadNamePtr,r0	  if we need this segment then
	ldy	#nameSize-2
lb1	lda	[r0],Y
	cmp	loadName,Y
	bne	lb2
	dey
	dey
	bpl	lb1
	move4 pc,loadPC	    save the program counter
	jsr	Pass2Prep	    make sure we are "pass 2 ready"
	jsr	CheckHeader	    check header parameters
	rts		    return
lb2	jsr	SaveSegment	  save the current segment info

lb3	move4 loadList,r0	find the correct segment
	move4 loadNamePtr,r8
lb4	lda	r0
	ora	r2
	beq	lb6
	add4	r0,#loadName-loadNext,r4
	ldy	#nameSize-2
lb5	lda	[r4],Y
	cmp	[r8],Y
	bne	lb5a
	dey
	dey
	bpl	lb5
	jsr	GetSegment	recover the segment
	jsr	CheckHeader	check header parameters
	rts

lb5a	ldy	#2	next segment
	lda	[r0],Y
	tax
	lda	[r0]
	sta	r0
	stx	r2
	bra	lb4

lb6	jsr	NewSegment	create a new segment
	jsr	CheckHeader
	rts
	end

****************************************************************
*
*  FinishLconst - finish the current lconst record
*
*  Inputs:
*	op - current ptr in buffer
*	opst - op at the start of this record
*
*  Outputs:
*	opst - set to op
*
****************************************************************
*
FinishLConst start

	sub4	op,opSt,r0
	sub4	r0,#5
	lda	#$F2
	sta	[opSt]
	ldy	#1
	lda	r0
	sta	[opSt],Y
	lda	r2
	iny
	iny
	sta	[opSt],Y
	move4 op,opSt
	rts
	end

****************************************************************
*
*  GetSegment - get a segment from storage
*
*  Inputs:
*	r0 - pointer to the segment to get
*	pass - current pass number
*
*  Outputs:
*	loadNext... - filled in
*	pc - set to last value
*	op,opst,dp,dpSize - set as appropriate
*
****************************************************************
*
GetSegment private
	using Common
	using OutCommon

	ldy	#loadSize-2	recover the segment
lb0	lda	[r0],Y
	sta	loadNext,Y
	dey
	dey
	bpl	lb0
	lda	loadSeg	if there is a segment buffer then
	ora	loadSeg+2
	beq	lb1
	ph4	loadSeg	  lock the buffer
	_HLock
	move4 loadSeg,r0	  dereference the segment
	ldy	#2
	lda	[r0]
	sta	loadSegStart
	lda	[r0],Y
	sta	loadSegStart+2
	add4	loadOp,loadSegStart,op	  set the segment pointer
	add4	loadOpSt,loadSegStart,opst set the lConst pointer
	bra	lb3	else if pass = 2 then
lb1	lda	pass
	cmp	#2
	bne	lb3
	move4 loadPC,pc	  save the program counter
	jsr	Pass2Prep	  prepare for pass2
lb3	anop		endif
	move4 loadPC,pc	reset the program counter
	lda	loadDict	if there is a dictionary then
	ora	loadDict+2
	beq	lb4
	ph4	loadDict	  lock the buffer
	_HLock
	move4 loadDict,r0	  dereference the segment
	ldy	#2
	lda	[r0]
	sta	loadDictStart
	lda	[r0],Y
	sta	loadDictStart+2
	add4	loadDp,loadDictStart,dp	  set the segment pointer

lb4	lda	loadLast	break the back link
	ora	loadLast+2
	beq	lb5
	move4 loadLast,r0
	ldy	#2
	lda	loadNext
	sta	[r0]
	lda	loadNext+2
	sta	[r0],Y
	bra	lb6
lb5	move4 loadNext,loadList
lb6	lda	loadNext	break the forward link
	ora	loadNext+2
	beq	lb7
	move4 loadNext,r0
	ldy	#loadLast-loadNext
	lda	loadLast
	sta	[r0],Y
	iny
	iny
	lda	loadLast+2
	sta	[r0],Y
lb7	rts
	end

****************************************************************
*
*  HexVariable - convert an OS string to a value
*
*  Inputs:
*	r0 - pointer to the shell variable value
*
*  Outputs:
*	C - set if all characters are hex digits, else clear
*	X-A - value (if C set); least sig. 32 bits only
*
****************************************************************
*
HexVariable private

	lda	[r0]	get a loop counter
	sta	r12
	stz	r8	set the initial value
	stz	r10
	add4	r0,#2,r4	get a character pointer

	lda	[r4]	the first char must be '$'
	and	#$00FF
	cmp	#'$'
	bne	lb3
	inc4	r4
	dec	r12
	beq	lb2a
lb1	lda	[r4]	if the character is not a digit then
	and	#$00FF	  return false
	cmp	#'0'
	blt	lb3
	cmp	#'9'+1
	blt	lb1a
	and	#$005F
	cmp	#'F'+1
	bge	lb3
	cmp	#'A'
	blt	lb3
lb1a	pha		r8 = r8*16 + value(r4^)
	mul4	r8,#16
	pla
	and	#$007F
	cmp	#'9'+1
	blt	lb1b
	sbc	#7
lb1b	and	#$000F
	clc
	adc	r8
	sta	r8
	bcc	lb2
	inc	r10
lb2	inc4	r4	loop
	dec	r12
	bne	lb1

lb2a	lda	r8	return value
	ldx	r10
	sec
	rts

lb3	clc		return false
	rts
	end

****************************************************************
*
*  InitOut - initialize the output buffer variables
*
****************************************************************
*
InitOut	start
	using Common
	using OutCommon

	lda	#-1	no load segment is active
	sta	loadNumber
	stz	lastLoadNumber	no load segments created
	stz	loadList	no load segments in the list
	stz	loadList+2
	stz	dynSegment	no dynamic segment
	rts
	end

****************************************************************
*
*  IsDynamic - is the segment dynamic?
*
*  Inputs:
*	A - segment to check
*
*  Outputs:
*	C - set if the segment is dynamic, else clear
*
****************************************************************
*
IsDynamic start
	using OutCommon

	cmp	loadNumber	if A = current segment then
	bne	lb2
	lda	loadType	  return dynamic(loadType)
lb0	bpl	lb1
	sec
	rts
lb1	clc
	rts

lb2	sta	r4	save the segment number
	move4 loadList,r0	for each segment in the list do
lb3	lda	r0
	ora	r2
	beq	lb1
	ldy	#loadNumber-loadNext	  if this is the correct segment then
	lda	[r0],Y
	cmp	r4
	bne	lb4
	ldy	#loadType-loadNext	    return dynamic(r0^.loadType)
	lda	[r0],Y
	bra	lb0
lb4	ldy	#2	  next segment
	lda	[r0],Y
	tax
	lda	[r0]
	sta	r0
	stx	r2
	bra	lb3
	end

****************************************************************
*
*  JumpTable - create/reuse a jump table entry
*
*  Inputs:
*	expValue - disp into the dynamic segment
*	expSegment - dynamic segment number
*	shiftFlag - shift flag for the expression
*
*  Outputs:
*	expValue - disp into the segment buffer
*	expSegment - dynamic segment number
*
****************************************************************
*
JumpTable start
	using OutCommon
	using ExpCommon
userID	equ	0	disp to the user ID field
file	equ	2	disp to the file number
seg	equ	4	disp to the segment number
offset	equ	6	disp to the offset (expr value)
jsl	equ	10	disp to the jsl to the loader

length	equ	14	length of a record

header	equ	8+5	bytes in the buffer header

	add4	dynStart,#header,r0	check for an existing entry
lb1	cmpl	r0,dy
	bge	lb3
	ldy	#seg
	lda	[r0],Y
	cmp	expSegment
	bne	lb2
	ldy	#offset
	lda	[r0],Y
	cmp	expValue
	bne	lb2
	iny
	iny
	lda	[r0],Y
	cmp	expValue+2
	beq	lb5
lb2	add4	r0,#length
	bra	lb1

lb3	lda	dynSize	no existing entry; create a new one
	cmp	#length
	bge	lb4
	jsr	ExpandDynamicBuffer
lb4	move4 dy,r0
	add4	dy,#length
	lda	#0
	sta	[r0]
	ldy	#file
	lda	#1
	sta	[r0],Y
	ldy	#seg
	lda	expSegment
	sta	[r0],Y
	ldy	#offset
	lda	expValue
	sta	[r0],Y
	iny
	iny
	lda	expValue+2
	sta	[r0],Y
	ldy	#jsl
	lda	#$0022
	sta	[r0],Y
	iny
	iny
	lda	#0
	sta	[r0],Y

lb5	sub4	r0,dynStart,expValue	set the new expValue
	add4	expValue,#jsl-5
	lda	dynSegment	set the new segment value
	sta	expSegment
	lda	shiftFlag	check for an illegal shift
	beq	lb6
	ph4	#0
	ph2	#20
	jsr	Error
lb6	rts
	end

****************************************************************
*
*  KeepFile - write the segments to the file
*
*  Inputs:
*	loadList - head of the load list
*	kname - pointer to the output name
*
****************************************************************
*
KeepFile start
	using Common
	using OutCommon

	lda	#1	set the segment number
	sta	segment
	jsr	CreateFile	create/check the file
	move4 kname,opPathname	open the file
	OSOpen opRec
	bcc	lb0
err12	lda	#12
	jmp	TermError

lb0	lda	opRefnum	save the keep refnum
	sta	keepRefnum
	sta	clRefnum
	sta	mkRefnum
	OSSet_EOF mkRec	erase the old file contents
	bcs	err12
	lda	compact	if compact then
	beq	lb0a
	jsr	DoCompact	  compact the dictionaries
lb0a	jsr	SaveSegment	save the current segment
	jsr	CreateDynamicSegment	create the dynamic segment
	lda	express	if express then
	beq	lb1
	jsr	ExpressLoad	  write the expressload segment (if any)
	inc	segment	  first segment to process is 2

lb1	move4 loadList,r0	find the proper segment
lb2	ldy	#loadNumber-loadNext
	lda	[r0],Y
	cmp	segment
	beq	lb3
	ldy	#2
	lda	[r0],Y
	tax
	lda	[r0]
	sta	r0
	stx	r2
	ora	r2
	bne	lb2
	OSClose clRec	none left -- close the file
	move4 loadList,r0	get a segment back
	jsr	GetSegment
	jsr	SetFileType
	rts

lb3	jsr	GetSegment	get and lock the segment
	jsr	AddEnd	add an end record to the dictionary
	jsr	FinishLConst	finish the current lconst record
	lda	express	if express then
	beq	lb4
	jsr	Remap	  remap the segment numbers
	bra	lb5	else
lb4	jsr	OptimizeDS	  optimize the DS, LConst records
lb5	jsr	WriteHeader	write the header
	jsr	WriteBody	write the segment image
	jsr	WriteDictionary	write the dictionary
	jsr	SaveSegment	save the segment
	inc	segment	next segment
	bra	lb1
;
;  Local data
;
clRec	dc	i'1'	close record
clRefnum ds	2

opRec	dc	i'2'	open record
opRefnum ds	2
opPathname ds	4

mkRec	dc	i'3'	set mark record
mkRefnum ds	2
	dc	i'0'
	dc	i4'0'

segment	ds	2	segment number
	end

****************************************************************
*
*  MoveSegment - move a segment record
*
*  Inputs:
*	r0 - pointer to the head of the new list
*	r4 - pointer to the last record in the new list (if r0 <> nil)
*	r8 - pointer to the record to move
*	loadList - pointer to the first record in the old list
*
*  Outputs:
*	input pointers are modified
*
****************************************************************
*
MoveSegment private
	using OutCommon
;
;  Break the back link
;
	ldy	#loadLast-loadNext	if r8^.last = nil then
	lda	[r8],Y
	iny
	iny
	ora	[r8],Y
	bne	lb1
	ldy	#2	  loadList := r8^.next
	lda	[r8]
	sta	loadList
	lda	[r8],Y
	sta	loadList+2
	bra	lb2	else
lb1	ldy	#loadLast-loadNext	  r12 := r8^.last
	lda	[r8],Y
	sta	r12
	iny
	iny
	lda	[r8],Y
	sta	r14
	ldy	#2	  r12^.next := r8^.next
	lda	[r8]
	sta	[r12]
	lda	[r8],Y
	sta	[r12],Y
lb2	anop		endif
;
;  Break the forward link
;
	ldy	#2	if r8^.next <> nil then
	lda	[r8]
	ora	[r8],Y
	beq	lb3
	lda	[r8]	  r12 := r8^.next
	sta	r12
	lda	[r8],Y
	sta	r14
	ldy	#loadLast-loadNext	  r12^.last := r8^.last
	lda	[r8],Y
	sta	[r12],Y
	iny
	iny
	lda	[r8],Y
	sta	[r12],Y
lb3	anop		endif
;
;  Add the record to the new list
;
	ldy	#2	r8^.next := nil
	lda	#0
	sta	[r8]
	sta	[r8],Y
	lda	r0	if r0 = nil then
	ora	r2
	bne	lb4
	ldy	#loadLast-loadNext	  r8^.last := nil
	lda	#0
	sta	[r8],Y
	iny
	iny
	sta	[r8],Y
	move4 r8,r0	  r0 := r8
	bra	lb5	else
lb4	ldy	#2	  r4^.next := r8
	lda	r8
	sta	[r4]
	lda	r10
	sta	[r4],Y
	ldy	#loadLast-loadNext	  r8^.last := r4
	lda	r4
	sta	[r8],Y
	iny
	iny
	lda	r6
	sta	[r8],Y
lb5	anop		endif
	move4 r8,r4	r4 := r8
	rts
	end

****************************************************************
*
*  NamedVariable - see if the variable is a file type
*
*  Inputs:
*	r0 - pointer to the shell variable value
*
*  Outputs:
*	C - set if we have a match, else clear
*	X-A - variable value (if C is set)
*
****************************************************************
*
NamedVariable private

	lda	[r0]	if length <> 3 then
	cmp	#3
	bne	lb3	  return false
	lda	#nameEnd-name-3	index into the name array
	sta	r4
	lda	#$BD	initial file number
	sta	r6
lb1	ldx	r4	check for match
	ldy	#2
	short M
lb1a	lda	[r0],Y
	cmp	name,X
	beq	lb1b
	and	#$5F
	cmp	name,X
	bne	lb2
lb1b	inx
	iny
	cpy	#5
	bne	lb1a
	long	M
	lda	r6	return the file type
	ldx	#0
	sec
	rts

lb2	long	M	no match - loop
	dec	r6
	sec
	lda	r4
	sbc	#3
	sta	r4
	bpl	lb1

lb3	clc		return false
	rts
;
;  Local data
;
name	dc	c'S16'	file type mnemonics
	dc	c'RTL'
	dc	c'EXE'
	dc	c'PIF'
	dc	c'TIF'
	dc	c'NDA'
	dc	c'CDA'
	dc	c'TOL'
	dc	c'DVR'
	dc	c'LDF'
	dc	c'FST'
nameEnd	anop
	end

****************************************************************
*
*  NewSegment - create a new segment record
*
*  Inputs:
*	loadNamePtr - pointer to the load name
*	lastLoadNumber - load number for the last segment created
*
*  Outputs:
*	loadNumber - number of this load segment
*	loadName - name of the load segment
*	pc - 0
*	* - all other load record variables are set to 0
*
****************************************************************
*
NewSegment private
	using Common
	using OutCommon

	move	#0,loadNext,#loadSize
	move4 loadNamePtr,r0
	ldy	#nameSize-2
lb1	lda	[r0],Y
	sta	loadName,Y
	dey
	dey
	bpl	lb1
	inc	lastLoadNumber
	lda	lastLoadNumber
	sta	loadNumber
	stz	pc
	stz	pc+2
	rts
	end

****************************************************************
*
*  OptimizeDS - optimize DS and LConst records
*
*  This subroutine removes LConst records that have a length
*  of zero, and combines adjacent DS records.
*
*  Inputs:
*	op - ptr to the end of the load segment body
*	opst - copy of op
*	loadSegStart - start of the load segment body
*
*  Outputs:
*	op - ptr to the end of the load segment body
*	opst - copy of op
*
****************************************************************
*
OptimizeDS private
	using Common
	using OutCommon
LConst	equ	$F2	long contanst op-code
DS	equ	$F0	DS op-code

	lda	loadSegStart	r0 = to address
	ldx	loadSegStart+2	r4 = from address
	sta	r0
	stx	r2
	sta	r4
	stx	r6
	stz	r8	r8 = false {r0, r4 are the same}
lb2	cmpl	r4,op	while r4 <> op do
	jge	lb12
	lda	[r4]	  if r4^ = LConst then
	and	#$00FF
	cmp	#LConst
	bne	lb3
	ldy	#1	    if length = 0 then
	lda	[r4],Y
	ldy	#3
	ora	[r4],Y
	bne	lb4
	add4	r4,#5	      skip the record
	lda	#1	      r8 := true
	sta	r8
	bra	lb2
lb3	add4	r4,#5,r10	  else {if r4^ = DS then}
lb3a	cmpl	r10,op	    if (r10 = r4+5) < op then
	bge	lb4
	lda	[r10]	      if r10^ = LConst then
	and	#$00FF
	cmp	#LConst
	bne	lb3b
	ldy	#1		if r10^.length = 0 then
	lda	[r10],Y
	ldy	#3
	ora	[r10],Y
	bne	lb4
	add4	r10,#5		  skip the lconst
	lda	#1		  r8 := true
	sta	r8
	bra	lb3a		  try again
lb3b	ldy	#1	      else if r10^ = DS then
	clc			r10^.length += r4^.length
	lda	[r10],Y
	adc	[r4],Y
	sta	[r10],Y
	ldy	#3
	lda	[r10],Y
	adc	[r4],Y
	sta	[r10],Y
	move4 r10,r4		r4 := r10
	lda	#1		r8 := true
	sta	r8
	bra	lb3		try again
lb4	lda	r8	  {move the record to the new spot}
	beq	lb10	  if the record needs to be moved then
	lda	[r4]	    if r4^ = LConst then
	and	#$00FF
	cmp	#Lconst
	bne	lb9
	ldy	#1
	lda	[r4],Y
	sta	r10
	ldy	#3
	lda	[r4],Y
	sta	r12
	add4	r10,#5
	ldx	r12
	beq	lb6
	ldy	#0
lb5	lda	[r4],Y
	sta	[r0],Y
	iny
	iny
	bne	lb5
	inc	r2
	inc	r6
	dex
	bne	lb5
lb6	lda	r10
	beq	lb10
	ldy	#0
	lsr	A
	tax
	bcc	lb7
	short M
	lda	[r4]
	sta	[r0]
	long	M
	iny
lb7	txa
	beq	lb10
lb8	lda	[r4],Y
	sta	[r0],Y
	iny
	iny
	dex
	bne	lb8
	bra	lb10	    else {if r4^ = DS then}
lb9	lda	[r4]
	sta	[r0]
	ldy	#2
	lda	[r4],Y
	sta	[r0],Y
	iny
	lda	[r4],Y
	sta	[r0],Y
lb10	lda	[r0]	  {next record}
	and	#$00FF
	cmp	#LConst
	bne	lb11
	clc
	ldy	#1
	lda	[r0],Y
	adc	r4
	tax
	ldy	#3
	lda	[r0],Y
	adc	r6
	sta	r6
	stx	r4
	clc
	ldy	#1
	lda	[r0],Y
	adc	r0
	tax
	ldy	#3
	lda	[r0],Y
	adc	r2
	sta	r2
	stx	r0
lb11	add4	r0,#5
	add4	r4,#5
	brl	lb2	endwhile

lb12	lda	r0	op = opst = r0
	ldx	r2
	sta	op
	stx	op+2
	sta	opst
	stx	opst+2
	rts
	end

****************************************************************
*
*  Pass2Prep - prepare a buffer for the first call to pass2
*
*  Inputs:
*	loadPass2 - has the buffer been prepared for pass 2?
*
****************************************************************
*
Pass2Prep private
	using Common
	using OutCommon

	lda	pass	if this is not pass 2
	cmp	#2
	bne	lb1
	lda	loadPass2	  or the buffer has already been prepared
	beq	lb2
lb1	rts		  return

lb2	add4	length,loadPC	update the length of the program
	lda	loadBanksize	check for a bank overflow
	ora	loadBanksize+2
	beq	lb2a
	cmpl	loadPC,loadBanksize
	blt	lb2a
	beq	lb2a
	ph4	#0
	ph2	#18
	jsr	Error
lb2a	pha		get memory for the output buffer
	pha
	clc
	lda	loadPC
	adc	#5
	tax
	lda	loadPC+2
	adc	#0
	pha
	phx
	ph2	userID
	ph2	#$8000
	ph4	#0
	_NewHandle
	bcc	lb3
	lda	#5
	jmp	TermError
lb3	pl4	loadSeg
	move4 loadSeg,r0	dereference the segment
	ldy	#2
	lda	[r0]
	sta	loadSegStart
	sta	op
	sta	opst
	lda	[r0],Y
	sta	loadSegStart+2
	sta	op+2
	sta	opst+2
	add4	op,#5	skip the space for the lConst header
	stz	loadPC	loadPC = 0
	stz	loadPC+2
	stz	loadOrg	loadOrg = 0
	stz	loadOrg+2
	stz	loadAlign	loadAlign = 0
	stz	loadAlign+2
	stz	pc	pc = 0
	stz	pc+2
	inc	loadPass2	loadPass2 = true
	rts
	end

****************************************************************
*
*  PrintSegmentInfo - print the segment statistics
*
*  Inputs:
*	loadList - head of the load list
*
****************************************************************
*
PrintSegmentInfo start
	using Common
	using OutCommon

	puts	#'Segment Information:',cr=t write segment header
	putcr
	puts	#'  Number    Name        Type     Length       Org',cr=t
	putcr
	jsr	SaveSegment	save the current segment
	lda	#1	set the segment number
	sta	segment
	lda	kflag
	beq	lb1
	lda	express
	beq	lb1
	inc	segment

lb1	move4 loadList,r0	find the proper segment
lb2	ldy	#loadNumber-loadNext
	lda	[r0],Y
	cmp	segment
	beq	lb3
	ldy	#2
	lda	[r0],Y
	tax
	lda	[r0]
	sta	r0
	stx	r2
	ora	r2
	bne	lb2

	move4 loadList,r0	done - get a segment back
	jsr	GetSegment
	rts

lb3	put2	segment,#6	print the segment number
	putc	#' ',#6
	inc	segment
	ldy	#loadName-loadNext	print the segment name
	short I,M
	ldx	#0
lb3a	lda	[r0],Y
	beq	lb3b
	sta	name+1,X
	iny
	inx
	cpx	#10
	bne	lb3a
lb3b	stx	name
	sec
	lda	#13
	sbc	name
	sta	fw
	long	I,M
	puts	name-1
	putc	#'$',fw	print the type
	pea	0
	ldy	#loadType-loadNext
	lda	[r0],Y
	pha
	ph2	#2
	ph2	#0
	jsr	PrintHex
	puts	#'      $'	print the length
	ldy	#loadPC-loadNext+2
	lda	[r0],Y
	pha
	dey
	dey
	lda	[r0],Y
	pha
	ph2	#8
	ph2	#0
	jsr	PrintHex
	puts	#'    '
	ldy	#loadORG-loadNext+2	print the org
	lda	[r0],Y
	sta	r6
	dey
	dey
	lda	[r0],Y
	sta	r4
	ora	r6
	beq	lb4
	putc	#'$'
	ph4	r4
	ph2	#8
	ph2	#0
	jsr	PrintHex
	putcr
	brl	lb1
lb4	puts	#'Relocatable',cr=t
	brl	lb1

segment	ds	2	segment number
name	dc	i1'10',10c' '	segment name
fw	dc	i'0'	field width for spaces after name
	end

****************************************************************
*
*  Remap - remap the segment numbers in interseg references
*
*  Inputs:
*	loadDictStart - start of the dictionary
*
*  Notes:
*	This remapping is needed when express loading a segment
*	changes the segment numbers.
*
****************************************************************
*
Remap	private
	using OutCommon

	move4 loadDictStart,r0

lb1	lda	[r0]	get an op code
	and	#$00FF
	jeq	lb5	quit if it is the end

	cmp	#$E2	skip reloc
	bne	lb2
	add4	r0,#11
	bra	lb1

lb2	cmp	#$E3	remap interseg
	bne	lb3
	ldy	#9
	lda	[r0],Y
	dec	A
	asl	A
	tay
	lda	[expMap],Y
	ldy	#9
	sta	[r0],Y
	add4	r0,#15
	bra	lb1

lb3	cmp	#$F5	skip cReloc
	bne	lb4
	add4	r0,#7
	bra	lb1

lb4	cmp	#$F7	remap Super
	jne	lb4a
	ldy	#5	if r0^.type = 2 {interseg1} then
	lda	[r0],Y
	and	#$00FF
	cmp	#2
	jne	sp7
	add4	r0,#6,r4	  r4 = disp to 1st subrecord
	sec		  r8 = page offset
	lda	loadSegStart
	sbc	#$0100-7
	sta	r8
	lda	loadSegStart+2
	sbc	#0
	sta	r10
	ldy	#1	  r12 = # bytes to process
	lda	[r0],Y
	dec	A
	sta	r12
sp1	lda	[r4]	  repeat
	and	#$00FF	    if r4^ & $80 <> 0 then
	bit	#$0080
	beq	sp4
	and	#$007F	      add in the page displacement
	bne	sp2
	lda	#$0080
sp2	xba
	clc
	adc	r8
	sta	r8
	bcc	sp3
	inc	r10
sp3	inc4	r4	      ++r4
	dec	r12	      --r12
	bra	sp6	      next entry
sp4	sta	r14	    else r14 = #bytes + 1
	inc4	r4	      skip the count byte
	dec	r12
	lda	r8	      new page
	clc
	adc	#$100
	sta	r8
	bcc	sp5
	inc	r10
sp5	short I,M	      repeat
	lda	[r4]		remap one segment
	tay
	tax
	lda	[r8],Y
	dec	A
	asl	A
	tay
	lda	[expMap],Y
	txy
	sta	[r8],Y
	long	I,M
	inc4	r4		++r4
	dec	r12		--r12
	dec	r14		--r14
	bpl	sp5	      until r14 < 0
sp6	lda	r12	  until r12 = 0
	bne	sp1
	bra	sp9
sp7	cmp	#14	else if r0^.type in [14..25] then
	blt	sp9
	cmp	#26
	bge	sp8
	short I,M	  remap the segment number
	sec
	sbc	#14
	asl	A
	tay
	lda	[expMap],Y
	clc
	adc	#13
	ldy	#5
	sta	[r0],Y
	long	I,M
	bra	sp9	else if r0^.type in [26..37] then
sp8	short I,M	  remap the segment number
	sec
	sbc	#26
	asl	A
	tay
	lda	[expMap],Y
	clc
	adc	#25
	ldy	#5
	sta	[r0],Y
	long	I,M
sp9	anop		endif
	ldy	#1	skip the record
	clc
	lda	[r0],Y
	adc	r0
	tax
	iny
	iny
	lda	[r0],Y
	adc	r2
	sta	r2
	stx	r0
	add4	r0,#5
	brl	lb1

lb4a	short I,M	remap cInterseg
	ldy	#5
	lda	[r0],Y
	dec	A
	asl	A
	tay
	lda	[expMap],Y
	ldy	#5
	sta	[r0],Y
	long	I,M
	add4	r0,#8
	brl	lb1

lb5	rts
	end

****************************************************************
*
*  RemapJumpTable - remap the segment numbers in the jump table
*
*  Inputs:
*	loadSegStart - start of the jump table buffer
*
*  Notes:
*	This remapping is needed when express loading a segment
*	changes the segment numbers.
*
****************************************************************
*
RemapJumpTable private
	using OutCommon

	add4	loadSegStart,#8+5,r0

lb1	ldy	#2	quit if file = 0
	lda	[r0],Y
	beq	lb2

	ldy	#4	remap a segment
	lda	[r0],Y
	dec	A
	asl	A
	tay
	lda	[expMap],Y
	ldy	#4
	sta	[r0],Y
	add4	r0,#14
	bra	lb1

lb2	rts
	end

****************************************************************
*
*  SaveSegment - save the current load segment record
*
****************************************************************
*
SaveSegment private
	using OutCommon

	move4 loadList,loadNext	insert the record in the linked list
	stz	loadLast
	stz	loadLast+2
	move4 pc,loadPC	save the current pc
	lda	loadSeg	if there is a segment buffer then
	ora	loadSeg+2
	beq	lb1
	sub4	op,loadSegStart,loadOp	  save the op displacement
	sub4	opst,loadSegStart,loadOpSt save the opst displacement
	ph4	loadSeg	  unlock the segment buffer
	_HUnlock
lb1	lda	loadDict	if there is a dictionary buffer then
	ora	loadDict+2
	beq	lb2
	sub4	dp,loadDictStart,loadDP	  save the dp displacement
	ph4	loadDict	  unlock the dictionary buffer
	_HUnlock
lb2	lda	loadPtr	if there is no spot reserved then
	ora	loadPtr+2
	bne	lb3
	ph2	#loadSize	  get some space
	jsr	GetSymbolMemory
	sta	loadPtr
	stx	loadPtr+2
lb3	move4 loadPtr,r0	save the record
	ldy	#loadSize-2
lb4	lda	loadNext,Y
	sta	[r0],Y
	dey
	dey
	bpl	lb4
	lda	loadList	if loadList <> nil then
	ora	loadList+2
	beq	lb5
	move4 loadList,r0	  loadList^.loadLast = loadPtr
	ldy	#loadLast-loadNext
	lda	loadPtr
	sta	[r0],Y
	iny
	iny
	lda	loadPtr+2
	sta	[r0],Y
lb5	move4 loadPtr,loadList	point the head to this segment
	rts
	end

****************************************************************
*
*  SetFileType - set the file type and aux type
*
*  Inputs:
*	kname - keep name pointer
*
****************************************************************
*
SetFileType private
;
;  Get the current file info
;
	move4 kname,giPathName
	OSGet_File_Info giRec
;
;  Set the file type
;
	ph4	#FileType	read the file type variable
	jsr	ReadVariable
	sta	r0
	stx	r2
	lda	[r0]	if the variable exists then
	beq	lb2
	jsr	NamedVariable	  handle a named variable
	bcs	lb1
	jsr	DecimalVariable	  handle a decimal variable
	bcs	lb1
	jsr	HexVariable	  handle a hex variable
	bcs	lb1
err13	ph4	#0	  flag a bad file name error
	ph2	#13
	jsr	Error
	bra	lb2
lb1	txy		  check for a file type out of range
	bne	err13
	cmp	#$B3
	blt	err13
	cmp	#$BF+1
	bge	err13
	sta	giFileType	  set the file type
	bra	lb3	else
lb2	lda	#EXE	  use the default file type
	sta	giFileType
lb3	anop		endif
	ph4	r0	Free(r0)
	jsr	Free
;
;  Set the aux type
;
	ph4	#AuxType	read the file type variable
	jsr	ReadVariable
	sta	r0
	stx	r2
	lda	[r0]	if the variable exists then
	beq	ax2
	jsr	DecimalVariable	  handle a decimal variable
	bcs	ax1
	jsr	HexVariable	  handle a hex variable
	bcs	ax1
	ph4	#0	  flag a bad file name error
	ph2	#19
	jsr	Error
	bra	ax2
ax1	stx	giAuxType+2	  save the aux type
	sta	giAuxType
	bra	ax3	else
ax2	lla	giAuxType,$100	  use the default aux type
ax3	anop		endif
	ph4	r0	Free(r0)
	jsr	Free
;
;  Update the file info
;
	OSSet_File_Info giRec
	bcs	fi1
	rts

fi1	lda	#12
	jmp	TermError
;
;  Local data
;
giRec	dc	i'4'	get/set file info record
giPathname ds	4
	ds	2
giFileType ds	2
giAuxType ds	4

FileType dos	KeepType	filetype shell variable name
AuxType	dos	AuxType	auxtype shell variable name
	end

****************************************************************
*
*  WriteBody - write the body of a load segment
*
*  Inputs:
*	loadNext... - load segment record
*	keepRefnum - keep file reference number
*
****************************************************************
*
WriteBody private
	using Common
	using OutCommon

	lda	express	if express then
	beq	lb0
	lda	loadNumber	  if loadNumber = dynSegment then
	cmp	dynSegment
	bne	lb0
	jsr	RemapJumpTable	    remap the dynamic jump table
lb0	move4 loadSegStart,wrBuff	set the buffer address
	sub4	op,loadSegStart,wrLength set the length
	lda	keepRefnum	set the refnum
	sta	wrRefnum
	OSWrite wrRec	write the segment
	bcs	lb1
	rts

lb1	lda	#12
	jmp	TermError
;
;  Local data
;
wrRec	dc	i'4'	write the segment
wrRefnum ds	2
wrBuff	ds	4
wrLength ds	4
	ds	4
	end

****************************************************************
*
*  WriteDictionary - write the dictionary for a load segment
*
*  Inputs:
*	loadNext... - load segment record
*	keepRefnum - keep file reference number
*
****************************************************************
*
WriteDictionary private
	using Common
	using OutCommon

	sub4	dp,loadDictStart,wrLength set the length
	move4 loadDictStart,wrBuff	set the buffer address
	lda	keepRefnum	set the refnum
	sta	wrRefnum
	OSWrite wrRec	write the segment
	bcs	lb1
	rts

lb1	lda	#12
	jmp	TermError
;
;  Local data
;
wrRec	dc	i'4'	write the segment
wrRefnum ds	2
wrBuff	ds	4
wrLength ds	4
	ds	4
	end

****************************************************************
*
*  WriteHeader - write the segment header for a load segment
*
*  Inputs:
*	loadNext... - load segment record
*	keepRefnum - keep file reference number
*
*  Notes:
*	This subroutine also writes the lConst opcode and
*	length field for the code image that will be written
*	by WriteBody.
*
****************************************************************
*
WriteHeader private
	using OutCommon

!			set the size of the segment
	sub4	opst,loadSegStart,byteCnt
	add4	byteCnt,#headerEnd-header
	sub4	dp,loadDictStart,r0
	add4	byteCnt,r0
	move4 loadPC,length	set the length of the segment
	move4 loadBankSize,bankSize	set the bankSize of the segment
	lda	loadType	set the kind of the segment
	sta	kind
	move4 loadORG,org	set the org of the segment
	move4 loadAlign,align	set the alignment factor of the segment
	lda	loadNumber	set the segment number
	sta	segNum
	move	loadName,segName,#nameSize set the segment name

	lda	keepRefnum	write the header
	sta	wrRefnum
	OSWrite wrRec
	bcs	lb1
	rts

lb1	lda	#12
	jmp	TermError
;
;  Local data
;
wrRec	dc	i'4'	write the segment
wrRefnum ds	2
	da	'header'
	dc	i4'headerEnd-header'
	ds	4

header	anop		segment header model
byteCnt	ds	4	length of the segment, in bytes
	dc	i4'0'	reserved space at the end of the segment
length	ds	4	length of the segment
	dc	i1'0'	undefined
	dc	i1'0'	label length
	dc	i1'4'	number length
	dc	i1'2'	OMF version
bankSize ds	4	bank size
kind	ds	2	segment kind
	dc	i'0'	undefined
org	ds	4	origin
align	ds	4	alignment factor
	dc	i1'0'	numSex; set to LSB first
	dc	i1'0'	undefined
segnum	ds	2	segment number
	ds	4	entry point
	dc	i'ldName-header'	disp to the segment name
	dc	i'headerEnd-header'	disp to the segment image
	dc	i4'0'	temp ORG
ldName	dc	10c' '	load segment name; unused in load segs
	dc	i1'10'	length of the segment name
segName	ds	10	segment name
headerEnd anop
	end
		      
