	mcopy linker.mac
	keep	obj/linker
****************************************************************
*
*  Linker 2.0
*
*  Link editor for ORCA/M.
*
****************************************************************
*
*  Linker 2.0.3 prepared Mar 96 by Mike Westerfield
*
****************************************************************
*
*  Linker 2.0.2 prepared Jul 94 by Mike Westerfield
*
****************************************************************
*
Linker	start
	using Common

	phk		use our data bank
	plb
	tsx		save the stack register
	stx	sreg
	ora	#$0100	set our user ID
	sta	userID
	jsl	SysIOStartup	start the I/O system
	jsr	Initialize	set up the linker
	bcs	exit
	jsr	DoPass1	do pass 1
	bcs	exit
	jsr	DoPass2	do pass 2
	bcs	exit
	lda	kflag	if kflag then
	beq	lb1
	jsr	KeepFile	  write the keep file
lb1	jsr	Terminate	do final processing

exit	entry
	jsr	PurgePlusM	purge memory only files
	jsr	SetLInfo	pass parameters back to the shell
	jsl	SysIOShutdown	shut down the I/O system
	lda	#0	return to the caller
	rtl          
	end

****************************************************************
*
*  Common - global data
*
****************************************************************
*
	copy	DirectPage
Common	data
;
;  Memory locations
;
keyboard equ	$C000	keyboard value
strobe	equ	$C010	keyboard strobe
kflags	equ	$C025	keyboard flags
;
;  Constants
;
flagB	equ	%01000000000000000000000000000000 command line flag masks
flagC	equ	%00100000000000000000000000000000
flagL	equ	%00000000000100000000000000000000
flagM	equ	%00000000000010000000000000000000
flagP	equ	%00000000000000010000000000000000
flagS	equ	%00000000000000000010000000000000
flagW	equ	%00000000000000000000001000000000
flagX	equ	%00000000000000000000000100000000
flagAll	equ	flagB+flagC+flagL+flagM+flagP+flagS+flagW+flagX

RETURN	equ	$0D	key codes
TAB	equ	$09
;
;  Symbol flags (bit masks for symFlag)
;
pass1Resolved equ 1	label defined on pass 1
pass2Resolved equ 2	label defined on pass 2
pass1Requested equ 4	label has been requested on pass1
pass2Requested equ 8	label has been requested on pass2
!			(see also, subroutine Reference2)
isConstant equ 16	is the value a constant?
isDataArea equ 32	is the symbol a data area?
isSegmentFlag equ 64	is the symbol a segment name?
;
;  global scalars
;
bankOrg	ds	2	bank org the program?
compact	ds	2	compact the object files?
dataAreas ds	256	data area array
dpReg	ds	2	default DP register
eoln	ds	2	script end of line flag
express	ds	2	expressload the file?
length	ds	4	length of the output file
libFromShell ds 2	did the shell have a variable?
libIndex ds	2	next library index
libSeg	ds	4	pointer to the current library segment
lineNumber ds	2	script line number
list	ds	2	list segment info?
memory	ds	2	is this a +m link?
numerror ds	2	number of linker errors found
pass	ds	2	pass number (1 or 2)
pause	ds	2	pause on error?
progress	ds	2	write progress info?
sreg	ds	2	stack register in main
symbols	ds	2	list the symbol table?
userID	ds	2	user ID; for memory manager calls
;
;  Current code segment information
;
segLength ds	4	length of the code in the segment
segDisp	ds	4	disp to the next segment in the file
segSpace ds	4	reserved space at the end of the segment
segType	ds	2	segment type
segName	ds	4	pointer to the name of the segment
segEntry ds	4	disp to entry point in segment
segAlign ds	4	segment alignment factor
segVersion ds	2	segment version number
segOrg	ds	4	origin for this segment
segBanksize ds 4	banksize for this segment

startpc	ds	4	pc at the start of the segment

fileNumber ds	2	source file number
dataNumber ds	2	data area number (0 for code segments)

lastDataNumber ds 2	last data area number used
lastFileNumber ds 2	last file number used
;
;  Scalars passed to and from the shell
;
merr	ds	2	maximum error level allowed
merrf	ds	2	maxiumum error level found so far
lops	ds	2	language operations
kflag	ds	2	keep flag
mflags	ds	4	minus flags
pflags	ds	4	plus flags
org	ds	4	origin
	end

****************************************************************
*
*  GetCh - get the current character from the script
*
*  Inputs:
*	r0 - ptr to the start of a file
*	r4 - length of the file
*
*  Outputs:
*	A - character read
*
*  Notes:
*	1.  All whitespace characters are converted to spaces.
*	2.  A null is returned if there are no more characters
*	    in the file.
*
****************************************************************
*
GetCh	private
	using Common

lb1	lda	r4	quit if at eof
	ora	r6
	beq	lb3
	lda	[r0]	A = r0^
	and	#$00FF
	cmp	#RETURN	if A in [RETURN,TAB] then
	beq	lb2
	cmp	#TAB	else if A = TAB then
	bne	lb3
lb2	lda	#' '	  return a space
lb3	rts
	end

****************************************************************
*
*  Initialize - get ready to do a link
*
*  Outputs:
*	C - set if an error occurred
*
****************************************************************
*
Initialize private
	using Common
;
;  Get the command line inputs
;
	tdc		set our DP register
	sta	dpReg
	jsr	GetLInfo	get the command line inputs
	jcs	rts
	jsr	GetLibList	read and handle {Libraries}
;
;  Initialize the global scalars
;
	jsr	InitSymbol	initialize the symbol table
	jsr	InitOut	initialize the output module
	stz	length	no bytes in the program
	stz	length+2
	stz	fname	no file name buffer allocated
	stz	fname+2
	stz	basename	no base name buffer allocated
	stz	basename+2
	stz	numerror	no errors so far
	stz	libSeg	no library segment buffer allocated
	stz	libSeg+2
;
;  Read the script file
;
	lda	lops	if this is a scripted link then
	lsr	A
	bcc	sf0
	stz	sdisp	  initialize the command line disp
	jsr	GetName	  get the script file name
	jsr	CopybaseName
	jsr	GetName	  make sure there is only one file
	bcc	rs1
	lda	#13
	jsr	TermError
	sec
	brl	rts
rs1	jsr	Read	  read the script file
	jsr	Script	  process the script file
	jsr	Purge	  purge the file
;
;  Set the various flags
;
sf0	stz	list	list = false
	lda	#^flagL	if +L then
	and	pflags+2
	beq	sf1
	inc	list	  list = true

sf1	stz	symbols	symbols = false
	lda	#flagS	if +S then
	and	pflags
	beq	sf2
	inc	symbols	  symbols = true

sf2	lda	#1	express = true
	sta	express
	lda	#flagX	if -X then
	and	mflags
	beq	sf3
	stz	express	  express = false

sf3	stz	pause	pause = false
	lda	#flagW	if +W then
	and	pflags
	beq	sf4
	inc	pause	  pause = true

sf4	stz	memory	memory = false
	lda	#^flagM	if +M then
	and	pflags+2
	beq	sf5
	inc	memory	  memory = true

sf5	lda	#1	compact = true
	sta	compact
	lda	#^flagC	if -C then
	and	mflags+2
	beq	sf6
	stz	compact	  compact = false

sf6	stz	bankOrg	bankOrg = false
	lda	#^flagB	if +B then
	and	pflags+2
	beq	sf7
	inc	bankOrg	  bankOrg = true

sf7	lda	#1	progress = true
	sta	progress
	lda	#^flagP	if -P then
	and	mflags+2
	beq	sf8
	stz	progress	  progress = false

sf8	anop
;
;  Write the header
;
	lda	progress
	beq	wh1
	puts	#'Link Editor 2.0.3',cr=t
	putcr
wh1	anop
;
;  Return to main
;
	clc
rts	rts
	end

****************************************************************
*
*  NextCh - get the next character from the script
*
*  Inputs:
*	r0 - ptr to the start of a file
*	r4 - length of the file
*	eoln - was the last character an eoln?
*
*  Outputs:
*	A - character read
*	eoln - was the last character an eoln?
*	r8 - set to the start of any new line
*
*  Notes:
*	1.  All whitespace characters are converted to spaces.
*	2.  A null is returned if there are no more characters
*	    in the file.
*	3.  Comments are skipped
*
****************************************************************
*
NextCh	private
	using Common
;
;  Check for EOF
;
	lda	r4	quit if at eof
	ora	r6
	jeq	lb5
;
;  Handle comments
;
	lda	eoln	if eoln then
	beq	lb3
	stz	eoln	  eoln = false
lb1	lda	[r0]	  if r0[1] in ['*','!',';'] then
	and	#$FF00
	xba
	cmp	#'*'
	beq	lb2
	cmp	#'!'
	beq	lb2
	cmp	#';'
	bne	lb3
lb2	inc4	r0	    skip this line
	dec4	r4
	lda	r4
	ora	r6
	beq	lb5
	lda	[r0]
	and	#$00FF
	cmp	#RETURN
	bne	lb2
	bra	lb1	    check for adjacent comments
;
;  Return the next character
;
lb3	inc4	r0	next char
	dec4	r4
	lda	r4	quit if at eof
	ora	r6
	beq	lb5
	lda	[r0]	A = r0^
	and	#$00FF
	cmp	#RETURN	if A = RETURN then
	bne	lb4
	add4	r0,#1,r8	  set the line start
	lda	#1	  eoln = true
	sta	eoln
	inc	lineNumber	  ++lineNumber
	lda	#' '	  return a space
	bra	lb5
lb4	cmp	#TAB	else if A = TAB then
	bne	lb5
	lda	#' '	  return a space
lb5	rts
	end

****************************************************************
*
*  Script - read and process a script file
*
*  Inputs:
*	r0 - ptr to the start of a file
*	r4 - length of the file
*	mflags,pflags - current file flags
*	kname - keep file name
*	kflag - keep file flag
*
*  Outputs:
*	mflags,pflags - file flags
*	kname - keep file name
*	kflag - keep file flag
*	slist - pointer to the file name list
*
****************************************************************
*
Script	private
	using Common
;
;  Set up the script
;
	lda	#1	eoln = true {starting a new line}
	sta	eoln
	sta	lineNumber	current line # = 1
	move4 r0,r8	set the first line pointer
	dec4	r0	get the first char (skipping comments)
	jsr	NextCh
;
;  Process flags
;
fl0	jsr	SkipBlanks	skip leading blanks
fl1	jsr	GetCh	if GetCh in ['+','-'] then
	cmp	#'+'
	beq	fl2
	cmp	#'-'
	bne	pn1
fl2	sta	flagCh	  save the flag sign
	jsr	NextCh	  get the flag character
	and	#$5F	  uppercase the character
	sec		  form the flag bit
	sbc	#'@'
	bmi	fl4
	tax
	stz	r12
	stz	r14
	sec
fl3	ror	r14
	ror	r12
	dex
	bne	fl3
	lda	r12	  make sure the flag is legal
	and	#flagAll
	bne	fl5
	lda	r14
	and	#^flagAll
	bne	fl5
fl4	lda	#1	  flag an illegal flag error
	jmp	ScriptError
fl5	jsr	NextCh	  skip the flag character
	lda	r12	  if this flag was set from the CL then
	bit	pFlags
	bne	fl0	    skip the flag
	bit	mFlags
	bne	fl0
	lda	r14
	bit	pFlags+2
	bne	fl0
	bit	mFlags+2
	bne	fl0
	lda	flagCh	  if flagCh = '+' then
	cmp	#'+'
	bne	fl6
	lda	r12	    pFlags |= r12
	ora	pFlags
	sta	pFlags
	lda	r14
	ora	pFlags+2
	sta	pFlags+2
	bra	fl0	  else
fl6	lda	r12	    mFlags |= r12
	ora	mFlags
	sta	mFlags
	lda	r14
	ora	mFlags+2
	sta	mFlags+2
	brl	fl0	  get the next flag
;
;  Process file names
;
pn1	ph4	slist	free the old list
	jsr	Free
	add4	r4,#4,r12	reserve plenty of space for the file list
	ph4	r12
	jsr	MLalloc
	sta	slist
	stx	slist+2
	stz	r16	no characters written
	add4	slist,#2,r12	set next char pointer
pn2	jsr	SkipBlanks	skip any blanks
	jsr	GetCh	if at eof then
	tax
	beq	pn6	  done
	lda	r6	if r4 > 5 then
	bne	pn2a
	lda	r4
	cmp	#6
	blt	pn4
pn2a	ldy	#4	  if r0^ = "keep=" then
pn3	lda	[r0],Y
	and	#$00FF
	jsr	ToUpper
	short M
	cmp	keep,Y
	long	M
	bne	pn4
	dey
	bpl	pn3	    done
	bra	pn6
pn4	jsr	GetCh	while not GetCh in [' ', chr(0)] do
	tax
	beq	pn5
	cmp	#' '
	beq	pn5
	short M	  save the character
	sta	[r12]
	long	M
	inc4	r12
	inc	r16	  update the line length
	jsr	NextCh	  skip to the next character
	bra	pn4	endwhile
pn5	short M	add a trailing space
	lda	#' '
	sta	[r12]
	long	M
	inc4	r12
	inc	r16
	bra	pn2	next name
pn6	lda	r16	set the list length
	beq	pn7
	dec	A
pn7	sta	[slist]
;
;  Process a keep name
;
	lda	kname	skip this step if we have a kname
	ora	kname+2
	beq	kn0
	lda	[kname]
	jne	kn5
kn0	jsr	NextCh	skip the keep name
	jsr	NextCh
	jsr	NextCh
	jsr	NextCh
	jsr	NextCh
kn1	ph4	kname	free the old list
	jsr	Free
	lda	#1	kflag = true
	sta	kflag
	add4	r4,#4,r12	reserve plenty of space for the file list
	ph4	r12
	jsr	MLalloc
	sta	kname
	stx	kname+2
	stz	r16	no characters written
	add4	kname,#2,r12	set next char pointer
	jsr	GetCh	if at eof then
	tax
	beq	kn3	  done
kn2	jsr	GetCh	while not GetCh in [' ', chr(0)] do
	tax
	beq	kn3
	cmp	#' '
	beq	kn3
	short M	  save the character
	sta	[r12]
	long	M
	inc4	r12
	inc	r16	  update the line length
	jsr	NextCh	  skip to the next character
	bra	kn2	endwhile
kn3	lda	r16	set the list length
	bne	kn4
	lda	#2	missing keep name
	jmp	ScriptError
kn4	sta	[kname]

	jsr	SkipBlanks	skip trailing blanks and comments
	jsr	GetCh	make sure there are no more chars
	tax
	beq	kn5
	lda	#3	unknow parameters
	jmp	ScriptError
kn5	rts
;
;  Local data
;
flagCh	ds	2	flag character
keep	dc	c'KEEP='	keep preamble
	end

****************************************************************
*
*  ScriptError - flag an error in a script file
*
*  Inputs:
*	r0 - ptr to the char where the error occurred
*	r4 - # chars left in the script
*	r8 - ptr to the start of the line
*	A - error number
*
****************************************************************
*
ScriptError private
	using Common

	sta	err	save the error number
	sub4	r0,r8,disp	get the disp to the error
	add4	r4,disp	move back to the start of the line
	move4 r8,r0
	put2	lineNumber,#5,errout=t	print the line number
	putc	#' ',errout=t	print one space
lb1	jsr	GetCh	print the line
	tax
	beq	lb2
	sta	ch
	putc	ch,errout=t
	jsr	NextCh
	lda	eoln
	beq	lb1
lb2	putcr errout=t
	add4	disp,#6	print the error pointer
lb3	putc	#' ',errout=t
	dec4	disp
	lda	disp
	ora	disp+2
	bne	lb3
	puts	#'^ ',errout=t

	dec	err	print the error message
	bne	lb4
	puts	#'Illegal flag',errout=t,cr=t
	bra	lb6
lb4	dec	err
	bne	lb5
	puts	#'Missing keep name',errout=t,cr=t
	bra	lb6
lb5	dec	err
	bne	lb6
	puts	#'Unrecognized parameter',errout=t,cr=t

lb6	lda	#14	stop the link
	jmp	TermError
;
;  Local data
;
ch	ds	2	character from script
err	ds	2	error number
disp	ds	4	# chars to the error
	end

****************************************************************
*
*  SkipBlanks - skip whitespace in a script file
*
****************************************************************
*
SkipBlanks private

	jsr	GetCh
	bra	lb2
lb1	jsr	NextCh
lb2	tax
	beq	lb3
	cmp	#' '
	beq	lb1
lb3	rts
	end

****************************************************************
*
*  Terminate - do terminal processing
*
****************************************************************
*
Terminate private
	using OutCommon
	using Common
;
;  Write the link statistics
;
	jsr	PrintSymbols	print the symbol table

	lda	list	if list then
	beq	sg1
	jsr	PrintSegmentInfo	  write the segment table

sg1	lda	numError	if there are errors then
	jeq	er3
	putcr errout=t	  write the error summary
	lda	numError
	dec	A
	bne	er1
	puts	#'1 error',errout=t
	bra	er2
er1	put2	numError,errout=t
	puts	#' errors',errout=t
er2	puts	#' found during link',cr=t,errout=t
	put2	merrf,errout=t
	puts	#' was the highest error level',cr=t,errout=t

er3	lda	progress	if progress or list then
	bne	er4
	lda	list
	jeq	er5
er4	putcr		  write the number, size of segments
	puts	#'There '
	lda	lastLoadNumber
	dec	A
	bne	lb1
	puts	#'is 1 segment'
	bra	lb2
lb1	puts	#'are '
	put2	lastLoadNumber
	puts	#' segments'
lb2	puts	#', for a length of $'
	ph4	length
	ph2	#8
	ph2	#0
	jsr	PrintHex
	puts	#' bytes.',cr=t
er5	anop		endif
	rts
	end
