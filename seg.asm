	keep	obj/seg
	mcopy seg.mac
****************************************************************
*
*  Segment Processing
*
*  This module contains the subroutines used to find the next
*  segment that needs to be linked.
*
****************************************************************
	copy	directPage
****************************************************************
*
*  SegCommon - global data for the segment module
*
****************************************************************
*
SegCommon privdata
;
;  Scalars
;
inFile	ds	2	are we processing a file?
isLibrary ds	2	is the file we are processing a library file?
largeLibFile ds 2	largest library file number
libDisp	ds	4	disp in library symbol table
suffix	ds	2	suffix letter
	end

****************************************************************
*
*  CopyBasename - make a copy of the base name
*
*  inputs:
*	basename - base keep name
*
*  outputs:
*	fname - copy of basename
*
****************************************************************
*
CopyBasename start
	using SegCommon

	ph4	fname	free old buffer
	jsr	Free
	lda	[basename]	get new buffer
	pea	0
	inc	A
	inc	A
	pha
	jsr	MLalloc
	sta	fname
	stx	fname+2
	sta	r4	copy basename to fname
	stx	r6
	move4 basename,r0
	jsr	MoveName
	rts
	end

****************************************************************
*
*  Exists - see if a file exists
*
*  Inputs:
*	fname - pointer to the file name
*
*  Returns:
*	1 if the file exists, else 0
*
****************************************************************
*
Exists	private
val	equ	1	does the file exist?

	sub	(4:fname),2

	stz	val	assume the file does not exist
	move4 fname,giPathname	if it does exist then
	OSGet_File_Info giRec
	bcs	lb1
	inc	val	  ++val

lb1	ret	2:val	return val

giRec	dc	i'2'
giPathname ds	4
	ds	2
	end

****************************************************************
*
*  ExistsM - see if a file exists in the memory list
*
*  Inputs:
*	fname - pointer to the file name
*	memory - is this a +m link?
*
*  Returns:
*	1 if the file exists, else 0
*
****************************************************************
*
ExistsM	private
	using Common
val	equ	1	does the file exist?

	sub	(4:fname),2

	ph4	fname	(needed for both if and else branch)
	lda	memory	if this is a +m link then
	beq	lb1
	jsr	ScanFastFile	  scan the FastFile list
	bra	lb2	else
lb1	jsr	Exists	  check the disk
lb2	sta	val

	ret	2:val	return val
	end

****************************************************************
*
*  FileType - get the type of a file
*
*  Inputs:
*	fname - pointer to the file name
*
*  Returns:
*	file type (0 for none)
*
****************************************************************
*
FileType private

	sub	(4:fname),0

	stz	giFiletype	assume the file does not exist
	move4 fname,giPathname	if it does exist then
	OSGet_File_Info giRec

	ret	2:giFiletype	return giFiletype

giRec	dc	i'3'
giPathname ds	4
	ds	2
giFiletype ds	2
	end

****************************************************************
*
*  FindSuffix - find the highest keep suffix
*
*  Inputs:
*	basename - base file name
*
*  Outputs:
*	suffix - highest existing obj file suffix letter
*
****************************************************************
*
FindSuffix private
	using SegCommon

	lda	#'A'	set the initial suffix
	sta	lsuffix
lb1	lda	lsuffix	try it out
	sta	suffix
	jsr	KeepName
	ph4	fname
	jsr	ExistsM
	tax
	beq	lb2
	inc	lsuffix	it works, so try the next one
	bra	lb1

lb2	lda	lsuffix	use the last one - it worked (or did
	dec	A	 not exist, as in 'A'-1)
	sta	suffix
	rts

lsuffix	ds	2	local suffix
	end

****************************************************************
*
*  GetName - get the next file name
*
*  Inputs:
*	sdisp - disp in the name list
*	slist - list of file names
*
*  Outputs:
*	basename - new file name
*	C - set if a name was found, else clear
*
****************************************************************
*
GetName	start

	ph4	baseName	Free(baseName)
	jsr	Free
	stz	baseName	basename = NULL
	stz	baseName+2

	lda	[slist]	maxDisp = length(slist)+2
	inc	A
	inc	A
	sta	maxDisp
	ldy	sdisp	Y = sdisp+2
	iny
	iny
lb1	cpy	maxDisp	while (Y < maxDisp)
	blt	lb1a
	clc
	rts
lb1a	lda	[slist],Y	  and (slist[Y] = ' ') do
	and	#$00FF
	cmp	#' '
	bne	lb2	  ++Y
	iny
	bra	lb1

lb2	sty	nDisp	save the starting disp
lb3	cpy	maxDisp	while (Y < maxDisp)
	bge	lb4
	lda	[slist],Y	  and (slist[Y] <> ' ') do
	and	#$00FF
	cmp	#' '
	beq	lb4
	iny		  ++Y
	bra	lb3
lb4	sec		A = Y-sDisp {length of the new string}
	tya
	sbc	nDisp
	dey		sdisp = Y-2
	dey
	sty	sdisp
	pha		baseName = mlalloc(A+2)
	inc	A
	inc	A
	pea	0
	pha
	jsr	MLalloc
	sta	baseName
	stx	baseName+2

	lda	1,S	set the file name length
	sta	[baseName]
	add4	slist,nDisp,r0	set r0 to the start of the name-2
	sub4	r0,#2
	plx		move in the new characters
	ldy	#2
	short M
lb5	lda	[r0],Y
	sta	[baseName],Y
	iny
	dex
	bne	lb5
	long	M
	sec		return found
	rts
;
;  Local data area
;
nDisp	ds	4	disp in sname
maxDisp	ds	2	max allowed disp
	end

****************************************************************
*
*  InitPass - initialize pass dependent variables
*
****************************************************************
*
InitPass start
	using Common
	using SegCommon

	stz	libIndex	no libraries scanned
	stz	sdisp	no chars processed in the source list
	stz	inFile	not processing a file
	stz	fileNumber	no files processed, so far
	stz	lastFileNumber
	stz	dataNumber	no data areas processed
	stz	lastDataNumber
	rts
	end

****************************************************************
*
*  KeepName - Update the Keep Name
*
*  inputs:
*	basename - base keep name
*	suffix - suffix letter to use
*
*  outputs:
*	fname - current keep file name
*	suffix - decremented
*	C - set if there is another dot name, else clear
*
****************************************************************
*
KeepName private
	using SegCommon

	lda	suffix	if suffix = 'A'-1 then
	cmp	#'A'-1
	bne	kn0
	clc		  return false
	rts

kn0	ph4	fname	free old buffer
	jsr	Free
	lda	[basename]	get new buffer
	clc
	adc	#4
	pea	0
	pha
	jsr	MLalloc
	sta	fname
	stx	fname+2
	sta	r4	copy basename to fname
	stx	r6
	move4 basename,r0
	jsr	MoveName

	lda	[fname]	append .suffix to the names
	inc	A
	inc	A
	sta	[fname]
	tay
	short M
kn1	lda	#'.'
	sta	[fname],Y
	iny
	lda	suffix
	sta	[fname],Y
	dec	suffix	--suffix
	long	M
	sec
	rts
	end

****************************************************************
*
*  MoveName - move a file name
*
*  Inputs:
*	r0 - pointer to the name to move
*	r4 - pointer to the new file buffer
*
*  Notes:
*	This subroutine assumes that the buffer is large
*	enough.
*
****************************************************************
*
MoveName private

	lda	[r0]
	inc	A
	tay
	short M
lb1	lda	[r0],Y
	sta	[r4],Y
	dey
	bpl	lb1
	long	M
	rts
	end

****************************************************************
*
*  NextFile - find the next file
*
*  Inputs:
*	sdisp - disp in the file list
*	slist - file list
*	fname - pointer to the base file name
*	suffix - suffix letter for the next obj file
*
*  Outputs:
*	C - set if a file was found, else clear
*	inFile - set to 1
*	isLibrary - 1 for a library, 0 for an obj segment
*	fname - pointer to the base file name
*	suffix - suffix letter for the next obj file
*
****************************************************************
*
NextFile start
	using Common
	using SegCommon
;
;  If there are more files left in an obj sequence, process the next one.  For
;  example, if we just processed foo.root, we need to look for foo.a.
;
	lda	inFile	if inFile then
	beq	lb1
	inc	lastFileNumber	  update the file number
	lda	isLibrary	  if not isLibrary then
	bne	lb0
	jsr	Purge	    mark the old file as purgeable
	stz	inFile	    inFile = false
	jsr	KeepName	    form the next file name
	bcc	lb1	    if exists(fname) then
	jsr	Open	      open(fname)
	stz	isLibrary	    isLibrary = false
	lda	#1	      inFile = true
	sta	inFile
	sec		      return more files
	rts
;
;  If the last file was a library file, close it
;
lb0	clc		update the file number
	lda	lastFileNumber
	dec	A
	adc	largeLibFile
	sta	lastFileNumber
	jsr	CloseLibrary	close the library file
;
;  If the next file in the file list is a library, process it.
;
lb1	jsr	GetName	if there are files left then
	jcc	li1
	ph4	basename	  get the next file
	jsr	Exists
	tay
	beq	lb2
	ph4	basename	  if filetype = LIB then
	jsr	FileType
	cmp	#LIB
	bne	lb2
	lda	#1	    isLibrary = true
	sta	isLibrary
!	lda	#1	    inFile = true
	sta	inFile
	stz	largeLibFile	    no files processed
	lda	lastFileNumber	    update the source file number
	sta	fileNumber
	jsr	CopyBasename	    make a copy of the file name
	jsr	OpenLibrary	    open the library file
	jsr	ReadLibraryHeader
	sec		    return more files
	rts
;
;  Get the next file name from the list of file names specified on the
;  command line.
;
lb2	lda	lastFileNumber	  update the source file number
	sta	fileNumber
	jsr	FindSuffix	  find the highest dot suffix
	jsr	RootName	  form root file
	ph4	fname	  if exists(fname) then
	jsr	ExistsM
	tay
	beq	lb3
	jsr	Open	    open(fname)
	lda	#1	    inFile = true
	sta	inFile
	stz	isLibrary	    isLibrary = false
	sec		    return more files
	rts
lb3	jsr	KeepName	  form .a name
	bcc	lb4	  if exists(fname) then
	jsr	Open	    open(fname)
	lda	#1	    inFile = true
	sta	inFile
	stz	isLibrary	    isLibrary = false
	sec		    return more files
	rts
lb4	lda	#1	  TermError(1)
	jmp	TermError
;
;  Process a library file from the library directory.
;
li1	jsr	Unresolved	see if we have unresolved references
	bcc	nf1
	lda	libFromShell	see if we are using a {Libraries}
	bne	nf1	 variable
	jsr	GetLibFile	find the next library file
	bcs	li2
	ph4	r0	none left -> free the buffer & quit
	jsr	Free
	bra	nf1

li2	ph4	baseName	Free(baseName)
	jsr	Free
	move4 r0,baseName	basename = r0
	jsr	CopyBaseName	make a copy of baseName
	lda	#1	isLibrary = true
	sta	isLibrary
!	lda	#1	inFile = true
	sta	inFile
	stz	largeLibFile	no files processed
	lda	lastFileNumber	update the source file number
	sta	fileNumber
	jsr	OpenLibrary	open the library file
	jsr	ReadLibraryHeader
	sec
	rts
;
;  There are no more files to process
;
nf1	clc		return no more files
	rts
	end

****************************************************************
*
*  NextLibrarySeg - get the next library segment
*
*  Inputs:
*	libSymbols - pointer to the symbol table
*	libLength - length of the symbol table
*	libNames - pointer to the names table
*	libDisp - disp of the next symbol to process
*	didLibSegment - did we process one, yet?
*
*  Outputs:
*	C - set if a segment was found, else clear
*
****************************************************************
*
NextLibrarySeg start
	using SegCommon
	using Common
dicName	equ	0	disp to the name displacement
dicFile	equ	4	disp to the file number
dicPriv	equ	6	disp to the private flag
dicSeg	equ	8	disp to the segment disp
dicLength equ	12	length of one entry

lb1	cmpl	libLength,libDisp	if we are at the end of the file then
	bne	lb2
	lda	didLibSegment	  if we did not processed a segment then
	bne	lb1a
	clc		    return false
	rts
lb1a	stz	libDisp	  start the scan over
	stz	libDisp+2
	stz	didLibSegment
lb2	add4	libSymbols,libDisp,r0	get a pointer to the entry
	add4	libDisp,#dicLength	skip to the next entry
	clc		push the disp to the name
	ldy	#2
	lda	libNames
	adc	[r0]
	tax
	lda	libNames+2
	adc	[r0],Y
	pha
	phx
	ldy	#dicPriv	push the private flag
	lda	[r0],Y
	pha

	ldy	#dicFile	set the file number
	lda	[r0],Y
	clc
	adc	lastFileNumber
	sta	fileNumber
	lda	[r0],Y	if file number > largest one so far then
	cmp	largeLibFile
	blt	lb3
	sta	largeLibFile	  update the largest library file

lb3	jsr	NeedSegment	if we don't need this segment then
	tax
	beq	lb1	  go get the next one

	lda	#1	note that we did one
	sta	didLibSegment
	ldy	#dicSeg	read the segment
	lda	[r0],Y
	tax
	iny
	iny
	lda	[r0],Y
	sta	r2
	stx	r0
	jsr	ReadLibrarySegment
	jsr	ProcessHeader	process the header
	sec		return true
	rts
	end

****************************************************************
*
*  NextObjSeg - get the next object segment
*
*  Inputs:
*	seg - pointer to the first byte in the last segment
*	len - # bytes left in the file
*	segDisp - length of the last segment
*
*  Outputs:
*	seg - pointer to the first byte in the new segment
*	len - # bytes left in the file
*	segLength - # of bytes of code in the segment
*	segDisp - length of the new segment, in bytes
*	sp - pointer to the first byte to process
*	segSpace - reserved space at the end of the segment
*	segType - segment type
*	segName - pointer to the segment name
*	segEntry - disp from start of segment for entry point
*	segAlign - segment alignment factor
*	startpc - pc at the start of the segment
*
****************************************************************
*
NextObjSeg private
	using ExpCommon
	using Common

vc0	sub4	len,segDisp	update the # of bytes left
	add4	seg,segDisp	move to the start of the next segment
	lda	len	if we are at the end of the file then
	ora	len+2
	bne	vc1
	clc		  return with no segment
	rts

vc1	jsr	ProcessHeader	process the segment header
	cmpl	len,segDisp	make sure there are enough bytes in the
	bge	vc2	 file
	lda	#4
	jmp	TermError
vc2	stz	expSegment	make sure the segment has not already
	ph4	segName	 been included
	ph2	#0
	jsr	GetSymbolValue
	lda	symbolData
	beq	vc2a
	lda	symbolFlag
	and	#isSegmentFlag
	beq	vc5
vc2a	lda	pass
	cmp	#2
	beq	vc3
	lda	#pass1Resolved
	bra	vc4
vc3	lda	#pass2Resolved
vc4	and	symbolFlag
	bne	vc6
vc5	sec
	rts

!			handle a duplicate segment
vc6	lda	symbolFile	if the segments are in the same file then
	cmp	fileNumber
	beq	vc0	  skip this segment
	lda	segType	if this segment is private then
	and	#$4000
	bne	vc5	  process the segment
	lda	pass	if this is pass 1 then
	cmp	#1
	jeq	vc0	  don't flag the error
	ph4	segName
	ph2	#4	flag a duplicate segment error
	jsr	Error
	brl	vc0
	end

****************************************************************
*
*  NextSegment - find the next segment
*
*  Outputs:
*	C - set if a segment was found, else clear
*
****************************************************************
*
NextSegment start
	using SegCommon

	lda	inFile	if we are not processing a file then
	bne	lb2
lb1	jsr	NextFile	  get one
	bcc	lb4
lb2	lda	isLibrary	if we are in a library then
	beq	lb3
	jsr	NextLibrarySeg	  get the next library segment
	bcc	lb1	  if none, go to the next file
	bra	lb4	else
lb3	jsr	NextObjSeg	  get the next obj segment
	bcc	lb1	  if none, go to the next file
lb4	anop		endif
	rts
	end

****************************************************************
*
*  Open - open an object file and prepare it for input
*
*  Inputs:
*	fname - file name
*
*  Outputs:
*	seg - pointer to the first byte in the file
*	len - length of the file
*	segDisp - 0
*
****************************************************************
*
Open	private
	using Common

	jsr	Read	open the file for input
	lda	r8	make sure the file is an obj file
	cmp	#OBJ
	beq	lb1
	lda	#2
	jmp	TermError
lb1	move4 r0,seg	set the initial byte pointer
	move4 r4,len	set the lengt of the file
	stz	segDisp	set the "previous" segment disp to 0
	stz	segDisp+2
	rts
	end

****************************************************************
*
*  ProcessHeader - process the header for the next code segment
*
*  Inputs:
*	seg - pointer to the first byte in the segment
*
*  Outputs:
*	segLength - # of bytes of code in the segment
*	segDisp - length of the new segment, in bytes
*	sp - pointer to the first byte to process
*	segSpace - reserved space at the end of the segment
*	segType - segment type
*	segName - pointer to the segment name
*	segEntry - disp from start of segment for entry point
*	segAlign - segment alignment factor
*	segBanksize - segment bank size
*	startpc - pc at the start of the segment
*
****************************************************************
*
ProcessHeader private
	using Common
	using OutCommon
resspc	equ	$04	disp to reserved space
length	equ	$08	disp to code length
lablen	equ	$0D	disp to label length
numlen	equ	$0E	disp to number length
version	equ	$0F	disp to the segment version
banksize equ	$10	disp to bank size

s0type	equ	$0C	disp to segment type
s0org	equ	$14	disp to org
s0align	equ	$18	disp to alignment factor
s0numsex equ	$1C	disp to the number type

s1type	equ	$0C	disp to segment type
s1org	equ	$18	disp to org
s1numsex equ	$20	disp to the number type
s1entry	equ	$24	disp to segment entry
s1dispname equ $28	disp to the name displacement
s1dispdata equ $2A	disp to the data displacement
s1align	equ	$1C	disp to alignment factor

s2type	equ	$14	disp to segment type
s2org	equ	$18	disp to org
s2numsex equ	$20	disp to the number type
s2entry	equ	$24	disp to segment entry
s2dispname equ $28	disp to the name displacement
s2dispdata equ $2A	disp to the data displacement
s2temporg equ	$2C	disp to temporg
s2align	equ	$1C	disp to alignment factor
;
;  Do processing common to all segments
;
	ldy	#resspc	get the reserved space
	lda	[seg],Y
	sta	segSpace
	iny
	iny
	lda	[seg],Y
	sta	segSpace+2
	ldy	#length	get the length of the code
	lda	[seg],Y
	sta	segLength
	iny
	iny
	lda	[seg],Y
	sta	segLength+2
	ldy	#banksize	get the bank size
	lda	[seg],Y
	sta	segBanksize
	iny
	iny
	lda	[seg],Y
	sta	segBanksize+2
	ldy	#lablen	make sure names are pstrings
	lda	[seg],Y
	and	#$00FF
	bne	vc2
	ldy	#numlen	make sure numbers are 4 bytes long
	lda	[seg],Y
	and	#$00FF
	cmp	#4
	beq	vt0
vc2	lda	#4	flag an illegal header value error
	jmp	TermError
;
;  Handle a version 2 header
;
vt0	ldy	#version	get the segment version number
	lda	[seg],Y
	and	#$00FF
	sta	segVersion
	cmp	#2	branch if not version 2
	jne	vo1

	ldy	#2	get the length of the segment
	lda	[seg]
	sta	segDisp
	lda	[seg],Y
	sta	segDisp+2
	ldy	#s2type	get the segment type
	lda	[seg],Y
	sta	segType
	ldy	#s2org	get the org
	lda	[seg],Y
	sta	segOrg
	iny
	iny
	lda	[seg],Y
	sta	segOrg+2
	ldy	#s2align	get the alignment factor
	lda	[seg],Y
	sta	segAlign
	iny
	iny
	lda	[seg],Y
	sta	segAlign+2
	ldy	#s2entry	get the entry disp
	lda	[seg],Y
	sta	segEntry
	iny
	iny
	lda	[seg],Y
	sta	segEntry+2
	ldy	#s2dispdata	get the disp to the first op code byte
	lda	[seg],Y
	clc
	adc	seg
	sta	sp
	lda	seg+2
	adc	#0
	sta	sp+2
	ldy	#s2dispname	get a pointer to the segment name
	lda	[seg],Y	 and find the proper load segment
	clc
	adc	seg
	sta	segName
	lda	seg+2
	adc	#0
	sta	segName+2
	move4 segName,loadNamePtr
	add4	segName,#10
	jsr	FindLoadSegment
	ldy	#s2numsex	verify that numsex = 0
	lda	[seg],Y
	and	#$00FF
	beq	vt1
	lda	#4
	jmp	TermError
vt1	lda	pass	if this is pass 2 then
	cmp	#2
	jne	vf1
	ldy	#s2dispname	  skip check if disp to names < $30
	lda	[seg],Y
	cmp	#$30
	jlt	vf1
	ldy	#s2temporg	    flag temporg errors
	lda	[seg],Y
	iny
	iny
	ora	[seg],Y
	jeq	vf1
	ph4	#0
	ph2	#12
	jsr	Error
	brl	vf1
;
;  Handle a version 1 header
;
vo1	cmp	#1	branch if not version 1
	jne	vz1

	ldy	#2	get the length of the segment
	lda	[seg]
	sta	segDisp+1
	lda	[seg],Y
	short M
	stz	segDisp
	sta	segDisp+3
	long	M
	asl	segDisp
	rol	segDisp+2
	ldy	#s1type	get the segment type
	lda	[seg],Y
	and	#$00FF
	pha
	and	#$001F
	sta	segType
	pla
	xba
	and	#$E000
	ora	segType
	sta	segType
	ldy	#s1org	get the org
	lda	[seg],Y
	sta	segOrg
	iny
	iny
	lda	[seg],Y
	sta	segOrg+2
	ldy	#s1align	get the alignment factor
	lda	[seg],Y
	sta	segAlign
	iny
	iny
	lda	[seg],Y
	sta	segAlign+2
	ldy	#s1entry	get the entry disp
	lda	[seg],Y
	sta	segEntry
	iny
	iny
	lda	[seg],Y
	sta	segEntry+2
	ldy	#s1dispdata	get the disp to the first op code byte
	lda	[seg],Y
	clc
	adc	seg
	sta	sp
	lda	seg+2
	adc	#0
	sta	sp+2
	ldy	#s1dispname	get a pointer to the segment name
	lda	[seg],Y	 and find the proper load segment
	clc
	adc	seg
	sta	segName
	lda	seg+2
	adc	#0
	sta	segName+2
	move4 segName,loadNamePtr
	add4	segName,#10
	jsr	FindLoadSegment
	ldy	#s1numsex	verify that numsex = 0
	lda	[seg],Y
	and	#$00FF
	jeq	vf1
	lda	#4
	jmp	TermError
	brl	vf1
;
;  Handle a version 0 header
;
vz1	cmp	#0	branch if not version 0
	jne	ve1

	ldy	#2	get the length of the segment
	lda	[seg]
	sta	segDisp+1
	lda	[seg],Y
	short M
	stz	segDisp
	sta	segDisp+3
	long	M
	asl	segDisp
	rol	segDisp+2
	ldy	#s0type	get the segment type
	lda	[seg],Y
	and	#$00FF
	pha
	and	#$001F
	sta	segType
	pla
	xba
	and	#$E000
	ora	segType
	sta	segType
	ldy	#s0org	get the org
	lda	[seg],Y
	sta	segOrg
	iny
	iny
	lda	[seg],Y
	sta	segOrg+2
	ldy	#s0align	get the alignment factor
	lda	[seg],Y
	sta	segAlign
	iny
	iny
	lda	[seg],Y
	sta	segAlign+2
	stz	segEntry	get the entry disp
	stz	segEntry+2
	add4	seg,#$24,segName	get a pointer to the segment name
	move4 segName,r0	get the disp to the first op code byte
	lda	[r0]
	and	#$00FF
	sec
	adc	segName
	sta	sp
	lda	segName+2
	adc	#0
	sta	sp+2
	ldy	#s0numsex	verify that numsex = 0
	lda	[seg],Y
	and	#$00FF
	beq	vz2
	lda	#4
	jmp	TermError

vz2	lla	loadNamePtr,blankSeg	find the proper load segment
	jsr	FindLoadSegment
	bra	vf1
;
;  Segment version error
;
ve1	lda	#3
	jmp	TermError
;
;  Do common end processing
;
vf1	stz	dataNumber	data area number is 0 for code files
	lda	segType	if this is a data area then
	and	#$00FF
	cmp	#1
	bne	vf2
	inc	lastDataNumber	  assign a data area number
	lda	lastDataNumber
	sta	dataNumber
vf2	move4 pc,startpc	record the pc
	sec
	rts
;
;  Local data
;
blankSeg dc	10c' '	default load segment name
	end

****************************************************************
*
*  RootName - Append .ROOT to file name
*
*  inputs:
*	basename - base file name
*
*  outputs:
*	ckname - current keep file name
*	tkname - .ROOT appended to contents of kname
*	kltr - suffix letter for the main obj file
*
****************************************************************
*
RootName private
	using SegCommon

	ph4	fname	free old buffer
	jsr	Free
	lda	[basename]	get new buffer
	clc
	adc	#2+l:root
	pea	0
	pha
	jsr	MLalloc
	sta	fname
	stx	fname+2
	sta	r4	copy basename to fname
	stx	r6
	move4 basename,r0
	jsr	MoveName

	lda	[fname]	append root to the name
	tay
	clc
	adc	#l:root
	sta	[fname]
	iny
	iny
	ldx	#0
	short M
kn1	lda	root,X
	sta	[fname],Y
	iny
	inx
	cpx	#l:root
	bne	kn1
	long	M
	rts

root	dc	c'.ROOT'
	end
