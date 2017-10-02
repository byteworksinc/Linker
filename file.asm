	keep	obj/file	mcopy file.mac******************************************************************  File2**  This module contains the subroutines that depend on the file*  system and shell.**  This version uses shell 2.0 calls.*****************************************************************	copy	directPage******************************************************************  FileCommon - common data for the file module******************************************************************FileCommon privdata;;  Constants;fileBuffSize equ 8*1024	max size of a file name buffer;;  Shell call records;ffDCB	dc	i'14'	fast file DCBffAction ds	2ffIndex	ds	2ffFlags	ds	2ffFileHandle ds 4ffPathName ds	4ffAccess ds	2ffFileType ds	2ffAuxType ds	4ffStorageType ds 2ffCreate ds	8ffMod	ds	8ffOption ds	4ffFileLength ds 4ffBlocksUsed ds 4ffCheckSum ds	2stlf_dcb dc	i'11'	set linfo DCBstlf_src ds	4	  source filestlf_out ds	4	  output filestlf_prm dc	a4'PRM'	  parameter liststlf_lan dc	a4'LAN'	  language specific stringstlf_mer ds	1	  maximum error allowedstlf_mef ds	1	  maximum error foundstlf_lop ds	1	  operations flagstlf_kep ds	1	  keep flagstlf_ltm ds	4	  set of letters with -stlf_ltp ds	4	  set of letters with +stlf_org ds	4	  originprm	dc	i'4,0'lan	dc	i'4,0';;  global variables;libRefnum ds	2	refnum for the open library filename	ds	256	file name	end******************************************************************  AppendOSNames - append two GS/OS path names**  Inputs:*	p1,p2 - pointers to input format GS/OS names*	flags -*		0 - return an input string*		1 - return an output string**  Outputs:*	X-A - nil for out of memory, otherwise a pointer to the string**  Notes:*	This subroutine reserves a memory buffer based on*	the actual length of the expanded path name.  The*	caller is responsible for disposing of the memory.******************************************************************AppendOSNames privateout	equ	1	return pointerchars	equ	5	pointer to input format portion of out	sub	(4:p1,4:p2,2:flags),8	clc		find the length of the buffer	lda	[p1]	adc	[p2]	inc	A	inc	A	ldx	flags	beq	lb1	inc	A	inc	A	sta	charslb1	pea	0	allocate the memory	pha	jsr	MLalloc	sta	out	stx	out+2	ora	out	beq	lb7	lda	flags	if flags then	beq	lb2	lda	chars	  set the length of the buffer	sta	[out]	add4	out,#2,chars	  chars = out+2	bra	lb3	elselb2	move4 out,chars	  chars = outlb3	anop		endif	clc		set the length	lda	[p1]	adc	[p2]	sta	[chars]	lda	[p1]	move in the first string	beq	lb4a	tax	ldy	#2	short Mlb4	lda	[p1],Y	sta	[chars],Y	iny	dex	bne	lb4	long	Mlb4a	clc		update chars	lda	[p1]	adc	chars	sta	chars	bcc	lb5	inc	chars+2lb5	lda	[p2]	move in the second string	beq	lb7	tax	ldy	#2	short Mlb6	lda	[p2],Y	sta	[chars],Y	iny	dex	bne	lb6	long	Mlb7	ret	4:out	end******************************************************************  CloseLibrary - close a library file**  Inputs:*	dictionary - ptr to dictionary buffer*	libRefnum - reference number for the library file******************************************************************CloseLibrary start	using FileCommon	ph4	dictionary	dispose of the dictionary buffer	jsr	Free	lda	libRefnum	close the library file	sta	clRefnum	OSClose clRec	rtsclRec	dc	i'1'clRefnum ds	2	end******************************************************************  ConvertString - convert an output string to an input string**  Inputs:*	str - output string pointer**  Outputs:*	Returns a pointer to the input string buffer**  Notes:*	Allocates an appropriate size buffer.******************************************************************ConvertString privateptr	equ	1	sub	(4:str),4	add4	str,#2	lda	[str]	inc	A	inc	A	pea	0	pha	jsr	MLalloc	sta	ptr	stx	ptr+2	lda	[str]	tay	iny	short Mlb1	lda	[str],Y	sta	[ptr],Y	dey	bpl	lb1	long	M	ret	4:ptr	end******************************************************************  GetLibFile - get a library file name from the library directory**  Inputs:*	libIndex - index in the directory of the last library**  Outputs:*	r0 - pointer to a GS/OS path name*	C - set if a file was found, else clear**  Notes:*	The file name pointer points to a dynamically*	allocated buffer.  The caller is responsible for*	disposing of the buffer.  The buffer is allocated*	and returned even if the call fails.******************************************************************GetLibFile start	using FileCommon	using Common	stz	found	no name found, yet	ph4	#fileBuffSize+4	get a file name buffer	jsr	MLalloc	sta	gdName	sta	r0	stx	gdName+2	stx	r2	lda	#fileBuffSize+4	set its size	sta	[r0]	OSOpen opRec	open the library prefix	bcs	lb3	lda	opRefnum	set the reference number	sta	gdRefnum	sta	clRefnumlb1	inc	libIndex	try the next file in the library prefix	lda	libIndex	sta	gdIndex	OSGet_Dir_Entry gdRec	bcs	lb2	lda	gdFiletype	cmp	#LIB	bne	lb1	ph4	#libname	append the file name to the library name	add4	r0,#2	ph4	r0	ph2	#0	jsr	AppendOSNames	sta	r0	stx	r2	ph4	gdName	free the name buffer	jsr	Free	inc	found	found a filelb2	OSClose clRec	close the library direcorylb3	lda	found	return found	lsr	A	rts;;  Local data;found	ds	2	was a path name found?libname	dos	'13/'	library directory nameopRec	dc	i'2'	open recordopRefnum ds	2	dc	a4'libname'clRec	dc	i'1'	close recordclRefnum ds	2gdRec	dc	i'7'	GetDirEntry recordgdRefnum ds	2	ds	2	file flags	dc	i'0'	base (absolute displacement number)gdIndex	ds	2	index into the directorygdName	ds	4	file name	ds	2	entry numbergdFiletype ds	2	file type	end******************************************************************  GetLibList - process libraries in {Libraries} shell variable**  Inputs:*	slist - list of command line libraries**  Outputs:*	slist - updated*	libFromShell - true if {Libraries} existed, else false******************************************************************GetLibList start	using FileCommon	using Common;;  Read the library shell variable;	stz	libFromShell	libFromShell = false	ph4	#fileBuffSize+4	allocate default space for the	jsr	MLAlloc	 variable	sta	rdValue	sta	r0	stx	rdValue+2	stx	r2	lda	#fileBuffSize	set the buffer size	sta	[r0]	OSRead_Variable rdRef	read the shell variable	bcs	lb1	ldy	#2	quit if the value is null	lda	[r0],Y	beq	lb1;;  Append the variable list to the command line file list;	ph4	slist	append a space	ph4	#blank	ph2	#0	jsr	AppendOSNames	phx	pha	ph4	slist	jsr	Free	pl4	slist	ph4	slist	append the variable's contents	add4	rdValue,#2,r0	ph4	r0	ph2	#0	jsr	AppendOSNames	phx	pha	ph4	slist	jsr	Free	pl4	slist	inc	libFromShell	libFromShell = truelb1	ph4	rdValue	dispose of our buffer	jsr	Free	rts;;  Local data;blank	dos	' '	spacelib	dos	Libraries	name of the library shell variablerdRef	dc	i'3'	Read_Variable recordrdName	dc	a4'lib'rdValue	ds	4	ds	2	end******************************************************************  GetLInfo - get the command link information**  Outputs:*	slist - ptr to list of input file names*	kname - keep file name (nil for none)*	merr - maximum error level allowed*	merrf - maxiumum error level found so far*	lops - language operations*	kflag - keep flag*	mflags - minus flags*	pflags - plus flags*	org - origin**	C - set if an error occurred******************************************************************GetLInfo start	using FileCommon	using Common;;  Get info;	ph4	#fileBuffSize+4	allocate default space for the names	jsr	MLAlloc	sta	stlf_src	sta	r0	stx	stlf_src+2	stx	r2	ph4	#fileBuffSize+4	jsr	MLAlloc	sta	stlf_out	sta	r4	stx	stlf_out+2	stx	r6	lda	#fileBuffSize	set the buffer sizes	sta	[r0]	sta	[r4]	lla	stlf_prm,prm	get file info	lla	stlf_lan,lan	OSGet_LInfo stlf_dcb;;  Convert the file names;	ph4	stlf_src	jsr	ConvertString	sta	slist	stx	slist+2	ph4	stlf_src	jsr	Free	ph4	stlf_out	jsr	ConvertString	sta	kname	stx	kname+2	ph4	stlf_out	jsr	Free;;  Set the scalars;	lda	stlf_mer	and	#$00FF	sta	merr	lda	stlf_mef	and	#$00FF	sta	merrf	lda	stlf_lop	and	#$00FF	sta	lops	lda	stlf_kep	and	#$00FF	sta	kflag	move4 stlf_ltm,mflags	move4 stlf_ltp,pflags	move4 stlf_org,org	clc	rts	end******************************************************************  OpenLibrary - open a library file**  Inputs:*	fname - name of the library to open**  Outputs:*	libRefnum - reference number for the library file******************************************************************OpenLibrary start	using FileCommon	move4 fname,opPathname	OSOpen opRec	bcc	lb1	lda	#1	jmp	TermErrorlb1	lda	opRefnum	sta	libRefnum	rtsopRec	dc	i'2'opRefnum ds	2opPathname ds	4	end******************************************************************  Purge - purge a file**  Inputs:*	fname - name of the file to purge******************************************************************Purge	start	using FileCommon	move4 fname,ffPathName	lda	#7	sta	ffAction	lda	#$C000	sta	ffFlags	OSFastFile ffDCB	rts	end******************************************************************  PurgePlusM - purge memory only files******************************************************************PurgePlusM start	using Common	using FileCommon	lda	memory	skip this check if +m was not used	jeq	ff4	ph4	#fileBuffSize+4	get a file name buffer	jsr	MLalloc	sta	ffPathName	sta	r0	stx	ffPathName+2	stx	r2	lda	#fileBuffSize+4	sta	[r0]	add4	ffPathName,#2	stz	index	for each file index doff1	lda	#1	  do an indexed load of the file	sta	ffAction	lda	index	sta	ffIndex	sub4	ffPathName,#2	OSFastFile ffDCB	bcs	ff3	  quit if there is no file	add4	ffPathName,#2	lda	ffFlags	  quit if the file is not a memory file	bne	ff2	lda	#5	  remove the file	sta	ffAction	OSFastFile ffDCB	bra	ff1	  try again with the same indexff2	lda	#7	  purge the file	sta	ffAction	OSFastFile ffDCB	inc	index	next file index	bra	ff1ff3	ph4	r0	dispose of the file buffer	jsr	Freeff4	rts;;  Local data;index	ds	2	indexed load index	end******************************************************************  Read - open a file for input**  Inputs:*	fname - name of the file to open**  Outputs:*	r0 - pointer to the first byte in the file*	r4 - length of the file*	r8 - file type******************************************************************Read	start	using Common	using FileCommon	lda	memory	if +m then	beq	ff1	move4 fname,ffPathName	  try loading the file from memory	lda	#2	sta	ffAction	lda	#$C000	sta	ffFlags	OSFastFile ffDCB	bra	ff2	elseff1	move4 fname,ffPathName	  try loading the file from disk	stz	ffAction	lda	#$C000	sta	ffFlags	OSFastFile ffDCBff2	bcc	lb1	lda	#6	file not found: flag the error	jmp	TermErrorlb1	lda	ffFileLength	make sure the file is not empty	ora	ffFileLength+2	bne	lb2	lda	#13	jmp	TermErrorlb2	move4 ffFileHandle,r4	return the file parameters	ldy	#2	lda	[r4]	sta	r0	lda	[r4],Y	sta	r2	move4 ffFileLength,r4	lda	ffFileType	sta	r8	rts	end******************************************************************  ReadLibraryHeader - read the dictionary for a library**  Inputs:*	libRefnum - reference number for the library file**  Outputs:*	libSymbols - pointer to the first entry in the lib symbol table*	libLength - length of the symbol table*	libNames - pointer to the first library name*	libDisp - set to 0*	didLibSegment - set to false******************************************************************ReadLibraryHeader start	using FileCommon	lda	libRefnum	read the library header	sta	rdRefnum	sta	dcRefnum	sta	mkRefnum	OSRead rdRec	bcc	lb0err1	lda	#1	jmp	TermErrorlb0	lda	version	if version = 2 then	and	#$00FF	cmp	#2	bne	lb1	lda	type2	  if the segment type is not libDict then	and	#$00FF	    flag the error	bra	lb2lb1	lda	type1	if the segment is not libDict then	and	#$001Flb2	cmp	#8	beq	lb3	lda	#10	  TermError(10)	jmp	TermErrorlb3	ph4	length	get space for the dictionary	jsr	MLalloc	sta	dictionary	sta	dcBuffer	stx	dictionary+2	stx	dcBuffer+2	OSSet_Mark mkRec	set the file mark to the file start	move4 length,dcLength	read the dictionary	OSRead dcRec	bcs	err1	move4 dictionary,r0	{find the library symbol table}	lda	version	if version = 0 then	and	#$00FF	bne	lb4	add4	r0,#$24	  add in the segment header length	bra	lb7	else if version in [1,2] thenlb4	cmp	#3	bge	lb6	ldy	#$2A	  add in the disp to the data	clc	lda	[r0],Y	adc	r0	sta	r0	bcc	lb5	inc	r2lb5	bra	lb7	elselb6	lda	#3	  flag an unsuported segment error	jmp	TermErrorlb7	jsr	SkipLConst	skip the first lconst record	add4	r0,#5,libSymbols	set the library symbol table pointer	ldy	#1	set the length of the symbol table	lda	[r0],Y	sta	libLength	iny	iny	lda	[r0],Y	sta	libLength+2	jsr	SkipLConst	skip the symbol table	add4	r0,#5,libNames	set the library names pointer	jsr	SkipLConst	verify the last record is LConst	stz	libDisp	no symbols processed	stz	libDisp+2	stz	didLibSegment	no segments processed	rts;;  SkipLConst - skip an lconst record;SkipLConst anop	lda	[r0]	verify that the first thing is an	and	#$00FF	  lconst record	cmp	#$F2	beq	sc1	lda	#11	  {illegal data error}	jmp	TermErrorsc1	ldy	#1	skip it	clc	lda	[r0],Y	adc	r0	tax	iny	iny	lda	[r0],Y	adc	r2	sta	r2	stx	r0	add4	r0,#5	rts;;  Local data;header	anop		header for the first library segmentlength	ds	4	length of the segment, in bytes	ds	4	reserved space	ds	4	lengthtype1	ds	1	segment type, versions 0 and 1	ds	1	label length	ds	1	number lengthversion	ds	1	segment version	ds	4	bank sizetype2	ds	2	segment type, version 2headerend anoprdRec	dc	i'4'	read record for reading the firstrdRefnum ds	2	 segment header	dc	a4'header'	dc	i4'headerend-header'	ds	4	dc	i'1'	cache the blocks!dcRec	dc	i'4'	read record for reading the dictionarydcRefnum ds	2dcBuffer ds	4dcLength ds	4	ds	4	dc	i'1'	cache the blocks!mkRec	dc	i'3'	for SetMark; used to set the filemkRefnum ds	2	 mark back to the start of the file	dc	i'0'	dc	i4'0'	end******************************************************************  ReadLibrarySegment - read a segment from the library**  Inputs:*	libseg - pointer to any old library segment*	libRefnum - reference number for the library*	r0 - disp in the file**  Outputs:*	libseg - pointer to the new library segment*	seg - pointer to the first byte in the segment******************************************************************ReadLibrarySegment start	using Common	using FileCommon	lda	libRefnum	set file reference numbers	sta	rdRefnum	sta	dcRefnum	sta	mkRefnum	move4 r0,mkDisp	set the mark in the file	OSSet_Mark mkRec	bcs	err1	OSRead rdRec	read the library length	bcc	lb1err1	lda	#6	jmp	TermErrorlb1	ph4	libSeg	free any old segment	jsr	Free	ph4	length	get space for the dictionary	jsr	MLalloc	sta	libSeg	sta	dcBuffer	sta	seg	stx	libSeg+2	stx	dcBuffer+2	stx	seg+2	OSSet_Mark mkRec	set the file mark to the segment start	move4 length,dcLength	read the segment	OSRead dcRec	bcs	err1	rts;;  Local data;header	anop		header for the first library segmentlength	ds	4	length of the segment, in bytesheaderend anoprdRec	dc	i'4'	read record for reading the firstrdRefnum ds	2	 segment header	dc	a4'header'	dc	i4'headerend-header'	ds	4	dc	i'1'	cache the blocks!dcRec	dc	i'4'	read record for reading the segmentdcRefnum ds	2dcBuffer ds	4dcLength ds	4	ds	4	dc	i'1'	cache the blocks!mkRec	dc	i'3'	for SetMark; used to set the filemkRefnum ds	2	 mark back to the start of the file	dc	i'0'mkDisp	dc	i4'0'	end******************************************************************  ReadVariable - read a shell variable**  Inputs:*	name - GS/OS version of the variable name**  Outputs:*	returns a pointer to the shell variable value**  Notes:*	A value is always returned.  If there is no shell*	variable, a value with a length of 0 is returned.**	The buffer is allocated dynamically.  The caller must*	dispose of the buffer.******************************************************************ReadVariable startvalue	equ	1	pointer to the value	sub	(4:name),4	move4 name,rdName	set the name	OSRead_Variable rdRef	read the shell variable	ph4	rdValue	return the value	jsr	ConvertString	sta	value	stx	value+2	ret	4:valuerdRef	dc	i'3'	Read_Variable recordrdName	ds	4rdValue	dc	a4'buff2'	ds	2buff2	dc	i'256'	variable value	ds	256	end******************************************************************  ScanFastFile - see if the file is in the FastFile list**  Inputs:*	fname - file name**  Outputs:*	A - 1 if the file is in the list and is memory, else 0******************************************************************ScanFastFile start	using Common	using FileCommonval	equ	1	return valueptr	equ	3	work pointerfullName	equ	7	expanded version of fnameindex	equ	11	indexed load index	sub	(4:fname),12	stz	val	assume there is no match	ph4	#fileBuffSize+4	get a file name buffer	jsr	MLalloc	sta	ffPathName	sta	ptr	stx	ffPathName+2	stx	ptr+2	lda	#fileBuffSize+4	sta	[ptr]	add4	ptr,#2	add4	ffPathName,#2	ph4	#fileBuffSize+4	get another buffer for the expanded	jsr	MLalloc                    input name	sta	fullName         sta	exOut	stx	fullName+2	stx	exOut+2	lda	#fileBuffSize+4	sta	[fullName]	move4	fname,exIn	OSExpandDevices exRec	jcs	ff5	add4	fullName,#2	make sure it is lowercase	lda	[fullName]	jeq	ff5	tax	ldy	#2	short	Mlb1	lda	[fullName],Y	ora	#$20	sta	[fullName],Y	iny	dex         bne	lb1	long	M	stz	index	for each file index doff1	lda	#1	  do an indexed load of the file	sta	ffAction	lda	index	sta	ffIndex	sub4	ffPathName,#2	OSFastFile ffDCB	bcs	ff5	  quit if there is no file	add4	ffPathName,#2	lda	ffFlags	  skip if the file is not a memory file	bne	ff4	lda	[fullName]	  skip if the names are different	cmp	[ptr]	bne	ff4	tax	ldy	#2	short Mff2	lda	[ptr],Y	ora	#$20	cmp	[fullName],Y	bne	ff4	iny	dex	bne	ff2	long	M	lda	#1	  names match: val = true	sta	val	lda	#7	  purge the file	sta	ffAction	OSFastFile ffDCB	bra	ff5	  quitff4	long	M	lda	#7	  purge the file	sta	ffAction	OSFastFile ffDCB	inc	index	next file index	brl	ff1ff5	ph4	ptr	free the name buffers	jsr	Free	ph4	fullName	jsr	Free	ret	2:valexRec	dc	i'2'                     ExpandDevices recordexIn	ds	4exOut	ds	4	end******************************************************************  SetLInfo - set language info before return to shell******************************************************************SetLInfo start	using FileCommon	using Common;;  Set the scalars;	short M	lda	merr	sta	stlf_mer	lda	merrf	sta	stlf_mef	lda	lops	and	#$FC	sta	stlf_lop	lda	kflag	sta	stlf_kep	long	M	move4 mflags,stlf_ltm	move4 pflags,stlf_ltp	move4 org,stlf_org;;  Return info to the shell;	move4 kname,stlf_src	move4 kname,stlf_out	lla	stlf_prm,prm	lla	stlf_lan,lan	OSSet_LInfo stlf_dcb	rts	end