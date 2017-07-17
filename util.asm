	keep	obj/util
	mcopy util.mac
****************************************************************
*
*  Util
*
*  This module contains general purpose utility subroutines
*  used throughout the editor.  It also contains utility
*  subroutines used by both linker passes.
*
****************************************************************
	copy	directPage
****************************************************************
*
*  CheckAlign - make sure the alignment is a power of 2
*
*  Inputs:
*	align - alignment factor
*
****************************************************************
*
CheckAlign start
count	equ	1	bit count

	sub	(4:align),2

	stz	count	count the bits
	ldx	#16
lb1	lsr	align+2
	ror	align
	bcc	lb2
	inc	count
lb2	dex
	bne	lb1
	lda	count	if count <> 1 then
	cmp	#1
	beq	lb3
	ph4	#0	  flag the error
	ph2	#23
	jsr	Error

lb3	ret
	end

****************************************************************
*
*  Error - Writes segment error messages
*
*  Inputs:
*	name - pointer to the symbol name; nil for none
*	num - error number
*
****************************************************************
*
Error	start
	using Common
temp	equ	1	temp work number
lpc	equ	5	local copy of the program counter

	sub	(4:name,2:num),8

	ldx	dpReg	get the program counter
	lda	>pc,X
	sta	lpc
	lda	>pc+2,X
	sta	lpc+2
	inc	numError	up the error count
	lda	list
	bne	lb1
	putcr errout=t
lb1	puts	#'Error at ',errout=t	print error info
	sub4	lpc,startpc,temp
	ph4	temp
	ph2	#8
	ph2	#1
	jsr	PrintHex
	puts	#' past ',errout=t
	sub4	segName,#1,temp
	puts	[temp],errout=t
	puts	#' PC = ',errout=t
	ph4	lpc
	ph2	#8
	ph2	#1
	jsr	PrintHex
	puts	#' : ',errout=t

	ldx	num	set the error level
	lda	erLev-1,X
	and	#$00FF
	cmp	merrf
	blt	ls3
	sta	merrf
ls3	lda	num	write the error message
	dec	A
	asl	A
	tax
	lda	erAdr,X
	sta	temp
	lda	#^er1
	sta	temp+2
	puts	[temp],errout=t
	lda	name	print segment name if any
	ora	name+2
	beq	ls6
	ldx	num
	cpx	#8
	bne	ls4
	puts	#' Data area: ',errout=t
	bra	ls5
ls4	puts	#' Label: ',errout=t
ls5	sub4	name,#1,temp
	puts	[temp],errout=t
ls6	putcr errout=t
	lda	pause	see if we need to pause on error
	beq	ls7
	jsr	Wait
ls7	ret
;
;  Local data
;
erLev	dc	I1'8,16,16,2'
	dc	I1'16,8,16,2'
	dc	I1'8,8,8,4'
	dc	I1'4,16,2,8'
	dc	I1'8,4,4,8'
	dc	I1'8,8,8,8'

erAdr	dc	a'er1-1'
	dc	a'er2-1'
	dc	a'er3-1'
	dc	a'er4-1'
	ds	2
	dc	a'er6-1'
	dc	a'er7-1'
	dc	a'er8-1'
	dc	a'er9-1'
	dc	a'er10-1'
	dc	a'er11-1'
	dc	a'er12-1'
	dc	a'er13-1'
	ds	2
	dc	a'er15-1'
	dc	a'er16-1'
	dc	a'er17-1'
	dc	a'er18-1'
	dc	a'er19-1'
	dc	a'er20-1'
	dc	a'er21-1'
	dc	a'er22-1'
	dc	a'er23-1'
	dc	a'er24-1'

er1	dw	'Duplicate label'
er2	dw	'Illegal shift operator'
er3	dw	'ORG location has been passed'
er4	dw	'Duplicate segment'
er6	dw	'Unresolved reference'
er7	dw	'Addressing error'
er8	dw	'Data area not found'
er9	dw	'Address is not in zero page'
er10	dw	'Address is not in current bank'
er11	dw	'Relative address out of range'
er12	dw	'Temporg not supported'
er13	dw	'Illegal {KeepType} shell variable'
er15	dw	'Segment types conflict'
er16	dw	'Invalid operation on relocatable expression'
er17	dw	'Only JSL can reference dynamic segment'
er18	dw	'Code exceeds code bank size'
er19	dw	'Illegal {AuxType} shell variable'
er20	dw	'Shift operator is not allowed on JSL to dynamic segment'
er21	dw	'Alignment and ORG conflict'
er22	dw	'Alignment factor must not exceed segment align factor'
er23	dw	'Alignment factor must be a power of two'
er24	dw	'Expression operand is not in same segment'
	end

****************************************************************
*
*  Free - free memory allocated by Malloc
*
*  Inputs:
*	ptr - address of the parameter block
*
*  Notes:
*	No action is taken if a nil pointer is passed.
*
*	This subroutine must be called in long mode.
*
****************************************************************
*
Free	start

	sub	(4:ptr),0

	lda	ptr
	ora	ptr+2
	beq	rts
	pha
	pha
	ph4	ptr
	_FindHandle
	_DisposeHandle
rts	ret
	end

****************************************************************
*
*  MLalloc - allocate memory
*
*  Inputs:
*	len - # of bytes to allocate
*
*  Outputs:
*	X-A - pointer to allocated memory
*
*  Notes:
*	Flags a terminal error and quits if there is not
*	emough memory.
*
*	This subroutine must be called in long mode.
*
****************************************************************
*
MLalloc	start
	using Common
ptr	equ	1	pointer to memory
hand	equ	5	handle of memory

	sub	(4:len),8

	pha		reserve the memory
	pha
	ph4	len
	ph2	userID
	ph2	#$C010
	ph4	#0
	_NewHandle
	pl4	hand	pull the handle
	bcc	lb1	branch if there was an error
	lda	#5
	jmp	TermError
lb1	ldy	#2	dereference the handle
	lda	[hand],Y
	sta	ptr+2
	lda	[hand]
	sta	ptr

	ret	4:ptr	return
	end

****************************************************************
*
*  CheckForPause - pause if a key was pressed; check for abort
*
****************************************************************
*
CheckForPause start
	using Common

	short I,M
	lda	>keyboard	see if we need to pause
	bpl	no	branch if not
	sta	>strobe	yes - clear strobe
	and	#$7F
	cmp	#'.'	quit if is an open apple .
	bne	lb1
	lda	>kflags
	bmi	yes

lb1	lda	>keyboard	wait for keypress
	bpl	lb1
	sta	>strobe
	and	#$7F
	cmp	#'.'	quit if is an open apple .
	bne	no
	lda	>kflags
	bmi	yes
no	long	I,M
	rts

yes	long	I,M	quit
	lda	#15
	jmp	TermError
	end

****************************************************************
*
*  PrintHex - print a hex number
*
*  Inputs:
*	val - hex value
*	digits - number of digits to print
*	errout - error out flag
*
****************************************************************
*
PrintHex start
temp	equ	1	temp work value

	sub	(4:val,2:digits,2:errout),4

	lda	digits	if digits <> 1 then
	cmp	#1
	beq	lb1
	move4 val,temp	  PrintHex(val>>4, digits-1)
	lsr	temp+2
	ror	temp
	lsr	temp+2
	ror	temp
	lsr	temp+2
	ror	temp
	lsr	temp+2
	ror	temp
	ph4	temp
	lda	digits
	dec	A
	pha
	ph2	errout
	jsr	PrintHex
lb1	lda	val	print a hex digit
	and	#$000F
	ora	#'0'
	cmp	#'9'+1
	blt	lb2
	adc	#6
lb2	sta	temp
	lda	errout
	bne	lb3
	putc	temp
	bra	lb4
lb3	putc	temp,errout=t
lb4	anop

	ret
	end

****************************************************************
*
*  PrintOSString - print an os string
*
*  Inputs:
*	ptr - pointer to the string
*	errout - error output flag
*
****************************************************************
*
PrintOSString start
loop	equ	1	loop counter
char	equ	3	character to write

	sub	(4:ptr,2:errout),4

	lda	ptr
	ora	ptr+2
	beq	lb4
	lda	[ptr]
	beq	lb4
	sta	loop
	add4	ptr,#2

lb1	lda	[ptr]
	sta	char
	lda	errout
	bne	lb2
	putc	char
	bra	lb3
lb2	putc	char,errout=t
lb3	inc4	ptr
	dec	loop
	bne	lb1

lb4	ret
	end

****************************************************************
*
*  TermError - handle a terminal error
*
*  Inputs:
*	A - error number
*	fname - file name (used for file errors)
*
*  1: Could not open file <fname>
*  2: Must be an object file: <fname>
*  3: Linker version mismatch
*  4: Illegal header value in <fname>
*  5: Out of memory
*  6: File read error: <fname>
*  7: Could not overwrite existing file: <fname>
*  8: Undefined opcode in <file>
*  9: Expression too complex in <file>
* 10: Could not find library header in <file>
* 11: Invalid dictionary in <file>
* 12: File write error
* 13: Only one script file is allowed
* 14: Script error: link aborted
* 15: Stopped by open-apple .
*
****************************************************************
*
TermError start
	using Common

	pha		print the message leader
	puts	#'Terminal error: ',errout=t
	lda	dpReg	restore the default DP register
	tcd
	lda	1,S	print the message
	asl	A
	tax
	lda	#^msg
	pha
	lda	msg-2,X
	pha
	ph2	#1
	jsr	PrintOSString
	plx		if needed, print the file name
	lda	needFname-1,X
	and	#$00FF
	beq	lb1
	ph4	fname
	ph2	#1
	jsr	PrintOSString
lb1	putcr errout=t
	stz	lops	set lops to 0
	lda	#127	set max error found to 127
	sta	merrf
	lda	Sreg	restore the original stack reg
	tcs
	jmp	exit	exit
;
;  Local data
;
msg	dc	a'e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15'

e1	dos	'Could not open file '
e2	dos	'Must be an object file: '
e3	dos	'Linker version mismatch'
e4	dos	'Illegal header value in '
e5	dos	'Out of memory'
e6	dos	'File read error: '
e7	dos	'Could not overwrite existing file'
e8	dos	'Undefined opcode in '
e9	dos	'Expression too complex in '
e10	dos	'Could not find library header in '
e11	dos	'Invalid dictionary in '
e12	dos	'File write error'
e13	dos	'Only one script file is allowed'
e14	dos	'Script error: link aborted'
e15	dos	'Stopped by open-apple .'

needFname dc	i1'1,1,0,1,0,1,0,1,1,1,1,0,0,0,0'
	end

****************************************************************
*
*  ToUpper - Convert to Upper-case
*
*  Inputs:
*	A - character to shift
*
*  Outputs:
*	A - upper-case character
*
*  Notes:
*	This subroutine can be called in long or short mode.
*
****************************************************************
*
ToUpper	start

	php
	long	M
	and	#$00FF
	cmp	#'a'
	blt	rts
	cmp	#'z'+1
	bge	rts
	adc	#'A'-'a'
rts	plp
	rts
	end

****************************************************************
*
*  Wait - Wait for a keypress
*
*  Notes:
*	Quits if the user presses open-apple .
*
****************************************************************
*
Wait	start
	using Common

	short I,M
wa1	lda	>keyboard	wait for keypress
	bpl	wa1
	sta	>strobe
	and	#$7F
	cmp	#'.'	quit if is an open apple .
	bne	wa2
	lda	>kflags
	bmi	abort
wa2	long	I,M
	rts

abort	long	I,M
	lda	#15
	jmp	TermError
	end
