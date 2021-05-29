
TITLE Assignment 6    (assign6.asm)

; Author: Tobi Fanibi
; Last Modified: 12/06/2020
; OSU email address: fanibit@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number:  6                Due Date: 12/06/2020
; Description: This program shows use of Macros and lower level programming, with  limited use of irvine print statements. 
;This is done  by printing a set number of digits and statiscical values 
; 

; ---------------------------------------------------------------------------------
; Name: StringWriter
; Writes a string to the console
; Receives:  stackconst A int value from the stack use to OFFSET EMP
; Does Not print a new line
; --------------------------------------------------------------------------------

INCLUDE Irvine32.inc
StringWriter	MACRO	stackconst
	PUSH  EDX				
	MOV   EDX,  [EBP+stackconst]; don't need to account for EDX, because EBX was set to a constant value
	CALL  WriteString
	POP   EDX				
ENDM



; ---------------------------------------------------------------------------------
; Name: mGetSring
; Writes a string to the console to  prompt user for input
;Preconditon Requires that EBP -4 and -8 are set and free, the output values are stored their
; Receives: stackconst A int value from the stack use to OFFSET EMP
; returns the size of input, and the input to stack values -4 and -8 via EBP
; --------------------------------------------------------------------------------


	mGetSring	MACRO stackconst
		local maxsize
		local userString
		
		.data
			maxsize = 100 ;actual max is 11, with sign. but this will allow us to check
			userString			  BYTE  maxsize DUP(0)
		

			
		
		.code
			PUSH  EDX				
			PUSH ECX
			PUSH EAX
			MOV   EDX, [EBP+stackconst]
			CALL WriteString
			CALL CRLF
			mov edx, OFFSET userString
			mov ecx, maxsize
			call ReadString
			;MOV EAX, userString
			MOV [EBP-4], OFFSET userString
			MOV [EBP-8], EAX
			POP EAX
			POP ECX
			POP EDX				
ENDM

; ---------------------------------------------------------------------------------
; Name: mDisplayStrin
; Converts a string representing an int, to the actual int. Number must be a sign int and is limited to 32 bits
;Pass the string my reference 
; Receives:input=OFFSET for string
; Prints the value to the console, after the conversion isdone
; ------------------------------------------------------------------------------
mDisplayString	MACRO input
	 Local maxsize
	 Local reversestr
	 Local outputstr
	 Local outputnum
	 Local isneg
	 local _negative
	 local _getsize
	 local _preparedivide 
	 local _divide
	 local _reverse
	.data
			maxsize = 13 
			reverseStr		  BYTE  maxsize DUP(0)
			outputStr		  BYTE  maxsize DUP(0)
			outputnum		  DWORD   maxsize DUP(0)
			isneg			  DWORD  1 DUP(0);local variable to check if number is negative
			
		
	.code	
		PUSH EDX
		PUSH EAX
		PUSH EBX
		PUSH EDI
		PUSH ESI
		PUSH ECX
		
	MOV EDI,OFFSET reverseStr		
	MOV ECX,1
	MOV EDX,0
	MOV EBX,10
	CMP EDX,input
	MOV isneg,0
	jl _getsize

  _negative:
	;do this if negative
	MOV EBX,-1
	MOV EAX,input
	IMUL EBX
	MOV EBX,EAX
	MOV input,EBX
	MOV isneg,1
	MOV EBX,10
	
	_getsize:
		cmp EBX,input
		jg _preparedivide
		ADD ECX,1
		MOV EAX,10
		MUL EBX
		MOV EBX,EAX
		jmp _getsize

	
	_preparedivide:
	PUSH ECX
	MOV AL,32
	STOSB
	MOV EAX,input
	_divide:
		MOV EBX,10
		cdq
		div ebx
		MOV EBX,EAX
		MOV outputnum,EDX
		MOV ESI, OFFSET outputnum  
		LODSB
		ADD AL,48
		STOSB
		MOV EAX,EBX
		loop _divide
		
;prepare for reverse string, skip the null value here before loop
	POP ECX 
	ADD ECX,1
	MOV ESI,EDI
	MOV EDI, OFFSET outputStr
	std
	LODSB	
	CMP isneg,1
	jne _reverse
	
	;add negative sign if needed to reverse
	cld
	ADD ECX,1
	MOV AL, 45
	STOSB
	MOV EBX,10
	
	_reverse:
		std
		LODSB
		cld
		STOSB
		loop _reverse
	MOV EDX, OFFSET outputStr	
	CALL writestring

	POP ECX
	POP ESI
	POP EDI
	POP EBX
	POP EAX
	POP EDX

					
ENDM




.data
;const
ARRAYSIZE=10

;strings
intro1		BYTE		"PROGRAMMING ASSIGNMENT 6: Designing low-level I/O procedures ", 10, 13, 0
intro2		BYTE		"Written by: Tobi Fanibi.", 0
instrt1		BYTE		"Please provide 10 signed decimal integers.", 0
instrt2		BYTE		"Each number needs to be small enough to fit inside a 32 bit register. After you have finished inputting the raw numbers I will display a list of the integers, their sum, and their average value. ", 0
wrongNum1	BYTE		"ERROR: You did not enter a signed number or your number was too big..", 0
wrongNum2	BYTE		"Please try again", 0
meanMsg		BYTE		"The rounded average is ",0
sumMsg		BYTE		"The sum of these numbers is: ",0
numMsg		BYTE		"You entered the following numbers: ",0
max			BYTE		"2147483647",0
getNum		BYTE	"Please enter an signed number: ", 0

;numbers
numArray    DWORD ARRAYSIZE DUP(?)





.code
; ---------------------------------------------------------------------------------
; Name: MAIN
; Writes CALLS Other proc and pushes req to stack
; Receives:NONE
; --------------------------------------------------------------------------------

main PROC
	PUSH  OFFSET intro1
	PUSH OFFSET  intro2
	PUSH OFFSET instrt1
	PUSH OFFSET instrt2
	CALL greet
	PUSH OFFSET getnum;28
	PUSH OFFSET numArray;24
	PUSH ARRAYSIZE ;20
	PUSH OFFSET max;16
	PUSH OFFSET wrongNum1 ;12
	PUSH OFFSET wrongNum2 ;8
	CALL ReadVal
	
	PUSH OFFSET sumMsg
	PUSH OFFSET meanMsg
	PUSH OFFSET numArray
	PUSH ARRAYSIZE 
	CALL statsCalc
	PUSh OFFSET numMsg
	PUSH OFFSET numArray
	PUSH ARRAYSIZE 
	CALL writeNum
	


	INVOKE ExitProcess, 0	;exit to operating system

main ENDP
; ---------------------------------------------------------------------------------
; Name: Greet
; Writes a long msg to the console
; Receives:A long greeting split into 3 parts 
; --------------------------------------------------------------------------------
greet PROC
	PUSH EBP
	MOV EBP,ESP
	PUSH EDX


	StringWriter	20
	CALL CRLF
	StringWriter	16
	CALL CRLF
	StringWriter	12
	CALL CRLF
	StringWriter	8
	CALL CRLF
	CALL CRLF
	POP EDX
	POP EBP
	RET

greet ENDP


; ---------------------------------------------------------------------------------
; Name: Readval
; Read the user enter value and process them for entering into the array, checks to make sure that the numbers entered are valid
; Receives:
;prompt=msg to user
;Arraysize
;Array itself
; max string for the size and type of input  desired
;Returns the filled array by refrence
; --------------------------------------------------------------------------------

ReadVal PROC
	PUSH EBP
	MOV EBP,ESP
	SUB ESP,8 ; this will hold the userString(-4) on the stack, and size(-8)
	PUSH EDX
	PUSH ECX
	PUSH EAX
	PUSH ESI
	PUSH EBX
	MOV ECX, [EBP+20]
	MOV EDI,[EBP+24]
	 
 ;fill an array with 10 valid values
 MOV EDX,0
 
 _checknumber:
	 PUSH ECX
	 PUSH EDI
	 mGetSring 28
	 MOV ECX, [EBP-8]
	 CMP ECX,1 
	 je _singledigit
	 MOV ESI, [EBP-4]
	; check if first char is sign
	 CLD
	 LODSB
	 CMP   AL, 43
	 je	   _sign
	 CMP   AL, 45
	 je	   _sign
	 

	_Nosign:
		CMP AL,49
		jl _invalid
		CMP AL,57
		jg _invalid
		SUB ECX, 1
		CMP ECX,9
		jg _invalid
		je _setmax
		CMP ECX,0
		je _arrayfill
		jmp _checknonmax
	
	_sign:
		LODSB
		SUB ECX,1
		CMP ECX,0
		je _invalid
		 CMP AL,49
		jle _invalid
		CMP AL,57
		jg _invalid
		SUB ECX,1
		CMP ECX,10
		jg _invalid
		je _signmax
		CMP ECX,0 
		je _arrayfill
		
	_checknonmax:
		LODSB
		CMP AL,48
		jl	_invalid
		CMP AL,57
		jg _invalid
		loop _checknonmax
	 jmp _arrayfill
	
	
	_signmax:
		; we need to move the direction somehow, not sure how else to force this
		LODSB
	_setmax:
		MOV ESI, [EBP-4]
		MOV EDI, [EBP+16]
		CLD
		_checkmax:
			CMP AL,48
			jl	_invalid
			CMP AL,57
			jg _invalid
			CMPSB
			jg _invalid
			loop _checkmax


	_arrayfill:
		pop EDI
		POP ECX
		PUSH [EBP-8]
		PUSH [EBP-4]
		CALL fillarray
	MOV EBX,[EBP-12]
	ADD EBX, 1
	MOV [EBP-12],EBX
	SUB ECX,1
	cmp ECX,0
	je _leaveprod
	ADD EDI,4
	jmp  _checknumber


	_invalid:
		pop EDI
		POP ECX
		StringWriter 12
		CALL CRLF
		StringWriter 8
		CALL CRLF
		jmp  _checknumber

		_singledigit:
			MOV ESI, [EBP-4]
			LODSB
			CMP AL,48
			jl	_invalid
			CMP AL,57
			jg _invalid
			jmp _arrayfill


_leaveprod:
	;return after array is filled
	CALL CRLF
	POP EBX
	POP ESI
	POP EAX
	POP ECX
	POP EDX
	MOV   ESP, EBP
	POP EBP
	RET


ReadVal ENDP
; ---------------------------------------------------------------------------------
; Name: fillarray
;Once value is finilized as valid, this will fill the array with the value entered. Converts a string to the int values, before entering into the array
receives:
	; ARRay OFFSET, Array size
	;prompt=msg to user
; max string for the size and type of input  desired
;Returns the filled array by refrence
; --------------------------------------------------------------------------------
fillarray PROC
	PUSH EBP
	MOV EBP,ESP
	PUSH EBX
	PUSH EAX
	PUSH EDX
	PUSH EDI
	PUSH ESI
	PUSH ECX
	MOV ESI,[EBP+8]
	MOV ECX,[EBP+12]
	

	LODSB
	CMP AL,43
	je _setcalc
	CMP AL,45 
	je _setcalc
	jmp _setcalc2
	

	_setcalc:
		sub ECX,1
		MOV EAX,EBX
		MOV EBX, 0
		MOV EAX, 0
		MOV EAX,10
		jmp _calcnum
	

	_setcalc2:
	    MOV ESI,[EBP+8]
		MOV EBX, 0
		MOV EAX, 0
		MOV EDX,10
		jmp _calcnum

		

	_calcnum:
		IMUL EDX
		MOV EDX,EAX
		MOV EAX,0
		LODSB
		SUB EAX,48
		ADD EAX, EDX
		MOV EDX, 10
	loop _calcnum
	
	; reverse to negative if needed
	MOV [EDI],EAX
	MOV ESI,[EBP+8]
	LODSB
	CMP AL,45
	jne _returnprod
	MOV EBX, 0
	SUB EBX,1
	MOV EAX,[EDI]
	IMUL EBX
	MOV [EDI], EAX


	
	

	

_returnprod:
	POP ECX
	POP ESI
	POP EDI
	POP EDX
	POP EAX
	POP EBX
	POP EBP
	RET 4

fillarray ENDP
; ---------------------------------------------------------------------------------
; Name: statsCalc
;Prints a msg about the MEAN and SUM of the numbers within an array
;Recieves a sum msg,
   ;avergage msg
	; ARRay OFFSET
;	Array size
; --------------------------------------------------------------------------------
statsCalc PROC
	PUSH EBP
	MOV EBP,ESP
	SUB ESP,4
	PUSH EDX
	PUSH EBX
	PUSH EAX
	PUSH EDI
	PUSH ECX 
	MOV ECX,[EBP+8]
	MOV EDI,[EBP+12]
	MOV EBX,0
	_mean:
		MOV EDX,EBX
		MOV EBX,[EDI]
		ADD EBX,EDX
		ADD EDI,4
	loop _mean
	MOV EAX,EBX
	MOV [EBP-4],EBX
	StringWriter 20
	mDisplayString [EBP-4]
	CALL CRLF
	MOV EBX,[EBP+8]
	CDQ
	IDIV EBX
	MOV [EBP-4],EAX
	StringWriter 16
	mDisplayString [EBP-4]
	
	POP ECX
	POP EDI
	Pop EAX
	Pop EBX
	POP EDX
	MOV ESP,EBP
	POP EBP
	RET

statsCalc ENDP



; ---------------------------------------------------------------------------------
; Name: writeNum 
;Prints all the numbers the user has written. Uses display macro
;Recieves a sum msg,
   ;avergage msg
	; ARRay OFFSET
;	Array size
; --------------------------------------------------------------------------------

writeNum PROC
	PUSH EBP
	MOV EBP,ESP
	SUB ESP,4
	PUSH EDX
	PUSH EBX
	PUSH EAX
	PUSH EDI
	PUSH ECX 
	MOV ECX,[EBP+8] 
	MOV EDI,[EBP+12]
	CALL CRLF
	StringWriter 16
	CALL CRLF
	_writenums:
		PUSH EBX
		MOV EBX, [EDI]
		MOV [EBP-4],EBX
		POP EBX
		mDisplayString [EBP-4]
		ADD EDI,4
		SUB ECX,1
		CMP ECX,0
		je _returnprod
	jmp _writenums
	
_returnprod:	
	POP ECX
	POP EDI
	Pop EAX
	Pop EBX
	POP EDX
	MOV ESP,EBP
	POP EBP
	RET

writeNum ENDP


END main
