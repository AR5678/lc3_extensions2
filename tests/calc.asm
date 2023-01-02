;
; The Calculator, Main Algorithm
;
	.ORIG x3000
	LEA R6,StackBase ; Initialize the Stack Pointer.
	ADD R6,R6,#1 ; R6 = StackBase + 1 --> empty stack

NewCommand LEA R0,PromptMsg
	PUTS
	GETC
	OUT
;
; Check the command
;
TestX LD R1,NegX ; Check for X.
	ADD R1,R1,R0
	BRnp TestC
	HALT
;
TestC LD R1,NegC ; Check for C.
	ADD R1,R1,R0
	BRnp TestAdd
	JSR OpClear ; See Figure 10.20
	BRnzp NewCommand
;
TestAdd LD R1,NegPlus ; Check for +
	ADD R1,R1,R0
	BRnp TestMult
	JSR OpAdd ; See Figure 10.8
	BRnzp NewCommand
;
TestMult LD R1,NegMult ; Check for *
	ADD R1,R1,R0
	BRnp TestMinus
	JSR OpMult ; See Figure 10.12
	BRnzp NewCommand
;
TestMinus LD R1,NegMinus ; Check for -
	ADD R1,R1,R0
	BRnp TestD
	JSR OpNeg ; See Figure 10.13
	BRnzp NewCommand
;
TestD LD R1,NegD ; Check for D
	ADD R1,R1,R0
	BRnp EnterNumber
	JSR OpDisplay ; See Figure 10.19
	BRnzp NewCommand
;
; Then we must be entering an integer
;
EnterNumber JSR PushValue ; See Figure 10.16
	BRnzp NewCommand
;
PromptMsg .FILL x000A
.STRINGZ "Enter a command:"
NegX	.FILL xFFA8
NegC	.FILL xFFBD
NegPlus .FILL xFFD5
NegMinus .FILL xFFD3
NegMult	.FILL xFFD6
NegD	.FILL xFFBC

; Globals
StackMax .BLKW #9
StackBase .BLKW #1
ASCIIBUFF .BLKW #4
	.FILL x0000 ; ASCIIBUFF sentinel
	
;
; This subroutine takes an ASCII string of up to three decimal digits and
; converts it into a binary number. R0 is used to collect the result.
; R1 keeps track of how many digits are left to process. ASCIIBUFF
; contains the most significant digit in the ASCII string.
;
ASCIItoBinary	ST R1,AtoB_Save1
	ST R2,AtoB_Save2
ST 	R3,AtoB_Save3
ST 	R4,AtoB_Save4
	AND R0,R0,#0 ; R0 will be used for our result.
	ADD R1,R1,#0 ; Test number of digits.
	BRz AtoB_Done ; There are no digits, result is 0.
;
	LD R2,AtoB_ASCIIBUFF ; R2 points to ASCIIBUFF
	ADD R2,R2,R1
	ADD R2,R2,#-1 ; R2 now points to "ones" digit.
;	
	LDR R4,R2,#0 ; R4 <-- "ones" digit
	AND R4,R4,x000F ; Strip off the ASCII template.
	ADD R0,R0,R4 ; Add ones contribution.
;
	ADD R1,R1,#-1
	BRz AtoB_Done ; The original number had one digit.
	ADD R2,R2,#-1 ; R2 now points to "tens" digit.
;
	LDR R4,R2,#0 ; R4 <-- "tens" digit
	AND R4,R4,x000F ; Strip off ASCII template.
	LEA R3,LookUp10 ; LookUp10 is BASE of tens values.
	ADD R3,R3,R4 ; R3 points to the right tens value.
	LDR R4,R3,#0
	ADD R0,R0,R4 ; Add tens contribution to total.
;
	ADD R1,R1,#-1
	BRz AtoB_Done ; The original number had two digits.
	ADD R2,R2,#-1 ; R2 now points to "hundreds" digit.
;
	LDR R4,R2,#0 ; R4 <-- "hundreds" digit
	AND R4,R4,x000F ; Strip off ASCII template.
	LEA R3,LookUp100 ; LookUp100 is hundreds BASE.
	ADD R3,R3,R4 ; R3 points to hundreds value.
	LDR R4,R3,#0
	ADD R0,R0,R4 ; Add hundreds contribution to total.
;
	AtoB_Done LD R1,AtoB_Save1
	LD R2,AtoB_Save2
	LD R3,AtoB_Save3
	LD R4,AtoB_Save4
	RET
;
AtoB_ASCIIBUFF .FILL ASCIIBUFF
AtoB_Save1 .BLKW #1
AtoB_Save2 .BLKW #1
AtoB_Save3 .BLKW #1
AtoB_Save4 .BLKW #1
LookUp10 .FILL #0
 	.FILL #10
	.FILL #20
	.FILL #30
	.FILL #40
	.FILL #50
	.FILL #60
	.FILL #70
	.FILL #80
	.FILL #90
;
LookUp100 .FILL #0
	.FILL #100
	.FILL #200
	.FILL #300
	.FILL #400
	.FILL #500
	.FILL #600
	.FILL #700
	.FILL #800
	.FILL #900
	
; This subroutine converts a 2’s complement integer within the range
; -999 to +999 (located in R0) into an ASCII character string consisting
; of a sign digit, followed by three decimal digits, and stores the
; character string into the four memory locations starting at ASCIIBUFF
; (see Figure 10.4).
;
BinarytoASCII ST R0,BtoA_Save0
	ST R1,BtoA_Save1
	ST R2,BtoA_Save2
	ST R3,BtoA_Save3
	LD R1,BtoA_ASCIIBUFF ; R1 keeps track of output string.
	ADD R0,R0,#0 ; R0 contains the binary value.
	BRn NegSign ;
	LD R2,ASCIIplus ; First store the ASCII plus sign.
	STR R2,R1,#0
	BRnzp Begin100
NegSign LD R2,ASCIIminus ; First store ASCII minus sign.
	STR R2,R1,#0
	NOT R0,R0 ; Convert the number to absolute
	ADD R0,R0,#1 ; value; it is easier to work with.
;
Begin100 LD R2,ASCIIoffset ; Prepare for "hundreds" digit.
;
	LD R3,Neg100 ; Determine the hundreds digit.
Loop100 ADD R0,R0,R3
	BRn End100
	ADD R2,R2,#1
	BRnzp Loop100
;
End100  STR R2,R1,#1 ; Store ASCII code for hundreds digit.
	LD R3,Pos100
	ADD R0,R0,R3 ; Correct R0 for one-too-many subtracts.
;
	LD R2,ASCIIoffset ; Prepare for "tens" digit.
;
Loop10  ADD R0,R0,#-10 ; Determine the tens digit.
	BRn End10
	ADD R2,R2,#1
	BRnzp Loop10
;
End10 	STR R2,R1,#2 ; Store ASCII code for tens digit.
	ADD R0,R0,#10 ; Correct R0 for one-too-many subtracts.
Begin1	LD R2,ASCIIoffset ; Prepare for "ones" digit.
	ADD R2,R2,R0
	STR R2,R1,#3
	LD R0,BtoA_Save0
	LD R1,BtoA_Save1
	LD R2,BtoA_Save2
	LD R3,BtoA_Save3
	RET
;
ASCIIplus .FILL x002B
ASCIIminus .FILL x002D
ASCIIoffset .FILL x0030
Neg100 	.FILL #-100
Pos100 	.FILL #100
BtoA_Save0 .BLKW #1
BtoA_Save1 .BLKW #1
BtoA_Save2 .BLKW #1
BtoA_Save3 .BLKW #1
BtoA_ASCIIBUFF .FILL ASCIIBUFF

;
; Subroutine to pop the top two elements from the stack,
; add them, and push the sum onto the stack. R6 is
; the stack pointer.
;
OpAdd	ST R0,OpAdd_Save0
	ST R1,OpAdd_Save1
	ST R5,OpAdd_Save5
	ST R7,OpAdd_Save7
	JSR POP ; Get first source operand.
	ADD R5,R5,#0 ; Test if POP was successful.
	BRp OpAdd_Exit ; Branch if not successful.
	ADD R1,R0,#0 ; Make room for second operand.
	JSR POP ; Get second source operand.
	ADD R5,R5,#0 ; Test if POP was successful.
	BRp OpAdd_Restore1 ; Not successful, put back first.
	ADD R0,R0,R1 ; THE Add.
	JSR RangeCheck ; Check size of result.
	ADD R5,R5,#0 ; Check R5 for success/failure.
	BRp OpAdd_Restore2 ; Out of range, restore both.
	JSR PUSH ; Push sum on the stack.
	BRnzp OpAdd_Exit ; On to the next task...
OpAdd_Restore2 ADD R6,R6,#-1 ; Decrement stack pointer.
OpAdd_Restore1 ADD R6,R6,#-1 ; Decrement stack pointer.
OpAdd_Exit LD R0,OpAdd_Save0
	LD R1,OpAdd_Save1
	LD R5,OpAdd_Save5
	LD R7,OpAdd_Save7
	RET
OpAdd_Save0 .BLKW #1
OpAdd_Save1 .BLKW #1
OpAdd_Save5 .BLKW #1
OpAdd_Save7 .BLKW #1

;
; Subroutine to check that a value is
; between -999 and +999.
;
RangeCheck LD R5,Neg999
	ADD R5,R0,R5 ; Recall that R0 contains the
	BRp BadRange ; result being checked.
	LD R5,Pos999
	ADD R5,R0,R5
	BRn BadRange
	AND R5,R5,#0 ; R5 <-- success
	RET
	
BadRange ST R0,RangeCheck_Save0
	LEA R0,RangeErrorMsg
	ST R7,RangeCheck_Save7
	PUTS ; Output character string
	LD R7,RangeCheck_Save7
	AND R5,R5,#0 ;
	ADD R5,R5,#1 ; R5 <-- failure
	LD R0,RangeCheck_Save0
	RET
	
Neg999 .FILL #-999
Pos999 .FILL #999
RangeErrorMsg .FILL x000A
.STRINGZ "Error: Number is out of range."
RangeCheck_Save0 .BLKW #1
RangeCheck_Save7 .BLKW #1

;
; Two values are popped from the stack, multiplied, and if
; their product is within the acceptable range, the result
; is pushed onto the stack. R6 is the stack pointer.
;
OpMult	ST R0,OpMult_Save0
	ST R1,OpMult_Save1
	ST R2,OpMult_Save2
	ST R3,OpMult_Save3
	ST R5,OpMult_Save5
	ST R7,OpMult_Save7
	AND R3,R3,#0 ; R3 holds sign of multiplier.
	JSR POP ; Get first source from stack.
	ADD R5,R5,#0 ; Test for successful POP.
	BRp OpMult_Exit ; Failure
	ADD R1,R0,#0 ; Make room for next POP.
	JSR POP ; Get second source operand.
	ADD R5,R5,#0 ; Test for successful POP.
	BRp OpMult_Restore1 ; Failure; restore first POP.
	ADD R2,R0,#0 ; Moves multiplier, tests sign.
	BRzp PosMultiplier
	ADD R3,R3,#1 ; Sets FLAG: Multiplier is neg.
	NOT R2,R2
	ADD R2,R2,#1 ; R2 contains -(multiplier).
PosMultiplier AND R0,R0,#0 ; Clear product register.
	ADD R2,R2,#0
	BRz PushMult ; Multiplier = 0, Done.
;
	MultLoop ADD R0,R0,R1 ; THE actual "multiply"
	ADD R2,R2,#-1 ; Iteration Control
	BRp MultLoop
;
	JSR RangeCheck
	ADD R5,R5,#0 ; R5 contains success/failure.
	BRp OpMult_Restore2
;
	ADD R3,R3,#0 ; Test for negative multiplier.
	BRz PushMult
	NOT R0,R0 ; Adjust for
	ADD R0,R0,#1 ; sign of result.
PushMult JSR PUSH ; Push product on the stack.
	BRnzp OpMult_Exit
OpMult_Restore2 ADD R6,R6,#-1 ; Adjust stack pointer.
OpMult_Restore1 ADD R6,R6,#-1 ; Adjust stack pointer.
OpMult_Exit LD R0,OpMult_Save0
	LD R1,OpMult_Save1
	LD R2,OpMult_Save2
	LD R3,OpMult_Save3
	LD R5,OpMult_Save5
	LD R7,OpMult_Save7
	RET
OpMult_Save0 .BLKW #1
OpMult_Save1 .BLKW #1
OpMult_Save2 .BLKW #1
OpMult_Save3 .BLKW #1
OpMult_Save5 .BLKW #1
OpMult_Save7 .BLKW #1

; Subroutine to pop the top of the stack, form its negative,
; and push the result onto the stack.
;
OpNeg	ST R0,OpNeg_Save0
	ST R5,OpNeg_Save5
	ST R7,OpNeg_Save7
	JSR POP ; Get the source operand.
	ADD R5,R5,#0 ; Test for successful pop
	BRp OpNeg_Exit ; Branch if failure.
	NOT R0,R0
	ADD R0,R0,#1 ; Form the negative of source.
	JSR PUSH ; Push result onto the stack.
;
OpNeg_Exit LD R0,OpNeg_Save0
	LD R5,OpNeg_Save5
	LD R7,OpNeg_Save7
	RET
OpNeg_Save0 .BLKW #1
OpNeg_Save5 .BLKW #1
OpNeg_Save7 .BLKW #1


; This subroutine takes a sequence of not more than three decimal digits
; typed by the user, converts its ASCII string to a binary value using the
; ASCIItoBinary subroutine, and pushes the binary value onto the stack.
; Anything else typed results in an error message.
;
PushValue ST R0,PushValue_Save0
	ST R1,PushValue_Save1
	ST R2,PushValue_Save2
	ST R7,PushValue_Save7
	LD R1,PushValue_ASCIIBUFF ; R1 points to string being
	LD R2,MaxDigits ; generated.
;
ValueLoop ADD R3,R0,x-0D ; Test for line feed, x0A
	BRz GoodInput
	ADD R2,R2,#0
	BRz TooLargeInput
	LD R3,NEGASCII0
	ADD R3,R0,R3
	BRn NotInteger
	LD R3,NEGASCII9
	ADD R3,R0,R3
	BRp NotInteger
	ADD R2,R2,#-1 ; Still room for more digits.
	STR R0,R1,#0 ; Store last character read.
	ADD R1,R1,#1
	GETC
	OUT ; Echo it.
	BRnzp ValueLoop
;
GoodInput LD R2,PushValue_ASCIIBUFF
	NOT R2,R2
	ADD R2,R2,#1
	ADD R1,R1,R2 ; R1 now contains no. of char.
	BRz NoDigit
	JSR ASCIItoBinary
	JSR PUSH
	BRnzp PushValue_Done
	NoDigit LEA R0,NoDigitMsg
	PUTS
	BRnzp PushValue_Done
; 
NotInteger GETC ; Spin until carriage return.
	OUT
	ADD R3,R0,x-0D ; Test for line feed, x0A
	BRnp NotInteger
	LEA R0,NotIntegerMsg
	PUTS
	BRnzp PushValue_Done
	
; 
TooLargeInput GETC ; Spin until carriage return.
	OUT
	ADD R3,R0,x-0D ; Test for line feed, x0A
	BRnp TooLargeInput
	LEA R0,TooManyDigits
	PUTS
; 
PushValue_Done LD R0,PushValue_Save0
	LD R1,PushValue_Save1
	LD R2,PushValue_Save2
	LD R7,PushValue_Save7
	RET
	
TooManyDigits .FILL x000A
	.STRINGZ "Too many digits"
NoDigitMsg .FILL x000A
	.STRINGZ "No number entered"
NotIntegerMsg .FILL x000A
	.STRINGZ "Not an integer"
MaxDigits .FILL x0003
NegASCII0 .FILL x-30
NegASCII9 .FILL x-39
PushValue_ASCIIBUFF .FILL ASCIIBUFF
PushValue_Save0 .BLKW #1
PushValue_Save1 .BLKW #1
PushValue_Save2 .BLKW #1
PushValue_Save7 .BLKW #1

; This subroutine POPs a value from the stack and puts it in
; R0 before returning to the calling program. R5 is used to
; report success (R5 = 0) or failure (R5 = 1) of the POP operation.
POP 	LD R0,POP_StackBase
	NOT R0,R0 ; R0 = -(addr. of StackBase + 1)
	ADD R0,R0,R6 ; R6 = StackPointer
	BRz Underflow
	LDR R0,R6,#0 ; The actual POP
	ADD R6,R6,#1 ; Adjust StackPointer
	AND R5,R5,#0 ; R5 <-- success
	RET
; 
Underflow LEA R0,UnderflowMsg
	ST R7,POP_Save7	
	PUTS ; Print error message.
	LD R7,POP_Save7		
	AND R5,R5,#0
	ADD R5,R5,#1 ; R5 <-- failure
	RET
	
UnderflowMsg .FILL x000A
.STRINGZ "Error: Too Few Values on the Stack."
POP_StackBase .FILL StackBase
POP_Save7 .BLKW #1
; This subroutine PUSHes on the stack the value stored in R0.
; R5 is used to report success (R5 = 0) or failure (R5 = 1) of
; the PUSH operation.
PUSH 	ST R1,PUSH_Save1 ; R1 is needed by this routine.
	LD R1,PUSH_StackMax
	NOT R1,R1
	ADD R1,R1,#1 ; R1 = - addr. of StackMax
	ADD R1,R1,R6 ; R6 = StackPointer
	BRz Overflow
	ADD R6,R6,#-1 ; Adjust StackPointer for PUSH.
	STR R0,R6,#0 ; The actual PUSH
	LD R1,PUSH_Save1 ; Restore R1.
	AND R5,R5,#0 ; R5 <-- success
	RET
; 
Overflow LEA R0,OverflowMsg
	ST R7,PUSH_Save7	
	PUTS
	LD R7, PUSH_Save7
	LD R1,PUSH_Save1 ; Restore R1.
	AND R5,R5,#0
	ADD R5,R5,#1 ; R5 <-- failure
	RET
PUSH_Save1 .BLKW #1
PUSH_Save7 .BLKW #1
OverflowMsg .FILL x000A
.STRINGZ "Error: Stack is Full."
PUSH_StackMax .FILL StackMax

; This subroutine calls BinarytoASCII to convert the 2’s complement
; number on the top of the stack into an ASCII character string, and
; then calls PUTS to display that number on the screen.
OpDisplay ST R0,OpDisplay_Save0
	ST R5,OpDisplay_Save5
	ST R7,OpDisplay_Save7
	JSR POP ; R0 gets the value to be displayed.
	ADD R5,R5,#0
	BRp OpDisplay_DONE ; POP failed, nothing on the stack.
	JSR BinarytoASCII
	LD R0,NewlineChar
	OUT
	LD R0,OpDisplay_ASCIIBUFF
	PUTS
	ADD R6,R6,#-1 ; Push displayed number back on stack.
; 
OpDisplay_DONE LD R0,OpDisplay_Save0
	LD R5,OpDisplay_Save5
	LD R7,OpDisplay_Save7
	RET
NewlineChar .FILL x000A
OpDisplay_ASCIIBUFF .FILL ASCIIBUFF
OpDisplay_Save0 .BLKW #1
OpDisplay_Save5 .BLKW #1
OpDisplay_Save7 .BLKW #1
	
;
; This routine clears the stack by resetting the stack pointer (R6).
;
OpClear LD R6,OpClear_StackBase ; Initialize the Stack Pointer.
	ADD R6,R6,#1 ; R6 = StackBase + 1 --> empty stack
	RET
OpClear_StackBase .FILL StackBase

	.END
