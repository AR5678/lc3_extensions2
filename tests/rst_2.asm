	.ORIG x3000
	; test that AND and ADD 
	; still work with RST
	ADD R3, R1, R2
	ADD R3, R2, #7

	AND R3, R1, R2
	AND R3, R2, #4
	RST R3
	RST R2
	ADD R3, R1, R2
	.END