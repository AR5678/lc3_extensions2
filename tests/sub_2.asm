	.ORIG x3000

    RST R1
    RST R2

    ADD R1, R1, #2
    ADD R2, R2, #3

    SUB R1, R2, R2

	.END