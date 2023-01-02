	.ORIG x3000

    RST R1
    RST R2
    RST R3

    ADD R1, R1, #2
    ADD R2, R2, #3
    ADD R3, R3, #2

    SUB R1, R2, R3

	.END