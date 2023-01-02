	.ORIG x3000
    RST R2
    RST R3

    ADD R2, R2, #3
    ADD R3, R3, #-3

    MLT R1, R2, R3
	.END