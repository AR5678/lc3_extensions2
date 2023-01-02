	.ORIG x3000
    RST R1
    RST R2

    ADD R1, R1, #-3
    ADD R2, R2, #-3
    ; should be 9 in R1
    ; and -3 in R2
    MLT R1, R1, R2
	.END