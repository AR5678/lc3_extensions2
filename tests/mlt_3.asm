	.ORIG x3000
    RST R0
    RST R1
    RST R2

    ADD R1, R1, #-2
    ADD R2, R2, #2
    
    MLT R0, R1, R2
	.END