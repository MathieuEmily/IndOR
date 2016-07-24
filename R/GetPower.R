getPower <- function(pDisease=0.5,pDiseaseBase=0.45,type=c("N","1L", "ME", "RR","DD","RD","IME",
                       "I", "T", "Mod","XOR"),pA=0.3,pB=0.3,nInd=1000,ratioCC=0.5)
{
	alpha <- pDiseaseBase/(1-pDiseaseBase)
	pAB <- Theo_P_H0_HWE_LD(pA=pA,pB=pB,r=r)
	theta <- OptimTheta(alpha=alpha,pAB=pAB,type=type,prevalence=pDisease)
	odds <- OddsMat(alpha=alpha,theta1=theta,theta2=theta,type=type)
	pDiseaseCond <- odds/(1+odds)
	pAB_Cases <- pDiseaseCond*pAB/pDisease
	pAB_Controls <- (1-pDiseaseCond)*pAB/(1-pDisease)
	nCases <- nInd
	nControls <- nCases*ratioCC
	TD <- cbind(nCases*pAB_Cases,nControls*pAB_Controls)
	power <- 1-pchisq(qchisq(0.95,df=4),df=4,ncp=max(0,IndOR(TD)$statistic))
	return(list(alpha=alpha,theta=theta,power=power))
}
