getPower <- function(pDisease,pDiseaseBase,type,pA,pB,nInd,ylim=c(0,1),lege,r,ratioCC)
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
	IndOR <- 1-pchisq(qchisq(0.95,df=4),df=4,ncp=max(0,IndOR(TD)$statistic))
	return(list(alpha=alpha,theta=theta,IndOR=IndOR))
}