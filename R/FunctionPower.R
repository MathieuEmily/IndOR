###################
## Odds matrices ##
###################

OddsMat <- function(alpha,theta1,theta2=theta1,type)
{
	if (type=="N"){return(OddsN(alpha,theta1))}
	if (type=="1L"){return(Odds1L(alpha,theta1))}
	if (type=="ME"){return(OddsME(alpha,theta1,theta2))}
	if (type=="RR"){return(OddsRR(alpha,theta1))}
	if (type=="DD"){return(OddsDD(alpha,theta1))}
	if (type=="RD"){return(OddsRD(alpha,theta1))}
	if (type=="IME"){return(OddsIME(alpha,theta1))}
	if (type=="I"){return(OddsI(alpha,theta1))}
	if (type=="T"){return(OddsT(alpha,theta1))}
	if (type=="Mod"){return(OddsMod(alpha,theta1))}
	if (type=="XOR"){return(OddsXOR(alpha,theta1))}
}

OddsN <- function(a,theta)
{
	return(c(a,a,a,a,a,a,a,a,a))	
}

Odds1L <- function(a,theta)
{
	return(c(a,a,a,a,a,a,a*(1+theta),a*(1+theta),a*(1+theta)))
}

OddsME <- function(a,theta,the2="1")
{
	if (the2=="1")
	{theta2=theta}
	else {theta2=the2}
	return(c(a*(1+theta)^0*(1+theta2)^0,a*(1+theta)^1*(1+theta2)^0,a*(1+theta)^2*(1+theta2)^0,a*(1+theta)^0*(1+theta2)^1,a*(1+theta)^1*(1+theta2)^1,a*(1+theta)^2*(1+theta2)^1,a*(1+theta)^0*(1+theta2)^2,a*(1+theta)^1*(1+theta2)^2,a*(1+theta)^2*(1+theta2)^2))
}

OddsRR <- function(a,theta)
{
	return(c(a,a,a,a,a,a,a,a,a*(1+theta)))
}

OddsDD <- function(a,theta)
{
	return(c(a,a,a,a,a*(1+theta)^1,a*(1+theta)^1,a,a*(1+theta)^1,a*(1+theta)^1))
}

OddsRD <- function(a,theta)
{
	return(c(a,a,a,a,a,a,a,a*(1+theta)^1,a*(1+theta)^1))
}

OddsIME <- function(a,theta)
{
	return(c(a,a,a,a,a*(1+theta)^1,a*(1+theta)^2,a,a*(1+theta)^2,a*(1+theta)^4))
}


OddsI <- function(a,theta)
{
	return(c(a,a,a,a,a*(1+theta),a,a,a,a))
}


OddsT <- function(a,theta)
{
	return(c(a,a,a,a,a,a*(1+theta)^1,a,a*(1+theta)^1,a*(1+theta)^1))
}

OddsMod <- function(a,theta)
{
	return(c(a,a,a,a,a,a*(1+theta)^1,a*(1+theta)^1,a*(1+theta)^1,a*(1+theta)^1))
}

OddsXOR <- function(a,theta)
{
	return(c(a,a,a*(1+theta)^1,a,a,a*(1+theta)^1,a*(1+theta)^1,a*(1+theta)^1,a))
}

###############################
## Theoretical Probabilities ##
###############################

Theo_P_H0_HWE_LD <- function(pA,pB,r)
{
	P0=c(rep(0,9))
	pa=1-pA
	pb=1-pB
	D <- r*sqrt(pA*pB*pa*pb)
	pAB <- D+pA*pB
	paB <- -D+pa*pB
	pAb <- -D+pA*pb
	pab <- D+pa*pb
	if (pAB > 1 | paB > 1 | pAb > 1 | pab > 1 | pAB < 0 | paB < 0 | pAb < 0 | pab < 0)
	{
		print("ERROR R2 impossible!!")
	} 
	else {
		#null hypothesis 
		P0[1]=pAB^2
		P0[2]=2*pAB*pAb
		P0[3]=pAb^2
		P0[4]=2*pAB*paB
		P0[5]=2*pAB*pab+2*pAb*paB
		P0[6]=2*pAb*pab
		P0[7]=paB^2
		P0[8]=2*paB*pab
		P0[9]=pab^2
		return(P0)
	}
}


##########################
## Prevalence and theta ##
##########################


GetPrevalence <- function(alpha,theta,pAB,type)
{
	pDisease <- 0
	odds <- OddsMat(a=alpha,theta1=theta,theta2=theta,type=type)
	pDiseaseCond <- odds/(1+odds)
	for (i in 1:9)
	{
		pDisease <- pDisease+pDiseaseCond[i]*pAB[i]
		
	}
	return(pDisease)
}


GetTheta <- function(theta,alpha,pAB,type,prevalence)
{
	p <- GetPrevalence(alpha=alpha,theta=theta,pAB,type=type)
	return(abs(p-prevalence))
}


OptimTheta <- function(alpha,pAB,type,prevalence)
{
	res <- optimize(f=GetTheta,interval=c(0,100),alpha=alpha,pAB=pAB,type=type,prevalence=prevalence)
	return(res$minimum)

}
