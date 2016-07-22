IndOR <- function(M) ##### M is a 9x2 table where the first column is made by counts for the nine genotypes in cases and the second column stores counts for controls
{
	B <- GetBLog(M)
	VarCov <- matrix(-1,ncol=4,nrow=4)
	for (i in 1:4)
	{	
		VarCov[i,i] <- VarDelta(M[,1],ind=i)+VarDelta(M[,2],ind=i)
	}
	for (i in 1:3)
	{
		for (j in (i+1):4)
		{
			tmp <- CovDelta(M[,1],ind1=i,ind2=j)+CovDelta(M[,2],ind1=i,ind2=j)
			VarCov[i,j] <- tmp
			VarCov[j,i] <- tmp
		}
	}
	Inv <- try(solve(VarCov))
	if (!inherits(Inv,"try-error")){S <- B%*%solve(VarCov)%*%B;return(list(statistic=S,p.value=1-pchisq(S,df=4)))}
	else (return((-1)))
}