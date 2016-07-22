GetBLog <- function(M)
{
	res <- rep(-1,times=4)
	res[1] <- log(M[5,1])-log(sum(M[c(4,5,6),1]))-log(sum(M[c(2,5,8),1]))-
	log(M[5,2])+log(sum(M[c(4,5,6),2]))+log(sum(M[c(2,5,8),2]))+
	log(M[1,2])-log(sum(M[c(1,2,3),2]))-log(sum(M[c(1,4,7),2]))-
	log(M[1,1])+log(sum(M[c(1,2,3),1]))+log(sum(M[c(1,4,7),1]))
	res[2] <- log(M[6,1])-log(sum(M[c(4,5,6),1]))-log(sum(M[c(3,6,9),1]))-
	log(M[6,2])+log(sum(M[c(4,5,6),2]))+log(sum(M[c(3,6,9),2]))+
	log(M[1,2])-log(sum(M[c(1,2,3),2]))-log(sum(M[c(1,4,7),2]))-
	log(M[1,1])+log(sum(M[c(1,2,3),1]))+log(sum(M[c(1,4,7),1]))
	res[3] <- log(M[8,1])-log(sum(M[c(7,8,9),1]))-log(sum(M[c(2,5,8),1]))-
	log(M[8,2])+log(sum(M[c(7,8,9),2]))+log(sum(M[c(2,5,8),2]))+
	log(M[1,2])-log(sum(M[c(1,2,3),2]))-log(sum(M[c(1,4,7),2]))-
	log(M[1,1])+log(sum(M[c(1,2,3),1]))+log(sum(M[c(1,4,7),1]))
	res[4] <- log(M[9,1])-log(sum(M[c(7,8,9),1]))-log(sum(M[c(3,6,9),1]))-
	log(M[9,2])+log(sum(M[c(7,8,9),2]))+log(sum(M[c(3,6,9),2]))+
	log(M[1,2])-log(sum(M[c(1,2,3),2]))-log(sum(M[c(1,4,7),2]))-
	log(M[1,1])+log(sum(M[c(1,2,3),1]))+log(sum(M[c(1,4,7),1]))
	return(res)
}

CoefDelta1 <- function(M)
{
	# M is 9 vector
	delta <- rep(-1,times=9)
	n <- sum(M)
	delta[1] <- -sqrt((n-M[1])/(n*M[1]))+
		sqrt((M[1]*(n-M[1]))/(n*(M[1]+M[2]+M[3])^2))+
		sqrt((M[1]*(n-M[1]))/(n*(M[1]+M[4]+M[7])^2))
	delta[2] <- sqrt((M[2]*(n-M[2]))/(n*(M[1]+M[2]+M[3])^2))-
		sqrt((M[2]*(n-M[2]))/(n*(M[2]+M[5]+M[8])^2))
	delta[3] <- sqrt((M[3]*(n-M[3]))/(n*(M[1]+M[2]+M[3])^2))
	delta[4] <- -sqrt((M[4]*(n-M[4]))/(n*(M[4]+M[5]+M[6])^2))+
		sqrt((M[4]*(n-M[4]))/(n*(M[1]+M[4]+M[7])^2))
	delta[5] <- sqrt((n-M[5])/(n*M[5]))-
		sqrt((M[5]*(n-M[5]))/(n*(M[4]+M[5]+M[6])^2))-
		sqrt((M[5]*(n-M[5]))/(n*(M[2]+M[5]+M[8])^2))
	delta[6] <- -sqrt((M[6]*(n-M[6]))/(n*(M[4]+M[5]+M[6])^2))
	delta[7] <- sqrt((M[7]*(n-M[7]))/(n*(M[1]+M[4]+M[7])^2))
	delta[8] <- -sqrt((M[8]*(n-M[8]))/(n*(M[2]+M[5]+M[8])^2))
	delta[9] <- 0
	return(delta)
}

CoefDelta2 <- function(M)
{
	# M is 9 vector
	delta <- rep(-1,times=9)
	n <- sum(M)
	delta[1] <- -sqrt((n-M[1])/(n*M[1]))+
		sqrt((M[1]*(n-M[1]))/(n*(M[1]+M[2]+M[3])^2))+
		sqrt((M[1]*(n-M[1]))/(n*(M[1]+M[4]+M[7])^2))
	delta[2] <- sqrt((M[2]*(n-M[2]))/(n*(M[1]+M[2]+M[3])^2))
	delta[3] <- -sqrt((M[3]*(n-M[3]))/(n*(M[3]+M[6]+M[9])^2))+
		sqrt((M[3]*(n-M[3]))/(n*(M[1]+M[2]+M[3])^2))
	delta[4] <- -sqrt((M[4]*(n-M[4]))/(n*(M[4]+M[5]+M[6])^2))+
		sqrt((M[4]*(n-M[4]))/(n*(M[1]+M[4]+M[7])^2))
	delta[5] <- -sqrt((M[5]*(n-M[5]))/(n*(M[4]+M[5]+M[6])^2))
	delta[6] <- sqrt((n-M[6])/(n*M[6]))-
		sqrt((M[6]*(n-M[6]))/(n*(M[4]+M[5]+M[6])^2))-
		sqrt((M[6]*(n-M[6]))/(n*(M[3]+M[6]+M[9])^2))
	delta[7] <- sqrt((M[7]*(n-M[7]))/(n*(M[1]+M[4]+M[7])^2))
	delta[8] <- 0
	delta[9] <- -sqrt((M[9]*(n-M[9]))/(n*(M[3]+M[6]+M[9])^2))
	return(delta)
}

CoefDelta3 <- function(M)
{
	# M is 9 vector
	delta <- rep(-1,times=9)
	n <- sum(M)
	delta[1] <- -sqrt((n-M[1])/(n*M[1]))+
		sqrt((M[1]*(n-M[1]))/(n*(M[1]+M[2]+M[3])^2))+
		sqrt((M[1]*(n-M[1]))/(n*(M[1]+M[4]+M[7])^2))
	delta[2] <- -sqrt((M[2]*(n-M[2]))/(n*(M[2]+M[5]+M[8])^2))+
		sqrt((M[2]*(n-M[2]))/(n*(M[1]+M[2]+M[3])^2))
	delta[3] <- sqrt((M[3]*(n-M[3]))/(n*(M[1]+M[2]+M[3])^2))
	delta[4] <- sqrt((M[4]*(n-M[4]))/(n*(M[1]+M[4]+M[7])^2))
	delta[5] <- -sqrt((M[5]*(n-M[5]))/(n*(M[2]+M[5]+M[8])^2))
	delta[6] <- 0
	delta[7] <- -sqrt((M[7]*(n-M[7]))/(n*(M[7]+M[8]+M[9])^2))+
		sqrt((M[7]*(n-M[7]))/(n*(M[1]+M[4]+M[7])^2))
	delta[8] <- sqrt((n-M[8])/(n*M[8]))-
		sqrt((M[8]*(n-M[8]))/(n*(M[7]+M[8]+M[9])^2))-
		sqrt((M[8]*(n-M[8]))/(n*(M[2]+M[5]+M[8])^2))
	delta[9] <- -sqrt((M[9]*(n-M[9]))/(n*(M[7]+M[8]+M[9])^2))
	return(delta)
}

CoefDelta4 <- function(M)
{
	# M is 9 vector
	delta <- rep(-1,times=9)
	n <- sum(M)
	delta[1] <- -sqrt((n-M[1])/(n*M[1]))+
		sqrt((M[1]*(n-M[1]))/(n*(M[1]+M[2]+M[3])^2))+
		sqrt((M[1]*(n-M[1]))/(n*(M[1]+M[4]+M[7])^2))
	delta[2] <- sqrt((M[2]*(n-M[2]))/(n*(M[1]+M[2]+M[3])^2))
	delta[3] <- -sqrt((M[3]*(n-M[3]))/(n*(M[3]+M[6]+M[9])^2))+
		sqrt((M[3]*(n-M[3]))/(n*(M[1]+M[2]+M[3])^2))
	delta[4] <- sqrt((M[4]*(n-M[4]))/(n*(M[1]+M[4]+M[7])^2))
	delta[5] <- 0
	delta[6] <- -sqrt((M[6]*(n-M[6]))/(n*(M[3]+M[6]+M[9])^2))
	delta[7] <- -sqrt((M[7]*(n-M[7]))/(n*(M[7]+M[8]+M[9])^2))+
		sqrt((M[7]*(n-M[7]))/(n*(M[1]+M[4]+M[7])^2))
	delta[8] <- -sqrt((M[8]*(n-M[8]))/(n*(M[7]+M[8]+M[9])^2))
	delta[9] <- sqrt((n-M[9])/(n*M[9]))-
		sqrt((M[9]*(n-M[9]))/(n*(M[7]+M[8]+M[9])^2))-
		sqrt((M[9]*(n-M[9]))/(n*(M[3]+M[6]+M[9])^2))
	return(delta)
}

VarDelta <- function(M,ind=1)
{
	if (ind==1)
	{
		Co <- CoefDelta1(M)
	}
	if (ind==2)
	{
		Co <- CoefDelta2(M)
	}
	if (ind==3)
	{
		Co <- CoefDelta3(M)
	}
	if (ind==4)
	{
		Co <- CoefDelta4(M)
	}
	n <- sum(M)
	res <- sum(Co^2)
	for (i in 1:8){
		for (j in (i+1):9){
			res <- res-2*Co[i]*Co[j]*sqrt((M[i]/(n-M[i]))*((M[j]/(n-M[j]))))
		}
	}
	return(res)
}

CovDelta <- function(M,ind1=1,ind2=2)
{
	if (ind1==1){Co1 <- CoefDelta1(M)}
	if (ind1==2){Co1 <- CoefDelta2(M)}
	if (ind1==3){Co1 <- CoefDelta3(M)}
	if (ind1==4){Co1 <- CoefDelta4(M)}
	if (ind2==1){Co2 <- CoefDelta1(M)}
	if (ind2==2){Co2 <- CoefDelta2(M)}
	if (ind2==3){Co2 <- CoefDelta3(M)}
	if (ind2==4){Co2 <- CoefDelta4(M)}
	n <- sum(M)
	res <- 0
	for (i in 1:9)
	{
		for (j in 1:9)
		{
			if (i==j){res <- res+Co1[i]*Co2[j]}
			else{res <- res-Co1[i]*Co2[j]*sqrt((M[i]/(n-M[i]))*((M[j]/(n-M[j]))))}
		}
	}
	return(res)
}
