\name{getPower}
\alias{getPower}
\title{Power estimation for IndOR}
\usage{
getPower(pDisease,pDiseaseBase,type,pA,pB,nInd,r,ratioCC)
}
\arguments{
\item{pDisease}{numeric. \code{pDisease} is unconditional probability of the disease.}

\item{pDiseaseBase}{numeric. \code{pDisease} is probability of the disease conditional to the baseline odds.}

\item{type}{numeric. \code{type} is the disease model.}

\item{pA}{numeric. \code{pA} is the MAF for the first SNP.}

\item{pB}{numeric. \code{pB} is the MAF for the second SNP.}

\item{nInd}{numeric. \code{nInd} is the number of individuals.}

\item{r}{numeric. \code{r} is the correlation measure of Linkage Disequilibrium between the two SNPs.}

\item{ratioCC}{numeric. \code{ratioCC} is Case-to-controls ratio.}

}
\value{
A list with class \code{"GGItest"} containing the following components:

	\item{alpha}{The baseline odds of the disease model}
	\item{theta}{The strength of the disease model}
	\item{power}{The power of IndOR to detect the disease model}
}
\description{
\code{getPower} computes the theoretical power for IndOR to detect a predifined disease model.}
\details{
}
\examples{
}
\references{
Mathieu Emily (2012) IndOR: A new statistical procedure to test for SNP-SNP epistasis in Genome-Wide Association Studies, Statistics In Medicine, 31(21) :2359-2373\cr
}
\seealso{
\code{\link{IndOR}}
}

