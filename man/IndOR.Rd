\name{IndOR}
\alias{IndOR}
\title{SNPxSNP interaction testing based on odds-ratio}
\usage{
IndOR(x)
}
\arguments{
\item{x}{A 9x2 \code{\link{matrix}} or \code{\link{data.frame}}}
}

\value{
A list containing the following components:

	\item{statistic}{The value of the test statistic}
	\item{p.value}{The p-value for the test}
}
\description{
	\code{IndOR} allows the search for SNP-SNP epistasis, i.e., for an association between a case-control phenotype and the interaction between two biallelic SNPs.
}
\details{
	\code{IndOR} is an implementation of the test proposed in Emily (2012). The procedure is based on the testing of the independence between odds-ratio. A matrix of genotype counts, \code{x} is taken as input. Dimensions for \code{x} should be 9 rows and 2 columns. Each column corresponds to the disease status (for example the first column for cases and the second column for controls). Each row corresponds to pair of genotypes as follows. If a (resp. b) is the minor allele for the first (resp. second) SNP then rows for \code{x} should be ordered as (AABB,AABb,AAbb,AaBB,AaBb,Aabb,aaBB,aaBb,aabb).
}
\examples{
## Example of genotype counts in cases and controls
Cases <- c(349,299,64,466,399,86,155,133,49)
Controls <- c(353,302,65,470,403,56,157,134,29)

## Counts are summarized in 9x2 matrix
x <- cbind(Cases,Controls)

## Test computation
IndOR(x)
}
\references{
Mathieu Emily (2012) IndOR: A new statistical procedure to test for SNP-SNP epistasis in Genome-Wide Association Studies, Statistics In Medicine, 31(21) :2359-2373\cr
}
\seealso{
}

