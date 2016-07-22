\name{IndOR}
\alias{IndOR}
\title{SNPxSNP interaction testing based on odds-ratio}
\usage{
IndOR(x)
}
\arguments{
\item{x}{numeric or factor vector with exactly two different values. \code{Y} is
the response variable and should be of length equal to the number of rows of
\code{G1} and \code{G2} arguments (number of individuals).}
}

\value{
A list containing the following components:

	\item{statistic}{The value of the test statistic}
	\item{p.value}{The p-value for the test}
}
\description{
}
\details{
}
\examples{
}
\references{
Mathieu Emily (2012) IndOR: A new statistical procedure to test for SNP-SNP epistasis in Genome-Wide Association Studies, Statistics In Medicine, 31(21) :2359-2373\cr
}
\seealso{
\code{\link{getPower}}
}

