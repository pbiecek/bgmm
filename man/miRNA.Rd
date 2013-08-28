\name{miRNA}
\alias{miRNA}
\alias{miR1Data}
\alias{miR124Data}
\alias{miRNABeliefs}
\alias{miRNAClass}
\docType{data}
\title{miRNA transfection data for miR1 and miR124 target genes}
\description{
  miRNA transfection data (Lim et al., 2005) and knowledge from computational
  miRNA target predictions.
}
\usage{data(miRNA)}
\format{
  miR1Data vector: 117, 
  miR124Data vector: 117, 
  miRNABeliefs matrix of example certainty:  26 x 2, 
  miRNAClass vector:  117
}
\details{
  \code{miR1Data} Log2 expression ratios of miR1 transfection versus
    wild type, for 117 genes.
  \code{miR124Data} Log2 expression ratios of miR124 transfection versus
    wild type, for 117 genes.
  \code{miRNABeliefs} Gives the certainty (belief/plausibility) for
    each out of 26 example miRNA targets to belong to their cluster.
  \code{miRNAClass} Gives the true cluster for each gene. Cluster 1
    corresponds to the experimentally verified targets of miR1. Cluster 2
    corresponds to the targets of miR124.
}
\references{
  Lim, L. P., Lau, N. C., Garrett-Engele, P., Grimson, A., Schelter, J. M., Castle, J., Bartel,
D. P., Linsley, P. S., and Johnson, J. M. (2005). Microarray analysis shows that some
microRNAs downregulate large numbers of target mRNAs. Nature, 433(7027).
}
\author{
Ewa Szczurek
}
\seealso{\code{\link{Ste12}},\code{\link{CellCycle}}}
\examples{
library(bgmm)
data(miRNA)
print(miR1Data)
print(miR124Data)
print(miRNABeliefs)
print(miRNAClass)
}
\keyword{datasets}
