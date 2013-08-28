\name{CellCycle}
\alias{CellCycle}
\alias{CellCycleData}
\alias{CellCycleBeliefs}
\alias{CellCycleCenters}
\alias{CellCycleClass}
\docType{data}
\title{Data for clustering of 384 cell cycle genes into five clusters
  corresponding to cell cycle phases}
\description{
  Time course expression data for 384 cell cycle genes (Cho et al.,
  1998). Literature examples of genes that should, and of genes that should not
  peak at each time point are given. For each cycle phase, there is a
  characteristic binary profile, stating when the phase occurs.
}
\usage{data(CellCycle)}
\format{
  CellCycleData list: 17x384
  CellCycleBeliefs list:  17x (35x2)
  CellCycleCenters matrix: 5x17
  CellCycleClass vector: 384
}
\details{
  \code{CellCycleData}: A list, where each entry corresponds to one
    time-point. A given time point entry contains a vector with expression
    ratios for 384 cell cycle genes measured in this time point.
  \code{CellCycleBeliefs}: A list, where each entry corresponds to one
    time-point. A given time point entry gives the certainty
    (belief/plausibility) for each out of 35 example genes. Out of the
    genes, seven are known to peak in this time point and the remaining
    28 are known to peak in other cycle phases. 
  \code{CellCycleCenters}: A matrix, where the columns are the 17
    time-points and the rows to the five cell phase clusters. A given
    entry in the matrix is equal to 1 if the genes from the cluster
    should peak in the time point, and 0 otherwise.
  \code{CellCycleClass}:Gives the true cluster for each gene. Each
    cluster corresponds to a cell cycle phase.
}

\references{
  Cho, R., Campbell, M., Winzeler, E., Steinmetz, L., Conway, A.,
  Wodicka, L., Wolfsberg, T., Gabrielian, A., Landsman, D., Lockhart,
  D., and Davis, R. (1998). A genome-wide transcriptional analysis of
  the mitotic cell cycle. Molecular Cell, 2(1), 65--73.

}
\author{
Ewa Szczurek
}
\seealso{\code{\link{miRNA}},\code{\link{Ste12}}}
\examples{
library(bgmm)
data(CellCycle)
print(CellCycleData)
print(CellCycleBeliefs)
print(CellCycleCenters)
print(CellCycleClass)
}
\keyword{datasets}
