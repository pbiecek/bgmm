\name{Ste12}
\alias{Ste12Data}
\alias{Ste12}
\alias{Ste12Beliefs}
\alias{Ste12Binding}
\docType{data}
\title{Ste12 knockout data under pheromone treatment versus wild type;
  Examples of Ste12 targets; Binding p-values of Ste12 to those targets.}
\description{
  Ste12 knockout expression data (Roberts et al., 2002) and knowledge from a Ste12 binding  experiment (Harbison et al., 2004) used for identifying Ste12 target genes under pheromone treatment.
}
\usage{data(Ste12)}
\format{
  Ste12Data vector: 601
  Ste12Beliefs matrix of example certainty:  42 x 2
  Ste12Binding vector: 42
}
\details{
  \code{Ste12Data} Log2 expression ratios of Ste12 knockout versus
  wild type, both under 50nM alpha-factor treatment for 30min. This
  data is for 601 genes that had more than 1.5 fold change in expression
  after pheromone  treatment versus wild type.
  \code{Ste12Beliefs}: Gives the certainty (belief/plausibility) for
    each out of 42 example Ste12 targets to belong to their cluster.
  \code{Ste12Beliefs}: Gives the certainty (belief/plausibility) for
    each out of 42 example Ste12 targets to belong to their cluster. The
    42 examples were chosen to meet two criteria: (1) Had a binding
    p-value <0.0001 (see \code{Ste12Binding}), and (2) Had a 2-fold change in response to pheromone treatment (versus wild-type)
  \code{Ste12Binding}: Gives the binding p-value for each example Ste12 target (see \code{Ste12Belief}).
}
\references{
  Roberts, C. J., Nelson, B., Marton, M. J., Stoughton, R., Meyer, M. R., Bennett, H. A.,
  He, Y. D., Dai, H., Walker, W. L., Hughes, T. R., Tyers, M., Boone, C., and Friend,
  S. H. (2000). Signaling and Circuitry of Multiple MAPK Pathways Revealed by a
  Matrix of Global Gene Expression Profiles. Science, 287(5454), 873--880.

Harbison, C. T., Gordon, D. B., Lee, T. I., Rinaldi, N. J., Macisaac, K. D., Danford,
T. W., Hannett, N. M., Tagne, J.-B., Reynolds, D. B., Yoo, J., Jennings, E. G., Zeitlinger,
J., Pokholok, D. K., Kellis, M., Rolfe, P. A., Takusagawa, K. T., Lander, E. S.,
Gifford, D. K., Fraenkel, E., and Young, R. A. (2004). Transcriptional regulatory
code of a eukaryotic genome. Nature, 431(7004), 99--104.
}
\author{
Ewa Szczurek
}
\seealso{\code{\link{miRNA}},\code{\link{CellCycle}}}
\examples{
    data("Ste12")
    print(Ste12Data)
    print(Ste12Beliefs)
    print(Ste12Binding)
}
\keyword{datasets}
