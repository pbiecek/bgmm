\name{getModelStructure}
\alias{getModelStructure}
\title{
Model structure 
}
\description{
This function creates an object which describes constraints over the model parameters.
}
\usage{
getModelStructure(mean = "D", between = "D", within = "D", cov = "D")
}
\arguments{
  Each argument is a single character, by default  equal "D" (Different). If argument is set to "E" (or "0" for the argument \code{cov}) then the given parameter is constrained.  By default all arguments are set to "D".

  \item{mean}{\code{mean}="E" forces equality of the means between the components,}
  \item{between}{\code{between}="E" forces equality of the covariance matrices among the components, }
  \item{within}{\code{within}="E" forces equality of variances within each covariance matrix i to some constant vi and equality of covariances to some constant wi}
  \item{cov}{\code{cov}="0" forces equality of covariances within each covariance matrix i to "0",}
}
\value{
List of four elements specifying the constraints on 1) relations between the component means, 2) relations between the covariance matrices of the model components, 3) relations within each covariance matrix and 4) the covariances within each matrix. By default, the function returns an unconstrained structure.
}
\references{
  http://bgmm.molgen.mpg.de
}
\author{
Ewa Szczurek
}
\examples{
 getModelStructure()
 getModelStructure(mean="E")
}
