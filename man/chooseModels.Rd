\name{chooseModels}
\alias{chooseModels}
\alias{chooseOptimal}
\title{
Selecting a subset of fitted models
}
\description{
The function chooseModels extracts a sublist of models that match constraints on the number of components or on the model structure. The function chooseOptimal returns the model which is the best according the given model selection criteria. 
}
\usage{
chooseModels(models, kList = NULL, struct = NULL)

chooseOptimal(models, penalty=2, ...)
}

\arguments{
  \item{models}{an object of the class \code{\link{mModelList}} which represents a list of fitted models.}
  \item{kList}{a vector which specifies the requested numbers of Gaussian components (constraints on the number of components). }
  \item{struct}{a vector which specifies four letter abbreviations of names of the requested model structures (constraints on the model structure). }
  \item{penalty}{a penalty parameter in the GIC criteria. This parameter can be a single number or a string, either "BIC" or "AIC".   }
  \item{...}{other arguments that will be passed to the getGIC function.}
}
\details{
  The function chooseModels() extracts a sublist of models from the \code{models} argument.
  The returned sublist is also an object of the class \code{\link{mModelList}} and is composed of models that simultaneously satisfy the restrictions of the number of Gaussian components defined by \code{kList} and restrictions of the model structure defined by \code{struct}.
  If the argument \code{kList} is set to NULL then no restrictions of the number of components are applied, same with the argument \code{struct}.

  The function chooseOptimal() returns an object of the class \code{\link{mModel}} which is the single model that has the best (smallest) GIC score.
}
\value{
An object of the class \code{\link{mModelList}} or \code{\link{mModel}}.
}
\references{
  http://bgmm.molgen.mpg.de
}
\author{
Przemyslaw Biecek
}
\examples{
 simulated = simulateData(d=2, k=3, n=100, m=50, cov="0", within="E", n.labels=2)
 
 models3 = beliefList(X=simulated$X, knowns=simulated$knowns, B=simulated$B,
               kList=2:4, mean="D", within="D")
 plotGIC(models3, penalty="BIC")

## Do not run
## It could take more than one minute
# simulated = simulateData(d=2, k=3, n=300, m=60, cov="0", within="E", n.labels=2)
# 
# models3 = beliefList(X=simulated$X, knowns=simulated$knowns, B=simulated$B,
#               kList=2:7, mean="D")
# plotGIC(models3, penalty="BIC")
#
# models4 = chooseModels(models3, kList=2:5, struct=c("DDDD","DDED","DDE0"))
# plot(models4)
# plotGIC(models4, penalty="BIC")
#
# model4 = chooseOptimal(models3, "BIC")
# plot(model4)
}
