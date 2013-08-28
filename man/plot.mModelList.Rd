\name{plot.mModelList}
\alias{plot.mModelList}
\title{
Plotting a graphical visualization of a model or a list of models
}
\description{
The function \code{plot.mModelList()} creates a grid of panels and then plots a set of input fitted models in the consecutive panels. The \code{plot.mModel()} function is used to plot each single model.
}
\usage{
\method{plot}{mModelList}(x, ...)
}

\arguments{
  \item{x}{an object of the class \code{mModelList}.  }
  \item{\dots}{graphical arguments that are passed to underlying \code{plot()} function.  }
}
\details{
The argument \code{x} is a list of models. If these models differ both by component numbers and by the model structures, 
in the resulting grid of panels columns correspond to the different model structures while rows correspond to the different component numbers. 

If considered models differ only by component numbers or only by the model structures, the grid of panels is as close to square as possible and consecutive panels contain consecutive models from the list of models \code{x}.
}
\references{
  Przemyslaw Biecek, Ewa Szczurek, Martin Vingron, Jerzy Tiuryn (2012), The R Package bgmm: Mixture Modeling with Uncertain Knowledge, Journal of Statistical Software.
}
\seealso{\code{\link{plot.mModel}}}
\author{
Przemyslaw Biecek
}
\examples{
 simulated = simulateData(d=2, k=3, n=100, m=60, cov="0", within="E", n.labels=2)
 models1=mModelList(X=simulated$X, knowns=simulated$knowns, B=simulated$B, 
             kList=3:4, mean=c("D","E"), between="D", within="D", 
             cov="0", funct=belief)
 plot(models1)

## Do not run
## It could take more than one minute
# simulated = simulateData(d=2, k=3, n=300, m=60, cov="0", within="E", n.labels=2)
# 
# models1=mModelList(X=simulated$X, knowns=simulated$knowns, B=simulated$B, 
#             kList=3, mean=c("D","E"), between=c("D","E"), within=c("D","E"), 
#             cov=c("D","0"), funct=belief)
# plot(models1)
#
# models2 = beliefList(X=simulated$X, knowns=simulated$knowns, B=simulated$B,
#                kList=2:7, mean="D", between="D", within="E", cov="0")
# plot(models2)
# 
# models3 = beliefList(X=simulated$X, knowns=simulated$knowns, B=simulated$B,
#               kList=2:7, mean="D")
# plot(models3)
}
