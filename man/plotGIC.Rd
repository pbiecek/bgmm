\name{plotGIC}
\alias{plotGIC}
\alias{getGIC}
\alias{getDF}
\title{
Plotting GIC scores 
}
\description{
The function \code{plotGIC()} plots the GIC scores for an input collection of models. The function \code{getGIC()} extracts GIC for given model and penalty function. The function \code{getDF()} extracts the number of degree of freedom for model parameters.
}
\usage{
plotGIC(models, penalty = 2, plot.it = TRUE, ...)

getGIC(model, p = 2, whichobs="unlabeled")

getDF(model)
}

\arguments{
  \item{models}{an object of the class \code{mModelList} or a matrix of GIC scores.
  }
  \item{model}{an object of the class \code{mModel}.
  }
  \item{penalty}{a penalty for the GIC criteria. This parameter can be a single number or a string, on of the "BIC", "AIC", "AIC3", "AIC4", "AICc", "AICu", "CAIC", "BIC", "MDL", "CLC", "ICL-BIC", "AWE".     
  }
  \item{p}{same as \code{penalty},  
  }
  \item{whichobs}{one of "unlabeled", "labeled", "all". This parameter specify which observations should be used in the likelihood and gic score calculation,  
  }
  \item{plot.it}{a logical value, if \code{TRUE} then the chart with the GIC scores will be plotted.
  }
  \item{...}{other arguments that will be passed to the getGIC function.}
}
\details{
The function \code{plotGIC()} calculates the GIC scores for each model from the \code{models} list and, given \code{plot.it=TRUE}, plots a dotchart with the calculated GIC scores. 

As a result the function \code{plotGIC()} returns a matrix with the calculated GIC scores. This matrix or its submatrix can be used in next call of the \code{plotGIC()} function as \code{models} argument. The columns of the  matrix correspond to different component numbers of the models, while the rows correspond to their structures. The structures are coded with four-letter strings. The letters refer, in order from left to right: first, the relation between the means' vectors of the components, which can either be equal (letter "E") or unconstrained ("D"). Second, the relation between covariance matrices, which can all either be equal ("E"), or unconstrained ("D"). Third, the relation between the data vector components (corresponding to data dimensions) within each covariance matrix, i.e. each covariance matrix can either have all variances equal to some constant and all covariances equal to some constant ("E") or can be unconstrained ("D"). Fourth, the covariances in each covariance matrix, which can either all be forced to equal 0 ("0") or be unconstrained ("D").

The best model, i.e. model with the smallest GIC score is marked with a star on the plotted chart. 
}
\value{
The matrix with GIC scores calculated for the list of models specified by the \code{models} argument. }
\references{
  Przemyslaw Biecek, Ewa Szczurek, Martin Vingron, Jerzy Tiuryn (2012), The R Package bgmm: Mixture Modeling with Uncertain Knowledge, Journal of Statistical Software.
}
\author{
Przemyslaw Biecek
}
\examples{
 simulated = simulateData(d=2, k=3, n=100, m=60, cov="0", within="E", n.labels=2)
 models1   = mModelList(X=simulated$X, knowns=simulated$knowns, B=simulated$B, 
             kList=3:4, mean=c("D","E"), between="D", within="D", 
             cov="0", funct=belief)
 plotGIC(models1, penalty="BIC")

## Do not run
## It could take more than one minute
# simulated = simulateData(d=2, k=3, n=300, m=60, cov="0", within="E", n.labels=2)
# 
# models1=mModelList(X=simulated$X, knowns=simulated$knowns, B=simulated$B, 
#             kList=3, mean=c("D","E"), between=c("D","E"), within=c("D","E"), 
#             cov=c("D","0"), funct=belief)
# plotGIC(models1, penalty="BIC")
# 
# models2 = beliefList(X=simulated$X, knowns=simulated$knowns, B=simulated$B,
#                kList=2:7, mean="D", between="D", within="E", cov="0")
# plotGIC(models2, penalty="BIC")
# 
# models3 = beliefList(X=simulated$X, knowns=simulated$knowns, B=simulated$B,
#               kList=2:7, mean="D")
# plotGIC(models3, penalty="BIC")
}
