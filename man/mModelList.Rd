\name{mModelList}
\alias{mModelList}
\alias{beliefList}
\alias{softList}
\alias{semisupervisedList}
\alias{supervisedList}
\alias{unsupervisedList}
\title{
Fitting Gaussian mixture model or collection of models
}
\description{
These functions fit collection of models of one particular variant/class. 
Models to be fitted may differ in the requested number of Gaussian components or in the requested model structure.
}
\usage{
mModelList(X, knowns, B = NULL, P = NULL, class = NULL, kList = ncol(B), 
    init.params = NULL, stop.likelihood.change = 10^-5, stop.max.nsteps = 100, 
    trace = FALSE, mean = c("D", "E"), between = c("D", "E"), within = c("D", 
        "E"), cov = c("D", "0"), funct = belief, all.possible.permutations = FALSE, ...)
        
beliefList(..., funct=belief)

softList(..., funct=soft)

semisupervisedList(..., funct=semisupervised)

unsupervisedList(X, kList = 2, ...)
}

\arguments{
  \item{X}{a data.frame with the unlabeled observations. The rows correspond to the observations while the columns to variables/dimensions of the data. }
  \item{knowns}{a data.frame with the labeled observations. The rows correspond to the observations while the columns to variables/dimensions of the data.  }
  \item{B}{a beliefs matrix which specifies the distribution of beliefs for the labeled observations. The number of rows in B should equal the number of rows in the  data.frame \code{knowns}. It is assumed that both the observations in \code{B} and in \code{knowns} are given in the same order. Columns correspond to the model components. If matrix B is provided, the number of columns has to be less or equal \code{k}. Internally, the matrix \code{B} is completed to \code{k} columns. }
  \item{P}{a  matrix of plausibilities, i.e., weights of the prior probabilities for the labeled observations. If matrix \code{P} is provided, the number of columns has to be less or equal \code{k}. The came conditions as for \code{B} apply.}
  \item{class}{a vector of classes/labels for the labeled observations. The number of its unique values has to be less or equal \code{min(kList)}. }
  \item{kList}{a vector or a list with numbers of Gaussian components to fit. By default it is one number equal to the number of columns of \code{B}. }
  \item{init.params}{initial values for the estimates of the model parameters (means, variances and mixing proportions). The initial parameters are internally passed to the \code{funct} function. }
  \item{stop.likelihood.change, stop.max.nsteps, trace}{the parameters for the EM  algorithm. Internally, these parameters are passed to the \code{funct} function.  }
  \item{mean, between, within, cov}{four vectors which define the model structures for models to be fitted. For example, if \code{mean="E"}, only models with constrained means are considered (means of Gaussian components are forced to be equal). On the other hand if \code{mean=c("E", "D")}, both models with constrained means and models without constraint on the means are fitted. }
  \item{funct}{a function which fits a variant of Gaussian mixture model, one of the: \code{belief}, \code{soft}, \code{semisupervised} or \code{unsupervised} functions.  }
 \item{\dots}{arguments that are passed to function \code{funct}.  }
  \item{all.possible.permutations}{If equal \code{TRUE}, all possible initial parameters' permutations of components are considered. Since there is kList! permutations,  model fitting is repeated kList! times. As a result only the model with the highest likelihood is returned.}
}
\details{
  Arguments \code{kList}, as well as \code{mean}, \code{between}, \code{within}, and \code{cov} define the list of models to be fitted. All combinations of specified model sizes and model structures are considered. List of fitted models is returned as a result. 
  
  The argument  \code{funct} defines which variant of Gaussian mixture models should be used for model fitting. One can use the wrappers \code{beliefList()}, \code{softList()}, \code{semisupervisedList()}, \code{unsupervisedList()} which call the \code{mModelList()} function and have a prespecified argument \code{funct}.
}
\value{
An object of the class mModelList, with the following slots:
\item{models}{ a list of models, each of the class \code{\link{mModel}}}
\item{loglikelihoods}{ a vector with log likelihoods of the models from list \code{models}}
\item{names}{ a vector with names of the models from list \code{models}} 
\item{params}{ a vector with the number of parameters of models from list \code{models}}
\item{kList}{equals the input argument \code{kList}}
}
\references{
  Przemyslaw Biecek, Ewa Szczurek, Martin Vingron, Jerzy Tiuryn (2012), The R Package bgmm: Mixture Modeling with Uncertain Knowledge, Journal of Statistical Software.
}
\author{
Przemyslaw Biecek
}
\seealso{\code{\link{mModel}}, \code{\link{getModelStructure}}}
\examples{
 simulated = simulateData(d=2, k=3, n=100, m=60, cov="0", within="E", n.labels=2)
 
 models1=mModelList(X=simulated$X, knowns=simulated$knowns, B=simulated$B, 
             kList=3:4, mean=c("D","E"), between="D", within="D", 
             cov="0", funct=belief)

 plot(models1)
 plotGIC(models1, penalty="BIC")

## Do not run
## It could take more than one minute
# simulated = simulateData(d=2, k=3, n=300, m=60, cov="0", within="E", n.labels=2)
# 
# models1=mModelList(X=simulated$X, knowns=simulated$knowns, B=simulated$B, 
#             kList=3, mean=c("D","E"), between=c("D","E"), within=c("D","E"), 
#             cov=c("D","0"), funct=belief)
# plot(models1)
# plotGIC(models1, penalty="BIC")
# 
# models2 = beliefList(X=simulated$X, knowns=simulated$knowns, B=simulated$B,
#                kList=2:7, mean="D", between="D", within="E", cov="0")
# plot(models2)
# plotGIC(models2, penalty="BIC")
# 
# models3 = beliefList(X=simulated$X, knowns=simulated$knowns, B=simulated$B,
#               kList=2:7, mean="D")
# plotGIC(models3, penalty="BIC")
}
