\name{mModel}
\alias{mModel}
\alias{belief}
\alias{soft}
\alias{semisupervised}
\alias{supervised}
\alias{unsupervised}
\title{
Fitting Gaussian Mixture Model 
}
\description{
These functions fit different variants of Gaussian mixture models. These variants 
differ in the fraction of knowledge utilized into the the fitting procedure.
}
\usage{
belief(X, knowns, B = NULL, k = ifelse(!is.null(B), ncol(B), 
    ifelse(!is.null(P), ncol(P), length(unique(class)))), P = NULL, 
    class = map(B), init.params = init.model.params(X, knowns, 
        B = B, P = P, class = class, k = k), model.structure = getModelStructure(), 
    stop.likelihood.change = 10^-5, stop.max.nsteps = 100, trace = FALSE, 
    b.min = 0.025, 
    all.possible.permutations=FALSE, pca.dim.reduction = NA)
    
soft(X, knowns, P = NULL, k = ifelse(!is.null(P), ncol(P), 
    ifelse(!is.null(B), ncol(B), length(unique(class)))), B = NULL, 
    class = NULL, init.params = init.model.params(X, knowns, 
        class = class, B = P, k = k), 
    model.structure = getModelStructure(), stop.likelihood.change = 10^-5, 
    stop.max.nsteps = 100, trace = FALSE, b.min = 0.025, 
	all.possible.permutations=FALSE, pca.dim.reduction = NA, ...)    
    
semisupervised(X, knowns, class = NULL, k = ifelse(!is.null(class), 
    length(unique(class)), ifelse(!is.null(B), ncol(B), ncol(P))), 
    B = NULL, P = NULL, ..., init.params = NULL,
	all.possible.permutations=FALSE, pca.dim.reduction = NA)    
    
supervised(knowns, class = NULL, k = length(unique(class)), B = NULL, P = NULL, 
    model.structure = getModelStructure(), ...)

unsupervised(X, k, init.params=init.model.params(X, knowns=NULL, k=k), 
      model.structure=getModelStructure(), stop.likelihood.change=10^-5, 
      stop.max.nsteps=100, trace=FALSE, ...) 
}

\arguments{
  \item{X}{a data.frame with the unlabeled observations. The rows correspond to the observations while the columns to variables/dimensions of the data. }
  \item{knowns}{a data.frame with the labeled observations. The rows correspond to the observations while the columns to variables/dimensions of the data. }
  \item{B}{a beliefs matrix which specifies the distribution of beliefs for the labeled observations. The number of rows in B should equal the number of rows in the  data.frame \code{knowns}. It is assumed that both the observations in \code{B} and in \code{knowns} are given in the same order. Columns correspond to the model components. If matrix B is provided, the number of columns has to be less or equal \code{k}. Internally, the matrix \code{B} is completed to \code{k} columns.  }
  \item{P}{a  matrix of plausibilities, i.e., weights of the prior probabilities for the labeled observations. If matrix \code{P} is provided, the number of columns has to be less or equal \code{k}. The came conditions as for \code{B} apply. }
  \item{class}{a vector of classes/labels for the labeled observations. The number of its unique values has to be less or equal \code{k}. }
  \item{k}{a number of components, by default equal to the number of columns of \code{B}. }
  \item{init.params}{initial values for the estimates of the model parameters (means, variances and mixing proportions), by default derived with the
  use of the \code{\link{init.model.params}} function. }
  \item{stop.likelihood.change, stop.max.nsteps}{ the parameters for the EM algorithms defining the stop criteria, i.e., the minimum required improvement of loglikelihood and the maximum number of steps. }
  \item{trace}{if \code{trace=TRUE} the loglikelihoods for every step of EM algorithm are printed out.   }
  \item{model.structure}{an object returned by the \code{\link{getModelStructure}} function, which specifies constraints for the parameters of the model to be fitted. }
  \item{b.min}{this argument is passed to the \code{\link{init.model.params}} function.  }
  \item{\dots}{these arguments will be passed tothe \code{\link{init.model.params}} function.   }
  \item{all.possible.permutations}{If equal \code{TRUE}, all possible initial parameters' permutations of components are considered. Since there is kList! permutations,  model fitting is repeated kList! times. As a result, only the model with the highest likelihood is returned.}
  \item{pca.dim.reduction}{Since the fitting for high dimensional space is numerically a bad idea an attempt to PCA will be performed if \code{pca.dim.reduction !- FALSE}. If equal \code{NA} then the target dimension is data driven, if it's a number then this will be the target dimension.}
}
\details{
  In the \code{belief()} function, if the argument \code{B} is not provided, it is
  by default initialized from the argument \code{P}. If the argument \code{P} is not
  provided, \code{B} is derived from the \code{class} argument with the use of the function \code{get.simple.beliefs()} 
  which assigns \code{1-(k-1)*b.min} to the component given by \code{class}  and 
  \code{b.min} to all remaining components.

  In the \code{soft()} function, if the argument \code{P} is not provided, it is
  by default initialized from the argument \code{B}. If the argument \code{B} is not
  provided, \code{P} is derived from the \code{class} argument as in the \code{belief()}
  function.

  In the \code{supervised()} function,  if the argument \code{class} is not provided,
  it is by default initialized from argument \code{B} or \code{P}, taking the label of each
  observation as its most believed or plausible component (by the MAP rule).
  
  The number of columns in the beliefs matrix \code{B} or in the matrix of
  plausibilities \code{P} may be smaller than the number of model components
  defined by the argument \code{k}.  Such situation corresponds to the scenario
  when the user does not know any examples for some   component. In other words, this component is not used as a label for
  any observation, and thus can be omitted from the beliefs matrix. An
  equivalent would be to include a column for this component and fill it
  with beliefs/plausibilities equal 0.

  Slots in the returned object are listed in section Value.
  The returned object differs slighty with respect to the used function. Namely, the \code{belief()} function returns an object with the slot \code{B}. The function \code{soft()} returns an object with a slot \code{P}, while the functions \code{supervised()} and \code{semisupervised()} return objects with a slot \code{class} instead. 
  
  The object returned by the function \code{supervised()} does not have the slot \code{X}.
}
\value{
An object of the class \code{mModel}, with the following slots:
\item{pi}{ a vector with the fitted mixing proportions}
\item{mu}{ a matrix with the means' vectors, fitted for all components}
\item{cvar}{ a three-dimensional matrix with the covariance matrices, fitted for all components} 
\item{X}{ the unlabeled observations}
\item{knowns}{the labeled observations}
\item{B}{the beliefs matrix}
\item{n}{ the number of all observations}
\item{m}{ the number of the unlabeled observations}
\item{k}{the number of fitted model components}
\item{d}{the data dimension}
\item{likelihood}{the log-likelihood of the fitted model}
\item{n.steps}{the number of steps performed by the EM algorithm}
\item{model.structure}{the set of constraints kept during the fitting process.}
}
\references{
  Przemyslaw Biecek, Ewa Szczurek, Martin Vingron, Jerzy Tiuryn (2012), The R Package bgmm: Mixture Modeling with Uncertain Knowledge, Journal of Statistical Software.
}
\author{
Przemyslaw Biecek
}
\examples{
data(genotypes)

modelSupervised = supervised(knowns=genotypes$knowns, 
            class=genotypes$labels)
plot(modelSupervised)

modelSemiSupervised = semisupervised(X=genotypes$X, 
            knowns=genotypes$knowns, class = genotypes$labels)
plot(modelSemiSupervised)

modelBelief = belief(X=genotypes$X, 
            knowns=genotypes$knowns, B=genotypes$B)
plot(modelBelief)

modelSoft = soft(X=genotypes$X, 
            knowns=genotypes$knowns, P=genotypes$B)
plot(modelSoft)

modelUnSupervised = unsupervised(X=genotypes$X, k=3)
plot(modelUnSupervised)
}
