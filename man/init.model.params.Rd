\name{init.model.params}
\alias{init.model.params}
\alias{init.model.params.knowns}
\title{
Initiation of model parameters 
}
\description{
Methods for the initiation of model parameters for the EM algorithm. Two initiation procedures are implemented. The first procedure is available by setting the argument \code{method='knowns'}. It takes into account only labeled observations and is thus suitable for datasets with a high percentage of labeled cases. The second is available by setting \code{method='all'} and does not take the labeling into account. 
}
\usage{
init.model.params(X = NULL, knowns = NULL, class = NULL, 
    k = length(unique(class)), method = "all", B = P, P = NULL)
}
\arguments{
  \item{X}{a \code{data.frame} with the unlabeled observations, its rows correspond to the observations while the columns correspond to variables/data dimensions.}
  \item{knowns}{a \code{data.frame} with the labeled observations, rows correspond to the observations while the columns correspond to variables/data dimensions.}
  \item{B}{a beliefs matrix with the distribution of beliefs for the labeled observations. If not specified and the argument \code{P} is given, the beliefs matrix is set to the value of \code{P}.}   
  \item{P}{a matrix of plausibilities, specified only for the labeled observations. The function assumes that the remaining observations are unlabeled and gives them
uniformly distributed plausibilities by default. If not specified and the argument \code{B} is given, the plausibilities matrix is set to the value of \code{B}.}
  \item{class}{\code{class} is a vector of labels for the known observations. If not specified, it is derived from eithter the argument \code{B} or \code{P} with the use of the MAP rule.}
  \item{k}{the desired number of model components. }
  \item{method}{a method for parameter initialization, one of following \code{c("knowns","all")}, see the section Details.}
 }
\details{
For \code{method='knowns'}, the initialization is based only on the labeled observations. i.e. those observations which have certain or probable components assigned. The initial model parameters
for each component are estimated in one step from the observations that are assigned to this component (as in fully supervised learning).

If \code{method='all'} (default), the initialization is based on all observations. In this case, to obtain the initial set of model components, we start by clustering the data using the k-means algorithm (repeated 10 times to get stable results). The only exception is for one dimensional data. In such a case the clusters are identified by dividing the data into \code{k} equal subsets of observations, where the subsets are separated by empirical quantiles c(1/2k, 3/2k, 5/2k, ..., (2k-1)/2k). After this initial clustering each cluster is linked to one model component and initial values for the model parameters are derived from the clustered observations. 
      
For the partially and semi-supervised methods, correspondence of labels from the initial clustering algorithm and labels for the observations in the \code{knowns} dataset rises a  technical problem. The cluster corresponding to component \code{y} should be as close as possible to the set of labeled observations with label \code{y}. 
      
      Note that for the unsupervised modeling this problem is irrelevant and any cluster may be used to initialize any component.
      
      To mach the cluster labels with the labels of model components a greedy heuristic is used. The heuristic calculates weighted distances between all possible pairs of cluster centers and sets of observations grouped by their labels. 
      In each step, the pair with a minimal distance is chosen (the pair: a group of observations with a common label and a cluster, for which the center of the group is the closest to the center of the cluster). For the chosen pair, the cluster is labeled with the same label as the group of observations. 
      Then, this pair is removed and the heuristic repeats for the reduced set of pairs. 
      }
\value{
A list with the following elements:
\item{pi}{ a vector of length \code{k} with the initial values for the mixing proportions.}
\item{mu}{ a matrix with the means' vectors with the initial values for \code{k} components.}
\item{cvar}{ a three-dimensional matrix with the covariance matrices with the initial values for \code{k} components.} 
}
\references{
  Przemyslaw Biecek, Ewa Szczurek, Martin Vingron, Jerzy Tiuryn (2012), The R Package bgmm: Mixture Modeling with Uncertain Knowledge, Journal of Statistical Software.
}
\author{
Przemyslaw Biecek
}
\examples{
 data(genotypes)
 initial.params = init.model.params(X=genotypes$X, knowns=genotypes$knowns,
									 class = genotypes$labels)
 str(initial.params)
}
