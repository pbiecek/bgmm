\name{DEprobs}
\alias{DEprobs}
\title{
Signed probabilities of differential expression
}
\description{
	The \code{DEprobs} function is an application of mixture modeling to differential gene expression analysis. The function takes as input a two- or three-component model of one-dimensional gene expression data. The data is assumed to represent log fold change expression values and be negative when the corresponding genes are down-regulated. The function calculates probabilities of differential expression for the data and gives them a sign according to the sign of the data.
}
\usage{
DEprobs(model, verbose=FALSE)
}
\arguments{
  \item{model}{an object of the class \code{mModel},   }
  \item{verbose}{indicates whether log messeges should be prited out.}
}
\details{
Given the input \code{model}, the function identifies the component which corresponds to the differentially expressed genes as the one which looks differential according to the posterior probabilities. 

For input models with two Gaussian components the differential component should be the one with a broader range (encompassing the other), or the one with higher deviation from 0 (we assume the data are centered around 0).

For input models with three Gaussian components there are two differential components: one corresponding to the down-regulated genes, and one corresponding to the up-regulated genes. Those components are identified as the ones with the lowest and the highest mean, respectively.

For \code{verbose=TRUE} the index of the differential component is printed out. 
}
\value{
An list with the following elements:
\item{diff.p.X}{a vector with the calculated signed differential expression probabilities for the unlabeled observations in the dataset \code{model$X}.}
\item{diff.p.knowns}{ a vector with the calculated signed differential expression probabilities for the unlabeled observations in the dataset \code{model$knowns}. For \code{model$knowns=NULL} \code{diff.p.knowns} is also NULL(null).}
\item{diff.c}{the index (or two indexes, in case of a three-component input model) of the identified differential component.}
}
\references{
  Przemyslaw Biecek, Ewa Szczurek, Martin Vingron, Jerzy Tiuryn (2012), The R Package bgmm: Mixture Modeling with Uncertain Knowledge, Journal of Statistical Software.
}
\author{
Przemyslaw Biecek
}
\examples{
data(Ste12)
X = Ste12Data[ match(names(Ste12Data), rownames(Ste12Beliefs), nomatch = 0) ==0 ]
knowns = Ste12Data[rownames(Ste12Beliefs)]
model = belief(X=X, knowns=knowns, B=Ste12Beliefs)
dep=DEprobs(model)
str(dep)
}
