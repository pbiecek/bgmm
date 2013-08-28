\name{predict.mModel}
\alias{predict.mModel}
\title{
Predictions for fitted Gaussian component model
}
\description{
For every row in the matrix \code{X} the posterior probability of belonging to class \code{i} is calculated.
}
\usage{
\method{predict}{mModel}(object, X, knowns = NULL, B = NULL, P = NULL, ...)
}
\arguments{
  \item{object}{an object of the class \code{mModel},   }
  \item{X}{a \code{matrix} or \code{data.frame} in which number of columns is equal to \code{object$d}.   }
  \item{knowns}{a \code{data.frame} or \code{matrix} with the labeled observations. If the argument \code{knowns} is specified then eighter \code{B} or \code{P} need to be specified. }
  \item{P}{a matrix with plausibilities for object \code{knowns}. }
  \item{B}{a matrix with beliefs for object \code{knowns}. }
  \item{\dots}{all other arguments will be neglected.   }
}
\details{
  The matrix tij of posterior probabilities is calculated as normalized products of priors pi's and density of model components in values specified by rows of the matrix \code{X}.

  If arguments \code{knowns} and \code{B} are specified then the priors's for objects in \code{knowns} are replaced by belief matrix \code{B}.
  If arguments \code{knowns} and \code{P} are specified then the priors's for objects in \code{knowns} are multiplied by plausibility matrix \code{P}.
}
\value{
An list with the following elements:
\item{tij.X, tij.knowns}{the matrix tij.X is a matrix with number of rows equal to number of rows in the matrix \code{X} and the number of columns equal to the number of components in model  defined by argument \code{object}. Values in this matrix are posterior probabilities that observation i belongs to component j. The slot \code{tij.knowns} is equal to \code{NULL} if neither \code{B} nor \code{P} are specified, otherwise it is a matrix with number of rows equal to number of rows in the matrix \code{knowns} and contains posterior probabilities for observarions with specified belief or plausibilities matrix}
\item{class.X, class.knowns}{vactors of labels/classes obtained with the MAP rule. The vector \code{class.X} corresponds to observations in \code{X} while the vector \code{class.knowns} corresponds to observations in \code{knowns}.}
}
\references{
  http://bgmm.molgen.mpg.de
}
\author{
  Przemyslaw Biecek, Ewa Szczurek, Martin Vingron, Jerzy Tiuryn (2012), The R Package bgmm: Mixture Modeling with Uncertain Knowledge, Journal of Statistical Software.
}
\seealso{\code{\link{belief}}}
\examples{
 data(genotypes)

 modelSoft = soft(X=genotypes$X, knowns=genotypes$knowns, P=genotypes$B)

 preds = predict(modelSoft, X = genotypes$X)
 str(preds)
}
