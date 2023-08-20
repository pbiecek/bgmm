\name{bgmm-package}
\alias{bgmm-package}
\alias{bgmm}
\docType{package}
\title{
Belief-Based Gaussian Mixture Modeling
}
\description{
This package implements partially supervised mixture modeling methods: soft-label and belief-based modeling, the semi-supervised methods and for completeness also unsupervised and fully supervised methods for mixture modeling.
}
\details{
For short overview see the webpage http://bgmm.molgen.mpg.de/rapBGMM/.
}
\author{
Przemyslaw Biecek \& Ewa Szczurek

Maintainer: Przemyslaw Biecek <Przemyslaw.Biecek@gmail.com>
}
\references{
  Przemyslaw Biecek, Ewa Szczurek, Martin Vingron, Jerzy Tiuryn (2012), The R Package bgmm: Mixture Modeling with Uncertain Knowledge, Journal of Statistical Software.
}
\keyword{ package }
\seealso{
Package for unsupervised learning of Gaussian mixture model \code{link{mclust}},
methods for supervised learning \code{link{MASS::lda()}}, \code{link{MASS::qda()}}.
}
\examples{
## Do not run
## It could take more than one minute
#demo(bgmm)
}
