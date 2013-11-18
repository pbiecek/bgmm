\name{bgmm-package}
\alias{bgmm-package}
\alias{bgmm}
\docType{package}
\title{
Belief-based Gaussian mixture modeling
}
\description{
This package implements partially supervised mixture modeling methods: soft-label and belief-based modeling, the semi-supervised methods and for completeness also unsupervised and fully supervised methods for mixture modeling. 
}
\details{
\tabular{ll}{
Package: \tab bgmm\cr
Type: \tab Package\cr
Version: \tab 1.6\cr
Date: \tab 2013-11-18\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
For short overview see the webpage http://bgmm.molgen.mpg.de/rapBGMM/.
}
\author{
Przemyslaw Biecek \& Ewa Szczurek

Maintainer: Przemyslaw Biecek <P.Biecek@mimuw.edu.pl>
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
