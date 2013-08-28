\name{simulateData}
\alias{simulateData}
\title{
Dataset generation
}
\description{
The function \code{simulateData} generates an artificial dataset from a mixture of Gaussian components with a given set of parameters.
}
\usage{
 simulateData(d = 2, k = 4, n = 100, m = 10, mu = NULL, cvar = NULL, 
    s.pi = rep(1/k, k), b.min = 0.02, mean = "D", between = "D", 
    within = "D", cov = "D", n.labels = k)
}
\arguments{
  \item{d}{the dimension of the data set,  }
  \item{k}{the number of the model components,  }
  \item{n}{the total number of observations, both labeled and unlabeled,  }
  \item{mu}{a matrix with \code{k} rows and \code{d} columns, which defines the means' vectors for the corresponding model components. If not specified, by default its values are generated from a normal distribution N(0,49),  }
  \item{cvar}{a three-dimensional array with the dimensions (\code{k}, \code{d}, \code{d}). If not specified, each covariance matrix is generated in three steps: first, 2*\code{d} samples from a \code{d}-dimensional normal distribution N(0, Id) are generated. Next, a covariance matrix d x d for these samples is calculated. Finally, the resulting sample covariance matrix is scaled by a factor generated from an exponential distribution  Exp(1),  }
  \item{s.pi}{ a vector of \code{k} probabilities, i.e. the mixing proportions of the model. The mixing proportions specify a multinomial distribution over the components, from which the numbers of observations in each cluster are generated. By default a uniform distribution is used.  }
  \item{mean, between, within, cov}{constraints on the model structure. By default all are equal to "D". If other values are set, the parameters \code{mu} and \code{cvar} are adjusted to match the specified constraints,  }
  \item{m}{the number of the observations, for which the beliefs are to be calculated,  }
  \item{b.min}{ the belief that an observation does not belong to a component. Formally, the belief bij for the observation i to belong to component j is equal \code{b.min} if i is not generated from component j. Thus, the belief that i belongs to its true component is set to \code{1-b.min*(n.labels-1)}, and \code{b.min} is constrained that \code{b.min}$<1/$\code{n.labels}. By default \code{b.min=0.02},  }
  \item{n.labels}{the number of components used as labels, defining the number of columns in the resulting beliefs matrix. By default \code{n.labels} equals \code{k}, but the user can specify a smaller number. Using this argument the user can define a scenario in which the data are generated from a mixture of three components, but only two of them are used as labels in the beliefs matrix (applied in the example below).  }
}
\value{
An list with the following elements:
 \item{X}{the matrix of size n-m rows and d columns with generated values of unlabeled observations,}
 \item{knowns}{the matrix of size m rows and d columns with generated values of labeled observations,}
 \item{B}{the belief matrix of the size m rows and k columns derived for knowns matrix,}
 \item{model.params}{the list of model parameters,}
 \item{Ytrue}{indexes of the true Gaussian components from which each observation was generated.}
}
\references{
  Przemyslaw Biecek, Ewa Szczurek, Martin Vingron, Jerzy Tiuryn (2012), The R Package bgmm: Mixture Modeling with Uncertain Knowledge, Journal of Statistical Software.
}
\author{
Przemyslaw Biecek
}
\examples{
 simulated = simulateData(d=2, k=3, n=300, m=60, cov="0", within="E", n.labels=2)
 model = belief(X = simulated$X, knowns = simulated$knowns, B=simulated$B)
 plot(model)

 simulated = simulateData(d=1, k=2, n=300, m=60, n.labels=2)
 model = belief(X = simulated$X, knowns = simulated$knowns, B=simulated$B)
 plot(model)
}
