\name{crossval}
\alias{crossval}
\title{
k-fold cross-validation for the specified model
}
\description{
The function \code{crossval()} performes k-fold cross-validation.
}
\usage{
crossval(model = NULL, X = NULL, knowns = NULL, class = NULL, 
    k = length(unique(class)), B = NULL, P = NULL, model.structure = getModelStructure(), 
    ..., folds = 2, fun = belief) 

}

\arguments{
  \item{model}{an object of the class \code{mModel}.
  }
  \item{X}{a data.frame with unknown realizations. If not supplied \code{X} is extracted from the \code{model} argument.
  }
  \item{knowns}{a data.frame with labeled realizations. If not supplied \code{knowns} is extracted from the \code{model} argument.
  }
  \item{class, B, P}{a vector of classes, beliefs and plausibilities. If not supplied they will be extracted from the \code{model} argument.
  }
  \item{fun}{function that will be used for modeling, one of \code{supervised}, \code{unsupervised}, \code{belief}, \code{soft}, \code{semisupervised}.
  }
  \item{model.structure, k, ...}{arguments that will be passed to \code{fun} function,  
  }
  \item{folds}{number of folds in k-fold cross validation. Cannot be grated that number of labeled samples.
  }
}
\details{
The function \code{crossval()} divides the dataset into \code{k} equal subsets, the number of labeled cases versus number of unlabeled cases is keep as close to constant as possible (the subset are generated with stratification). 
Then each subset is used as test set against a train set build from all remaining sets. In total \code{k} new models are estimated thus this procedure is time consuming.

For each model the error is calculated as average absolute differences between the distribution of estimated posteriors and distribution of beliefs/plausibilities for labeled cases.
}
\value{
The list with three vectors: errors calculated as mean absolute differences between estimated posteriors and initial beliefs for known cases, indexes of folds for both labeled and unlabeled cases. }
\references{
  Przemyslaw Biecek, Ewa Szczurek, Martin Vingron, Jerzy Tiuryn (2012), The R Package bgmm: Mixture Modeling with Uncertain Knowledge, Journal of Statistical Software.
}
\author{
Przemyslaw Biecek
}
\examples{                
 set.seed(1313)
 simulated = simulateData(d=2, k=3, n=300, m=60, cov="0", within="E", n.labels=2)
 amodel = belief(X=simulated$X, knowns=simulated$knowns, B=simulated$B, k=4)
 str(crossval(model=amodel, folds=6))
}
