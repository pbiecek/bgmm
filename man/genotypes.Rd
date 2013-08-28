\name{genotypes}
\alias{genotypes}
\docType{data}
\title{
Fluorescence signals corresponding to a given allele for 333 SNPs
}
\description{
The genotypes dataset describes 333 SNPs. Each SNP is characterized by the presence of one of its two possible alleles (or the presence of both of them). Therefore, the SNPs can be divided into three types. The first type corresponds to the SNPs with the first possible allele, the second type with the second allele, and the third with both alleles. The presence of the alleles is measured experimentally with fluorescence intensities. The dataset contains the intensities in the slots \code{X} and \code{knowns}. 

15 SNPs in the dataset are given their correct type. These 'known' SNPs can be used as the input for the semi-, partially and fully superised modeling methods. They were selected by taking at random five SNPs per each type.  Their intensities are contained in the slot \code{knowns}. Their belief/plausibility values (given in the slot \code{B}) of the most probable type (the slot \code{labels}) are set to 0.95, and of the other two types are equal 0.025. 
The remaining 318 SNPs are kept unlabeled.
}
\usage{data(genotypes)}
\format{
  X       : a matrix with 318 rows (unlabeled SNPs) and 2 columns (alleles)
  knowns  : a matrix with 15 rows (known SNPs) and 2 columns (alleles)
  B       : a matrix with 15 rows (known SNPs) and 3 columns (types)
  labels  : a vector of length 15 (types of the known SNPs)
  }
\details{
	The rows of both the slots \code{X} and \code{knowns}  correspond to the SNPs. For each SNP, the values in the columns represent the intensities of the fluorescence signal corresponding to the alleles of the SNP. The slot  \code{B} corresponds to the belief matrix while  \code{labels} contains the true types for the labeled SNPs.
}

\references{
   Takitoh, S.  Fujii, S.  Mase, Y.  Takasaki, J.  Yamazaki, T.  Ohnishi, Y.  Yanagisawa, M.  Nakamura, Y.  Kamatani, N., Accurate automated clustering of two-dimensional data for single-nucleotide polymorphism genotyping by a combination of clustering methods: evaluation by large-scale real data, Bioinformatics (2007) Vol. 23, 408--413.
}
\examples{
library(bgmm)
data(gnotypes)
}
\keyword{datasets}
