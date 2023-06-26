\name{SNVMutations-package}
\alias{SNVMutations-package}
\alias{SNVMutations}
\docType{package}
\title{Analysis of SNV types of a vcf file
\packageTitle{SNVMutations}
}
\description{ The package takes in input a vcf file, filters for single nucleotide mutations and returns them or counts them.
\packageDescription{SNVMutations}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{SNVMutations}
\packageIndices{SNVMutations}
give to the functions a vcf file, a reference genome in form of a string specifying the name of a NCBI assembly (e.g. "GRCh38") or UCSC genome (e.g. "hg19") and a context length for the MutationType function. For the MutationType the context length is not required in the MutationCount function }
\author{ Michele Rossi
\packageAuthor{SNVMutations}

Maintainer: \packageMaintainer{SNVMutations}
}

\examples{
vcf_file <- system.file("extdata", "gl_chr1.vcf", package="VariantAnnotation")
MutationType(vcf_file,"hg19",6)
MutationCount(vcf_file,"hg19",graph=FALSE)
}
