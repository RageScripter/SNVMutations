## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SNVMutations)

## -----------------------------------------------------------------------------
library(VariantAnnotation)
vcf_file <- vcf_file <- system.file("extdata", "gl_chr1.vcf", package="VariantAnnotation")
reference_genome <-  "hg19"

## -----------------------------------------------------------------------------
library("BSgenome")
library("GenomicRanges")
MutationType(vcf_file,reference_genome,3)

## -----------------------------------------------------------------------------
library(VariantAnnotation)
vcf_file <- vcf_file <- system.file("extdata", "gl_chr1.vcf", package="VariantAnnotation")
reference_genome <-  "hg19"

## -----------------------------------------------------------------------------
MutationCount(vcf_file,reference_genome,graph = FALSE)

## -----------------------------------------------------------------------------
sessionInfo()

