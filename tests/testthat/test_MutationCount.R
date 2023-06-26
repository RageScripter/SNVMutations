test_that('MutationCount doesnt work on a different genome', {
  
  vcffile <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")

  
  regerence_genome <- "hg38"
  
  
  expect_error(MutationCount(vcf, regerence_genome,graph=FALSE))
}
)



test_that('MutationCount outputs a table', {
  library(VariantAnnotation)
  
  vcffile <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
  
  regerence_genome <- "hg19"
  
  expect_equal(class(MutationCount(vcffile, regerence_genome, graph=FALSE)),"data.frame")
}
)


test_that('MutationCount works with graph', {
  library(VariantAnnotation)
  
  vcffile <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
  
  regerence_genome <- "hg19"
  
  expect_silent(MutationCount(vcffile, regerence_genome, graph=TRUE))
}
)

test_that('MutationCount works without graph', {
  library(VariantAnnotation)
  
  vcffile <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
  
  regerence_genome <- "hg19"
  
  expect_silent(MutationCount(vcffile, regerence_genome, graph=FALSE))
}
)