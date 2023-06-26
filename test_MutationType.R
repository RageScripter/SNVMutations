test_that('MutationType doesnt work on a different genome', {

  vcffile <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
  
  reference_genome <- "hg38"

  
  expect_error(MutationType(vcffile, reference_genome, 3))
}
)



test_that('MutationType output are of the corregth length', {
  library(VariantAnnotation)
  
  vcffile <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")

  
  reference_genome <- "hg19"
  
  example_run <-  MutationType(vcffile, reference_genome, 3)
  
  expect_equal(unique(sapply(example_run,nchar)),7)
}
)


test_that("MutationType works odd context_length ", {
  
  vcffile <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
  
  reference_genome <- "hg19"
  expect_silent(MutationType(vcffile, reference_genome,3))
})

test_that("MutationType works evev context_length ", {
  
  vcffile <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
  
  reference_genome <- "hg19"
  expect_silent(MutationType(vcffile, reference_genome,4))
})

