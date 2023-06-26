#' Reads .vcf file, the reference genome and a desired length
#' and returns a vector of SNVs
#'
#' This function reads a .vcf file and, given a reference genome and a length 
#'  returns a vector of SNVs in the format UP REF>ALT DOWN
#'
#' @usage MutationType(vcf_file,reference_genome,context_length)
#' @param MutationType a  VCF file
#' @param reference_genome the reference genome
#' @param context_length the length of the region considered
#' @return Vector of SNVs 
#' @author Michele Rossi \cr Politecnico di Milano\cr Maintainer: Michele
#' Rossi\cr E-Mail: <michele9.rossi@@polimi.it>
#' @import VariantAnnotation
#' @import BSgenome
#' @import GenomicRanges
#' @import Biostrings
#' @examples
#' ex_vcf_file = system.file("extdata", "gl_chr1.vcf", package="VariantAnnotation")
#' MutationType(ex_vcf_file, "hg19", 3)
#' 
#'
#' @export


MutationCount <-
function(vcf_file,reference_genome,graph = FALSE){
   
     # read the vcf file
    vcf <- readVcf(vcf_file, reference_genome)
    
    # 
    mutations <- MutationType(vcf_file,reference_genome,1)
    
    #I build the count table
    count_table <- as.data.frame(table(mutations))
    
    #plot 
    p <- ggplot(count_table,aes(x=mutations,y=Freq)) + 
      geom_bar(stat="identity",linewidth = 0.5, color = "black", fill = "white"  ) +
      xlab("Mutations") + theme_minimal() + 
      theme(panel.grid.major.x = element_blank())
    
    if(graph) {print(p)}
    return (count_table)
  }
