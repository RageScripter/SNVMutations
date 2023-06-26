#' This function reads a .vcf file and, given a reference genome returns 
#' a count table and a graph of SNV in the format UP REF>ALT DOWN
#'
#' @usage MutationCount(vcf_file,reference_genome,graph)
#' @param vcf_file a  VCF file
#' @param reference_genome the reference genome
#' @param graph a boolean, if TRUE returns a graph
#' @return Distance in miles
#' @author Michele Rossi \cr Politecnico di Milano\cr Maintainer: Michele
#' Rossi\cr E-Mail: <michele9.rossi@@polimi.it>
#' @import VariantAnnotation
#' @import BSgenome
#' @import ggplot2
#' @examples
#' ex_vcf_file = system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation"))
#' MutationCount(ex_vcf_file, "hg19",graph = TRUE)
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
