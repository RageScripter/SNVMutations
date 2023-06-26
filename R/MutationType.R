
#' Reads .vcf file, the reference genome and a desired length
#' and returns a vector of SNVs
#'
#' This function reads a .vcf file and, given a reference genome and a length 
#' returns a vector of SNVs in the format UP REF>ALT DOWN
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
#' @examples
#' ex_vcf_file = system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
#' MutationType(ex_vcf_file, "hg19", 3)
#' 
#'
#' @export



MutationType <-function(vcf_file,reference_genome,context_length){
  
  # read the vcf file
  vcf <- readVcf(vcf_file, reference_genome)
  
  # vcf files can have more than one alt, I ignore those rare cases
  mask_alt <-  lapply(alt(vcf),length) ==1
  
  #here I check that the mutation is a single nucleotide
  mask_SNP <- start(vcf[mask_alt]) == end(vcf[mask_alt]) & width(unlist(alt(vcf)[mask_alt])) == 1
  
  # I select the mutations 
  vcf <-  vcf[mask_alt][mask_SNP]
  ref <- ref(vcf)
  alt <- unlist(alt(vcf))
  
  # I get position informations
  chr <- seqnames(vcf)
  center<- start(vcf)
  
  #if the context_length is is even i'm making the start 1 base longer
  start <- ifelse(context_length%%2==0,context_length/2,(context_length-1)/2)
  end <- ifelse(context_length%%2==0,(context_length/2)-1,(context_length-1)/2)
  
  # Since I've found .vcf with unambiguous description of the chromosme number
  # (e.g. both "22" and "chr22") I add this check to make the code resistant
  chr_start <- c(rep("chr",length(chr)))[!startsWith(as.character(chr),"chr")]
  
  # I build the sequences of interest 
  context_length_ranges <- GRanges(paste(chr_start, chr,":",(center-start),"-",(center+end),":",sep = ""))
  genome <- getBSgenome(reference_genome, masked=FALSE)
  ref_seq <- getSeq(genome,context_length_ranges)
  
  mutations <- c(rep(NA, length(vcf)))
  
  # I remove the redundacy by computing the Reverse complement of 
  # everything where the ref is not T or C
  RC_mask <- ref %in% c(DNAStringSet("G"),  DNAStringSet("A"))
  ref[RC_mask] <- reverseComplement(ref[RC_mask])
  alt[RC_mask] <- reverseComplement(alt[RC_mask])
  ref_seq[RC_mask] <- reverseComplement(ref_seq[RC_mask])
  
  for (x in seq(1,length(vcf))) {
    mutations[x] <-  paste(substr(as.character(ref_seq[x]), 1, start),'[', ref[[x]], '>', alt[[x]], ']', substr(as.character(ref_seq[x]),context_length-end+1 , context_length), sep = "")
  }
  

  
  
  return(mutations)
  }
