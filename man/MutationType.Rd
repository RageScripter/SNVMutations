\name{MutationType}
\alias{MutationType}
\title{
Mutation types from a vcf file}
\description{
This function takes in input a .vcf file, the refenrece genome and a given length. It returns a vector of SNVs in the format UP REF>ALT DOWN }
\usage{
MutationType(vcf_file, reference_genome, context_length)
}
\arguments{
  \item{vcf_file}{
a .vcf file
}
  \item{reference_genome}{
a string specifying the name of a NCBI assembly (e.g. "GRCh38") or UCSC genome (e.g. "hg19")}
  \item{context_length}{
length of the reference genome with the mutation -  format [REF>ALT] - in the middle.}
}
\details{
If the context_length provided id even the upstream side will be 1 base longer. Redundancy on the reverse strand, is addressed such that the REF is always T or C
}
\value{
A vector containing the SNVs in the context of the reference genome of the given length.
}
\references{
}
\author{
Michele Rossi}
\note{
}


\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (vcf_file, reference_genome, context_length) 
{
    vcf <- readVcf(vcf_file, reference_genome)
    mask_alt <- lapply(alt(vcf), length) == 1
    mask_SNP <- start(vcf[mask_alt]) == end(vcf[mask_alt]) & 
        width(unlist(alt(vcf)[mask_alt])) == 1
    vcf <- vcf[mask_alt][mask_SNP]
    ref <- ref(vcf)
    alt <- unlist(alt(vcf))
    chr <- seqnames(vcf)
    center <- start(vcf)
    start <- ifelse(context_length\%\%2 == 0, context_length/2, 
        (context_length - 1)/2)
    end <- ifelse(context_length\%\%2 == 0, (context_length/2) - 
        1, (context_length - 1)/2)
    chr_start <- c(rep("chr", length(chr)))[!startsWith(as.character(chr), 
        "chr")]
    context_length_ranges <- GRanges(paste(chr_start, chr, ":", 
        (center - start), "-", (center + end), ":", sep = ""))
    genome <- getBSgenome(reference_genome, masked = FALSE)
    ref_seq <- getSeq(genome, context_length_ranges)
    mutations <- c(rep(NA, length(vcf)))
    RC_mask <- ref \%in\% c(DNAStringSet("G"), DNAStringSet("A"))
    ref[RC_mask] <- reverseComplement(ref[RC_mask])
    alt[RC_mask] <- reverseComplement(alt[RC_mask])
    ref_seq[RC_mask] <- reverseComplement(ref_seq[RC_mask])
    for (x in seq(1, length(vcf))) {
        mutations[x] <- paste(substr(as.character(ref_seq[x]), 
            1, start), "[", ref[[x]], ">", alt[[x]], "]", substr(as.character(ref_seq[x]), 
            context_length - end + 1, context_length), sep = "")
    }
    return(mutations)
  }
}


