\name{MutationCount}
\alias{MutationCount}
\title{
Counts mutations from a vcf file}
\description{
This function takes in input a .vcf file, the refenrece genome. It returns a count table of SNVs in the format  REF>ALT  
}
\usage{
MutationCount(vcf_file, reference_genome, graph = FALSE)
}
\arguments{
  \item{vcf_file}{
a .vcf file
}
  \item{reference_genome}{
a string specifying the name of a NCBI assembly (e.g. "GRCh38") or UCSC genome (e.g. "hg19")
}
  \item{graph}{
a booleant that if TRUE returns the graph of the count table
}
}
\details{
}
\value{
a table containing SNV counts
}
\references{
}
\author{
Michele Rossi
}
\note{
}


\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (vcf_file, reference_genome, graph = FALSE) 
{
    vcf <- readVcf(vcf_file, reference_genome)
    mutations <- MutationType(vcf_file, reference_genome, 1)
    count_table <- as.data.frame(table(mutations))
    p <- ggplot(count_table, aes(x = mutations, y = Freq)) + 
        geom_bar(stat = "identity", linewidth = 0.5, color = "black", 
            fill = "white") + xlab("Mutations") + theme_minimal() + 
        theme(panel.grid.major.x = element_blank())
    if(graph) {print(p)}
    return(count_table)
  }
}

