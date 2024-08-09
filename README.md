# SNVMutations
Bioconductor-compliant package that provides functions to determine the mutation types of SNV from a VCF files.


## Inputs
- set of SNV in VCF format
- a reference genome
- the parameter “context_length” (length of the context of the mutation uspstream[SNV]downstream) 

## Outputs
-  a vector of SNVs displayed as “UP[REF>ALT]DOWN”, where every nucleotide - to avoid redundancy - is displayed as T or C.
-  a count table of the SNVs and a countplot
