#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument. If not, return an error
if (length(args)<1) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

if(!require(taxonomizr)){
  cat("Installing dplyr package...\n")
  install.packages("taxonomizr")
}

library(taxonomizr)


diamond <- read.table(args[1],header = TRUE)

if(!file.exists("accessionTaxa.sql") & !file.exists("names.dmp") & !file.exists("nodes.dmp")){
  res <- try(taxonomizr::prepareDatabase('accessionTaxa.sql'))
} else{
  file.remove("accessionTaxa.sql")
  file.remove("names.dmp")
  file.remove("nodes.dmp")
  res <- try(taxonomizr::prepareDatabase('accessionTaxa.sql'))
  }

res2 <- try(if(inherits(res, "try-error")){
  if (file.exists("accessionTaxa.sql") | file.exists("names.dmp") | file.exists("nodes.dmp")) {
  #Delete files if they exist
  file.remove("accessionTaxa.sql")
  file.remove("names.dmp")
  file.remove("nodes.dmp")
  getAccession2taxid(baseUrl='https://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/')
  prepareDatabase('accessionTaxa.sql')
  } 
})

if(inherits(res2, "try-error")){
  if (file.exists("accessionTaxa.sql") | file.exists("names.dmp") | file.exists("nodes.dmp")) {
  #Delete files if they exist
  file.remove("accessionTaxa.sql")
  file.remove("names.dmp")
  file.remove("nodes.dmp")
  cat("Try to download the two files manually, put in the directory you are using, and run again:\n")
  cat("https://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/nucl_wgs.accession2taxid.gz\n")
  cat("https://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/nucl_gb.accession2taxid.gz\n")
  } 
}

cat("Extracting taxonomic data...\n")

taxaId<-getId(diamond$Taxon,'accessionTaxa.sql')

taxa<-getTaxonomy(taxaId,'accessionTaxa.sql')
taxa_df <- as.data.frame(taxa)
diamond <- cbind(diamond, taxa_df)
diamond <- diamond[ , -which(names(diamond) == "species")]
colnames(diamond)[which(names(diamond) == "Name")] <- paste("Protein Identification")
colnames(diamond)[which(names(diamond) == "Taxon")] <- paste("Species")
colnames(diamond)[which(names(diamond) == "superkingdom")] <- paste("Superkingdom")
colnames(diamond)[which(names(diamond) == "phylum")] <- paste("Phylum")
colnames(diamond)[which(names(diamond) == "class")] <- paste("Class")
colnames(diamond)[which(names(diamond) == "order")] <- paste("Order")
colnames(diamond)[which(names(diamond) == "family")] <- paste("Family")
colnames(diamond)[which(names(diamond) == "genus")] <- paste("Genus")
colnames(diamond)[which(names(diamond) == "NCBI_ID")] <- paste("NCBI ID")

diamond <- diamond[, c("Contigs", "Contig_lenght", "Protein Identification", "NCBI ID", "Species","Genus","Family","Order","Class","Phylum","Superkingdom", "Lenght", "Score", "Expect", "Identities", "Positives", "Gaps")]

cat("Done!\n")

write.table(diamond, file="diamond_data.txt", row.names=FALSE)
