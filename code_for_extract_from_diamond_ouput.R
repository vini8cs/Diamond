#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)==1) {
  # default output file
  args[2] = "out.txt"
  #args[3] = "out2.txt"
}

if(!require(dplyr)){
  cat("Installing dplyr package\n")
  install.packages("dplyr")
}


if(!require(qdapRegex)){
  cat("Installing qdapRegex package\n")
  install.packages("qdapRegex")
}

df <- read.delim(args[1])
df <- dplyr::rename(df, "Name" = colnames(df[1]))

cat("Starting extraction process of Diamond output. This may take a while!\n")
cat("Go grab a coffe if you want :)\n")


genome_ncbi <- "***** No hits found *****"
first_node <- 0 #checks if it is the fisrt NODE of the diamond file
position_df <- 0 #sets the position in the final data frame being created

pb <- txtProgressBar(min = 0, max = nrow(df), style = 3) #show the running progress
for (i in 1:nrow(df)){
    setTxtProgressBar(pb,i) 
    Sys.sleep(1 / 100)
    row <- df[i,]
    if (substr(row,1,1) == ">"){ #whether the row is a matched genome
      genome_ncbi <- row
      if (df_final[position_df,1] == contig_name & df_final[position_df,3] == "***** No hits found *****"){ #check whether or not the data frame created has any matched sequences
          df_temp <- data.frame(Contigs = contig_name, Contig_lenght = contig_lenght, Name = genome_ncbi, NCBI_ID = paste0(qdapRegex::ex_between(df[i,], ">", ".1")[[1]],".1"), Taxon = qdapRegex::ex_between(df[i,], "[", "]")[[1]], Lenght = as.numeric(sub('.+=(.+)', '\\1', df[i+1,])), Score = qdapRegex::ex_between(df[i+2,], "Score =", ",")[[1]], Expect = as.numeric(sub('.+Expect = (.+)', '\\1', df[i+2,])), Identities = qdapRegex::ex_between(df[i+3,], "Identities = ", ", P")[[1]], Positives = qdapRegex::ex_between(df[i+3,], "Positives = ", ", G")[[1]], Gaps = sub('.+Gaps = (.+)', '\\1', df[i+3,]))
          df_final <- rbind(df_final,df_temp)
          df_final <- df_final[-(position_df),] #delete the repeted row if the Node contains any matched sequence
        } else{
          df_temp <- data.frame(Contigs = contig_name, Contig_lenght = contig_lenght, Name = genome_ncbi, NCBI_ID = paste0(qdapRegex::ex_between(df[i,], ">", ".1")[[1]],".1"), Taxon = qdapRegex::ex_between(df[i,], "[", "]")[[1]], Lenght =as.numeric(sub('.+=(.+)', '\\1', df[i+1,])), Score = qdapRegex::ex_between(df[i+2,], "Score =", ",")[[1]], Expect = as.numeric(sub('.+Expect = (.+)', '\\1', df[i+2,])), Identities = qdapRegex::ex_between(df[i+3,], "Identities = ", ", P")[[1]], Positives = qdapRegex::ex_between(df[i+3,], "Positives = ", ", G")[[1]], Gaps = sub('.+Gaps = (.+)', '\\1', df[i+3,])) #Continue running if the node has any matched sequences without deleting any rows
          df_final <- rbind(df_final,df_temp)
          position_df <- position_df + 1
        }
      }
    else if(substr(row,1,6) == "Query="){ #if the row is the contig name
      contig_lenght <- as.numeric(sub('.+=(.+)', '\\1', df[i+1,]))
      if(first_node == 0){
        df_final <- data.frame(Contigs = row, Contig_lenght = contig_lenght, Name = genome_ncbi, NCBI_ID = NA, Taxon = NA, Lenght = NA, Score = NA, Expect = NA, Identities = NA, Positives = NA, Gaps = NA) #create de final data frame if it is the first Node 
        contig_name <- row
        position_df <- position_df + 1
    } else { #creates a  data frame to be linked to the final one with another contig
      df_temp <- data.frame(Contigs = row, Contig_lenght = contig_lenght, Name = genome_ncbi, NCBI_ID = NA, Taxon = NA, Lenght = NA, Score = NA, Expect = NA, Identities = NA, Positives = NA, Gaps = NA)
      df_final <- rbind(df_final, df_temp)
      contig_name <- row
      position_df <- position_df + 1
      }
    }
    genome_ncbi <- "***** No hits found *****"
    first_node <- first_node + 1
}
close(pb)
cat("Done!\n")





write.table(df_final, file=args[2], row.names=FALSE) #write the final data frame
