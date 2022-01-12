#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument. If not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)==1) {
  args[2] = "output.txt" #output file
}

if(!require(dplyr)){
  print("Installing dplyr package")
  install.packages("dplyr")
}

if(!require(progress)){
  print("Installing progress package")
  install.packages("progress")
}
library(progress)

if(!require(qdapRegex)){
  print("Installing qdapRegex package")
  install.packages("qdapRegex")
}

df <- read.delim(args[1])
df <- dplyr::rename(df, "Name" = colnames(df[1]))

print("Starting extraction process of Diamond output. This may take a while!")
print("Go grab a coffe if you want :)")


genome_ncbi <- "***** No hits found *****"
first_node <- 0 #variable to check if it is the fisrt NODE of the diamond file
position_df <- 0 #variable to set the position in the final data frame being created

pb <- progress::progress_bar$new(total = nrow(df)) #show the running progress
for (i in 1:nrow(df)){
    pb$tick()
    Sys.sleep(1 / 100)
    row <- df[i,]
    if (substr(row,1,1) == ">"){ #whether the row is a matched genome
      genome_ncbi <- row
      if (df_final[position_df,1] == contig_name & df_final[position_df,2] == "***** No hits found *****"){ #check whether or not the data frame created has any matched sequences
          df_temp <- data.frame(Contigs = contig_name, Name = genome_ncbi, Lenght = as.numeric(sub('.+=(.+)', '\\1', df[i+1,])), Score = qdapRegex::ex_between(df[i+2,], "Score =", ",")[[1]], Expect = as.numeric(sub('.+Expect = (.+)', '\\1', df[i+2,])), Identities = qdapRegex::ex_between(df[i+3,], "Identities = ", ", P")[[1]], Positives = qdapRegex::ex_between(df[i+3,], "Positives = ", ", G")[[1]], Gaps = sub('.+Gaps = (.+)', '\\1', df[i+3,]))
          df_final <- rbind(df_final,df_temp)
          df_final <- df_final[-(position_df),] #delete the repeted row if the Node contains any matched sequence
        } else{
          df_temp <- data.frame(Contigs = contig_name, Name = genome_ncbi, Lenght =as.numeric(sub('.+=(.+)', '\\1', df[i+1,])), Score = qdapRegex::ex_between(df[i+2,], "Score =", ",")[[1]], Expect = as.numeric(sub('.+Expect = (.+)', '\\1', df[i+2,])), Identities = qdapRegex::ex_between(df[i+3,], "Identities = ", ", P")[[1]], Positives = qdapRegex::ex_between(df[i+3,], "Positives = ", ", G")[[1]], Gaps = sub('.+Gaps = (.+)', '\\1', df[i+3,])) #Continue running if the node has any matched sequences without deleting any rows
          df_final <- rbind(df_final,df_temp)
          position_df <- position_df + 1
        }
      }
    else if(substr(row,8,11) == "NODE"){ #if the row is the contig name
      if(first_node == 0){
        df_final <- data.frame(Contigs = row, Name = genome_ncbi, Lenght = NA, Score = NA, Expect = NA, Identities = NA, Positives = NA, Gaps = NA) #create de final data frame if it is the first Node 
        contig_name <- row
        position_df <- position_df + 1
    } else { #create a  data frame to be linked to the final one with another NODE
      df_temp <- data.frame(Contigs = row, Name = genome_ncbi, Lenght = NA, Score = NA, Expect = NA, Identities = NA, Positives = NA, Gaps = NA)
      df_final <- rbind(df_final, df_temp)
      contig_name <- row
      position_df <- position_df + 1
      }
    }
    genome_ncbi <- "***** No hits found *****"
    first_node <- first_node + 1
}

write.table(df_final, file=args[2], row.names=FALSE) #write the final data frame


