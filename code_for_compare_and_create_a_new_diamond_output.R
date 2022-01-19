#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

# test if there is at least two argument: if not, return an error
if (length(args) != 3) {
  stop("Exactly three arguments must be supplied, including the name of the output (input file).n", call.=FALSE)
} else {
  # default output file
  args[3] = "out.txt"
}

if(!require(qdapRegex)){
  print("Installing qdapRegex package")
  install.packages("qdapRegex")
}

cat("Reading input files...")


x <- read.delim(args[1])
y <- read.delim(args[2])

if(length(x[,1]) <= length(y[,1])){
  df <- x
  df2 <- y
} else{
  df <- y
  df2 <- x
}

cat("Starting comparing process of Diamond outputs. This may take a while!")
cat("Go grab a coffe if you want :)")

i <- 1
j <- 1
n <- 0
hits <- data.frame(NODE = " ", Sequence_1= " ", Sequence_2 = " ", Type = " ")

library(utils)
total = nrow(df2)

pb <- txtProgressBar(min = 0, max = total, style = 3) #progress bar
while(TRUE){
  setTxtProgressBar(pb,i) 
  if(substr(df[i,],1,6) == "Query="){ #In case the row being read is from the first dataset
    while (df[i,] != df2[j,]) {
      j <- j + 1 #loop through the second dataset until it finds the match
      }
    if(substr(df[i+2,],1,1) == ">" & substr(df2[j+2,],1,1) == ">"){ #checks if the first sequences from each contig are both hits
      if (as.numeric(qdapRegex::ex_between(df[i+4, ], "Score = ", " bits")[[1]]) > as.numeric(qdapRegex::ex_between(df2[j+4, ], "Score = ", " bits")[[1]])){ #if the score from the first hit is higher than that of the other
        y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 1")
        hits <- rbind(hits, y)
          if(n == 0){ #if it's the first row
            df_final <- data.frame(Name = c(df[seq(i,i+5,1), ]))
          } else { #if it's not
            df_temp <- data.frame(Name = c(df[seq(i,i+5,1), ])) #add hit info
            df_final <- rbind(df_final, df_temp)
          }
      } else if(as.numeric(qdapRegex::ex_between(df[i+4, ], "Score = ", " bits")[[1]]) < as.numeric(qdapRegex::ex_between(df2[j+4, ], "Score = ", " bits")[[1]])){ #if the score from the first hit is lower than that of the other
        y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 2")
        hits <- rbind(hits, y)
        if(n == 0){ #if it's the first row
          df_final <- data.frame(Name = c(df2[seq(j,j+5,1), ]))
        } else{ #if it's not
          df_temp <- data.frame(Name = c(df2[seq(j,j+5,1), ])) #add hit info
        df_final <- rbind(df_final, df_temp)
        }
      } else if(as.numeric(qdapRegex::ex_between(df[i+4, ], "Score = ", " bits")[[1]]) == as.numeric(qdapRegex::ex_between(df2[j+4, ], "Score = ", " bits")[[1]])){ #if the score from the first hit is equal to the other
        y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Equal")
        hits <- rbind(hits, y)
          if(n == 0){ #if it's the first row
            df_final <- data.frame(Name = c(df[seq(i,i+5,1), ]))
          } else { #if it's not
            df_temp <- data.frame(Name = c(df[seq(i,i+5,1), ])) #add hit info
            df_final <- rbind(df_final, df_temp)
          }
      }
    } else if(df[i+2,] == "***** No hits found *****"){ #if the contig from the first dataset has no hit
        if(n == 0){
           if(substr(df2[j+2,],1,1) == ">"){ #if the contig from the second dataset has a hit
             y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 2") #reports the sequence that replaced the other
             hits <- rbind(hits, y)
             df_final <- data.frame(Name = c(df2[seq(j,j+5,1), ]))
             } else{ #if it's not 
               y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found") #reports that there is no hit in any of the datasets
               hits <- rbind(hits, y)
               df_final <- data.frame(Name = c(df2[seq(j,j+2,1), ]))
               }
      } else{
          if(substr(df2[j+2,],1,1) == ">"){ #if the contig from the second dataset has no hit
            y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 2") #reports the sequence that replaced the other
            hits <- rbind(hits, y)
            df_temp <- data.frame(Name = c(df2[seq(j,j+5,1), ]))
            df_final <- rbind(df_final, df_temp)
            } else{
              y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found") #reports that there is no hit in any of the datasets
              hits <- rbind(hits, y)
              df_temp <- data.frame(Name = c(df2[seq(j,j+2,1), ]))
              df_final <- rbind(df_final, df_temp)
        }
      }
    } else if(df2[j+2,] == "***** No hits found *****"){ #if the contig from the second dataset has no hit
      if(n == 0){
        if(substr(df[i+2,],1,1) == ">"){ #if the contig from the first dataset has a hit
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 1") #reports the sequence that replaced the other
          hits <- rbind(hits, y)
          df_final <- data.frame(Name = c(df[seq(i,i+5,1), ]))
        } else{
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found") #reports that there is no hit in any of the datasets
          hits <- rbind(hits, y)
          df_final <- data.frame(Name = c(df[seq(i,i+2,1), ]))
        }
      } else{
        if(substr(df[i+2,],1,1) == ">"){
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 1") #reports the sequence that replaced the other
          hits <- rbind(hits, y)
          df_temp <- data.frame(Name = c(df[seq(i,i+5,1), ]))
          df_final <- rbind(df_final, df_temp)
        } else{
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found") #reports that there is no hit in any of the datasets
          hits <- rbind(hits, y)
          df_temp <- data.frame(Name = c(df2[seq(i,i+2,1), ]))
          df_final <- rbind(df_final, df_temp)
        }
      }
    }
  } else if(substr(df2[j,],1,6) == "Query="){ #In case the row being read is from the second dataset
    while (df[i,] != df2[j,]) {
      i <- i + 1 #loop through the first dataset until it finds the match
        }
    if(substr(df[i+2,],1,1) == ">" & substr(df2[j+2,],1,1) == ">"){  #checks if the first sequences from each contig are both hits
      if (as.numeric(qdapRegex::ex_between(df[i+4, ], "Score = ", " bits")[[1]]) > as.numeric(qdapRegex::ex_between(df2[j+4, ], "Score = ", " bits")[[1]])){ #if the score from the first hit is higher than that of the other
        y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 1")
        hits <- rbind(hits, y)
        if(n == 0){ #if it's the first row
            df_final <- data.frame(Name = c(df[seq(i,i+5,1), ]))
          } else { #if it's not
            df_temp <- data.frame(Name = c(df[seq(i,i+5,1), ])) #add hit info
            df_final <- rbind(df_final, df_temp)
          }  
      } else if(as.numeric(qdapRegex::ex_between(df[i+4, ], "Score = ", " bits")[[1]]) < as.numeric(qdapRegex::ex_between(df2[j+4, ], "Score = ", " bits")[[1]])){ #if the score from the first hit is lower than that of the other
        y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 2")
        hits <- rbind(hits, y)
        if(n == 0){ #if it's the first row
          df_final <- data.frame(Name = c(df2[seq(j,j+5,1), ]))
        } else{ #if it's not
          df_temp <- data.frame(Name = c(df2[seq(j,j+5,1), ])) #add hit info
        df_final <- rbind(df_final, df_temp)
        }
      }  else if(as.numeric(qdapRegex::ex_between(df[i+4, ], "Score = ", " bits")[[1]]) == as.numeric(qdapRegex::ex_between(df2[j+4, ], "Score = ", " bits")[[1]])){ #if the score from the first hit is equal to the other
        y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Equal")
        hits <- rbind(hits, y)
          if(n == 0){ #if it's the first row
            df_final <- data.frame(Name = c(df[seq(i,i+5,1), ]))
          } else { #if it's not
            df_temp <- data.frame(Name = c(df[seq(i,i+5,1), ])) #add hit info
            df_final <- rbind(df_final, df_temp)
          }
      }
    } else if(df[i+2,] == "***** No hits found *****"){ #if the contig from the first dataset has no hit
        if(n == 0){
           if(substr(df2[j+2,],1,1) == ">"){ #if the contig from the second dataset has a hit
             y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 2") #reports the sequence that replaced the other
             hits <- rbind(hits, y)
             df_final <- data.frame(Name = c(df2[seq(j,j+5,1), ]))
             } else{
               y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found") #reports that there is no hit in any of the datasets
               hits <- rbind(hits, y)
               df_final <- data.frame(Name = c(df2[seq(j,j+2,1), ]))
               }
      } else{
          if(substr(df2[j+2,],1,1) == ">"){
            y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 2") #reports the sequence that replaced the other
            hits <- rbind(hits, y)
            df_temp <- data.frame(Name = c(df2[seq(j,j+5,1), ]))
            df_final <- rbind(df_final, df_temp)
            } else{
              y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found") #reports that there is no hit in any of the datasets
              hits <- rbind(hits, y)
              df_temp <- data.frame(Name = c(df2[seq(j,j+2,1), ]))
              df_final <- rbind(df_final, df_temp)
        }
      }
    } else if(df2[j+2,] == "***** No hits found *****"){ #if the contig from the second dataset has no hit
      if(n == 0){
        if(substr(df[i+2,],1,1) == ">"){
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 1") #reports the sequence that replaced the other
          hits <- rbind(hits, y)
          df_final <- data.frame(Name = c(df[seq(i,i+5,1), ]))
        } else{
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found") #reports that there is no hit in any of the datasets
          hits <- rbind(hits, y)
          df_final <- data.frame(Name = c(df[seq(i,i+2,1), ]))
        }
      } else{
        if(substr(df[i+2,],1,1) == ">"){
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 1") #reports the sequence that replaced the other
          hits <- rbind(hits, y)
          df_temp <- data.frame(Name = c(df[seq(i,i+5,1), ]))
          df_final <- rbind(df_final, df_temp)
        } else{
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found") #reports that there is no hit in any of the datasets
          hits <- rbind(hits, y)
          df_temp <- data.frame(Name = c(df2[seq(i,i+2,1), ]))
          df_final <- rbind(df_final, df_temp)
        }
      }
    }
    
    }
  
  if(i == length(df[,1]) | j == length(df2[,1])){ #if reach the end of the dataset, stop!
    break
    
  }
  n <- n + 1
  i <- i + 1
  j <- j + 1
}
close(pb)

cat("Done!")

write.table(df_final, file=args[3], row.names=FALSE) #write the final data frame
write.table(hits, file="hits.txt",row.names = FALSE) #wirte the hit data frame
