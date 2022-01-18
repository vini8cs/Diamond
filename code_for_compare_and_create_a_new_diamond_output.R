#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args) <= 1 | length(args) > 3) {
  stop("Exactly three arguments must be supplied, including the name of the output (input file).n", call.=FALSE)
} else if (length(args)==3) {
  # default output file
  args[3] = "out.txt"
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

print("Reading input files...")


x <- read.delim(args[1])
y <- read.delim(args[2])

if(length(x[,1]) <= length(y[,1])){
  df <- x
  df2 <- y
} else{
  df <- y
  df2 <- x
}

print("Starting comparing process of Diamond outputs. This may take a while!")
print("Go grab a coffe if you want :)")

i <- 1
j <- 1
n <- 0
hits <- data.frame(NODE = " ", Sequence_1= " ", Sequence_2 = " ", Type = " ")

library(utils)
total = nrow(df2)

pb <- txtProgressBar(min = 0, max = total, style = 3)
while(TRUE){ 
  setTxtProgressBar(pb,i)
  if(substr(df[i,],1,6) == "Query="){
    while (df[i,] != df2[j,]) {
      iterStatus <- paste(round(100*(i/total),0), "% Complete", sep="")
      setTxtProgressBar(pb,i)
      j <- j + 1
      }
    if(substr(df[i+2,],1,1) == ">" & substr(df2[j+2,],1,1) == ">"){
      if (as.numeric(qdapRegex::ex_between(df[i+4, ], "Score = ", " bits")[[1]]) > as.numeric(qdapRegex::ex_between(df2[j+4, ], "Score = ", " bits")[[1]])){
        y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 1")
        hits <- rbind(hits, y)
          if(n == 0){
            df_final <- data.frame(Name = c(df[seq(i,i+5,1), ]))
          } else {
            df_temp <- data.frame(Name = c(df[seq(i,i+5,1), ]))
            df_final <- rbind(df_final, df_temp)
          }
      } else if(as.numeric(qdapRegex::ex_between(df[i+4, ], "Score = ", " bits")[[1]]) < as.numeric(qdapRegex::ex_between(df2[j+4, ], "Score = ", " bits")[[1]])){
        y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 2")
        hits <- rbind(hits, y)
        if(n == 0){
          df_final <- data.frame(Name = c(df2[seq(j,j+5,1), ]))
        } else{
          df_temp <- data.frame(Name = c(df2[seq(j,j+5,1), ]))
        df_final <- rbind(df_final, df_temp)
        }
      } else if(as.numeric(qdapRegex::ex_between(df[i+4, ], "Score = ", " bits")[[1]]) == as.numeric(qdapRegex::ex_between(df2[j+4, ], "Score = ", " bits")[[1]])){
        y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Same")
        hits <- rbind(hits, y)
          if(n == 0){
            df_final <- data.frame(Name = c(df[seq(i,i+5,1), ]))
          } else {
            df_temp <- data.frame(Name = c(df[seq(i,i+5,1), ]))
            df_final <- rbind(df_final, df_temp)
          }
      }
    } else if(df[i+2,] == "***** No hits found *****"){
        if(n == 0){
           if(substr(df2[j+2,],1,1) == ">"){
             y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 2")
             hits <- rbind(hits, y)
             df_final <- data.frame(Name = c(df2[seq(j,j+5,1), ]))
             } else{
               y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found")
               hits <- rbind(hits, y)
               df_final <- data.frame(Name = c(df2[seq(j,j+2,1), ]))
               }
      } else{
          if(substr(df2[j+2,],1,1) == ">"){
            y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 2")
            hits <- rbind(hits, y)
            df_temp <- data.frame(Name = c(df2[seq(j,j+5,1), ]))
            df_final <- rbind(df_final, df_temp)
            } else{
              y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found")
              hits <- rbind(hits, y)
              df_temp <- data.frame(Name = c(df2[seq(j,j+2,1), ]))
              df_final <- rbind(df_final, df_temp)
        }
      }
    } else if(df2[j+2,] == "***** No hits found *****"){
      if(n == 0){
        if(substr(df[i+2,],1,1) == ">"){
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 1")
          hits <- rbind(hits, y)
          df_final <- data.frame(Name = c(df[seq(i,i+5,1), ]))
        } else{
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found")
          hits <- rbind(hits, y)
          df_final <- data.frame(Name = c(df[seq(i,i+2,1), ]))
        }
      } else{
        if(substr(df[i+2,],1,1) == ">"){
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 1")
          hits <- rbind(hits, y)
          df_temp <- data.frame(Name = c(df[seq(i,i+5,1), ]))
          df_final <- rbind(df_final, df_temp)
        } else{
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found")
          hits <- rbind(hits, y)
          df_temp <- data.frame(Name = c(df2[seq(i,i+2,1), ]))
          df_final <- rbind(df_final, df_temp)
        }
      }
    }
  } else if(substr(df2[j,],1,6) == "Query="){
    while (df[i,] != df2[j,]) {
      setTxtProgressBar(pb,i)
      i <- i + 1
        }
    if(substr(df[i+2,],1,1) == ">" & substr(df2[j+2,],1,1) == ">"){
      if (as.numeric(qdapRegex::ex_between(df[i+4, ], "Score = ", " bits")[[1]]) > as.numeric(qdapRegex::ex_between(df2[j+4, ], "Score = ", " bits")[[1]])){
        y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 1")
        hits <- rbind(hits, y)
        if(n == 0){
            df_final <- data.frame(Name = c(df[seq(i,i+5,1), ]))
          } else {
            df_temp <- data.frame(Name = c(df[seq(i,i+5,1), ]))
            df_final <- rbind(df_final, df_temp)
          }  
      } else if(as.numeric(qdapRegex::ex_between(df[i+4, ], "Score = ", " bits")[[1]]) < as.numeric(qdapRegex::ex_between(df2[j+4, ], "Score = ", " bits")[[1]])){
        y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 2")
        hits <- rbind(hits, y)
        if(n == 0){
          df_final <- data.frame(Name = c(df2[seq(j,j+5,1), ]))
        } else{
          df_temp <- data.frame(Name = c(df2[seq(j,j+5,1), ]))
        df_final <- rbind(df_final, df_temp)
        }
      }  else if(as.numeric(qdapRegex::ex_between(df[i+4, ], "Score = ", " bits")[[1]]) == as.numeric(qdapRegex::ex_between(df2[j+4, ], "Score = ", " bits")[[1]])){
        y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Same")
        hits <- rbind(hits, y)
          if(n == 0){
            df_final <- data.frame(Name = c(df[seq(i,i+5,1), ]))
          } else {
            df_temp <- data.frame(Name = c(df[seq(i,i+5,1), ]))
            df_final <- rbind(df_final, df_temp)
          }
      }
    } else if(df[i+2,] == "***** No hits found *****"){
        if(n == 0){
           if(substr(df2[j+2,],1,1) == ">"){
             y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 2")
             hits <- rbind(hits, y)
             df_final <- data.frame(Name = c(df2[seq(j,j+5,1), ]))
             } else{
               y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found")
               hits <- rbind(hits, y)
               df_final <- data.frame(Name = c(df2[seq(j,j+2,1), ]))
               }
      } else{
          if(substr(df2[j+2,],1,1) == ">"){
            y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 2")
            hits <- rbind(hits, y)
            df_temp <- data.frame(Name = c(df2[seq(j,j+5,1), ]))
            df_final <- rbind(df_final, df_temp)
            } else{
              y <- data.frame(NODE = df2[j,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found")
              hits <- rbind(hits, y)
              df_temp <- data.frame(Name = c(df2[seq(j,j+2,1), ]))
              df_final <- rbind(df_final, df_temp)
        }
      }
    } else if(df2[j+2,] == "***** No hits found *****"){
      if(n == 0){
        if(substr(df[i+2,],1,1) == ">"){
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 1")
          hits <- rbind(hits, y)
          df_final <- data.frame(Name = c(df[seq(i,i+5,1), ]))
        } else{
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found")
          hits <- rbind(hits, y)
          df_final <- data.frame(Name = c(df[seq(i,i+2,1), ]))
        }
      } else{
        if(substr(df[i+2,],1,1) == ">"){
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "Replaced by Sequence 1")
          hits <- rbind(hits, y)
          df_temp <- data.frame(Name = c(df[seq(i,i+5,1), ]))
          df_final <- rbind(df_final, df_temp)
        } else{
          y <- data.frame(NODE = df[i,], Sequence_1 = df[i+2,], Sequence_2 = df2[j+2,], Type = "No hits found")
          hits <- rbind(hits, y)
          df_temp <- data.frame(Name = c(df2[seq(i,i+2,1), ]))
          df_final <- rbind(df_final, df_temp)
        }
      }
    }
    
    }
  
  if(i == length(df[,1]) | j == length(df2[,1])){
    break
    
  }
  n <- n + 1
  i <- i + 1
  j <- j + 1
}
close(pb)


write.table(df_final, file=args[3], row.names=FALSE) #write the final data frame
write.table(hits, file="hits.txt",row.names = FALSE)
