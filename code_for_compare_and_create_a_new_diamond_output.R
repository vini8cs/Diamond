#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

# test if there is at least two argument: if not, return an error
if (length(args) < 2) {
  stop("Exactly three arguments must be supplied, including the name of the output last (input file).n", call.=FALSE)
} 

if(!require(qdapRegex)){
  cat("Installing qdapRegex package...\n")
  install.packages("qdapRegex")
}

cat("Reading input files...\n")


x <- read.delim(args[1])
y <- read.delim(args[2])

if(length(x[,1]) <= length(y[,1])){
  df<- x
  df2 <- y
} else{
  df <- y
  df2 <- x
}

cat("Processing first dataset...\n")

pb <- txtProgressBar(min = 0, max = nrow(df), style = 3)
n <- 0
Expect_max <- 10000000
Identities_max <- -1000000
df_final <- data.frame(Name = " ")
for (i in 1:nrow(df)){
  setTxtProgressBar(pb,i)
  if(substr(df[i,],1,6) == "Query="){
     if(n == 1){
      df_final <- rbind(df_final, df_temp)
      n <- n - 1
    }
    df_temp <- data.frame(Name = c(df[i,], df[i+1,]))
    df_final <- rbind(df_final, df_temp)
    Expect_max <- 10000000
    Identities_max <- -1000000
    n <- n + 1
  } else if(substr(df[i,],1,1) == ">"){
    
    Identities <- as.numeric(qdapRegex::ex_between(df[i+3,], "(", "%)")[[1]])[1]
    Expect <- as.numeric(sub('.+Expect = (.+)', '\\1', df[i+2,]))
    
    
    if(Expect < Expect_max | Identities > Identities_max){
      Expect_max = Expect
      Identities_max = Identities
      df_temp <- data.frame(Name = c(df[seq(i,i+3,1),]))
    } 
  } else if(df[i,] == "***** No hits found *****"){
    df_temp <- data.frame(Name = c(df[i,],"N","N","N"))
    df_final <- rbind(df_final, df_temp)
    n <- n - 1
  }
}
close(pb)

cat("Done!\n")

cat("Processing second dataset...\n")

pb <- txtProgressBar(min = 0, max = nrow(df2), style = 3)
n <- 0
Expect_max <- 10000000
Identities_max <- -1000000
df_final2 <- data.frame(Name = " ")
for (i in 1:nrow(df2)){
  setTxtProgressBar(pb,i)
  if(substr(df2[i,],1,6) == "Query="){
     if(n == 1){
      df_final2 <- rbind(df_final2, df_temp)
      n <- n - 1
    }
    df_temp <- data.frame(Name = c(df2[i,], df2[i+1,]))
    df_final2 <- rbind(df_final2, df_temp)
    Expect_max <- 10000000
    Identities_max <- -1000000
    n <- n + 1
  } else if(substr(df2[i,],1,1) == ">"){
    
    Identities <- as.numeric(qdapRegex::ex_between(df2[i+3,], "(", "%)")[[1]])[1]
    Expect <- as.numeric(sub('.+Expect = (.+)', '\\1', df2[i+2,]))
    
    
    if(Expect < Expect_max | Identities > Identities_max){
      Expect_max = Expect
      Identities_max = Identities
      df_temp <- data.frame(Name = c(df2[seq(i,i+3,1),]))
    } 
  } else if(df2[i,] == "***** No hits found *****"){
    df_temp <- data.frame(Name = c(df2[i,],"N","N","N"))
    df_final2 <- rbind(df_final2, df_temp)
    n <- n - 1
  }
}
close(pb)

cat("Done!\n")


cat("Starting comparing process of Diamond outputs. This may take a while!\n")
cat("Go grab a coffe if you want :)\n")

hits <- data.frame(Contig = " ", Sequence_1= " ", Sequence_2 = " ", Type = " ")
df_final3 <- data.frame(Name = "")
pb <- txtProgressBar(min = 0, max = nrow(df_final2), style = 3) #progress bar
for(i in 1:nrow(df_final2)){
  setTxtProgressBar(pb,i)
  if(substr(df_final[i,],1,6) == "Query="){
    df_temp <- data.frame(Name = c(df_final[i, ], df_final[i+1, ]))
    df_final3 <- rbind(df_final3, df_temp)
    
    if(substr(df_final[i+2,],1,1) == ">" & substr(df_final2[i+2,],1,1) == ">"){
    
    Identities1 <- as.numeric(qdapRegex::ex_between(df_final[i+5,], "(", "%)")[[1]])[1]
    Identities2 <- as.numeric(qdapRegex::ex_between(df_final2[i+5,], "(", "%)")[[1]])[1]
    Expect1 <- as.numeric(sub('.+Expect = (.+)', '\\1', df_final[i+4,]))
    Expect2 <- as.numeric(sub('.+Expect = (.+)', '\\1', df_final2[i+4,]))
    
    if (Expect2 <= Expect1 | Identities2 >= Identities1){
       hit <- data.frame(Contig = df_final2[i,], Sequence_1 = df_final[i+2,], Sequence_2 = df_final2[i+2,], Type = "Replaced by Sequence 2")
       hits <- rbind(hits, hit)
       
       df_temp <- data.frame(Name = c(df_final2[seq(i+2,i+5,1), ]))
       df_final3 <- rbind(df_final3, df_temp)
       
  } else if(Expect1 < Expect2 | Identities1 > Identities2){
       hit <- data.frame(Contig = df_final[i,], Sequence_1 = df_final[i+2,], Sequence_2 = df_final2[i+2,], Type = "Replaced by Sequence 1")
       hits <- rbind(hits, hit)
       
       df_temp <- data.frame(Name = c(df_final[seq(i+2,i+5,1), ]))
       df_final3 <- rbind(df_final3, df_temp)
       
  } else if(Expect1 == Expect2 & Identities1 == Identities2){
       hit <- data.frame(Contig = df_final[i,], Sequence_1 = df_final[i+2,], Sequence_2 = df_final2[i+2,], Type = "Equal")
       hits <- rbind(hits, hit)
       
       df_temp <- data.frame(Name = c(df_final[seq(i+2,i+5,1), ]))
       df_final3 <- rbind(df_final3, df_temp)
  }
    } else if(df_final[i+2,] == "***** No hits found *****"){
      if(substr(df_final2[i+2,],1,1) == ">"){
        hit <- data.frame(Contig = df_final2[i,], Sequence_1 = df_final[i+2,], Sequence_2 = df_final2[i+2,], Type = "Replaced by Sequence 2")
        hits <- rbind(hits, hit)
        
        df_temp <- data.frame(Name = c(df_final2[seq(i+2,i+5,1), ]))
        df_final3 <- rbind(df_final3, df_temp)
        } else{
          hit <- data.frame(Contig = df_final[i,], Sequence_1 = df_final[i+2,], Sequence_2 = df_final2[i+2,], Type = "No hits found")
          hits <- rbind(hits, hit)
          df_temp <- data.frame(Name = c(df_final[seq(i+2,i+5,1), ]))
          df_final3 <- rbind(df_final3, df_temp)
          }
      } else if(df_final2[i+2,] == "***** No hits found *****"){
        if(substr(df_final[i+2,],1,1) == ">"){
          hit <- data.frame(Contig = df_final[i,], Sequence_1 = df_final[i+2,], Sequence_2 = df_final2[i+2,], Type = "Replaced by Sequence 1")
          hits <- rbind(hits, hit)
          
          df_temp <- data.frame(Name = c(df_final[seq(i+2,i+5,1), ]))
          df_final3 <- rbind(df_final3, df_temp)
          } else{
            hit <- data.frame(Contig = df_final[i,], Sequence_1 = df_final[i+2,], Sequence_2 = df_final2[i+2,], Type = "No hits found")
            hits <- rbind(hits, hit)
            
            df_temp <- data.frame(Name = c(df_final[seq(i+2,i+5,1), ]))
            df_final3 <- rbind(df_final3, df_temp)
      }
  }
  }
}
close(pb)

cat("Done!\n")



write.table(df_final3, file="final_diamond_output.txt", row.names=FALSE)

write.table(hits, file="hits.txt",row.names = FALSE)  #write the hits data frame
write.table(df_final, file="first_diamond_input.txt",row.names = FALSE)
write.table(df_final2, file="second_diamond_input.txt",row.names = FALSE)
