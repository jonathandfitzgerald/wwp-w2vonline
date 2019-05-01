library("rjson")
library(magrittr)
library(tidyverse)
library(wordVectors)
library(DT)
library(wordcloud)


flname <- "/home/parth/work_files/summer-2018/wwp_tool_dsg_folder/wwp-w2vonline/data/wwo_reg-morph_allTexts.bin"

vector <- read.vectors(flname) 

data <- as.matrix(vector) 


print(data)


#fileConn<-file("output.txt")
words <- c()
x <- 1
for (word in rownames(vector))
{
  vectStr <- word
#  for (vect in vector[[word]])
#  {   
#    vectStr <- paste(vectStr, vect, sep=" ")  
#  }
  
  vectStr <- paste(as.vector(vector[[word]]), sep=" ", collapse=" ")
  vectStr <- paste(word, vectStr, sep=" ", collapse="")
  
  words <- append(vectStr,words) 
  
  x <- x + 1
  if(x == 1000)
  {
    break
  }

}
writeLines(words, "output.txt")




