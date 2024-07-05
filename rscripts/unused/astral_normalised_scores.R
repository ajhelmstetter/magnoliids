rm(list = ls())

#list of filepaths to read in
filepaths <-
  list.files(
    path = paste(here::here(),"/data/trees",sep=""),
    pattern = "*LPP.log",
    full.names = TRUE,
    recursive = TRUE
  )

#names of analyses used to make trees
filenames <-
  list.files(
    path = paste(here::here(),"/data/trees",sep=""),
    pattern = "*LPP.log",
    full.names = FALSE,
    recursive = TRUE
  )

filenames <- gsub("/astral_bs10_LPP.log", "", filenames)

#empty vectors
scores<-vector()
quartets<-vector()


for(i in 1:length(filepaths)){

  #read in log file
  astral_log <- readLines(filepaths[i])

  #get lines with info
  score <- astral_log[grep("Normalized score",astral_log)]
  quartet <- astral_log[grep("quartet trees in the gene trees",astral_log)]

  #convert to numbers
  #put in vectors
  scores[i] <- as.numeric(gsub(".*:","",score))
  quartets[i] <- as.numeric(gsub(".*:","",quartet))
}


names(scores)<-filenames
sort(scores)

names(quartets)<-filenames
sort(quartets)

names(scores)==names(quartets)

plot(scores~quartets)
text(quartets,scores,labels=names(scores),cex=0.5,pos=4)

library(ggplot2)
library(ggrepel)

ggplot(data.frame(scores,quartets), aes(x = scores, y = quartets)) +
  geom_point(
    color="black",
    shape=21,
    alpha=0.5,
    size=3,
    stroke = 0.5
  ) + xlim(0.925,0.97) +
  geom_text_repel(label = rownames(data.frame(scores,quartets)),
                  size = 3)
