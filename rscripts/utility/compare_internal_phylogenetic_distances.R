rm(list = ls())

library(ape)
library(phylotools)
library(RColorBrewer)
library(tidyr)

#set ASTRAL output folder path
af <- "data/trees/astral_magus_trimal_auto_r10_l50_i50/"

#set sample data filepath
sample_data <- "data/sample_data - samples_for_phylo.csv"

#read in tree and tips
phy <- read.tree(paste(af, "astral_bs10_LPP.tre", sep = ""))

#insert arbitrary terminal branch length so internal branch lengths are kept
phy$edge.length[phy$edge.length == "NaN"] <- 0

#pairwise distances
cophenetic.phylo(phy)


#list of filepaths to read in
filepaths <-
  list.files(
    path = "/home/andrew/Dropbox/projects/AJH_magnoliids/PAFTOL_magnoliids/data/trees",
    pattern = "*LPP.tre",
    full.names = TRUE,
    recursive = TRUE
  )

#list of filepaths to read in
filenames <-
  list.files(
    path = "/home/andrew/Dropbox/projects/AJH_magnoliids/PAFTOL_magnoliids/data/trees",
    pattern = "*LPP.tre",
    full.names = FALSE,
    recursive = TRUE
  )

filenames <- gsub("/astral_bs10_LPP.tre", "", filenames)

#empty list
pwdist_list <- list()

head(filepaths)

for (i in 1:length(filepaths)) {
  #read in tree
  phy <- read.tree(filepaths[[i]])

  #insert arbitrary terminal branch length so internal branch lengths are kept
  phy$edge.length[phy$edge.length == "NaN"] <- 0

  #calc pairwise distances
  pd_phy <- cophenetic.phylo(phy)

  #alphabetically order row and column names
  pd_phy <- pd_phy[order(rownames(pd_phy)), ]
  pd_phy <- pd_phy[, order(colnames(pd_phy))]

  #put in list
  pwdist_list[[i]] <- pd_phy

}

#set names of items in list
names(pwdist_list) <- filenames

#check names
rownames(pwdist_list[[2]]) == rownames(pwdist_list[[4]])

#plot agains each other
plot(pwdist_list[[1]] ~ pwdist_list[[2]])
abline(1, 1, col = "red", lty = 2)

#NOTE: some phylos l75i75, r25, r50 have species that drop out

for (i in 1:17) {

  if(i==1){

    #make two columns with allpairs of species
    xy <- t(combn(colnames(pwdist_list[[i]]), 2))

    #add distance column
    pwdist_melted <- data.frame(xy, dist=pwdist_list[[i]][xy])

  } else {

    pwdist_melted <- data.frame(pwdist_melted,pwdist_list[[i]][xy])

  }

}



#calc sd per row
row_stdev <- apply(pwdist_melted[,3:19], 1, sd, na.rm=TRUE)

#add column to melted df
pwdist_melted$stdev <- row_stdev

#sort by sd
pwdist_melted <- pwdist_melted[order(pwdist_melted$stdev,decreasing = TRUE),]

#plot distribution of stdev values
plot(pwdist_melted$stdev)

#high sd values only
pwdist_melted_high_sd<-pwdist_melted[pwdist_melted$stdev>2.25,]

#species that most frequency appear in pairs with high standard deviation
sort(table(c(pwdist_melted_high_sd$X1,pwdist_melted_high_sd$X2)),decreasing = T)

#distribution of frequencies of species appearance
#Appears that ~50 is the breakpoint
plot(as.numeric(sort(table(c(pwdist_melted_high_sd$X1,pwdist_melted_high_sd$X2)),decreasing = T)))
