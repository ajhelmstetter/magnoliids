library(gplots)
library(ggVennDiagram)
library(ape)

#read in recovery data
sample.filename = "data/test_seq_lengths.txt"
sample.data= as.matrix(read.table(sample.filename,header=T,row.names = 1, sep="\t"))

#length reference exon
reference.len = as.numeric(sample.data[1,])

#read in meta data
meta_df<-read.csv("data/sample_data - samples_for_phylo.csv")

#extract only rows used in hybpiper run
meta_df<-meta_df[meta_df$Namelist%in%rownames(sample.data),]

#families
canel<-meta_df[meta_df$Order=="Canellales",]$Namelist
chlor<-meta_df[meta_df$Order=="Chloranthales",]$Namelist
laura<-meta_df[meta_df$Order=="Laurales",]$Namelist
magno<-meta_df[meta_df$Order=="Magnoliales",]$Namelist
piper<-meta_df[meta_df$Order=="Piperales",]$Namelist

#order
ord_l<-list(canel,chlor,laura,magno,piper)


###
# 25_25
###

#empty list for storage
l_25_25<-vector()

for(i in 1:length(ord_l)){

  #extract only sample exon lengths
  sample.len = sample.data[rownames(sample.data) %in% ord_l[[i]],]

  #calculate percentage of exon recovered for each exon in each individual
  percent.len= sweep(sample.len, 2, reference.len, "/")
  percent.len = ifelse(percent.len>1,1,percent.len)
  percent.len.limit <- percent.len

  # if percentage exon length recovered is >= 50% make value 1
  # if not make value 0
  percent.len.limit[percent.len.limit>=0.25] = 1
  percent.len.limit[percent.len.limit<0.25] = 0

  #For each exon calculate number of individuals with >= 50% of exon
  col <- colSums(percent.len.limit != 0)

  #Calculate % individuals with >= 50% of exon for each exon
  col <- col / (length(sample.len[,1]))
  head(col)

  #number of exons
  l_25_25[i]<-sum(col>=0.25)

}

###
# 10_10
###

#empty list for storage
l_10_10<-vector()

for(i in 1:length(ord_l)){

  #extract only sample exon lengths
  sample.len = sample.data[rownames(sample.data) %in% ord_l[[i]],]

  #calculate percentage of exon recovered for each exon in each individual
  percent.len= sweep(sample.len, 2, reference.len, "/")
  percent.len = ifelse(percent.len>1,1,percent.len)
  percent.len.limit <- percent.len

  # if percentage exon length recovered is >= 10% make value 1
  # if not make value 0
  percent.len.limit[percent.len.limit>=0.1] = 1
  percent.len.limit[percent.len.limit<0.1] = 0

  #For each exon calculate number of individuals with >= 10% of exon
  col <- colSums(percent.len.limit != 0)

  #Calculate % individuals with >= 10% of exon for each exon
  col <- col / (length(sample.len[,1]))
  head(col)

  #number of exons
  l_10_10[i]<-sum(col>=0.1)

}

###
# 25_25
###

#empty list for storage
l_25_25<-vector()

for(i in 1:length(ord_l)){

  #extract only sample exon lengths
  sample.len = sample.data[rownames(sample.data) %in% ord_l[[i]],]

  #calculate percentage of exon recovered for each exon in each individual
  percent.len= sweep(sample.len, 2, reference.len, "/")
  percent.len = ifelse(percent.len>1,1,percent.len)
  percent.len.limit <- percent.len

  # if percentage exon length recovered is >= 25% make value 1
  # if not make value 0
  percent.len.limit[percent.len.limit>=0.25] = 1
  percent.len.limit[percent.len.limit<0.25] = 0

  #For each exon calculate number of individuals with >= 25% of exon
  col <- colSums(percent.len.limit != 0)

  #Calculate % individuals with >= 25% of exon for each exon
  col <- col / (length(sample.len[,1]))
  head(col)

  #number of exons
  l_25_25[i]<-sum(col>=0.25)

}

###
# 50_50
###

#empty list for storage
l_50_50<-vector()

for(i in 1:length(ord_l)){

  #extract only sample exon lengths
  sample.len = sample.data[rownames(sample.data) %in% ord_l[[i]],]

  #calculate percentage of exon recovered for each exon in each individual
  percent.len= sweep(sample.len, 2, reference.len, "/")
  percent.len = ifelse(percent.len>1,1,percent.len)
  percent.len.limit <- percent.len

  # if percentage exon length recovered is >= 50% make value 1
  # if not make value 0
  percent.len.limit[percent.len.limit>=0.5] = 1
  percent.len.limit[percent.len.limit<0.5] = 0

  #For each exon calculate number of individuals with >= 50% of exon
  col <- colSums(percent.len.limit != 0)

  #Calculate % individuals with >= 50% of exon for each exon
  col <- col / (length(sample.len[,1]))
  head(col)

  #number of exons
  l_50_50[i]<-sum(col>=0.5)

}

###
# 75_75
###

#empty list for storage
l_75_75<-vector()

for(i in 1:length(ord_l)){

  #extract only sample exon lengths
  sample.len = sample.data[rownames(sample.data) %in% ord_l[[i]],]

  #calculate percentage of exon recovered for each exon in each individual
  percent.len= sweep(sample.len, 2, reference.len, "/")
  percent.len = ifelse(percent.len>1,1,percent.len)
  percent.len.limit <- percent.len

  # if percentage exon length recovered is >= 75% make value 1
  # if not make value 0
  percent.len.limit[percent.len.limit>=0.75] = 1
  percent.len.limit[percent.len.limit<0.75] = 0

  #For each exon calculate number of individuals with >= 75% of exon
  col <- colSums(percent.len.limit != 0)

  #Calculate % individuals with >= 75% of exon for each exon
  col <- col / (length(sample.len[,1]))
  head(col)

  #number of exons
  l_75_75[i]<-sum(col>=0.75)

}

#combine filtering
t1<-data.frame(l_10_10,
           l_25_25,
           l_50_50,
           l_75_75)

#add rownames (should be same as order of groupings)
rownames(t1)<-c(sort(unique(meta_df$Order)))

#output csv file
write.csv(t1,"outputs/table1.csv")
