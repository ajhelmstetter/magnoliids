library(gplots)
library(ggVennDiagram)
library(ape)

#Calculate sizes for gene and sample labels, increase the multiplier to make text bigger
gene.size.multiplier = .1
sample.size.multiplier = .5

sample.filename = "data/test_seq_lengths.txt"
sample.data= as.matrix(read.table(sample.filename,header=T,row.names = 1, sep="\t"))

#length reference exon
reference.len = as.numeric(sample.data[1,])

#read in meta data
meta_df<-read.csv("data/sample_data - samples_for_phylo.csv")

#extract only rows used in hybpiper run
meta_df<-meta_df[meta_df$Namelist%in%rownames(sample.data),]

#orders
canel<-meta_df[meta_df$Order=="Canellales",]$Namelist
chlor<-meta_df[meta_df$Order=="Chloranthales",]$Namelist
laura<-meta_df[meta_df$Order=="Laurales",]$Namelist
magno<-meta_df[meta_df$Order=="Magnoliales",]$Namelist
piper<-meta_df[meta_df$Order=="Piperales",]$Namelist

#make list of orders
ord_l<-list(canel,chlor,laura,magno,piper)

#set names
names(ord_l)<- c("Canellales",
"Chloranthales",
"Laurales",
"Magnoliales",
"Piperales")

#empty list for storage
l_50_50<-list()

for(i in 1:5){

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

#Extract only 50_50 exons from original dataset
length50_50<-sample.data[,col>=0.50]

#Get a list of names of 50_50 exons
l_50_50[[i]]<-names(length50_50[1,])

}

#set names
names(l_50_50)<- c("Canellales",
                 "Chloranthales",
                 "Laurales",
                 "Magnoliales",
                 "Piperales")

# Default plot
ggVennDiagram(l_50_50,
              #label = c("percent"),
              label_alpha = 0.5,
              label_geom = c("label")) +
  ggplot2::scale_fill_distiller(palette = "Greys", direction=1) +
  ggplot2::theme(legend.position = "none")

ggplot2::ggsave("figures/figSX_venn.png",width = 12.5,height =12.5)

