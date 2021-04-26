
library(ape)

#Calculate sizes for gene and sample labels, increase the multiplier to make text bigger
gene.size.multiplier = .1
sample.size.multiplier = .5

sample.filename = "data/test_seq_lengths.txt"
sample.data= as.matrix(read.table(sample.filename,header=T,row.names=1,sep="\t"))

#length reference exon
reference.len = as.numeric(sample.data[1,])

#families
canel<-read.table("data/canel.txt")
canel<-as.character(canel$V1)

chlor<-read.table("data/chlor.txt")
chlor<-as.character(chlor$V1)

laura<-read.table("data/laura.txt")
laura<-as.character(laura$V1)

magno<-read.table("data/magno.txt")
magno<-as.character(magno$V1)

piper<-read.table("data/piper.txt")
piper<-as.character(piper$V1)

c(canel,chlor,laura,magno,piper)

ord_l<-list(canel,chlor,laura,magno,piper,c(canel,chlor,laura,magno,piper))

l_75_75<-list()

for(i in 1:6){

#extract only sample exon lengths
sample.len = sample.data[rownames(sample.data) %in% ord_l[[i]],]

#calculate percentage of exon recovered for each exon in each individual
percent.len= sweep(sample.len, 2, reference.len, "/")
percent.len = ifelse(percent.len>1,1,percent.len)
percent.len.limit <- percent.len

# if percentage exon length recovered is >= 75% make value 1
# if not make value 0
percent.len.limit[percent.len.limit>=0.5] = 1
percent.len.limit[percent.len.limit<0.5] = 0

#For each exon calculate number of individuals with >= 75% of exon
col <- colSums(percent.len.limit != 0)

#Calculate % individuals with >= 75% of exon for each exon
col <- col / (length(sample.len[,1]))

#Extract only 75_75 exons from original dataset
length7575<-sample.data[,col>=0.50]

#Get a list of names of 75_75 exons
l_75_75[[i]]<-names(length7575[1,])

}

library(gplots)

png("figures/venn.png", height = 20, width = 20, units = "cm", res = 100)
par(mar=c(0,0,0,0))

venn(list(Canellales=l_75_75[[1]],
          Chloranthales=l_75_75[[2]],
          Laurales=l_75_75[[3]],
          Magnoliales=l_75_75[[4]],
          Piperales=l_75_75[[5]]))

dev.off()

x<-list(Canellales=l_75_75[[1]],
        Chloranthales=l_75_75[[2]],
        Laurales=l_75_75[[3]],
        Magnoliales=l_75_75[[4]],
        Piperales=l_75_75[[5]])

library("ggVennDiagram")
# Default plot
ggVennDiagram(x,
              label = c("percent"),
              label_alpha = 0.5,
              label_geom = c("label"))

ggplot2::ggsave("figures/venn2.png")

####
# lactoris densities
####

lactoris_df<-data.frame(c(sample.data["P_019189", ], sample.data["P0102B", ]),
           c(rep("P_019189", 353), rep("P0102B", 353)))

colnames(lactoris_df)<-c("exon_length","name")

ggplot(lactoris_df,aes(x=exon_length, fill=name)) +
  geom_density(alpha=0.5)
