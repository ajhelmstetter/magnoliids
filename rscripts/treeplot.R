library(ape)
library(phylotools)
par(mar=c(1,1,1,1))

setwd("~/Dropbox/projects/AJH_magnoliids/")

###
# QS tree
###

#read in tree and tips
phy<-read.tree("astral_all_mag_bs10_QS.tre")
df<-read.csv("rename_tips.csv")
df<-df[df$tip_name%in%phy$tip.label,]

##tip label change
dfnames<-data.frame(df$tip_name,df$genus_species)
phy<-sub.taxa.label(phy,dfnames)
phy$edge.length[phy$edge.length=="NaN"]<-0.001

#plot tree QS
jpeg("astral_mag_bs10_QS.jpg",width = 2500, height = 2500,res=300)
phy<-root(phy,"Chloranthus_spicatus",edgelabel = T)
phy<-ladderize(phy)
plot(phy,cex=0.7,label.offset = 0.1)
p <- character(length(phy$node.label))
nodelabels(pie=cbind(as.numeric(phy$node.label),100-as.numeric(phy$node.label)),
           piecol=c("black","white"),cex=0.3)
dev.off()

###
# LPP tree
###

#read in tree and tips
phy<-read.tree("astral_all_mag_bs10.tre")
df<-read.csv("rename_tips.csv")
df<-df[df$tip_name%in%phy$tip.label,]

##tip label change
dfnames<-data.frame(df$tip_name,df$genus_species)
phy<-sub.taxa.label(phy,dfnames)
phy$edge.length[phy$edge.length=="NaN"]<-0.001

#plot tree QS
jpeg("astral_mag_bs10.jpg",width = 2500, height = 2500,res=300)
phy<-root(phy,"Chloranthus_spicatus",edgelabel = T)
phy<-ladderize(phy)
plot(phy,cex=0.7,label.offset = 0.1)
p <- character(length(phy$node.label))
nodelabels(pie=cbind(as.numeric(phy$node.label),1-as.numeric(phy$node.label)),
           piecol=c("black","white"),cex=0.3)
dev.off()


####
# IQTREE
####

phy<-read.tree("~/Dropbox/projects/TC_raphia/iqtree/raphia.partitions.contree")
df<-read.csv("tip_names.csv")
df<-df[df$old%in%phy$tip.label,]
dfnames<-data.frame(df$old,df$new)
dfnames<-dfnames[phy$tip.label%in%dfnames$df.old,]
phy<-sub.taxa.label(phy,dfnames)
plot(phy)
nodelabels()

phy<-root(phy,node=110)
phy<-ladderize(phy)
plot(phy,cex=0.7,label.offset = 0.001)

iqphy<-drop.tip(phy,c("Mauritiella_armata_R37_T6","Laccosperma_cristalensis_R41_T41","Eremospatha_cabrae_R67_T28","Eremospatha_quiquecostulata_R41_T39"))

jpeg("iqtree_raphia.jpg",width = 2500, height = 2500,res=300)
plot(iqphy,cex=0.7,label.offset = 0.001)
p <- character(length(iqphy$node.label))
nodelabels(pie=cbind(as.numeric(iqphy$node.label),100-as.numeric(iqphy$node.label)),
           piecol=c("black","white"),cex=0.3)
dev.off()
