
###
# ASTRAL
###

par(mar=c(0,0,0,0))

library(ape)
library(phylotools)
library(RColorBrewer)
library(tidyr)

palette(colorRampPalette(brewer.pal(name="Set1", n = 8))(17))

#read in tree and tips
phy <- read.tree("data/trees/astral_all_bs10_lpp.tre")
df <- read.csv("data/taxonomy_namelist.csv")
df <- df[df$Namelist %in% phy$tip.label, ]
df <-df %>% unite("genus_species", Genus:Species, na.rm = TRUE, remove = FALSE)

##tip label change
dfnames <- data.frame(df$Namelist, df$genus_species)
phy <- sub.taxa.label(phy, dfnames)
phy$edge.length[phy$edge.length == "NaN"] <- 0.25

#cols
df <- df[match(phy$tip.label, df$genus_species), ]

#plot tree QS
phy <- root(phy, "Chloranthus_spicatus", edgelabel = T)
phy <- ladderize(phy)
plot(
  phy,
  cex = 0.5,
  label.offset = 0.05,
  align.tip.label = T,
  edge.width = 2,
  edge.color = "grey",
  tip.color = as.numeric(as.factor(df$Order))
)
p <- character(length(phy$node.label))

phy <- read.tree("data/trees/astral_all_bs10_QS.tre")
nodelabels(
  pie = cbind(as.numeric(phy$node.label), 100 - as.numeric(phy$node.label)),
  piecol = c("grey", "white"),
  cex = 0.25
)

phy <- read.tree("data/trees/astral_all_bs10_LPP.tre")
nodelabels(
  pie = cbind(as.numeric(phy$node.label), 1 - as.numeric(phy$node.label)),
  piecol = c("black", "white"),
  cex = 0.125
)

legend(
  'bottomleft',
  legend = c(
    'canellales',
    'chloranthales',
    'laurales',
    'magnoliales',
    'piperales',
    'Local PP',
    'Quartet support'
  ),
  text.col = c(1, 2, 3, 4, 5,"black","black"),
  pt.cex = c(NA,NA,NA,NA,NA,2,2),
  pch = c(NA,NA,NA,NA,NA,16,16),
  col = c(NA,NA,NA,NA,NA,"black","grey"),
  bty = 'n'
)


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
