rm(list=ls())

#set ASTRAL output folder path
iqf<-"data/trees/iqtree_r10_l50_i50/"

#set sample data filepath
sample_data<-"data/sample_data - samples_for_phylo_AZ.csv"

#set PDF name
pdf("figures/iqtree_r10_l50_i50.pdf",width=10,height=20)

####
# IQTREE
####

par(mar = c(0, 0, 0, 0))

library(ape)
library(phylotools)
library(RColorBrewer)
library(tidyr)

palette(colorRampPalette(brewer.pal(name = "Set1", n = 8))(17))

#read in tree and tips
phy <- read.tree(paste(iqf,"magnoliids_r10_l50_i50.partitions.contree",sep=""))
df <- read.csv(sample_data)
df <- df[df$Namelist %in% phy$tip.label,]
df <-
  df %>% unite("genus_species",
               c(Family:Species,Source),
               na.rm = TRUE,
               remove = FALSE)

head(df)

##match order of namelist and tip labels
df<-df[match(phy$tip.label, df$Namelist),]

#diffs
setdiff(phy$tip.label, df$Namelist)
setdiff(df$Namelist, phy$tip.label)

##tip label change
dfnames <- data.frame(df$Namelist, df$genus_species)
phy <- sub.taxa.label(phy, dfnames)

#cols
df <- df[match(phy$tip.label, df$genus_species),]

#plot tree QS
phy <- ape::root(phy, "Chloranthaceae_Chloranthus_spicatus_PAFTOL", edgelabel = T, resolve.root = T)
phy <- ladderize(phy)
plot(
  phy,
  cex = 0.5,
#  label.offset = 0.005,
  align.tip.label = T,
  edge.width = 2,
  edge.color = "grey",
  tip.color = as.numeric(as.factor(df$Order))
)
p <- character(length(phy$node.label))
nodelabels(
  pie = cbind(
    as.numeric(phy$node.label),
    100 - as.numeric(phy$node.label)
  ),
  piecol = c("black", "white"),
  cex = 0.15
)

dev.off()