rm(list=ls())

#set ASTRAL output folder path
af<-"data/trees/astral_r10_l50_i50/"

#set sample data filepath
sample_data<-"data/sample_data - samples_for_phylo.csv"

#set PDF name
pdf("figures/myristicaceae.pdf",width=10,height=10)

####
# Plot
####

par(mar = c(0, 0, 0, 0))

library(ape)
library(phylotools)
library(RColorBrewer)
library(tidyr)

palette(colorRampPalette(brewer.pal(name = "Set1", n = 8))(17))

#read in tree and tips
phy <- read.tree(paste(af,"astral_bs10_LPP.tre",sep=""))
df <- read.csv(sample_data)
df <- df[df$Namelist %in% phy$tip.label,]
df <-
  df %>% unite("genus_species",
               c(Family:Species,Source),
               na.rm = TRUE,
               remove = FALSE)

##match order of namelist and tip labels
df<-df[match(phy$tip.label, df$Namelist),]

#diffs
setdiff(phy$tip.label, df$Namelist)
setdiff(df$Namelist, phy$tip.label)

##tip label change
dfnames <- data.frame(df$Namelist, df$genus_species)
phy <- sub.taxa.label(phy, dfnames)
phy$edge.length[phy$edge.length == "NaN"] <- 0.25

#cols
df <- df[match(phy$tip.label, df$genus_species),]

#extract Myristicaceae subclade
phy<-extract.clade(phy,node = 335)

#correct tip label
phy$tip.label[28]<-"Myristicaceae_Iryanthera_sp._SMDO"

#plot tree QS
#phy <- ape::root(phy, "Chloranthaceae_Chloranthus_spicatus_PAFTOL", edgelabel = T, resolve.root = T)
phy <- ladderize(phy)
plot(
  phy,
  cex = 0.75,
  label.offset = 0.05,
  align.tip.label = T,
  edge.width = 2,
  edge.color = "grey")

p <- character(length(phy$node.label))

phy <- read.tree(paste(af,"astral_bs10_QS.tre",sep=""))
phy<-extract.clade(phy,node = 335)

nodelabels(
  pie = cbind(as.numeric(phy$node.label), 100 - as.numeric(phy$node.label)),
  piecol = c("grey", "white"),
  cex = 0.5
)

phy <- read.tree(paste(af,"astral_bs10_LPP.tre",sep=""))
phy<-extract.clade(phy,node = 335)
nodelabels(
  pie = cbind(as.numeric(phy$node.label), 1 - as.numeric(phy$node.label)),
  piecol = c("black", "white"),
  cex = 0.25
)

legend(
  'bottomleft',
  legend = c(
    'Local PP',
    'Quartet support'
  ),
  text.col = c("black", "black"),
  pt.cex = c( 2, 2),
  pch = c( 16, 16),
  col = c( "black", "grey"),
  bty = 'n'
)

dev.off()
