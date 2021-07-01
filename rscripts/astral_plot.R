rm(list=ls())

#set ASTRAL output folder path
af<-"data/trees/astral_r10_l50_i50/"

#set sample data filepath
sample_data<-"data/sample_data - samples_for_phylo_AZ.csv"

#set PDF name
pdf("figures/astral_r10_l50_i50_AZ.pdf",width=10,height=20)

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

#plot tree QS
#phy <- ape::root(phy, "Chloranthaceae_Chloranthus_spicatus_PAFTOL", edgelabel = T, resolve.root = T)
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

phy <- read.tree(paste(af,"astral_bs10_QS.tre",sep=""))
nodelabels(
  pie = cbind(as.numeric(phy$node.label), 100 - as.numeric(phy$node.label)),
  piecol = c("grey", "white"),
  cex = 0.25
)

phy <- read.tree(paste(af,"astral_bs10_LPP.tre",sep=""))
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
  text.col = c(1, 2, 3, 4, 5, "black", "black"),
  pt.cex = c(NA, NA, NA, NA, NA, 2, 2),
  pch = c(NA, NA, NA, NA, NA, 16, 16),
  col = c(NA, NA, NA, NA, NA, "black", "grey"),
  bty = 'n'
)

dev.off()
