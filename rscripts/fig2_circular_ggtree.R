rm(list=ls())
library(ggtree)

#set ASTRAL output folder path
af<-"data/trees/astral_r10_l50_i50_auto_b2_0/"

#set sample data filepath
sample_data<-"data/sample_data - samples_for_phylo.csv"

#set PDF name
#pdf("figures/astral_r50_l50_i50_auto_b2_d.pdf",width=10,height=20)
#pdf("figures/astral_r10_l50_i50_circular.pdf",width=15,height=15)
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
dfnames$df.genus_species <- make.unique(dfnames$df.genus_species)
phy <- sub.taxa.label(phy, dfnames)
phy$edge.length[phy$edge.length == "NaN"] <- 0.25

#cols
df <- df[match(phy$tip.label, df$genus_species),]

#make a data frame containing columns for only the tip label and the corresponding order
df3<-df[,c("genus_species","Order")]

#reroot
phy <- ape::root(phy, node=251, edgelabel = T, resolve.root = T)

#ladderize
phy <- ladderize(phy)

pdf("figures/r10_l50_i50_node_numbers.pdf",width=10,height=20)
plot(
  phy,
  cex = 0.5,
  label.offset = 0.05,
  edge.width = 2,
  edge.color = "grey"
)
nodelabels(cex=0.4)
dev.off()

pdf("figures/fig2_ggtree_circular.pdf",width=15, height=15)

#plot tree with orders labelled
p<-ggtree(phy, layout="circular",ladderize = T) +
  geom_hilight(node=252, fill="steelblue", alpha=.25) +
  geom_cladelab(node=252, label="Canellales", angle=0,
                fontsize=5, offset=.1, vjust=.15, offset.text=0.75) +
  geom_hilight(node=261, fill="darkgreen", alpha=.25) +
  geom_cladelab(node=261, label="Piperales", angle=0,
                fontsize=5, offset=.1, vjust=.15, offset.text=0.75) +
  geom_hilight(node=278, fill="red", alpha=.25)  +
  geom_cladelab(node=278, label="Laurales", angle=0,
                fontsize=5, offset=.1, vjust=.15, offset.text=2) +
  geom_hilight(node=405, fill="purple", alpha=.25) +
  geom_cladelab(node=405, label="Magnoliales", angle=0,
                fontsize=5, offset=0.01, vjust=1, offset.text=2)

p

dev.off()


