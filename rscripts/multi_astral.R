rm(list=ls())

#load packages
library(ape)
library(phylotools)
library(RColorBrewer)
library(tidyr)

#set colour palette
palette(colorRampPalette(brewer.pal(name = "Set1", n = 8))(17))

#list of tree folders to be plotted
tree_names<-list.files(path="data/trees/",pattern="astral_r10*")
tree_paths<-list.files(path="data/trees/",pattern="astral_r10*",full.names = T)

#set sample data filepath
sample_data<-"data/sample_data - samples_for_phylo.csv"



for(i in 1:length(tree_paths)){

  #set PDF name
  pdf(paste("figures/",tree_names[i],".pdf",sep=""),width=10,height=20)

  #read in tree and tips
  phy <- read.tree(paste(tree_paths[i],"/astral_bs10_LPP.tre",sep=""))
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

  #root tree for comparison
  phy <- ape::root(phy, "Chloranthaceae_Chloranthus_spicatus_PAFTOL", edgelabel = T, resolve.root = T)

  #custom tip colour
  tip_cols<-as.numeric(as.factor(df$Order))

  #ladderize
  phy <- ladderize(phy)

  #plot tree
  plot(
    phy,
    cex = 0.5,
    label.offset = 0.05,
    align.tip.label = T,
    edge.width = 2,
    edge.color = "grey",
    tip.color = tip_cols
  )

  p <- character(length(phy$node.label))

  phy2 <- read.tree(paste(tree_paths[i],"/astral_bs10_QS.tre",sep=""))
  nodelabels(
    pie = cbind(as.numeric(phy2$node.label), 100 - as.numeric(phy2$node.label)),
    piecol = c("grey", "white"),
    cex = 0.25
  )

  phy2 <- read.tree(paste(tree_paths[i],"/astral_bs10_LPP.tre",sep=""))
  nodelabels(
    pie = cbind(as.numeric(phy2$node.label), 1 - as.numeric(phy2$node.label)),
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

}
