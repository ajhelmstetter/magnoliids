rm(list=ls())

#set ASTRAL output folder path
af<-"data/trees/astral-mafft_r10_l50_i50_b2_0/"

#set sample data filepath
sample_data<-"data/sample_data - samples_for_phylo.csv"

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

#combine genus and species
df <-
  df %>% unite("genus_species",
               c(Genus:Species),
               na.rm = TRUE,
               remove = FALSE)

#UNCOMMENT TO: add family and source to tip name
# df <-
#   df %>% unite("genus_species",
#                c(Family:Species,Source),
#                na.rm = TRUE,
#                remove = FALSE)


#insert arbitrary terminal branch length so internal branch lengths are kept
phy$edge.length[phy$edge.length == "NaN"] <- 0.25

#plot tree with node numbers
#pdf("figures/r10_l50_i50_node_numbers.pdf",width=10,height=20)

plot(
  phy,
  cex = 0.5,
  label.offset = 0.05,
  edge.width = 2,
  edge.color = "grey"
)
nodelabels(cex=0.4)

#dev.off()

#reroot
phy <- ape::root(phy, node=251, edgelabel = T, resolve.root = T)

#ladderize
phy <- ladderize(phy)

#plot tree with node numbers
plot(phy, cex = 0.5, label.offset = 0.05, edge.width = 2, edge.color = "grey")
nodelabels(cex=0.4)

####
# add outgroup, root and then drop outgroup
####

library(phytools)

#add outgroup tip
phy_add<-phy
phy_add<-bind.tip(phy_add,tip.label="outgroup",edge.length = 0.25, where = 250, position = 0.25)

#root on outgroup
phy_add<-root(phy_add,outgroup = "outgroup", edgelabel = T, resolve.root = T)

#drop outgroup
phy<-drop.tip(phy_add,tip = "outgroup")

#plot tree with node numbers
plot(phy, cex = 0.5, label.offset = 0.05, edge.width = 2, edge.color = "grey")
nodelabels(cex=0.4)

#repeat for QS tree
phy_qs_add <- read.tree(paste(af,"astral_bs10_QS.tre",sep=""))
phy_qs_add$edge.length[phy_qs_add$edge.length == "NaN"] <- 0.25

#reroot
phy_qs_add  <- ape::root(phy_qs_add , node=251, edgelabel = T, resolve.root = T)

#ladderize
phy_qs_add  <- ladderize(phy_qs_add )

#add outgroup tip
phy_qs_add<-bind.tip(phy_qs_add,tip.label="outgroup",edge.length = 0.25, where = 250, position = 0.25)

#root on outgroup
phy_qs_add<-root(phy_qs_add,outgroup = "outgroup")

#drop outgroup
phy_qs<-drop.tip(phy_qs_add,tip = "outgroup")

#plot tree with node numbers
plot(phy, cex = 0.5, label.offset = 0.05, edge.width = 2, edge.color = "grey")
nodelabels(cex=0.4)

#tip colours
##match order of namelist and tip labels
df<-df[match(phy$tip.label, df$Namelist),]

#diffs
setdiff(phy$tip.label, df$Namelist)
setdiff(df$Namelist, phy$tip.label)

##tip label change
dfnames <- data.frame(df$Namelist, df$genus_species)
dfnames$df.genus_species <- make.unique(dfnames$df.genus_species)
phy <- sub.taxa.label(phy, dfnames)

#custom tip colour
tip_cols<-as.numeric(as.factor(df$Order))

#plot tree with node numbers
pdf("figures/fig5_support_base.pdf",width=10,height=20)

plot(
  phy,
  cex = 0.4,
  label.offset = 0.1,
  #align.tip.label = T,
  edge.width = 2,
  edge.color = "grey",
  tip.color = tip_cols#,
  #type="fan",
  #x.lim=c(-40,40), #for circular tree
  #y.lim=c(-40,40) #for circular tree
)
p <- character(length(phy$node.label))

nodelabels(
  pie = cbind(as.numeric(phy_qs$node.label), 100 - as.numeric(phy_qs$node.label)),
  piecol = c("grey", "white"),
  cex = 0.25
)

nodelabels(
  pie = cbind(as.numeric(phy$node.label), 1 - as.numeric(phy$node.label)),
  piecol = c("black", "white"),
  cex = 0.125
)

#TO CHECK SUPPORT VALUES ARE PLOTTED CORRECTLY
#nodelabels(as.numeric(phy_qs$node.label), cex = 0.5)

legend(
  'bottomleft',
  legend = c(
    'Canellales',
    'Chloranthales',
    'Laurales',
    'Magnoliales',
    'Piperales',
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

#write tree file
write.tree(phy,"data/trees/astral-mafft_r10_l50_i50_b2_0/astral_bs10_LPP_rerooted_tiplabels.tre")
