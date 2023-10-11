# tutorial:
# https://arftrhmn.net/how-to-make-cophylogeny/

rm(list=ls())
library(ggtree)
library(ape)
library(phylotools)
library(RColorBrewer)
library(tidyr)
library(ggplot2)
library(phangorn)
library(dplyr)

#set sample data filepath
sample_data<-"data/sample_data - samples_for_phylo.csv"

##
# IQTREE
##

#read in tree and tips
iq_phy <- read.tree(paste("data/trees/iqtree_magnoliids_r10_auto_b2_0_l50_i50_1212013/mag.partitions.contree",sep=""))
df <- read.csv(sample_data)
df <- df[df$Namelist %in% iq_phy$tip.label,]
df <-
  df %>% unite("genus_species",
               c(Family:Species,Source),
               na.rm = TRUE,
               remove = FALSE)

head(df)

##match order of namelist and tip labels
df<-df[match(iq_phy$tip.label, df$Namelist),]

#diffs
setdiff(iq_phy$tip.label, df$Namelist)
setdiff(df$Namelist, iq_phy$tip.label)

##tip label change
dfnames <- data.frame(df$Namelist, df$genus_species)
dfnames$df.genus_species <- make.unique(dfnames$df.genus_species)
iq_phy <- sub.taxa.label(iq_phy, dfnames)

#cols
df <- df[match(iq_phy$tip.label, df$genus_species),]

#plot IQTREE tree with node numbers
pdf("figures/iqtree_r10_l50_i50_node_numbers.pdf",width=10,height=20)
plot(
  iq_phy,
  cex = 0.5,
  label.offset = 0.05,
  edge.width = 2,
  edge.color = "grey"
)
nodelabels(cex=0.4)
dev.off()

#root tree
iq_phy <- ape::root(iq_phy, node=288, edgelabel = T, resolve.root = T)

#rescale IQTREE branches
iq_phy$edge.length <- iq_phy$edge.length*30

#plot tree with orders labelled
i<-ggtree(iq_phy, ladderize = T)
i


##
# ASTRAL
##

#read in tree and tips
ast_phy <- read.tree("data/trees/astral_r10_l50_i50_auto_b2_0/astral_bs10_LPP.tre")
df <- read.csv(sample_data)
df <- df[df$Namelist %in% ast_phy$tip.label,]
df <-
  df %>% unite("genus_species",
               c(Family:Species,Source),
               na.rm = TRUE,
               remove = FALSE)

##match order of namelist and tip labels
df<-df[match(ast_phy$tip.label, df$Namelist),]

#diffs
setdiff(ast_phy$tip.label, df$Namelist)
setdiff(df$Namelist, ast_phy$tip.label)

##tip label change
dfnames <- data.frame(df$Namelist, df$genus_species)
dfnames$df.genus_species <- make.unique(dfnames$df.genus_species)
ast_phy <- sub.taxa.label(ast_phy, dfnames)
ast_phy$edge.length[ast_phy$edge.length == "NaN"] <- 0.25

#cols
df <- df[match(ast_phy$tip.label, df$genus_species),]

#make a data frame containing columns for only the tip label and the corresponding order
df3<-df[,c("genus_species","Order")]

#reroot
ast_phy <- ape::root(ast_phy, node=251, edgelabel = T, resolve.root = T)

#plot tree with orders labelled
a<-ggtree(ast_phy, ladderize = T)
a

###
# TANGLEPLOT
###

#Here, we are grabbing the backend data frame from both trees and updating the tree 2 data frame x-coordinate.
#We are using this equation for the update: max(d2$x) - d2$x + max(d1$x) + max(d1$x)*0.3.
#You can toy with different values depending on the branch length unit of your tree
#to get good visualization (I particularly suggest changing max(d1$x)*0.3 terms).

t1 <- ggtree(ast_phy) + geom_tiplab(size=1, color = as.numeric(as.factor(df$Order)))
t2 <- ggtree(iq_phy)

d1 <- t1$data

#add some space for tip label in ASTRAL phylo
d1$x<-d1$x+3.5

d2 <- t2$data

d1$tree <-'t1'
d2$tree <-'t2'

d2$x <- max(d2$x) - d2$x + max(d1$x) +  max(d1$x)*0.3
pp <- t1 + geom_tree(data=d2)
pp

#Let’s join d1 and d2 for dataset so that we can use the coordinates of the tips for
#making connections between both of the trees.

dd <- bind_rows(d1, d2) %>%
  filter(isTip == TRUE)
dd1 <- as.data.frame(dd)

#Now, we are going to conditionally join the tips of both trees for the feature we are interested in.
# Connected tips will represent the same species.

pdf("figures/figSX_tangle.pdf",width=20,height=20)
pp + geom_line(aes(x, y, group=label), color='grey',linetype=2, data=dd1)
dev.off()
