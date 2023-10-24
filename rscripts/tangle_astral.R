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

#Trees to compare
ast_phy <- read.tree("data/trees/astral-magusexons_r10_l50_i50_b2_0/astral_bs10_LPP.tre")
ast_phy2 <- read.tree("data/trees/astral-magusexons_r25_l50_i50_b2_0/astral_bs10_LPP.tre")

pdf_path <- "figures/tangle_astrals/l_i/astral-r10_vs_r25_magus-exons_l50_i50_b2_0.pdf"

#drop tips that arent present in both
ast_phy<-drop.tip(ast_phy,setdiff(ast_phy$tip.label,ast_phy2$tip.label))
ast_phy2<-drop.tip(ast_phy2,setdiff(ast_phy2$tip.label,ast_phy$tip.label))

##
# ASTRAL TREE 1
##

df <- read.csv(sample_data)
df <- df[df$Namelist %in% ast_phy$tip.label,]
df <-
  df %>% unite("genus_species",
               c(Family:Species,Source),
               na.rm = TRUE,
               remove = FALSE)

#reroot
ast_phy <- ape::root(ast_phy, node=251, edgelabel = T, resolve.root = T)

#ladderize tree
ast_phy<-ladderize(ast_phy)

##match order of namelist and tip labels
df<-df[match(ast_phy$tip.label, df$Namelist),]

#diffs
setdiff(ast_phy$tip.label, df$Namelist)
setdiff(df$Namelist, ast_phy$tip.label)

#check matches
df$Namelist==ast_phy$tip.label

##tip label change
dfnames <- data.frame(df$Namelist, df$genus_species)
dfnames$df.genus_species <- make.unique(dfnames$df.genus_species)
ast_phy <- sub.taxa.label(ast_phy, dfnames)

#check number of NaN match number of tips
table(is.na(ast_phy$edge.length))
length(ast_phy$tip.label)

#make tips arbitrary length
ast_phy$edge.length[ast_phy$edge.length == "NaN"] <- 0.25

#cols
df <- df[match(ast_phy$tip.label, df$genus_species),]

#make a data frame containing columns for only the tip label and the corresponding order
df3<-df[,c("genus_species","Order")]

#plot tree with orders labelled
a<-ggtree(ast_phy, ladderize = T)
a


##
# ASTRAL tree 2
##

#read in tree and tips

df <- read.csv(sample_data)
df <- df[df$Namelist %in% ast_phy2$tip.label,]
df <-
  df %>% unite("genus_species",
               c(Family:Species,Source),
               na.rm = TRUE,
               remove = FALSE)

#reroot
ast_phy2 <- ape::root(ast_phy2, node=251, edgelabel = T, resolve.root = T)

#ladderize trees
ast_phy2<-ladderize(ast_phy2)

##match order of namelist and tip labels
df<-df[match(ast_phy2$tip.label, df$Namelist),]

#diffs
setdiff(ast_phy2$tip.label, df$Namelist)
setdiff(df$Namelist, ast_phy2$tip.label)

##tip label change
dfnames <- data.frame(df$Namelist, df$genus_species)
dfnames$df.genus_species <- make.unique(dfnames$df.genus_species)
ast_phy2 <- sub.taxa.label(ast_phy2, dfnames)
ast_phy2$edge.length[ast_phy2$edge.length == "NaN"] <- 0.25

#cols
df <- df[match(ast_phy2$tip.label, df$genus_species),]

#make a data frame containing columns for only the tip label and the corresponding order
df3<-df[,c("genus_species","Order")]

#plot tree with orders labelled
a<-ggtree(ast_phy2, ladderize = T)
a

###
# TANGLEPLOT
###

#Here, we are grabbing the backend data frame from both trees and updating the tree 2 data frame x-coordinate.
#We are using this equation for the update: max(d2$x) - d2$x + max(d1$x) + max(d1$x)*0.3.
#You can toy with different values depending on the branch length unit of your tree
#to get good visualization (I particularly suggest changing max(d1$x)*0.3 terms).


##match order of namelist and tip labels
df3<-df3[match(ast_phy$tip.label, df3$genus_species),]

#check order
df3$genus_species==ast_phy$tip.label

t1 <- ggtree(ast_phy) + geom_tiplab(size=1.25, color = as.numeric(as.factor(df3$Order)))
t2 <- ggtree(ast_phy2)

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

pdf(pdf_path,width=20,height=20)
pp + geom_line(aes(x, y, group=label), color='grey',linetype=2, data=dd1,alpha=0.6)
dev.off()

#test branch lengths
ast_phy$edge.length==ast_phy2$edge.length
plot(ast_phy$edge.length,ast_phy2$edge.length)
