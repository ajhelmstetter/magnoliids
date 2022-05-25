rm(list=ls())

#set ASTRAL output folder path
af<-"data/trees/astral_r10_l50_i50_exons_b2_0/"

#set sample data filepath
sample_data<-"data/sample_data - samples_for_phylo.csv"

#set PDF name
pdf("figures/astral_r10_l50_i50_exons_b2_0.pdf",width=10,height=20)
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

#plot tree QS
#phy <- ape::root(phy, "Chloranthaceae_Chloranthus_spicatus_PAFTOL", edgelabel = T, resolve.root = T)

###
# Rooting script from Herve
###

#Add an acrogymnosperm branch to this poor, rootless tree
#Note: this code is the best I could come up with to do this programmatically with ape functions (the alternative would be to edit the tree file directly or programmatically)
#(inspired from the bind.tip() function of phytools)
#FBDtree_root.time <- node.depth.edgelength(phy)[1] #root-to-tip distance (same for all extant tips)
#acrotime <- 1 #all 3 MCC trees from Ramirez-Barahona et al. (2020), CC, RC, and UC, have crown seed plants at ca. 374 Ma
#acrotip <- list(edge=matrix(c(4,4,5,5,1,5,2,3),4,2),
#                tip.label=c("prunetip1","prunetip2","acrogymnosperms"),
#                edge.length=c(0.5, acrotime-FBDtree_root.time, 0.75, acrotime),
#                Nnode=2)
#class(acrotip) <- "phylo"
#phy <- bind.tree(phy, acrotip)
#phy <- drop.tip(phy,"prunetip1")
#phy <- root(phy, outgroup = "acrogymnosperms")
#phy <- drop.tip(phy, "prunetip2")
#phy <- ladderize(phy, FALSE)

#custom tip colour
tip_cols<-as.numeric(as.factor(df$Order))
#tip_cols[grepl("Glossocalyx",df$Genus)]<-"black"


phy <- ladderize(phy)
plot(
  phy,
  cex = 0.5,
  label.offset = 0.05,
  align.tip.label = T,
  edge.width = 2,
  edge.color = "grey",
  tip.color = tip_cols#,
  #type="fan",
  #x.lim=c(-40,40), #for circular tree
  #y.lim=c(-40,40) #for circular tree
)
p <- character(length(phy$node.label))

phy2 <- read.tree(paste(af,"astral_bs10_QS.tre",sep=""))
nodelabels(
  pie = cbind(as.numeric(phy2$node.label), 100 - as.numeric(phy2$node.label)),
  piecol = c("grey", "white"),
  cex = 0.25
)

phy2 <- read.tree(paste(af,"astral_bs10_LPP.tre",sep=""))
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

#write labelled tree
write.tree(phy,"outputs/astral_r10_l50_i50_exons_b2_0_tiplabels.tree")

#prop bootstrap > 90 (-2 for NA)
table(as.numeric(phy2$node.label)>0.9)[2]/(length(phy2$node.label)-2)

