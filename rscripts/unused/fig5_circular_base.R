rm(list=ls())

#set ASTRAL output folder path
af<-"data/trees/astral_r10_l50_i50_auto_b2_0/"

#set sample data filepath
sample_data<-"data/sample_data - samples_for_phylo.csv"

#set margins
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


#add family and source to tip name
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


#plot tree with node numbers
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

#reroot
phy <- ape::root(phy, node=250, edgelabel = T, resolve.root = T)

#ladderize
phy <- ladderize(phy)

#make a data frame containing columns for only the tip label and the corresponding order
df3<-df[,c("genus_species","Order")]

#edges for orders
can <- which.edge(phy, df[df$Order=="Canellales",]$genus_species)
chl <- which.edge(phy, df[df$Order=="Chloranthales",]$genus_species)
lau <- which.edge(phy, df[df$Order=="Laurales",]$genus_species)
mag <- which.edge(phy, df[df$Order=="Magnoliales",]$genus_species)
pip <- which.edge(phy, df[df$Order=="Piperales",]$genus_species)

#number of edges
ec <- rep("grey", Nedge(phy))
ec[can] <- 1
ec[chl] <- 2
ec[lau] <- 3
ec[mag] <- 4
ec[pip] <- 5

#plot figure 1
#tree with edges coloured by order and LPP/Quartet support
pdf("figures/fig2_circular_base.pdf",height = 10,width = 10)

plot(
  phy,
  cex = 0.5,
  label.offset = 0.05,
  #align.tip.label = T,
  show.tip.label = F,
  edge.width = 2,
  edge.color = ec,
  type="fan",
  x.lim=c(-17.5,32.5), #for circular tree
  y.lim=c(-15,30) #for circular tree
)


#UNCOMMENT TO ADD SUPPORT PIE CHARTS
#p <- character(length(phy$node.label))
#
#phy2 <- read.tree(paste(af,"astral_bs10_QS.tre",sep=""))
#nodelabels(
#  pie = cbind(as.numeric(phy2$node.label), 100 - as.numeric(phy2$node.label)),
#  piecol = c("grey", "white"),
#  cex = 0.25
#)

#phy2 <- read.tree(paste(af,"astral_bs10_LPP.tre",sep=""))
#nodelabels(
#  pie = cbind(as.numeric(phy2$node.label), 1 - as.numeric(phy2$node.label)),
#  piecol = c("black", "white"),
#  cex = 0.125
#)

par(cex = 0.75)

legend(
  'bottomleft',
  legend = c(
    'Canellales',
    'Chloranthales',
    'Laurales',
    'Magnoliales',
    'Piperales'
  ),
  text.col = c(1, 2, 3, 4, 5),
  pt.cex = c(1, 1, 1, 1, 1),
  lty = c(1, 1, 1, 1, 1),
  pch = c(NA, NA, NA, NA, NA),
  col = c(1, 2, 3, 4, 5),
  bty = 'n'
)

dev.off()



