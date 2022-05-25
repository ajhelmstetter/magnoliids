rm(list=ls())

#set ASTRAL output folder path
iqf<-"data/trees/iqtree_r10_l50_i50/"

#set sample data filepath
sample_data<-"data/sample_data - samples_for_phylo.csv"

#set PDF name
pdf("figures/iqtree_r10_l50_i50.pdf",width=15,height=30)

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
phy <- read.tree(paste(iqf,"mag.partitions.contree",sep=""))
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
dfnames$df.genus_species <- make.unique(dfnames$df.genus_species)
phy <- sub.taxa.label(phy, dfnames)

#cols
df <- df[match(phy$tip.label, df$genus_species),]

#plot tree QS
phy <- ape::root(phy, "Chloranthaceae_Chloranthus_spicatus_PAFTOL", edgelabel = T, resolve.root = T)

#rooting
#plot(phy,show.tip.label = F)
#nodelabels()
#phy2 <- ape::root(phy,node=239,resolve.root=T,edgelabel=T)
#plot(ladderize(phy2),show.tip.label = F)

#custom tip colour
tip_cols<-as.numeric(as.factor(df$Order))
tip_cols[grepl("Glossocalyx",df$Genus)]<-"black"
tip_cols[grepl("Chimonanthus",df$Genus)]<-"black"
tip_cols[grepl("Xymalos",df$Genus)]<-"black"
tip_cols[grepl("Hennecartia",df$Genus)]<-"black"
tip_cols[grepl("Ephippiandra",df$Genus)]<-"black"
tip_cols[grepl("Canella",df$Genus)]<-"black"
tip_cols[grepl("Peumus",df$Genus)]<-"black"
tip_cols[grepl("maudiae",df$Species)]<-"black"
tip_cols[grepl("bennettii",df$Species)]<-"black"
tip_cols[grepl("Mezilaurus",df$Genus)]<-"black"
tip_cols[grepl("Asarum",df$Genus)]<-"black"
tip_cols[grepl("Alseodaphnopsis",df$Genus)]<-"black"
tip_cols[grepl("Dehaasia",df$Genus)]<-"black"
tip_cols[grepl("Machilus",df$Genus)]<-"black"
tip_cols[grepl("Pseudowintera",df$Genus)]<-"black"
tip_cols[grepl("Warburgia",df$Genus)]<-"black"


phy <- ladderize(phy)
plot(
  phy,
  cex = 0.75,
#  label.offset = 0.005,
  align.tip.label = F,
  edge.width = 3,
  edge.color = "grey",
  tip.color = tip_cols#,
  #type="fan"
  #x.lim=c(-0.5,0.7), #for circular tree
  #y.lim=c(-0.7,0.5) #for circular tree
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

#write labelled tree
write.tree(phy,"outputs/iqtree_r10_l50_i50_tiplabels.tree")

#prop bootstrap > 90 (-2 for NA)
table(as.numeric(phy$node.label)>90)[2]/(length(phy$node.label)-2)
