rm(list=ls())

#set ASTRAL output folder path
iqf<-"data/trees/iqtree-magus_r10_l50_i50_trimal_auto//"

#set sample data filepath
sample_data<-"data/sample_data - samples_for_phylo.csv"

#output filename
output_file<-"figures/support_iqtree_2249007.png"


####
# IQTREE
####

par(mar = c(0, 0, 0, 0))

library(ape)
library(phylotools)
library(RColorBrewer)
library(tidyr)

cols<-c(harrypotter::hp(n = 4, option = "RonWeasley"),'#606060')

palette(cols[c(1,5,4,3,2)])

#NOTE: May need to be rerooted in FigTree
#phy <- read.nexus(paste(iqf,"mag.partitions.contree",sep=""))

#read in tree and tips
phy <- read.tree(paste(iqf,"mag.partitions.contree",sep=""))
df <- read.csv(sample_data)
df <- df[df$Namelist %in% phy$tip.label,]
df <-
  df %>% unite("genus_species",
               c(Genus:Species), #c(Family:Species,Source) to include family and source as well
               na.rm = TRUE,
               remove = FALSE)

head(df)

#make unique for duplicates
df$genus_species <- make.unique(df$genus_species)

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

#plot tree with node numbers
pdf("figures/iqtree_node_numbers.pdf",width=10,height=20)
plot(phy,cex = 0.5,label.offset = 0.005,edge.width = 2,edge.color = "grey")
nodelabels(cex=0.4)
dev.off()

#reroot
#outgroup species
outgroup <- c("Chloranthus_spicatus", "Sarcandra_glabra.1","Sarcandra_glabra",
              "Ascarina_rubricaulis","Ascarina_rubricaulis.1","Hedyosmum_arborescens")

#reroot
phy <- ape::root(phy, outgroup = outgroup, edgelabel = TRUE, resolve.root = TRUE)

#colour tip labels by order
tip_cols<-as.numeric(as.factor(df$Order))

#UNCOMMENT FOR: custom tip colour for newly added tips
#tip_cols[grepl("Glossocalyx",df$Genus)]<-"black"


#set PDF/PNG name
#pdf("figures/figSX_support_iqtree.pdf",width=15,height=30)
png(output_file,width=3000,height=4500,res=200)

phy <- ladderize(phy)
plot(
  phy,
  cex = 0.75,
#  label.offset = 0.005,
  align.tip.label = F,
  edge.width = 2,
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
  cex = 0.3
)

add.scale.bar(0.05, 1, length=0.1,lwd = 3, lcol = "grey")

legend(
  x=0.025,
  y=25,
  legend = c(
    'Chloranthales',
    'Canellales',
    'Piperales',
    'Laurales',
    'Magnoliales',
    'Bootstrap support'
  ),
  text.col = c(cols[5],cols[1], cols[2], cols[4], cols[3],"black"),
  pt.cex = c(NA, NA, NA, NA, NA, 2),
  pch = c(NA, NA, NA, NA, NA, 16),
  col = c(NA, NA, NA, NA, NA, "black"),
  bty = 'n'
)



dev.off()


#UNCOMMENT TO: write labelled tree
#write.tree(phy,"outputs/iqtree_r10_l50_i50_tiplabels.tree")

#prop bootstrap > 90 (-2 for NA)
table(as.numeric(phy$node.label)>90)[2]/(length(phy$node.label)-2)
