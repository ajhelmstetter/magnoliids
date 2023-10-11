rm(list=ls())
library(ggtree)

#set ASTRAL output folder path
af<-"data/trees/astral-mafft_r10_l50_i50_b2_0/"

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
               c(Genus:Species),
               na.rm = TRUE,
               remove = FALSE)

#make unique for duplicates
df$genus_species <- make.unique(df$genus_species)

#remove underscore
df$genus_species <- gsub("_"," ",df$genus_species)

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

#make a data frame containing columns for only the tip label and the corresponding order
df3<-df[,c("genus_species","Order")]

#outgroup species
outgroup <- c("Chloranthus spicatus", "Sarcandra glabra.1","Sarcandra glabra",
  "Ascarina rubricaulis","Ascarina rubricaulis.1","Hedyosmum arborescens")

#reroot
phy <- ape::root(phy, outgroup = outgroup, edgelabel = TRUE, resolve.root = TRUE)

#ggtree::reroot()

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



#colours for plot
#list_cols<-c(brewer.pal(4,"Set2"),'#000000')
#list_cols<-c(wesanderson::wes_palette("FantasticFox1")[2:5],'#000000')
#list_cols<-c(harrypotter::hp(n = 4, option = "LunaLovegood"),'#808080')
list_cols<-c(harrypotter::hp(n = 4, option = "RonWeasley"),'#606060')

#plot tree with orders labelled
p<-ggtree(phy, ladderize = T, right = TRUE) +
  #geom_hilight(node=252, fill="steelblue", alpha=.25) +
  geom_cladelab(node=252, label="Canellales", angle=0, barsize = 1.5, align=TRUE,
                fontsize=5, offset=10, vjust=.15, offset.text=0.75,
                barcolour=list_cols[1], textcolour=list_cols[1]) +
  #geom_hilight(node=261, fill="darkgreen", alpha=.25) +
  geom_cladelab(node=261, label="Piperales", angle=0, barsize = 1.5, align=TRUE,
                fontsize=5, offset=10, vjust=.15, offset.text=0.75,
                barcolour=list_cols[2], textcolour=list_cols[2]) +
  #geom_hilight(node=278, fill="red", alpha=.25)  +
  geom_cladelab(node=278, label="Magnoliales", angle=0, barsize = 1.5, align=TRUE,
                fontsize=5, offset=10, vjust=.15, offset.text=0.5,
                barcolour=list_cols[3], textcolour=list_cols[3]) +
  #geom_hilight(node=405, fill="purple", alpha=.25) +
  geom_cladelab(node=405, label="Laurales", angle=0, barsize = 1.5, align=TRUE,
                fontsize=5, offset=10, vjust=1, offset.text=0.5,
                barcolour=list_cols[4], textcolour=list_cols[4]) +
  # geom_cladelab(node=245, label="Chloranthales", angle=0, barsize = 1.5, align=TRUE,
  #               fontsize=5, offset=10, vjust=1, offset.text=0.5,
  #               barcolour=list_cols[5], textcolour=list_cols[5]) +
  xlim(0,47)

#read in QS phylo
phy2 <- read.tree(paste(af,"astral_bs10_QS.tre",sep=""))
##tip label change
phy2 <- sub.taxa.label(phy2, dfnames)
#reroot
phy2 <- ape::root(phy2, outgroup = outgroup, edgelabel = TRUE, resolve.root = TRUE)
#ladderize
phy2 <- ladderize(phy2,TRUE)
#pies
pie <- data.frame(as.numeric(phy2$node.label), 100 - as.numeric(phy2$node.label),(c(1:(length(phy$tip.label)-1))+length(phy$tip.label)))
colnames(pie)<-c("a","b","node")
pies_qs<-nodepie(pie, cols=1:2, color=c("darkgrey","white"), outline.color = "black", alpha=0.8)
#add pies with quartet support
p2 <- p + geom_inset(pies_qs, width=0.045, height=0.045)

#read in LPP phylo
phy2 <- read.tree(paste(af,"astral_bs10_LPP.tre",sep=""))
##tip label change
phy2 <- sub.taxa.label(phy2, dfnames)
#reroot
phy2 <- ape::root(phy2, outgroup = outgroup, edgelabel = TRUE, resolve.root = TRUE)
#ladderize
phy2 <- ladderize(phy2,TRUE)
#pies
pie <- data.frame(as.numeric(phy2$node.label)*100, 100 - (as.numeric(phy2$node.label)*100),(c(1:(length(phy$tip.label)-1))+length(phy$tip.label)))
colnames(pie)<-c("a","b","node")
pies_lpp<-nodepie(pie, cols=1:2, color=c("black","white"), outline.color = "black", alpha=0.7)
#add pies with LPP support
p3 <- p2 + geom_inset(pies_lpp, width=0.025, height=0.025)

#Colours
cols<-df$Order

cols[grep("Canellales",cols)]<-list_cols[1]
cols[grep("Piperales",cols)]<-list_cols[2]
cols[grep("Magnoliales",cols)]<-list_cols[3]
cols[grep("Laurales",cols)]<-list_cols[4]
cols[grep("Chloranthales",cols)]<-list_cols[5]

pdf("figures/fig3_support_ggtree.pdf",width=10, height=15)

#plot
p3 + geom_tiplab(size=2, color = cols, offset = 0.4,fontface='italic',align=TRUE)


dev.off()

ggsave("figures/fig3_support_ggtree.png",width=10,height=15)
