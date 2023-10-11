rm(list=ls())

#set ASTRAL output folder path
af<-"data/trees/astral_r10_l50_i50_no_empty/"

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


##match order of namelist and tip labels
df<-df[match(phy$tip.label, df$Namelist),]

#diffs
setdiff(phy$tip.label, df$Namelist)
setdiff(df$Namelist, phy$tip.label)

# Orders
df_ord<-data.frame(df$Namelist,df$Order)
str(df_ord)
write.table(df_ord,"outputs/order_annotation.txt",col.names=FALSE,row.names=FALSE,sep="\t", quote = FALSE)

df_fam<-data.frame(df$Namelist,df$Family)
write.table(df_fam,"outputs/family_annotation.txt",col.names=FALSE,row.names=FALSE,sep="\t", quote = FALSE)

###
# Meiocarpidium
###

# NODE NUMBERS WILL NEED TO BE UPDATED IF NEW TREE USED
# NODE NUMBERS TAKEN FROM r10_l50_i50_node_numbers.pdf

#Ambavioideae
amba_phy<-extract.clade(phy,node=321)
amba_phy$tip.label
df[df$Namelist%in%amba_phy$tip.label,]

#Malmeoideae
malm_phy<-extract.clade(phy,node=328)
malm_phy$tip.label
df[df$Namelist%in%malm_phy$tip.label,]

#Annonoideae
anno_phy<-extract.clade(phy,node=366)
anno_phy$tip.label
df[df$Namelist%in%anno_phy$tip.label,]

#Anaxagoreoideae
anax_phy<-extract.clade(phy,node=318)
anax_phy$tip.label
df[df$Namelist%in%anax_phy$tip.label,]

#Eupomatia
eupo_phy<-extract.clade(phy,node=316)
eupo_phy$tip.label
df[df$Namelist%in%eupo_phy$tip.label,]

#Eupomatia
eupo_phy<-extract.clade(phy,node=316)
eupo_phy$tip.label
df[df$Namelist%in%eupo_phy$tip.label,]

#Magnoliaceae, Degeneriaceae, Himantandraceae
magn_phy<-extract.clade(phy,node=306)
magn_phy$tip.label
df[df$Namelist%in%magn_phy$tip.label,]

#Myristicaceae
myri_phy<-extract.clade(phy,node=280)
myri_phy$tip.label
df[df$Namelist%in%myri_phy$tip.label,]


#Start with orders
df_meio<-data.frame(df$Namelist,df$Order)
colnames(df_meio)<-c("Namelist","Group")

#replace orders with groups of interest
df_meio$Group[df_meio$Namelist%in%anax_phy$tip.label]<-"Anaxagoreoideae"
df_meio$Group[df_meio$Namelist%in%anno_phy$tip.label]<-"Annonoideae"
df_meio$Group[df_meio$Namelist%in%malm_phy$tip.label]<-"Malmeoideae"
df_meio$Group[df_meio$Namelist%in%amba_phy$tip.label]<-"Ambavioideae"
df_meio$Group[df_meio$Namelist%in%eupo_phy$tip.label]<-"Eupomatia"
df_meio$Group[df_meio$Namelist%in%magn_phy$tip.label]<-"Magn_Degen_Himan"
df_meio$Group[df_meio$Namelist%in%myri_phy$tip.label]<-"Myristicaceae"
df_meio$Group[df_meio$Namelist%in%"P_012861"]<-"Meiocarpidium"


write.table(df_meio,"outputs/meio_annotation.txt",col.names=FALSE,row.names=FALSE,sep="\t", quote = FALSE)


###
# Hydnoraceae
###

#Start with orders
df_hydn<-data.frame(df$Namelist,df$Order)
colnames(df_hydn)<-c("Namelist","Group")

#replace orders with groups of interest
df_hydn$Group[df_hydn$Group%in%"Piperales"]<-df[df$Order=="Piperales",]$Genus

write.table(df_hydn,"outputs/hydn_annotation.txt",col.names=FALSE,row.names=FALSE,sep="\t", quote = FALSE)

