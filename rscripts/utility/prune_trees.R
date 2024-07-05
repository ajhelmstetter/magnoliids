rm(list=ls())

par(mar=c(5,5,5,5))

#load packages
library(ape)

 #SPECIES TREE
 #list of filepaths to read in
 filepaths <-
   list.files(
     path = "/home/andrew.helmstetter/Dropbox/projects/AJH_magnoliids/DiscoVista-master/mag/species_pruned_trees",
     pattern = "tree.tree$",
     full.names = TRUE,
     recursive = TRUE
   )

#GENETREES
#list of filepaths to read in
filepaths <-
  list.files(
    path = "/home/andrew.helmstetter/Dropbox/projects/AJH_magnoliids/DiscoVista-master/mag/genetrees_pruned/",
    pattern = "trees.tree$",
    full.names = TRUE,
    recursive = TRUE
  )


filepaths

for(i in 1:length(filepaths)){

  #read in tree
  phy<-read.tree(filepaths[[i]])

  #drop tip
  phy<-drop.tip(phy,c("P_011589", #Yasunia	sessiliflora
                      "P_011607", #Pleurothyrium	trianae
                      "P_011709", #Chlorocardium	venenosum
                      "P_011719", #Apollonias	arnottii
                      "P_011701"))#Potameia	thouarsii

  write.tree(phy, file = filepaths[[i]])

}


####
# ---- Change IQTREE support to 0-1 to match ASTRAL for DiscoVista species tree analysis
####

#read in tree
iq1<-read.tree("/home/andrew.helmstetter/Dropbox/projects/AJH_magnoliids/DiscoVista-master/mag/species_pruned_trees/iqtree-mafft_r10_l50_i50_b2_0/estimated_species_tree.tree")
iq2<-read.tree("/home/andrew.helmstetter/Dropbox/projects/AJH_magnoliids/DiscoVista-master/mag/species_pruned_trees/iqtree-magus_r10_l50_i50_trimal_auto/estimated_species_tree.tree")

#change node label
iq1$node.label <- as.numeric(iq1$node.label)/100
iq2$node.label <- as.numeric(iq2$node.label)/100

#fix root
iq1$node.label[1] <- ""
iq2$node.label[1] <- ""

#write tree
write.tree(iq1,"/home/andrew.helmstetter/Dropbox/projects/AJH_magnoliids/DiscoVista-master/mag/species_pruned_trees/iqtree-mafft_r10_l50_i50_b2_0/estimated_species_tree.tree")
write.tree(iq2,"/home/andrew.helmstetter/Dropbox/projects/AJH_magnoliids/DiscoVista-master/mag/species_pruned_trees/iqtree-magus_r10_l50_i50_trimal_auto/estimated_species_tree.tree")

