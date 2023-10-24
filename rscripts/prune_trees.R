rm(list=ls())

par(mar=c(5,5,5,5))

#load packages
library(ape)

 #SPECIES TREE
 #list of filepaths to read in
 filepaths <-
   list.files(
     path = "/home/andrew/Dropbox/projects/AJH_magnoliids/DiscoVista-master/mag/species_pruned_trees",
     pattern = "tree.tree$",
     full.names = TRUE,
     recursive = TRUE
   )

# #GENETREES
# #list of filepaths to read in
# filepaths <-
#   list.files(
#     path = "/home/andrew/Dropbox/projects/AJH_magnoliids/DiscoVista-master/mag/genetrees2_trimmed/",
#     pattern = "trees.tree$",
#     full.names = TRUE,
#     recursive = TRUE
#   )


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

