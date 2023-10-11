library(ape)

#TREE1
phy <- read.tree("data/trees/iqtree-mafft_r10_l50_i50_b2_0/mag.partitions.contree")

phy$node.label

phy$node.label <- as.character ( as.numeric(phy$node.label) / 100 )

phy$node.label[is.na(phy$node.label)] <- ""

phy<-drop.tip(phy,c("P_011589", #Yasunia	sessiliflora
                    "P_011607", #Pleurothyrium	trianae
                    "P_011709", #Chlorocardium	venenosum
                    "P_011719", #Apollonias	arnottii
                    "P_011701"))#Potameia	thouarsii


write.tree(phy,"~/Dropbox/projects/AJH_magnoliids/DiscoVista-master/mag/species_pruned_trees/iqtree-mafft_r10_l50_i50_b2_0/estimated_species_tree.tree")

#TREE2
phy <- read.tree("data/trees/iqtree-magus_r10_l50_i50_trimal_auto/mag.partitions.contree")

phy$node.label

phy$node.label <- as.character ( as.numeric(phy$node.label) / 100 )

phy$node.label[is.na(phy$node.label)] <- ""

phy<-drop.tip(phy,c("P_011589", #Yasunia	sessiliflora
                    "P_011607", #Pleurothyrium	trianae
                    "P_011709", #Chlorocardium	venenosum
                    "P_011719", #Apollonias	arnottii
                    "P_011701"))#Potameia	thouarsii


write.tree(phy,"~/Dropbox/projects/AJH_magnoliids/DiscoVista-master/mag/species_pruned_trees/iqtree-magus_r10_l50_i50_trimal_auto/estimated_species_tree.tree")
