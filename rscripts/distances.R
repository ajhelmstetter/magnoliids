rm(list = ls())

library(ape)
library(phylotools)
library(RColorBrewer)
library(tidyr)
library(Quartet)
library(phytools)

#list of filepaths to read in
filepaths <-
  list.files(
    path = paste(here::here(),"/data/trees",sep=""),
    pattern = "*LPP.tre",
    full.names = TRUE,
    recursive = TRUE
  )

filenames <-
  list.files(
    path = paste(here::here(),"/data/trees",sep=""),
    pattern = "*LPP.tre",
    full.names = FALSE,
    recursive = TRUE
  )

filenames <- gsub("/astral_bs10_LPP.tre", "", filenames)

tree_list <- list()

for(i in 1:length(filepaths)){
  tree_list[[i]]<-read.tree(filepaths[i])

}

names(tree_list)<-filenames


status <- ManyToManyQuartetAgreement(tree_list, nTip = TRUE)
df<-as.data.frame(QuartetDivergence(status, similarity = FALSE))

library(tidyverse)

df2 <- df %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
head(df2)

hist(df2$value)

ggplot(df2, aes(x = rowname, y = colname, fill = value)) +
  geom_tile() + theme(axis.text.x = element_text(angle=90,vjust = 0.5,hjust = 1)) + scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"))

ggsave("figures/heatmap_quartet_distance.png",height=15,width=15)


###
# ---- Distance between aligners ----
###

statuses <- QuartetStatus(tree_list[["astral-magus_r10_l50_i50_b2_0"]], tree_list[["astral-mafft_r10_l50_i50_b2_0"]])
QuartetDivergence(statuses, similarity = FALSE)
SimilarityMetrics(statuses, similarity = FALSE)
SimilarityMetrics(SplitStatus(tree_list[["astral-magus_r10_l50_i50_b2_0"]], tree_list[["astral-mafft_r10_l50_i50_b2_0"]]))
VisualizeQuartets(tree_list[["astral-magus_r10_l50_i50_b2_0"]], tree_list[["astral-mafft_r10_l50_i50_b2_0"]])

###
# ---- Distance between r values ----
###

statuses <- QuartetStatus(tree_list[["astral-mafft_r10_l50_i50_b2_0"]], tree_list[["astral-mafft_r25_l50_i50_b2_0"]], nTip = TRUE)
QuartetDivergence(statuses, similarity = FALSE)

statuses <- QuartetStatus(tree_list[["astral-mafft_r10_l50_i50_b2_0"]], tree_list[["astral-mafft_r50_l50_i50_b2_0"]], nTip = TRUE)
QuartetDivergence(statuses, similarity = FALSE)

###
# ---- Distance between gblocks algorithms ----
###

statuses <- QuartetStatus(tree_list[["astral-mafft_r10_l50_i50_b2_0"]], tree_list[["astral-mafft_r10_l50_i50_b2_d"]])
QuartetDivergence(statuses, similarity = FALSE)

statuses <- QuartetStatus(tree_list[["astral-magus_r10_l50_i50_b2_0"]], tree_list[["astral-magus_r10_l50_i50_b2_d"]])
QuartetDivergence(statuses, similarity = FALSE)

###
# ---- Distance between trimal algorithms ----
###

statuses <- QuartetStatus(tree_list[["astral-magus_r10_l50_i50_trimal_auto"]], tree_list[["astral-magus_r10_l50_i50_trimal_02"]])
QuartetDivergence(statuses, similarity = FALSE)

statuses <- QuartetStatus(tree_list[["astral-magus_r10_l50_i50_trimal_auto"]], tree_list[["astral-magus_r10_l50_i50_trimal_gappyout"]])
QuartetDivergence(statuses, similarity = FALSE)

statuses <- QuartetStatus(tree_list[["astral-magus_r10_l50_i50_trimal_02"]], tree_list[["astral-magus_r10_l50_i50_trimal_gappyout"]])
QuartetDivergence(statuses, similarity = FALSE)

###
# ---- Distance between % individual filters ----
###

statuses <- QuartetStatus(tree_list[["astral-mafft_r10_l25_i25_b2_0"]], tree_list[["astral-mafft_r10_l50_i25_b2_0"]])
QuartetDivergence(statuses, similarity = FALSE)

statuses <- QuartetStatus(tree_list[["astral-mafft_r10_l50_i25_b2_0"]], tree_list[["astral-mafft_r10_l75_i25_b2_0"]])
QuartetDivergence(statuses, similarity = FALSE)

statuses <- QuartetStatus(tree_list[["astral-mafft_r10_l25_i25_b2_0"]], tree_list[["astral-mafft_r10_l75_i25_b2_0"]])
QuartetDivergence(statuses, similarity = FALSE)

###
# ---- Distance between exon length filters ----
###

statuses <- QuartetStatus(tree_list[["astral-mafft_r10_l25_i25_b2_0"]], tree_list[["astral-mafft_r10_l25_i50_b2_0"]])
QuartetDivergence(statuses, similarity = FALSE)

statuses <- QuartetStatus(tree_list[["astral-mafft_r10_l25_i50_b2_0"]], tree_list[["astral-mafft_r10_l25_i75_b2_0"]])
QuartetDivergence(statuses, similarity = FALSE)

statuses <- QuartetStatus(tree_list[["astral-mafft_r10_l25_i25_b2_0"]], tree_list[["astral-mafft_r10_l25_i75_b2_0"]])
QuartetDivergence(statuses, similarity = FALSE)

###
# ---- Distance between exons and supercontigs ----
###

statuses <- QuartetStatus(tree_list[["astral-mafftexons_r10_l50_i50_b2_0"]], tree_list[["astral-mafft_r10_l50_i50_b2_0"]])
QuartetDivergence(statuses, similarity = FALSE)

statuses <- QuartetStatus(tree_list[["astral-magus_r10_l50_i50_b2_0"]], tree_list[["astral-magusexons_r10_l50_i50_b2_0"]])
QuartetDivergence(statuses, similarity = FALSE)


#https://ms609.github.io/TreeDist/articles/treespace.html
library("TreeDist")
distances_ci <- ClusteringInfoDistance(tree_list)
distances_rf <- RobinsonFoulds(tree_list)
distances_pi <- PhylogeneticInfoDistance(tree_list)
distances_qs <- as.dist(Quartet::QuartetDivergence(
  Quartet::ManyToManyQuartetAgreement(tree_list, nTip = TRUE), similarity = FALSE))

spectrum <- hcl.colors(length(tree_list), "plasma")
treeCols <- spectrum[1:length(tree_list)]

#r
analyses <-
  data.frame(t(data.frame(strsplit(
    names(tree_list), "_"
  ))))

colnames(analyses) <- c("aligner",
                        "r",
                        "len",
                        "ind",
                        "trim1",
                        "trim2")

analyses$trim <- paste(analyses$trim1, analyses$trim2)

#remove columns not used in scoring
analyses <- analyses[, !names(analyses) %in% c("trim1", "trim2")]

#CHECK ORDER
rownames(analyses) <- filenames

plot(distances_ci,distances_rf)
plot(distances_ci,distances_pi)
plot(distances_ci,distances_qs)
plot(distances_rf,distances_qs)
plot(distances_pi,distances_qs)
plot(distances_pi,distances_rf)


### ---- Heatmap QS ----

dist_mat<-as.matrix(distances_qs)

rownames(dist_mat)<-filenames
colnames(dist_mat)<-filenames

melted_dist_mat <- reshape2::melt(dist_mat)


#reorder levels
melted_dist_mat$Var1<-factor(melted_dist_mat$Var1,levels = c("astral-mafft_r10_l10_i10_b2_0",
  "astral-mafft_r10_l25_i25_b2_0",
  "astral-mafft_r10_l25_i50_b2_0",
  "astral-mafft_r10_l25_i75_b2_0",
  "astral-mafft_r10_l50_i25_b2_0",
  "astral-mafft_r10_l50_i50_b2_0",
  "astral-mafft_r10_l50_i50_b2_d",
  "astral-mafft_r10_l75_i25_b2_0",
  "astral-mafft_r10_l75_i75_b2_0",
  "astral-mafft_r25_l50_i50_b2_0",
  "astral-mafft_r50_l50_i50_b2_0",
  "astral-magus_r10_l50_i50_b2_0",
  "astral-magus_r10_l50_i50_b2_d",
  "astral-magus_r10_l50_i50_trimal_auto",
  "astral-magus_r10_l50_i50_trimal_02",
  "astral-magus_r10_l50_i50_trimal_gappyout",
  "astral-mafftexons_r10_l50_i50_b2_0",
  "astral-magusexons_r10_l50_i50_b2_0",
  "astral-magusexons_r25_l50_i50_b2_0"
))

melted_dist_mat$Var2<-factor(melted_dist_mat$Var2,levels = c("astral-mafft_r10_l10_i10_b2_0",
                                                             "astral-mafft_r10_l25_i25_b2_0",
                                                             "astral-mafft_r10_l25_i50_b2_0",
                                                             "astral-mafft_r10_l25_i75_b2_0",
                                                             "astral-mafft_r10_l50_i25_b2_0",
                                                             "astral-mafft_r10_l50_i50_b2_0",
                                                             "astral-mafft_r10_l50_i50_b2_d",
                                                             "astral-mafft_r10_l75_i25_b2_0",
                                                             "astral-mafft_r10_l75_i75_b2_0",
                                                             "astral-mafft_r25_l50_i50_b2_0",
                                                             "astral-mafft_r50_l50_i50_b2_0",
                                                             "astral-magus_r10_l50_i50_b2_0",
                                                             "astral-magus_r10_l50_i50_b2_d",
                                                             "astral-magus_r10_l50_i50_trimal_auto",
                                                             "astral-magus_r10_l50_i50_trimal_02",
                                                             "astral-magus_r10_l50_i50_trimal_gappyout",
                                                             "astral-mafftexons_r10_l50_i50_b2_0",
                                                             "astral-magusexons_r10_l50_i50_b2_0",
                                                             "astral-magusexons_r25_l50_i50_b2_0"
))

ggplot(data = melted_dist_mat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  xlab("Tree 1") +
  ylab("Tree 2") +
  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust=0.5))  +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"))

### ---- Heatmap PI ----

dist_mat<-as.matrix(distances_pi)

rownames(dist_mat)<-filenames
colnames(dist_mat)<-filenames

melted_dist_mat <- reshape2::melt(dist_mat)

#reorder levels
melted_dist_mat$Var1<-factor(melted_dist_mat$Var1,levels = c("astral-mafft_r10_l10_i10_b2_0",
                                                             "astral-mafft_r10_l25_i25_b2_0",
                                                             "astral-mafft_r10_l25_i50_b2_0",
                                                             "astral-mafft_r10_l25_i75_b2_0",
                                                             "astral-mafft_r10_l50_i25_b2_0",
                                                             "astral-mafft_r10_l50_i50_b2_0",
                                                             "astral-mafft_r10_l50_i50_b2_d",
                                                             "astral-mafft_r10_l75_i25_b2_0",
                                                             "astral-mafft_r10_l75_i75_b2_0",
                                                             "astral-mafft_r25_l50_i50_b2_0",
                                                             "astral-mafft_r50_l50_i50_b2_0",
                                                             "astral-magus_r10_l50_i50_b2_0",
                                                             "astral-magus_r10_l50_i50_b2_d",
                                                             "astral-magus_r10_l50_i50_trimal_auto",
                                                             "astral-magus_r10_l50_i50_trimal_02",
                                                             "astral-magus_r10_l50_i50_trimal_gappyout",
                                                             "astral-mafftexons_r10_l50_i50_b2_0",
                                                             "astral-magusexons_r10_l50_i50_b2_0",
                                                             "astral-magusexons_r25_l50_i50_b2_0"
))

melted_dist_mat$Var2<-factor(melted_dist_mat$Var2,levels = c("astral-mafft_r10_l10_i10_b2_0",
                                                             "astral-mafft_r10_l25_i25_b2_0",
                                                             "astral-mafft_r10_l25_i50_b2_0",
                                                             "astral-mafft_r10_l25_i75_b2_0",
                                                             "astral-mafft_r10_l50_i25_b2_0",
                                                             "astral-mafft_r10_l50_i50_b2_0",
                                                             "astral-mafft_r10_l50_i50_b2_d",
                                                             "astral-mafft_r10_l75_i25_b2_0",
                                                             "astral-mafft_r10_l75_i75_b2_0",
                                                             "astral-mafft_r25_l50_i50_b2_0",
                                                             "astral-mafft_r50_l50_i50_b2_0",
                                                             "astral-magus_r10_l50_i50_b2_0",
                                                             "astral-magus_r10_l50_i50_b2_d",
                                                             "astral-magus_r10_l50_i50_trimal_auto",
                                                             "astral-magus_r10_l50_i50_trimal_02",
                                                             "astral-magus_r10_l50_i50_trimal_gappyout",
                                                             "astral-mafftexons_r10_l50_i50_b2_0",
                                                             "astral-magusexons_r10_l50_i50_b2_0",
                                                             "astral-magusexons_r25_l50_i50_b2_0"
))

ggplot(data = melted_dist_mat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust=0.5))  +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  ggtitle("PI")

### ---- Heatmap RF ----

dist_mat<-as.matrix(distances_rf)

rownames(dist_mat)<-filenames
colnames(dist_mat)<-filenames

melted_dist_mat <- reshape2::melt(dist_mat)

#reorder levels
melted_dist_mat$Var1<-factor(melted_dist_mat$Var1,levels = c("astral-mafft_r10_l10_i10_b2_0",
                                                             "astral-mafft_r10_l25_i25_b2_0",
                                                             "astral-mafft_r10_l25_i50_b2_0",
                                                             "astral-mafft_r10_l25_i75_b2_0",
                                                             "astral-mafft_r10_l50_i25_b2_0",
                                                             "astral-mafft_r10_l50_i50_b2_0",
                                                             "astral-mafft_r10_l50_i50_b2_d",
                                                             "astral-mafft_r10_l75_i25_b2_0",
                                                             "astral-mafft_r10_l75_i75_b2_0",
                                                             "astral-mafft_r25_l50_i50_b2_0",
                                                             "astral-mafft_r50_l50_i50_b2_0",
                                                             "astral-magus_r10_l50_i50_b2_0",
                                                             "astral-magus_r10_l50_i50_b2_d",
                                                             "astral-magus_r10_l50_i50_trimal_auto",
                                                             "astral-magus_r10_l50_i50_trimal_02",
                                                             "astral-magus_r10_l50_i50_trimal_gappyout",
                                                             "astral-mafftexons_r10_l50_i50_b2_0",
                                                             "astral-magusexons_r10_l50_i50_b2_0",
                                                             "astral-magusexons_r25_l50_i50_b2_0"
))

melted_dist_mat$Var2<-factor(melted_dist_mat$Var2,levels = c("astral-mafft_r10_l10_i10_b2_0",
                                                             "astral-mafft_r10_l25_i25_b2_0",
                                                             "astral-mafft_r10_l25_i50_b2_0",
                                                             "astral-mafft_r10_l25_i75_b2_0",
                                                             "astral-mafft_r10_l50_i25_b2_0",
                                                             "astral-mafft_r10_l50_i50_b2_0",
                                                             "astral-mafft_r10_l50_i50_b2_d",
                                                             "astral-mafft_r10_l75_i25_b2_0",
                                                             "astral-mafft_r10_l75_i75_b2_0",
                                                             "astral-mafft_r25_l50_i50_b2_0",
                                                             "astral-mafft_r50_l50_i50_b2_0",
                                                             "astral-magus_r10_l50_i50_b2_0",
                                                             "astral-magus_r10_l50_i50_b2_d",
                                                             "astral-magus_r10_l50_i50_trimal_auto",
                                                             "astral-magus_r10_l50_i50_trimal_02",
                                                             "astral-magus_r10_l50_i50_trimal_gappyout",
                                                             "astral-mafftexons_r10_l50_i50_b2_0",
                                                             "astral-magusexons_r10_l50_i50_b2_0",
                                                             "astral-magusexons_r25_l50_i50_b2_0"
))

ggplot(data = melted_dist_mat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_hline(yintercept=5.5) +
  geom_hline(yintercept=6.5) +
  geom_vline(xintercept=5.5) +
  geom_vline(xintercept=6.5) +
  xlab("Tree 1") +
  ylab("Tree 2") +
  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust=0.5,size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=18))  +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  labs(fill="RF distance")

ggsave("figures/figS13_rf_dist_heatmap.png",width=15,height=15)

distances<-distances_qs

#run PCoA on distance matrix
dist_pcoa <- ape::pcoa(distances)

ggplot(data.frame(dist_pcoa$vectors), aes(x = Axis.1, y = Axis.2, colour = as.factor(analyses$aligner))) +
  geom_point()
ggplot(data.frame(dist_pcoa$vectors), aes(x = Axis.1, y = Axis.2, colour = as.factor(analyses$r))) +
  geom_point()
ggplot(data.frame(dist_pcoa$vectors), aes(x = Axis.1, y = Axis.2, colour = as.factor(analyses$len))) +
  geom_point()
ggplot(data.frame(dist_pcoa$vectors), aes(x = Axis.1, y = Axis.2, colour = as.factor(analyses$ind))) +
  geom_point()
ggplot(data.frame(dist_pcoa$vectors), aes(x = Axis.1, y = Axis.2, colour = as.factor(analyses$trim))) +
  geom_point()


####
# ---- Checking distances simulated trees ----
####

library("TreeTools", quietly = TRUE)
treeNumbers <- c(1:220)
trees <- as.phylo(treeNumbers, 8)
spectrum <- hcl.colors(220, "plasma")
treeCols <- spectrum[treeNumbers]

library("TreeDist")
distances_ci <- ClusteringInfoDistance(trees)
distances_rf <- RobinsonFoulds(trees)
distances_pi <- PhylogeneticInfoDistance(trees)
distances_qs <- as.dist(Quartet::QuartetDivergence(
  Quartet::ManyToManyQuartetAgreement(trees, nTip = TRUE), similarity = FALSE))
