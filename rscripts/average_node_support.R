rm(list = ls())

par(mar = c(5, 5, 5, 5))

#load packages
library(ape)
library(ggplot2)
library(patchwork)


#list of filepaths to read in
filepaths_lpp <-
  list.files(
    path = "/home/andrew/Dropbox/projects/AJH_magnoliids/PAFTOL_magnoliids/data/trees",
    pattern = "*LPP.tre",
    full.names = TRUE,
    recursive = TRUE
  )

filepaths_qs <-
  list.files(
    path = "/home/andrew/Dropbox/projects/AJH_magnoliids/PAFTOL_magnoliids/data/trees",
    pattern = "*QS.tre",
    full.names = TRUE,
    recursive = TRUE
  )

#names of analyses used to make trees
filenames <-
  list.files(
    path = "/home/andrew/Dropbox/projects/AJH_magnoliids/PAFTOL_magnoliids/data/trees",
    pattern = "*LPP.tre",
    full.names = FALSE,
    recursive = TRUE
  )

filenames <- gsub("/astral_bs10_LPP.tre", "", filenames)

#empty vectors
mean_lpps <- vector()
mean_qss <- vector()
median_lpps <- vector()
median_qss <- vector()
sd_lpps <- vector()
sd_qss <- vector()
tree_names <- vector()

for (i in 1:length(filepaths_lpp)) {
  #read in tree
  phy_lpp <- read.tree(filepaths_lpp[[i]])

  #get LPP values
  lpp <- na.omit(as.numeric(phy_lpp$node.label))

  #calc and save LPP values
  mean_lpps[i] <- mean(lpp)
  median_lpps[i] <- median(lpp)
  sd_lpps[i] <- sd(lpp)

  #read in tree
  phy_qs <- read.tree(filepaths_qs[[i]])

  #get QS values
  qs <- na.omit(as.numeric(phy_qs$node.label))

  #calc and save QS values
  mean_qss[i] <- mean(qs)
  median_qss[i] <- median(qs)
  sd_qss[i] <- sd(qs)

}

#make and sort results table
res <-
  data.frame(filenames,
             mean_lpps,
             median_lpps,
             sd_lpps,
             mean_qss,
             median_qss,
             sd_qss)
res[order(res$mean_lpps), ]
res[order(res$mean_qss), ]

#plot
plot(mean_lpps ~ sd_lpps, data = res)
plot(mean_lpps ~ mean_qss, data = res)
plot(mean_qss ~ median_qss, data = res)
plot(mean_qss ~ sd_qss, data = res)

#write results table
write.csv(res, "outputs/mean_lpps.csv")

#rename dataset
res_all <- res

###
# ---- At shallow node depths only ----
###

#CUTOFF FOR SHALLOW/DEEP
cutoff <- 5

#add terminal branch length to fix plotting
phy_lpp$edge.length[phy_lpp$edge.length == "NaN"] <- 0.25

#To help check whether depths and node labels match:

#plot node depths to check
pdf("figures/phylo_with_node_depth.pdf",
    width = 10,
    height = 20)
plot(ladderize(phy_lpp),
     align.tip.label = TRUE,
     cex = 0.25)
nodelabels((node.depth(phy_lpp)[node.depth(phy_lpp) > 1]), cex = 0.5)
dev.off()

#plot node depths to check
pdf("figures/phylo_with_LPP_for_node_depth.pdf",
    width = 10,
    height = 20)
plot(ladderize(phy_lpp),
     align.tip.label = TRUE,
     cex = 0.25)
nodelabels(phy_lpp$node.label, cex = 0.5)
dev.off()

#matrix to compare
cbind((node.depth(phy_lpp)[node.depth(phy_lpp) > 1]), phy_lpp$node.label)

#plot node depth against QS
plot(phy_qs$node.label, node.depth(phy_qs)[node.depth(phy_qs) > 1])

#empty vectors
mean_lpps <- vector()
mean_qss <- vector()
median_lpps <- vector()
median_qss <- vector()
sd_lpps <- vector()
sd_qss <- vector()
tree_names <- vector()

for (i in 1:length(filepaths_lpp)) {
  #read in tree
  phy_lpp <- read.tree(filepaths_lpp[[i]])

  #only shallow nodes
  node_labs_shallow_lpp <-
    phy_lpp$node.label[node.depth(phy_lpp)[node.depth(phy_lpp) > 1] < cutoff]

  #get LPP values
  lpp <- na.omit(as.numeric(node_labs_shallow_lpp))

  #calc and save LPP values
  mean_lpps[i] <- mean(lpp)
  median_lpps[i] <- median(lpp)
  sd_lpps[i] <- sd(lpp)

  #read in tree
  phy_qs <- read.tree(filepaths_qs[[i]])

  #only shallow nodes
  node_labs_shallow_qs <-
    phy_qs$node.label[node.depth(phy_qs)[node.depth(phy_qs) > 1] < cutoff]

  #get QS values
  qs <- na.omit(as.numeric(node_labs_shallow_qs))

  #calc and save QS values
  mean_qss[i] <- mean(qs)
  median_qss[i] <- median(qs)
  sd_qss[i] <- sd(qs)

}

#make and sort results table
res_shallow <-
  data.frame(filenames,
             mean_lpps,
             median_lpps,
             sd_lpps,
             mean_qss,
             median_qss,
             sd_qss)
res_shallow


###
# ---- At deep node depths only ----
###

#empty vectors
mean_lpps <- vector()
mean_qss <- vector()
median_lpps <- vector()
median_qss <- vector()
sd_lpps <- vector()
sd_qss <- vector()
tree_names <- vector()

for (i in 1:length(filepaths_lpp)) {
  #read in tree
  phy_lpp <- read.tree(filepaths_lpp[[i]])

  #only deep nodes
  node_labs_deep_lpp <-
    phy_lpp$node.label[node.depth(phy_lpp)[node.depth(phy_lpp) > 1] >= cutoff]

  #get LPP values
  lpp <- na.omit(as.numeric(node_labs_deep_lpp))

  #calc and save LPP values
  mean_lpps[i] <- mean(lpp)
  median_lpps[i] <- median(lpp)
  sd_lpps[i] <- sd(lpp)

  #read in tree
  phy_qs <- read.tree(filepaths_qs[[i]])

  #only deep nodes
  node_labs_deep_qs <-
    phy_qs$node.label[node.depth(phy_qs)[node.depth(phy_qs) > 1] >= cutoff]

  #get QS values
  qs <- na.omit(as.numeric(node_labs_deep_qs))

  #calc and save QS values
  mean_qss[i] <- mean(qs)
  median_qss[i] <- median(qs)
  sd_qss[i] <- sd(qs)

}

#make and sort results table
res_deep <-
  data.frame(filenames,
             mean_lpps,
             median_lpps,
             sd_lpps,
             mean_qss,
             median_qss,
             sd_qss)
res_deep

#check that dfs are in the same order
res_deep$filenames == res_shallow$filenames
res_deep$filenames == res$filenames

#write results table
write.csv(res, "outputs/mean_lpps_deep.csv")

#compare different support values at shallow, deep and all nodes
plot(res_shallow$mean_lpps ~ res_deep$mean_lpps)
plot(res_shallow$sd_lpps ~ res_deep$sd_lpps)
plot(res_shallow$mean_qss ~ res_deep$mean_qss)
plot(res_shallow$median_qss ~ res_deep$median_qss)
plot(res_shallow$sd_qss ~ res_deep$sd_qss)
plot(res$mean_lpps ~ res_deep$mean_lpps)
plot(res$mean_lpps ~ res_shallow$mean_lpps)

###
# ---- Normalised scores and quartet numbers ----
###

#list of filepaths to read in
filepaths_logs <-
  list.files(
    path = "/home/andrew/Dropbox/projects/AJH_magnoliids/PAFTOL_magnoliids/data/trees",
    pattern = "*LPP.log",
    full.names = TRUE,
    recursive = TRUE
  )

filenames <-
  list.files(
    path = "/home/andrew/Dropbox/projects/AJH_magnoliids/PAFTOL_magnoliids/data/trees",
    pattern = "*LPP.log",
    full.names = FALSE,
    recursive = TRUE
  )

filenames <- gsub("/astral_bs10_LPP.log", "", filenames)

#empty vectors
scores <- vector()
quartets <- vector()
trees <- vector()

for (i in 1:length(filepaths_logs)) {
  #read in log file
  astral_log <- readLines(filepaths_logs[i])

  #get lines with info
  score <- astral_log[grep("Normalized score", astral_log)]
  quartet <-
    astral_log[grep("quartet trees in the gene trees", astral_log)]
  tree <- astral_log[grep("all_bs10.trees", astral_log)]

  #convert to numbers
  #put in vectors
  scores[i] <- as.numeric(gsub(".*:", "", score))
  quartets[i] <- as.numeric(gsub(".*:", "", quartet))
  trees[i] <- as.numeric(gsub("trees.*", "", tree))

}


names(scores) <- filenames
sort(scores)

names(quartets) <- filenames
sort(quartets)

names(trees) <- filenames
sort(trees)

names(scores) == names(quartets)

plot(scores ~ quartets)

plot(trees ~ quartets)

text(
  quartets,
  trees,
  labels = names(scores),
  cex = 0.5,
  pos = 4
)

names(scores) == res_all$filenames

combined_res <- data.frame(scores, quartets, trees, res_all)

plot(combined_res$scores,combined_res$mean_qss)

#aligner
as.factor(grepl("mafft", rownames(combined_res)))

#r
analyses <-
  data.frame(t(data.frame(strsplit(
    rownames(combined_res), "_"
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

pdf("figures/scatterplots_lpp_vs_qss_filled_by_methods.pdf")

for (i in 1:length(colnames(analyses))) {
  p <-
    ggplot(combined_res, aes(x = quartets, y = scores, fill = analyses[, i])) +
    geom_point(
      color = "black",
      shape = 21,
      alpha = 0.5,
      size = 3,
      stroke = 0.5
    )

  print(p)

}

for (i in 1:length(colnames(analyses))) {
  p2 <-
    ggplot(combined_res, aes(x = mean_lpps, y = mean_qss, fill = analyses[, i])) +
    geom_point(
      color = "black",
      shape = 21,
      alpha = 0.5,
      size = 3,
      stroke = 0.5
    )

  print(p2)

}

for (i in 1:length(colnames(analyses))) {
  p3 <-
    ggplot(combined_res, aes(x = mean_lpps, y = scores, fill = analyses[, i])) +
    geom_point(
      color = "black",
      shape = 21,
      alpha = 0.5,
      size = 3,
      stroke = 0.5
    )

  print(p3)

}

dev.off()

colnames(res_shallow) <- paste(colnames(res_shallow),"_shal",sep="")
colnames(res_deep) <- paste(colnames(res_deep),"_deep",sep="")

res_shallow$filenames_shal==res_deep$filenames_deep

res_shal_deep<-cbind(res_shallow,res_deep)

library(ggrepel)

p1 <- ggplot(res_shal_deep, aes(x = mean_lpps_shal, y = mean_lpps_deep)) +
  geom_point(
    color = "black",
    shape = 21,
    alpha = 0.5,
    size = 3,
    stroke = 0.5
  ) + xlim(0.8,1) + ylim(0.8,1) +
  geom_text_repel(
    label = rownames(combined_res),
    max.overlaps = 40,
    box.padding = 1,
    min.segment.length = 0,
    force = 1,
    size = 3
  ) + geom_abline(slope = 1, intercept = 0,col='red')

p2 <- ggplot(res_shal_deep, aes(x = mean_qss_shal, y = mean_qss_deep)) +
  geom_point(
    color = "black",
    shape = 21,
    alpha = 0.5,
    size = 3,
    stroke = 0.5
  ) + xlim(55,80) + ylim(55,80) +
  geom_text_repel(
    label = rownames(combined_res),
    max.overlaps = 40,
    box.padding = 1,
    min.segment.length = 0,
    force = 1,
    size = 3
  ) + geom_abline(slope = 1, intercept = 0,col="red")

p1+p2

ggsave(
  "figures/scatterplots_lpp_qs_shallow_vs_deep.png",
  width = 30,
  height = 15
)


ggplot(combined_res, aes(x = quartets, y = trees, fill = mean_lpps)) +
  geom_point(
    color = "black",
    shape = 21,
    alpha = 0.5,
    size = 3,
    stroke = 0.5
  ) + xlim(1.0e+08, 3e+10) +
  geom_text_repel(
    label = rownames(combined_res),
    max.overlaps = 40,
    box.padding = 1,
    min.segment.length = 0,
    force = 1,
    size = 3
  ) + scale_fill_gradientn(colours = rainbow(5))

ggsave(
  "figures/scatter_quartets_vs_notrees_filled_by_mean_lpp.png",
  width = 15,
  height = 15
)

ggplot(combined_res, aes(x = mean_lpps, y = mean_qss, fill = scores)) +
  geom_point(
    color = "black",
    shape = 21,
    alpha = 0.5,
    size = 3,
    stroke = 0.5
  ) + xlim(0.8, 1) +
  geom_text_repel(
    label = rownames(combined_res),
    max.overlaps = 20,
    box.padding = 1,
    min.segment.length = 0,
    force = 1,
    size = 3
  ) + scale_fill_gradientn(colours = rainbow(5))

ggsave(
  "figures/scatter_lpp_vs_qss_filled_by_scores.png",
  width = 15,
  height = 15
)

#remove columns not used in scoring
scoring_res <- combined_res[, !names(combined_res) %in% c("filenames",
                                                          "median_lpps",
                                                          "sd_lpps",
                                                          "median_qss",
                                                          "sd_qss",
                                                          "trees")]

scoring_res

scoring_res_scaled <- scale(scoring_res,center = TRUE, scale = TRUE)
scoring_res_scaled<-data.frame(scoring_res_scaled)


cor(scoring_res_scaled)
sort(rowSums(scoring_res_scaled),decreasing = TRUE)[1:10]

#order
rownames(scoring_res)==names(rowSums(scoring_res_scaled))

scoring_res$combined_scaled<-rowSums(scoring_res_scaled)

colnames(scoring_res)<-
  c("Normalized scores","Total Quartets","Mean LPP","Mean QS","Sum of scaled values")


scoring_res <- round(scoring_res,3)

tree_names<-data.frame(t(sapply(str_split(rownames(scoring_res), "_"),c)))

scoring_res$aligner <- tree_names[,1]
scoring_res$r <- tree_names[,2]
scoring_res$r<-gsub("r","",scoring_res$r)
scoring_res$l <- tree_names[,3]
scoring_res$l<-gsub("l","",scoring_res$l)
scoring_res$i <- tree_names[,4]
scoring_res$i<-gsub("i","",scoring_res$i)
scoring_res$trimmer <- paste(tree_names[,5],tree_names[,6],sep="_")
scoring_res$trimmer<-gsub("b2_0","gblocks_b2_0",scoring_res$trimmer)
scoring_res$trimmer<-gsub("b2_d","gblocks_b2_d",scoring_res$trimmer)
scoring_res$data_type <- rep("supercontigs",length(scoring_res$`Normalized scores`))
scoring_res$data_type[grep("exons",scoring_res$aligner)] <- "exons"
scoring_res$aligner<-gsub("exons","",scoring_res$aligner)
scoring_res$aligner<-gsub("astral-","",scoring_res$aligner)

scoring_res <- scoring_res[,c(6:length(colnames(scoring_res)),1:5)]

write.csv(scoring_res,"outputs/average_node_support.csv",row.names = FALSE)

res_shal_deep$filenames_shal==res_shal_deep$filenames_deep

res_shall_deep_cleaned<-res_shal_deep[,c("mean_lpps_shal",
                                         "mean_lpps_deep",
                                         "mean_qss_shal",
                                         "mean_qss_deep")]

rownames(res_shall_deep_cleaned)<-res_shal_deep$filenames_shal

res_shall_deep_cleaned <- round(res_shall_deep_cleaned,3)

write.csv(res_shall_deep_cleaned,"outputs/shallow_vs_deep_average_node_support.csv")

###PAirs plot


png("figures/pairs_quality_scores.png",width=750,height=750)
pairs(scoring_res[,c("Normalized scores","Total Quartets","Mean LPP","Mean QS")])
dev.off()
