rm(list = ls())

library(ape)

#set paths to the trees folder you downloaded
#should contain all trees output from paralogs.sh
files <-
  list.files(
    path = "data/paralog_trees/",
    pattern = "RA*",
    full.names = T,
    recursive = FALSE
  )
filenames <-
  list.files(
    path = "data/paralog_trees/",
    pattern = "RA*",
    full.names = F,
    recursive = FALSE
  )

#####
# MANUAL
#####

par(mfrow = c(2, 1))
par(mar = c(1, 1, 1, 1))

paralog_yn <- vector()

for (i in 1:length(files))
{
  t <- read.tree(files[i])

  #remove suffixes from names
  t_names <- gsub("\\..*$", "", t$tip.label)

  #if fewer than 4 taxa then assign as not paralog
  #not enough info
  if (length(table(t_names)) < 4) {
    paralog_yn[i] <- 0
  } else {
    #otherwise plot trees and ask for input

    plot(t, type = 'un', cex = 0.5)
    add.scale.bar()
    title(main = filenames[i], cex = 0.5)
    plot(t, cex = 0.5)
    add.scale.bar()

    paralog_response <-
      readline(prompt = "Paralog? 1 = yes, 0 = no: ")
    paralog_yn[i] <- paralog_response

  }
}

#make dataframe of results per tree
paralog_results <- data.frame(filenames, paralog_yn)

#true paralogs only
paralog_results_positive <-
  paralog_results[paralog_results$paralog_yn == 1, ]

#reformat filenames for mv
paralog_results_positive$filenames<-gsub(".aligned.combined_","*",paralog_results_positive$filenames)
paralog_results_positive$filenames<-gsub(".fasta","*",paralog_results_positive$filenames)

#Copy list of names into a file, and modify to the format shown in README
cat(
  x = paste0("mv ", paralog_results_positive$filenames, " paralogs/"),
  file = "outputs/mv_paralogs_handpicked.txt",
  sep = "\n"
)

####
# AUTO
####

dists_tab_full_list<-list()
paralog_yn <- vector()

for (i in 1:length(files))
{
  t <- read.tree(files[i])

  #remove suffixes from names
  t_namestems <- gsub("\\..*$", "", t$tip.label)

  #if fewer than 4 taxa then assign as not paralog
  #not enough info
  if (length(table(t_namestems)) < 4) {
    paralog_yn[i] <- FALSE
  } else {

    #identify unique taxa
    uniq_taxa <- unique(t_namestems)
    uniq_taxa

    for (m in 1:length(uniq_taxa)) {
      #get different paralogs/alleles of one taxon
      alternates <- t$tip.label[grep(uniq_taxa[m], t$tip.label)]
      alternates

      l <- 1
      dists <- vector()

      for (j in 1:length(alternates)) {
        for (k in 1:length(alternates)) {
          #calculate distance between two tips of alternate paralogs
          dists[l] <- cophenetic(t)[alternates[j], alternates[k]]
          l <- l + 1
        }
      }

      #remove 0 (same tip) and get unique values (remove duplicate comparisons)
      #very low possibility that two alternates might be exact same distance
      dists_tab <-
        data.frame(rep(uniq_taxa[m], length(unique(dists[dists > 0]))), unique(dists[dists >
                                                                                       0]))
      colnames(dists_tab) <- c("taxon", "distance")
      dists_tab

      #combine into per-tree table
      if (m == 1) {
        dists_tab_full <- dists_tab
      } else {
        dists_tab_full <- rbind(dists_tab_full, dists_tab)
      }

    }

    #save to list
    dists_tab_full_list[[i]]<-dists_tab_full

    #average distance among all potential paralogs pairs across all taxa
    mean(dists_tab_full$distance)

    #average distance among all tips in tree
    all_dists<-cophenetic(t)
    mean(mean(all_dists[all_dists!=0]))

    #is mean distance between potential paralog tips larger than mean distance between all tips
    #if yes classify as paralog
    paralog_yn[i] <- mean(dists_tab_full$distance) > mean(mean(all_dists[all_dists!=0]))

  }
}

#make dataframe of results per tree
paralog_results <- data.frame(filenames, paralog_yn)

#true paralogs only
paralog_results_positive <-
  paralog_results[paralog_results$paralog_yn == 1, ]

#reformat filenames for mv
paralog_results_positive$filenames<-gsub(".aligned.combined_","*",paralog_results_positive$filenames)
paralog_results_positive$filenames<-gsub(".fasta","*",paralog_results_positive$filenames)

#Copy list of names into a file, and modify to the format shown in README
cat(
  x = paste0("mv ", paralog_results_positive$filenames, " paralogs/"),
  file = "outputs/mv_paralogs_auto.txt",
  sep = "\n"
)
