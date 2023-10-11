rm(list = ls())

library(ape)

#set paths to the trees folder you downloaded
#should contain all trees output from paralogs.sh
files <-
  list.files(
    path = "/home/andrew/Dropbox/projects/AJH_magnoliids/phyparts/mag/trees",
    pattern = "RA*",
    full.names = T,
    recursive = FALSE
  )
filenames <-
  list.files(
    path = "/home/andrew/Dropbox/projects/AJH_magnoliids/phyparts/mag/trees",
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


#"OSHQ",
#"WZFE"
#"P_016909"
#"P_018831"
#"P0109B"
#"P0203H"

#gene trees
for (i in 1:length(files)){
  t <- read.tree(files[i])

  if("WZFE"%in%t$tip.label){
    t2<-root(t,"WZFE")
    write.tree(t2,file=paste("/home/andrew/Dropbox/projects/AJH_magnoliids/phyparts/mag/trees/rooted/",filenames[i],sep=""))
  } else if("OSHQ"%in%t$tip.label){
    t2<-root(t,"OSHQ")
    write.tree(t2,file=paste("/home/andrew/Dropbox/projects/AJH_magnoliids/phyparts/mag/trees/rooted/",filenames[i],sep=""))
  } else if("P_016909"%in%t$tip.label){
    t2<-root(t,"P_016909")
    write.tree(t2,file=paste("/home/andrew/Dropbox/projects/AJH_magnoliids/phyparts/mag/trees/rooted/",filenames[i],sep=""))
  }

}

#ASTRAL
t <- read.tree("/home/andrew/Dropbox/projects/AJH_magnoliids/phyparts/mag/astral_bs10_LPP.tre")
t2<-root(t,"WZFE")
write.tree(t2,file="/home/andrew/Dropbox/projects/AJH_magnoliids/phyparts/mag/rooted.astral_bs10_LPP.tre")
