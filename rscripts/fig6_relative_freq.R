
#load packages
library(reshape2)
library(ggplot2)
library(wesanderson)
library(patchwork)
library(ape)
library(ggtree)

###
# ---- Meiocarpidium ----
###


#read in data
freqs_meio <-read.csv("~/Dropbox/projects/AJH_magnoliids/DiscoVista-master/mag/meio_output/freqQuadCorrected.csv",
    header = F,
    sep = '\t'
  )

#remove phylogeny column
freqs_meio <- freqs_meio[, !colnames(freqs_meio) %in% c("V3")]
head(freqs_meio)

#calculate relative frequency
freqs_meio$value <- freqs_meio$V5 / freqs_meio$V6

#order so most frequent is first
freqs_meio$V8 <- reorder(freqs_meio$V8, -freqs_meio$value)

#select desired branch
freqs_meio_20 <- freqs_meio[freqs_meio$V7==20,]
freqs_meio_20

#plot relative freqs
r1 <- ggplot(data = freqs_meio_20) + aes(x = V8, y = value, fill = V9) + geom_bar(
  stat = 'identity',
  width = 0.8,
  position = 'dodge') +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c(wesanderson::wes_palettes$Royal1[1],"#D3D3D3","#808080"), name = 'Topology') +
  geom_hline(yintercept = 1 / 3,size = 0.4,linetype = 2) +
  ylab('Relative frequency') +
  xlab('') +
  ylim(0,0.6) +
  theme(legend.position = "none")

#plot freqs
f1 <- ggplot(data = freqs_meio_20) + aes(x = V8, y = V5, fill = V9) + geom_bar(
  stat = 'identity',
  width = 0.8,
  position = 'dodge') +
  theme_bw() +
  theme(legend.position = "none",axis.text.x = element_text(size=12), axis.title.y = element_text(vjust=2)) +
  scale_fill_manual(values = c(wesanderson::wes_palettes$Royal1[1],"#D3D3D3","#808080"), name = 'Topology') +
  ylab('Frequency') +
  xlab('') +
  ylim(0,100) +
  theme(legend.position = "none")

#Phylo
tree <-read.tree("~/Dropbox/projects/AJH_magnoliids/DiscoVista-master/mag/meio_output/main.tre.out")

#NOTE: Alternative plotting strategies
#plot(tree,use.edge.length=F,edge.width = 5,edge.color="orange",root.edge = F, type = "unrooted",lab4ut="axial")
#plot(tree,use.edge.length=F,edge.width = 5,edge.color="orange",root.edge = F, type = "radial")
#plot(tree,use.edge.length=F,edge.width = 5,edge.color="orange",root.edge = T, type = "cladogram")

plot(tree,use.edge.length=F,edge.width = 5,edge.color="orange",root.edge = F, lab4ut="axial")
edgelabels(tree$edge.length, bg = "black",col="white", font=2)
#edgelabels()

edge <- data.frame(tree$edge, edge_num=tree$edge.length)
colnames(edge)=c("parent", "node", "edge_num")

edge$labs<-NA
edge$labs[grep("^3$",edge$edge_num)]<-"3"
edge$labs[grep("19",edge$edge_num)]<-"19"
#edge$labs[grep("20",edge$edge_num)]<-"20"
edge$labs[grep("22",edge$edge_num)]<-"22"
edge$labs[grep("23",edge$edge_num)]<-"23"


edge$cols<-"nq"
edge$cols[grep("^3$",edge$edge_num)]<-"quartet"
edge$cols[grep("19",edge$edge_num)]<-"quartet"
edge$cols[grep("20",edge$edge_num)]<-"quartet"
edge$cols[grep("22",edge$edge_num)]<-"quartet"
edge$cols[grep("23",edge$edge_num)]<-"quartet"

d<-data.frame(node = edge$node, trait = edge$cols)
d$node <- as.numeric(d$node)
d$lwd <- as.numeric(as.factor(d$trait))
d$lwd <- d$lwd/2
d$lwd[grep(2,d$lwd)]<-1.1

tree <- treeio::full_join(tree, d, by = 'node')

p <- ggtree(tree,aes(colour=trait),ladderize = T, right = T, size=1.5, branch.length = 'none') +
  geom_tiplab(aes(subset = node == c(1:6), label=label),offset = 0.3) +
  geom_tiplab(aes(subset = node == 7, label=label),fontface='italic',offset = 0.3) +
  geom_tiplab(aes(subset = node == 8, label=label),offset = 0.3) +
  geom_tiplab(aes(subset = node == c(9,10), label=label),offset = 0.3) +
  geom_tiplab(aes(subset = node == 11, label=label),fontface='bold.italic',offset = 0.3) +
  geom_tiplab(aes(subset = node == 12, label=label),offset = 0.3) +
  xlim(0,17) +
  theme(legend.position = "none") +
  scale_color_manual(values=c("black", wesanderson::wes_palettes$Royal1[1])) +
  annotate("text", x = 0, y = 13, label = "(b)")

p1 <- p %<+% edge + geom_label(aes(x=branch, label=labs))

p1


###
# ---- Hydnoraceae ----
###


#read in data
freqs_hydn <-read.csv("~/Dropbox/projects/AJH_magnoliids/DiscoVista-master/mag/hydn_output/freqQuadCorrected.csv",
                      header = F,
                      sep = '\t'
)

#remove phylogeny column
freqs_hydn <- freqs_hydn[, !colnames(freqs_hydn) %in% c("V3")]
head(freqs_hydn)

#calculate relative frequency
freqs_hydn$value <- freqs_hydn$V5 / freqs_hydn$V6

#order so most frequent is first
freqs_hydn$V8 <- reorder(freqs_hydn$V8, -freqs_hydn$value)

#select desired branch
freqs_hydn_19 <- freqs_hydn[freqs_hydn$V7==19,]
freqs_hydn_19

#plot relative freqs
r2 <- ggplot(data = freqs_hydn_19) + aes(x = V8, y = value, fill = V9) + geom_bar(
  stat = 'identity',
  width = 0.8,
  position = 'dodge') +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c(wesanderson::wes_palettes$IsleofDogs1[1],"#808080","#D3D3D3"), name = 'Topology') +
  geom_hline(yintercept = 1 / 3,size = 0.4,linetype = 2) +
  ylab('Relative frequency') +
  xlab('') +
  ylim(0,0.6) +
  theme(legend.position = "none")

#plot freqs
f2 <- ggplot(data = freqs_hydn_19) + aes(x = V8, y = V5, fill = V9) + geom_bar(
  stat = 'identity',
  width = 0.8,
  position = 'dodge') +
  theme_bw() +
  theme(legend.position = "none",axis.text.x = element_text(size=12), axis.title.y = element_text(vjust=2)) +
  scale_fill_manual(values = c(wesanderson::wes_palettes$IsleofDogs1[1],"#808080","#D3D3D3"), name = 'Topology') +
  ylab('Frequency') +
  xlab('') +
  ylim(0,50)

#Phylo
tree <-read.tree("~/Dropbox/projects/AJH_magnoliids/DiscoVista-master/mag/hydn_output/main.tre.out")

#NOTE: Alternative plotting strategies
#plot(tree,use.edge.length=F,edge.width = 5,edge.color="orange",root.edge = F, type = "unrooted",lab4ut="axial")
#plot(tree,use.edge.length=F,edge.width = 5,edge.color="orange",root.edge = F, type = "radial")
#plot(tree,use.edge.length=F,edge.width = 5,edge.color="orange",root.edge = T, type = "cladogram")

plot(tree,use.edge.length=F,edge.width = 5,edge.color="orange",root.edge = F, lab4ut="axial")
edgelabels(tree$edge.length, bg = "black",col="white", font=2)
#edgelabels()


edge <- data.frame(tree$edge, edge_num=tree$edge.length)
colnames(edge)=c("parent", "node", "edge_num")
edge$labs<-NA
edge$labs[grep("^8$",edge$edge_num)]<-"8"
edge$labs[grep("10",edge$edge_num)]<-"10"
edge$labs[grep("18",edge$edge_num)]<-"18"
#edge$labs[grep("19",edge$edge_num)]<-"19"
edge$labs[grep("20",edge$edge_num)]<-"20"

edge$cols<-"nq"
edge$cols[grep("^8$",edge$edge_num)]<-"quartet"
edge$cols[grep("10",edge$edge_num)]<-"quartet"
edge$cols[grep("18",edge$edge_num)]<-"quartet"
edge$cols[grep("19",edge$edge_num)]<-"quartet"
edge$cols[grep("20",edge$edge_num)]<-"quartet"

d<-data.frame(node = edge$node, trait = edge$cols)
d$node <- as.numeric(d$node)
tree <- treeio::full_join(tree, d, by = 'node')

p <- ggtree(tree,aes(colour=trait),ladderize = T, right = T, branch.length = 'none',size=1.5) +
  geom_tiplab(aes(subset= node != c(1,2,3,4,5,6), label=label),fontface='italic',offset = 0.3) +
  geom_tiplab(aes(subset= node == c(5,6), label=label),fontface='bold.italic',offset = 0.3) +
  geom_tiplab(aes(subset= node == c(1,2,3,4), label=label),fontface=1,offset = 0.3) +
  xlim(0,13) +
  theme(legend.position = "none") +
  scale_color_manual(values=c("black", wesanderson::wes_palettes$IsleofDogs1[1])) +
  annotate("text", x = 0, y = 19, label = "(a)")

p2 <- p %<+% edge + geom_label(aes(x=branch, label=labs))

p2

#combine phylo and absolute frequency plots
 ( p2 + f2 ) / ( p1 + f1 )

ggsave("figures/fig6_relative_freq.png",height = 10,width = 10)



