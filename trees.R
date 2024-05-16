#remotes::install_github("YuLab-SMU/ggtree", force = TRUE)
#remotes::install_github("YuLab-SMU/tidytree")
library(BiocManager)
library(ggtree)
library(tidytree)
library(treeio)
library(tidyverse)
library(viridis)
library(ggpubr)

#this was to figure out colors to match map palette
#scales::col_numeric(palette = viridisLite::turbo(10), domain = range(1:10))(1:10)
#000000

#read in base tree
iq <- read.newick("D:/Delphinus/iqtree_peru_r.txt",node.label = 'support')
bayes <- read.newick("D:/Delphinus/mrbayes_revisions_for_r.txt",node.label = 'support')

bayes@data$support <- as.integer(bayes@data$support)

#metadata file of samples, locations, and beak length
# this will stay the same
tips <- read.csv(file = "D:/Delphinus/tree_tip_info_peru.csv")

non_out <- c("Black Sea","Eastern North Atlantic","Eastern North Pacific","Eastern South Pacific","Eastern Tropical Pacific","Indian Ocean","Senegal","Western North Atlantic","Western North Pacific","Western South Pacific")

#reformat order of beak lengths
tips$Beak <- factor(tips$Beak, levels=c("Long","Short","Unknown","Outgroup"))
tips$Region <- factor(tips$Region, levels=c("Black Sea","Eastern North Atlantic","Eastern North Pacific","Eastern South Pacific","Eastern Tropical Pacific","Indian Ocean","Senegal","Western North Atlantic","Western North Pacific","Western South Pacific","Outgroup"))


iq_test <-  (ggtree(iq) %<+% tips + 
               geom_tiplab(align = TRUE, 
                           offset = 0.001, 
                           size=3.5) +
             scale_color_manual(values = c("#30123B","#4662D7","#36AAF9","#1AE4B6","#72FE5E","#C7EF34","#FABA39","#F66B19","#CB2A04","#7A0403","#000000"),
                                  limits = non_out) +
             geom_tippoint(aes(color=Region,shape=Beak), size=2.7) +
             scale_shape_manual(values=c(17,16,15,1))
             # scale bar
             #+ geom_treescale(x=0, y=40,fontsize=3)
             # adds support scores
             #+ geom_nodelab(aes(x=branch, label=round(support, 2), subset=support < 95),hjust=0, vjust=-.5, size=3)
             # get node numbers
             #+ geom_text2(aes(subset=!isTip, label=node, fontsize=.5), hjust=-.3)
             # black dot
             #+ geom_point2(aes(subset=(node==69)), shape=20, size=2)
             # red dot
             + geom_point2(aes(subset=(support < 95)), shape=20, size=2, colour="#CB2A04")
             + coord_cartesian(clip = 'off')
             + xlim(0, 0.070)
             + theme(legend.position = c(0.13,0.72),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14)) 
)
ggplot_build(iq_test)              

bayes_test <- (ggtree(bayes) %<+% tips 
               + geom_tippoint(aes(color = Region, shape = Beak), size = 3) 
               + geom_tiplab(align = TRUE, offset = 1, size = 3.45)
               + scale_color_manual(values = c("#30123B","#4662D7","#36AAF9","#1AE4B6","#72FE5E","#C7EF34","#FABA39","#F66B19","#CB2A04","#7A0403","#000000"),
                                    limits = non_out)
               + scale_shape_manual(values = c(17,16,15,1))
               # scale bar
               #+ geom_treescale(x=0, y=40,fontsize=3)
               # labels not cut off
               + coord_cartesian(clip = 'off')
               # adds support scores
               + geom_nodelab(aes(x=branch, label=round(support, 2), subset=support < 95), vjust=-0.25, size=4)
               # add dots
               + geom_point2(aes(subset=(support < 95)), shape=20, size=2, colour="#CB2A04")
               + xlim(0, 30)
               + theme(legend.position = c(0.12,0.72),
                       legend.text = element_text(size = 14),
                       legend.title = element_text(size = 14)) 
               
)


ggplot_build(bayes_test)

#to save: export, pdf, landscape US letter


b_no_labs <- (ggtree(bayes) %<+% tips 
              + geom_tippoint(aes(color = Region, shape = Beak), size = 3) 
              + scale_color_manual(values = c("#30123B","#4662D7","#36AAF9","#1AE4B6","#72FE5E","#C7EF34","#FABA39","#F66B19","#CB2A04","#7A0403","#000000"),
                                   limits = non_out)
              + scale_shape_manual(values = c(17,16,15,1))
              # scale bar
              #+ geom_treescale(x=0, y=40,fontsize=3)
              # adds support scores
              #+ geom_nodelab(aes(x=branch, label=round(support, 2), subset=support < 95), vjust=-0.25, size=4)
              # add dots
              + geom_point2(aes(subset=(support < 95)), 
                            shape=20, size=2, colour="#CB2A04")
              #+ xlim(0, 30)
              + theme(legend.position = "left",
                      legend.text = element_text(size = 14),
                      legend.title = element_text(size = 14))
)

iq_no_labs <-  (ggtree(iq) %<+% tips 
                #+ geom_tiplab(align = TRUE, offset = 0.001, size=3.5)
                + scale_color_manual(values = c("#30123B","#4662D7","#36AAF9","#1AE4B6","#72FE5E","#C7EF34","#FABA39","#F66B19","#CB2A04","#7A0403","#000000"),
                                     limits = non_out)
                + geom_tippoint(aes(color=Region,shape=Beak), size=2.7) 
                + scale_shape_manual(values=c(17,16,15,1))
                # scale bar
                #+ geom_treescale(x=0, y=40,fontsize=3)
                # red dot
                + geom_point2(aes(subset=(support < 95)), 
                              shape=20, size=1.5, colour="#CB2A04")
                #+ xlim(0, 0.070)
                + theme(legend.position = "none")
                          #c(0.13,0.72)) 
)

# export to pdf landscape US letter
both <- ggarrange(b_no_labs, iq_no_labs, 
                  labels = c("(a)", "(b)"),
                  label.x = c(0.5,0.1),
                  ncol = 2, nrow = 1)
