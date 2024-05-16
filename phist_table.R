library(tidyverse)
library(reshape2) 
library(ggpubr)

# read in data
phi_raw <-
  read.csv('C:/Users/madel/Documents/R_scripts_files/delphinus/Sig_Phi_3.csv',
           row.names = 1, header=TRUE ,encoding = "UTF-8") 

F_raw <-
  read.csv('C:/Users/madel/Documents/R_scripts_files/delphinus/Sig_F_3.csv',
           row.names = 1, header=TRUE ,encoding = "UTF-8")


# phist -------------------------------------------------------------------


# fix column names using row names
colnames(phi_raw) <- rownames(phi_raw)

# order I want (vector)  
order <- c("SEN (n=31)",
           "BLS (n=11)", 
           "WNA (n=76)", 
           "Alboran (n=34)",
           "Azores (n=100)",
           "Canary (n=21)",
           "France (n=6)",
           "Galicia (n=30)",
           "Ionian (n=19)",
           "Ireland (n=22)",
           "Madeira (n=52)",
           "Portugal (n=17)",
           "Tyrrenian (n=5)")

# make a matrix, reorder according to vector, make NA's for half of square
phi_mat <- as.matrix(phi_raw)
phi_mat <- phi_mat[order, order]
tri <- phi_mat
tri[upper.tri(phi_mat, diag=TRUE)] <- NA

# convert to tidy format for plotting; refactor for y-axis, make separate value columns for plotting (fill) vs. appearance (-ve's and *'s)
melt_phi <- melt(tri, na.rm =TRUE) %>% 
  cbind(value2 = .$value) %>% 
  mutate(value2 =  str_remove_all(value2, "\\*")) %>% 
  mutate(value2 = if_else(as.double(value2) < 0, 0, as.double(value2))) %>% 
  mutate(Var1 =
           Var1 %>% 
           fct_relevel(rev(order)))

# plot
p <- ggplot(data = melt_phi, 
            aes(Var2, Var1, fill = value2)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient(low = "white", high = "red", name = expression(italic(Φ)[ST])) +  
  labs( x = element_blank(), y = element_blank()) + 
  geom_text(aes(label = value),
            size = 4) +
  scale_x_discrete(
    expand = c(0, 0), 
    position = 'top') +
  scale_y_discrete(
    expand = c(0, 0), 
    position = 'left') +
  theme_grey() +
  theme(
    #text = element_text(size = 14),
    axis.text.x = 
          element_text(angle = 45, vjust = 0, size = 14, hjust = 0, color = "black"),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        axis.text.y = element_text(size = 14, color = "black"),
        panel.border = element_rect(colour = "grey92", fill=NA, linewidth=0.1)) 


# FST ---------------------------------------------------------------------

# fix column names using row names
colnames(F_raw) <- rownames(F_raw)

# order I want (vector)  
order <- c("SEN (n=31)",
           "BLS (n=11)", 
           "WNA (n=76)", 
           "Alboran (n=34)",
           "Azores (n=100)",
           "Canary (n=21)",
           "France (n=6)",
           "Galicia (n=30)",
           "Ionian (n=19)",
           "Ireland (n=22)",
           "Madeira (n=52)",
           "Portugal (n=17)",
           "Tyrrenian (n=5)")

# make a matrix, reorder according to vector, make NA's for half of square
F_mat <- as.matrix(F_raw)
F_mat <- F_mat[order, order]
tri <- F_mat
tri[upper.tri(F_mat, diag=TRUE)] <- NA

# convert to tidy format for plotting; refactor for y-axis, make separate value columns for plotting (fill) vs. appearance (-ve's and *'s)
melt_F <- melt(tri, na.rm =TRUE) %>% 
  cbind(value2 = .$value) %>% 
  mutate(value2 =  str_remove_all(value2, "\\*")) %>% 
  mutate(value2 = if_else(as.double(value2) < 0, 0, as.double(value2))) %>% 
  mutate(Var1 =
           Var1 %>% 
           fct_relevel(rev(order)))

# plot
F <- ggplot(data = melt_F, 
            aes(Var2, Var1, fill = value2)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient(low = "white", high = "red", name = expression(italic("F")[ST])) +  
  labs( x = element_blank(), y = element_blank()) + 
  geom_text(aes(label = value),
            size = 4) +
  scale_x_discrete(
    expand = c(0, 0), 
    position = 'top') +
  scale_y_discrete(
    expand = c(0, 0), 
    position = 'left') +
  theme_grey() +
  theme(
    #text = element_text(size = 14),
    axis.text.x = 
      element_text(angle = 45, vjust = 0, size = 14, hjust = 0, color = "black"),
    legend.title=element_text(size = 14),
    legend.text=element_text(size = 14),
    axis.text.y = element_text(size = 14, color = "black"),
    panel.border = element_rect(colour = "grey92", fill=NA, linewidth=0.1))  

# save --------------------------------------------------------------------

both <- ggarrange(p,F, 
                  labels = c("(a)", "(b)"), 
                  ncol = 1, nrow = 2)
# save
ggsave('C:/Users/madel/Documents/R_scripts_files/delphinus/phist_Fst.png',both, width = 10, height = 9.5, dpi = 600)
#8 by 9.5

ggsave('C:/Users/madel/Documents/R_scripts_files/delphinus/phist_Fst.pdf',both, width = 10, height = 9.5, device = cairo_pdf)
        