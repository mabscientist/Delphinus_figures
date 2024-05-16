library(tmap)
library(ggplot2)
library(svglite)
library(viridis)
library(cowplot)
library(magick)
data(World)

#Make table of info
sample_sites <- tibble::tribble( 
  ~Location,           ~Latitude,     ~Longitude,     ~Label,     ~Beak,
  "Black Sea",    43.4130, 31.5,  "n=4***","Short-beaked",
  "Eastern North Atlantic", 52, -4, "n=1**","Short-beaked",
  "Eastern North Atlantic", 58, 9.5018, "n=1*","Short-beaked",
  "Eastern North Pacific",  33.7701, -118.1937, "n=2","Long-beaked",
  "Eastern South Pacific", -9,-80,"n=3","Long-beaked",
  "Eastern Tropical Pacific",  9.7489, -83.7534, "n=3","Short-beaked",
  "Indian Ocean",  18, 55.9754, "n=1**","Long-beaked",
  "Senegal",     14.666667, -17.416667, "n=31","Unknown/Mixed",
  "Western North Atlantic",    38.9072, -76, "n=13","Short-beaked",
  "Western North Pacific",  30.0603, 120.5, "n=1*","Long-beaked",
  "Western North Pacific",  35.9078, 127.7669, "n=1*","Short-beaked",
  "Western South Pacific",    -40.9006, 174.8860, "n=2*","Short-beaked",
)

#make plot
map<- ggplot() + 
  geom_sf(data = World, fill=NA,show.legend = TRUE,colour="grey") + 
  geom_point(data = sample_sites, mapping = aes(x = Longitude, y = Latitude,colour=Location,shape=Beak),size=3)+
  geom_text(data = sample_sites, aes(x = Longitude, y = Latitude,label=Label),size=3.5, hjust = 0, nudge_x = 4,nudge_y=-3)+
  scale_shape_manual(values=c(17,16,15)) +
  scale_colour_viridis(discrete=TRUE,option="turbo")+
  theme(panel.background = element_rect(fill = "white"),axis.title.x=element_blank(),axis.title.y=element_blank()
          )


final <- ggdraw() +
  draw_plot(map)+
  draw_image("D:/Delphinus/Skulls_v2-transparent.png", x =-.365, y =-.108, scale = .22)

#save plot
ggsave(file="test.tiff", plot=final, width=10, height=8, dpi = 600)
