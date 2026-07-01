# packages
library(ggplot2)
library(sf)
library(ggsflabel)

# read in a map of subrealms
subrealm_map <- invisible(st_read("Data/OE_subrealms/OE_subrealms.shp"))


ggplot()+
  geom_sf(data=subrealm_map, fill="gray80", color="black")+
  theme_bw()+
  geom_sf_label_repel(data=subrealm_map, aes(label=subrealm), box.padding=0.1, force=100, nudge_y=7, max.overlaps=19, seed=10, size=1.8)+
  theme(axis.text=element_blank())+
  theme(axis.title=element_blank())+
  theme(legend.position="none")+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  ggtitle("Map of One Earth subrealms (N=52)")

ggsave("Figures/map_of_subrealms_with_labels.png", width=8, height=7.8, units="in")
