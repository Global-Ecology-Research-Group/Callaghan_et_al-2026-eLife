# this script is to visualize the subrealms where we have data from
# a supplementary figure for the paper

# packages
library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(sf)
library(forcats)
library(tidyr)
library(patchwork)

# read in all response variables (urban scores)
urban_scores <- readRDS("urban_scores/subrealm_potential_urban_scores.RDS")

# read in a map of subrealms
subrealm_map <- invisible(st_read("Data/OE_subrealms/OE_subrealms.shp"))

# make a map of number of species per subrealm
animalia_map_dat <- subrealm_map %>%
  left_join(., urban_scores %>%
              group_by(subrealm, kingdom) %>%
              summarize(number_species=length(unique(species))) %>%
              dplyr::filter(kingdom=="Animalia")) %>%
  ungroup() %>%
  replace_na(list(kingdom="Animalia", number_species=0))

animals_map <- ggplot()+
  geom_sf(data=animalia_map_dat, aes(fill=log10(number_species)))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~kingdom, ncol=1)+
  scale_fill_viridis_c(option="inferno", name="Number of species:",
                       breaks=c(1, 2, 3), labels=c(10^1, 10^2, 10^3))+
  theme(legend.position="none")+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())


# make a map of number of species per subrealm
plantae_map_dat <- subrealm_map %>%
  left_join(., urban_scores %>%
              group_by(subrealm, kingdom) %>%
              summarize(number_species=length(unique(species))) %>%
              dplyr::filter(kingdom=="Plantae")) %>%
  ungroup() %>%
  replace_na(list(kingdom="Plantae", number_species=0))

plants_map <- ggplot()+
  geom_sf(data=plantae_map_dat, aes(fill=log10(number_species)))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~kingdom, ncol=1)+
  scale_fill_viridis_c(option="inferno", name="Number of species:",
                       breaks=c(1, 2, 3), labels=c(10^1, 10^2, 10^3))+
  theme(legend.position="right")+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())

animals_map + plants_map + plot_layout(ncol=1, guides = "collect")

ggsave("Figures/sample_size_of_urban_tolerance_scores_by_subrealm.png", height=7.6, width=6.8, units="in")


