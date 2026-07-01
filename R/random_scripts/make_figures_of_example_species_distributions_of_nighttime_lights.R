# make distributions of some species
# and their nighttime lights for a supporting information figure


# packages
library(tidyverse)
library(ggplot2)



# the following query was used in Google BigQuery to extract the 'raw data'
# compared to the 'processed data' that was used elsewhere in the repository
# WITH annotated AS (SELECT 
#                    subrealm,
#                    geohash7,
#                    CAST(
#                      NULLIF(NULLIF(viirs, 'NA'), 'viirs')
#                      as numeric) as viirs
#                    FROM `gbif-data.gbif_2021_02_22.gbif_geohash7_assigned` 
# ), 
# gbif_species_in_geohash as (
#   SELECT species, 
#   ST_GEOHASH(ST_GEOGPOINT(decimalLongitude, decimalLatitude), 7) as geohash7
#   FROM `gbif-data.gbif_2021_02_22.gbif_occurrence`
#   WHERE year>=2010
#   AND coordinateUncertaintyInMeters <= 1000
#   AND NOT(((`species`) IS NULL))
# ),
# species_in_geohash_count as (
#   SELECT species, geohash7, count(geohash7) as recorded_count
#   FROM gbif_species_in_geohash
#   WHERE species in ("Gasteracantha cancriformis", "Setophaga magnolia", "Eurytides marcellus", "Osmia aurulenta", "Ophrys scolopax")
#   GROUP BY species, geohash7
# ), 
# joined as (
#   SELECT 
#   annotated.subrealm, 
#   species_in_geohash_count.species,
#   species_in_geohash_count.recorded_count,
#   annotated.viirs
#   FROM annotated, 
#   species_in_geohash_count 
#   WHERE annotated.geohash7 = species_in_geohash_count.geohash7
# )
# SELECT
# *
#   FROM joined




# read data in
# raw observation data from BigQuery, as described above
observation_dat <- read_csv("Data/example_full_data_for_some_species/bquxjob_5748d5b3_18e1acc4755.csv")

# subrealm summary
subrealm_dat_viirs <- read_csv("sql_processed_data/subrealm_level_summary_viirs.csv") %>%
  dplyr::filter(complete.cases(.)) %>%
  rename(sub_viirs_avg=viirs_avg)

# make figure for Magnolia Warbler
mawa <- observation_dat %>%
  dplyr::filter(species=="Setophaga magnolia") %>%
  group_by(subrealm) %>%
  mutate(number_records=n()) %>%
  dplyr::filter(number_records>=100)

mawa_mean <- mawa %>%
  group_by(subrealm) %>%
  summarize(mean_viirs=mean(viirs))

mawa_subrealm_mean <- subrealm_dat_viirs %>%
  dplyr::filter(subrealm %in% mawa$subrealm)

mawa_urban_scores <- urban_scores %>%
  dplyr::filter(species=="Setophaga magnolia")

ggplot(mawa, aes(x=viirs))+
  geom_density(fill="gray80")+
  scale_x_log10()+
  facet_wrap(~subrealm, scales="free", ncol=2)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(panel.grid=element_blank())+
  geom_vline(data=mawa_subrealm_mean, aes(xintercept=sub_viirs_avg, color="Overall mean subrealm viirs"), linetype="dashed") +
  geom_vline(data=mawa_mean, aes(xintercept=mean_viirs, color="Magnolia Warbler mean viirs"), linetype="dashed") +
  geom_vline(data=mawa_urban_scores, aes(xintercept=mean_urban_score_viirs, color="Magnolia Warbler urban score"), linetype="dashed") +
  scale_color_manual(values=c("Overall mean subrealm viirs"="purple", "Magnolia Warbler mean viirs"="green", "Magnolia Warbler urban score"="gold")) +
  guides(color=guide_legend(title="Legend:"))+
  theme(legend.position="bottom")+
  labs(x=expression("Average radiance (nW cm"^"-2"~"sr"^"-1"~")"),
       y="Density")

ggsave("Figures/MAWA_example_viirs_distribution.png", height=6, width=8, units='in')








# Now make another figure where multiple species are shown for the same subrealm
# another way to visualize an example of data
# read data in
gef_dat <- read_csv("Data/example_full_data_for_some_species/bquxjob_8a59d87_18e2f00e674.csv") %>%
  bind_rows(read_csv("Data/example_full_data_for_some_species/bquxjob_5d8c0912_18e2effb562.csv")) %>%
  dplyr::filter(subrealm=="Great European Forests") %>%
  group_by(species) %>%
  mutate(N=n()) %>%
  dplyr::filter(N>=100)

subrealm_mean <- subrealm_dat_viirs %>%
  dplyr::filter(subrealm=="Great European Forests")

species_urban_scores <- urban_scores %>%
  dplyr::filter(subrealm=="Great European Forests") %>%
  dplyr::filter(species %in% unique(gef_dat$species))

species_mean <- gef_dat %>%
  group_by(subrealm, species) %>%
  summarize(mean_viirs=mean(viirs))

ggplot(gef_dat, aes(x=viirs, fill=species))+
  geom_density()+
  scale_fill_brewer(palette="Pastel1")+
  scale_x_log10()+
  facet_wrap(~species, scales="free", ncol=2)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(panel.grid=element_blank())+
  geom_vline(data=subrealm_mean, aes(xintercept=sub_viirs_avg, color="Overall mean subrealm viirs"), linetype="dashed") +
  geom_vline(data=species_mean, aes(xintercept=mean_viirs, color="Species-specific mean viirs"), linetype="dashed") +
  #geom_vline(data=species_urban_scores, aes(xintercept=mean_urban_score_viirs, color="Species-specific urban score"), linetype="dashed") +
  theme(legend.position="bottom")+
  theme(legend.position="bottom",
        legend.text = element_text(size = 8), # Adjust size as needed
        legend.title = element_text(size = 9), # Adjust size as needed
        legend.text.align = 0.5,
        legend.key.size = unit(0.5, 'cm'), # Adjust key size as needed
        legend.margin = margin(0,0,0,0))+
  guides(color=guide_legend(title="Legend:"),
         fill=guide_legend(title="Species:"))+
  theme(strip.text = element_text(face = "italic"))+
  labs(x=expression("Average radiance (nW cm"^"-2"~"sr"^"-1"~")"),
       y="Density", 
       title="Great European Forests")+
  geom_text(data=species_urban_scores, aes(label=paste("Urban tolerance =", round(mean_urban_score_viirs, digits=3)), x=Inf, y=Inf, hjust=1.1, vjust=1.1), color="black", size=3.5)

ggsave("Figures/great_european_forests_example_distributions.png", height=8, width=9, units='in')



