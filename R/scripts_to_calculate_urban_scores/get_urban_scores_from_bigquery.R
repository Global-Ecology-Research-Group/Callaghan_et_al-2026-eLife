# calculate urban scores for every gbif species
# in big query
# first using the viirs approach
# and then using the ghm approach


# get data from eBird
## packages
library(readr)
library(bigrquery)
library(dbplyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)

# first get a list of species from GBIF
# create connection with online database
con <- DBI::dbConnect(bigrquery::bigquery(),
                      dataset= "gbif_2021_02_22",
                      project="gbif-data",
                      billing="gbif-data")

# create a table of distinct species/family/class/etc. to link up with all the species names
gbif <- tbl(con, 'gbif_occurrence')

species_key <- gbif %>%
  dplyr::select(gbifid, kingdom, phylum, class, order, family, genus, species,
                coordinateUncertaintyInMeters, decimalLatitude, decimalLongitude,
                year, eventDate) %>%
  dplyr::filter(!is.na(species)) %>%
  dplyr::filter(year>=2010) %>%
  dplyr::filter(coordinateUncertaintyInMeters<=1000) %>%
  dplyr::select(kingdom, phylum, class, order, family, genus, species) %>%
  distinct() %>%
  collect(N=Inf)

# now read in viirs dat for bioregions
bioregion_dat_viirs <- read_csv("sql_processed_data/bioregion_level_summary_viirs.csv") %>%
  dplyr::filter(complete.cases(.)) %>%
  rename(bio_viirs_avg=viirs_avg)

species_dat_viirs <- read_csv("sql_processed_data/species_level_summary_bioregion_viirs.csv")

length(unique(species_dat_viirs$species))
sum(species_dat_viirs$recorded_count)


potential_dat_viirs <- species_key %>%
  left_join(., species_dat_viirs, by="species") %>%
  dplyr::filter(recorded_count>=100) %>%
  left_join(., bioregion_dat_viirs, by="BIOREGION_NAME") %>%
  dplyr::filter(kingdom %in% c("Animalia", "Plantae")) %>%
  group_by(class) %>%
  mutate(N_class_sp=n()) %>%
  dplyr::filter(N_class_sp>=10) %>%
  mutate(median_urban_score_viirs=f3_-viirs_median) %>%
  mutate(mean_urban_score_viirs=viirs_avg-bio_viirs_avg) %>%
  dplyr::select(1:8, 14, 19:21)

# now read in ghm dat for bioregions
bioregion_dat_ghm <- read_csv("sql_processed_data/bioregion_level_summary_ghm.csv") %>%
  dplyr::filter(complete.cases(.)) %>%
  rename(bio_ghm_avg=ghm_avg)

species_dat_ghm <- read_csv("sql_processed_data/species_level_summary_bioregion_ghm.csv")

length(unique(species_dat_ghm$species))
sum(species_dat_ghm$recorded_count)


potential_dat_ghm <- species_key %>%
  left_join(., species_dat_ghm, by="species") %>%
  dplyr::filter(recorded_count>=100) %>%
  left_join(., bioregion_dat_ghm, by="BIOREGION_NAME") %>%
  dplyr::filter(kingdom %in% c("Animalia", "Plantae")) %>%
  group_by(class) %>%
  mutate(N_class_sp=n()) %>%
  dplyr::filter(N_class_sp>=10) %>%
  mutate(ghm_median=as.numeric(as.character(ghm_median))) %>%
  mutate(bio_ghm_avg=as.numeric(as.character(bio_ghm_avg))) %>%
  mutate(median_urban_score_ghm=f3_-ghm_median) %>%
  mutate(mean_urban_score_ghm=ghm_avg-bio_ghm_avg) %>%
  dplyr::select(1:8, 14, 19:21)

potential_dat_bioregion <- potential_dat_viirs %>%
  left_join(., potential_dat_ghm) %>%
  # get rid of 'hybrids' that are classified as 'species' in GBIF
  mutate(words=stringr::str_count(species, '\\w+')) %>%
  dplyr::filter(words <= 3) %>%
  dplyr::select(-words)

# get rid of species that appear for both kingdoms
species_that_double <- potential_dat_bioregion %>%
  group_by(species) %>%
  summarize(kingdoms=length(unique(kingdom))) %>%
  dplyr::filter(kingdoms==2)

potential_dat_bioregion <- potential_dat_bioregion %>%
  dplyr::filter(species != species_that_double$species) %>%
  dplyr::filter(species != "Isothecium myosuroides")

saveRDS(potential_dat_bioregion, "urban_scores/bioregion_potential_urban_scores.RDS")

#######################################################
#######################################################
############# repeat the above but for subrealms ######
# now read in viirs dat for subrealms
subrealm_dat_viirs <- read_csv("sql_processed_data/subrealm_level_summary_viirs.csv") %>%
  dplyr::filter(complete.cases(.)) %>%
  rename(sub_viirs_avg=viirs_avg)

species_dat_viirs <- read_csv("sql_processed_data/species_level_summary_subrealm_viirs.csv")

length(unique(species_dat_viirs$species))
sum(species_dat_viirs$recorded_count)


potential_dat_viirs <- species_key %>%
  left_join(., species_dat_viirs, by="species") %>%
  dplyr::filter(recorded_count>=100) %>%
  left_join(., subrealm_dat_viirs, by="subrealm") %>%
  dplyr::filter(kingdom %in% c("Animalia", "Plantae")) %>%
  group_by(class) %>%
  mutate(N_class_sp=n()) %>%
  dplyr::filter(N_class_sp>=10) %>%
  mutate(median_urban_score_viirs=f3_-viirs_median) %>%
  mutate(mean_urban_score_viirs=viirs_avg-sub_viirs_avg) %>%
  dplyr::select(1:8, 14, 19:21)

# now read in ghm dat for bioregions
subrealm_dat_ghm <- read_csv("sql_processed_data/subrealm_level_summary_ghm.csv") %>%
  dplyr::filter(complete.cases(.)) %>%
  rename(sub_ghm_avg=ghm_avg)

species_dat_ghm <- read_csv("sql_processed_data/species_level_summary_subrealm_ghm.csv")

length(unique(species_dat_ghm$species))
sum(species_dat_ghm$recorded_count)


potential_dat_ghm <- species_key %>%
  left_join(., species_dat_ghm, by="species") %>%
  dplyr::filter(recorded_count>=100) %>%
  left_join(., subrealm_dat_ghm, by="subrealm") %>%
  dplyr::filter(kingdom %in% c("Animalia", "Plantae")) %>%
  group_by(class) %>%
  mutate(N_class_sp=n()) %>%
  dplyr::filter(N_class_sp>=10) %>%
  mutate(ghm_median=as.numeric(as.character(ghm_median))) %>%
  mutate(sub_ghm_avg=as.numeric(as.character(sub_ghm_avg))) %>%
  mutate(median_urban_score_ghm=f3_-ghm_median) %>%
  mutate(mean_urban_score_ghm=ghm_avg-sub_ghm_avg) %>%
  dplyr::select(1:8, 14, 19:21)

potential_dat_subrealm <- potential_dat_viirs %>%
  left_join(., potential_dat_ghm) %>%
  # get rid of 'hybrids' that are classified as 'species' in GBIF
  mutate(words=stringr::str_count(species, '\\w+')) %>%
  dplyr::filter(words <= 3) %>%
  dplyr::select(-words)

# get rid of species that appear for both kingdoms
species_that_double <- potential_dat_subrealm %>%
  group_by(species) %>%
  summarize(kingdoms=length(unique(kingdom))) %>%
  dplyr::filter(kingdoms==2)

potential_dat_subrealm <- potential_dat_subrealm %>%
  dplyr::filter(species != species_that_double$species) %>%
  dplyr::filter(species != "Isothecium myosuroides")

# check to make sure the "genus" matches the genus of the species name
# this is because I found a mistake in the GBIF for plants
# the species "Frasera speciosa" was listed as calculated the same urban tolerance scores
# but listed with two different genera
# this was found on May 30th, 2023.
# turns out that 89 observations had this problem of 65 species
# by doing this taxonomic check it helps problems downstream

urban_scores2 <- potential_dat_subrealm %>%
  mutate(genus_2=stringr::str_extract(species, '[A-Za-z]+')) %>%
  mutate(test=genus_2==genus)

urban_scores3 <- urban_scores2 %>%
  dplyr::filter(test=="TRUE") %>%
  dplyr::select(-test) %>%
  dplyr::select(-genus_2)

saveRDS(urban_scores3, "urban_scores/subrealm_potential_urban_scores.RDS")
