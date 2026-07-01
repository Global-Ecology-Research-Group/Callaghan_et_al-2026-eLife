# This script is used to create a single dataframe ("analysis_data.RDS")
# that is the match between urban scores and body size data
# it is the dataframe that will be pared down further and used for model fitting
# and for summarizing the data etc.
# will also do some manual processing here if needed

# It creates 2 dataframes, however, 1 of which is a harmonized version of taxonomy
# following on from the script: "R/cleaning_and_processing_files/taxonomic_harmonization.R"
# which creates two harmonized files based on taxonomic harmonization

# packages
library(tidyverse)
library(data.table)
library(taxize)
library(rgbif)
library(forcats)
library(ggplot2)


# read in all response variables (urban scores)
urban_scores <- readRDS("urban_scores/subrealm_potential_urban_scores.RDS") 

# read in all predictor variables (body size)
body_size <- readRDS("body_size_data_all/all_body_size.RDS")

# save all possible body size measures for a species
analysis_data <- urban_scores %>%
  left_join(., body_size, by=c("species", "kingdom")) %>%
  dplyr::filter(complete.cases(body_size)) %>%
  dplyr::filter(complete.cases(subrealm))

saveRDS(analysis_data, "analysis_data/analysis_data.RDS")

########################################################################
########################################################################
######### Now create a harmonized dataset 
########### this follows the "R/cleaning_and_processing_files/taxonomic_harmonization.R" script
############################
bodysize_plantae_harmonized <- readRDS("analysis_data/bodysize_plantae_harmonized.RDS")


# this df has the following items: 
# Taxon = the original input name from TRY, 
# scientificname = is the scientificname from the input Taxon, it's not the accepted name!
# speciesKey = the GBIF identifier for the accepted species name, you can simply do
# e.g., https://www.gbif.org/species/3092961 to find the species with the speciesKey
# species = contains the accepted species name
# and the rest speaks for itself
head(bodysize_plantae_harmonized)

# now create a new dataframe that matches all possible values from urban scores with body size
plants_urban_scores <- urban_scores %>%
  dplyr::filter(kingdom=="Plantae")

# first match the two we have already (urban scores and body size)
plants_first_match <- plants_urban_scores %>%
  left_join(., body_size, by=c("species", "kingdom")) %>%
  dplyr::filter(complete.cases(body_size)) %>%
  dplyr::filter(complete.cases(subrealm))

# how many species already match?
length(unique(plants_first_match$species))

# now get list of non-matching species
non_matches <- plants_urban_scores %>%
  dplyr::filter(! species %in% plants_first_match$species)

# now link the non-matches with 'taxon' name from the harmonization process
# important to note that if a species from our 'urban scores' (i.e., GBIF)
# has more than one match to a species from the metadata, then we randomly sample
# one of the 'matches'
plants_second_match <- non_matches %>%
  left_join(., bodysize_plantae_harmonized, by=c("species", "phylum")) %>%
  rename(species_gbif=species) %>%
  dplyr::filter(complete.cases(Taxon)) %>%
  rename(species=Taxon) %>%
  left_join(., body_size, by=c("species", "kingdom")) %>%
  dplyr::filter(complete.cases(body_size)) %>%
  dplyr::filter(complete.cases(subrealm)) %>%
  dplyr::select(-c(15:19)) %>%
  rename(species=species_gbif) %>%
  group_by(species, subrealm, metadata) %>% 
  sample_n(1) %>%
  ungroup()

# now join the two together to create a cleaned body size dataframe for plantae
analysis_data_plants <- plants_first_match %>%
  bind_rows(plants_second_match)

############################################################
############################################################
# Now for animals!
bodysize_animalia_harmonized <- readRDS("analysis_data/bodysize_animalia_harmonized.RDS")

# now create a new dataframe that matches all possible values from urban scores with body size
animals_urban_scores <- urban_scores %>%
  dplyr::filter(kingdom=="Animalia")

# first match the two we have already (urban scores and body size)
animals_first_match <- animals_urban_scores %>%
  left_join(., body_size, by=c("species", "kingdom")) %>%
  dplyr::filter(complete.cases(body_size)) %>%
  dplyr::filter(complete.cases(subrealm))

# how many species already match?
length(unique(animals_first_match$species))

# now get list of non-matching species
non_matches <- animals_urban_scores %>%
  dplyr::filter(! species %in% animals_first_match$species)

animals_second_match <- non_matches %>%
  left_join(., bodysize_animalia_harmonized, by=c("species", "phylum")) %>%
  rename(species_gbif=species) %>%
  dplyr::filter(complete.cases(Taxon)) %>%
  rename(species=Taxon) %>%
  left_join(., body_size, by=c("species", "kingdom")) %>%
  dplyr::filter(complete.cases(body_size)) %>%
  dplyr::filter(complete.cases(subrealm)) %>%
  dplyr::select(-c(15:19)) %>%
  rename(species=species_gbif) %>%
  group_by(species, subrealm, metadata) %>% 
  sample_n(1) %>%
  ungroup()

# now join the two together to create a cleaned body size dataframe for animalia
analysis_data_animals <- animals_first_match %>%
  bind_rows(animals_second_match)

# now join back together the two datasets of harmonized values
analysis_data_harmonized <- analysis_data_plants %>%
  bind_rows(analysis_data_animals)

# write out as a separate dataframe
saveRDS(analysis_data_harmonized, "analysis_data/analysis_data_harmonized.RDS")

##########################################################################
##########################################################################
# can investigate the species that were 'retreived' through harmonization
harmonized_sp <- analysis_data_harmonized %>%
  dplyr::filter(! species %in% analysis_data$species)

length(unique(harmonized_sp$species))

harmonized_sp %>%
  group_by(class) %>%
  summarize(N=length(unique(species))) %>%
  dplyr::filter(complete.cases(.)) %>%
  arrange(N) %>%
  ggplot(., aes(x=fct_inorder(class), y=N))+
  geom_col()+
  coord_flip()+
  xlab("")+
  ylab("Number of species gained through harmonization")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))

ggsave("Figures/species_gained_through_harmonization.png", width=5.5, height=6.7, units="in")

