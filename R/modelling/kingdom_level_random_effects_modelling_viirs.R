# R script to model 
# From Diana: "This one asks whether there are mean differences between plants and animals.
# I think it would be relevant since this cross-kingdom bit is a key part of our novelty.

# read in the analysis dataframe
# this was made using the "R/cleaning_and_processing_files/create_a_dataframe_for_analysis.R"
analysis_data <- readRDS("analysis_data/analysis_data_harmonized.RDS")

# source some global helper functions
source("R/global_functions.R")

# packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(brms)

# first calculate the number of kingdoms with >2 species in a subrealm
# this seems like a logical cutoff in that we want at least 2 species in a subrealm fo
# form a slope
kingdom_subrealm <- analysis_data %>%
  group_by(kingdom, subrealm) %>%
  summarize(number_species_sub_kingdom=n()) %>%
  dplyr::filter(number_species_sub_kingdom>=2)


# Now improve on that simplistic strategy initially employed
# of randomly sampling 1 body size measure per species
# to get the species which has the greatest 'metadata' included
# to give more credence to the specific metadata that is most popular
# in the most extreme case, this will knock out one of the random effects
# in the modelling exercise
body_size_summarized <- analysis_data %>%
  ungroup() %>%
  unite(kingdom_sub, kingdom, subrealm, sep="_", remove=FALSE) %>%
  dplyr::filter(kingdom_sub %in% local(kingdom_subrealm %>%
                                         unite(kingdom_sub, kingdom, subrealm, sep="_", remove=FALSE) %>%
                                         .$kingdom_sub)) %>%
  unite(metadata2, type, units, measurement_detail, sep="_", remove=FALSE) %>%
  group_by(kingdom, metadata2) %>%
  summarize(N=n())

# get metadata sample size
metadata_sample <- analysis_data %>%
  group_by(metadata) %>%
  summarize(number_of_samples_from_metadata=n())

# write a quick function
# to make a 'new' analysis data that selects the most numerous option
# for each species in a family
select_body_size_function <- function(species_name){
  
  tmp <- analysis_data %>%
    ungroup() %>%
    dplyr::filter(species==species_name) %>%
    unite(metadata2, type, units, measurement_detail, sep="_", remove=FALSE) %>%
    left_join(., body_size_summarized) %>%
    left_join(., metadata_sample) %>%
    arrange(desc(N), desc(number_of_samples_from_metadata)) %>%
    slice(1) %>%
    dplyr::select(-N) %>%
    dplyr::select(-number_of_samples_from_metadata) %>%
    dplyr::select(kingdom, species, body_size, type, units, measurement_detail, data_id, metadata)
  
  return(tmp)
  
}

# apply this function to get a 'new' analysis data dataframe
body_size_collapsed <- bind_rows(lapply(unique(analysis_data$species), select_body_size_function))

# now create an analysis dataframe
# that gets all the relevant data and only 1 body size value for
# each species
analysis_data <- analysis_data %>%
  dplyr::select(1:14) %>%
  distinct() %>%
  left_join(., body_size_collapsed)

# now summarize the data at the kingdom level
# first filter out based on the data above regarding 2 species in  kingdom subrealm
# number of kingdomes with >10 species
# assign four 'model types' based on the critera for fitting random effects
# type 1: random effect for subrealm and random effect for dataset
# type 2: no random effects
# type 3: random effect for subrealm no dataset inclusion
# type 4: random effect for dataset no subrealm inclusion
kingdom_summary <- analysis_data %>%
  ungroup() %>%
  unite(kingdom_sub, kingdom, subrealm, sep="_", remove=FALSE) %>%
  dplyr::filter(kingdom_sub %in% local(kingdom_subrealm %>%
                                         unite(kingdom_sub, kingdom, subrealm, sep="_", remove=FALSE) %>%
                                         .$kingdom_sub)) %>%
  unite(metadata2, type, units, measurement_detail, sep="_", remove=FALSE) %>%
  group_by(kingdom) %>%
  summarize(number_species=length(unique(species)),
            number_of_obs=n(),
            number_of_subrealms=length(unique(subrealm)),
            number_of_datasets=length(unique(metadata2))) %>%
  mutate(model_type=case_when(number_species<10 ~ "no model",
                              number_species>=10 & number_of_subrealms>=2 & number_of_datasets>=2 ~ "type 1",
                              number_species>=10 & number_of_subrealms==1 & number_of_datasets==1 ~ "type 2",
                              number_species>=10 & number_of_subrealms>=2 & number_of_datasets==1 ~ "type 3",
                              number_species>=10 & number_of_subrealms==1 & number_of_datasets>=2 ~ "type 4"))  %>%
  dplyr::filter(model_type!="no model")

# Now trim the dataset to a filtered down dataset
# based on the above
analysis_data2 <- analysis_data %>%
  ungroup() %>%
  unite(kingdom_sub, kingdom, subrealm, sep="_", remove=FALSE) %>%
  dplyr::filter(kingdom_sub %in% local(kingdom_subrealm %>%
                                         unite(kingdom_sub, kingdom, subrealm, sep="_", remove=FALSE) %>%
                                         .$kingdom_sub)) %>%
  unite(metadata2, type, units, measurement_detail, sep="_", remove=FALSE) %>%
  dplyr::filter(complete.cases(mean_urban_score_viirs)) %>%
  group_by(metadata2) %>%
  mutate(body_size_scaled_log10=scale(log10(body_size))) %>%
  ungroup()

animalia_dat <- analysis_data2 %>%
  dplyr::filter(kingdom=="Animalia")

plantae_dat <- analysis_data2 %>%
  dplyr::filter(kingdom=="Plantae")

################################
# set a prior before modelling
myprior <- c(set_prior("normal(0,1)", class = "b"))

# tests the difference in the slopes
# on average, accounting for inherent differences of body size among taxonomic groups
# what does body size affect urban tolerance
brm_big_mod_1 <- brms::brm(mean_urban_score_viirs ~ body_size_scaled_log10 * kingdom + (1 + body_size_scaled_log10|class/order/family) + (1 + body_size_scaled_log10| metadata2) + (1 | subrealm),
                          data=analysis_data2,
                          warmup=1000,
                          iter=6000,
                          chains=4,
                          prior=myprior,
                          cores=6,
                          control=list(adapt_delta=0.99),
                          file=paste0("model_objects/kingdom_level/animals_and_plants_RE_viirs_model_v1"))


# does not test the difference, but estimates the slopes
brm_big_mod_2 <- brms::brm(mean_urban_score_viirs ~  kingdom + body_size_scaled_log10:kingdom + (1 + body_size_scaled_log10|class/order/family) + (1 + body_size_scaled_log10| metadata2) + (1 | subrealm),
                           data=analysis_data2,
                           warmup=1000,
                           iter=6000,
                           chains=4,
                           prior=myprior,
                           cores=6,
                           control=list(adapt_delta=0.99),
                           file=paste0("model_objects/kingdom_level/animals_and_plants_RE_viirs_model_v2"))





