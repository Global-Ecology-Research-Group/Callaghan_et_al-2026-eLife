# this script is fitting models at the family level
# where the urban scores were calculated using the viirs night-time lights
# it mirrors the script "R/modelling/family_level_modelling_ghm.R"

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

# first calculate the number of families with >2 species in a subrealm
# this seems like a logical cutoff in that we want at least 2 species in a subrealm to
# form a slope
family_subrealm <- analysis_data %>%
  group_by(family, subrealm) %>%
  summarize(number_species_sub_family=length(unique(species))) %>%
  dplyr::filter(number_species_sub_family>=2)


# Now improve on that simplistic strategy initially employed
# of randomly sampling 1 body size measure per species
# to get the species which has the greatest 'metadata' included
# to give more credence to the specific metadata that is most popular
# in the most extreme case, this will knock out one of the random effects
# in the modelling exercise
body_size_summarized <- analysis_data %>%
  ungroup() %>%
  unite(family_sub, family, subrealm, sep="_", remove=FALSE) %>%
  dplyr::filter(family_sub %in% local(family_subrealm %>%
                                        unite(family_sub, family, subrealm, sep="_", remove=FALSE) %>%
                                        .$family_sub)) %>%
  unite(metadata2, type, units, measurement_detail, sep="_", remove=FALSE) %>%
  group_by(family, metadata2) %>%
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

# now summarize the data at the family level
# first filter out based on the data above regarding 2 species in  family subrealm
# number of families with >10 species
# assign four 'model types' based on the critera for fitting random effects
# type 1: random effect for subrealm and random effect for dataset
# type 2: no random effects
# type 3: random effect for subrealm no dataset inclusion
# type 4: random effect for dataset no subrealm inclusion
family_summary <- analysis_data %>%
  ungroup() %>%
  unite(family_sub, family, subrealm, sep="_", remove=FALSE) %>%
  dplyr::filter(family_sub %in% local(family_subrealm %>%
                  unite(family_sub, family, subrealm, sep="_", remove=FALSE) %>%
                  .$family_sub)) %>%
  unite(metadata2, type, units, measurement_detail, sep="_", remove=FALSE) %>%
  group_by(family, order, class, phylum, kingdom) %>%
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

saveRDS(family_summary, "analysis_data/summary_family.RDS")

# Now trim the dataset to a filtered down dataset
# based on the above
analysis_data2 <- analysis_data %>%
  ungroup() %>%
  unite(family_sub, family, subrealm, sep="_", remove=FALSE) %>%
  dplyr::filter(family_sub %in% local(family_subrealm %>%
                                     unite(family_sub, family, subrealm, sep="_", remove=FALSE) %>%
                                     .$family_sub)) %>%
  unite(metadata2, type, units, measurement_detail, sep="_", remove=FALSE) %>%
  dplyr::filter(complete.cases(mean_urban_score_viirs))

saveRDS(analysis_data2, "analysis_data/final_data_for_analysis_family.RDS")

analysis_data2 %>%
  group_by(metadata) %>%
  summarize(N=n()) %>%
  mutate(model_type="family_viirs") %>%
  saveRDS(., "intermediate_results/model_metadata_usage_summary/metadata_summary_family_viirs.RDS")

# now a function to model every family possible
# and save the model results out
# as a file
family_level_modelling_function <- function(family_name){
  
  message(paste0("Modelling ", family_name))
  
  # filter to data
  dat <- analysis_data2 %>%
    dplyr::filter(family==family_name) %>%
    left_join(., family_summary %>%
                dplyr::select(family, model_type) %>%
                distinct()) %>%
    ungroup() %>%
    group_by(metadata2) %>%
    mutate(body_size_scaled_log10=scale(log10(body_size)))
  
  ggplot(dat, aes(x=body_size, y=mean_urban_score_viirs, color=metadata2))+
    geom_point()+
    scale_x_log10()+
    theme_bw()+
    theme(axis.text=element_text(color="black"))
  
  ggplot(dat, aes(x=body_size_scaled_log10, y=mean_urban_score_viirs, color=metadata2))+
    geom_point()+
    theme_bw()+
    theme(axis.text=element_text(color="black"))
  
  # set a prior before modelling
  myprior <- c(set_prior("normal(0,1)", class = "b"))
  
  # now run through the different model types
  # and then save an RDS of the model fit as part of the brms call
  if(unique(dat$model_type)=="type 1"){
    
    brms::brm(mean_urban_score_viirs ~ body_size_scaled_log10 + (1 + body_size_scaled_log10|subrealm) + (1|metadata2),
              data=dat,
              warmup=1000,
              iter=6000, 
              chains=4,
              prior=myprior,
              control=list(adapt_delta=0.99),
              file=paste0("model_objects/family_level/", family_name, "_viirs_type_1"))
    
  } else if(unique(dat$model_type)=="type 2"){
    
    brms::brm(mean_urban_score_viirs ~ body_size_scaled_log10,
              data=dat,
              warmup=1000,
              iter=6000, 
              chains=4,
              prior=myprior,
              control=list(adapt_delta=0.99),
              file=paste0("model_objects/family_level/", family_name, "_viirs_type_2"))
    
  } else if(unique(dat$model_type)=="type 3"){
    
    brms::brm(mean_urban_score_viirs ~ body_size_scaled_log10 + (1 + body_size_scaled_log10|subrealm),
              data=dat,
              warmup=1000,
              iter=6000, 
              chains=4,
              prior=myprior,
              control=list(adapt_delta=0.99),
              file=paste0("model_objects/family_level/", family_name, "_viirs_type_3"))
    
  } else if(unique(dat$model_type)=="type 4"){
    
    brms::brm(mean_urban_score_viirs ~ body_size_scaled_log10 + (1|metadata2),
              data=dat,
              warmup=1000,
              iter=6000, 
              chains=4,
              prior=myprior,
              control=list(adapt_delta=0.99),
              file=paste0("model_objects/family_level/", family_name, "_viirs_type_4"))
    
  }
}

# now apply this modelling function over every family
# but will use an lapply with error
lapply_with_error(family_summary$family, family_level_modelling_function)





