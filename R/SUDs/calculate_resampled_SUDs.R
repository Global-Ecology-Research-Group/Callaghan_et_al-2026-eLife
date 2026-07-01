# This script is to assess the difference in 
# skewness of a SUD (species urbanness distribution)
# compared to a 'null model'
# using the simplest null model which is that of
# random sampling
# It currently loops through the different subrealms
# resamples a SUD
# and saves out the urban scores from that resampled SUD for
# later processing

# packages
library(readr)
library(bigrquery)
library(dbplyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(ggplot2)

# resample SUDS function
resample_SUD <- function(subrealm_name){
  
  # subrealm ALL geohash7 records
  sub_dat <- readRDS("Data/gbif_geohash_assigned.RDS") %>%
    dplyr::filter(subrealm==subrealm_name) %>%
    dplyr::filter(complete.cases(.))
  
  # read in data for our urban scores
  urban_scores <- readRDS("urban_scores/subrealm_potential_urban_scores.RDS") %>%
    dplyr::filter(subrealm==subrealm_name)
  
  ###########################################################################
  ###########################################################################
  # resampling function 1 (all observation level pixels)
  ###########################################################################
  ###########################################################################
  boot_resampling <- function(draw_number){
    
    message(paste0("computing draw number ", draw_number, " for function 1; ", subrealm_name))
    
    random_sample_function <- function(species_name){
      
      sp <- urban_scores %>%
        dplyr::filter(species==species_name) %>%
        slice(1)
      
      fake_sp <- sub_dat %>%
        sample_n(sp$recorded_count, replace=TRUE) %>%
        mutate(sp_id=species_name)
      
      return(fake_sp)
      
    }
    
    # now generate a pseudo sampling dataframe where 
    # each species is matched with a 'pseudo sampled' species
    resampled_null_data <- bind_rows(lapply(unique(urban_scores$species), random_sample_function))
    
    
    # now calculate the distribution if all species are randomly using urban areas
    random_scores <- resampled_null_data %>%
      group_by(sp_id) %>%
      summarize(mean_urban_score_viirs=mean(viirs, na.rm=TRUE)) %>%
      mutate(all_sampled_viirs=mean(resampled_null_data$viirs, na.rm=TRUE)) %>%
      mutate(mean_urban_score_viirs=mean_urban_score_viirs-all_sampled_viirs) %>%
      mutate(draw=draw_number)
    
    return(random_scores)
    
  }
  
  resampled_urban_distribution <- bind_rows(lapply(c(1:100), boot_resampling)) %>%
    mutate(analysis_type="all observations")
  
  saveRDS(resampled_urban_distribution, paste0("Results/SUDs/", subrealm_name, "_all.RDS"))
  
  ###########################################################################
  ###########################################################################
  # resampling function 2 (removing >0.5 quantile of urbanness)
  ###########################################################################
  ###########################################################################
  boot_resampling_2 <- function(draw_number){
    
    message(paste0("computing draw number ", draw_number, " for function 2; ", subrealm_name))
    
    random_sample_function <- function(species_name){
      
      sp <- urban_scores %>%
        dplyr::filter(species==species_name) %>%
        slice(1)
      
      fake_sp <- sub_dat %>%
        dplyr::filter(viirs<=quantile(viirs, 0.5)) %>%
        sample_n(sp$recorded_count, replace=TRUE) %>%
        mutate(sp_id=species_name)
      
      return(fake_sp)
      
    }
    
    # now generate a pseudo sampling dataframe where 
    # each species is matched with a 'pseudo sampled' species
    resampled_null_data <- bind_rows(lapply(unique(urban_scores$species), random_sample_function))
    
    
    # now calculate the distribution if all species are randomly using urban areas
    random_scores <- resampled_null_data %>%
      group_by(sp_id) %>%
      summarize(mean_urban_score_viirs=mean(viirs, na.rm=TRUE)) %>%
      mutate(all_sampled_viirs=mean(resampled_null_data$viirs, na.rm=TRUE)) %>%
      mutate(mean_urban_score_viirs=mean_urban_score_viirs-all_sampled_viirs) %>%
      mutate(draw=draw_number)
    
    return(random_scores)
    
  }
  
  resampled_urban_distribution_2 <- bind_rows(lapply(c(1:100), boot_resampling_2)) %>%
    mutate(analysis_type="only bottom 50% of observations")
  
  saveRDS(resampled_urban_distribution_2, paste0("Results/SUDs/", subrealm_name, "_less_than_0.5.RDS"))
  
  
  ###########################################################################
  ###########################################################################
  # resampling function 3 (removing <0.5 quantile of urbanness)
  ###########################################################################
  ###########################################################################
  boot_resampling_3 <- function(draw_number){
    
    message(paste0("computing draw number ", draw_number, " for function 3; ", subrealm_name))
    
    random_sample_function <- function(species_name){
      
      sp <- urban_scores %>%
        dplyr::filter(species==species_name) %>%
        slice(1)
      
      fake_sp <- sub_dat %>%
        dplyr::filter(viirs>=quantile(viirs, 0.5)) %>%
        sample_n(sp$recorded_count, replace=TRUE) %>%
        mutate(sp_id=species_name)
      
      return(fake_sp)
      
    }
    
    # now generate a pseudo sampling dataframe where 
    # each species is matched with a 'pseudo sampled' species
    resampled_null_data <- bind_rows(lapply(unique(urban_scores$species), random_sample_function))
    
    
    # now calculate the distribution if all species are randomly using urban areas
    random_scores <- resampled_null_data %>%
      group_by(sp_id) %>%
      summarize(mean_urban_score_viirs=mean(viirs, na.rm=TRUE)) %>%
      mutate(all_sampled_viirs=mean(resampled_null_data$viirs, na.rm=TRUE)) %>%
      mutate(mean_urban_score_viirs=mean_urban_score_viirs-all_sampled_viirs) %>%
      mutate(draw=draw_number)
    
    return(random_scores)
    
  }
  
  resampled_urban_distribution_3 <- bind_rows(lapply(c(1:100), boot_resampling_3)) %>%
    mutate(analysis_type="only top 50% of observations")
  
  saveRDS(resampled_urban_distribution_3, paste0("Results/SUDs/", subrealm_name, "_greater_than_0.5.RDS"))
  
}

# now get a list of all subrealms
urban_scores <- readRDS("urban_scores/subrealm_potential_urban_scores.RDS") %>%
  group_by(subrealm) %>%
  summarize(number_species=length(unique(species))) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::filter(number_species>100)

# Now apply the big function above to calculate each thing for
# every subrealm in the df we just made
lapply(urban_scores$subrealm, resample_SUD)

