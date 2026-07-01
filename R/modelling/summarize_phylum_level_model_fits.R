# this script is to summarize the phylum level models
# to understand the results of the model fits
# and look at the results across all models that were fit
# it saves out the summary dataframe as an 'intermediate result'
# which will be further processed

# first it mimics the 'phylum_level_modelling.R' script 
# in phylum to see how many possible models could have been fit

# packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(brms)
library(tidybayes)

# read in the analysis data
analysis_data <- readRDS("analysis_data/analysis_data_harmonized.RDS")

# source some global helper functions
source("R/global_functions.R")

# read in prepared summary of potential model fits
phylum_summary <- readRDS("analysis_data/summary_phylum.RDS")

# read in prepared data for analysis
analysis_data2 <- readRDS("analysis_data/final_data_for_analysis_phylum.RDS")

# read in list of files for the modelling results
model_fit_files <- list.files("model_objects/phylum_level")

# split to 'ghm' and 'viirs' file names
model_fit_files_ghm <- data.frame(file_name=model_fit_files) %>%
  mutate(type=stringr::word(file_name, 2, sep="_")) %>%
  dplyr::filter(type=="ghm") %>%
  .$file_name

model_fit_files_viirs <- data.frame(file_name=model_fit_files) %>%
  mutate(type=stringr::word(file_name, 2, sep="_")) %>%
  dplyr::filter(type=="viirs") %>%
  .$file_name

# did each potential phylum write out a model object?
nrow(phylum_summary)==length(model_fit_files_ghm)
nrow(phylum_summary)==length(model_fit_files_viirs)

# write a function that
# reads in each model fit, one at a time
# and then summarizes the information
# in that model fit
summarize_phylum_models_function <- function(phylum_name){
  
  message(paste0("Summarizing ", stringr::word(phylum_name, 1, sep="_")))
  
  # get original data used for modelling
  # filter to data
  dat <- analysis_data2 %>%
    dplyr::filter(phylum==stringr::word(phylum_name, 1, sep="_")) %>%
    left_join(., phylum_summary %>%
                dplyr::select(phylum, model_type) %>%
                distinct()) %>%
    ungroup() %>%
    group_by(metadata) %>%
    mutate(body_size_scaled_log10=scale(log10(body_size)))
  
  ggplot(dat, aes(x=body_size, y=mean_urban_score_ghm, color=metadata2))+
    geom_point()+
    scale_x_log10()+
    theme_bw()+
    theme(axis.text=element_text(color="black"))
  
  ggplot(dat, aes(x=body_size_scaled_log10, y=mean_urban_score_ghm, color=metadata2))+
    geom_point()+
    theme_bw()+
    theme(axis.text=element_text(color="black"))
  
  mod <- readRDS(paste0("model_objects/phylum_level/", phylum_name))
  
  # coefficients table
  coefficients <- brms_SummaryTable(mod)
  
  # get a sample of 1000 draws from the posterior
  draws <- mod %>%
    spread_draws(b_body_size_scaled_log10) %>%
    sample_n(10000) %>%
    dplyr::select(b_body_size_scaled_log10)
  
  # compute R2
  r2 <- bayes_R2(mod, summary=TRUE) %>%
    as.data.frame()
  
  out_df <- draws %>%
    mutate(estimate=coefficients %>%
             dplyr::filter(Covariate=="body_size_scaled_log10") %>%
             .$Estimate %>%
             as.numeric()) %>%
    mutate(estimate.error=coefficients %>%
             dplyr::filter(Covariate=="body_size_scaled_log10") %>%
             .$Est.Error %>%
             as.numeric()) %>%
    mutate(lwr_95=coefficients %>%
             dplyr::filter(Covariate=="body_size_scaled_log10") %>%
             .$`l-95% CI` %>%
             as.numeric()) %>%
    mutate(upr_95=coefficients %>%
             dplyr::filter(Covariate=="body_size_scaled_log10") %>%
             .$`u-95% CI` %>%
             as.numeric()) %>%
    mutate(model_R2=r2$Estimate) %>%
    mutate(phylum=stringr::word(phylum_name, 1, sep="_")) %>%
    ungroup()
  
  return(out_df)
  
}

# now apply this function to every model fit that was run
# first for ghm
phylum_results_ghm <- bind_rows(lapply_with_error(model_fit_files_ghm, summarize_phylum_models_function)) %>%
  mutate(response_type="ghm")

# then for viirs
phylum_results_viirs <- bind_rows(lapply_with_error(model_fit_files_viirs, summarize_phylum_models_function)) %>%
  mutate(response_type="viirs")

#combine
phylum_overall_summary <- phylum_results_ghm %>%
  bind_rows(phylum_results_viirs) %>%
  left_join(., phylum_summary, by="phylum")

saveRDS(phylum_overall_summary, "intermediate_results/phylum_level_results.RDS")
