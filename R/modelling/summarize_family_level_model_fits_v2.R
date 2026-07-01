# this script is to summarize the family level models
# to understand the results of the model fits
# and look at the results across all models that were fit
# it saves out the summary dataframe as an 'intermediate result'
# which will be further processed

# first it mimics the 'family_level_modelling.R' script 
# in order to see how many possible models could have been fit

# packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(brms)
library(tidybayes)
library(tibble)
library(bayestestR)

# read in the analysis data
analysis_data <- readRDS("analysis_data/analysis_data_harmonized.RDS")

# source some global helper functions
source("R/global_functions.R")

# read in prepared summary of potential model fits
family_summary <- readRDS("analysis_data/summary_family.RDS")

# read in prepared data for analysis
analysis_data2 <- readRDS("analysis_data/final_data_for_analysis_family.RDS")

# read in list of files for the modelling results
model_fit_files <- list.files("model_objects/family_level_v2")

# split to 'ghm' and 'viirs' file names
model_fit_files_ghm <- data.frame(file_name=model_fit_files) %>%
  mutate(type=stringr::word(file_name, 2, sep="_")) %>%
  dplyr::filter(type=="ghm") %>%
  .$file_name

model_fit_files_viirs <- data.frame(file_name=model_fit_files) %>%
  mutate(type=stringr::word(file_name, 2, sep="_")) %>%
  dplyr::filter(type=="viirs") %>%
  .$file_name

# did each potential family write out a model object?
nrow(family_summary)==length(model_fit_files_ghm)
nrow(family_summary)==length(model_fit_files_viirs)

# write a function that
# reads in each model fit, one at a time
# and then summarizes the information
# in that model fit
summarize_family_models_function <- function(family_name){
  
  message(paste0("Summarizing ", stringr::word(family_name, 1, sep="_")))
  
  # get original data used for modelling
  # filter to data
  dat <- analysis_data2 %>%
    dplyr::filter(family==stringr::word(family_name, 1, sep="_")) %>%
    left_join(., family_summary %>%
                dplyr::select(family, model_type) %>%
                distinct()) %>%
    ungroup() %>%
    group_by(metadata2) %>%
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
  
  mod <- readRDS(paste0("model_objects/family_level_v2/", family_name))
  
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
  
  # compute bayesian p-value
  p_value <- p_map(mod) %>%
    as.data.frame()
  
  # ci (HDI method)
  ci_95 <- ci(mod, method="HDI")
  ci_90 <- ci(mod, ci=0.90, method="HDI")
  ci_80 <- ci(mod, ci=0.80, method="HDI")
  
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
    mutate(fixed_p_value=p_value %>%
             dplyr::filter(Parameter=="b_body_size_scaled_log10") %>%
             .$p_MAP) %>%
    mutate(lower_80_CI=ci_80 %>%
             slice(2) %>%
             .$CI_low) %>%
    mutate(upper_80_CI=ci_80 %>%
             slice(2) %>%
             .$CI_high) %>%
    mutate(lower_90_CI=ci_90 %>%
             slice(2) %>%
             .$CI_low) %>%
    mutate(upper_90_CI=ci_90 %>%
             slice(2) %>%
             .$CI_high) %>%
    mutate(lower_95_CI=ci_95 %>%
             slice(2) %>%
             .$CI_low) %>%
    mutate(upper_95_CI=ci_95 %>%
             slice(2) %>%
             .$CI_high) %>%
    mutate(family=stringr::word(family_name, 1, sep="_")) %>%
    ungroup()
  
  return(out_df)
  
}

# then for viirs
family_results_viirs <- bind_rows(lapply_with_error(model_fit_files_viirs, summarize_family_models_function)) %>%
  mutate(response_type="viirs")

#combine
family_overall_summary <- family_results_viirs %>%
  left_join(., family_summary, by="family")

saveRDS(family_overall_summary, "intermediate_results/family_level_results_v2.RDS")


###########################################
###########################################
######### Now an additional function to calculate 
######### the spatial variability in the slopes
###########################################
spatial_variability_summary_function <- function(family_name){
  
  message(paste0("Summarizing ", family_name))
  
  dat <- family_summary %>%
    dplyr::filter(family==family_name)
  
  mod <- readRDS(paste0("model_objects/family_level/", family_name, "_viirs_", gsub(" ", "_", dat$model_type), ".rds"))
  
  # get standard deviation of the slope estimates?
  SD <- VarCorr(mod)$subrealm$sd["body_size_scaled_log10",]
  
  # get random effects by subrealm ----
  RandomSlopes <- ranef(mod)$subrealm[,,"body_size_scaled_log10"] %>%
    as_tibble(rownames = NA) %>%
    add_column(subrealm = as.character(row.names(.)))
  
  # create a summary df for output
  summary_df <- RandomSlopes %>%
    mutate(SD_estimate=SD[1],
           SD_error=SD[2],
           SD_lwr_95=SD[3],
           SD_upr_95=SD[4]) %>%
    mutate(family=family_name)
  
  return(summary_df)
  
}

# now run the spatial variability summary
# but only for families with >= 10 subrealms where random slopes were estimated
# can always come back to this cutoff later
# but essentially trying to only get the broadly distributed families for which we have data
spatial_variability_results <- bind_rows(lapply(family_summary %>%
                                                  dplyr::filter(number_of_subrealms>=10) %>%
                                                  .$family, spatial_variability_summary_function))


saveRDS(spatial_variability_results, "intermediate_results/family_level_spatial_variability_results.RDS")





