# this script is to summarize the order level models
# to understand the results of the model fits
# and look at the results across all models that were fit
# it saves out the summary dataframe as an 'intermediate result'
# which will be further processed

# first it mimics the 'order_level_modelling.R' script 
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
order_summary <- readRDS("analysis_data/summary_order.RDS")

# read in prepared data for analysis
analysis_data2 <- readRDS("analysis_data/final_data_for_analysis_order.RDS")

# read in list of files for the modelling results
model_fit_files <- list.files("model_objects/order_level")

# split to 'ghm' and 'viirs' file names
model_fit_files_ghm <- data.frame(file_name=model_fit_files) %>%
  mutate(type=stringr::word(file_name, 2, sep="_")) %>%
  dplyr::filter(type=="ghm") %>%
  .$file_name

model_fit_files_viirs <- data.frame(file_name=model_fit_files) %>%
  mutate(type=stringr::word(file_name, 2, sep="_")) %>%
  dplyr::filter(type=="viirs") %>%
  .$file_name

# did each potential order write out a model object?
nrow(order_summary)==length(model_fit_files_ghm)
nrow(order_summary)==length(model_fit_files_viirs)

# write a function that
# reads in each model fit, one at a time
# and then summarizes the information
# in that model fit
summarize_order_models_function <- function(order_name){
  
  message(paste0("Summarizing ", stringr::word(order_name, 1, sep="_")))
  
  # get original data used for modelling
  # filter to data
  dat <- analysis_data2 %>%
    dplyr::filter(order==stringr::word(order_name, 1, sep="_")) %>%
    left_join(., order_summary %>%
                dplyr::select(order, model_type) %>%
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
  
  mod <- readRDS(paste0("model_objects/order_level/", order_name))
  
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
    mutate(order=stringr::word(order_name, 1, sep="_")) %>%
    ungroup()
  
  return(out_df)
  
}

# now apply this function to every model fit that was run
# first for ghm
order_results_ghm <- bind_rows(lapply_with_error(model_fit_files_ghm, summarize_order_models_function)) %>%
  mutate(response_type="ghm")

# then for viirs
order_results_viirs <- bind_rows(lapply_with_error(model_fit_files_viirs, summarize_order_models_function)) %>%
  mutate(response_type="viirs")

#combine
order_overall_summary <- order_results_ghm %>%
  bind_rows(order_results_viirs) %>%
  left_join(., order_summary, by="order")

saveRDS(order_overall_summary, "intermediate_results/order_level_results.RDS")


###########################################
###########################################
######### Now an additional function to calculate 
######### the spatial variability in the slopes
###########################################
spatial_variability_summary_function <- function(order_name){
  
  message(paste0("Summarizing ", order_name))
  
  dat <- order_summary %>%
    dplyr::filter(order==order_name)
  
  mod <- readRDS(paste0("model_objects/order_level/", order_name, "_viirs_", gsub(" ", "_", dat$model_type), ".rds"))
  
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
    mutate(order=order_name)
  
  return(summary_df)
  
}

# now run the spatial variability summary
# but only for families with >= 10 subrealms where random slopes were estimated
# can always come back to this cutoff later
# but essentially trying to only get the broadly distributed families for which we have data
spatial_variability_results <- bind_rows(lapply(order_summary %>%
                                                  dplyr::filter(number_of_subrealms>=10) %>%
                                                  .$order, spatial_variability_summary_function))


saveRDS(spatial_variability_results, "intermediate_results/order_level_spatial_variability_results.RDS")
