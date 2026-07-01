# this script is used to visualize and summarize
# the modelling results
# the results are a combination of the following two scripts
# 1) R/modelling/order_level_modelling.R
# 2) R/modelling/summarize_order_level_model_fits.R

# read in data
dat <- readRDS("intermediate_results/order_level_results.RDS")

# packages
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(forcats)
library(tidybayes)
library(ggokabeito)
library(patchwork)


##########################################################
##########################################################
######## This is a function that loops through every model
######## fit for every order and makes a visualization of the results
######## and then saves out this visualization as an individual png
######## into the following folder: "Results/order_level_model_vis"

#
analysis_data <- readRDS("analysis_data/analysis_data_harmonized.RDS")

# source some global helper functions
source("R/global_functions.R")


# read in prepared summary of potential model fits
order_summary <- readRDS("analysis_data/summary_order.RDS")

# read in prepared data for analysis
analysis_data2 <- readRDS("analysis_data/final_data_for_analysis_class.RDS")

# file names
order_mods <- list.files("model_objects/order_level/")

# visualize model results function
model_results_viz <- function(order_name){
  
  message(paste0("Summarizing ", order_name))
  
  dat <- order_summary %>%
    dplyr::filter(order==order_name) %>%
    mutate(model_type=gsub(" ", "_", model_type))
  
  # read in model output
  mod <- readRDS(paste0("model_objects/order_level/", order_name, "_viirs_", dat$model_type, ".RDS"))
  
  # get fixed effects plot
  fe_only <- tibble(body_size_scaled_log10 = seq(min(mod$data$body_size_scaled_log10), 
                                                 max(mod$data$body_size_scaled_log10), length.out=100)) %>%
    add_fitted_draws(mod,
                     re_formula = NA,
                     scale = "response", n = 1e3)
  
  fe_only_mean <- fe_only %>% 
    group_by(body_size_scaled_log10) %>%
    summarize(.value = mean(.value))
  
  # make plot of predicted line
  predicted_fit <- ggplot()+
    geom_line(data=fe_only, aes(x=body_size_scaled_log10, y=.value, group=.draw), 
              alpha=0.1)+
    geom_line(data=fe_only_mean, aes(x=body_size_scaled_log10, y=.value),
              color="red", lwd=2, group=1)+
    theme_bw()+
    theme(axis.text=element_text(color="black"))+
    xlab("Body size")+
    ylab("Urban tolerance")+
    ggtitle("Fitted relationship")
  
  predicted_fit
  
  # get posterior predictions of parameter estimate
  draws <- mod %>%
    spread_draws(b_body_size_scaled_log10)
  
  # make plot of posterior predictions
  posterior <- ggplot(draws, aes(x=b_body_size_scaled_log10))+
    stat_halfeye(fill="gray60")+
    scale_fill_okabe_ito()+
    xlab("Effect of body size on urban tolerance")+
    ggtitle("Posterior predictions")+
    theme_minimal()+
    theme(axis.text=element_text(color="black"))+
    theme(axis.title.y=element_blank())+
    theme(axis.text.y=element_blank())+
    theme(panel.grid=element_blank())+
    geom_vline(xintercept=0, color="red", linetype="dashed")
  
  posterior
  
  table <- gridExtra::tableGrob(dat %>%
                                  ungroup() %>%
                                  dplyr::select(5:7) %>%
                                  rename(`Number of unique species:`=number_species,
                                         `Total number of observations:`=number_of_obs,
                                         `Number of subrealms included:`=number_of_subrealms) %>%
                                  t())
  
  
  table
  
  # put them all together into one plot
  plot_summary <- (wrap_elements(table) | posterior) / predicted_fit + plot_annotation(title = paste0(order_name, ", ", dat$class))
  
  plot_summary
  
  if (dat$kingdom=="Animalia") {
    
    ggsave(paste0("Results/order_level_model_vis/Animalia/", order_name, ".png"), width=6.5, height=5.8, units="in")
    
  } else {
    
    ggsave(paste0("Results/order_level_model_vis/Plantae/", order_name, ".png"), width=6.5, height=5.8, units="in")
    
  }
  
  
}

lapply(order_summary$order, model_results_viz)
