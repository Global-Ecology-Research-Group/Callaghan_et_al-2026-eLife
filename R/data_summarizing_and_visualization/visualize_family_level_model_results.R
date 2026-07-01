# this script is used to visualize and summarize
# the modelling results
# the results are a combination of the following two scripts
# 1) R/modelling/family_level_modelling.R
# 2) R/modelling/summarize_family_level_model_fits.R


# packages
library(ggplot2)
library(see)
library(ggridges)
library(dplyr)
library(tidyr)
library(forcats)
library(tidybayes)
library(ggokabeito)
library(patchwork)


# read in data
# dat <- readRDS("intermediate_results/family_level_results.RDS")

# read in the parameter estimates
dat_family <- readRDS("intermediate_results/family_level_results.RDS") %>%
  dplyr::filter(response_type=="viirs") %>%
  dplyr::select(-b_body_size_scaled_log10) %>%
  distinct() %>%
  mutate(overlap_zero=ifelse(lwr_95<=0 & upr_95>=0, "True", "False"))

# make a plot of family parameter estimates
# for animals
dat_family %>%
  dplyr::filter(kingdom=="Animalia") %>%
  ggplot(aes(x=estimate))+
  geom_histogram(color="black", fill="grey80", bins=15)+
  theme_classic()+
  xlab("Parameter estimate")+
  ylab("Number of families")+
  theme(axis.text=element_text(color="black", size=8))+
  theme(axis.title=element_text(size=10))+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  geom_vline(xintercept=0, color="red", linetype="dashed")+
  ggtitle(paste0("Animalia", "\nN = ", dat_family %>%
                   dplyr::filter(kingdom=="Animalia") %>%
                   nrow(.), " families"))+
  theme(plot.title=element_text(size=12))+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'), #transparent legend panel
  )

ggsave("Figures/small_animal_histogram_of_parameter_estimates.png", width=3, height=2, units="in", bg="transparent")


dat_family %>%
  dplyr::filter(kingdom=="Plantae") %>%
  ggplot(aes(x=estimate))+
  geom_histogram(color="black", fill="darkseagreen2", bins=15)+
  theme_classic()+
  xlab("Parameter estimate")+
  ylab("Number of families")+
  theme(axis.text=element_text(color="black", size=8))+
  theme(axis.title=element_text(size=10))+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  geom_vline(xintercept=0, color="red", linetype="dashed")+
  ggtitle(paste0("Plantae", "\nN = ", dat_family %>%
                   dplyr::filter(kingdom=="Plantae") %>%
                   nrow(.), " families"))+
  theme(plot.title=element_text(size=12))+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

ggsave("Figures/small_plant_histogram_of_parameter_estimates.png", width=3, height=2, units="in", bg="transparent")


# get the family level summary as well
#
analysis_data <- readRDS("analysis_data/analysis_data_harmonized.RDS")

# source some global helper functions
source("R/global_functions.R")

# read in prepared summary of potential model fits
family_summary <- readRDS("analysis_data/summary_family.RDS")

# read in prepared data for analysis
analysis_data2 <- readRDS("analysis_data/final_data_for_analysis_family.RDS")


##########################################################
##########################################################
######## This is a function that loops through every model
######## fit for every family and makes a visualization of the results
######## and then saves out this visualization as an individual png
######## into the following folder: "Results/family_level_model_vis"


# file names
family_mods <- list.files("model_objects/family_level/")

families_viirs <- data.frame(file_name=family_mods) %>%
  mutate(type=stringr::word(file_name, 2, sep="_")) %>%
  dplyr::filter(type=="viirs") %>%
  mutate(family=stringr::word(file_name, 1, sep="_")) %>%
  .$family

# visualize model results function
model_results_viz <- function(family_name){
  
  message(paste0("Summarizing ", family_name))
  
  dat <- family_summary %>%
    dplyr::filter(family==family_name) %>%
    mutate(model_type=gsub(" ", "_", model_type))
  
  # read in model output
  mod <- readRDS(paste0("model_objects/family_level/", family_name, "_viirs_", dat$model_type, ".RDS"))
  
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
                                  dplyr::select(6:8) %>%
                                  rename(`Number of unique species:`=number_species,
                                         `Total number of observations:`=number_of_obs,
                                         `Number of subrealms included:`=number_of_subrealms) %>%
                                  t())
  
  
  table
  
  # put them all together into one plot
  plot_summary <- (wrap_elements(table) | posterior) / predicted_fit + plot_annotation(title = paste0(family_name, ", ", dat$order, ", ", dat$class))
  
  plot_summary
  
  if (dat$kingdom=="Animalia") {
    
    ggsave(paste0("Results/family_level_model_vis/Animalia/", family_name, ".png"), width=6.5, height=5.8, units="in")
    
  } else {
    
    ggsave(paste0("Results/family_level_model_vis/Plantae/", family_name, ".png"), width=6.5, height=5.8, units="in")
    
  }
  
  
}

lapply(families_viirs, model_results_viz)












