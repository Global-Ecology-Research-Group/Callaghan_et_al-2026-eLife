# this script is meant to test a few sensitivity things
# and confirm/answer some things about modelling
# to see what does or does not influence the results
# questions I want to answer include:
# 1) The influence of scaling body size measures from within each metadata.
# 2) The difference between a random intercept model & a random slope + random intercept model

# my aim is to test these things using a random subset of families


# read in the analysis dataframe
analysis_data <- readRDS("analysis_data/analysis_data.RDS")

# source some global helper functions
source("R/global_functions.R")

# packages
library(dplyr)
library(ggplot2)
library(brms)
library(tidybayes)

# number of families with >10 species
# assign four 'model types' based on the critera for fitting random effects
# type 1: random effect for subrealm and random effect for dataset
# type 2: no random effects
# type 3: random effect for subrealm no dataset inclusion
# type 4: random effect for dataset no subrealm inclusion
family_summary <- analysis_data %>%
  group_by(family, order, class, phylum, kingdom) %>%
  summarize(number_species=length(unique(species)),
            number_of_obs=n(),
            number_of_subrealms=length(unique(subrealm)),
            number_of_datasets=length(unique(metadata))) %>%
  mutate(model_type=case_when(number_species<10 ~ "no model",
                              number_species>=10 & number_of_subrealms>=2 & number_of_datasets>=2 ~ "type 1",
                              number_species>=10 & number_of_subrealms==1 & number_of_datasets==1 ~ "type 2",
                              number_species>=10 & number_of_subrealms>=2 & number_of_datasets==1 ~ "type 3",
                              number_species>=10 & number_of_subrealms==1 & number_of_datasets>=2 ~ "type 4"))  %>%
  dplyr::filter(model_type!="no model")

# pick 10 families of different 'types' and different sample sizes
# to use as examples
# I made these selections manually
examples <- c("Nymphalidae", "Dasyuridae", 
              "Limnodynastidae", "Chironomidae", "Noctuidae",
              "Linyphiidae", "Grossulariaceae", "Chironomidae",
              "Formicidae", "Muscicapidae")

# first test will be to test the influence of 'scaling'
# the predictor variable (body size)
# by making each dataset body size values from 0 to 1
test_scaling <- function(family_name){
  
  message(paste0("Modelling ", family_name))
  
  # filter to data
  dat <- analysis_data %>%
    dplyr::filter(family==family_name) %>%
    left_join(., family_summary %>%
                dplyr::select(family, model_type) %>%
                distinct())
  
  ggplot(dat, aes(x=body_size, y=mean_urban_score_ghm, color=metadata))+
    geom_point()+
    scale_x_log10()+
    theme_bw()+
    theme(axis.text=element_text(color="black"))
  
  # repeat but now scale the data within each dataset (metadata)
  dat_scaled <- analysis_data %>%
    dplyr::filter(family==family_name) %>%
    left_join(., family_summary %>%
                dplyr::select(family, model_type) %>%
                distinct()) %>%
    group_by(metadata) %>%
    mutate(body_size_scaled=scales::rescale(body_size))
  
  ggplot(dat_scaled, aes(x=body_size_scaled+0.01, y=mean_urban_score_ghm, color=metadata))+
    geom_point()+
    scale_x_log10()+
    theme_bw()+
    theme(axis.text=element_text(color="black"))
  
  # now run through the different model types
  # and then save an RDS of the model fit as part of the brms call
 mod_0 <- if(unique(dat$model_type)=="type 1"){
    
    brms::brm(mean_urban_score_ghm ~ log10(body_size) + (1|subrealm) + (1|metadata),
              data=dat,
              warmup=1000,
              iter=4000, 
              chains=4)
    
  } else if(unique(dat$model_type)=="type 2"){
    
    brms::brm(mean_urban_score_ghm ~ log10(body_size),
              data=dat,
              warmup=1000,
              iter=4000, 
              chains=4)
    
  } else if(unique(dat$model_type)=="type 3"){
    
    brms::brm(mean_urban_score_ghm ~ log10(body_size) + (1|subrealm),
              data=dat,
              warmup=1000,
              iter=4000, 
              chains=4)
    
  } else if(unique(dat$model_type)=="type 4"){
    
    brms::brm(mean_urban_score_ghm ~ log10(body_size) + (1|metadata),
              data=dat,
              warmup=1000,
              iter=4000, 
              chains=4)
    
  }
 
 # now run through the different model types
 # and then save an RDS of the model fit as part of the brms call
 mod_scaled <- if(unique(dat$model_type)=="type 1"){
   
   brms::brm(mean_urban_score_ghm ~ log10(body_size_scaled+0.01) + (1|subrealm) + (1|metadata),
             data=dat_scaled,
             warmup=1000,
             iter=4000, 
             chains=4)
   
 } else if(unique(dat$model_type)=="type 2"){
   
   brms::brm(mean_urban_score_ghm ~ log10(body_size_scaled+0.01),
             data=dat_scaled,
             warmup=1000,
             iter=4000, 
             chains=4)
   
 } else if(unique(dat$model_type)=="type 3"){
   
   brms::brm(mean_urban_score_ghm ~ log10(body_size_scaled+0.01) + (1|subrealm),
             data=dat_scaled,
             warmup=1000,
             iter=4000, 
             chains=4)
   
 } else if(unique(dat$model_type)=="type 4"){
   
   brms::brm(mean_urban_score_ghm ~ log10(body_size_scaled+0.01) + (1|metadata),
             data=dat_scaled,
             warmup=1000,
             iter=4000, 
             chains=4)
   
 }
 
 #####################################################
 #####################################################
 # now summarize the two different models fit above
 
 # first for unscaled model
 coefficients_0 <- brms_SummaryTable(mod_0)
 
 draws_0 <- mod_0 %>%
   spread_draws(b_log10body_size) %>%
   sample_n(1000) %>%
   dplyr::select(b_log10body_size) %>%
   rename(body_size=1)
 
 out_df_0 <- draws_0 %>%
   mutate(intercept=coefficients_0 %>%
            dplyr::filter(Covariate=="Intercept") %>%
            .$Estimate %>%
            as.numeric()) %>%
   mutate(estimate=coefficients_0 %>%
            dplyr::filter(Covariate=="log10body_size") %>%
            .$Estimate %>%
            as.numeric()) %>%
   mutate(estimate.error=coefficients_0 %>%
            dplyr::filter(Covariate=="log10body_size") %>%
            .$Est.Error %>%
            as.numeric()) %>%
   mutate(lwr_95=coefficients_0 %>%
            dplyr::filter(Covariate=="log10body_size") %>%
            .$`l-95% CI` %>%
            as.numeric()) %>%
   mutate(upr_95=coefficients_0 %>%
            dplyr::filter(Covariate=="log10body_size") %>%
            .$`u-95% CI` %>%
            as.numeric()) %>%
   mutate(family=family_name) %>%
   ungroup() %>%
   mutate(type="no scaling")
 
 # then for scaled model
 coefficients_scaled <- brms_SummaryTable(mod_scaled)
 
 draws_scaled <- mod_scaled %>%
   spread_draws(b_log10body_size_scaledP0.01) %>%
   sample_n(1000) %>%
   dplyr::select(b_log10body_size_scaledP0.01) %>%
   rename(body_size=1)
 
 out_df_scaled <- draws_scaled %>%
   mutate(intercept=coefficients_scaled %>%
            dplyr::filter(Covariate=="Intercept") %>%
            .$Estimate %>%
            as.numeric()) %>%
   mutate(estimate=coefficients_scaled %>%
            dplyr::filter(Covariate=="log10body_size_scaledP0.01") %>%
            .$Estimate %>%
            as.numeric()) %>%
   mutate(estimate.error=coefficients_scaled %>%
            dplyr::filter(Covariate=="log10body_size_scaledP0.01") %>%
            .$Est.Error %>%
            as.numeric()) %>%
   mutate(lwr_95=coefficients_scaled %>%
            dplyr::filter(Covariate=="log10body_size_scaledP0.01") %>%
            .$`l-95% CI` %>%
            as.numeric()) %>%
   mutate(upr_95=coefficients_scaled %>%
            dplyr::filter(Covariate=="log10body_size_scaledP0.01") %>%
            .$`u-95% CI` %>%
            as.numeric()) %>%
   mutate(family=family_name) %>%
   ungroup() %>%
   mutate(type="scaling")
 
 # combine into one comparison dataframe that will be written out
 comparison_df <- out_df_0 %>%
   bind_rows(out_df_scaled)
 
 return(comparison_df)
}

# now apply this function over the 10 example families used for sensitivity analysis
scaling_test <- bind_rows(lapply_with_error(examples, test_scaling))

##################################################################
##################################################################
########### Visualize the results now
scaling_test %>%
  dplyr::select(2:8) %>%
  distinct() %>%
  ggplot(., aes(x=intercept, y=estimate, color=type, group=family))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_line(color="black")+
  geom_point()+
  scale_color_brewer(palette="Dark2")























