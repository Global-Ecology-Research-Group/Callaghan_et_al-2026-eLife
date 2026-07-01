## This script is used to
## summarize the model fits across the family level

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
dat <- readRDS("intermediate_results/family_level_results.RDS") %>%
  dplyr::filter(response_type=="viirs")

# get the family level summary as well
analysis_data <- readRDS("analysis_data/analysis_data_harmonized.RDS")

# source some global helper functions
source("R/global_functions.R")

# read in prepared summary of potential model fits
family_summary <- readRDS("analysis_data/summary_family.RDS")

# read in prepared data for analysis
analysis_data2 <- readRDS("analysis_data/final_data_for_analysis_family.RDS")


length(unique(analysis_data2$species))
length(unique(analysis_data2$family))


# how many species fit for the different families
sum(family_summary$number_species)
length(unique(family_summary$family))

# check something
analysis_data2 %>%
  dplyr::filter(family %in% family_summary$family) %>%
  .$species %>%
  unique() %>%
  length()


# assess 80, 90, and 95% CI
credible_interval_summary <- dat %>%
  dplyr::select(-b_body_size_scaled_log10) %>%
  distinct() %>%
  mutate(strength_of_effect=case_when(lower_95_CI <= 0 & upper_95_CI <= 0 ~ "Strong",
                                      lower_95_CI >= 0 & upper_95_CI >= 0 ~ "Strong",
                                      lower_90_CI <= 0 & upper_90_CI <= 0 ~ "Moderate",
                                      lower_90_CI >= 0 & upper_90_CI >= 0 ~ "Moderate",
                                      lower_80_CI <= 0 & upper_80_CI <= 0 ~ "Weak",
                                      lower_80_CI >= 0 & upper_80_CI >= 0 ~ "Weak")) %>%
  mutate(direction_of_effect=ifelse(estimate>=0, "positive", "negative"))


# summarize this now for paper
credible_interval_summary %>%
  dplyr::filter(complete.cases(strength_of_effect)) %>%
  group_by(strength_of_effect) %>%
  summarize(N=n())

credible_interval_summary %>%
  dplyr::filter(complete.cases(strength_of_effect)) %>%
  group_by(kingdom) %>%
  summarize(N=n())

credible_interval_summary %>%
  dplyr::filter(complete.cases(direction_of_effect)) %>%
  group_by(direction_of_effect, kingdom) %>%
  summarize(N=n())

credible_interval_summary %>%
  dplyr::filter(complete.cases(direction_of_effect)) %>%
  group_by(kingdom, direction_of_effect, strength_of_effect) %>%
  summarize(N=n())


table(credible_interval_summary$kingdom, 
      credible_interval_summary$strength_of_effect)

table(credible_interval_summary$kingdom, 
      credible_interval_summary$strength_of_effect,
      credible_interval_summary$direction_of_effect)



# assess the p-values
# number of 'significant' effect sizes
p_value_summary <- dat %>%
  dplyr::select(kingdom, family, fixed_p_value) %>%
  distinct() %>%
  mutate(significant=fixed_p_value<=0.05)

