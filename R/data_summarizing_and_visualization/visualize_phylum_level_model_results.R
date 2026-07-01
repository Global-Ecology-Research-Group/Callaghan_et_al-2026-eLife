# this script is used to visualize and summarize
# the modelling results
# the results are a combination of the following two scripts
# 1) R/modelling/phylum_level_modelling.R
# 2) R/modelling/summarize_phylum_level_model_fits.R

# read in data
dat <- readRDS("intermediate_results/phylum_level_results.RDS")

# packages
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(forcats)
library(tidybayes)
library(ggokabeito)
library(patchwork)

# are viirs and ghm responses correlated?
dat %>%
  dplyr::select(phylum, response_type, estimate, kingdom) %>%
  distinct() %>%
  pivot_wider(names_from=response_type, values_from=estimate) %>%
  ggplot(., aes(x=ghm, y=viirs))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Urban score - global human modification")+
  ylab("Urban score - VIIRS night-time lights")+
  geom_smooth(method="lm")


# make a histogram of the fixed effects parameter estimates
dat %>%
  dplyr::filter(response_type=="ghm") %>%
  dplyr::select(phylum, estimate, kingdom) %>%
  distinct() %>%
  ggplot(., aes(x=estimate))+
  geom_histogram(bins=15, fill="gray80", color="black")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Parameter estimate")+
  ylab("Number of families")+
  facet_wrap(~kingdom, scales="free")


estimates <- dat %>%
  dplyr::filter(response_type=="ghm") %>%
  dplyr::select(-b_body_size_scaled_log10) %>%
  distinct() %>%
  mutate(overlap_zero=ifelse(lwr_95<=0 & upr_95>=0, "True", "False"))

table(estimates$overlap_zero)

ggplot(estimates, aes(y=estimate.error, x=model_type, fill=model_type))+
  geom_boxplot()+
  scale_y_log10()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))


# idk quick thing.
dat %>%
  dplyr::filter(response_type=="ghm") %>%
  group_by(kingdom) %>%
  arrange(estimate) %>%
  group_by(phylum) %>%
  dplyr::filter(b_body_size_scaled_log10>quantile(b_body_size_scaled_log10, 0.3)&
                  b_body_size_scaled_log10<quantile(b_body_size_scaled_log10, 0.7)) %>%
  ggplot(., aes(y=fct_inorder(phylum), x=b_body_size_scaled_log10))+
  geom_density_ridges()+
  theme_bw()+
  facet_wrap(~kingdom, scales="free")+
  xlab("Posterior")+
  ylab("")

ggplot(estimates, aes(x=number_of_obs, y=estimate.error))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))

ggplot(estimates, aes(x=model_R2))+
  geom_histogram(fill="gray80", color="black")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Number of families")+
  xlab("R2")+
  facet_wrap(~kingdom)

ggplot(estimates, aes(x=model_R2, y=estimate.error))+
  geom_point()+
  #scale_x_log10()+
  #scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))



