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

family_name="Apidae"

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

myprior <- c(set_prior("normal(0,1)", class = "b"))

Apidae <- brms::brm(mean_urban_score_viirs ~ metadata2*body_size_scaled_log10 + (1 + body_size_scaled_log10|subrealm),
                    data=dat,
                    warmup=1000,
                    iter=6000, 
                    chains=4,
                    prior=myprior,
                    control=list(adapt_delta=0.99))

summary(Apidae)

# First, calculate sample size per group
n_per_group <- dat %>%
  count(metadata2) %>%
  mutate(label = paste0(metadata2, " (n = ", n, ")"))

# Get conditional effects object
ce <- conditional_effects(Apidae, effects = "body_size_scaled_log10:metadata2")

ce_data <- ce[[1]]

# Step 1: Create short labels and attach to metadata2
short_labels <- dat %>%
  count(metadata2) %>%
  arrange(metadata2) %>%
  ungroup() %>%
  mutate(
    short = LETTERS[1:n()],
    facet_label = paste0(short, ". (n = ", n, ")"),
    legend_label = paste0(short, ". ", metadata2)
  )

# Step 2: Merge short labels into plotting data
ce_data_labeled_final <- ce_data %>%
  left_join(short_labels, by = "metadata2")

# Step 3: Plot
apid_interactions <- ggplot(ce_data_labeled_final, aes(x = body_size_scaled_log10, y = estimate__)) +
  geom_line(aes(color = legend_label)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = legend_label), alpha = 0.3) +
  facet_wrap(~facet_label) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    legend.title = element_text(size = 10)
  ) +
  labs(
    x = "Body size (scaled log10)",
    y = "Urban tolerance",
    title = "(A) Apidae metadata interactions",
    fill = "metadata",
    color = "metadata"
  )

apid_interactions

v1_apid <- readRDS("model_objects/family_level/Apidae_viirs_type_1.rds")

v2_apid <- readRDS("model_objects/family_level_v2/Apidae_viirs_type_1.rds")

summary(v1_apid)

summary(v2_apid)

# Extract draws for the same parameter from both models
v1_draws <- posterior_samples(v1_apid, pars = "b_body_size_scaled_log10") %>%
  rename(estimate = b_body_size_scaled_log10) %>%
  mutate(model = "V1")

v2_draws <- posterior_samples(v2_apid, pars = "b_body_size_scaled_log10") %>%
  rename(estimate = b_body_size_scaled_log10) %>%
  mutate(model = "V2")

# Combine into one data frame
all_draws <- bind_rows(v1_draws, v2_draws)

# Plot using bayesplot
apid_posterior <- ggplot(all_draws, aes(x = estimate, fill = model)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "(B) Comparison of posterior distributions",
    x = "Slope: body_size_scaled_log10",
    y = "Density"
  ) +
  theme_bw()

apid_posterior


wrap_elements(apid_interactions) + wrap_elements(apid_posterior) + 
  plot_layout(ncol = 1) 


ggsave("Figures/random_slopes_example_Apidae.png", width=8, height=7.5, units="in")

# now a function to model every family possible
# and save the model results out
# as a file

family_name="Accipitridae"

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

myprior <- c(set_prior("normal(0,1)", class = "b"))

Accipitridae <- brms::brm(mean_urban_score_viirs ~ metadata2*body_size_scaled_log10 + (1 + body_size_scaled_log10|subrealm),
                       data=dat,
                       warmup=1000,
                       iter=6000, 
                       chains=4,
                       prior=myprior,
                       control=list(adapt_delta=0.99))


# First, calculate sample size per group
n_per_group <- dat %>%
  count(metadata2) %>%
  mutate(label = paste0(metadata2, " (n = ", n, ")"))

# Get conditional effects object
ce <- conditional_effects(Accipitridae, effects = "body_size_scaled_log10:metadata2")

ce_data <- ce[[1]]

# Step 1: Create short labels and attach to metadata2
short_labels <- dat %>%
  count(metadata2) %>%
  arrange(metadata2) %>%
  ungroup() %>%
  mutate(
    short = LETTERS[1:n()],
    facet_label = paste0(short, ". (n = ", n, ")"),
    legend_label = paste0(short, ". ", metadata2)
  )

# Step 2: Merge short labels into plotting data
ce_data_labeled_final <- ce_data %>%
  left_join(short_labels, by = "metadata2")

# Step 3: Plot
accip_interactions <- ggplot(ce_data_labeled_final, aes(x = body_size_scaled_log10, y = estimate__)) +
  geom_line(aes(color = legend_label)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = legend_label), alpha = 0.3) +
  facet_wrap(~facet_label) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    legend.title = element_text(size = 10)
  ) +
  labs(
    x = "Body size (scaled log10)",
    y = "Urban tolerance",
    title = "(A) Accipitridae metadata interactions",
    fill = "metadata",
    color = "metadata"
  )

accip_interactions

v1_accip <- readRDS("model_objects/family_level/Accipitridae_viirs_type_1.rds")

v2_accip <- readRDS("model_objects/family_level_v2/Accipitridae_viirs_type_1.rds")

summary(v1_accip)

summary(v2_accip)

# Extract draws for the same parameter from both models
v1_draws <- posterior_samples(v1_accip, pars = "b_body_size_scaled_log10") %>%
  rename(estimate = b_body_size_scaled_log10) %>%
  mutate(model = "V1")

v2_draws <- posterior_samples(v2_accip, pars = "b_body_size_scaled_log10") %>%
  rename(estimate = b_body_size_scaled_log10) %>%
  mutate(model = "V2")

# Combine into one data frame
all_draws <- bind_rows(v1_draws, v2_draws)

# Plot using bayesplot
accip_posterior <- ggplot(all_draws, aes(x = estimate, fill = model)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "(B) Comparison of posterior distributions",
    x = "Slope: body_size_scaled_log10",
    y = "Density"
  ) +
  theme_bw()

accip_posterior


wrap_elements(accip_interactions) + wrap_elements(accip_posterior) + 
  plot_layout(ncol = 1) 


ggsave("Figures/random_slopes_example_Accipitridae.png", width=8, height=7.5, units="in")


