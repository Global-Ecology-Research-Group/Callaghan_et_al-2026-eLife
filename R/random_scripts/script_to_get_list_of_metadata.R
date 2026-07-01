# this script is to get a list of all the metadata files
# of the body size data that was used in the modelling efforts
# because not all body size datasets found will be used necessarily in the
# modelling. So we want to summarize this appropriately
# and do it for the different taxonomic levels of models

# packages
library(tidyverse)
library(dplyr)
library(bib2df)

# read in the dataframes
kingdom <- readRDS("intermediate_results/model_metadata_usage_summary/metadata_summary_kingdom_viirs.RDS")
kingdom_random <- readRDS("intermediate_results/model_metadata_usage_summary/metadata_summary_kingdom_random_effects_viirs.RDS")
phylum <- readRDS("intermediate_results/model_metadata_usage_summary/metadata_summary_phylum_viirs.RDS")
class <- readRDS("intermediate_results/model_metadata_usage_summary/metadata_summary_class_viirs.RDS")
order <- readRDS("intermediate_results/model_metadata_usage_summary/metadata_summary_order_viirs.RDS")
family <- readRDS("intermediate_results/model_metadata_usage_summary/metadata_summary_family_viirs.RDS")

# remove the model type column and change the "N" column header to reflect the taxonomic level
kingdom <- kingdom %>% select(metadata, N) %>% rename("kingdom_N"="N")
kingdom_random <- kingdom_random %>% select(metadata, N) %>% rename("kingdom_random_N"="N")
phylum <- phylum %>% select(metadata, N) %>% rename("phylum_N"="N")
class <- class %>% select(metadata, N) %>% rename("class_N"="N")
order <- order %>% select(metadata, N) %>% rename("order_N"="N")
family <- family %>% select(metadata, N) %>% rename("family_N"="N")

# create a list and merge the data frames 
all <- list(kingdom, kingdom_random, phylum, class, order, family)
all <- all %>% reduce(full_join, by="metadata")

# save the final data frame
saveRDS(all, "intermediate_results/model_metadata_usage_summary/metadata_summary_all_viirs.RDS")

### 3 Columns ###

# read in the dataframes
kingdom <- readRDS("intermediate_results/model_metadata_usage_summary/metadata_summary_kingdom_viirs.RDS")
kingdom_random <- readRDS("intermediate_results/model_metadata_usage_summary/metadata_summary_kingdom_random_effects_viirs.RDS")
phylum <- readRDS("intermediate_results/model_metadata_usage_summary/metadata_summary_phylum_viirs.RDS")
class <- readRDS("intermediate_results/model_metadata_usage_summary/metadata_summary_class_viirs.RDS")
order <- readRDS("intermediate_results/model_metadata_usage_summary/metadata_summary_order_viirs.RDS")
family <- readRDS("intermediate_results/model_metadata_usage_summary/metadata_summary_family_viirs.RDS")


# remove the model type column and change the "N" column header to reflect the taxonomic level
kingdom$model_type <- "kingdom"
kingdom_random$model_type <- "kingdom_random"
phylum$model_type <- "phylum"
class$model_type <- "class"
order$model_type <- "order"
family$model_type <- "family"

all <- rbind(kingdom, kingdom_random, phylum, class, order, family)

saveRDS(all, "intermediate_results/model_metadata_usage_summary/metadata_summary_all_2_viirs.RDS")
