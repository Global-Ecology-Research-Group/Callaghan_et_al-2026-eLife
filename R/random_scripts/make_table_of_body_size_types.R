# script to make a table of the different types of body size data included in the dataset

# read in the analysis dataframe
# this was made using the "R/cleaning_and_processing_files/create_a_dataframe_for_analysis.R"
analysis_data <- readRDS("analysis_data/analysis_data_harmonized.RDS")

# source some global helper functions
source("R/global_functions.R")

# packages
library(dplyr)
library(tidyr)
library(readr)

analysis_data %>%
  group_by(type) %>%
  summarize(N=n()) %>%
  dplyr::select(type) %>%
  write_csv(., "Tables/Table_S2.csv")
