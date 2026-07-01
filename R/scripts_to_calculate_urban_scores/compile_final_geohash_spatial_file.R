# script to read in all the bioregions assigned csvs
# and then compile them into 1 large csv
# with the viirs assigned csvs

# packages
library(readr)
library(dplyr)
library(purrr)

file_names <- dir("Data/geohash_bioregions_joined/")
setwd("Data/geohash_bioregions_joined/")
bioregions_all <- do.call(rbind,lapply(file_names, read_csv))
setwd("../..")

file_names <- dir("Data/geohash_subrealms_joined/")
setwd("Data/geohash_subrealms_joined/")
subrealms_all <- do.call(rbind,lapply(file_names, read_csv))
setwd("../..")

file_names <- dir("Data/GEE_viirs_results/")
setwd("Data/GEE_viirs_results/")
viirs_all <- do.call(rbind,lapply(file_names, read_csv))
setwd("../..")

file_names <- dir("Data/GEE_ghm_results/")
setwd("Data/GEE_ghm_results/")
ghm_all <- do.call(rbind,lapply(file_names, read_csv))
setwd("../..")

length(unique(viirs_all$geohash7))
length(unique(bioregions_all$geohash7))
length(unique(ghm_all$geohash7))
length(unique(subrealms_all$geohash7))

gbif_geohash_assigned <- viirs_all %>%
  dplyr::select(first, geohash7) %>%
  rename(viirs=first) %>%
  left_join(., bioregions_all %>%
              dplyr::select(geohash7, BIOREGION_NAME)) %>%
  left_join(., subrealms_all %>%
              dplyr::select(geohash7, subrealm)) %>%
  left_join(., ghm_all %>%
              dplyr::select(first, geohash7) %>%
              rename(ghm=first))

readr::write_csv(gbif_geohash_assigned, "Data/gbif_geohash_assigned.csv")
saveRDS(gbif_geohash_assigned, "Data/gbif_geohash_assigned.RDS")
