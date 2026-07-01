# create a bioregion to subrealm table
# I think this has to be done manually
# and that is annoying
library(sf)
library(readr)
library(tidyr)

# bioregions
bioregions <- st_read("Data/OE_Bioregions/OE_Bioregions.shp") %>%
  rename(BIOREGION_NAME=BIOREGIO_1)

subrealm_bioregions_table <- read_csv("Data/OE_bioregions_subrealm_table.csv")

bioregions.2 <- bioregions %>%
  left_join(., subrealm_bioregions_table)

plot(bioregions.2)

subrealms <- bioregions.2 %>%
  group_by(subrealm) %>%
  summarize() %>%
  ungroup() %>%
  st_as_sf()

plot(subrealms)

st_write(subrealms, "Data/OE_subrealms/OE_subrealms.shp")
