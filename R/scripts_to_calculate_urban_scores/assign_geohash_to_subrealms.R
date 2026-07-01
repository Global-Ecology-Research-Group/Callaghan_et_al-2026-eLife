library(sf)
library(dplyr)
library(readr)

# bioregions
subrealms <- st_read("Data/OE_subrealms/OE_subrealms.shp")

# function to read in shapefile and
# intersect and then export as csv
assign_subrealm_function <- function(chunk_number){
  
  message(paste0("Analyzing chunk number ", chunk_number))
  
  shp <- st_read(paste0("Data/gbif_geohash_", chunk_number, "/gbif_geohash_", chunk_number, ".shp")) %>%
    mutate(row.id=1:nrow(.))
  
  inter <- shp %>%
    st_within(st_make_valid(subrealms)) %>%
    as.data.frame() %>%
    right_join(., shp %>%
                 st_set_geometry(NULL) %>%
                 dplyr::select(row.id, geohash7), by="row.id") %>%
    left_join(., subrealms %>%
                mutate(col.id=1:nrow(.)) %>%
                st_set_geometry(NULL) %>%
                dplyr::select(col.id, subrealm), by="col.id")
  
  write_csv(inter, paste0("Data/geohash_subrealms_joined/gbif_geohash_", chunk_number, ".csv"))
  
}

lapply(c(1:22), assign_subrealm_function)
