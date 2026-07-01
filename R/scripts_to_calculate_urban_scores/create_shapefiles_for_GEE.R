# This is an R script used to
# create some shapefiles to upload to GEE
# These shapefiles will be of geohash character strings and 
# the average lat and average long of the GBIF records within that
# specific geohash

# get data from eBird
## packages
library(readr)
library(bigrquery)
library(dbplyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)

# create connection with online database
con <- DBI::dbConnect(bigrquery::bigquery(),
                      dataset= "gbif_2021_02_22",
                      project="gbif-data",
                      billing="gbif-data")

# create ebird table
geohash <- tbl(con, 'geohash7_gbif_2021_02_22')

## pull in all
## of the unique geohash and lat lng
## values
geohash_dat <- geohash %>%
  collect(n=Inf)

geohash_dat %>%
  dplyr::slice(1:2000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_1/gbif_geohash_1.shp")

geohash_dat %>%
  dplyr::slice(2000001:4000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_2/gbif_geohash_2.shp")

geohash_dat %>%
  dplyr::slice(4000001:6000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_3/gbif_geohash_3.shp")

geohash_dat %>%
  dplyr::slice(6000001:8000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_4/gbif_geohash_4.shp")

geohash_dat %>%
  dplyr::slice(8000001:10000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_5/gbif_geohash_5.shp")

geohash_dat %>%
  dplyr::slice(10000001:12000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_6/gbif_geohash_6.shp")

geohash_dat %>%
  dplyr::slice(12000001:14000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_7/gbif_geohash_7.shp")

geohash_dat %>%
  dplyr::slice(14000001:16000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_8/gbif_geohash_8.shp")

geohash_dat %>%
  dplyr::slice(16000001:18000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_9/gbif_geohash_9.shp")

geohash_dat %>%
  dplyr::slice(18000001:20000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_10/gbif_geohash_10.shp")

geohash_dat %>%
  dplyr::slice(20000001:22000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_11/gbif_geohash_11.shp")

geohash_dat %>%
  dplyr::slice(22000001:24000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_12/gbif_geohash_12.shp")

geohash_dat %>%
  dplyr::slice(24000001:26000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_13/gbif_geohash_13.shp")

geohash_dat %>%
  dplyr::slice(26000001:28000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_14/gbif_geohash_14.shp")

geohash_dat %>%
  dplyr::slice(28000001:30000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_15/gbif_geohash_15.shp")

geohash_dat %>%
  dplyr::slice(30000001:32000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_16/gbif_geohash_16.shp")

geohash_dat %>%
  dplyr::slice(32000001:34000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_17/gbif_geohash_17.shp")

geohash_dat %>%
  dplyr::slice(34000001:36000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_18/gbif_geohash_18.shp")

geohash_dat %>%
  dplyr::slice(36000001:38000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_19/gbif_geohash_19.shp")

geohash_dat %>%
  dplyr::slice(38000001:40000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_20/gbif_geohash_20.shp")

geohash_dat %>%
  dplyr::slice(40000001:42000000) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_21/gbif_geohash_21.shp")

geohash_dat %>%
  dplyr::slice(42000001:43052017) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_write("Data/gbif_geohash_22/gbif_geohash_22.shp")

















