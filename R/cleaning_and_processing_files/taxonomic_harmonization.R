# This script is used to do some taxonomic data harmonization!
# Written by the immaculate Ingmar Staude
# split into two chunks: plants and animals

# packages
library(tidyverse)
library(data.table)
library(taxize)
library(rgbif)

# read in all response variables (urban scores)
urban_scores <- readRDS("urban_scores/subrealm_potential_urban_scores.RDS")

# read in all predictor variables (body size)
body_size <- readRDS("body_size_data_all/all_body_size.RDS")

##############################################################################
##############################################################################
# PLANTAE -----------------------------------------------------------------
# harmonization code for plants
# given that urban scores are already retrieved from a GBIF based taxonomy
# we will only check the taxononmy of the body size data frame.

# create species vector
species <- body_size %>% 
  filter(kingdom == "Plantae") %>% 
  select(species) %>% 
  distinct() %>% 
  arrange(species) %>%
  pull() %>%
  na.omit()

# run harmonization code - assuming we only want vascular plants here.
df <- list()
for(i in 1:length(species)){
  
  temp <- get_gbifid_(species[i])[[1]] 
  if(nrow(temp)>=1){
    temp <- temp %>% 
      filter(kingdom == "Plantae") %>% 
      filter(phylum %in% c("Tracheophyta", "Bryophyta", "Marchantiophyta")) %>% 
      filter(!(rank == "genus"| rank == "family"| rank == "order"| rank == "phylum"))
  } else {temp <- temp} 
  
  
  if(nrow(temp) == 0){next} else{
    if(nrow(temp)>=1 & temp %>% filter(matchtype == "EXACT") %>% nrow >= 1){
      
      temp <-  temp %>% 
        filter(kingdom == "Plantae") %>% 
        filter(phylum %in% c("Tracheophyta", "Bryophyta", "Marchantiophyta")) %>% 
        filter(matchtype == "EXACT")
      
      if(temp %>% filter(confidence == 100) %>% nrow == 1) {
        
        temp <- temp %>% filter(confidence == 100) 
        
        temp2 <-  temp %>% 
          filter(row_number()== 1 ) %>% 
          select_if(~ !any(is.na(.)))
        
        if(length(temp2$specieskey) >= 1){
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp$scientificname,
            speciesKey = ifelse(length(temp$specieskey) == 1, temp$specieskey, NA) ,
            species = ifelse(length(temp$species) == 1, temp$species, NA) , 
            phylum = temp$phylum,
            rank = temp$rank,
            status = temp$status)
        } else 
          
        {
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp$scientificname[z],
            speciesKey = ifelse(temp$rank == "subspecies", NA, temp$usagekey[z]),
            species = ifelse(temp$rank == "subspecies", NA, temp$canonicalname[z]), 
            phylum = temp$phylum[z],
            rank = temp$rank[z],
            status = temp$status[z]) 
        }
        
      } else {
        
        hc <-c()
        
        for(k in 1:nrow(temp)) {
          hc[k] <- occ_count(temp$usagekey[k])
        }
        z <- first(which.max(hc))
        
        temp2 <-  temp %>% 
          filter(row_number()== z ) %>% 
          select_if(~ !any(is.na(.)))
        
        if(length(temp2$specieskey[z]) > 0){
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp$scientificname[z],
            speciesKey = ifelse(length(temp$specieskey[z]) == 1, temp$specieskey[z], NA) ,
            species = ifelse(length(temp$species[z]) == 1, temp$species[z], NA) , 
            phylum = temp$phylum[z],
            rank = temp$rank[z],
            status = temp$status[z]) 
        } else {
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp2$scientificname[z],
            speciesKey = ifelse(temp2$rank == "subspecies", NA, temp2$usagekey[z]),
            species = ifelse(temp2$rank == "subspecies", NA, temp2$canonicalname[z]), 
            phylum = temp2$phylum[z],
            rank = temp2$rank[z],
            status = temp2$status[z]) 
        }
        
      }} else {
        
        if(nrow(temp)>=1){
          
          temp <-  temp %>% 
            filter(kingdom == "Plantae") %>% 
            filter(phylum == "Tracheophyta") %>% 
            filter(!(rank == "genus"| rank == "family"| rank == "order"| rank == "phylum")) %>% 
            filter(row_number()==1 )
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp$scientificname,
            speciesKey = ifelse(length(temp$specieskey) == 1, temp$specieskey, NA) ,
            species = ifelse(length(temp$species) == 1, temp$species, NA) , 
            phylum = temp$phylum,
            rank = temp$rank,
            status = temp$status)
        } else {
          next
        }
      }}
}

# create a dataframe
bodysize_plantae_harmonized <- bind_rows(df)

# save and load
saveRDS(bodysize_plantae_harmonized, file = "analysis_data/bodysize_plantae_harmonized.RDS")

# this df has the following items: 
# Taxon = the original input name from TRY, 
# scientificname = is the scientificname from the input Taxon, it's not the accepted name!
# speciesKey = the GBIF identifier for the accepted species name, you can simply do
# e.g., https://www.gbif.org/species/3092961 to find the species with the speciesKey
# species = contains the accepted species name
# and the rest speaks for itself
head(bodysize_plantae_harmonized)

table(bodysize_plantae_harmonized$phylum)

############################################################################
############################################################################
############ Now essentially repeat the same thing, but for animalia
# ANIMALIA ----------------------------------------------------------------
# harmonization code for animalia
# given that urban scores are already retrieved from a GBIF based taxonomy
# we will only check the taxononmy of the body size data frame.
# A few species gives an error in the code, I don't know why
# and no way to really 'know' ahead of time until the code runs and it quits at that specific species
# then needs to be removed. This was only for 3 species, and so these three are removed here with a filter call
# if more errors as more data are added arise then can add those species here
species <- body_size %>% 
  filter(kingdom == "Animalia") %>% 
  select(species) %>% 
  distinct() %>% 
  arrange(species) %>%
  pull() %>% 
  na.omit()

# run harmonization code 
df <- list()
for(i in 1:length(species)){
  
  temp <- get_gbifid_(species[i])[[1]] 
  if(nrow(temp)>=1){
    temp <- temp %>% 
      filter(kingdom == "Animalia") %>% 
      filter(!(rank == "genus"| rank == "family"| rank == "order"| rank == "phylum"))
  } else {temp <- temp} 
  
  
  if(nrow(temp) == 0){next} else{
    if(nrow(temp)>=1 & temp %>% filter(matchtype == "EXACT") %>% nrow >= 1){
      
      temp <-  temp %>% 
        filter(kingdom == "Animalia") %>% 
        filter(matchtype == "EXACT")
      
      if(temp %>% filter(confidence == 100) %>% nrow == 1) {
        
        temp <- temp %>% filter(confidence == 100) 
        
        temp2 <-  temp %>% 
          filter(row_number()== 1) %>% 
          select_if(~ !any(is.na(.)))
        
        if(length(temp2$specieskey) >= 1){
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp$scientificname,
            speciesKey = ifelse(length(temp$specieskey) == 1, temp$specieskey, NA) ,
            species = ifelse(length(temp$species) == 1, temp$species, NA) , 
            phylum = temp$phylum,
            rank = temp$rank,
            status = temp$status)
        } else 
          
        {
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp$scientificname[z],
            speciesKey = ifelse(temp$rank == "subspecies", NA, temp$usagekey[z]),
            species = ifelse(temp$rank == "subspecies", NA, temp$canonicalname[z]), 
            phylum = temp$phylum[z],
            rank = temp$rank[z],
            status = temp$status[z]) 
        }
        
      } else {
        
        hc <-c()
        
        for(k in 1:nrow(temp)) {
          hc[k] <- occ_count(temp$usagekey[k])
        }
        z <- first(which.max(hc))
        
        temp2 <-  temp %>% 
          filter(row_number()== z ) %>% 
          select_if(~ !any(is.na(.)))
        
        if(length(temp2$specieskey[z]) > 0){
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp$scientificname[z],
            speciesKey = ifelse(length(temp$specieskey[z]) == 1, temp$specieskey[z], NA) ,
            species = ifelse(length(temp$species[z]) == 1, temp$species[z], NA) , 
            phylum = ifelse(length(temp$phylum[z]) == 1, temp$phylum[z], NA),
            rank = temp$rank[z],
            status = temp$status[z]) 
        } else {
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp2$scientificname[z],
            speciesKey = ifelse(temp2$rank == "subspecies", NA, temp2$usagekey[z]),
            species = ifelse(temp2$rank == "subspecies", NA, temp2$canonicalname[z]), 
            phylum = ifelse(length(temp2$phylum[z]) == "subspecies", temp2$phylum[z], NA),
            rank = temp2$rank[z],
            status = temp2$status[z]) 
        }
        
      }} else {
        
        if(nrow(temp)>=1){
          
          temp <-  temp %>% 
            filter(kingdom == "Animalia") %>% 
            filter(!(rank == "genus"| rank == "family"| rank == "order"| rank == "phylum")) %>% 
            filter(row_number()==1 )
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp$scientificname,
            speciesKey = ifelse(length(temp$specieskey) == 1, temp$specieskey, NA) ,
            species = ifelse(length(temp$species) == 1, temp$species, NA) , 
            phylum = ifelse(length(temp$phylum) == 1, temp$phylum, NA),
            rank = temp$rank,
            status = temp$status)
        } else {
          next
        }
      }}
}

# create a dataframe
bodysize_animalia_harmonized <- bind_rows(df)

saveRDS(bodysize_animalia_harmonized , "analysis_data/bodysize_animalia_harmonized.RDS")

# 99456 unique accepted species
length(unique(bodysize_animalia_harmonized$species))

