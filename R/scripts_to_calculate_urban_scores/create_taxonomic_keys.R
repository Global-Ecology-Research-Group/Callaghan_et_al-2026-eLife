# a quick R script to create the taxonomic keys
# that we can use to (1) assign people to different levels to collect data
# and (2) see the species we need body size data for


library(dplyr)
library(tidyverse)


dat <- readRDS("urban_scores/subrealm_potential_urban_scores.RDS")

plants <- dat %>%
  dplyr::filter(kingdom=="Plantae") %>%
  dplyr::select(1:7) %>%
  distinct() %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.))

write_csv(plants, "taxonomic_keys/plants_all.csv")

animals <- dat %>%
  dplyr::filter(kingdom=="Animalia") %>%
  dplyr::select(1:7) %>%
  distinct() %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.))

write_csv(animals, "taxonomic_keys/animals_all.csv")

animal_class <- dat %>%
  dplyr::filter(kingdom=="Animalia") %>%
  group_by(class) %>%
  mutate(number_sp=length(unique(species))) %>%
  dplyr::select(1:3, number_sp) %>%
  distinct() %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.)) %>%
  mutate(in_charge=NA)

write_csv(animal_class, "taxonomic_keys/animals_class.csv")


insects_order <- dat %>%
  dplyr::filter(kingdom=="Animalia") %>%
  dplyr::filter(class=="Insecta") %>%
  group_by(order) %>%
  mutate(number_sp=length(unique(species))) %>%
  dplyr::select(1:4, number_sp) %>%
  distinct() %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.)) %>%
  mutate(in_charge=NA)

write_csv(insects_order, "taxonomic_keys/insects_order.csv")

lepidoptera_family <- dat %>%
  dplyr::filter(kingdom=="Animalia") %>%
  dplyr::filter(class=="Insecta") %>%
  dplyr::filter(order=="Lepidoptera") %>%
  group_by(family) %>%
  mutate(number_sp=length(unique(species))) %>%
  dplyr::select(1:5, number_sp) %>%
  distinct() %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.)) %>%
  mutate(in_charge=NA)

write_csv(lepidoptera_family, "taxonomic_keys/lepidoptera_family.csv")

coleoptera_family <- dat %>%
  dplyr::filter(kingdom=="Animalia") %>%
  dplyr::filter(class=="Insecta") %>%
  dplyr::filter(order=="Coleoptera") %>%
  group_by(family) %>%
  mutate(number_sp=length(unique(species))) %>%
  dplyr::select(1:5, number_sp) %>%
  distinct() %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.)) %>%
  mutate(in_charge=NA)

write_csv(coleoptera_family, "taxonomic_keys/coleoptera_family.csv")

diptera_family <- dat %>%
  dplyr::filter(kingdom=="Animalia") %>%
  dplyr::filter(class=="Insecta") %>%
  dplyr::filter(order=="Diptera") %>%
  group_by(family) %>%
  mutate(number_sp=length(unique(species))) %>%
  dplyr::select(1:5, number_sp) %>%
  distinct() %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.)) %>%
  mutate(in_charge=NA)

write_csv(diptera_family, "taxonomic_keys/diptera_family.csv")

hymenoptera_family <- dat %>%
  dplyr::filter(kingdom=="Animalia") %>%
  dplyr::filter(class=="Insecta") %>%
  dplyr::filter(order=="Hymenoptera") %>%
  group_by(family) %>%
  mutate(number_sp=length(unique(species))) %>%
  dplyr::select(1:5, number_sp) %>%
  distinct() %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.)) %>%
  mutate(in_charge=NA)

write_csv(hymenoptera_family, "taxonomic_keys/hymenoptera_family.csv")


hemiptera_family <- dat %>%
  dplyr::filter(kingdom=="Animalia") %>%
  dplyr::filter(class=="Insecta") %>%
  dplyr::filter(order=="Hemiptera") %>%
  group_by(family) %>%
  mutate(number_sp=length(unique(species))) %>%
  dplyr::select(1:5, number_sp) %>%
  distinct() %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.)) %>%
  mutate(in_charge=NA)

write_csv(hemiptera_family, "taxonomic_keys/hemiptera_family.csv")




