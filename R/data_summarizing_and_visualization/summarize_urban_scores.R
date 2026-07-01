# this script is used to summarize the urban scores data
# only the data for urban scores

# packages
library(dplyr)
library(ggplot2)
library(GGally)

# read in all response variables (urban scores)
urban_scores <- readRDS("urban_scores/subrealm_potential_urban_scores.RDS") %>%
  dplyr::filter(complete.cases(subrealm))

# how many potential species
length(unique(urban_scores$species))

# how many potential classes
length(unique(urban_scores$class))

# how many subrealms 
length(unique(urban_scores$subrealm))

# total number of observations
nrow(urban_scores)

# how many species had more than one subrealm?
species_subrealm_dat <- urban_scores %>%
  group_by(species) %>% 
  summarize(N=n())

summary(species_subrealm_dat$N)

# percent that only occur in 1 subrealm
species_subrealm_dat %>%
  dplyr::filter(N==1) %>% nrow()/nrow(species_subrealm_dat)*100

ggplot(data=species_subrealm_dat, aes(x=N))+
  geom_histogram(fill="gray80", color="black")+
  ylab("Number of species")+
  xlab("Number of subrealms a species was found in")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))

ggsave("Figures/number_of_subrealms_per_species_urban_tolerance.png", width=5.4, height=4.9, units="in")


# organize and write out a csv to serve as Table S1
# of all the urban tolerance scores
urban_scores %>%
  dplyr::select(1:9, 12) %>%
  rename(urban_tolerance=mean_urban_score_viirs) %>%
  arrange(kingdom, subrealm, urban_tolerance) %>%
  write_csv("Tables/Table_S1.csv")


######################### now do some examples of the data to provide visualizations of the data
###############################################################################
###############################################################################
###############################################################################
# make a plot for an example group (e.g., lepidoptera in southeast asian forests)
lep_example <- urban_scores %>%
  dplyr::filter(subrealm=="Southeast Asian Forests") %>%
  dplyr::filter(order=="Lepidoptera")

ggplot(lep_example %>%
         arrange(mean_urban_score_viirs), aes(x=fct_inorder(species), y=mean_urban_score_viirs))+
  geom_point()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text.y=element_text(face="italic"))+
  ylab("Urban affinity")+
  xlab("")+
  theme(axis.text.y=element_text(size=5))+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  ggtitle(paste0("Lepidoptera, Southeast Asian Forests; N = ", nrow(lep_example), " species"))

ggsave("Figures/urban_score_ranking_example_lepidoptera.png", width=7.4, height=9.4, units="in")

bee_example <- urban_scores %>%
  dplyr::filter(subrealm=="Northeast American Forests") %>%
  dplyr::filter(order=="Hymenoptera")

ggplot(bee_example %>%
         arrange(mean_urban_score_viirs), aes(x=fct_inorder(species), y=mean_urban_score_viirs))+
  geom_point()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text.y=element_text(face="italic"))+
  ylab("Urban afffinity")+
  xlab("")+
  theme(axis.text.y=element_text(size=5))+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  ggtitle(paste0("Hymenoptera, Northeast American Forests; N = ", nrow(bee_example), " species"))

ggsave("Figures/urban_score_ranking_example_hymenoptera.png", width=7.4, height=9.4, units="in")

aster_example <- urban_scores %>%
  dplyr::filter(subrealm=="Scandinavia & West Boreal Forests") %>%
  dplyr::filter(order=="Asterales")

ggplot(aster_example %>%
         arrange(mean_urban_score_viirs), aes(x=fct_inorder(species), y=mean_urban_score_viirs))+
  geom_point()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text.y=element_text(face="italic"))+
  ylab("Urban affinity")+
  xlab("")+
  theme(axis.text.y=element_text(size=5))+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  ggtitle(paste0("Asterales, Scandinavia & West Boreal Forests; N = ", nrow(aster_example), " species"))

ggsave("Figures/urban_score_ranking_example_asterales.png", width=7.4, height=9.4, units="in")

