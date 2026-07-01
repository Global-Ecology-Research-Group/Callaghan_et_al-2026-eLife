# this script is used to summarize the 'analysis data'
# all the data that matches between both urban scores and for which we have body size data

# packages
library(dplyr)
library(ggplot2)
library(GGally)
library(forcats)
library(tidyr)
library(ggcorrplot)
library(sf)

analysis_data <- readRDS("analysis_data/analysis_data_harmonized.RDS") %>%
  dplyr::filter(complete.cases(subrealm)) %>%
  dplyr::filter(complete.cases(mean_urban_score_viirs))

table(analysis_data$kingdom)
table(analysis_data$class)
table(analysis_data$family)

length(unique(analysis_data$species))
length(unique(analysis_data$subrealm))

# read in a map of subrealms
subrealm_map <- invisible(st_read("Data/OE_subrealms/OE_subrealms.shp"))

# make a plot of the taxonomic coverage
analysis_data %>%
  group_by(class, kingdom) %>%
  summarize(number_sp=length(unique(species))) %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.)) %>%
  arrange(number_sp) %>%
  dplyr::filter(number_sp>=10) %>%
  ggplot(., aes(x=fct_inorder(class), y=number_sp, fill=kingdom))+
  geom_col()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_y_log10()+
  ylab("Number of taxa (log10)")+
  xlab("")+
  theme(panel.grid=element_blank())+
  scale_fill_manual(values=c("#F68E19", "#82B513"),
                    breaks=c("Animalia", "Plantae"),
                    labels=c("Animalia", "Plantae"))+
  geom_text(aes(label=paste0("(", number_sp, ")"), hjust=1))+
  theme(legend.title=element_blank())+
  theme(legend.position=c(0.8, 0.2))

ggsave("Figures/sample_size_by_class.png", width=7.4, height=6.8, units="in")

# spatial coverage
analysis_data %>%
  group_by(subrealm) %>%
  summarize(number_of_species=n()) %>%
  ggplot(., aes(x=number_of_species))+
  geom_histogram(fill="gray80", bins=40, color="black")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Number of species")+
  ylab("Number of subrealms")



# make plot
# get 12 random subrealms
subrealm_sample <- analysis_data %>%
  group_by(subrealm) %>%
  summarize(N=n()) %>%
  dplyr::filter(N>50) %>%
  sample_n(12)

analysis_data %>%
  dplyr::filter(subrealm %in% subrealm_sample$subrealm) %>%
  ggplot(., aes(x=mean_urban_score_viirs))+
  geom_histogram(fill="gray80", bins=40, color="black")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~subrealm, scales="free", ncol=3)+
  geom_vline(xintercept=0, color="red", linetype="dashed")+
  xlab("Urban affinity (VIIRS urban score)")+
  ylab("Number of species")+
  theme(panel.grid=element_blank())+
  theme(strip.text=element_text(size=7))

ggsave("Figures/urban_score_histogram_example_twelve_random_subrealms.png", width=6.8, height=7.9, units="in")

# VIIRS or GHM?
analysis_data %>%
  dplyr::filter(subrealm %in% subrealm_sample$subrealm) %>%
  ggplot(., aes(x=mean_urban_score_viirs))+
  geom_histogram(fill="gray80", bins=40, color="black")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~subrealm, scales="free")

analysis_data %>%
  dplyr::filter(subrealm %in% subrealm_sample$subrealm) %>%
  ggplot(., aes(x=median_urban_score_viirs))+
  geom_histogram(fill="gray80", bins=40, color="black")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~subrealm, scales="free")

analysis_data %>%
  dplyr::filter(subrealm %in% subrealm_sample$subrealm) %>%
  ggplot(., aes(x=mean_urban_score_ghm))+
  geom_histogram(fill="gray80", bins=40, color="black")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~subrealm, scales="free")

analysis_data %>%
  dplyr::filter(subrealm %in% subrealm_sample$subrealm) %>%
  ggplot(., aes(x=median_urban_score_ghm))+
  geom_histogram(fill="gray80", bins=40, color="black")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~subrealm, scales="free")

analysis_data %>%
  dplyr::filter(subrealm %in% subrealm_sample$subrealm) %>%
  ggplot(., aes(x=mean_urban_score_ghm, y=mean_urban_score_viirs))+
  geom_point(fill="gray80", bins=40, color="black", alpha=0.3)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~subrealm, scales="free")+
  xlab("Urban affinity (GHM)")+
  ylab("Urban affinity (VIIRS)")

###############################################################################
###############################################################################
###############################################################################
# make a plot for an example group (e.g., lepidoptera in great european forests)
lep_example <- analysis_data %>%
  dplyr::filter(subrealm=="Southeast Asian Forests") %>%
  dplyr::filter(order=="Lepidoptera")

ggplot(lep_example %>%
         arrange(mean_urban_score_viirs), aes(x=fct_inorder(species), y=mean_urban_score_viirs))+
  geom_point()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text.y=element_text(face="italic"))+
  ylab("Urban affinity (VIIRS urban score)")+
  xlab("")+
  theme(axis.text.y=element_text(size=6))+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  ggtitle(paste0("Lepidoptera, Southeast Asian Forests; N = ", nrow(lep_example), " species"))

ggsave("Figures/urban_score_ranking_example_lepidoptera.png", width=7.4, height=8, units="in")

bee_example <- analysis_data %>%
  dplyr::filter(subrealm=="Northeast American Forests") %>%
  dplyr::filter(order=="Hymenoptera")

ggplot(bee_example %>%
         arrange(mean_urban_score_viirs), aes(x=fct_inorder(species), y=mean_urban_score_viirs))+
  geom_point()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text.y=element_text(face="italic"))+
  ylab("Urban affinity (VIIRS urban score)")+
  xlab("")+
  theme(axis.text.y=element_text(size=6))+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  ggtitle(paste0("Hymenoptera, Northeast American Forests; N = ", nrow(bee_example), " species"))

ggsave("Figures/urban_score_ranking_example_hymenoptera.png", width=7.4, height=8, units="in")

aster_example <- analysis_data %>%
  dplyr::filter(subrealm=="Scandinavia & West Boreal Forests") %>%
  dplyr::filter(order=="Asterales")

ggplot(aster_example %>%
         arrange(mean_urban_score_viirs), aes(x=fct_inorder(species), y=mean_urban_score_viirs))+
  geom_point()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text.y=element_text(face="italic"))+
  ylab("Urban affinity (VIIRS urban score)")+
  xlab("")+
  theme(axis.text.y=element_text(size=6))+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  ggtitle(paste0("Asterales, Scandinavia & West Boreal Forests; N = ", nrow(aster_example), " species"))

ggsave("Figures/urban_score_ranking_example_asterales.png", width=7.4, height=8, units="in")

######################################################
######################################################
# make a plot of different orders within 1 subrealm
analysis_data %>%
  dplyr::filter(subrealm=="Northeast American Forests") %>%
  group_by(order) %>%
  mutate(number_sp=length(unique(species))) %>%
  dplyr::filter(number_sp>=10) %>%
  ggplot(., aes(x=mean_urban_score_viirs))+
  geom_histogram(fill="gray80", bins=10, color="black")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text=element_text(size=7.23))+
  facet_wrap(~subrealm, scales="free", ncol=3)+
  geom_vline(xintercept=0, color="red", linetype="dashed")+
  xlab("Urban affinity (VIIRS urban score)")+
  ylab("Number of species")+
  theme(panel.grid=element_blank())+
  theme(strip.text=element_text(size=7))+
  facet_wrap(~order, scales="free", ncol=5)

ggsave("Figures/urban_score_histogram_example_Northeast_American_Forests.png", width=7.4, height=8, units="in")

##################################################
##################################################
##################################################
##### Summarize the sample size by order x subrealm
sample_size <- analysis_data %>%
  group_by(order, subrealm) %>%
  summarize(N=n())

# how many total subrealm x order combinations
nrow(sample_size)

# how many subrealms
length(unique(sample_size$subrealm))

# how many orders
length(unique(sample_size$order))

# how many subrealm x order combinations only had 1 representative species
sample_size %>%
  dplyr::filter(N==1) %>%
  nrow()

# the top 5 combinations
sample_size %>%
  ungroup() %>%
  arrange(desc(N)) %>%
  slice(1:10)

# make a plot of sample size
ggplot(sample_size, aes(x=N))+
  geom_histogram(fill="gray80", bins=15, color="black")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Number of taxa")+
  ylab("Number of subrealm x order combinations")+
  theme(panel.grid=element_blank())+
  scale_x_log10()

ggsave("Figures/subrealm_x_order_sample_size.png", height=7.4, width=7.6, units="in")


################################
####### correlation among species that have at least 5 observations
#######

at_least_five_obs <- analysis_data %>%
  group_by(species) %>%
  summarize(number_subrealm=length(unique(subrealm))) %>%
  dplyr::filter(number_subrealm>=5)

cor_mat <- analysis_data %>%
  ungroup() %>%
  dplyr::filter(species %in% local(at_least_five_obs$species)) %>%
  dplyr::select(species, subrealm, mean_urban_score_viirs) %>%
  group_by(subrealm) %>%
  mutate(number_species=length(unique(species))) %>%
  dplyr::filter(number_species>=20) %>%
  ungroup() %>%
  dplyr::select(species, subrealm, mean_urban_score_viirs) %>%
  pivot_wider(names_from=subrealm, values_from=mean_urban_score_viirs) %>%
  dplyr::select(-species) %>%
  cor(., use = "pairwise.complete.obs")

count_mat <- analysis_data %>%
  ungroup() %>%
  dplyr::filter(species %in% local(at_least_five_obs$species)) %>%
  dplyr::select(species, subrealm, mean_urban_score_viirs) %>%
  group_by(subrealm) %>%
  mutate(number_species=length(unique(species))) %>%
  dplyr::filter(number_species>=20) %>%
  ungroup() %>%
  dplyr::select(species, subrealm, mean_urban_score_viirs) %>%
  pivot_wider(names_from=subrealm, values_from=mean_urban_score_viirs) %>%
  dplyr::select(-species) %>%
  pairwiseCount(.)

count_mat[count_mat < 10] <- 0

count_mat[count_mat==0] <- NA

cor_mat[is.na(count_mat)] <- NA

cor_mat %>%
  ggcorrplot()
