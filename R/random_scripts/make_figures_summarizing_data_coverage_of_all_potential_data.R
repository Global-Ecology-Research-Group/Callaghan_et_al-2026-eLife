# make a figure summarizing data coverage of the analysis data (Table S3)
# packages
library(dplyr)
library(ggplot2)
library(GGally)
library(forcats)
library(tidyr)
library(ggcorrplot)
library(sf)


analysis_data <- readRDS("analysis_data/analysis_data_harmonized.RDS") %>%
  dplyr::select(1:9, 12, 15:18, 20) %>%
  dplyr::filter(complete.cases(subrealm)) %>%
  rename(urban_tolerance=mean_urban_score_viirs) %>%
  dplyr::filter(metadata %in% unique(readRDS("intermediate_results/model_metadata_usage_summary/metadata_summary_all_2_viirs.RDS")$metadata)) %>%
  arrange(kingdom, metadata, subrealm) %>%
  dplyr::filter(complete.cases(subrealm)) %>%
  dplyr::filter(complete.cases(urban_tolerance))

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


