### Script to assess resampled SUDs


# packages
library(dplyr)
library(ggplot2)
library(tidyr)

subrealm_name="Australia"

# function to summarize resampling analysis (found in the "R/SUDs/calculate_resampled_SUDs.R" script)
# collect resampling results

collect_resampling_results <- function(subrealm_name){
  
  # read in data for our urban scores
  urban_scores <- readRDS("urban_scores/subrealm_potential_urban_scores.RDS") %>%
    dplyr::filter(subrealm==subrealm_name)
  
  dat_greater_than_0.5 <- readRDS(paste0("Results/SUDs/", subrealm_name, "_greater_than_0.5.RDS"))
  dat_less_than_0.5 <- readRDS(paste0("Results/SUDs/", subrealm_name, "_less_than_0.5.RDS"))
  dat_all <- readRDS(paste0("Results/SUDs/", subrealm_name, "_all.RDS"))
  dat_random <- readRDS(paste0("Results/SUDs/", subrealm_name, "_randomized_pixels.RDS"))
  
  observed_skew <- e1071::skewness(urban_scores$mean_urban_score_viirs, na.rm=TRUE)
  
  resampled_skew_all <- dat_all %>%
    group_by(draw) %>%
    summarize(skewness=e1071::skewness(mean_urban_score_viirs, na.rm=TRUE)) %>%
    mutate(approach="all")
  
  resampled_skew_less_than_0.5 <- dat_less_than_0.5 %>%
    group_by(draw) %>%
    summarize(skewness=e1071::skewness(mean_urban_score_viirs, na.rm=TRUE)) %>%
    mutate(approach="< 0.5")
  
  resampled_skew_greater_than_0.5 <- dat_greater_than_0.5 %>%
    group_by(draw) %>%
    summarize(skewness=e1071::skewness(mean_urban_score_viirs, na.rm=TRUE)) %>%
    mutate(approach="> 0.5")
  
  resampled_skew_random <- dat_random %>%
    group_by(draw) %>%
    summarize(skewness=e1071::skewness(mean_urban_score_viirs, na.rm=TRUE)) %>%
    mutate(approach="random")
  
  
  # combine into one dataframe
  summary_df <- resampled_skew_all %>%
    bind_rows(resampled_skew_less_than_0.5) %>%
    bind_rows(resampled_skew_greater_than_0.5) %>%
    bind_rows(resampled_skew_random) %>%
    bind_rows(data.frame(skewness=observed_skew, approach="observed")) %>%
    mutate(subrealm=subrealm_name) %>%
    mutate(number_species=nrow(urban_scores)) %>%
    mutate(number_observations=sum(urban_scores$recorded_count))
  
  return(summary_df)
  
}


# read in and collate all the data
files <- list.files("Results/SUDs")

subrealms <- stringr::word(files, 1, sep="_") %>%
  as.data.frame() %>%
  rename(subrealm=1) %>%
  group_by(subrealm) %>%
  summarize(N=n()) %>%
  dplyr::filter(N==4)

collated_data <- bind_rows(lapply(subrealms$subrealm, collect_resampling_results))

ggplot()+
  geom_boxplot(data=collated_data %>%
                 dplyr::filter(approach!="observed"), aes(x=subrealm, y=skewness, fill=approach), color="black")+
  geom_point(data=collated_data %>%
               dplyr::filter(approach=="observed"), aes(x=subrealm, y=skewness), color="purple")+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~subrealm, scales="free")+
  theme(axis.text.y=element_blank())+
  theme(axis.ticks.y=element_blank())+
  xlab("")+
  ylab("Skewness")


summarized_difference <- collated_data %>%
  group_by(approach, subrealm) %>%
  summarize(mean_skew=mean(skewness)) %>%
  pivot_wider(names_from=approach, values_from=mean_skew) %>%
  mutate(difference=observed-all) %>%
  left_join(., collated_data %>%
              dplyr::select(subrealm, number_species, number_observations) %>%
              distinct())

ggplot(summarized_difference, aes(x=difference))+
  geom_histogram()+
  theme_bw()


ggplot(summarized_difference, aes(x=difference, y=number_observations))+
  geom_point()+
  theme_bw()+
  scale_y_log10()

ggplot(summarized_difference, aes(x=difference, y=number_species))+
  geom_point()+
  theme_bw()+
  scale_y_log10()

