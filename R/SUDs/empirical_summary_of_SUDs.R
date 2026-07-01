# A script to empirically summarize SUDs
# across different taxonomic groups and geographic realms
# Script to test ideas about the shape of the urban affinity distribution
# to see if it is 'universal' or not...

library(ggplot2)
library(e1071)
library(dplyr)
library(tidyr)
library(purrr)
library(moments)  
library(kSamples)
library(patchwork)
library(tidyverse)
library(ggthemes)

# read in data
analysis_data <- readRDS("analysis_data/analysis_data_harmonized.RDS") %>%
  dplyr::filter(complete.cases(subrealm)) %>%
  dplyr::filter(complete.cases(mean_urban_score_viirs))

urban_scores <- readRDS("urban_scores/subrealm_potential_urban_scores.RDS") %>%
  dplyr::filter(complete.cases(subrealm))

subrealm_sample <- analysis_data %>%
  group_by(subrealm) %>%
  dplyr::summarize(N=n()) %>%
  dplyr::filter(N>100) %>%
  sample_n(12)


urban_scores %>%
  dplyr::filter(subrealm %in% subrealm_sample$subrealm) %>%
  ggplot(., aes(x=mean_urban_score_viirs))+
  geom_histogram(fill="gray80", bins=40, color="black")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~subrealm, scales="free")

# most well-sampled subrealm in terms of number of species
gef <- urban_scores %>%
  dplyr::filter(subrealm=="Great European Forests")

ggplot(gef, aes(x=mean_urban_score_viirs))+
  geom_histogram(alpha=0.5, color="black", fill="gray80")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("urban affinity scores")+
  ylab("Number of species")

ggplot(gef, aes(x=mean_urban_score_viirs))+
  geom_histogram(alpha=0.5, color="black", fill="gray80")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~kingdom, scales="free")+
  xlab("urban affinity scores")+
  ylab("Number of species")

gef %>%
  group_by(class) %>%
  dplyr::mutate(N=n()) %>%
  dplyr::filter(N>=100) %>%
  ggplot(., aes(x=mean_urban_score_viirs))+
  geom_histogram(alpha=0.5, color="black", fill="gray80")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~class, scales="free")+
  xlab("urban affinity scores")+
  ylab("Number of species")

gef %>%
  dplyr::filter(class=="Insecta") %>%
  group_by(order) %>%
  dplyr::mutate(N=n()) %>%
  dplyr::filter(N>=100) %>%
  ggplot(., aes(x=mean_urban_score_viirs))+
  geom_histogram(alpha=0.5, color="black", fill="gray80")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~order, scales="free")+
  xlab("urban affinity scores")+
  ylab("Number of species")

gef %>%
  dplyr::filter(class=="Insecta") %>%
  group_by(order) %>%
  dplyr::mutate(N=n()) %>%
  dplyr::filter(N>=100) %>%
  ggplot(., aes(x=mean_urban_score_viirs, color=order))+
  geom_density(alpha=0.5)+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("urban affinity scores")+
  ylab("Number of species")


# figure out how many species are within the .9 quantile of the histogram
# first for subrealms
# and then for subrealm X order combination
temp <- urban_scores %>%
  dplyr::filter(complete.cases(mean_urban_score_viirs)) %>%
  group_by(subrealm) %>%
  dplyr::mutate(Number_of_species=n()) %>%
  dplyr::filter(Number_of_species>100) %>%
  ungroup() %>%
  group_by(subrealm) %>%
  mutate(quant_0.9=quantile(mean_urban_score_viirs, 0.9)) %>%
  mutate(in_0.9_quantile=mean_urban_score_viirs>=0.9) %>%
  group_by(subrealm) %>%
  summarize(Number_of_species=mean(Number_of_species),
            Number_in_0.9_quantile=sum(in_0.9_quantile==TRUE))

# now write a function to calculate skewness for every subrealm/group combination
skew_function <- function(subrealm_name){
  
  sub_dat <- urban_scores %>%
    dplyr::filter(subrealm==subrealm_name)
  
  skew_all <- e1071::skewness(sub_dat$mean_urban_score_viirs)
  
  skew_kingdom <- sub_dat %>%
    group_by(kingdom) %>%
    dplyr::mutate(N=n()) %>%
    dplyr::filter(N>=50) %>%
    dplyr::summarize(skewness=e1071::skewness(mean_urban_score_viirs, na.rm=TRUE),
              species_richness=n()) %>%
    dplyr::rename(taxonomic_group=1) %>%
    dplyr::mutate(taxonomic_level="kingdom")
  
  skew_class <- sub_dat %>%
    group_by(class) %>%
    dplyr::mutate(N=n()) %>%
    dplyr::filter(N>=50) %>%
    group_by(class) %>%
    dplyr::summarize(skewness=e1071::skewness(mean_urban_score_viirs, na.rm=TRUE),
              species_richness=n()) %>%
    dplyr::rename(taxonomic_group=1) %>%
    dplyr::mutate(taxonomic_level="class")
  
  skew_order <- sub_dat %>%
    dplyr::filter(class=="Insecta") %>%
    group_by(order) %>%
    dplyr::mutate(N=n()) %>%
    dplyr::filter(N>=50) %>%
    group_by(order) %>%
    dplyr::summarize(skewness=e1071::skewness(mean_urban_score_viirs, na.rm=TRUE),
              species_richness=n()) %>%
    dplyr::rename(taxonomic_group=1) %>%
    dplyr::mutate(taxonomic_level="order")
  
  summary_df <- data.frame(taxonomic_group="All",
                           skewness=skew_all,
                           species_richness=nrow(sub_dat),
                           taxonomic_level="All") %>%
    bind_rows(skew_kingdom) %>%
    bind_rows(skew_class) %>%
    bind_rows(skew_order) %>%
    dplyr::mutate(subrealm=subrealm_name)
  
}

subrealms <- urban_scores %>%
  group_by(subrealm) %>%
  dplyr::summarize(N=n()) %>%
  dplyr::filter(N>=100)

# now apply this function to the different subrealms
skewness_summary <- bind_rows(lapply(subrealms$subrealm, skew_function))

ggplot(skewness_summary, 
       aes(x=taxonomic_group, y=skewness))+
  #geom_point()+
  geom_boxplot()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("Skewness")

ggplot(skewness_summary, 
       aes(x=taxonomic_group, y=skewness))+
  #geom_point()+
  geom_boxplot()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("Skewness")+
  facet_wrap(~taxonomic_level, scales="free")


# How many species to uncover the skewness pattern?
# do this just for the most speciose subrealm to start out with.
gef <- urban_scores %>%
  dplyr::filter(subrealm=="Great European Forests") %>%
  ungroup()

# function to resample skewness
resampling_function <- function(sample_size){
  
  boot_fun <- function(draw_number){
    
    samp_dat <- gef %>%
      sample_n(sample_size)
    
    skew <- e1071::skewness(samp_dat$mean_urban_score_viirs)
    
    dat <- data.frame(skewness=skew,
                      draw=draw_number)
    
  }
  
  # do 100 times
  boot_results <- bind_rows(lapply(c(1:100), boot_fun)) %>%
    dplyr::mutate(sample=sample_size)
  
  return(boot_results)
  
}

resampling_results <- bind_rows(lapply(seq(10, 5000, by=10), resampling_function))

ggplot(resampling_results, aes(x=sample, y=skewness))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))

gef_diptera <- urban_scores %>%
  dplyr::filter(subrealm=="Great European Forests") %>%
  dplyr::filter(order=="Diptera") %>%
  ungroup()

# function to resample skewness
resampling_function <- function(sample_size){
  
  boot_fun <- function(draw_number){
    
    samp_dat <- gef_diptera %>%
      sample_n(sample_size)
    
    skew <- e1071::skewness(samp_dat$mean_urban_score_viirs)
    
    dat <- data.frame(skewness=skew,
                      draw=draw_number)
    
  }
  
  # do 100 times
  boot_results <- bind_rows(lapply(c(1:100), boot_fun)) %>%
    mutate(sample=sample_size)
  
  return(boot_results)
  
}

resampling_results <- bind_rows(lapply(seq(10, 400, by=10), resampling_function))

ggplot(resampling_results, aes(x=sample, y=skewness))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))



# make figures to compile together for a figure 1.
# make a map figure to export

world_map = map_data("world") %>% 
  filter(! long > 180)

countries = world_map %>% 
  distinct(region) %>% 
  rowid_to_column()

map <- countries %>% 
  ggplot(aes(map_id = region)) +
  geom_map(map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") +
  theme_map()

map

ggsave("Figures/SUD_shape_figure_stuff/map.png", width=4.5, height=4.2, units="in")

# now get some example SUDs for six different subrealms
# but from different parts of the world
make_subrealm_sud_function <- function(subrealm_name){
  
  gef <- urban_scores %>%
    dplyr::filter(subrealm==subrealm_name) %>%
    dplyr::mutate(title=paste0(subrealm, " N=", nrow(.), " species"))
  
  gef_sud <- ggplot(gef, aes(x=mean_urban_score_viirs))+
    geom_histogram(bins=40, fill="gray80", color="black")+
    theme_bw()+
    theme(axis.text=element_text(color="black"))+
    theme(panel.grid.major=element_blank())+
    theme(panel.grid.minor=element_blank())+
    theme(axis.ticks=element_blank())+
    facet_wrap(~title)+
    ylab("Number of species")+
    xlab("Urban affinity measure")+
    geom_vline(xintercept=0, color="red", linetype="dashed")+
    theme(axis.text.y=element_blank())+
    theme(axis.title.y=element_blank())+
    theme(strip.background=element_blank())+
    theme(panel.border=element_blank())+
    theme(strip.text=element_text(size=7))+
    theme(axis.text.x=element_text(size=4))+
    theme(axis.title.x=element_text(size=6))
  
  gef_sud
  
  ggsave(paste0("Figures/SUD_shape_figure_stuff/", subrealm_name, ".png"), 
         width=2.2, height=2, units="in")
  
}

lapply(c("Great European Forests", "Australia", "Northeast American Forests",
         "Southern Afrotropics", "Mexican Drylands", "North Pacific Coast", 
         "Central America"), make_subrealm_sud_function)

# now get some example SUDs for six different subrealms
# repeat for family
make_subrealm_sud_function <- function(subrealm_name, color){
  
  gef <- urban_scores %>%
    dplyr::filter(subrealm==subrealm_name) %>%
    dplyr::rename("families"="family") %>%
    dplyr::mutate(title=paste0(subrealm, " N=", nrow(.), " families"))
  
  gef_sud <- ggplot(gef, aes(x=mean_urban_score_viirs))+
    geom_histogram(bins=40, fill=color, color="black")+
    theme_bw()+
    theme(axis.text=element_text(color="black"))+
    theme(panel.grid.major=element_blank())+
    theme(panel.grid.minor=element_blank())+
    theme(axis.ticks=element_blank())+
    facet_wrap(~title)+
    ylab("Number of families")+
    xlab("Urban affinity measure")+
    geom_vline(xintercept=0, color="red", linetype="dashed")+
    theme(axis.text.y=element_blank())+
    theme(axis.title.y=element_blank())+
    theme(strip.background=element_blank())+
    theme(panel.border=element_blank())+
    theme(strip.text=element_text(size=11))+
    theme(axis.text.x=element_text(size=8))+
    theme(axis.title.x=element_text(size=10))
  
  gef_sud
  
  ggsave(paste0("Figures/SUD_shape_figure_stuff/", subrealm_name, "_family", ".png"), 
         width=3.5, height=2, units="in")
  
}

subrealm_name <- c("Great European Forests", "Australia", "Northeast American Forests",
                "Southern Afrotropics", "Mexican Drylands", "North Pacific Coast", 
                "Central America", "Southeast Asian Forests")

colors <- c("lightgreen", "orange", "mediumorchid2",
            "firebrick3", "yellow3", "cyan","hotpink",
            "mediumslateblue")

mapply(make_subrealm_sud_function, subrealm_name, colors)


# Now make some stuff for panel B
# first different orders of insects within the great european forests
gef <- urban_scores %>%
  dplyr::filter(subrealm=="Great European Forests")

gef_order <- gef %>%
  dplyr::filter(class=="Insecta") %>%
  group_by(order) %>%
  mutate(N=n()) %>%
  dplyr::filter(N>=100) %>%
  ggplot(., aes(x=mean_urban_score_viirs, color=order))+
  geom_density(alpha=0.5)+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("urban affinity measure")+
  ylab("Density")+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  ggtitle("Orders within the Great European Forests subrealm")+
  geom_vline(xintercept=0, color="red", linetype="dashed")+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  theme(title=element_text(size=8.7)) +
  guides(color = guide_legend(nrow = 2))

gef_order 

ggsave("Figures/SUD_shape_figure_stuff/orders_in_great_european_forests.png", 
       width=3.8, height=3.4, units="in")

# Kindgom figure
mex_dry <- urban_scores %>%
  dplyr::filter(subrealm=="Mexican Drylands")

mex_dry_kingdom <- mex_dry %>%
  ggplot(., aes(x=mean_urban_score_viirs, color=kingdom))+
  geom_density(alpha=0.5)+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("urban affinity measure")+
  ylab("Density")+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  ggtitle("Kingdoms within the Mexican Drylands subrealm")+
  geom_vline(xintercept=0, color="red", linetype="dashed")+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  theme(title=element_text(size=9))

mex_dry_kingdom

ggsave("Figures/SUD_shape_figure_stuff/kingdoms_in_mexican_drylands.png", 
       width=3.8, height=3.4, units="in")

# class figure
aus <- urban_scores %>%
  dplyr::filter(subrealm=="Australia")

aus_class <- aus %>%
  group_by(class) %>%
  mutate(N=n()) %>%
  dplyr::filter(N>=100) %>%
  ggplot(., aes(x=mean_urban_score_viirs, color=class))+
  geom_density(alpha=0.5)+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("urban affinity measure")+
  ylab("Density")+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  ggtitle("Classes within the Australia subrealm")+
  geom_vline(xintercept=0, color="red", linetype="dashed")+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  theme(title=element_text(size=9)) +
  guides(color = guide_legend(nrow = 3)) 

aus_class

ggsave("Figures/SUD_shape_figure_stuff/classes_in_australia.png", 
       width=3.8, height=3.4, units="in")


# make an example SUD to use as a schematic
gef <- urban_scores %>%
  dplyr::filter(subrealm=="Great European Forests")

ggplot(gef, aes(x=mean_urban_score_viirs))+
  geom_histogram(bins=50, fill="gray70", color="white")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  theme(axis.ticks=element_blank())+
  ylab("Number of species")+
  xlab("urban affinity measure")+
  geom_vline(xintercept=0, color="red", linetype="dashed")+
  theme(strip.background=element_blank())+
  theme(panel.border=element_rect(color="white"))+
  theme(axis.text=element_blank())+
  theme(axis.title=element_text(size=8, color="white"))+
  theme(panel.background=element_rect(fill="gray20"))+
  theme(plot.background=element_rect(fill="gray20"))

ggsave("Figures/SUD_shape_figure_stuff/example_for_schematic.png", width=3.7, height=2.1, units="in")

# ------------------------------------------------------
# Determine if distributions are statistically different
# ------------------------------------------------------

# function to calculate Schoener's D
schoeners_d <- function(x, y, n_bins = 50) {
  
  # Remove NAs
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  
  # Common range
  rng <- range(c(x, y))
  
  breaks <- seq(rng[1], rng[2], length.out = n_bins + 1)
  
  h1 <- hist(x, breaks = breaks, plot = FALSE)
  h2 <- hist(y, breaks = breaks, plot = FALSE)
  
  p <- h1$counts / sum(h1$counts)
  q <- h2$counts / sum(h2$counts)
  
  1 - 0.5 * sum(abs(p - q))
}

# safety for AD analysis
safe_ad <- function(x, y) {
  
  out <- tryCatch(
    kSamples::ad.test(x, y),
    error = function(e) NULL
  )
  
  if (is.null(out) || is.null(out$ad)) {
    return(list(statistic = NA_real_, p.value = NA_real_))
  }
  
  # Version 1, asymptotic p-value
  list(
    statistic = out$ad[1, 1],
    p.value   = out$ad[1, 3]
  )
}

taxon_overlap <- function(df, taxon_col) {
  
  taxon_col <- rlang::ensym(taxon_col)
  
  df %>%
    ungroup() %>%
    select(subrealm, !!taxon_col, mean_urban_score_viirs) %>%
    group_by(subrealm) %>%
    nest() %>%
    mutate(
      results = map(data, ~ {
        
        df <- .x
        
        # need at least 2 taxa
        if (n_distinct(df[[rlang::as_string(taxon_col)]]) < 2) {
          return(NULL)
        }
        
        df %>%
          select(!!taxon_col, mean_urban_score_viirs) %>%
          nest(data = mean_urban_score_viirs) %>%
          rename(data_x = data) %>%
          crossing(
            rename(., taxon_y = !!taxon_col, data_y = data_x)
          ) %>%
          filter(!!taxon_col < taxon_y) %>%
          mutate(
            # Schoener's D
            schoener_d = map2_dbl(
              data_x, data_y,
              ~ schoeners_d(.x$mean_urban_score_viirs,
                            .y$mean_urban_score_viirs)
            ),
            
            # KS test
            ks = map2(
              data_x, data_y,
              ~ ks.test(.x$mean_urban_score_viirs,
                        .y$mean_urban_score_viirs)
            ),
            ks_statistic = map_dbl(ks, "statistic"),
            ks_p_value   = map_dbl(ks, "p.value"),
            
            # Kurtosis
            kurtosis_x = map_dbl(
              data_x,
              ~ kurtosis(.x$mean_urban_score_viirs, na.rm = TRUE)
            ),
            kurtosis_y = map_dbl(
              data_y,
              ~ kurtosis(.x$mean_urban_score_viirs, na.rm = TRUE)
            ),
            delta_kurtosis = kurtosis_x - kurtosis_y,
            
            # Anderson–Darling (tail-sensitive)
            ad = map2(
              data_x, data_y,
              ~ safe_ad(
                .x$mean_urban_score_viirs,
                .y$mean_urban_score_viirs
              )
            ),
            ad_statistic = map_dbl(ad, "statistic"),
            ad_p_value   = map_dbl(ad, "p.value")
          ) %>%        # <-- mutate() closes here
          select(
            taxon_x = !!taxon_col,
            taxon_y,
            schoener_d,
            ks_statistic,
            ks_p_value,
            delta_kurtosis,
            ad_statistic,
            ad_p_value
          )
      })
    ) %>%
    select(subrealm, results) %>%
    unnest(results)
}

kingdom_overlap <- taxon_overlap(urban_scores, kingdom)
# only look at classes with greater than 20 observations
class_overlap   <- taxon_overlap(urban_scores %>%  
                                   group_by(subrealm, class) %>%
                                   filter(n() >= 20) %>%
                                   ungroup(), class)
# only look at orders with greater than 20 observations
order_overlap   <- taxon_overlap(urban_scores %>%  
                                   group_by(subrealm, order) %>%
                                   filter(n() >= 20) %>%
                                   ungroup(), order)

# count of urban scores by subrealm

kingdom_counts <- obs_per_realm_kingdom %>%
  filter(kingdom %in% c("Plantae", "Animalia")) %>%
  pivot_wider(
    names_from  = kingdom,
    values_from = count,
    values_fill = 0,
    names_prefix = "n_"
  )

kingdom_overlap <- kingdom_overlap %>%
  left_join(kingdom_counts, by = "subrealm")

# plot the data
# kingdom
hist(kingdom_overlap$schoener_d)
hist(kingdom_overlap$delta_kurtosis)

kingdom_summary <- kingdom_overlap %>%
  group_by(subrealm) %>%
  summarise(
    mean_schoener = mean(schoener_d, na.rm = TRUE)
  ) %>%
  ungroup()

delta_kurt_summary_kingdom <- kingdom_overlap %>%
  group_by(subrealm) %>%
  summarise(
    mean_delta_kurt = mean(delta_kurtosis, na.rm = TRUE)
  ) %>%
  ungroup()

p1 <- ggplot(kingdom_summary, aes(x = subrealm, y = mean_schoener)) +
  geom_point(size = 3, color = "darkgreen") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Schoener's D",
    title = "A"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Delta kurtosis plot
p2 <- ggplot(delta_kurt_summary_kingdom, aes(x = subrealm, y = mean_delta_kurt)) +
  geom_point(size = 3, color = "darkgreen") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Δ Kurtosis",
    title = "B"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank())  # hide y-axis labels on second plot to avoid duplication

# Combine with shared y-axis (subrealm)
p1 + p2 + plot_layout(ncol = 2, widths = c(1, 1))

ggsave("Figures/SUD_shape_figure_stuff/distribution_comparision_kingdom.jpeg", height=6, width=7, units="in")

# plot class
hist(class_overlap$schoener_d)
hist(class_overlap$delta_kurtosis)

class_summary <- class_overlap %>%
  group_by(subrealm) %>%
  summarise(
    mean_schoener = mean(schoener_d, na.rm = TRUE),
    ci_lower = mean_schoener - 1.96 * sd(schoener_d, na.rm = TRUE)/sqrt(n()),
    ci_upper = mean_schoener + 1.96 * sd(schoener_d, na.rm = TRUE)/sqrt(n())
  ) %>%
  ungroup()

delta_kurt_summary_class <- class_overlap %>%
  group_by(subrealm) %>%
  summarise(
    mean_delta_kurt = mean(delta_kurtosis, na.rm = TRUE),
    ci_lower = mean_delta_kurt - 1.96 * sd(delta_kurtosis, na.rm = TRUE)/sqrt(n()),
    ci_upper = mean_delta_kurt + 1.96 * sd(delta_kurtosis, na.rm = TRUE)/sqrt(n())
  ) %>%
  ungroup()

p1 <- ggplot(class_summary, aes(x = subrealm, y = mean_schoener)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "darkgreen") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Mean Schoener's D",
    title = "A"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Delta kurtosis plot
p2 <- ggplot(delta_kurt_summary_class, aes(x = subrealm, y = mean_delta_kurt)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "darkgreen") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Mean Δ Kurtosis",
    title = "B"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank())  # hide y-axis labels on second plot to avoid duplication

# Combine with shared y-axis (subrealm)
p1 + p2 + plot_layout(ncol = 2, widths = c(1, 1))

ggsave("Figures/SUD_shape_figure_stuff/distribution_comparision_class.jpeg", height=6, width=7, units="in")

# plot order
hist(order_overlap$schoener_d)
hist(order_overlap$delta_kurtosis)

order_summary <- order_overlap %>%
  group_by(subrealm) %>%
  summarise(
    mean_schoener = mean(schoener_d, na.rm = TRUE),
    ci_lower = mean_schoener - 1.96 * sd(schoener_d, na.rm = TRUE)/sqrt(n()),
    ci_upper = mean_schoener + 1.96 * sd(schoener_d, na.rm = TRUE)/sqrt(n())
  ) %>%
  ungroup()

delta_kurt_summary_order <- order_overlap %>%
  group_by(subrealm) %>%
  summarise(
    mean_delta_kurt = mean(delta_kurtosis, na.rm = TRUE),
    ci_lower = mean_delta_kurt - 1.96 * sd(delta_kurtosis, na.rm = TRUE)/sqrt(n()),
    ci_upper = mean_delta_kurt + 1.96 * sd(delta_kurtosis, na.rm = TRUE)/sqrt(n())
  ) %>%
  ungroup()

p1 <- ggplot(order_summary, aes(x = subrealm, y = mean_schoener)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "darkgreen") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Mean Schoener's D",
    title = "A"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Delta kurtosis plot
p2 <- ggplot(delta_kurt_summary_order, aes(x = subrealm, y = mean_delta_kurt)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "darkgreen") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Mean Δ Kurtosis",
    title = "B"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank())  # hide y-axis labels on second plot to avoid duplication

# Combine with shared y-axis (subrealm)
p1 + p2 + plot_layout(ncol = 2, widths = c(1, 1))

ggsave("Figures/SUD_shape_figure_stuff/distribution_comparision_order.jpeg", height=6, width=7, units="in")


# Empirical summary of the data

# top 10 subrealms based on kingdom
kingdom_counts <- urban_scores %>%
  group_by(subrealm, kingdom) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  group_by(subrealm) %>%
  summarise(count_total=sum(count)) %>%
  arrange(desc(count_total)) %>%
  slice_head(n=12)

best_kingdom <- urban_scores %>%
  dplyr::filter(subrealm %in% kingdom_counts$subrealm)

best_kingdom %>%
  ggplot(aes(x = kingdom, y = mean_urban_score_viirs)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  facet_wrap(~subrealm, scales = "free",
             labeller = labeller(subrealm = label_wrap_gen(width = 20))) +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() + 
  theme_bw() +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_text(size = 9)) +
  xlab("") +
  ylab("urban affinity measure") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed")

ggsave("Figures/SUD_shape_figure_stuff/example_dist_kingdom.jpeg", height=6, width=8, units="in")


# top 10 subrealms based on class
class_counts <- urban_scores %>%
  filter(complete.cases(class), class != "NA") %>%
  group_by(subrealm, class) %>%
  summarise(count=n()) %>%
  dplyr::filter(count>=50) %>%
  ungroup() %>%
  group_by(subrealm) %>%
  summarise(count_total=sum(count)) %>%
  arrange(desc(count_total)) %>%
  slice_head(n=12)

class_subrealm <- urban_scores %>%
  dplyr::filter(subrealm %in% class_counts$subrealm,
                !is.na(class)) %>%
  group_by(subrealm, class) %>%
  add_count(name = "number_class") %>%
  ungroup()

class_subrealm %>%
  filter(number_class > 50) %>%
  ggplot(aes(x = class, y = mean_urban_score_viirs)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  facet_wrap(~subrealm, scales = "free",
             labeller = labeller(subrealm = label_wrap_gen(width = 20)),
             ncol=3) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(drop = TRUE) +
  coord_flip() + 
  theme_bw() +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_text(size = 9)) +
  xlab("") +
  ylab("urban affinity measure") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed")

ggsave("Figures/SUD_shape_figure_stuff/example_dist_class.jpeg", height=8, width=8, units="in")


# top 10 subrealms based on order
order_counts <- urban_scores %>%
  filter(complete.cases(order), class != "NA") %>%
  group_by(subrealm, order) %>%
  summarise(count=n()) %>%
  dplyr::filter(count>=50) %>%
  ungroup() %>%
  group_by(subrealm) %>%
  summarise(count_total=sum(count)) %>%
  arrange(desc(count_total)) %>%
  slice_head(n=9)

order_subrealm <- urban_scores %>%
  dplyr::filter(subrealm %in% order_counts$subrealm,
                !is.na(order)) %>%
  group_by(subrealm, order) %>%
  add_count(name = "number_order") %>%
  ungroup()

order_subrealm %>%
  filter(number_order > 50) %>%
  ggplot(aes(x = order, y = mean_urban_score_viirs)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  facet_wrap(~subrealm, scales = "free",
             labeller = labeller(subrealm = label_wrap_gen(width = 20)),
             ncol=3) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(drop = TRUE) +
  coord_flip() + 
  theme_bw() +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_text(size = 9)) +
  xlab("") +
  ylab("urban affinity measure") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed")

ggsave("Figures/SUD_shape_figure_stuff/example_dist_order.jpeg", height=10, width=8, units="in")
