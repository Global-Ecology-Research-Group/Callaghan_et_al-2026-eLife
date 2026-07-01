# simulation to test the effect of a random effect for 'family'
# simulate two 'families' that belong to the same 'order'

library(ggplot2)
library(brms)
library(dplyr)
library(tidybayes)
library(patchwork)

set.seed(610)

family_1_ut <- rnorm(100, 3.4, 2.5)
family_1_body <- rnorm(100, 8, 1)

family_2_ut <- rnorm(100, -3, 2.5)
family_2_body <- rnorm(100, 3, 1)

example_dat <- data.frame(body_size=c(family_1_body, family_2_body),
                          urban_tolerance=c(family_1_ut, family_2_ut)) %>%
  mutate(family=c(rep("family_1", 100), rep("family_2", 100)))

sim_data <- ggplot(example_dat, aes(x=body_size, y=urban_tolerance, color=family, shape=family))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color='black'))+
  xlab("Body size")+
  ylab("Urban tolerance")+
  scale_color_brewer(palette = "Dark2")

sim_data

# family 1 model
fam1_mod <- brm(urban_tolerance ~ body_size,
                data=example_dat %>%
                  dplyr::filter(family=="family_1"))

summary(fam1_mod)

# family 2 model
fam2_mod <- brm(urban_tolerance ~ body_size,
                data=example_dat %>%
                  dplyr::filter(family=="family_2"))

summary(fam2_mod)

# create dataframe of posterior distribution for both families
fam_plots <- fam1_mod %>%
  spread_draws(b_body_size) %>%
  mutate(family="family_1") %>%
  bind_rows(fam2_mod %>%
              spread_draws(b_body_size) %>%
              mutate(family="family_2"))

# model without random effect for family
mod1 <- brm(urban_tolerance ~ body_size,
            data=example_dat)

# model with random effect for family
mod2 <- brm(urban_tolerance ~ body_size + (1|family),
            data=example_dat)

summary(mod1)

summary(mod2)

# create dataframe of posterior distribution for model 1
mod1_draws <- mod1 %>%
  spread_draws(b_body_size) %>%
  mutate(type="no random effect")

mod2_draws <- mod2 %>%
  spread_draws(b_body_size) %>%
  mutate(type="random effect")

effect_vs_no_effect <- mod1_draws %>%
  bind_rows(mod2_draws) %>%
  ggplot(., aes(x=b_body_size))+
  geom_density(aes(fill=type), alpha=0.6)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_brewer(palette = "Pastel1")+
  ylab("Density")+
  xlab("Estimate")

effect_vs_no_effect

fam_plot <- ggplot(fam_plots, aes(x=b_body_size))+
  geom_density(aes(fill=family), alpha=0.6)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_brewer(palette = "Dark2")+
  ylab("Density")+
  xlab("Estimate")

fam_plot

sim_data + fam_plot + effect_vs_no_effect + plot_layout(ncol=1)

ggsave("Figures/model_simulation_of_random_effects.png", height=7.9, width=6.9, units="in")

# Now do a slightly more complicated simulation
# to test the influence of including a 'random effect' for
# something with only 2 levels - i.e., subrealm
family_1_ut <- rnorm(100, 3.4, 2.5)
family_1_body <- rnorm(100, 8, 1)

family_2_ut <- rnorm(100, -3, 2.5)
family_2_body <- rnorm(100, 3, 1)

example_dat_sub1 <- data.frame(body_size=c(family_1_body, family_2_body),
                          urban_tolerance=c(family_1_ut, family_2_ut)) %>%
  mutate(family=c(rep("family_1", 100), rep("family_2", 100))) %>%
  mutate(sp_id=c(rep(c(1:100), 2))) %>%
  mutate(subrealm="subrealm_1")

family_1_ut <- rnorm(100, 3.4, 2.5)
family_1_body <- rnorm(100, 8, 1)

family_2_ut <- rnorm(100, -3, 2.5)
family_2_body <- rnorm(100, 3, 1)

example_dat_sub2 <- data.frame(body_size=c(family_1_body, family_2_body),
                               urban_tolerance=c(family_1_ut, family_2_ut)) %>%
  mutate(family=c(rep("family_1", 100), rep("family_2", 100))) %>%
  mutate(sp_id=c(rep(c(1:50, 101:150), 2))) %>%
  mutate(subrealm="subrealm_2")

# join data to one simulated dataset
example_dat_v2 <- example_dat_sub1 %>%
  bind_rows(example_dat_sub2)

sim_data_v2 <- ggplot(example_dat_v2, aes(x=body_size, y=urban_tolerance, shape=family))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color='black'))+
  xlab("Body size")+
  ylab("Urban tolerance")+
  facet_wrap(~subrealm)

sim_data_v2

# model with random effect for subrealm (despite having only 2 levels)
mod1_v2 <- brm(urban_tolerance ~ body_size + (1|subrealm),
            data=example_dat_v2)

# model with subrealm as a fixed effect
mod2_v2 <- brm(urban_tolerance ~ body_size + subrealm,
            data=example_dat_v2)

summary(mod1_v2)

summary(mod2_v2)

fixed_vs_random <- mod1_v2 %>%
  spread_draws(b_body_size) %>%
  mutate(subrealm="random effect") %>%
  bind_rows(mod2_v2 %>%
              spread_draws(b_body_size) %>%
              mutate(subrealm="fixed effect"))


ggplot(fixed_vs_random, aes(x=b_body_size))+
  geom_density(aes(fill=subrealm), alpha=0.6)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_brewer(palette = "Dark2")+
  ylab("Density")+
  xlab("Estimate")







# test that the 'individual' family level models
# correspond well with the 'random effect' family level models
test <- mod2 %>%
  spread_draws(r_family[family,term])

ggplot(test, aes(x=r_family))+
  geom_density(aes(fill=family), alpha=0.6)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_brewer(palette = "Dark2")+
  ylab("Density")+
  xlab("Estimate")

