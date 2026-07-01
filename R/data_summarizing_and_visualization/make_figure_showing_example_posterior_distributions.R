# a script to make an example figure of the
# results using posterior distributions
# it is manually made, by interpreting the key overall results
# and manually selecting differences

# packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidybayes)
library(brms)
library(patchwork)
library(forcats)

# read in top level results
# kingdom level
animals <- readRDS("model_objects/kingdom_level/Animalia_viirs_type_1.rds")
plants <- readRDS("model_objects/kingdom_level/Plantae_viirs_type_3.rds")

# get the fixed effect posterior distribution
animals_draws <- animals %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(kingdom="Animalia")

plants_draws <- plants %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(kingdom="Plantae")

kingdom_level <- animals_draws %>%
  bind_rows(plants_draws) %>%
  mutate(kingdom=factor(kingdom, levels=c("Plantae", "Animalia"))) %>%
  ggplot(., aes(x=b_body_size_scaled_log10, fill=kingdom))+
  geom_density()+
  theme_minimal()+
  facet_wrap(~kingdom, ncol=2, scales="free")+
  scale_fill_brewer(palette="Set2")+
  guides(fill="none")+
  theme(axis.text=element_text(color="black"))+
  theme(axis.ticks.y=element_blank())+
  theme(axis.text.y=element_blank())+
  theme(axis.title.y=element_blank())+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  geom_vline(xintercept=0, color="gray30", linetype="dashed")+
  xlab("")

kingdom_level

# now repeat the above for class level
aves <- readRDS("model_objects/class_level/Aves_viirs_type_1.rds")
reptilia <- readRDS("model_objects/class_level/Reptilia_viirs_type_1.rds")
insecta <- readRDS("model_objects/class_level/Insecta_viirs_type_1.rds")
arachnida <- readRDS("model_objects/class_level/Arachnida_viirs_type_1.rds")
magnoliopsida <- readRDS("model_objects/class_level/Magnoliopsida_viirs_type_3.rds")
liliopsida <- readRDS("model_objects/class_level/Liliopsida_viirs_type_3.rds")

aves_draws <- aves %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(class="Aves") %>%
  mutate(kingdom="Animalia")

reptilia_draws <- reptilia %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(class="Reptilia") %>%
  mutate(kingdom="Animalia")

insecta_draws <- insecta %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(class="Insecta") %>%
  mutate(kingdom="Animalia")

arachnida_draws <- arachnida %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(class="Arachnida") %>%
  mutate(kingdom="Animalia")

magnoliopsida_draws <- magnoliopsida %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(class="Magnoliopsida") %>%
  mutate(kingdom="Plantae")

liliopsida_draws <- liliopsida %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(class="Liliopsida") %>%
  mutate(kingdom="Plantae")

class_level <- magnoliopsida_draws %>%
  bind_rows(liliopsida_draws) %>%
  bind_rows(aves_draws) %>%
  bind_rows(insecta_draws) %>%
  mutate(class=factor(class, levels=c("Liliopsida", "Magnoliopsida", "Insecta", "Aves"))) %>%
  mutate(kingdom=factor(kingdom, levels=c("Plantae", "Animalia"))) %>%
  ggplot(., aes(x=b_body_size_scaled_log10, fill=kingdom))+
  geom_density()+
  theme_minimal()+
  facet_wrap(~class, ncol=4, scales="free")+
  scale_fill_brewer(palette="Set2")+
  guides(fill="none")+
  theme(axis.text=element_text(color="black"))+
  theme(axis.ticks.y=element_blank())+
  theme(axis.text.y=element_blank())+
  theme(axis.title.y=element_blank())+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  geom_vline(xintercept=0, color="gray30", linetype="dashed")+
  xlab("")

class_level

kingdom_level+class_level+plot_layout(ncol=1)

# now repeat the above for order level
lepidoptera <- readRDS("model_objects/order_level/Lepidoptera_viirs_type_1.rds")
coleoptera <- readRDS("model_objects/order_level/Coleoptera_viirs_type_1.rds")
odonata <- readRDS("model_objects/order_level/Odonata_viirs_type_1.rds")
hymenoptera <- readRDS("model_objects/order_level/Hymenoptera_viirs_type_1.rds")
passeriformes <- readRDS("model_objects/order_level/Passeriformes_viirs_type_1.rds")
apodiformes <- readRDS("model_objects/order_level/Apodiformes_viirs_type_1.rds")
poales <- readRDS("model_objects/order_level/Poales_viirs_type_3.rds")
asparagales <- readRDS("model_objects/order_level/Asparagales_viirs_type_3.rds")
rosales <- readRDS("model_objects/order_level/Rosales_viirs_type_3.rds")
fabales <- readRDS("model_objects/order_level/Fabales_viirs_type_3.rds")

lepidoptera_draws <- lepidoptera %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Lepidoptera") %>%
  mutate(kingdom="Animalia")

coleoptera_draws <- coleoptera %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Coleoptera") %>%
  mutate(kingdom="Animalia")

odonata_draws <- odonata %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Odonata") %>%
  mutate(kingdom="Animalia")

hymenoptera_draws <- hymenoptera %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Hymenoptera") %>%
  mutate(kingdom="Animalia")

fabales_draws <- fabales %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Fabales") %>%
  mutate(kingdom="Plantae")

rosales_draws <- rosales %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Rosales") %>%
  mutate(kingdom="Plantae")

asparagales_draws <- asparagales %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Asparagales") %>%
  mutate(kingdom="Plantae")

poales_draws <- poales %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Poales") %>%
  mutate(kingdom="Plantae")

apodiformes_draws <- apodiformes %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Apodiformes") %>%
  mutate(kingdom="Animalia")

passeriformes_draws <- passeriformes %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Passeriformes") %>%
  mutate(kingdom="Animalia")

order_level <- lepidoptera_draws %>%
  bind_rows(hymenoptera_draws) %>%
  bind_rows(passeriformes_draws) %>%
  bind_rows(apodiformes_draws) %>% 
  bind_rows(poales_draws) %>%
  bind_rows(asparagales_draws) %>%
  bind_rows(rosales_draws) %>%
  bind_rows(fabales_draws) %>%
  mutate(order=factor(order, levels=c("Poales", "Asparagales", 
                                      "Rosales", "Fabales",
                                      "Lepidoptera", "Hymenoptera",
                                      "Passeriformes", "Apodiformes"))) %>%
  mutate(kingdom=factor(kingdom, levels=c("Plantae", "Animalia"))) %>%
  ggplot(., aes(x=b_body_size_scaled_log10, fill=kingdom))+
  geom_density()+
  theme_minimal()+
  facet_wrap(~order, ncol=8, scales="free")+
  scale_fill_brewer(palette="Set2")+
  guides(fill="none")+
  theme(axis.text=element_text(color="black"))+
  theme(axis.ticks.y=element_blank())+
  theme(axis.text.y=element_blank())+
  theme(axis.title.y=element_blank())+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  geom_vline(xintercept=0, color="gray30", linetype="dashed")+
  xlab("")

order_level

kingdom_level+class_level+order_level+plot_layout(ncol=1)

# Now the smallest level - family level
# this is 16 plots though using the x2 rule, so perhaps a bit difficult!
nymphalidae <- readRDS("model_objects/family_level/Nymphalidae_viirs_type_1.rds")
noctuidae <- readRDS("model_objects/family_level/Noctuidae_viirs_type_1.rds")
apidae <- readRDS("model_objects/family_level/Apidae_viirs_type_1.rds")
megachilidae <- readRDS("model_objects/family_level/Megachilidae_viirs_type_1.rds")
parulidae <- readRDS("model_objects/family_level/Parulidae_viirs_type_1.rds")
tyrannidae <- readRDS("model_objects/family_level/Tyrannidae_viirs_type_3.rds")
apodidae <- readRDS("model_objects/family_level/Apodidae_viirs_type_3.rds")
trochilidae <- readRDS("model_objects/family_level/Trochilidae_viirs_type_1.rds")
poaceae <- readRDS("model_objects/family_level/Poaceae_viirs_type_3.rds")
cyperaceae <- readRDS("model_objects/family_level/Cyperaceae_viirs_type_3.rds")
orchidaceae <- readRDS("model_objects/family_level/Orchidaceae_viirs_type_3.rds")
asparagaceae <- readRDS("model_objects/family_level/Asparagaceae_viirs_type_3.rds")
moraceae <- readRDS("model_objects/family_level/Moraceae_viirs_type_3.rds")
rosaceae <- readRDS("model_objects/family_level/Rosaceae_viirs_type_3.rds")
fabaceae <- readRDS("model_objects/family_level/Fabaceae_viirs_type_3.rds")
polygalaceae <- readRDS("model_objects/family_level/Polygalaceae_viirs_type_3.rds")

apidae_draws <- apidae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Hymenoptera") %>%
  mutate(family="Apidae") %>%
  mutate(kingdom="Animalia")

megachilidae_draws <- megachilidae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Hymenoptera") %>%
  mutate(family="Megachilidae") %>%
  mutate(kingdom="Animalia")

nymphalidae_draws <- nymphalidae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Lepidoptera") %>%
  mutate(family="Nymphalidae") %>%
  mutate(kingdom="Animalia")

noctuidae_draws <- noctuidae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Lepidoptera") %>%
  mutate(family="Noctuidae") %>%
  mutate(kingdom="Animalia")

parulidae_draws <- parulidae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Passeriformes") %>%
  mutate(family="Parulidae") %>%
  mutate(kingdom="Animalia")

tyrannidae_draws <- tyrannidae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Passeriformes") %>%
  mutate(family="Tyrannidae") %>%
  mutate(kingdom="Animalia")

apodidae_draws <- apodidae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Apodiformes") %>%
  mutate(family="Apodidae") %>%
  mutate(kingdom="Animalia")

trochilidae_draws <- trochilidae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Apodiformes") %>%
  mutate(family="Trochilidae") %>%
  mutate(kingdom="Animalia")

poaceae_draws <- poaceae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Poales") %>%
  mutate(family="Poaceae") %>%
  mutate(kingdom="Plantae")

cyperaceae_draws <- cyperaceae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Poales") %>%
  mutate(family="Cyperaceae")%>%
  mutate(kingdom="Plantae")

orchidaceae_draws <- orchidaceae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Asparagales") %>%
  mutate(family="Orchidaceae") %>%
  mutate(kingdom="Plantae")

asparagaceae_draws <- asparagaceae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Asparagales") %>%
  mutate(family="Asparagaceae") %>%
  mutate(kingdom="Plantae")

moraceae_draws <- moraceae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Rosales") %>%
  mutate(family="Moraceae") %>%
  mutate(kingdom="Plantae")

rosaceae_draws <- rosaceae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Rosales") %>%
  mutate(family="Rosaceae") %>%
  mutate(kingdom="Plantae")

fabaceae_draws <- fabaceae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Fabales") %>%
  mutate(family="Fabaceae") %>%
  mutate(kingdom="Plantae")

polygalaceae_draws <- polygalaceae %>%
  spread_draws(b_body_size_scaled_log10) %>%
  dplyr::select(b_body_size_scaled_log10) %>%
  mutate(order="Fabales") %>%
  mutate(family="Polygalaceae") %>%
  mutate(kingdom="Plantae")

family_level <- noctuidae_draws %>%
  bind_rows(nymphalidae_draws) %>%
  bind_rows(megachilidae_draws) %>%
  bind_rows(apidae_draws) %>%
  bind_rows(polygalaceae_draws) %>%
  bind_rows(fabaceae_draws) %>%
  bind_rows(rosaceae_draws) %>%
  bind_rows(moraceae_draws) %>%
  bind_rows(asparagaceae_draws) %>%
  bind_rows(orchidaceae_draws) %>%
  bind_rows(cyperaceae_draws) %>%
  bind_rows(poaceae_draws) %>%
  bind_rows(trochilidae_draws) %>%
  bind_rows(apodidae_draws) %>%
  bind_rows(tyrannidae_draws) %>%
  bind_rows(parulidae_draws) %>%
  mutate(family=factor(family, levels=c("Cyperaceae", "Poaceae",
                                        "Asparagaceae", "Orchidaceae", 
                                        "Rosaceae", "Moraceae", 
                                        "Fabaceae", "Polygalaceae",
                                        "Nymphalidae", "Noctuidae",
                                        "Apidae", "Megachilidae",
                                        "Parulidae", "Tyrannidae",
                                        "Trochilidae", "Apodidae"))) %>%
  mutate(kingdom=factor(kingdom, levels=c("Plantae", "Animalia"))) %>%
  ggplot(., aes(x=b_body_size_scaled_log10, fill=kingdom))+
  geom_density()+
  theme_minimal()+
  facet_wrap(~family, ncol=16, scales="free")+
  scale_fill_brewer(palette="Set2")+
  guides(fill="none")+
  theme(axis.text=element_text(color="black"))+
  theme(axis.ticks.y=element_blank())+
  theme(axis.text.y=element_blank())+
  theme(axis.title.y=element_blank())+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  geom_vline(xintercept=0, color="gray30", linetype="dashed")+
  theme(axis.text.x=element_text(size=6.5))+
  theme(strip.text=element_text(size=6.5))+
  xlab("Effect of body size on urban tolerance")

family_level

kingdom_level+ggtitle("A")+
  class_level+ggtitle("B")+
  order_level+ggtitle("C")+
  family_level+ggtitle("D")+
  plot_layout(ncol=1)

ggsave("Figures/example_posterior_distributions.png", width=12, height=7.2, units="in")

