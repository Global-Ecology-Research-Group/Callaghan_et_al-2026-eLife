# script to visualize 'urban scores' averaged at family level across
# a phylogenetic tree


# packages
library(rotl)
library(dplyr)
library(ggplot2)
library(GGally)
library(ggtree)
library(tidytree)

# read in all response variables (urban scores)
urban_scores <- readRDS("urban_scores/subrealm_potential_urban_scores.RDS")


# first standardize the urban scores
# from 0-1 because urban scores among subrealms are not directly comparable
# but the ranking is thoeretically
dat_summary <- urban_scores %>%
  dplyr::filter(complete.cases(mean_urban_score_ghm)) %>%
  group_by(subrealm) %>%
  mutate(urban_rank=scales::rescale(mean_urban_score_ghm)) %>%
  group_by(kingdom, family) %>%
  summarize(mean_urban_score=mean(urban_rank),
            total_species=length(unique(species))) %>%
  dplyr::filter(total_species>=10)

# now try to make a tree
my_taxa <- dat_summary$family
resolved_names <- rotl::tnrs_match_names(names = my_taxa)

resolved_names$in_tree <- rotl::is_in_tree(resolved_names$ott_id)

table(resolved_names$in_tree)

my_tree <- rotl::tol_induced_subtree(resolved_names %>%
                                       dplyr::filter(in_tree=="TRUE") %>%
                                       .$ott_id)

tips <- data.frame(tips=my_tree$tip.label) %>%
  mutate(family=stringr::word(tips, 1, sep="_")) %>%
  left_join(., dat_summary)

my_tree$urban <- tips$mean_urban_score
my_tree$family_name <- tips$family

ape::plot.phylo(my_tree, cex = 2)

tree_tibble <- my_tree %>%
  as_tibble() %>%
  mutate(family=stringr::word(label, 1, sep="_")) %>%
  left_join(., dat_summary, by="family")

tree_dat <- tree_tibble %>%
  as.treedata()



ggtree(tree_dat, layout="circular")+
  geom_point(aes(shape=isTip, color=isTip), size=3)

ggtree(tree_dat, layout="circular")+
  geom_tippoint(aes(color=mean_urban_score), size=1)+
  scale_color_gradientn(colours=c("red", 'orange', 'green', 'cyan', 'blue'))

ggtree(tree_dat, layout="circular")+
  geom_tippoint(aes(color=mean_urban_score), size=1)+
  scale_color_gradientn(colours=c("red", 'orange', 'green', 'cyan', 'blue'))+
  facet_wrap(~kingdom)

# get the node number that matches with plantae
plant_nodes <- tree_dat@data %>%
  dplyr::filter(kingdom=="Plantae") %>%
  .$node


ggtree(tree_dat)+
  geom_tippoint(aes(color=mean_urban_score), size=1)+
  scale_color_gradientn(colours=c("red", 'orange', 'green', 'cyan', 'blue'))+
  geom_cladelabel(node=plant_nodes, label="Plantae", 
                  color="red2", offset=.8, align=TRUE)

ggtree(tree_dat, layout="circular")+
  geom_tippoint(aes(color=mean_urban_score), size=1)+
  scale_color_gradientn(colours=c("red", 'orange', 'green', 'cyan', 'blue'))+
  geom_cladelabel(node=1, label="Plantae", 
                color="red2", offset=.8, align=TRUE)+
  theme_tree2() + 
  xlim(0, 70) + 
  theme_tree()


  geom_tippoint(color="#FDAC4F", shape=8, size=3)
  geom_tree(aes(color=mean_urban_score), continuous = 'colour', size=2) +  
  scale_color_gradientn(colours=c("red", 'orange', 'green', 'cyan', 'blue')) +
  geom_tiplab(aes(color=mean_urban_score), hjust = -.1) + 
  xlim(0, 1.2) + 
  theme(legend.position = c(.05, .85)) 
  scale_color_continuous(low='darkgreen', high='red') +
  theme(legend.position="right")
