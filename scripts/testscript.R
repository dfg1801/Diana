# Test script for final project :)

#Final project will be aa light morpho vs phylo distance analysis
# load packages--------------
library(tidyverse)
library(here)
library(janitor)
library(reshape2)
library(phytools)

# load data -----------
data <- read_csv(here("data", "AVONET_Raw_Data.csv")) # avonet data
names <- read_csv(here("data", "Honeycreeper_info.csv")) # gathered honeycreeper info

tree_name <- file.path(here("data", "Lerner_etal_2011_CurrBio_honeycreepers_mod.newick"))
tree <- read.newick(tree_name)
plot(tree) # test that the tree read in correctly

# clean the data -------------
#morpho data
names_clean <- names %>%
  clean_names()
morpho <- data %>%
  select(Avibase.ID:Publication) %>% # for some reason imported with a bunch of extra empty cols? 
  clean_names() %>% # clean names lol 
  #there are species with alternate names, so I want to make sure that if they are named under this name they are still captured within the dataset
  mutate(across(species1_bird_life:species3_bird_tree, 
                ~ ifelse(. == "Hemignathus munroi", "Hemignathus wilsoni", .))) %>% #change alt sci names (munroi) into their primary names as listed in names_clean (wilsoni), . means if false keep value in cell currently
  mutate(across(species1_bird_life:species3_bird_tree, 
                ~ ifelse(. == "Hemignathus flavus", "Chlorodrepanis flava", .))) %>%
  mutate(across(species1_bird_life:species3_bird_tree, 
                ~ ifelse(. == "Hemignathus kauaiensis", "Chlorodrepanis stejnegeri", .))) %>%
  mutate(across(species1_bird_life:species3_bird_tree, 
                ~ ifelse(. == "Loxops coccineus coccineus", "Loxops coccineus", .))) %>%
  mutate(across(species1_bird_life:species3_bird_tree, 
                ~ ifelse(. == "Oreomystis mana", "Manucerthia mana", .))) %>%
  mutate(across(species1_bird_life:species3_bird_tree, 
                ~ ifelse(. == "Loxops mana", "Manucerthia mana", .))) %>%
  mutate(across(species1_bird_life:species3_bird_tree, 
                ~ ifelse(. == "Hemignathus virens", "Chlorodrepanis virens", .))) %>%
  filter(species1_bird_life != "Himatione fraithii") %>% # this one particular sp seems to be mis-named as Himatione sanguinea in the birdtree dataset but im not sure? so I will remove these rows instead
  rename("sci_name" = species1_bird_life) %>% # make col names match for join
  inner_join(names_clean, by = "sci_name") %>%
  select(avibase_id, sci_name, hi_name, common_name, diet, detailed_diet, source:age, beak_length_culmen:tail_length) %>%
  mutate(common_name = coalesce(common_name, hi_name)) %>% # coalesce (frm dplyr) is good for replacing NAs with values from other vectors. if there is a missing common name, use the name from the hi_name column
  mutate(common_name = str_replace_all(common_name, "`", "")) %>% # get rid of tone markings so it can match the names from the phylo 
  mutate(common_name = str_replace_all(common_name, "ō", "o")) %>%
  mutate(common_name = str_replace_all(common_name, "ā", "a"))
  # print this table in final
  #Loxops coccineus has no data but keep in table ig? 

#matrix stuff-------------------
###### now turn this into pairwise matrix for beak_length_culmen
#first cut up the table a bit
beak_length <- morpho %>%
  mutate(common_name = str_replace_all(common_name, " ", "_")) %>%
  mutate(common_name = str_replace_all(common_name, "Lesser_Amakihi", "Anianiau")) %>% # the phylo used hawaiian names for these T_T
  mutate(common_name = str_replace_all(common_name, "Hawaii_akepa", "Akepa")) %>%
  mutate(common_name = str_replace_all(common_name, "Crested_honeycreeper", "Akohekohe")) %>%
  filter(common_name != "Akepa") %>% # akepa has no data so remove it from the matrix, crested honeycreeper is not in the morpho
  select(common_name, beak_length_culmen) %>%
  group_by(common_name) %>%
  summarise(mean_length_culmen = mean(beak_length_culmen)) 

# now make the pairwise distances
length_matrix=dist(beak_length)
length_matrix=as.matrix(length_matrix, labels=TRUE)
colnames(length_matrix) <- rownames(length_matrix) <- beak_length[['common_name']] #this is the only line

cleaned_length_dist <- length_matrix %>%
  melt() %>%
  filter(Var1 != Var2) %>% # remove the comparisons btween the same sp (inherently will be 0), so they dont skew scaling
  mutate(Var1 = str_to_lower(Var1)) %>% # make everything lowercase to make the matching easier 
  mutate(Var2 = str_to_lower(Var2)) %>%
  mutate(pair = paste(Var1, ",", Var2))


#####pairwise matrix for beak_width
beak_width <- morpho %>%
  mutate(common_name = str_replace_all(common_name, " ", "_")) %>%
  mutate(common_name = str_replace_all(common_name, "Lesser_Amakihi", "Anianiau")) %>% # the phylo used hawaiian names for these T_T
  mutate(common_name = str_replace_all(common_name, "Hawaii_akepa", "Akepa")) %>%
  mutate(common_name = str_replace_all(common_name, "Crested_honeycreeper", "Akohekohe")) %>%
  filter(common_name != "Akepa") %>% # akepa has no data so remove it from the matrix, crested honeycreeper is not in the morpho
  select(common_name, beak_width) %>%
  group_by(common_name) %>%
  summarise(mean_width = mean(beak_width)) 

# now make the pairwise distances
width_matrix=dist(beak_width)
width_matrix=as.matrix(width_matrix, labels=TRUE)
colnames(width_matrix) <- rownames(width_matrix) <- beak_width[['common_name']] #this is the only line

cleaned_width_dist <- width_matrix %>%
  melt() %>%
  filter(Var1 != Var2) %>%
  mutate(Var1 = str_to_lower(Var1)) %>% # make everything lowercase to make the matching easier 
  mutate(Var2 = str_to_lower(Var2)) %>%
  mutate(pair = paste(Var1, ",", Var2))
#######pairwise matrix for beak_depth 
beak_depth <- morpho %>%
  mutate(common_name = str_replace_all(common_name, " ", "_")) %>%
  mutate(common_name = str_replace_all(common_name, "Lesser_Amakihi", "Anianiau")) %>% # the phylo used hawaiian names for these T_T
  mutate(common_name = str_replace_all(common_name, "Hawaii_akepa", "Akepa")) %>%
  mutate(common_name = str_replace_all(common_name, "Crested_honeycreeper", "Akohekohe")) %>%
  filter(common_name != "Akepa") %>% # akepa has no data so remove it from the matrix, crested honeycreeper is not in the morpho
  select(common_name, beak_depth) %>%
  group_by(common_name) %>%
  summarise(mean_depth = mean(beak_depth)) 

# now make the pairwise distances
depth_matrix=dist(beak_depth)
depth_matrix=as.matrix(depth_matrix, labels=TRUE)
colnames(depth_matrix) <- rownames(depth_matrix) <- beak_depth[['common_name']] #this is the only line

cleaned_depth_dist <- depth_matrix %>%
  melt() %>%
  filter(Var1 != Var2) %>%
  mutate(Var1 = str_to_lower(Var1)) %>% # make everything lowercase to make the matching easier 
  mutate(Var2 = str_to_lower(Var2)) %>%
  mutate(pair = paste(Var1, ",", Var2))
  
#phylo distance matrix
phylo_dist <- cophenetic.phylo(tree) # make pairwise matrix of phylogenetic distances
# show this
cleaned_phylo_dist <- phylo_dist %>%
  melt() %>% #turn matrix into a dataframe
  mutate(Var1 = as.character(Var1)) %>%
  mutate(Var2 = as.character(Var2)) %>%
  filter(Var1 != Var2) %>%
  filter(Var1 != "Akepa") %>% # no morpho data for Akepa/Hawaii Akepa, remove
  filter(Var2 != "Akepa") %>%
  filter(Var1 != "Maui_Amakihi") %>% # no morpho data for maui amakihi, remove
  filter(Var2 != "Maui_Amakihi") %>%
  mutate(Var1 = str_to_lower(Var1)) %>% # make everything lowercase to make the matching easier 
  mutate(Var2 = str_to_lower(Var2)) %>%
  mutate(pair = paste(Var1, ",", Var2))


# Plotting ---------

# Brownian motion model
