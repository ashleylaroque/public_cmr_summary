#' Author: Ashley LaRoque, Akira Terui
#' Description:
#' This script setup data for CJS models

# setup -------------------------------------------------------------------

## remove objects
rm(list = ls())
source(here::here("code/library.R"))


# data formatting ---------------------------------------------------------

## transform information
df_tag <- readRDS("data_formatted/data_cmr.rds") %>% 
  filter(species == "redbreast_sunfish",
         occasion < 5) %>% # for practice - must be removed for future analysis
  mutate(tag_index = as.numeric(as.factor(tag_id))) %>% 
  arrange(tag_index, occasion) %>% 
  relocate(tag_index, occasion)

## create vectorized recapture format
df_y <- df_tag %>% 
  pivot_wider(id_cols = tag_index,
              names_from = occasion,
              values_from = section) %>% 
  pivot_longer(cols = -tag_index,
               names_to = "occasion",
               values_to = "y") %>% 
  mutate(y = ifelse(is.na(y), 0, 1),
         occasion = as.numeric(occasion)) %>% 
  arrange(tag_index, occasion)

# create first capture vector 
df_fc <- df_y %>% 
  group_by(tag_index) %>% 
  filter(y == 1) %>% 
  slice(which.min(occasion)) %>% 
  ungroup() %>% 
  arrange(tag_index)

## create movement vector
df_dist <- df_tag %>% 
  pivot_wider(id_cols = tag_index,
              names_from = occasion,
              values_from = section) %>% 
  pivot_longer(cols = -tag_index,
               names_to = "occasion",
               values_to = "section") %>% 
  mutate(x = 10 * section - 5,
         occasion = as.numeric(occasion)) %>% 
  arrange(tag_index, occasion)

## format predictors
## - density
df_den <- readRDS("data_formatted/data_density.rds") %>% 
  dplyr::select(occasion,
                section,
                area,
                density_redbreast_sunfish)
