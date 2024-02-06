# Data set up for CJS model 

library(tidyverse)
source("code/function.R")

# Setup Data for Temporal CJS ---------------------------------------

df0 <- read_csv(here::here("data_formatted/formatted_cmr.csv")) %>% 
  select(-c(julian, 
            f_occasion,
            date)) %>% 
  subset(species == "bluehead_chub") %>% 
  group_by(occasion, tag) %>% 
  sample_n(1) %>% ## sample_n(1) is to make sure to have one capture per occasion: must be corrected
  ungroup()

## convert the data into matrix format
## replace NA with zero
Y <- df0 %>% 
  pivot_wider(id_cols = tag,
              names_from = occasion,
              values_from = section,
              values_fn = function(x) !is.na(x)) %>% 
  dplyr::select(-tag) %>% 
  data.matrix()

Y[is.na(Y)] <- 0  

## replace zeros with NA before the first capture
for(i in 1:nrow(Y)) {
  id_one <- getf(Y[i, ])
  if (id_one > 1) Y[i, 1:(id_one - 1)] <- NA
}

## get first capture id
apply(Y, MARGIN = 1, FUN = getf)



# Setup Data for Movement Model-----------------------------------------------------

# Format Data for Movement
Y2 <- read_csv(here::here("data_formatted/formatted_cmr.csv")) %>% # formatted cmr data
  select(f_occasion, tag, section, species) %>%
  subset(species == "bluehead_chub") %>% # select only bluehead chubs
  group_by(f_occasion, tag) %>% 
  sample_n(1) %>% 
  filter(!is.na(tag)) %>%
  distinct(f_occasion, tag, .keep_all = TRUE) %>%
  spread(f_occasion, section) %>% 
  ungroup() %>% 
  select(-c(tag, species)) %>% 
  data.matrix()


