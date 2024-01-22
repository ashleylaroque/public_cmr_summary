# Data set up for CJS model 

library(tidyverse)
source("code/function.R")

# real data for cjs model ---------------------------------------

## sample_n(1) is to make sure to have one capture per occasion: must be corrected
df0 <- read_csv(here::here("data_formatted/formatted_cmr.csv")) %>% 
  select(-c(julian, 
            f_occasion,
            date)) %>% 
  subset(species == "redbreast_sunfish") %>% 
  group_by(occasion, tag) %>% 
  sample_n(1) %>% 
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
