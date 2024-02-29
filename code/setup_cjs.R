# Data set up for CJS model 

source("code/library.R")
source("code/function.R")
source("code/format_movement.R")

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

# Format Data for Movement across occasions
ft_move <- read_csv(here::here("data_formatted/formatted_cmr.csv")) %>%   # formatted cmr data
  select(occasion, tag, section, species) %>%
  subset(species == "bluehead_chub") %>% # select only bluehead chubs
  group_by(occasion, tag) %>% 
  slice(which.max(section)) %>% 
  filter(!is.na(tag)) %>%
  spread(occasion, section) %>% 
  ungroup()

Y2 <- ft_move %>% 
  select(-c(tag, species)) %>% 
  data.matrix()

fmat <- apply(Y2, MARGIN = 1, FUN = get_nonna)


# Format Density Matrix ---------------------------------------------------

# Format Data with density over all occasions
df_d <- df_density %>% # from 'format_movement'
  select(occasion, section, species, d, n) %>% 
  uncount(n)
  
# density for target species across occasion and section
ft_density <- df_d %>% 
  select(occasion, section, species, d) %>% 
  pivot_wider(names_from = occasion, 
              values_from = d, 
              values_fn = list) 

ft_density <- data.frame(lapply(ft_density, function(x) {replace(x, is.na(x), 0 )}))

#if multiple individuals i in the same section, you have to give the same density number for those individuals
 Y3 <- ft_density %>% 
   subset(species == "bluehead_chub") %>% # select only bluehead chubs
   select(-c(species, section)) %>% 
   data.matrix()


# Format Size Matrix ------------------------------------------------------

# Format Data with Body Length over all occasions
# ft_size <- read_csv(here::here("data_formatted/formatted_cmr.csv")) %>% # formatted cmr data
#   select(occasion, tag, section, species, length) %>%
#   subset(species == "bluehead_chub") %>% # select only bluehead chubs
#   group_by(occasion, tag) %>% 
#   slice(which.max(section)) %>%  
#   filter(!is.na(tag)) %>%
#   spread(occasion, length) %>% 
#   ungroup()
# 
# Y4 <- ft_size %>% 
#   select(-c(tag, species, section)) %>% 
#   data.matrix()
# 
# fmat_length <- apply(Y4, MARGIN = 1, FUN = get_nonna) # can use same function because it takes first non-NA
