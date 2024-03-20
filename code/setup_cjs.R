# Data set up for CJS model 

source("code/library.R")
source("code/function.R")
source("code/format_movement.R")

# Setup Data for Temporal CJS ---------------------------------------

df2 <- read_csv(here::here("data_formatted/formatted_cmr.csv")) %>% 
  select(-c(julian, 
            f_occasion,
            date)) %>% 
  subset(species == "bluehead_chub") %>% 
  group_by(occasion, tag) %>% 
  sample_n(1) %>% ## sample_n(1) is to make sure to have one capture per occasion: must be corrected
  ungroup()

## convert the data into matrix format
## replace NA with zero
# Y <- df2 %>% 
#   pivot_wider(id_cols = tag,
#               names_from = occasion,
#               values_from = section,
#               values_fn = function(x) !is.na(x)) %>% 
#   dplyr::select(-tag) %>% 
#   data.matrix()

# generate movement 
Y <- df2 %>% 
  mutate(tag_index = as.numeric(as.factor(tag))) %>% 
  arrange(tag_index, occasion) %>% 
  relocate(tag_index, occasion) %>% 
  mutate(x = section * 10 - 5)

#create vectorized movement format
Y <- Y %>% 
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
fc <- df %>% 
  group_by(tag_index) %>% 
  filter(y == 1) %>% 
  slice(which.min(occasion)) %>% 
  ungroup() %>% 
  arrange(tag_index)

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
# combine non-target, habitat, and tagged data sets and calculate density
df_density <- df_h_sec %>% #df with all habitat data
  left_join(df_non_target, #df with cmr fish abundance data
            by = c("section", "occasion")) %>%
  left_join(df_target, #df with all non-target fish data
            by = c("species", "section", "occasion")) %>% 
  rowwise() %>% 
  mutate(abundance.x = ifelse(is.na(abundance.x), 0, abundance.x),
         abundance.y = ifelse(is.na(abundance.y), 0, abundance.y), 
         n = sum(c(abundance.x, abundance.y), # abundance (x and y are from cmr and non target) = n
                 na.rm = F),
         d = n / area) %>% # density (d) = abundance per area
  select(-c(abundance.x, abundance.y)) %>% 
  arrange(occasion, section) 

# separate abundance values of 0 
df_d <- df_density %>% # from 'format_movement'
  filter(n == 0) %>% 
  select(occasion, section, species, d)

# separate abundance values that are not 0 and duplicate rows based on abundance  
ft_density <- df_density %>% 
  filter(n != 0) %>% 
  select(occasion, section, species, d, n) %>% 
  uncount(n) %>% 
  rbind(df_d) # bind 0 and non-zero data together

# join with tag_id info for movement connection
df3 <- df2 %>% # from 'format_cmr'
  select(occasion, section, species, tag, length, weight, julian) %>% 
  group_by(occasion, tag) %>% 
  slice(which.max(section)) %>% 
  ungroup() %>% 
  left_join(ft_density, by = c("occasion", "section"),
            relationship = "many-to-many") %>%  # join with mark recapture data 
  filter(species.y == "bluehead_chub",
         species.x == "bluehead_chub") %>%   # simplify with one species for now (change later)
  distinct(tag, .keep_all = T) # remove duplicates for tag
  
# density for target species across occasion and section
ft <- df3 %>% 
  select(occasion, section, tag, d) %>% 
  spread(occasion, d) 

# replace NA values with 0 in matrix 
ft <- data.frame(lapply(ft, function(x) {replace(x, is.na(x), 0 )}))

#if multiple individuals i in the same section, you have to give the same density number for those individuals
 Y3 <- ft %>% 
   select(-c(tag, section)) %>% 
   data.matrix()
 
# create vectorized format
 
 
 # create first capture vector 
 fc <- df %>% 
   group_by(tag_index) %>% 
   filter(y == 1) %>% 
   slice(which.min(occasion)) %>% 
   ungroup() %>% 
   arrange(tag_index)


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
