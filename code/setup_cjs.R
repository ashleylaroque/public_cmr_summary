# Data set up for CJS model 

source("code/library.R")

# Setup Data ---------------------------------------

df_format <- read_csv(here::here("data_formatted/formatted_cmr.csv")) %>% 
  select(-c(julian, 
            f_occasion,
            date)) %>% 
  group_by(occasion, tag) %>% 
  slice(which.max(section)) %>% 
  ungroup() %>% 
  filter(species == "bluehead_chub")

# generate tag index and arrange  
Y0 <- df_format %>% 
  mutate(tag_index = as.numeric(as.factor(tag))) %>% 
  arrange(tag_index, occasion) %>% 
  relocate(tag_index, occasion)

#create vectorized recapture format
Y <- Y0 %>% 
  pivot_wider(id_cols = tag_index,
              names_from = occasion,
              values_from = section) %>% 
  pivot_longer(cols = -tag_index,
               names_to = "occasion",
               values_to = "x") %>% 
  mutate(x = ifelse(is.na(x), 0, 1),
         occasion = as.numeric(occasion)) %>% 
  arrange(tag_index, occasion)

# create first capture vector 
fc <- Y %>% 
  group_by(tag_index) %>% 
  filter(x == 1) %>% 
  slice(which.min(occasion)) %>% 
  ungroup() %>% 
  arrange(tag_index)


# Movement -----------------------------------------------------

Y1 <- Y0 %>% 
  pivot_wider(id_cols = tag_index,
              names_from = occasion,
              values_from = section) %>% 
  pivot_longer(cols = -tag_index,
               names_to = "occasion",
               values_to = "section") %>% 
  mutate(x = 10 * section - 5,
         occasion = as.numeric(occasion)) %>% 
  arrange(tag_index, occasion)


# Format Density ---------------------------------------------------

source("code/format_movement.R")

# combine non-target, habitat, and tagged data sets and calculate density
df_density <- df_h_sec %>% #df with all habitat data
  left_join(df_non_target, #df with all non-target data
            by = c("section", "occasion")) %>%
  left_join(df_target, #df with all cmr fish abundance data
            by = c("species", "section", "occasion")) %>% 
  rowwise() %>% 
  mutate(abundance.x = ifelse(is.na(abundance.x), 0, abundance.x), # keeps sections of NA because density would be 0
         abundance.y = ifelse(is.na(abundance.y), 0, abundance.y), 
         n = sum(c(abundance.x, abundance.y), # abundance (x and y are from cmr and non target) = n
                 na.rm = F),
         d = n / area) %>% # density (d) = abundance per area
  select(-c(abundance.x, abundance.y)) %>% 
  arrange(occasion, section) 

# formatted density for later use with all species 
# Y2 <- df_density %>% 
#   dplyr::select(occasion, section, d, species)

# get bluehead chub density for now
df_density_sub <- df_density %>% 
  filter(species == "bluehead_chub") %>% 
  dplyr::select(occasion, section, d)


# Format Size  ------------------------------------------------------

## keep this out for now - think later (as of 3/25/24)
# 
# # create vectorized format
#  Y3 <- Y0 %>% 
#    pivot_wider(id_cols = tag_index,
#                names_from = occasion,
#                values_from = length) %>% 
#    pivot_longer(cols = -tag_index,
#                 names_to = "occasion",
#                 values_to = "length") %>% 
#    arrange(tag_index, occasion)
#  
#  