# R version 4.3.1
# Ashley LaRoque
# Combine and format for analysis - CMR

source(here::here("code/library.R"))
source(here::here("code/format_cmr.R"))
source(here::here("code/format_habitat.R"))

# Bind all data sets together ---------------------------------------------

# combine non-target, habitat, and tagged data sets 
# calculate density for all fish including sections/occasions with no fish across all species
df_den <- df_h_sec %>% #df with all habitat data
  left_join(df_non_target, #df with non-target data
            by = c("section", "occasion")) %>%
  left_join(df_target, #df with all target fish data
            by = c("species", "section", "occasion")) %>% 
  rowwise() %>% 
  mutate(n = sum(c(abundance.x, abundance.y)),# abundance (x and y are from cmr and non target) = n
         d = n / area) %>% # density (d) = abundance per area
  select(-c(abundance.x, abundance.y)) %>% 
  arrange(occasion, section) %>% 
  subset(n > 0) # only use non-na / non-zero density for wide format

#density for target species across occasion and section
df_density_wide <- df_den %>% 
  pivot_wider(id_cols = c(occasion, section),
              names_from = species,
              values_from = d,
              values_fill = 0,
              names_prefix = "d_") %>%
  select(occasion:d_striped_jumprock) #select which columns 

# wide format density and habitat for figure correlations
df_d_m <- df_density_wide %>% 
  left_join(df_h_sec,
            by = c("section", "occasion"))

# Calculate Movement ------------------------------------------------------

# merge all density and fish info data
# calculate movement/emigration
df_m <- df_interval %>% #cmr data formatted with cap/recap intervals
  left_join(df1, # formatted cmr data with length, weight, tag, etc
            by = c("tag",
                   "species",
                   "occasion_cap" = "occasion")) %>%
  left_join(df1,
            suffix = c("_cap", "_recap"),
            by = c("tag",
                   "species",
                   "occasion_recap" = "occasion")) %>%
  left_join(df_density_wide, #target species density across occ & section
            by = c("occasion_cap" = "occasion",
                   "section_cap" = "section")) %>%
  mutate(across(starts_with("d_"),
                .fns = function(x) replace_na(x, 0)), # after merge areas of na get density = 0
       move = (section_recap - section_cap) * 10 - 5,
       move_abs = abs(section_recap - section_cap) * 10 - 5,
       emigration = as.numeric(abs(section_recap - section_cap) > 0),
       size_ratio = length_cap / weight_cap) %>%   # mm per unit g
  pivot_longer(cols = starts_with("d_"),
               values_to = "density",
               names_to = "opponent") %>%
  filter(opponent %in% paste0("d_", f_species)) 

#write.csv(df_m, file = "data_formatted/formatted_cmr_movement.csv", row.names = F)


# lengthen habitat variables with fish density for figures 
df_hab_l <- df_m %>% 
  select(occasion_cap, section_cap, species, density, move, opponent) %>% 
  left_join(df_den,
            by = c("occasion_cap" = "occasion",
                   "section_cap" = "section",
                   "species")) %>% 
  pivot_longer(cols = c("width", "section_length", "depth_mean", "velocity_mean",
                        "substrate_mean", "area_ucb", "area", "area_pool",
                        "area_run", "area_riffle"),
               names_to = "habitat_variable",
               values_to = "value")  
  



