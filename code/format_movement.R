# R version 4.3.1
# Ashley LaRoque
# Combine and format for analysis - CMR

source(here::here("code/library.R"))
source(here::here("code/format_cmr.R"))
source(here::here("code/format_habitat.R"))

# Bind all data sets together ---------------------------------------------

# combine non-target, habitat, and tagged data sets and calculate density
df_density <- df_h_sec %>% #df with all habitat data
  left_join(df_t, #df with cmr fish abundance data
            by = c("section", "occasion")) %>%
  left_join(df_nt, #df with all non-target fish data
            by = c("species", "section", "occasion")) %>%
  rowwise() %>%
  mutate(n = sum(c(abundance.x, abundance.y), # abundance = n
                 na.rm = TRUE),
         d = n / area) %>% # density (d) = abundance per area
  select(-c(abundance.x, abundance.y)) %>% 
  arrange(occasion, section)

#density for target species across occasion and section
df_density_wide <- df_density %>% 
  pivot_wider(id_cols = c(occasion, section),
              names_from = species,
              values_from = d,
              values_fill = 0,
              names_prefix = "d_") %>%
  select(occasion:d_striped_jumprock) #select which columns 

# merge all density and fish info data
df_m <- df_interval %>% #cmr data formatted with cap/recap intervals
  left_join(df0, #cmr data with length, weight, tag, etc
            by = c("tag",
                   "species",
                   "occasion_cap" = "occasion")) %>%
  left_join(df0,
            suffix = c("_cap", "_recap"),
            by = c("tag",
                   "species",
                   "occasion_recap" = "occasion")) %>%
  left_join(df_density_wide, #target species density across occ & section
            by = c("occasion_cap" = "occasion",
                   "section_cap" = "section")) %>%
  mutate(across(starts_with("d_"),
                .fns = function(x) replace_na(x, 0))) %>% 
mutate(move = (section_recap - section_cap) * 10,
       move_abs = abs(section_recap - section_cap) * 10,
       emigration = as.numeric(abs(section_recap - section_cap) > 0),
       size_ratio = length_cap / weight_cap) # mm per unit g 

# Format Movement ----------------------------------------------------------------

# Calculate movement between occasions in wide format
df_move <- filter(df_cmr, !is.na(tag)) %>%
  distinct(f_occasion, tag, .keep_all = TRUE) %>%
  select(f_occasion, tag, section, species) %>%
  spread(f_occasion, section) 

df_occ_move <- df_move %>%
  mutate(Mv12 = occ2 - occ1, Mv23 = occ3 - occ2, Mv34 = occ4 - occ3, Mv45 = occ5 - occ4, Mv56 = occ6 - occ5, 
         Mv67 = occ7 - occ6, Mv78 = occ8 - occ7, Mv89 = occ9 - occ8, Mv910 = occ10 - occ9, 
         Mv1011 = occ11 - occ10, Mv1112 = occ12 - occ11) %>%
  select(tag, species, Mv12:Mv1112) %>%
  gather(interv, move, Mv12:Mv1112, factor_key = TRUE) %>%
  mutate(move = move * 10) %>% # section = 10m
  na.omit()

## the following df_mo needs to be checked when joining df_density (habitat data)
# Calculate movement and emigration with density
df_mo <- df_interval %>% 
  left_join(df0,
            by = c("tag",
                   "species",
                   "occasion_cap" = "occasion")) %>%
  left_join(df0,
            suffix = c("_cap", "_recap"),
            by = c("tag",
                   "species",
                   "occasion_recap" = "occasion")) %>%
  left_join(df_density,
            by = c("occasion_cap" = "occasion",
                   "section_cap" = "section"),
                   "species") %>% 
  rename("d_species" = "species.y") %>% 
  mutate(move = (section_recap - section_cap) * 10,
         move_abs = abs(section_recap - section_cap) * 10,
         emigration = as.numeric(abs(section_recap - section_cap) > 0),
         size_ratio = length_cap / weight_cap) # mm per unit g 

df_z <- df_mo %>% 
  pivot_longer(cols = width:area_ucb, 
               names_to = "habitat_variable", 
               values_to = "value")



## fish abundance combined with habitat means 
df_abund_comb <- df_t %>% #df with cmr fish abundance data
  group_by(occasion, section) %>%
  pivot_wider(names_from = species, values_from = abundance, values_fill = 0) %>% # section-wide format and fill 0 value
  arrange(occasion, section) %>%
  right_join(df_h_sec, by = c("occasion" ,"section")) %>%
  mutate(redbreast_sunfish = ifelse(is.na(redbreast_sunfish), 0, redbreast_sunfish), # replace NA to 0
         bluegill = ifelse(is.na(bluegill), 0, bluegill), # replace NA to 0
         green_sunfish = ifelse(is.na(green_sunfish), 0, green_sunfish), # replace NA to 0
         bluehead_chub = ifelse(is.na(bluehead_chub), 0, bluehead_chub), # replace NA to 0
         creek_chub = ifelse(is.na(creek_chub), 0, creek_chub),# replace NA to 0
         striped_jumprock = ifelse(is.na(striped_jumprock), 0, striped_jumprock))# replace NA to 0




# Not in Use --------------------------------------------------------------


f_species <- c("green_sunfish",
               "redbreast_sunfish",
               "creek_chub",
               "bluehead_chub",
               "striped_jumprock",
               "bluegill")
 
df_plot <- df_m %>%
  pivot_longer(cols = starts_with("d_"),
               values_to = "density",
               names_to = "opponent") %>%
  filter(opponent %in% paste0("d_", f_species)) 
  

 



