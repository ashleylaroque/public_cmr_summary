# R version 4.1.3 
# Ashley LaRoque
# Occasion Sampling Summary 


# Setup -------------------------------------------------------------------

rm(list=ls())
source(here::here("code/library.R"))

# Read Data ---------------------------------------------------------------

df_cmr <- read_csv(here::here("data_raw/north_campus_master_corrected.csv")) %>% 
  rename_with(.fn = str_to_lower,
              .cols = everything()) %>% # make column titles lowercase
  dplyr::select(-c(error_corrected, comments)) %>% # remove extraneous columns
  drop_na(tag_id2) %>% #omit NA values in tagid
  mutate(f_occasion = paste0("occ", occasion))

# check data summary
skimr::skim(df_cmr)


# Remove Outliers ---------------------------------------------------------

# columns weight and length: check for outliers and remove
# vector of species name
v_sp <- unique(df_cmr$species)

# threshold value of "weight"
z <- 0.3  

# repeat robust regression analysis to identify outliers
df0 <- foreach(i = seq_len(n_distinct(df_cmr$species)),
               .combine = bind_rows) %do% {
                 
                 # subset data by species
                 df_sp <- df_cmr %>% 
                   filter(species == v_sp[i])
                 
                 # fit robust linear model
                 # robust linear model returns "weight" value for each data point based on deviation from the general trend
                 fit <- MASS::rlm(log(weight) ~ log(length),
                                  df_sp)
                 
                 if (any(fit$w < z)) {
                   # remove entries with weight < z
                   # which(fit$w < z) returns row numbers with w < z
                   # minus sign means "remove"
                   df_filter <- df_sp %>% 
                     slice(-which(fit$w < z))
                 } else {
                   df_filter <- df_sp
                 }
                 
                 return(df_filter)
               }

## visualize after outlier removal
ggplot(df0) +
  geom_point(aes(length, weight, color = f_occasion)) +
  theme_minimal() +
  facet_wrap(~species, ncol= 3, scales="free") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")
  

# Format Habitat -----------------------------------------------------------------

## input habitat data
df_habitat <- read_csv("data_raw/north_campus_habitat_raw.csv") %>%
  rename_with(.fn = str_to_lower, .cols = everything()) %>% 
  drop_na(velocity1, velocity2, velocity3)  # velocity cells hava NA

## manipulate habitat data
## aggregate quadrats to transect
df_tr <- df_habitat %>%
  mutate(across(starts_with("substrate"),
                .fns = function(x) {
                  case_when(x == "SL" ~ 0.01,
                            x == "SD" ~ 1,
                            x == "GV" ~ 9,
                            x == "PE" ~ 40,
                            x == "CO" ~ 160,
                            x == "BO" ~ 384,
                            x == "BR" ~ 520)
                } # convert substrate to numeric values
  )) %>%
  rowwise() %>% # rowwise operation
  mutate(depth = mean(depth1, depth2, depth3),
         velocity = mean(velocity1, velocity2, velocity3),
         substrate = mean(substrate1, substrate2, substrate3)) %>%
  select(-c(paste0("depth", 1:3),
            paste0("velocity", 1:3),
            paste0("substrate", 1:3))) %>% # remove depth1, depth2, ...
  ungroup() %>%
  relocate(depth, velocity, substrate) # just to show edited columns

## aggregate transects to section
## 6/20/23 note: deepest depth lacks after occasion 7 - check
df_sec <- df_tr %>%
  rowwise() %>% # rowwise operation
  mutate(section_length = sum(pool_length,
                              riffle_length,
                              run_length)) %>% # sum pool, riffle, run length for each row
  ungroup() %>%
  group_by(occasion, section) %>% # mean or remove NAs by occasion and section
  summarize(width = mean(width),
            section_length = na.omit(section_length),
            depth_mean = mean(depth),
            velocity_mean = mean(velocity),
            substrate_mean = mean(substrate),
            #depth_max = na.omit(deepest_depth),
            area = width * section_length,
            area_pool = width * na.omit(pool_length),
            area_run = width * na.omit(run_length),
            area_riffle = width * na.omit(riffle_length)) %>%
  ungroup()

## undercut bank
df_ucb <- df_habitat %>%
  pivot_longer(cols = starts_with("ucb"), # select columns starting with "ucb"
               names_to = c("ucb_id", "dimension"),
               values_to = "value",
               names_sep = "_") %>% # split ucb columns into "ucb_id" and "dimension" by "_"
  dplyr::select(occasion,
                section,
                ucb_id,
                dimension,
                value) %>%
  mutate(dimension = case_when(dimension == "len" ~ "length",
                               str_detect(dimension, "dep") ~ "depth")) %>%  # change "dimensions" column elements
  drop_na(value) %>% # drop NAs in "value" column
  group_by(occasion,
           section,
           ucb_id,
           dimension) %>%
  summarize(value = mean(value)) %>% # take means for each ucb
  pivot_wider(values_from = value,
              names_from = dimension) %>% # lateral extension
  ungroup()

df_ucb_sec <- df_ucb %>%
  mutate(area = depth * length) %>%
  group_by(occasion, section) %>%
  summarize(area_ucb = sum(area)) # sum by section

## merge data
df_h_sec <- df_sec %>%
  left_join(df_ucb_sec,
            by = c("occasion", "section")) %>% 
  mutate(area_ucb = replace_na(area_ucb, 0)) # replace NA with zero


# Format for Density ------------------------------------------------------

## prepare data for merging with non-target and habitat data
df0 <- df0 %>% 
  dplyr::select(-c(time,
                   tag_id,
                   recap,
                   mortality,
                   site)) %>% # remove unnecessary columns
  rename(tag = tag_id2) %>%  # change column name
  mutate(date = as.Date(date, format = "%m/%d/%Y"), # data type change: date column
         julian = julian(date)) %>% # julian date
  group_by(tag, occasion) %>% # group by tag and occasion
  slice(which.min(date)) %>% # pick the first capture in each occasion
  ungroup()

df_wide <- df0 %>% 
  pivot_wider(id_cols = c(tag, species),
              names_from = occasion,
              values_from = c(length, weight, julian),
              names_sort = TRUE)

## julian data - calculate interval
df_interval <- df_wide %>% 
  select(starts_with("julian")) %>% 
  purrrlyr::by_row(lift_vl(diff),
                   .to = "y_",
                   .collate = "cols") %>% 
  select(starts_with("y")) %>% 
  mutate(select(df_wide, c(tag, species))) %>% 
  pivot_longer(cols = starts_with("y"),
               values_to = "interval",
               names_to = c("var_name", "occasion_cap"),
               names_sep = "_") %>% 
  drop_na(interval) %>% 
  select(-var_name) %>% 
  mutate(occasion_cap = as.numeric(occasion_cap),
         occasion_recap = occasion_cap + 1) %>% 
  relocate(tag,
           species,
           occasion_cap,
           occasion_recap)

## non-target individuals
df_nt <- read_csv(here::here("data_raw/data_non_target.csv")) %>% 
  rename_at(vars(everything()),
            .funs = str_to_lower) %>% 
  drop_na(species) %>% 
  mutate(species = case_when(species == "BHC" ~ "bluehead_chub",
                             species == "BLG" ~ "bluegill",
                             species == "CCS" ~ "creekchub_sucker",
                             species == "CRC" ~ "creek_chub",
                             species == "DACE" ~ "dace",
                             species == "DAT" ~ "darter",
                             species == "EMF" ~ "eastern_mosquitofish",
                             species == "GSF" ~ "green_sunfish",
                             species == "Killifish" ~ "killifish",
                             species == "LMB" ~ "largemouth_bass",
                             species == "LOACH" ~ "loach",
                             species == "Madtom" ~ "madtom",
                             species == "RBS" ~ "redbreast_sunfish",
                             species == "REDHORSE" ~ "redhorse",
                             species == "Shiner" ~ "shiner",
                             species == "STJ" ~ "striped_jumprock",
                             species == "Warmouth" ~ "warmouth")) %>% 
  group_by(species, section, occasion) %>% 
  summarize(abundance = n()) %>% 
  ungroup()

## tagged individuals
df_t <- df_cmr %>% 
  rename_at(vars(everything()),
            .funs = str_to_lower) %>% 
  drop_na(species) %>% 
  group_by(species,
           section,
           occasion) %>% 
  summarize(abundance = n()) %>% 
  ungroup()

# combine non-target, habitat, and tagged data sets

df_density <- df_h_sec %>%
  left_join(df_t,
            by = c("section", "occasion")) %>%
  left_join(df_nt,
            by = c("species", "section", "occasion")) %>%
  rowwise() %>%
  mutate(n = sum(c(abundance.x, abundance.y), # abundance = n
                 na.rm = TRUE),
         d = n / area) %>% # density = d
  select(-abundance.x,
         -abundance.y) %>% 
  arrange(occasion, section)

df_density_wide <- df_density %>% 
  pivot_wider(id_cols = c(occasion, section),
              names_from = species,
              values_from = d,
              values_fill = 0,
              names_prefix = "d_") %>%
  select(occasion:d_striped_jumprock)

# merge all density data

df_m <- df_interval %>%
  left_join(df0,
            by = c("tag",
                   "species",
                   "occasion_cap" = "occasion")) %>%
  left_join(df0,
            suffix = c("_cap", "_recap"),
            by = c("tag",
                   "species",
                   "occasion_recap" = "occasion")) %>%
  left_join(df_density_wide,
            by = c("occasion_cap" = "occasion",
                   "section_cap" = "section")) %>%
  mutate(across(starts_with("d_"),
                .fns = function(x) replace_na(x, 0)))

# Format Movement ----------------------------------------------------------------
# 
# ## tagged fish abundance
# df_abundance <- df_cmr %>% 
#   group_by(occasion, section, species) %>% 
#   summarize(abundance = n())
# 
# ## fish abundance combined with habitat means 
# df_abund_comb <- df_abundance %>%
#   group_by(occasion, section) %>%
#   pivot_wider(names_from = species, values_from = abundance, values_fill = 0) %>% # section-wdie format and fill 0 value
#   arrange(occasion, section) %>%
#   right_join(df_h_sec, by = c("occasion" ,"section")) %>% 
#   mutate(redbreast_sunfish = ifelse(is.na(redbreast_sunfish), 0, redbreast_sunfish), # replace NA to 0
#          bluegill = ifelse(is.na(bluegill), 0, bluegill), # replace NA to 0
#          green_sunfish = ifelse(is.na(green_sunfish), 0, green_sunfish), # replace NA to 0
#          bluehead_chub = ifelse(is.na(bluehead_chub), 0, bluehead_chub), # replace NA to 0
#          creek_chub = ifelse(is.na(creek_chub), 0, creek_chub))# replace NA to 0
# 

# 
# # calculate movement between occasions in wide format
# df_move <- filter(df_cmr, !is.na(tag_id2)) %>%
#   distinct(f_occasion, tag_id2, .keep_all = TRUE) %>%
#   select(f_occasion, tag_id2, section, species) %>%
#   spread(f_occasion, section) %>%
#   rename(tag = tag_id2)
# 
# df_occ_move <- df_move %>%
#   mutate(Mv12 = occ2 - occ1, Mv23 = occ3 - occ2, Mv34 = occ4 - occ3, Mv45 = occ5 - occ4, Mv56 = occ6 - occ5, 
#          Mv67 = occ7 - occ6, Mv78 = occ8 - occ7, Mv89 = occ9 - occ8, Mv910 = occ10 - occ9, Mv1011 = occ11 - occ10) %>%
#   select(tag, species, Mv12:Mv1011) %>%
#   gather(interv, move, Mv12:Mv1011, factor_key = TRUE) %>%
#   mutate(move = move * 10) %>% # section = 10m
#   na.omit()

## format for 'figure_movement'
 df_m_ab <- df_m %>% 
   mutate(move = abs(section_recap - section_cap) * 10,
          emigration = as.numeric(abs(section_recap - section_cap) > 0))

df_move <- df_interval %>%
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
  mutate(move = abs(section_recap - section_cap) * 10,
         emigration = as.numeric(abs(section_recap - section_cap) > 0),
         size_ratio = length_cap / weight_cap) # mm per unit g 

df_z <- df_move %>% 
  pivot_longer(cols = width:area_ucb, 
               names_to = "habitat_variable", 
               values_to = "value")


# f_species <- c("green_sunfish",
#                "redbreast_sunfish",
#                "creek_chub",
#                "bluehead_chub",
#                "striped_jumprock",
#                "bluegill")
# 
# df_plot <- df_m_ab %>% 
#   pivot_longer(cols = starts_with("d_"),
#                values_to = "density",
#                names_to = "opponent") %>% 
#   filter(opponent %in% paste0("d_", f_species))
# 
# # GLM
# list_m <- lapply(1:length(f_species), function(i) {
#   
#   glm(emigration ~ 
#         scale(length_cap) + 
#         scale(d_creek_chub) +
#         scale(d_bluehead_chub) +
#         scale(d_green_sunfish) +
#         scale(d_redbreast_sunfish)+
#         scale(d_striped_jumprock),
#       family = "binomial",
#       data = filter(df_m_ab, species == f_species[i]))
#   
# })
# 
# names(list_m) <- f_species
# lapply(list_m, summary)
# 
# 
# 
