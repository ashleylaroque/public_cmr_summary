# R version 4.1.3 
# Ashley LaRoque
# Format CMR Data 


# Setup -------------------------------------------------------------------

rm(list=ls())
source(here::here("code/library.R"))

# Read Data ---------------------------------------------------------------

df_cmr <- read_csv(here::here("data_raw/north_campus_master_corrected.csv")) %>% 
  rename_with(.fn = str_to_lower,
              .cols = everything()) %>% # make column headers lowercase
  dplyr::select(-c(error_corrected, 
                   comments, 
                   tag_id, 
                   time, 
                   glued,
                   fin_clip,
                   site,
                   `vial_#`)) %>% # remove extraneous columns
  rename(tag = tag_id2) %>%  # change column name
  drop_na(tag) %>% #omit NA values
  filter(mortality == "n") %>% #filter out moralities 
  mutate(f_occasion = paste0("occ", occasion)) #add column to make occasions into characters

# Check Data --------------------------------------------------------------

# check data summary
skimr::skim(df_cmr)

# check recurring tags have matching species
error_id <- df_cmr %>% 
  group_by(tag) %>% 
  summarize(n_species = n_distinct(species)) %>%    #finds tags that are duplicates
  filter(n_species > 1) %>%                         #duplicate tags extracted
  pull(tag)

df_cmr %>% 
  filter(tag %in% error_id) %>%     #filters out the tags from error_id
  arrange(tag) %>% 
  view()

# check length / weight relationship (visual)
ggplot(df_cmr, aes(x = length , y = weight, color = f_occasion)) +
  geom_point()+
  facet_wrap(~species) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

# Remove Outliers ---------------------------------------------------------

# vector of species name
v_sp <- unique(df_cmr$species)

# threshold value of "weight"
z <- 0.3  # lower value only big will be removed = more deviation from line (play around with this value to know)

# repeat robust regression analysis to identify outliers
  # finds anomalies in relationship 
df0 <- foreach(i = seq_len(n_distinct(df_cmr$species)), 
               .combine = bind_rows) %do% {
                 
                 # subset data by species
                 df_sp <- df_cmr %>% 
                   filter(species == v_sp[i])
                 
                 # fit robust linear model
                 # robust linear model returns "weight" value for each data point
                 # based on deviation from the general trend (RLM)
                 fit <- MASS::rlm(log(weight) ~ log(length) + f_occasion,
                                  df_sp)
                 
                 cout <- df_sp %>% 
                   mutate(w = fit$w)
                 
                 # if (any(fit$w < z)) {
                 #   # remove entries with weight < z
                 #   # which(fit$w < z) returns row numbers with w < z
                 #   # minus sign means "remove"
                 #   df_filter <- df_sp %>% 
                 #     slice(-which(fit$w < z))
                 # } else {
                 #   df_filter <- df_sp
                 # }
                 
                 return(cout)
               }
# ## select what rows are irrelgular
# df_sp[fit$w < .3, ]

df0 <- mutate(df0, col = ifelse(w < 0.3, "yes", "no"))
write_csv(df0, "outlier_correction.csv")

## visualize after outlier removal
ggplot(df0) +
  geom_point(aes(x = length,
                 y = weight,
                 color = col)) +
  theme_minimal() +
  facet_wrap(~species, ncol= 3, scales="free") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")
  

# Calculate intervals for movement and merging ------------------------------------------------------

## prepare data for merging with non-target and habitat data
df0 <- df0 %>% #df0 is after outlier correction
  dplyr::select(-c(recap,
                   mortality,
                   fin_recap)) %>% # remove unnecessary columns
  mutate(date = as.Date(date, format = "%m/%d/%Y"), # data type change: date column
         julian = julian(date)) %>% # julian date because R wont recognize other date formats for gathering earliest occurance
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

## abundance of tagged individuals per section per occasion
df_t <- df_cmr %>% 
  drop_na(species) %>% 
  group_by(species,
           section,
           occasion) %>% 
  summarize(abundance = n()) %>% 
  ungroup()


# Format Non-Target Data --------------------------------------------------

# read data
df_nt <- read_csv(here::here("data_raw/data_non_target.csv")) %>% 
  rename_at(vars(everything()),
            .funs = str_to_lower) %>% # make all column headers lowercase
  drop_na(species) %>% #omit cells that have an NA
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


