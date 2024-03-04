# R version 4.3.2
# Ashley LaRoque
# Format CMR Data 



# Read Data ---------------------------------------------------------------
drive_download("data_cmr_master_workingcopy", 
               type = "csv", 
               path = "data_raw/data_cmr_master.csv", 
               overwrite = T )

data <- read_csv(here::here("data_raw/data_cmr_master.csv"))

df_cmr <- data %>% 
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
  mutate(f_occasion = paste0("occ", occasion))  #add column to make occasions into characters


# separate na values for merging later to calculate density including NA
 df_na <- df_cmr %>% # for use to merge for density
   filter(is.na(species)) %>%
   mutate(date = as.Date(date, format = "%m/%d/%Y"), # data type change: date column
         julian = julian(date)) 


# separate non-na for outlier correction
df_cmr <- df_cmr  %>% #filter out moralities
  drop_na(tag) %>%  #omit NA values
  filter(mortality == "n")



# Check Data --------------------------------------------------------------

# check data summary
skimr::skim(df_cmr)

# check recurring tags have matching species
error_id <- df_cmr %>% 
  group_by(tag) %>% 
  summarize(n_species = n_distinct(species)) %>%    #finds tags that are duplicates
  filter(n_species > 1) %>%                         #duplicate tags extracted
  pull(tag)

# df_cmr %>% 
#   filter(tag %in% error_id) %>%     #filters out the tags from error_id
#   arrange(tag) %>%                  #each tag should have the same species throughout occasions
#   view()

# check length / weight relationship (visual)
ggplot(df_cmr, aes(x = length , y = weight, color = f_occasion)) +
  geom_point()+
  facet_wrap(~species) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") #shows some outliers of l v w relationship

# Remove Outliers ---------------------------------------------------------

# vector of species name
v_sp <- unique(df_cmr$species)

# threshold value of "weight"
z <- 0.3  # lower value only big will be removed = more deviation from line (play around with this value to know)

# Pull outliers to be corrected
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

                 if (any(fit$w < z)) {
                   # remove entries with weight < z
                   # which(fit$w < z) returns row numbers with w < z
                   # minus sign means "remove"
                   df_filter <- df_sp %>%
                     slice(-which(fit$w < z))
                 } else {
                   df_filter <- df_sp
                 }

                 return(cout)
               }
## select what rows are irrelgular
df_sp[fit$w < .3, ]

df1 <- mutate(df0, col = ifelse(w < 0.3, "yes", "no")) 

## visualize possible outliers
ggplot(df1) +
  geom_point(aes(x = length,
                 y = weight,
                 color = col)) +
  theme_minimal() +
  facet_wrap(~species, ncol= 3, scales="free") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

df1 <- df1 %>%
  filter(col == "yes")    # filters out which tags are potential outliers for correction

# repeat robust regression analysis to identify outliers
# finds anomalies in relationship 
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
  geom_point(aes(x = length,
                 y = weight,
                 color = f_occasion)) +
  theme_minimal() +
  facet_wrap(~species, ncol= 3, scales="free") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

# Calculate intervals for movement and merging ------------------------------------------------------

## prepare data for merging with non-target and habitat data
df1 <- df0 %>% #df0 is after outlier correction
  mutate(date = as.Date(date, format = "%m/%d/%Y"), # data type change: date column
         julian = julian(date)) %>% # julian date because R wont recognize other date formats for gathering earliest occurance
  group_by(tag, occasion) %>% # group by tag and occasion
  slice(which.min(date)) %>% # pick the first capture in each occasion
  ungroup() %>% 
  rbind(df_na) # bind with na values 

write.csv(df1, file = "data_formatted/formatted_cmr.csv", row.names = F)

df_wide <- df1 %>% 
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
df_t <- df1 %>% 
  group_by(species,
           section,
           occasion) %>% 
  summarize(abundance = n()) %>% 
  ungroup() 

# abundance of tagged individuals including NA/0's
df_target <- with(df1, 
     expand.grid(occasion = sort(unique(occasion)),
                 section = seq(1, 43, by = 1),
                 species = sort(unique(species)))) %>% 
  as_tibble() %>% 
  left_join(df_t, by = c("occasion", "section", "species")) %>% 
  mutate(abundance = ifelse(is.na(abundance), 0, abundance))


# Format Non-Target Data --------------------------------------------------

drive_download("data_non_target_workingcopy", 
               type = "csv", 
               path = "data_raw/data_non_target.csv", 
               overwrite = T )

# read data

# transform data into abundance for each species per occasion per section
df_nt <- read_csv(here::here("data_raw/data_non_target.csv")) %>% 
  rename_at(vars(everything()),
            .funs = str_to_lower) %>% # make all column headers lowercase
  mutate(species = case_when(species == "BHC" ~ "bluehead_chub",
                             species == "BLG" ~ "bluegill",
                             species == "CCS" ~ "creekchub_sucker",
                             species == "CRC" ~ "creek_chub",
                             species == "DACE" ~ "dace",
                             species == "DAT" ~ "darter",
                             species == "EMF" ~ "eastern_mosquitofish",
                             species == "GSF" ~ "green_sunfish",
                             species == "KF" ~ "killifish",
                             species == "LMB" ~ "largemouth_bass",
                             species == "LOACH" ~ "loach",
                             species == "MADTOM" ~ "madtom",
                             species == "RBS" ~ "redbreast_sunfish",
                             species == "REDHORSE" ~ "redhorse",
                             species == "SHINER" ~ "shiner",
                             species == "STJ" ~ "striped_jumprock",
                             species == "WAR" ~ "warmouth",
                             species == "YB" ~ "yellow_bullhead")) %>% 
  group_by(occasion, section, species) %>% 
  summarize(abundance = n()) %>% 
  ungroup() 


# abundance of tagged individuals including NA/0's
df_non_target <- with(df_nt, 
                  expand.grid(occasion = sort(unique(occasion)),
                              section = seq(1, 43, by = 1),
                              species = sort(unique(species)))) %>% 
  as_tibble() %>% 
  left_join(df_nt, by = c("occasion", "section", "species")) %>% 
  mutate(abundance = ifelse(is.na(abundance), 0, abundance))

