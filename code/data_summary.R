# R version 4.1.3 
# Ashley LaRoque
# Occasion Sampling Summary 


# setup -------------------------------------------------------------------

rm(list=ls())
source(here::here("code/library.R"))


# read data ---------------------------------------------------------------

df_cmr <- read_csv(here::here("data_raw/north_campus_master_corrected.csv")) %>% 
  rename_with(.fn = str_to_lower,
              .cols = everything()) %>% # make column titles lowercase
  select(-c(error_corrected, comments)) %>% # remove extraneous columns
  drop_na(tag_id2) %>% #omit NA values in tagid
  mutate(f_occasion = paste0("occ", occasion))

# check data summary
skimr::skim(df_cmr)


# remove outliers ---------------------------------------------------------

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
  facet_wrap(~species, ncol= 3, scales="free")


# format for density ------------------------------------------------------

## prepare data for merging with non-target and habitat data
df0 <- df0 %>% 
  select(-time,
         -tag_id,
         -recap,
         -mortality,
         -site) %>% # remove unnecessary columns
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
df_t <- df0 %>% 
  rename_at(vars(everything()),
            .funs = str_to_lower) %>% 
  drop_na(species) %>% 
  group_by(species,
           section,
           occasion) %>% 
  summarize(abundance = n()) %>% 
  ungroup()


# habitat -----------------------------------------------------------------

## input habitat data
df_habitat <- read_csv("data_raw/north_campus_habitat_raw.csv") %>% 
  rename_with(.fn = str_to_lower, .cols = everything()) 
df_habitat <- df_habitat[-c(798),]

## manipulate habitat data
## aggregate quadrats to transect
df_tr <- df_habitat %>% 
  mutate(across(starts_with("substrate"),
                .fns = function(x) {
                  case_when(x == "SL" ~ 0.01,
                            x == "SD" ~ 1,
                            x == "GV" ~ 9,
                            x == "PE" ~ 40,
                            x %in% c("CO", "CP") ~ 160,
                            x == "BO" ~ 384,
                            x %in% c("BE", "BR") ~ 520)
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
df_sec <- df_tr %>% 
  rowwise() %>% # rowwise operation
  mutate(section_length = sum(pool_length,
                              riffle_length,
                              run_length)) %>% # sum pool, riffle, run length for each row
  ungroup() %>% 
  group_by(occasion, section) %>% # mean or remove NAs by occation and section
  summarize(width = mean(width),
            section_length = na.omit(section_length),
            depth_mean = mean(depth),
            velocity_mean = mean(velocity),
            substrate_mean = mean(substrate),
            depth_max = na.omit(deepest_depth),
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
  select(occasion,
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
            by = c("occasion", "section"))


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
  pivot_wider(id_cols = c(occasion, section),
              names_from = species,
              values_from = d,
              values_fill = 0,
              names_prefix = "d_") %>% 
  arrange(occasion, section) %>% 
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
  left_join(df_density,
            by = c("occasion_cap" = "occasion",
                   "section_cap" = "section")) %>% 
  mutate(across(starts_with("d_"),
                .fns = function(x) replace_na(x, 0)))



##' Movement ----------------------------------------------------------------

#' calculate movement between occasions
df_move <- filter(df_cmr, !is.na(tag_id2)) %>% 
  distinct(f_occasion, tag_id2, .keep_all = TRUE) %>%
  select(f_occasion, tag_id2, section, species) %>%
  spread(f_occasion, section) %>% 
  rename(tag = tag_id2)
df_move <- df_move %>%
  mutate(Mv12 = Occ2 - Occ1, Mv23 = Occ3 - Occ2, Mv34 = Occ4 - Occ3, Mv45 = Occ5 - Occ4, Mv56 = Occ6 - Occ5, Mv67 = Occ7 - Occ6, Mv78 = Occ8 - Occ7) %>%
  select(tag, species, Mv12:Mv78) %>%
  gather(interv, move, Mv12:Mv78, factor_key = TRUE) %>%
  mutate(move = move * 10) %>% # section = 10m
  na.omit()

df_m <- df_m %>% 
  mutate(move = (section_recap - section_cap) * 10,
         emigration = as.numeric(abs(section_recap - section_cap) > 0))

  ## merge datasets - not working due to more rows after merging 
    ### df_mp <- merge(x= df_m, y= df_move, by= c("tag", "species", "move")) ###


  # Frequency Histogram of Cyprinid and Catastomid Movement

chub.move.dist <- gghistogram(df_move[df_move$species %in% c('bluehead_chub','creek_chub', 'striped_jumprock'), ], x = "move", fill = "lightgrey",
                              xlab = "Distance (m)", ylab = "Frequency", binwidth = 10, facet.by = c("interv","species")) + 
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9) + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank())

chub.move.dist

  # Frequency Histogram of Centrarchids Movement

sunf.move.dist <- gghistogram(df_move[df_move$species %in% c('bluegill','green_sunfish','redbreast_sunfish'), ], x = "move", fill = "lightgrey",
                              xlab = "Distance (m)", ylab = "Frequency", binwidth = 10, facet.by = c("interv","species")) + 
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9) + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank())

sunf.move.dist


#' - Figure: General Movement over all occasions 

fish_labs <- c(bluehead_chub = "Bluehead Chub", creek_chub = "Creek Chub", striped_jumprock = "Striped Jumprock", 
               redbreast_sunfish = "Redbreast Sunfish", green_sunfish = "Green Sunfish", bluegill = "Bluegill")
fish_order <- c('bluehead_chub','creek_chub',' striped_jumprock',
                'green_sunfish','redbreast_sunfish', 'bluegill')

df_m2 <- arrange(transform(df_m,
                           species=factor(species,levels=fish_order)),species)

all_movement <- gghistogram(df_m2 [df_m2$species %in% c('bluehead_chub','creek_chub', 'striped_jumprock',
                                                        'green_sunfish','redbreast_sunfish', 'bluegill'), ], x = "move", fill = "dodgerblue",
                            xlab = "Distance (m)", ylab = "Frequency", binwidth = 10) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9) + 
  facet_wrap(species ~ ., factor(levels= fish_order), labeller = as_labeller(fish_labs))
all_movement



