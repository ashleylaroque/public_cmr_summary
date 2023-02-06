# R version 4.1.3 
# Ashley LaRoque
# PIT Detecction Summary

# setup -------------------------------------------------------------------

rm(list=ls())
source(here::here("code/library.R"))
source(here::here("code/data_summary.R"))

# read data ---------------------------------------------------------------

# read data: master PIT detection
df_pit <- read_csv(here::here("data_raw/pit_wand_master.csv")) %>% 
  drop_na(tag_id2) %>% 
  dplyr::select(-tag_id) %>% 
  mutate(f_occasion = paste0("occ", occasion)) %>% 
  rename(tag = tag_id2) %>% 
  filter(tag != "ID989.002009085551") # dummy tag

# check number of individuals tagged
skimr::skim(df_pit)
ftable(df_pit$ghost_tag)


# format data -------------------------------------------------------------

## format for pit wanding data
### remove duplicates within each occasion
df_pit_unique <- df_pit %>% 
  group_by(f_occasion,
           tag) %>% 
  slice(which.max(section)) %>% 
  ungroup()

### make sure unique observation per occasion & tag ID
df_pit_unique %>% 
  group_by(f_occasion,
           tag) %>% 
  tally() %>% 
  pull(n) %>% 
  unique()

## format for cmr data
### remove duplicates within each occasion
df_cmr_unique <- df_cmr %>%
  rename(tag = tag_id2) %>% 
  group_by(f_occasion, tag) %>% 
  slice(which.max(section)) %>% 
  ungroup()

### make sure unique observation per occasion & tag ID
df_cmr_unique %>% 
  group_by(f_occasion,
           tag) %>% 
  tally() %>% 
  pull(n) %>% 
  unique()

### pick 2022 data
df_cmr22 <- df_cmr_unique %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  filter(between(date,
                 as.Date("2022-01-01"),
                 as.Date("2022-12-31")))


# Wanding vs CMR  ---------------------------------------------------------

## work on this part

#' compare PIT wanding and CMR to assess whether ghost tags have been marked correctly 

df_combined <- df_cmr %>%
  rename(tag = tag_id2) %>%
  left_join(df_pit, by = c("tag")) %>%
  select(-c(tag_id.x, time.x, site, mortality, fin_clip, fin_recap, time.y, tag_id.y )) %>%
  mutate(date.x = as.POSIXlt(date.x, format = "%m/%d/%Y"),
         date.y = as.POSIXlt(date.y, format = "%m/%d/%Y")) %>%
  filter(date.x > as.POSIXlt("2022-01-01")) %>%
  na.omit() # had 62 NAs (need to check into this because there shouldnt be NAs)

# pit wand from jan detected in feb
# pit wand from jan, march, aril detected in may

df_change <- df_combined %>%
  mutate(date_subtracted = date.x - date.y)
