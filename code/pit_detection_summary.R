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
  filter(tag != "ID989.002009085551") %>%  # dummy tag
  dplyr::select(-occasion)

# check number of individuals tagged
skimr::skim(df_pit)
ftable(df_pit$ghost_tag)


# format data -------------------------------------------------------------

## format for pit wanding data
### remove duplicates within each occasion
df_pit_unique <- df_pit %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
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
                 as.Date("2022-12-31"))) %>% 
  dplyr::select(-occasion)


# Wanding vs CMR  ---------------------------------------------------------

tag_common <- intersect(df_pit_unique$tag, df_cmr22$tag)

df_common_tags <- df_pit_unique %>% 
  filter(tag %in% tag_common)

df_bound <- df_common_tags %>% 
  left_join(df_cmr22, by = "tag") %>% 
  dplyr::select(-c(time.x, tag_id, site, time.y, mortality, fin_clip, fin_recap, ...16))
  

trial <- bind_rows(df_common_tags, df_cmr22)



