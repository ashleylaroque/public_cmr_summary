# R version 4.1.3 
# Ashley LaRoque
# PIT Detecction Summary

# setup -------------------------------------------------------------------

rm(list=ls())
source(here::here("code/library.R"))


# read data ---------------------------------------------------------------

# read data: master PIT detection
df_pit <- read_csv(here::here("data_raw/pit_wand_master.csv")) %>% 
  drop_na(tag_id2) %>% 
  dplyr::select(-tag_id) %>% 
  mutate(f_occasion = paste0("occ", occasion)) %>% 
  rename(tag = tag_id2)

# check number of individuals tagged
n_distinct(df_pit$tag)

ftable(df_pit$ghost_tag)

## check below later

# format data -------------------------------------------------------------

# filter out ghost tags
pit_fil <- df_pit %>% 
  filter(ghost_tag == "No")
  
df_pit_move <- pit_fil %>% 
  distinct(f_occasion, tag, .keep_all = TRUE) %>%
  select(f_occasion, tag_id2, section) %>%
  spread(f_occasion, section) %>% 
  rename(tag = tag_id2)

df_pit_move <- df_pit_move %>%
  mutate(Mv12 = Occ2 - Occ1, Mv23 = Occ3 - Occ2, Mv34 = Occ4 - Occ3, Mv45 = Occ5 - Occ4, 
         Mv56 = Occ6 - Occ5, Mv67 = Occ7 - Occ6, Mv78 = Occ8 - Occ7) %>%
  select(tag, Mv12:Mv78) %>%
  gather(interv, move, Mv12:Mv78, factor_key = TRUE) %>%
  mutate(move = move * 10) %>% # section = 10m
  na.omit()

##' Figures -----------------------------------------------------------------
# plot raw data
ggplot(df_pit, aes(x= ghost_tag, fill= ghost_tag)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-.5) +
  theme_minimal()

# movement over all occasions
plot_move <- gghistogram(df_pit_move, x = "move", fill = "darkblue",
                         xlab = "Distance (m)", ylab = "Frequency", 
                         binwidth = 10) + 
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9)
plot_move

# movement between occasions
plot_occ_move <- gghistogram(df_pit_move, x = "move", fill = "darkblue",
                         xlab = "Distance (m)", ylab = "Frequency", 
                         binwidth = 10, facet.by = "interv") + 
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9)
plot_occ_move


# Wanding vs CMR  ---------------------------------------------------------

#' compare PIT wanding and CMR to assess whether ghost tags have been marked correctly 
 
df_combined <- df_cmr %>% 
  left_join(df_pit, by = "tag_id2") %>% 
  select(-c(tag_id.x, time.x, site, mortality, fin_clip, fin_recap, time.y, tag_id.y )) %>% 
  mutate(date.x = as.POSIXlt(date.x, format = "%m/%d/%Y"),
         date.y = as.POSIXlt(date.y, format = "%m/%d/%Y")) %>% 
  filter(date.x > as.POSIXlt("2022-01-01")) %>% 
  na.omit() # had 62 NAs (need to check into this because there shouldnt be NAs)

# pit wand from jan detected in feb
# pit wand from jan, march, aril detected in may

df_change <- df_combined %>% 
  mutate(date_subtracted = date.x - date.y)







