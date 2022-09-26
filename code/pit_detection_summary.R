# R version 4.1.3 
# Ashley LaRoque
# PIT Detecction Summary

# R version 4.1.3 
# Ashley LaRoque
# PIT Detecction Summary

rm(list=ls(all.names=T))
##' Prepare workspace -------------------------------------------------------

# load packages
pacman::p_load(tidyverse,
               ggpubr,
               ggplot2)


##' Gather Data -------------------------------------------------------------
# read data: master PIT detection
df_pit <- read_csv(here::here("data_raw/pit_wand_master.csv")) %>% 
  na.omit() # omit NA values 

# check number of individuals tagged
n_distinct(df_pit$tag_id2)

ftable(df_pit$ghost_tag)

##' Format Data -------------------------------------------------------------------------
# plot raw data
ggplot(df_pit, aes(x= ghost_tag, fill= ghost_tag)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-.5) +
  theme_minimal()

# filter out ghost tags
pit_fil <- df_pit %>% 
  filter(ghost_tag == "No")
n_distinct(pit_fil$tag_id2)
  
# create column for labeling occasion in future plots 
pit_fil$occasion2 <- ifelse(pit_fil$occasion == 1, "Occ1", 
                           ifelse(pit_fil$occasion == 2, "Occ2",
                                  ifelse(pit_fil$occasion == 3, "Occ3",
                                         ifelse(pit_fil$occasion == 4, "Occ4", "Occ5"))))

df_pit_move <- filter(pit_fil, !is.na(tag_id2)) %>% 
  distinct(occasion2, tag_id2, .keep_all = TRUE) %>%
  select(occasion2, tag_id2, section) %>%
  spread(occasion2, section) %>% 
  rename(tag = tag_id2)

df_pit_move <- df_pit_move %>%
  mutate(Mv12 = Occ2 - Occ1, Mv23 = Occ3 - Occ2, Mv34 = Occ4 - Occ3, Mv45 = Occ5 - Occ4) %>%
  select(tag, Mv12:Mv45) %>%
  gather(interv, move, Mv12:Mv45, factor_key = TRUE) %>%
  mutate(move = move * 10) %>% # section = 10m
  na.omit()

##' Figures -----------------------------------------------------------------

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




