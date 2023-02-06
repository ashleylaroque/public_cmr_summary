
# setup -------------------------------------------------------------------

rm(list=ls())
source(here::here("code/library.R"))
#source(data source)

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
