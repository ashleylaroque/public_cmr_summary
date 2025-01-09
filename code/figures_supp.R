# Figures for supporting information

rm(list = ls())

# Load Data ---------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/format_cmr.R"))
source(here::here("code/format_habitat.R"))
df_combined <- readRDS("data_formatted/data_combined.rds") %>% 
  mutate(move = (section1 - section0) * 10, 
         abs_move = abs(move), # generate absolute movement for figures
         month = format(datetime0, "%m") %>% 
           as.numeric(month),
         season = ifelse(between(month, 4, 9),
                         yes = 1, # summer
                         no = 0),
         log_length = log(length0)) %>% 
  filter(species %in% c("bluehead_chub",
                        "creek_chub",
                        "green_sunfish",
                        "redbreast_sunfish"))

# Histogram of recaps by species ------------------------------------------

df_cmr %>% 
  filter(species %in% c("creek_chub", "bluehead_chub", "green_sunfish", "redbreast_sunfish")) %>% 
  mutate(Recap = ifelse(recap == "n", "No", "Yes")) %>% 
  ggplot(aes(x= recap, fill = Recap)) +
  scale_fill_manual(values = alpha(c("gray", "steelblue3"))) +
  geom_bar() +
  theme_minimal() +
  facet_wrap(~species)


# Abundance of all captured species ---------------------------------------

fig_abundance <- df_n %>%
  group_by(species) %>%
  tally(n) %>%
  ungroup() %>%
  ggplot(aes(reorder(species, -n), n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        text = element_text(size = 15)) +
  xlab("Species") +
  ylab("Abundance")

ggsave(fig_abundance, 
       filename = "output/fig_abundance.pdf",
       height = 10,
       width = 12)

# df_abund <- df_n %>% 
#   group_by(species) %>% 
#   tally(n) %>% 
#   mutate(total = sum(n),
#          percent = n / total) %>% 
#   filter(species %in% c("creek_chub", "bluehead_chub", "green_sunfish", "redbreast_sunfish")) %>% 
#   mutate(top4 = sum(n))


# Boxplot of mean body size -----------------------------------------------

fig_size_dist <- ggplot(df_combined, aes(species, length0, fill = species)) +
  geom_boxplot() +
  scale_fill_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3"))+
  theme(legend.position = "none",
        text = element_text(size = 15)) +
  xlab("Species") +
  ylab("Length (mm)")

ggsave(fig_size_dist, 
       filename = "output/fig_size_dist.pdf",
       height = 10,
       width = 12)

# Histogram of total movement  ----------------------------------------
species.labs <- c("Bluehead Chub", "Creek Chub", "Green Sunfish", "Redbreast Sunfish")
names(species.labs) <- c("bluehead_chub", "creek_chub", "green_sunfish", "redbreast_sunfish")

# visualize movement across all occasions
# gghistogram(df_combined, x= "abs_move", fill = "#20A387FF",
#                             xlab = "Distance (m)", ylab = "Frequency", binwidth = 10) +
#   #geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9) +
#   facet_wrap2(species ~ ., 
#               scales = "free",
#               labeller = as_labeller(species.labs))+
#   theme(axis.text.x = element_text(angle = 45, vjust = .5)) 



# Histogram of total movement across seasons ------------------------------

# movement between seasons
season.labs <- c("0" = "Winter", "1" = "Summer")
fig_total_move <- gghistogram(df_combined, x= "abs_move", fill = "#20A387FF",
            xlab = "Distance (m)", ylab = "Frequency", binwidth = 10) +
  theme(text = element_text(size = 15)) +
  facet_grid(season ~ species, 
              scales = "free",
              labeller = labeller(species = species.labs, season = season.labs))

ggsave(fig_total_move, 
       filename = "output/fig_total_move.pdf",
       height = 10,
       width = 12)

# Correlations of habitat -----------------------------------------------------------------



chart.Correlation(df_combined[, c("depth_mean", "velocity_mean", "substrate_mean", 
                                         "area", "area_ucb", "mean_temp")], 
                         method="spearman", 
                         histogram=TRUE, 
                         cex = 10)

# PCA Habitat -------------------------------------------------------------

# cmr.pca <- prcomp(df_combined[,c("depth_mean", "velocity_mean", "substrate_mean", 
#                                  "area", "area_ucb", "mean_temp")], 
#                    center = TRUE, 
#                    scale. = TRUE) 
# summary(cmr.pca)
# autoplot(cmr.pca, data = df_combined,
#        loadings = TRUE, loadings.colour = '#20A387FF',
#        loadings.label = TRUE, loadings.label.colour = 'mediumpurple1')
# 


