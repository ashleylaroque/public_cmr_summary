# R version 4.1.3 
# Ashley LaRoque


# Set up  -----------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/format_cmr_data.R"))
source(here::here("code/format_pit_detection.R"))

# Correlations of fish abundance with habitat -----------------------------------------------------------------

# Correlation plot fish and habitat  
## sunfish
chart.Correlation(df_abund_comb[, c("redbreast_sunfish", "green_sunfish", 
                                    "depth_mean", "velocity_mean", "substrate_mean", 
                                    "area", "area_ucb")], method="spearman", histogram=TRUE, cex = 10) 
## chubs
chart.Correlation(df_abund_comb[, c("creek_chub", "bluehead_chub",
                                    "depth_mean", "velocity_mean", "substrate_mean", 
                                    "area", "area_ucb")], method="spearman", histogram=TRUE, cex = 10) 


# Histograms of movement frequency ----------------------------------------


# Histogram of Cyprinid and Catastomid Movement

chub.move.dist <- gghistogram(df_occ_move[df_occ_move$species %in% c('bluehead_chub','creek_chub', 'striped_jumprock'), ], x = "move", fill = "lightgrey",
                              xlab = "Distance (m)", ylab = "Frequency", binwidth = 10, facet.by = c("interv","species")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank())

chub.move.dist

# Histogram of Centrarchids Movement

sunf.move.dist <- gghistogram(df_occ_move[df_occ_move$species %in% c('bluegill','green_sunfish','redbreast_sunfish'), ], x = "move", fill = "lightgrey",
                              xlab = "Distance (m)", ylab = "Frequency", binwidth = 10, facet.by = c("interv","species")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank())

sunf.move.dist

# Histogram of general movement over all occasions

## generate labels and order of plots 
fish_labs <- c(bluehead_chub = "Bluehead Chub", creek_chub = "Creek Chub", striped_jumprock = "Striped Jumprock",
               redbreast_sunfish = "Redbreast Sunfish", green_sunfish = "Green Sunfish", bluegill = "Bluegill")
fish_order <- c('bluehead_chub','creek_chub',' striped_jumprock',
                'green_sunfish','redbreast_sunfish', 'bluegill')

## factor species column by the fish_order 
df_m2 <- arrange(transform(df_m,
                           species=factor(species,levels=fish_order)),species)

## plot movement across all occasions

all_movement <- gghistogram(df_occ_move, x= "move", fill = "dodgerblue",
            xlab = "Distance (m)", ylab = "Frequency", binwidth = 10) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9) +
  facet_wrap(species ~ ., factor(levels= fish_order), labeller = as_labeller(fish_labs))+
  theme(axis.text.x = element_text(angle = 45, vjust = .5))
all_movement

ftable(df_cmr$recap)
ggplot(df_cmr, aes(x= recap, fill = species))+
  geom_bar() +
  theme_minimal()


# Plot emigration ~ density 
df_plot %>% 
  ggplot(aes(x = density,
             y = emigration, color= species)) +
  geom_point(alpha = 0.2) +
  facet_grid(rows = vars(species),
             cols = vars(opponent)) +
  ylim(0, 1.4) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), se=F) +
  scale_color_manual(values=c("darkcyan", "maroon", "mediumpurple1", "royalblue4", "palegreen4", "steelblue3"), 
                     name="Species") +
  labs(x= "Density", y= "Emigration") +
  theme(legend.position = "none")   



# Plot emigration ~ length
df_plot %>% 
  ggplot(aes(x = length_cap,
             y = emigration, color= species)) +
  geom_point(alpha = 0.2) +
  facet_wrap(facets = ~ species) +
  ylim(0, 1.2) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), se=F) +
  scale_color_manual(values=c("darkcyan", "maroon", "mediumpurple1", "royalblue4", "palegreen4", "steelblue3"), 
                     name="Species") +
  labs(x= "Length at Capture", y= "Emigration") +
  theme(legend.position = "none")+
  theme(text = element_text(size = 20)) 


