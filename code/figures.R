# R version 4.3.1
# Ashley LaRoque


# Set up  -----------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/format_cmr.R"))
source(here::here("code/format_habitat.R"))
source(here::here("code/format_movement.R"))

# Correlations of fish abundance with habitat -----------------------------------------------------------------

# Correlation plot fish and habitat  
## sunfish
chart.Correlation(df_abund_comb[, c("redbreast_sunfish", "green_sunfish", "bluegill",
                                    "depth_mean", "velocity_mean", "substrate_mean", 
                                    "area", "area_ucb")], method="spearman", histogram=TRUE, cex = 10) 
## chubs
chart.Correlation(df_abund_comb[, c("creek_chub", "bluehead_chub", "striped_jumprock",
                                    "depth_mean", "velocity_mean", "substrate_mean", 
                                    "area", "area_ucb")], method="spearman", histogram=TRUE, cex = 10) 


# Histograms of movement frequency ----------------------------------------


# Histogram of Cyprinid and Catastomid Movement

cent.move.dist <- gghistogram(df_occ_move[df_occ_move$species %in% c('bluehead_chub','creek_chub', 'striped_jumprock'), ], x = "move", fill = "lightgrey",
            xlab = "Distance (m)", ylab = "Frequency", binwidth = 10, facet.by = c("interv","species")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank())
cent.move.dist

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
fish_order <- c('bluehead_chub','creek_chub','striped_jumprock',
                'green_sunfish','redbreast_sunfish', 'bluegill')

## factor species column by the fish_order 
df_m2 <- arrange(transform(df_m,
                           species=factor(species,levels=fish_order)),species)

## plot movement across all occasions

all_movement <- gghistogram(df_occ_move, x= "move", fill = "dodgerblue",
            xlab = "Distance (m)", ylab = "Frequency", binwidth = 10) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9) +
  facet_wrap(species ~ ., labeller = as_labeller(fish_labs))+
  theme(axis.text.x = element_text(angle = 45, vjust = .5))
all_movement

# place before labeller but not working saying must be whole number = factor(levels= fish_order), 

ftable(df_cmr$recap)
df_cmr %>% 
  mutate(fi_col = ifelse(recap == "n", "darkblue", "lightgreen")) %>% 
  ggplot(aes(x= recap, fill = fi_col)) +
    geom_bar() +
   theme_minimal() +
   facet_wrap(~species)


# Plot emigration predicted by density and size -----------------------------------------------------------------

# Plot emigration ~ density 
df_plot %>% 
  ggplot(aes(x = density,
             y = emigration, color= species)) +
  geom_point(alpha = 0.2) +
  facet_grid(rows = vars(species),
             cols = vars(opponent),
             scales = "free") +
  ylim(0, 1.4) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), se=F) +
  scale_color_manual(values=c("darkcyan", "maroon", "mediumpurple1", "royalblue4", "palegreen4", "steelblue3"), 
                     name="Species") +
  labs(x= "Density", y= "Emigration") +
  theme(legend.position = "none")   

# plot for SFS
df_plot2 <- df_plot %>% 
  filter(species %in% c('bluehead_chub', 'creek_chub', 'green_sunfish','redbreast_sunfish')) %>% 
  filter(opponent %in% c('d_bluehead_chub', 'd_creek_chub', 'd_green_sunfish','d_redbreast_sunfish'))

species.labs <- c("Bluehead Chub", "Creek Chub", "Green Sunfish", "Redbreast Sunfish")
names(species.labs) <- c("bluehead_chub", "creek_chub" ,"green_sunfish", "redbreast_sunfish")

opp.labs <- c("Bluehead Chub", "Creek Chub", "Green Sunfish", "Redbreast Sunfish")
names(opp.labs) <- c("d_bluehead_chub", "d_creek_chub" ,"d_green_sunfish", "d_redbreast_sunfish")
#___________________
species.l <- c( "Creek Chub")
names(species.l) <- c( "creek_chub")

opp.l <- c("Bluehead Chub", "Green Sunfish")
names(opp.l) <- c("d_bluehead_chub", "d_green_sunfish")

new <- df_plot2 %>% 
  subset(species == "creek_chub") %>% 
  subset(opponent == "d_bluehead_chub" |
           opponent == "d_green_sunfish")

sfs_d <- ggplot(new, 
      aes(x = density ,
          y = emigration,
          color= "species")) +
  geom_point(alpha = 0.2) +
  ylim(0, 1.4) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), se=F) +
  facet_grid(rows = vars(species),
             cols = vars(opponent),
             scales = "free",
             switch = "x",  # use switch = "y" to swap strip to the left side
             labeller = labeller(species = species.l, opponent = opp.l)) +
  labs(x= "Density (count/m^2)", y= "Emigration") +
  theme_minimal() +
  ggtitle("Opponent")+
  scale_color_manual(values=c( "maroon"), 
                       name="Species")+
  theme(legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.background = element_rect(color = "black"),
        strip.text = element_text(color = 'white'),
        strip.placement = "outside")
g <- ggplot_gtable(ggplot_build(sfs_d))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("darkcyan", "mediumpurple1", "maroon" )
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)


sfs_density <- ggplot(df_plot2, 
                   aes(x = density ,
                       y = emigration, 
                      color= species)) +
  geom_point(alpha = 0.2) +
  facet_grid(rows = vars(species),
             cols = vars(opponent),
             scales = "free",
             switch = "x",  # use switch = "y" to swap strip to the left side
             labeller = labeller(species = species.labs, opponent = opp.labs)) +
  ylim(0, 1.4) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), se=F) +
  scale_color_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3"), 
                     name="Species") +
  labs(x= "Density (count/m^2)", y= "Emigration") +
  theme_minimal() +
  ggtitle("Opponent")+
  theme(legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.background = element_rect(color = "black"),
        strip.text = element_text(color = 'white'),
        strip.placement = "outside")
sfs_density


g <- ggplot_gtable(ggplot_build(sfs_density))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("darkcyan", "maroon", "mediumpurple1", "steelblue3", "darkcyan", "maroon", "mediumpurple1", "steelblue3")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)



# Plot emigration ~ weight

df_plot %>% 
  ggplot(aes(x = length_cap,
             y = emigration, color= species)) +
  geom_point(alpha = 0.2) +
  facet_wrap(facets = ~ species, 
             scales = "free") +
  ylim(0, 1.2) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), se=F) +
  scale_color_manual(values=c("darkcyan", "maroon", "mediumpurple1", "royalblue4", "palegreen4", "steelblue3"), 
                     name="Species") +
  labs(x= "N at Capture", y= "Emigration") +
  theme(legend.position = "none")+
  theme(text = element_text(size = 20)) 

# plot for SFS
strip1 <- strip_themed(background_x = elem_list_rect(fill = c("darkcyan", "maroon", "mediumpurple1", "steelblue3")))
species.labs <- c(" Bluehead Chub", "Creek Chub", "Green Sunfish", "Redbreast Sunfish")
names(species.labs) <- c("bluehead_chub", "creek_chub" ,"green_sunfish", "redbreast_sunfish")

sfs_weight <- ggplot(df_plot[df_plot$species %in% c('bluehead_chub', 'creek_chub', 'green_sunfish','redbreast_sunfish'), ], 
         aes(x = weight_cap,
             y = emigration, 
             color= species)) +
  geom_point(alpha = 0.2) +
  facet_wrap2(~ species, 
             scales = "free",
             strip = strip1,
             labeller = labeller(species = species.labs)) +
  ylim(0, 1.2) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), se=F) +
  scale_color_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3"), 
                     name="Species") +
  labs(x= "Weight at Capture", y= "Emigration") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        strip.text = element_text(color = 'white'))

sfs_weight

strip1 <- strip_themed(background_x = elem_list_rect(fill = c("darkcyan", "maroon", "mediumpurple1", "steelblue3", "lightblue", "lightpink")))
species.labs2 <- c(" Bluehead Chub", "Creek Chub","Striped Jumprock", "Bluegill", "Green Sunfish", "Redbreast Sunfish")
names(species.labs2) <- c("bluehead_chub", "creek_chub", "striped_jumprock", "bluegill" ,"green_sunfish", "redbreast_sunfish")

ggplot(df_plot[df_plot$species %in% c('bluehead_chub', 'striped_jumprock', 'creek_chub', 'bluegill', 'green_sunfish','redbreast_sunfish'), ], 
       aes(x = section_cap,
           y = density, 
           color= species)) +
  geom_point(alpha = 0.2) +
  facet_wrap2(~ species, 
              scales = "free",
              strip = strip1,
              labeller = labeller(species = species.labs2)) +
  #ylim(0, 1.2) +
  geom_smooth(method = "gam", 
              method.args = list(family = "gaussian"), se=F) +
  scale_color_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3", "lightblue", "lightpink"), 
                     name="Species") +
  labs(x= "Section", y= "Density") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 20))

ggplot(df_plot) +
  aes(x = section_cap,
      y = section_recap) +
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(df_plot[df_plot$species %in% c('bluehead_chub', 'striped_jumprock', 'creek_chub', 'bluegill', 'green_sunfish','redbreast_sunfish'), ], 
       aes(x = section_cap,
           y = density, 
           color= species)) +
  geom_point(alpha = 0.2) +
  facet_wrap2(~ species, 
              scales = "free",
              strip = strip1,
              labeller = labeller(species = species.labs2)) +
  #ylim(0, 1.2) +
  geom_smooth(method = "gam", 
              method.args = list(family = "gaussian"), se=F) +
  scale_color_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3", "lightblue", "lightpink"), 
                     name="Species") +
  labs(x= "Section", y= "Movement") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 20))

# Plot  raw values of movement and density -------------------------------------------------------------------

## plot movement vs. density
ggplot(df_mo[df_mo$d_species %in% c('bluegill','green_sunfish','redbreast_sunfish'), ], aes(d, move)) +
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Fish Density") +
  theme_minimal()+
  facet_grid(d_species ~ species.x, scales = "free")  # each grid shows how each species reacts to other species density
#geom_smooth(method=lm, se=FALSE, color = "darkgrey")

ggplot(df_mo[df_mo$d_species %in% c('creek_chub','bluehead_chub','striped_jumprock'), ], aes(d, move)) +
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Fish Density") +
  theme_minimal()+
  facet_grid(d_species ~ species.x, scales = "free")
#geom_smooth(method=lm, se=FALSE, color = "darkgrey")
#### for the two above figures, is this actually pulling the correct information or is it getting mismatched?

## plot movement vs. body size
ggplot(df_mo[df_mo$d_species %in% c('bluegill','green_sunfish','redbreast_sunfish'), ], aes(size_ratio, move)) + # here this uses length per unit weight
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Body Size Ratio (mm/g)") +
  theme_minimal()+
  facet_grid(d_species ~ species.x , scales = "free") 
#geom_smooth(method=lm, se=FALSE, color = "darkgrey")

ggplot(df_mo[df_mo$d_species %in% c('creek_chub','bluehead_chub','striped_jumprock'), ], aes(size_ratio, move)) + # here this uses length per unit weight
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Body Size Ratio (mm/g)") +
  theme_minimal()+
  facet_grid(d_species ~ species.x , scales = "free")
#geom_smooth(method=lm, se=FALSE, color = "darkgrey")

## plot all habitat variables vs. fish density

ggplot(df_z, aes(value , d )) + 
  geom_point(color= "#20A387FF")+
  labs(x = "Habitat", y = "Fish Density") +
  theme_minimal()+
  facet_wrap(~habitat_variable, scales = "free")

## plot selected habitat variables vs. movement

ggplot(df_z[df_z$habitat_variable %in% c('velocity_mean', 'area_ucb', 'substrate_mean', 'depth_mean'), ], # some likely correlated with each other ie pool area and velocity
       aes(value , move)) + 
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Habitat Variable") +
  theme_minimal()+
  facet_wrap(~habitat_variable, scales = "free")
