# R version 4.3.1
# Ashley LaRoque


# Set up  -----------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/format_cmr.R"))
source(here::here("code/format_habitat.R"))
source(here::here("code/format_movement.R"))


# General Plots -----------------------------------------------------------
# visualize section recapture vs section cap relationship
ggplot(df_move) +
  aes(x = section0,
      y = section1) +
  geom_point() +
  geom_smooth(method = "lm")

# visualize recap per target species
ftable(df_cmr$recap)
df_cmr %>% 
  mutate(Recap = ifelse(recap == "n", "No", "Yes")) %>% 
  ggplot(aes(x= recap, fill = Recap)) +
  scale_fill_manual(values = alpha(c("gold", "darkorange"))) +
  geom_bar() +
  theme_minimal() +
  facet_wrap(~species)

df_n %>%
  group_by(species) %>%
  tally(n) %>%
  ungroup() %>%
  ggplot(aes(reorder(species, -n), n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5))

# Correlations of fish abundance with habitat -----------------------------------------------------------------

# Correlation plot fish and habitat  
## sunfish
chart.Correlation(df_d_m[, c("d_bluegill", "d_green_sunfish", "d_redbreast_sunfish",
                             "depth_mean", "velocity_mean", "substrate_mean", 
                             "area", "area_ucb")], method="spearman", histogram=TRUE, cex = 10) 
## chubs
chart.Correlation(df_d_m[, c("d_creek_chub", "d_bluehead_chub", "d_striped_jumprock",
                             "depth_mean", "velocity_mean", "substrate_mean", 
                             "area", "area_ucb")], method="spearman", histogram=TRUE, cex = 10) 


# Histograms of movement frequency ----------------------------------------
df_m <- df_move %>% 
  mutate(move = (section1 - section0) * 10 - 5)

# Histogram of Cyprinid and Catastomid Movement per occasion at recap
cent.move.dist <- gghistogram(df_m[df_m$species %in% c('bluehead_chub','creek_chub', 'striped_jumprock'), ], 
                              x = "move", fill = "lightgrey",
                              xlab = "Distance (m)", ylab = "Frequency", binwidth = 10, 
                              facet.by = c("occasion1","species")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9) 
cent.move.dist

# Histogram of Centrarchids Movement
sunf.move.dist <- gghistogram(df_m[df_m$species %in% c('bluegill','green_sunfish','redbreast_sunfish'), ], 
                              x = "move", fill = "lightgrey",
                              xlab = "Distance (m)", ylab = "Frequency", binwidth = 10, 
                              facet.by = c("occasion_recap","species")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank())
sunf.move.dist

# Histogram of general movement over all occasions

# generate labels and order of plots 
fish_labs <- c(bluehead_chub = "Bluehead Chub", creek_chub = "Creek Chub", striped_jumprock = "Striped Jumprock",
               redbreast_sunfish = "Redbreast Sunfish", green_sunfish = "Green Sunfish", bluegill = "Bluegill")
fish_order <- c('bluehead_chub','creek_chub','striped_jumprock',
                'green_sunfish','redbreast_sunfish', 'bluegill')

# factor species column by the fish_order 
df_m2 <- arrange(transform(df_m,
                           species=factor(species,levels=fish_order)),species)

# visualize movement across all occasions
df_am <- df_m %>% 
  mutate(abs_move = abs(move)) %>% 
  filter(species %in% c("bluehead_chub", "creek_chub", "green_sunfish", "redbreast_sunfish"))

all_movement <- gghistogram(df_am, x= "abs_move", fill = "dodgerblue",
                            xlab = "Distance (m)", ylab = "Frequency", binwidth = 10) +
  #geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9) +
  facet_wrap2(species ~ ., 
              scales = "free",
              labeller = as_labeller(fish_labs))+
  theme(axis.text.x = element_text(angle = 45, vjust = .5)) 
all_movement


# Plot  raw values of movement and density -------------------------------------------------------------------

## plot movement vs. density
ggplot(df_m[df_m$species %in% c('bluegill','green_sunfish','redbreast_sunfish'), ], aes(density, move)) +
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Fish Density") +
  theme_minimal()+
  facet_grid(species ~ opponent, scales = "free")  # each grid shows how each species reacts to other species density
#geom_smooth(method=lm, se=FALSE, color = "darkgrey")

ggplot(df_m[df_m$species %in% c('creek_chub','bluehead_chub','striped_jumprock'), ], aes(density, move)) +
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Fish Density") +
  theme_minimal()+
  facet_grid(species ~ opponent, scales = "free")
#geom_smooth(method=lm, se=FALSE, color = "darkgrey")

## plot movement vs. body size
ggplot(df_m[df_m$species %in% c('bluegill','green_sunfish','redbreast_sunfish'), ], aes(size_ratio, move)) + # here this uses length per unit weight
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Body Size Ratio (mm/g)") +
  theme_minimal()+
  facet_grid(~ species, scales = "free") 
#geom_smooth(method=lm, se=FALSE, color = "darkgrey")

ggplot(df_m[df_m$species %in% c('creek_chub','bluehead_chub','striped_jumprock'), ], aes(size_ratio, move)) + # here this uses length per unit weight
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Body Size Ratio (mm/g)") +
  theme_minimal()+
  facet_grid(~ species , scales = "free")
#geom_smooth(method=lm, se=FALSE, color = "darkgrey")

# plot all habitat variables vs. fish density
ggplot(df_h_sec, aes(value, d )) + 
  geom_point(color= "#20A387FF")+
  labs(x = "Habitat", y = "Fish Density") +
  theme_minimal()+
  facet_wrap(~habitat_variable, scales = "free")

# plot selected habitat variables vs. movement
ggplot(df_h_sec[df_h_sec$habitat_variable %in% c('velocity_mean', 'area_ucb', 'substrate_mean', 'depth_mean'), ], # some likely correlated with each other ie pool area and velocity
       aes(value, move)) + 
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Habitat Variable") +
  theme_minimal()+
  facet_wrap(~habitat_variable, scales = "free")



# model estimate ----------------------------------------------------------

# format data from 'run_model_move'
df_out <- list.files("data_formatted",
                     pattern = "df_",
                     full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows()

dat_fig <- df_out %>% 
  select(y,
         para,
         "50%",
         "97.5%",
         "2.5%") %>% 
  rename(Species = y,
         Estimate = "50%" ,
         Upper95 = "97.5%" ,
         Lower95 = "2.5%") %>% 
  filter(str_detect(para, "b")) %>% 
  mutate(para = case_when(para == 'b[1]' ~ 'Intercept',
                          para == 'b[2]' ~ 'Length',
                          para == 'b[3]' ~ 'UCB Area',
                          para == 'b[4]' ~ 'Mean Temperature',
                          para == 'b[5]' ~ 'Density Creek Chub',
                          para == 'b[6]' ~ 'Density Bluehead Chub',
                          para == 'b[7]' ~ 'Density Green Sunfish',
                          para == 'b[8]' ~ 'Density Redbreast Sunfish')) %>% 
  mutate(para = factor(para, levels = rev(unique(para))))

## plot theme
plt_theme <- theme_bw() + theme(
  plot.background = element_blank(),
  
  panel.background = element_rect(grey(0.99)),
  panel.border = element_rect(),
  
  panel.grid = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  
  strip.background = element_blank(),
  strip.text.x = element_text(size = 15),
  strip.text.y = element_text(size = 15),
  axis.title = element_text(size = 15)
)


theme_set(plt_theme)
pd <- position_dodge(0.3)

fig <- dat_fig %>%
  ggplot(aes(x = Estimate, y = para, color = Species)) +
  geom_errorbar(aes(xmin = Lower95, xmax = Upper95),
                width = 0,
                position = pd) +
  geom_point(position = pd) +
  geom_vline(xintercept = 0,
             lty = 2,
             col = "gray") +
  ylab(NULL)
fig

species.labs <- c("Bluehead Chub", "Creek Chub")
names(species.labs) <- c("bluehead_chub", "creek_chub")

chub_est <- dat_fig %>% 
  filter(Species %in% c("creek_chub", "bluehead_chub")) %>% 
  ggplot(aes(x = Estimate, y = para, color = para)) +
  geom_errorbar(aes(xmin = Lower95, xmax = Upper95),
                width = 0,
                position = pd) +
  geom_point(position = pd) +
  geom_vline(xintercept = 0,
             lty = 2,
             col = "gray") +
  ylab(NULL) +
  theme(legend.position = "none") +
  facet_wrap2(~ Species, 
              labeller = labeller(Species = species.labs))
chub_est


species.labs <- c("Green Sunfish", "Redbreast Sunfish")
names(species.labs) <- c("green_sunfish", "redbreast_sunfish")

sun_est <- dat_fig %>% 
  filter(Species %in% c("green_sunfish", "redbreast_sunfish")) %>% 
  ggplot(aes(x = Estimate, y = para, color = para)) +
  geom_errorbar(aes(xmin = Lower95, xmax = Upper95),
                width = 0,
                position = pd) +
  geom_point(position = pd) +
  geom_vline(xintercept = 0,
             lty = 2,
             col = "gray") +
  ylab(NULL) +
  theme(legend.position = "none") +
  facet_wrap2(~ Species, 
              labeller = labeller(Species = species.labs))
sun_est

# abs move vs body size ---------------------------------------------------


strip1 <- strip_themed(background_x = elem_list_rect(fill = c("darkcyan", "maroon", "mediumpurple1", "steelblue3")))
species.labs <- c(" Bluehead Chub", "Creek Chub", "Green Sunfish", "Redbreast Sunfish")
names(species.labs) <- c("bluehead_chub", "creek_chub" ,"green_sunfish", "redbreast_sunfish")

fig_size <- df_am %>% 
  filter(species %in% c('bluehead_chub', 'creek_chub', 'green_sunfish','redbreast_sunfish')) %>% 
  drop_na(move) %>% 
  ggplot(aes(x = length0,
             y = abs_move,
             color = species)) +
  geom_point() +
  facet_wrap2(~ species, 
              scales = "free",
              strip = strip1,
              labeller = labeller(species = species.labs)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "gaussian"), se=F) +
  scale_color_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3"), 
                     name="Species") +
  labs(x= "Length at Capture (mm)", y= "Absolute Movement (m)") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        strip.text = element_text(color = 'white'))
fig_size 


fig_den <- df_am %>% 
  select(species, abs_move, adj_density_bluehead_chub, adj_density_creek_chub, 
         adj_density_green_sunfish, adj_density_redbreast_sunfish) %>% 
  filter(species %in% c("bluehead_chub", "creek_chub" ,"green_sunfish", "redbreast_sunfish")) %>% 
  pivot_longer(cols = starts_with("adj_"),
               names_to = "opponent", 
               values_to = "density") %>% 
  drop_na(abs_move) %>% 
  ggplot(aes(x = density ,
             y = abs_move, 
             color = species)) +
  geom_point(alpha = 0.5) +
  facet_grid(rows = vars(species),
             cols = vars(opponent),
             scales = "free",
             switch = "x",  # use switch = "y" to swap strip to the left side
             labeller = labeller(species = species.labs, opponent = opp.labs)) +
  # ylim(0, 1.4) +
  geom_smooth(method = "glm", 
              method.args = list(family = "gaussian"), se=F) +
  scale_color_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3"), 
                     name="Species") +
  labs(x= "Density (n/m^2)", y= "Absolute Movement (m)") +
  theme_minimal() +
  ggtitle("Opponent")+
  theme(legend.position = "none",
        text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.background = element_rect(color = "black"),
        strip.text = element_text(color = 'white'),
        strip.placement = "outside")
fig_den


g <- ggplot_gtable(ggplot_build(fig_den))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("darkcyan", "maroon", "mediumpurple1", "steelblue3", "darkcyan", "maroon", "mediumpurple1", "steelblue3")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)



# SFS Figures ----------------------------------------------------------
df_m2 <- df_m %>% 
  filter(species %in% c('bluehead_chub', 'creek_chub', 'green_sunfish','redbreast_sunfish')) %>% 
  filter(opponent %in% c('d_bluehead_chub', 'd_creek_chub', 'd_green_sunfish','d_redbreast_sunfish'))

species.l <- c( "Creek Chub")
names(species.l) <- c( "creek_chub")

opp.l <- c("Bluehead Chub", "Green Sunfish")
names(opp.l) <- c("d_bluehead_chub", "d_green_sunfish")

new <- df_m2 %>% 
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
  labs(x= "Density (n/m^2)", y= "Absolute Movement (m)") +
  theme_minimal() +
  ggtitle("Opponent")+
  scale_color_manual(values=c("maroon"), 
                     name="Species")+
  theme(legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.background = element_rect(color = "black"),
        strip.text = element_text(color = 'white'),
        strip.placement = "outside")
sfs_d

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


species.labs <- c("Bluehead Chub", "Creek Chub", "Green Sunfish", "Redbreast Sunfish")
names(species.labs) <- c("bluehead_chub", "creek_chub" ,"green_sunfish", "redbreast_sunfish")

opp.labs <- c("Bluehead Chub", "Creek Chub", "Green Sunfish", "Redbreast Sunfish")
names(opp.labs) <- c("adj_density_bluehead_chub", "adj_density_creek_chub" ,"adj_density_green_sunfish", "adj_density_redbreast_sunfish")


strip1 <- strip_themed(background_x = elem_list_rect(fill = c("darkcyan", "maroon", "mediumpurple1", "steelblue3")))
species.labs <- c(" Bluehead Chub", "Creek Chub", "Green Sunfish", "Redbreast Sunfish")
names(species.labs) <- c("bluehead_chub", "creek_chub" ,"green_sunfish", "redbreast_sunfish")

sfs_weight <- ggplot(df_m[df_m$species %in% c('bluehead_chub', 'creek_chub', 'green_sunfish','redbreast_sunfish'), ], 
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


strip2 <- strip_themed(background_x = elem_list_rect(fill = c( "maroon", "mediumpurple1")))
afs_size1 <- df_am %>% 
  filter(species %in% c('creek_chub', 'green_sunfish')) %>% 
  drop_na(move) %>% 
  ggplot(aes(x = length0,
             y = abs_move,
             color = species)) +
  geom_point() +
  facet_wrap2(~ species, 
              scales = "free",
              strip = strip2,
              labeller = labeller(species = species.labs)) +
  #  geom_smooth(method = "glm", 
  #            method.args = list(family = "gaussian"), se=F) +
  scale_color_manual(values=c("maroon", "mediumpurple1"), 
                     name="Species") +
  labs(x= "Length at Capture (mm)", y= "Absolute Movement (m)") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 20),
        strip.text = element_text(color = 'white'))
afs_size1
