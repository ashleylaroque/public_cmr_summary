# R version 4.3.1
# Ashley LaRoque

rm(list = ls())

# Load Data ---------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/format_cmr.R"))
source(here::here("code/format_habitat.R"))

df_combined <- readRDS("data_formatted/data_combined.rds")  # comes from 'run_model_move'
df_output <- readRDS("data_formatted/output_move.rds") %>% 
  bind_rows() %>% 
  rename(species = "y",
         median = "50%" ,
         upper95 = "97.5%" ,
         lower95 = "2.5%") %>% 
  drop_na(var) # comes from 'run_model_move'

# Set up  -----------------------------------------------------------------

# format data for figures
df_combined <- df_combined %>% 
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

# separate target species for use in loops
usp <- c("bluehead_chub",
         "creek_chub",
         "green_sunfish",
         "redbreast_sunfish") %>%
  sort()

# calculate mean size of each species 
df_size <- df_combined %>% 
  group_by(species) %>% 
  summarize(mu = mean(length0),
            sigma = sd(length0))

# Plot Theme --------------------------------------------------------------

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
  axis.title = element_text(size = 15))

theme_set(plt_theme)

pd <- position_dodge(0.3)

species.labs <- c("Bluehead Chub", "Creek Chub", "Green Sunfish", "Redbreast Sunfish")
names(species.labs) <- c("bluehead_chub", "creek_chub", "green_sunfish", "redbreast_sunfish")

opp.labs <- c("Density Bluehead Chub", "Density Creek Chub", "Density Green Sunfish", "Density Redbreast Sunfish")
names(opp.labs) <- c("adj_density_bluehead_chub", "adj_density_creek_chub" ,"adj_density_green_sunfish", "adj_density_redbreast_sunfish")
strip1 <- strip_themed(background_x = elem_list_rect(fill = c("darkcyan", "maroon", "mediumpurple1", "steelblue3")))

# General Plots -----------------------------------------------------------
# visualize section recapture vs section cap relationship
ggplot(df_combined) +
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

# visualize abundance of all captured species
df_n %>%
  group_by(species) %>%
  tally(n) %>%
  ungroup() %>%
  ggplot(aes(reorder(species, -n), n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5))

# visualize mean size
ggplot(df_combined, aes(species, length0)) +
  geom_boxplot() 

# Correlations of fish abundance with habitat -----------------------------------------------------------------

## sunfish
chart.Correlation(df_combined[, c("adj_density_green_sunfish", "adj_density_redbreast_sunfish",
                                  "depth_mean", "velocity_mean", "substrate_mean", 
                                  "area", "area_ucb")], method="spearman", histogram=TRUE, cex = 10) 
## chubs
chart.Correlation(df_combined[, c("adj_density_creek_chub", "adj_density_bluehead_chub",
                                  "depth_mean", "velocity_mean", "substrate_mean", 
                                  "area", "area_ucb")], method="spearman", histogram=TRUE, cex = 10) 


# Histograms of movement frequency ----------------------------------------

# Histogram of Cyprinid and Catastomid Movement per occasion at recap
gghistogram(df_combined[df_combined$species %in% c('bluehead_chub','creek_chub', 'striped_jumprock'), ], 
            x = "move", fill = "lightgrey",
            xlab = "Distance (m)", ylab = "Frequency", binwidth = 10, 
            facet.by = c("occasion1","species")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9) 


# Histogram of Centrarchids Movement
gghistogram(df_combined[df_combined$species %in% c('bluegill','green_sunfish','redbreast_sunfish'), ], 
            x = "move", fill = "lightgrey",
            xlab = "Distance (m)", ylab = "Frequency", binwidth = 10, 
            facet.by = c("occasion1","species")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9)


# Histogram of general movement over all occasions

# generate labels and order of plots 
fish_labs <- c(bluehead_chub = "Bluehead Chub", creek_chub = "Creek Chub", striped_jumprock = "Striped Jumprock",
               redbreast_sunfish = "Redbreast Sunfish", green_sunfish = "Green Sunfish", bluegill = "Bluegill")
fish_order <- c('bluehead_chub','creek_chub','striped_jumprock',
                'green_sunfish','redbreast_sunfish', 'bluegill')

# visualize movement across all occasions
all_movement <- gghistogram(df_combined, x= "abs_move", fill = "dodgerblue",
                            xlab = "Distance (m)", ylab = "Frequency", binwidth = 10) +
  #geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.9) +
  facet_wrap2(species ~ ., 
              scales = "free",
              labeller = as_labeller(fish_labs))+
  theme(axis.text.x = element_text(angle = 45, vjust = .5)) 
all_movement

# movement between seasons
season_labs <- c("0" = "Winter", "1" = "Summer")
gghistogram(df_combined, x= "abs_move", fill = "dodgerblue",
            xlab = "Distance (m)", ylab = "Frequency", binwidth = 10) +
  facet_wrap2(season ~ ., 
              scales = "free",
              labeller = as_labeller(season_labs))

# Model estimate ----------------------------------------------------------

dat_fig <- df_output %>% 
  filter(str_detect(para, "b")) %>% 
  mutate(para = case_when(var == '(Intercept)' ~ 'Intercept',
                          var == 'log_length' ~ 'log Length',
                          var == 'area_ucb' ~ 'UCB Area',
                          var == 'mean_temp' ~ 'Mean Temperature',
                          var == 'adj_density_creek_chub' ~ 'Density Creek Chub',
                          var == 'adj_density_bluehead_chub' ~ 'Density Bluehead Chub',
                          var == 'adj_density_green_sunfish' ~ 'Density Green Sunfish',
                          var == 'adj_density_redbreast_sunfish' ~ 'Density Redbreast Sunfish')) %>% 
  mutate(para = factor(para, levels = rev(unique(para))))

# estimates all together colored by species
dat_fig %>%
  ggplot(aes(x = median, y = para, color = species)) +
  geom_errorbar(aes(xmin = lower95, xmax = upper95),
                width = 0,
                position = pd) +
  geom_point(position = pd) +
  geom_vline(xintercept = 0,
             lty = 2,
             col = "gray") +
  ylab(NULL)

# estimates faceted by species 
fig_est <- dat_fig %>% 
  ggplot(aes(x = median, y = para, color = para)) +
  geom_errorbar(aes(xmin = lower95, xmax = upper95),
                width = 0,
                position = pd) +
  geom_point(position = pd) +
  geom_vline(xintercept = 0,
             lty = 2,
             col = "gray") +
  labs(y = NULL,
       x = "Estimate") +
  theme(legend.position = "none") +
  facet_wrap( ~ species, nrow = 2, ncol = 2, 
              labeller = labeller(species = species.labs))
fig_est


# Effect of Body Size ---------------------------------------------------

x_name <- df_output %>% 
  filter(var != "(Intercept)") %>% 
  pull(var) %>% 
  unique()

df_y <- foreach(k = usp, .combine = bind_rows) %do% {
  
  v_b <- df_output %>% 
    filter(species == k) %>% 
    pull(median)
  
  df_x <- foreach(v = x_name,
                  .combine = bind_rows) %do% {
                    bid <- which(c("(Intercept)", x_name) == v)        
                    
                    ## rename focused predictor name to `x`
                    df_v <- df_combined %>%
                      filter(species == k) %>% 
                      rename(x = all_of(v))
                    
                    ## calculate mean and sd for standardization
                    ## - this calculation must be done BEFORE dropping rows
                    ## - otherwise mean & sd are different from what was used in the model
                    mu_x <- mean(df_v$x)
                    sd_x <- sd(df_v$x)
                    
                    cout <- df_v %>% 
                      drop_na(section1) %>% 
                      reframe(x_value = seq(min(x, na.rm = T),
                                            max(x, na.rm = T),
                                            length = 100),
                              scl_x = (x_value - mu_x) / sd_x,
                              species = rep(unique(species))) %>% 
                      mutate(log_sigma = v_b[1] + v_b[bid] * scl_x, 
                             y = (exp(log_sigma) * sqrt(2)) / sqrt(pi),
                             focus = v)
                    
                    ## safer to specify what is a return object
                    return(cout)
                  }
  
}

df_fig <- df_y %>% 
  left_join(df_output,
            by = c("species",
                   "focus" = "var")) %>% 
  rowwise() %>% 
  mutate(sig = ifelse(between(0, lower95, upper95), 
                      "no",
                      "yes")) %>% 
  ungroup()

(fig_size <- df_combined %>%
    drop_na(section1) %>% # figure only includes recap
    ggplot(aes(x = log_length,
               y = abs_move / intv,
               color = species)) +
    geom_point(alpha = 0.5) +
    geom_line(data = df_fig %>% 
                filter(focus == "log_length"),
              aes(x = x_value,
                  y = y,
                  linetype = sig)) +
    scale_linetype_manual(values = c("yes" = "solid",
                                     "no" = "dashed")) +
    facet_wrap2(~ species,
                scales = "free",
                strip = strip1,
                labeller = labeller(species = species.labs)) +
    scale_color_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3"),
                       name="Species") +
    labs(x= "Length at Capture (mm)", y= "Absolute Movement (m/day)") +
    theme(legend.position = "none",
          #text = element_text(size = 20),
          strip.text = element_text(color = 'white')))

# Effect of Density -------------------------------------------------------

(fig_den <- df_combined %>% 
   select(species,
          intv,
          abs_move,
          adj_density_bluehead_chub,
          adj_density_creek_chub, 
          adj_density_green_sunfish,
          adj_density_redbreast_sunfish) %>% 
   pivot_longer(cols = starts_with("adj_"),
                names_to = "opponent", 
                values_to = "density") %>% 
   drop_na(abs_move) %>% 
   ggplot(aes(x = density,
              y = abs_move / intv, 
              color = species)) +
   geom_point(alpha = 0.5) + 
   geom_line(data = df_fig %>% 
               filter(str_detect(focus, "adj_density")) %>% 
               rename(opponent = "focus"),
             aes(x = x_value,
                 y = y,
                 color = species,
                 linetype = sig))  +
   scale_linetype_manual(values = c("yes" = "solid",
                                    "no" = "dashed")) +
   facet_grid(rows = vars(species),
              cols = vars(opponent),
              scales = "free",
              switch = "x",  # use switch = "y" to swap strip to the left side
              labeller = labeller(species = species.labs, opponent = opp.labs)) +
   scale_color_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3"), 
                      name = "species") +
   labs(x= expression("Density (n /"~m^2*")"),
        y= "Absolute Movement (m / day)") +
   theme(legend.position = "none",
         text = element_text(size = 15),
         plot.title = element_text(hjust = 0.5, size = 20),
         strip.background = element_rect(color = "black"),
         strip.text = element_text(color = 'white'),
         strip.placement = "outside"))

fig_density <- ggplot_gtable(ggplot_build(fig_den))
strip_both <- which(grepl('strip-', fig_density$layout$name))
fills <- c("darkcyan", "maroon", "mediumpurple1", "steelblue3", "darkcyan", "maroon", "mediumpurple1", "steelblue3")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', fig_density$grobs[[i]]$grobs[[1]]$childrenOrder))
  fig_density$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(fig_density)



