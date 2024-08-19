#' Author: Ashley LaRoque, Akira Terui
#' Description:
#' This script runs modified dispersal-observation model for predicting movement with density and body size

# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")
source("code/function.R")


# data --------------------------------------------------------------------

## format data
df_move0 <- readRDS("data_formatted/data_move.rds") %>% 
  drop_na(section0)

df_zeta <- readRDS("data_formatted/data_detection.rds") # comes from 'run_model_cjs_move'
df_season <- readRDS("data_formatted/data_season.rds") # comes from 'run_model_cjs_move'
df_hobo <- readRDS("data_formatted/data_water_hobo.rds")   # comes from 'format_water_level'


df_den <- readRDS("data_formatted/data_density.rds") %>% 
  left_join(df_season, by = "occasion") %>% 
  mutate(season= case_when(season == 0 ~ "winter",
                           season == 1 ~ "summer")) %>% 
   left_join(df_zeta,
            by = c("species", "season")) %>% 
  mutate(adj_density = (density / estimate))

df_den_adj <- df_den %>% 
  pivot_wider(id_cols = c(occasion, section, area),
              names_from = species,
              values_from = c(n, adj_density))

df_h <- readRDS("data_formatted/data_habitat.rds") %>% 
  dplyr::select(-area)

## combine movement, density, and habitat
df_move <- df_move0 %>% # movement dataframe
  left_join(df_den_adj, # add density values
            by = c("occasion0" = "occasion",
                   "section0" = "section")) %>% 
  left_join(df_h, # add habitat variables 
            by = c("occasion0" = "occasion",
                   "section0" = "section")) %>% 
  left_join(df_hobo, # add water temp
            by = c("occasion0" = "occasion")) %>% 
  mutate(intv = as.numeric(datetime1 - datetime0),
         y = ifelse(is.na(section1), 0, 1)) %>% 
  group_by(occasion0) %>% 
  mutate(intv = ifelse(is.na(intv),
                       median(intv, na.rm = TRUE),
                       intv)) %>% 
  ungroup() %>% 
  select(-c(starts_with("n_")))


# run jags ----------------------------------------------------------------

usp <- c("green_sunfish",
         "redbreast_sunfish",
         "creek_chub",
         "bluehead_chub") %>% 
  sort()

list_est <- foreach(x = usp) %do% {
  
  df_i <- filter(df_move, species == x)
  
  ## data for jags
  list_jags <- with(df_i,
                    list(Y = y,
                         X1 = section1 * 10 - 5,
                         X0 = section0 * 10 - 5, 
                         Nsample = nrow(df_i),
                         Intv = intv))
  
  ## select predictors
  X <- df_i %>% 
    dplyr::select(length0,    # total length of individual
                  area_ucb,   # area of undercut bank coverage
                  mean_temp,  # temp
                  adj_density_bluehead_chub, # seasonally adjusted density
                  adj_density_creek_chub, 
                  adj_density_green_sunfish,
                  adj_density_redbreast_sunfish) %>% 
    mutate(int_bhc = length0 * adj_density_bluehead_chub, # interactive term for each species
           int_crc = length0 * adj_density_creek_chub,
           int_gsf = length0 * adj_density_green_sunfish,
           int_rbs = length0 * adj_density_redbreast_sunfish,
      across(.cols = c(length0, area_ucb, mean_temp, 
                       starts_with("adj_density"), starts_with("int_")),
             .fns = function(x) c(scale(x)))) %>% 
    model.matrix(~., data = .)
  
  list_jags$X <- X
  list_jags$K <- ncol(X)
  
  ## run.jags arguments
  ## - initial values
  n_chain <- 3
  
  inits <- replicate(n_chain,
                     list(.RNG.name = "base::Mersenne-Twister",
                          .RNG.seed = NA),
                     simplify = FALSE)
  
  for (j in 1:n_chain) inits[[j]]$.RNG.seed <- 100 * j
  
  ## - parameters to be monitored
  para <- c("b")
  
  ## model files
  m <- runjags::read.jagsfile("code/model_move.R")
  
  ## run model
  post <- runjags::run.jags(model = m$model,
                            data = list_jags,
                            monitor = para,
                            burnin = 1000,
                            sample = 1000,
                            thin = 5,
                            n.chains = n_chain,
                            inits = inits,
                            method = "parallel",
                            module = "glm")
  
  MCMCvis::MCMCsummary(post$mcmc) %>% 
    as_tibble(rownames = "para") %>% 
    mutate(y = x,
           var = colnames(X)) %>% 
    relocate(var)
}

## export
saveRDS(list_est, file = "data_formatted/output_move.rds")

## check for convergence and save as .csv
# a few values are ~1.12/1.13 do those need to be at 1.10 for us to say it has truly converged? seems like yes
bhc <- list_est[[1]]
write_csv(bhc, "data_formatted/df_bhc.csv")
crc <- list_est[[2]]
write_csv(crc, "data_formatted/df_crc.csv")
gsf <- list_est[[3]]
write_csv(gsf, "data_formatted/df_gsf.csv")
rbs <- list_est[[4]]
write_csv(rbs, "data_formatted/df_rbs.csv")

df_out <- list.files("data_formatted",
              pattern = "df_",
              full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows()

# Ignore --> playing with figures will move later-----------------------------------------------------------------

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
                          para == 'b[4]' ~ 'Density Creek Chub',
                          para == 'b[5]' ~ 'Density Bluehead Chub',
                          para == 'b[6]' ~ 'Density Green Sunfish',
                          para == 'b[7]' ~ 'Density Redbreast Sunfish')) %>% 
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

dat_fig %>% 
  ggplot(aes(x = Estimate, y = para, color = para)) +
  geom_errorbar(aes(xmin = Lower95, xmax = Upper95),
                width = 0,
                position = pd) +
  geom_point(position = pd) +
  geom_vline(xintercept = 0,
             lty = 2,
             col = "gray") +
  ylab(NULL) +
  facet_grid(~Species)

