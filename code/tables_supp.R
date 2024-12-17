# Tables for Supporting Information

rm(list = ls())

# Load Data ---------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/format_cmr.R"))
source(here::here("code/format_habitat.R"))
df_est <- readRDS("data_formatted/data_est.rds")
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

# calculate mean size of each tagged species (including replicates)
df_size_avg <- df_combined %>% 
  group_by(species) %>% 
  summarize(mu = mean(length0),
            sigma = sd(length0))

# calculate mean density of each tagged species 
df_den_avg <- df_combined %>% 
  select(adj_density_bluehead_chub, adj_density_creek_chub,
         adj_density_green_sunfish, adj_density_redbreast_sunfish) %>% 
  pivot_longer(cols = starts_with("adj_"),
               names_to = "opponent", 
               values_to = "density") %>% 
  group_by(opponent) %>% 
  summarize(mu = mean(density),
            sigma = sd(density))



# Table of Recaptures -------------------------------------------------------
 
tab_recap <- df_combined %>% 
  #group_by(species) %>%
  summarise(n_unique_cap = n_distinct(tag_id), # all unique individuals
            n_unique_recap = n_distinct(tag_id[!is.na(length1)]), # unique recaps
            n_replicate_cap = n(), # all captured (including replicates)
            n_replicate_recap = sum(!is.na(length1))) %>% # recaptured consecutive individuals (replicates)
          ungroup()
          
table_recap <- tab_recap %>% 
  kbl(format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      col.names = linebreak(c('Species',
                              'Capture\n(unique)',
                              'Recapture\n(unique)',
                              'Capture\n(total replicate)',
                              'Recapture\n(total replicate)'),
                            align = "c")) %>% 
  kable_styling(latex_options = "hold_position")

kableExtra::save_kable(table_recap, file = "output/table_recap.pdf")

# Table of Coefficients ---------------------------------------------------

tab_est <- df_est %>% 
  select(para, species, lower95, upper95, median) %>% 
  rename("Effect" = "para",
         "Lower 95% CI" = "lower95",
         "Upper 95% CI" = "upper95",
         "Species" = "species",
         "Estimate" = "median")

tab_est <- tab_est %>% 
  group_by(Species) %>% 
  mutate(Species =c(unique(Species), rep(" ", n()-1))) %>% 
  kbl(booktabs = TRUE,
      digits = 2,
      format = 'pandoc') %>% 
  kable_styling(latex_options = "hold_position")

kableExtra::save_kable(tab_est, file = "output/table_estimates.pdf")


# Table of Habitat Variables -----------------------------------------------

tab_hab <- df_h_sec 


# Table for Detection Probabilities ---------------------------------------

options(xtable.comment = FALSE)

df_p <- readRDS("data_formatted/output_cjs.rds") %>% 
  bind_rows() %>% 
  filter(str_detect(para, "zeta")) %>% 
  mutate(species = str_to_sentence(species) %>% 
           str_replace("_", " "),
         season = ifelse(para == "zeta[1]", "Winter", "Summer"),
         estimate = paste0(sprintf("%.2f", `50%`),
                           " [",
                           sprintf("%.2f", `2.5%`),
                           " - ",
                           sprintf("%.2f", `97.5%`),
                           "]")) %>% 
  select(Species = species,
         Season = season,
         Estimate = estimate) %>% 
  mutate(Species = ifelse(duplicated(Species), NA, Species))

## export
print(xtable(df_p,
             caption = "Seasonal detection probabilities estimated using the spatial Cormack-Jolly-Seber (CJS) model, presented as median estimates with corresponding 95% credible intervals.",
             label = "tab:detection"),
      tabular.environment = "tabular", # use \begin{tabular}
      sanitize.text.function = function(x) x, # for math mode
      include.rownames = FALSE,
      caption.placement = "top",
      file = "tex/table_detection.tex")

         