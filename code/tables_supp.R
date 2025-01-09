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

# Mean size of each tagged species (including replicates) -----------------

df_size_avg <- df_combined %>% 
  group_by(species) %>% 
  summarize(Mu = mean(length0),
            Sigma = sd(length0)) %>% 
  rename("Species" = "species")

tab_size <- df_size_avg %>% 
  kbl(format = "html",
      booktabs = TRUE,
      escape = FALSE,
      digits = 2) %>% 
  kable_styling() %>% 
  save_kable("table_body_size.pdf")

# Mean density of each tagged species  ------------------------------------

df_den_avg <- df_combined %>% 
  select(adj_density_bluehead_chub, adj_density_creek_chub,
         adj_density_green_sunfish, adj_density_redbreast_sunfish) %>% 
  pivot_longer(cols = starts_with("adj_"),
               names_to = "opponent", 
               values_to = "density") %>% 
  group_by(opponent) %>% 
  summarize(Mu = mean(density),
            Sigma = sd(density)) %>% 
  rename("Species" = "opponent") %>% 
  mutate(Species = case_when(Species == "adj_density_bluehead_chub" ~ "Bluehead chub",
                             Species == "adj_density_creek_chub" ~ "Creek chub",
                             Species == "adj_density_green_sunfish" ~ "Green sunfish",
                             Species == "adj_density_redbreast_sunfish" ~ "Redbreast sunfish"))

df_den_avg %>% 
  kbl(format = "html",
      booktabs = TRUE,
      escape = FALSE,
      digits = 2) %>% 
  kable_styling() %>% 
  add_header_above(c(" " = 1, "Adjusted Density" = 2), align = "c") %>% 
  save_kable("table_den.pdf")



# Table of Recaptures -------------------------------------------------------
 
tab_recap <- df_combined %>% 
  group_by(species) %>%
  summarise(n_unique_cap = n_distinct(tag_id), # all unique individuals
            n_unique_recap = n_distinct(tag_id[!is.na(length1)]), # unique recaps
            n_replicate_cap = n(), # all captured (including replicates)
            n_replicate_recap = sum(!is.na(length1))) %>% # recaptured consecutive individuals (replicates)
          ungroup() %>% 
  mutate(species = case_when(species == "bluehead_chub" ~ "Bluehead chub",
                             species == "creek_chub" ~ "Creek chub",
                             species == "green_sunfish" ~ "Green sunfish",
                             species == "redbreast_sunfish" ~ "Redbreast sunfish"))
          
tab_recap %>% 
  kbl(format = "html",
      booktabs = TRUE,
      escape = FALSE,
      digits = 2,
      col.names = c('Species',
                    'Capture',
                    'Recapture',
                    'Capture',
                    'Recapture')) %>% 
  kable_styling() %>% 
  add_header_above(c(" " = 1, "Unique" = 2, "Total Replicate" = 2)) %>% 
  save_kable("table_recap.pdf")


# Table of Coefficients ---------------------------------------------------

tab_est <- df_est %>% 
  select(species, para, lower95, upper95, median) %>% 
  rename("Effect" = "para",
         "Lower 95% CI" = "lower95",
         "Upper 95% CI" = "upper95",
         "Species" = "species",
         "Estimate" = "median") %>% 
  mutate(Species = case_when(Species == "bluehead_chub" ~ "Bluehead chub",
                             Species == "creek_chub" ~ "Creek chub",
                             Species == "green_sunfish" ~ "Green sunfish",
                             Species == "redbreast_sunfish" ~ "Redbreast sunfish"))

tab_est %>% 
  group_by(Species) %>% 
  mutate(Species =c(unique(Species), rep(" ", n()-1))) %>% 
  kbl(format = "html",
      booktabs = TRUE,
      escape = FALSE,
      digits = 2) %>% 
  kable_styling() %>% 
  save_kable("table_estimates.pdf")


# Table of Habitat Variables -----------------------------------------------

tab_hab <- df_h_sec %>% 
  pivot_longer(cols = -c(occasion, section),
               names_to = "habitat_variable",
               values_to = "value") %>% 
  group_by(habitat_variable) %>% 
  summarise(Mu = mean(value),
            Sigma = sd(value)) %>% 
  mutate(habitat_variable = case_when(habitat_variable == "area" ~ "Total Section Area",
                                      habitat_variable == "width" ~ "Mean Width",
                                      habitat_variable == "section_length" ~ "Section Length",
                                      habitat_variable == "area_pool" ~ "Pool Area",
                                      habitat_variable == "area_riffle" ~ "Riffle Area",
                                      habitat_variable == "area_run" ~ "Run Area",
                                      habitat_variable == "area_ucb" ~ "Habitat Refuge Area",
                                      habitat_variable == "depth_mean" ~ "Mean Depth",
                                      habitat_variable == "velocity_mean" ~ "Mean Velocity",
                                      habitat_variable == "substrate_mean" ~ "Mean Substrate")) %>% 
  rename("Habitat Metric" = "habitat_variable") 

tab_hab <- tab_hab %>% 
  kbl(format = "html",
      booktabs = TRUE,
      escape = FALSE,
      digits = 2) %>% 
  kable_styling() %>% 
  save_kable("table_habitat.pdf")


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
                           " -- ",
                           sprintf("%.2f", `97.5%`),
                           "]")) %>% 
  select(Species = species,
         Season = season,
         Estimate = estimate) %>% 
  mutate(Species = ifelse(duplicated(Species), NA, Species))

## export
print(xtable(df_p,
             caption = "Seasonal detection probabilities estimated using the spatial Cormack-Jolly-Seber (CJS) model, presented as median estimates with corresponding 95\\% credible intervals in brackets.",
             label = "tab:detection"),
      tabular.environment = "tabular", # use \begin{tabular}
      sanitize.text.function = function(x) x, # for math mode
      include.rownames = FALSE,
      caption.placement = "top",
      file = "tex/table_detection.tex")










         