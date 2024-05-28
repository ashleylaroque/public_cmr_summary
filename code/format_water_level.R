# Adapted from Seog Kim's code 
# Format water water level data 

# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))

# # Load data
# ## data from occasion 1-10
# drive_download("05_12_23_AirTemp",
#                type = "csv",
#                path = "data_raw/data_air_temp1.csv",
#                overwrite = T)
#
# drive_download("05_12_23_WaterTemp",
#                type = "csv",
#                path = "data_raw/data_water_temp1.csv",
#                overwrite = T)
#
# ## data from occasion 11+
# drive_download("11_12_23_AirTemp",
#                type = "csv",
#                path = "data_raw/data_air_temp2.csv",
#                overwrite = T)
#
# drive_download("11_12_23_WaterTemp",
#                type = "csv",
#                path = "data_raw/data_water_temp2.csv",
#                overwrite = T)

# data --------------------------------------------------------------------

# Read data
df_air <- list.files("data_raw",
                     pattern = "data_air",
                     full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  rename(air_pres = abs_pres,
         air_temp = temp)

df_water <- list.files("data_raw",
                       pattern = "data_water",
                       full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  rename(water_pres = abs_pres, 
         water_temp = temp)

# Calculate water level
# convert air and water pressure (mmHG) to water level (m) 
daily_dat <- df_air %>% 
  left_join(df_water, by = c("date", "time")) %>%
  mutate(water_level = (13.595 * (water_pres - air_pres)) / 1000,
         date = as.Date(date, format = "%m/%d/%y"))

## Water temperature
ggplot(daily_dat, aes(x = date, y = water_temp)) + 
  geom_line(size = 0.8, color = "red") +
  scale_x_date(date_breaks = "10 day", labels = date_format("%m-%d")) +
  scale_y_continuous("Water temperature") +
  ggtitle("Hourly temperature (c)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_rect(fill = "white"), 
        legend.position=c(0.10,0.80))  # remove grid

## Water level
ggplot(daily_dat, aes(x=date, y=water_level)) + 
  geom_line(size=0.8, color= "blue") +
  scale_x_date(date_breaks = "10 day", labels = date_format("%m-%d")) +
  scale_y_continuous("Water temperature") +
  ggtitle("Hourly water level (m)")+ 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_rect(fill = "white"), 
        legend.position=c(0.10,0.80))

# Calculate daily mean temperature and water level
water_temp <- daily_dat %>% 
  group_by(date) %>% 
  summarize(n = n(),
            mean = mean(water_temp, na.rm=TRUE))

depth <- daily_dat %>% 
  group_by(date) %>% 
  summarize(n = n(),
            mean = mean(water_level, na.rm=TRUE))

# Combine water temperature and level
water_dat <- water_temp %>%
  left_join(depth, by ="date") %>%
  select(date, temperature = mean.x, water_level = mean.y)


# Daily mean plot

## Water temperature
ggplot(water_dat, aes(x=date, y=temperature)) + 
  geom_line(size=0.8, color= "red") +
  scale_x_date(date_breaks = "10 day", labels = date_format("%m-%d"))+
  scale_y_continuous("Water temperature") +
  ggtitle("Mean daily temperature (c)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_rect(fill = "white"), 
        legend.position=c(0.10,0.80))
## Water level
ggplot(water_dat, aes(x = date, y = water_level)) + 
  geom_line(size=0.8, color= "blue") +
  scale_x_date(date_breaks = "10 day", labels = date_format("%m-%d")) +
  scale_y_continuous("Water temperature") +
  ggtitle("Mean daily water level (m)")+ 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_rect(fill = "white"), 
        legend.position=c(0.10,0.80))

# Add occasion by sampling date
# needs to be reformatted below
## occasion was added based on the interval between fish sampling date
v_date <- read_csv("data_raw/data_cmr_src.csv") %>% 
  mutate(date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  group_by(Occasion) %>% 
  slice(which.min(date)) %>% 
  pull(date)

## option one
date_obs <- water_dat$date

occasion <- rep(NA, length(date_obs))
for(i in 1:(length(v_date) - 1)) {
  tf <- between(date_obs, v_date[i], v_date[i + 1] - 1)
  occasion[tf] <- i
}

water_dat <- water_dat %>% 
  mutate(occasion = occasion)

# ## option two
# water_dat %>% 
#   mutate(occasion = case_when(between(date, v_date[1], v_date[2]) ~ 1,
#                               between(date, v_date[2], v_date[3]) ~ 2,
#                               between(date, v_date[3], v_date[4]) ~ 3,
#                               ...)
#          )


# Calculate temperature and water level by occasion
water_dat_occ <- water_dat %>%
  group_by(occasion) %>%
  summarize(mean_temp = mean(temperature, na.rm=TRUE),
            max_temp = max(temperature, na.rm=TRUE),
            mean_level = mean(water_level, na.rm=TRUE),
            max_level = max(water_level, na.rm=TRUE)) %>% 
  group_by(occasion) %>%
  slice(1) %>% # remove duplicate and keep the first row for each occasion
  select(occasion, mean_temp, max_temp, mean_level, max_level) %>%
  mutate(occasion = as.numeric(occasion))

# Export file
saveRDS(water_dat_occ, file = "data_formatted/data_water_level.rds")


