# Adapted from Seog Kim's code 
# Format water water level data 


source(here::here("code/library.R"))

# Load data
## data from occasion 1-10
drive_download("05_12_23_AirTemp",
               type = "csv",
               path = "data_raw/data_air_temp1.csv",
               overwrite = T)

drive_download("05_12_23_WaterTemp",
               type = "csv",
               path = "data_raw/data_water_temp1.csv",
               overwrite = T)

## data from occasion 11+
drive_download("11_12_23_AirTemp",
               type = "csv",
               path = "data_raw/data_air_temp2.csv",
               overwrite = T)

drive_download("11_12_23_WaterTemp",
               type = "csv",
               path = "data_raw/data_water_temp2.csv",
               overwrite = T)

# Read data
df_air1 <- read_csv("data_raw/data_air_temp1.csv") %>% 
  rename(air_pres = abs_pres, 
         air_temp = temp)

df_water1 <- read_csv(here::here("data_raw/data_water_temp1.csv")) %>% 
  rename(water_pres = abs_pres, 
         water_temp = temp) 

df_air2 <- read_csv("data_raw/data_air_temp2.csv") %>% 
  rename(air_pres = abs_pres, 
         air_temp = temp)

df_water2 <- read_csv(here::here("data_raw/data_water_temp2.csv")) %>% 
  rename(water_pres = abs_pres, 
         water_temp = temp) 

# Join all occasions

df_air <- df_air1 %>% 
  full_join(df_air2)

df_water <- df_water1 %>% 
  full_join(df_water2)


# Calculate water level
daily_dat <- df_air %>% 
  left_join(df_water, by = c("date", "time")) %>%
  mutate(water_level = (13.595*(water_pres - air_pres))/1000) # convert air and water pressure (mmHG) to water level (m) 

daily_dat$date <- as.Date(as.POSIXct(daily_dat$date, format='%m/%d/%y'))

# Hourly-scale plot

## Water temperature
ggplot(daily_dat, aes(x=date, y=water_temp)) + 
  geom_line(size=0.8, color= "red") +
  scale_x_date(date_breaks = "10 day", labels = date_format("%m-%d"))+
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
  summarize(n=n(),mean=mean(water_temp,na.rm=TRUE))%>% 
  as.data.frame()

depth <- daily_dat %>% 
  group_by(date) %>% 
  summarize(n=n(),mean=mean(water_level,na.rm=TRUE)) %>% 
  as.data.frame()

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
ggplot(water_dat, aes(x=date, y=water_level)) + 
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
water_dat$occasion <- ifelse(water_dat$date <= as.Date("2020-11-08"), "1",
                      ifelse(water_dat$date <= as.Date("2021-02-21"), "2",
                      ifelse(water_dat$date <= as.Date("2021-05-15"), "3",
                      ifelse(water_dat$date <= as.Date("2021-08-13"), "4",
                      ifelse(water_dat$date <= as.Date("2021-11-13"), "5",
                      ifelse(water_dat$date <= as.Date("2022-05-03"), "6",
                      ifelse(water_dat$date <= as.Date("2022-08-06"), "7",
                      ifelse(water_dat$date <= as.Date("2022-12-07"), "8",
                      ifelse(water_dat$date <= as.Date("2023-02-16"), "9",
                      ifelse(water_dat$date <= as.Date("2023-05-11"), "10",
                      ifelse(water_dat$date <= as.Date("2023-09-08"), "11",
                      ifelse(water_dat$date <= as.Date("2023-11-11"), "12",
                      ifelse(water_dat$date <= as.Date("2024-02-10"), "13","14")
                      ))))))))))))

# Calculate temperature and water level by occasion
water_dat_occ <- water_dat %>%
  group_by(occasion) %>%
  mutate(mean_temp = mean(temperature, na.rm=TRUE),
         max_temp = max(temperature, na.rm=TRUE),
         mean_level = mean(water_level, na.rm=TRUE),
         max_level = max(water_level, na.rm=TRUE)) %>% 
  group_by(occasion) %>%
  slice(1) %>% # remove duplicate and keep the first row for each occasion
  select(occasion, mean_temp, max_temp, mean_level, max_level) %>%
  mutate(occasion = as.numeric(occasion))

# Export file
saveRDS(water_dat_occ, file = "data_formatted/data_water_level.rds")


