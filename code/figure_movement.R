
# Setup -------------------------------------------------------------------

rm(list=ls())
source(here::here("code/library.R"))

# call "format_cmr_data.R"
source(here::here("code/format_cmr_data.R"))


# figure ------------------------------------------------------------------

## plot movement vs. density
ggplot(df_move, aes(move, d)) +
  geom_point(color= "#20A387FF")+
  labs(x = "Distance Moved (m)", y = "Fish Density") +
  theme_minimal()+
  facet_wrap(~species.x) # each grid shows how each species reacts to other species density

## plot movement vs. body size
ggplot(df_move, aes(move, size_ratio)) + # here this uses length per unit weight
  geom_point(color= "#20A387FF")+
  labs(x = "Distance Moved (m)", y = "Body Size Ratio (mm/g)") +
  theme_minimal()+
  facet_wrap(~species.x)

## plot habitat variables vs. density

ggplot(df_z, aes(value , d )) + 
  geom_point(color= "#20A387FF")+
  labs(x = "Habitat", y = "Fish Density") +
  theme_minimal()+
  facet_wrap(~habitat_variable)

## plot habitat variables vs. movement

ggplot(df_z, aes(move , value)) + 
  geom_point(color= "#20A387FF")+
  labs(x = "Distance Moved (m)", y = "Habitat Variable") +
  theme_minimal()+
  facet_wrap(~habitat_variable)

