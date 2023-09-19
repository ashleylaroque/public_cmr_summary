
# Setup -------------------------------------------------------------------

rm(list=ls())
source(here::here("code/library.R"))

# call "format_cmr_data.R"
source(here::here("code/format_cmr_data.R"))


# figure ------------------------------------------------------------------

## plot movement vs. density
ggplot(df_move[df_move$d_species %in% c('bluegill','green_sunfish','redbreast_sunfish'), ], aes(d, move)) +
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Fish Density") +
  theme_minimal()+
  facet_grid(d_species ~ species.x, scales = "free") + # each grid shows how each species reacts to other species density
  #geom_smooth(method=lm, se=FALSE, color = "darkgrey")

ggplot(df_move[df_move$d_species %in% c('creek_chub','bluehead_chub','striped_jumprock'), ], aes(d, move)) +
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Fish Density") +
  theme_minimal()+
  facet_grid(d_species ~ species.x, scales = "free")+
  #geom_smooth(method=lm, se=FALSE, color = "darkgrey")
#### for the two above figures, is this actually pulling the correct information or is it getting mismatched?

## plot movement vs. body size
ggplot(df_move[df_move$d_species %in% c('bluegill','green_sunfish','redbreast_sunfish'), ], aes(size_ratio, move)) + # here this uses length per unit weight
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Body Size Ratio (mm/g)") +
  theme_minimal()+
  facet_grid(d_species ~ species.x , scales = "free") +
  #geom_smooth(method=lm, se=FALSE, color = "darkgrey")

ggplot(df_move[df_move$d_species %in% c('creek_chub','bluehead_chub','striped_jumprock'), ], aes(size_ratio, move)) + # here this uses length per unit weight
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Body Size Ratio (mm/g)") +
  theme_minimal()+
  facet_grid(d_species ~ species.x , scales = "free")+
  #geom_smooth(method=lm, se=FALSE, color = "darkgrey")

## plot habitat variables vs. density

ggplot(df_z, aes(value , d )) + 
  geom_point(color= "#20A387FF")+
  labs(x = "Habitat", y = "Fish Density") +
  theme_minimal()+
  facet_wrap(~habitat_variable, scales = "free")

## plot habitat variables vs. movement

ggplot(df_z[df_z$habitat_variable %in% c('velocity_mean', 'area_ucb', 'area_pool', 'area_riffle', 'area_run', 'substrate_mean'), ], aes(value , move)) + 
  geom_point(color= "#20A387FF")+
  labs(y = "Distance Moved (m)", x = "Habitat Variable") +
  theme_minimal()+
  facet_wrap(~habitat_variable, scales = "free")

