# SFS Figures ----------------------------------------------------------
# factor species column by the fish_order 
df_m2 <- arrange(transform(df_combined,
                           species=factor(species,levels=fish_order)), species)

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