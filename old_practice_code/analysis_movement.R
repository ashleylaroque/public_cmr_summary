
#load glm package
library(pacman)
pacman:: p_load(tidyverse, 
                lme4, 
                visreg,
                lattice,
                sjPlot, 
                ggplot2,
                sjlabelled,
                sjmisc)


# NMDS --------------------------------------------------------------------


#' #' - NMDS species abundance 
#' # use df_abundance2 from format_habitat_data
#' df_abundance1 <- df_abundance2 %>% 
#'   select(creek_chub, bluehead_chub, green_sunfish, redbreast_sunfish)
#' 
#' # get rid of rows with all zeros in fish abundance columns
#' df_abundance1 <- df_abundance1[-c(30, 62, 182, 192, 224:259),]
#' df_abundance1 <- df_abundance1[-c(179, 188),]
#' #select just fish abundance columns 
#' df_abundance3 <- df_abundance1 %>% 
#'   ungroup() %>% 
#'   select(creek_chub, bluehead_chub, green_sunfish, redbreast_sunfish) 
#' 
#' # Remove rows with all 0s
#' df_abundance3<- df_abundance3[rowSums(df_abundance3[])>0,]
#' 
#' 
#' # create NMDS 
#' veg_abund <- vegdist(df_abundance3, method= "bray")
#'   
#' fish.nmds <- metaMDS(veg_abund, distance = 'bray', 
#'                          k = 3, trymax = 500, maxit= 999, autotransform = FALSE)
#' fish.nmds
#' capture.output(fish.nmds, file= "./output/fish_nmds.txt")
#' 
#' # plot NMDS axis 1 and 2
#' cols = c(sample(colours(), 43))
#' p <- ordiplot(fish.nmds, choices = c(1, 2), display = 'sites')
#' ordihull(p, groups = as.factor(df_abundance1$section), conf = 0.95, col = cols)
#' 
#' # plot NMDS axis 2 and 3
#' p_23 <- ordiplot(fish.nmds, choices = c(2, 3), display = 'sites')
#' ordihull(p_23, groups = as.factor(df_abundance1$section), conf = 0.95, col = cols)
#' 
#' # plot NMDS axis 1 and 3
#' p_13 <- ordiplot(fish.nmds, choices = c(1, 3), display = 'sites')
#' ordihull(p_13, groups = as.factor(df_abundance1$section), conf = 0.95, col = cols)
#' 
#' # test for community difference
#' y.anosim <- anosim(veg_abund, df_abundance1$section) #ANOSIM
#' summary(y.anosim)
#' 
#' y.adonis <- adonis(df_abundance3 ~ section, data = df_abundance1, permutations = 1000, method = 'bray') # PERMANOVA
#' y.adonis
#' 
#' 


# GLMM ---------------------------------------------------------
f_species <- c("green_sunfish",
               "redbreast_sunfish",
               "bluegill",
               "creek_chub",
               "bluehead_chub",
               "striped_jumprock")

# Absolute movement predicted by weight and density 
mm <- lapply(1:length(f_species), function(i) {
  
  lmer(move_abs ~ scale(weight_cap) +
         scale(d_creek_chub) +
         scale(d_bluehead_chub) +
         scale(d_green_sunfish) +
         scale(d_redbreast_sunfish)+
         scale(d_striped_jumprock),
          (1 | tag),
        data = filter(df_m, species == f_species[i]))
  
})

names(mm) <- f_species
mm$bluehead_chub
mm$creek_chub
mm$green_sunfish
mm$redbreast_sunfish


# Log distance pre
mm1 <- lapply(1:length(f_species), function(i) {
  
  lmer(log_dist ~ tr_cyp + tr_cent + tr_other +
         (1 | tag),
       data = filter(new_move, species == f_species[i]))
  
})

names(mm1) <- f_species
mm1$bluehead_chub
mm1$creek_chub
mm1$green_sunfish
mm1$redbreast_sunfish

# Weight and density as predictor
mm2 <- lapply(1:length(f_species), function(i) {
  
  lmer(log_dist ~ log_weight + tr_cyp + tr_cent + tr_other +
         (1 | tag),
       data = filter(new_move, species == f_species[i]))
  
})

names(mm2) <- f_species
mm2$bluehead_chub
mm2$creek_chub
mm2$green_sunfish
mm2$redbreast_sunfish

# Weight and density as predictor with interactions
mm3 <- lapply(1:length(f_species), function(i) {
  
  lmer(log_dist ~ log_weight + tr_cyp * tr_cent * tr_other +
         (1 | tag),
       data = filter(new_move, species == f_species[i]))
  
})

names(mm3) <- f_species
summary(mm3$creek_chub)
mm3$creek_chub
mm3$green_sunfish
mm3$redbreast_sunfish

plot(mm3$green_sunfish)
m0.ranef <- ranef(mm3$redbreast_sunfish)
dotplot(m0.ranef)

# Test AIC for lowest score between models 
AIC(mm$green_sunfish)
AIC(mm$redbreast_sunfish)
AIC(mm$creek_chub)
AIC(mm$bluehead_chub)

AIC(mm1$green_sunfish)
AIC(mm1$redbreast_sunfish)
AIC(mm1$creek_chub)
AIC(mm1$bluehead_chub)

AIC(mm2$green_sunfish)
AIC(mm2$redbreast_sunfish)
AIC(mm2$creek_chub)
AIC(mm2$bluehead_chub)

AIC(mm3$green_sunfish)
AIC(mm3$redbreast_sunfish)
AIC(mm3$creek_chub)
AIC(mm3$bluehead_chub)
#mm3 best AIC score 



# GLM ---------------------------------------------------------------------

# GLM both weight and density
list_m <- lapply(1:length(f_species), function(i) {

  glm(emigration ~
        scale(weight_cap) +
        scale(d_creek_chub) +
        scale(d_bluehead_chub) +
        scale(d_green_sunfish) +
        scale(d_redbreast_sunfish)+
        scale(d_striped_jumprock),
      family = "binomial",
      data = filter(df_m, species == f_species[i]))

})

names(list_m) <- f_species
lapply(list_m, summary)


# GLM just density 
list_m <- lapply(1:length(f_species), function(i) {
  
  glm(emigration ~
        scale(d_creek_chub) +
        scale(d_bluehead_chub) +
        scale(d_green_sunfish) +
        scale(d_redbreast_sunfish)+
        scale(d_striped_jumprock),
      family = "binomial",
      data = filter(df_m, species == f_species[i]))
  
})

names(list_m) <- f_species
lapply(list_m, summary)

# GLM just weight 
list_m <- lapply(1:length(f_species), function(i) {
  
  glm(emigration ~
        scale(weight_cap),
      family = "binomial",
      data = filter(df_m, species == f_species[i]))
  
})

names(list_m) <- f_species
lapply(list_m, summary)
