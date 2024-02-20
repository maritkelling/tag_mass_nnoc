## load libraries
library(glmmTMB)
library(tidyverse)
library(afpt)
library(readxl)
library(here)

path_to_raw <- here("data_raw")
path_to_output <- here("output")

## compute mean morphometrics from Prieros population 

wings <- read.csv("data_raw/Prieros_15-09-23.csv", sep = ";") %>% 
         mutate(wingArea = wing_area_cm2/10000, 
         wingSpan = tip_to_tip_wing_span_cm/100, 
         wingAspect = wingSpan^2/wingArea)

wings_mean <- wings %>% 
  summarise(mean_wingSpan = mean(wingSpan),
            sd_wingspan = sd(wingSpan),
            mean_wingAspect = mean(wingAspect),
            sd_AR = sd(wingAspect),
            mean_wingArea = mean(wingArea), 
            sd_wingArea = sd(wingArea),
            mean_BM = mean(mass_g)/1000, 
            sd_BM = sd(mass_g)/1000)

## generate an average common noctule dataframe for afpt

Noctule <- Bird(name.scientific = "Nyctalus noctula",
                massTotal = wings_mean$mean_BM, 
                wingSpan = wings_mean$mean_wingSpan,
                wingArea = wings_mean$mean_wingArea,
                wingAspect = wings_mean$mean_wingAspect,
                coef.bodyDragCoefficient = 0.4,
                type = 'bat', 
                wingbeatFrequency = 0.0698 * (wings_mean$mean_BM*1000) + 5.554,
                muscleFraction = 0.09,
                source = "unpublished")

## compute power (W) for average noctule at flight speed of 8 ms
Power_8ms <- computeFlappingPower(Noctule, speed = c(8))

## compute conversion efficiency from muscle (nfm)
## conversion efficiency (n) given BM of common noctules and model from Currie et al 2023

n = 0.07367838
R = 1.1
BMR = Noctule$basalMetabolicRate
Pchem  = Power_8ms$power.chem # (W) @ 8ms
Pmech  = Power_8ms$power # (W) @ 8ms

nfm = (n*R*Pmech)/(Pmech-n*R*BMR)

## create bird object for untagged common noctule with correct conversion efficiency
Noctule_untagged <- Bird(name.scientific = "Nyctalus noctula",
                massTotal = wings_mean$mean_BM, 
                wingSpan = wings_mean$mean_wingSpan,
                wingArea = wings_mean$mean_wingArea,
                wingAspect = wings_mean$mean_wingAspect,
                coef.bodyDragCoefficient = 0.4,
                type = 'bat', 
                coef.conversionEfficiency = nfm,
                wingbeatFrequency = 0.0698 * (wings_mean$mean_BM*1000) + 5.554,
                muscleFraction = 0.09,
                source = "unpublished")

## create bird object for common noctule tagged with 3.8g tag and 45% increased in drag coefficient
Noctule_tagged <- Bird(name.scientific = "Nyctalus noctula",
                 massEmpty = wings_mean$mean_BM, 
                 wingSpan = wings_mean$mean_wingSpan,
                 wingArea = wings_mean$mean_wingArea,
                 wingAspect = wings_mean$mean_wingAspect,
                 coef.bodyDragCoefficient = 0.4*1.45,
                 type = 'bat',  
                 wingbeatFrequency = 0.0698 * ((wings_mean$mean_BM+0.0038)*1000) + 5.554,
                 muscleFraction = 0.09,
                 massLoad = 0.0038,
                 coef.conversionEfficiency = nfm,
                 source = "unpublished")

speeds <- seq(1,15, by = 0.1)

# calculate power curve for range of flight speeds for untagged noctule
Noctule_untagged_power <- computeFlappingPower(Noctule_untagged, speed = speeds) %>% mutate(treatment = "untagged")

# calculate power curve for range of flight speeds for tagged noctule
Noctule_tagged_power <- computeFlappingPower(Noctule_tagged, speed = speeds) %>% mutate(treatment = "tagged")

# create dataframe for flight speeds between 6 & 10ms
Power_6_10ms <- Noctule_untagged_power %>% 
  bind_rows(Noctule_tagged_power) %>% 
  filter(speed %in% c(6,7,8,9,10)) %>% 
  select(bird.name, speed, power, power.chem, treatment)

# calculate proportionate predicted increase in flight power
Power_differences <- Power_6_10ms %>% group_by(speed, treatment) %>%
  pivot_wider(names_from = treatment, values_from = c(power,power.chem)) %>% 
  mutate(cost_diff = power.chem_tagged/power.chem_untagged)

mean_increased_costs <- Power_differences %>% ungroup() %>% 
  summarise(mean_cost_diff = mean(cost_diff),
            sd_cost_diff = sd(cost_diff))

(Noctule_speed_plot <- ggplot() +
     geom_path(data = Noctule_untagged_power, aes(x= speed, y= power.chem), linewidth = 1, colour = "#1B9E77", alpha = 0.7) +
    geom_path(data = Noctule_tagged_power, aes(x= speed, y= power.chem), colour = "#D95F02", linewidth = 1, alpha = 0.7) +
    scale_y_continuous(limits = c(0 , 12), expand = c(0,0)) +
    scale_x_continuous(limits = c(0 , 12), expand = c(0,0)) +
    guides(alpha ="none")+
    theme_bw() +
    theme(strip.background = element_blank(), strip.text = element_blank(),
          legend.background= element_blank(), 
          legend.box.background = element_blank(),
          legend.position = "none",
          legend.key = element_rect(fill = "white"),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.spacing = unit(1,"lines"),
          legend.title = element_blank(),
          plot.margin = margin(t = 14, r = 14, b = 14, l = 14, unit = "pt"),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 16),
          axis.line = element_line(color= "black"),
          axis.ticks.length = unit(6, "pt")) +
    labs(y=expression(Metabolic~Power~(W)), x = expression(Flight~Speed~(m/s))))


