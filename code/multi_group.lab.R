pacman::p_load(tidyverse)
rm(list = ls())
df_pg <- as_tibble(PlantGrowth)
df_pg |> 
  mutate(
    group_label = case_when(
      group =="ctrl" ~ "control",
      group == "trt1" ~ "Treatrment 1",
      group == "trt2" ~ "Treatment 2"
    )
  ) |> 
  ggplot(
    aes(x = group_label,
        y = weight)
  ) +
  geom_violin (
    draw_quantiles = 0.5 ,
    alpha = 0.2
  )+
  geom_jitter (
    width = 0.2,
    alpha = 0.2
  )+
  labs (x = "Treatment group",
        y = "Weight"
  )+
  theme_classic()

# conduct ANOVA

fit <- aov(weight ~ group, data = df_pg)
summary(fit)
view(fit)  
#values 
#DF of group = 2, DF of Residuals = 27, F value = 4.846, p value = 0.0159


#power analysis
pwr::pwr.anova.test(
  k = 3,
  f = 0.5,
  sig.level = 0.05,
  power = 0.8
)




pwr::pwr.anova.test(
  k = 3,
  n = 5,
  f = 0.5,
  sig.level = 0.05
)
pwr::pwr.anova.test(
  k = 10,
  n = 3,
  f = 0.5 ,
  sig.level = 0.05
)






