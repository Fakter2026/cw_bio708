#' linear model

rm(list = ls())
pacman::p_load(tidyverse)
# categorical

df_fl <- read_csv("data_src/data_fish_length.csv")

## t.test vs lm

## lm() approach
m_fl <- lm(length ~ lake,
           data = df_fl)

summary(m_fl)

## calculate mean fish length for lake a & b
## take the difference
v_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)

v_mu[2] - v_mu[1]

## t-test for a comparison between a and b
t.test(length ~ lake, data = df_fl, var.equal = TRUE)

## anova vs lm
df_anova <- read_csv("data_src/data_fish_length_anova.csv")

## lm 
m_anova <- lm(length ~ lake,
              data = df_anova)

summary(m_anova)

## calculate means for each group
## differences between a and b, a and c
v_mu_anova <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)

## a vs b
v_mu_anova[2] - v_mu_anova[1]

## a vs c
v_mu_anova[3] - v_mu_anova[1]

## try anova for comparison
fit_aov <- aov(length ~ lake,
               data = df_anova)

summary(fit_aov)

## how to modify the order of groups
df_anova <- df_anova %>% 
  mutate(lake_factor = factor(lake, levels = c("c", "a", "b")))

unique(df_anova$lake)
unique(df_anova$lake_factor)

## lm() with lake_factor
lm(length ~ lake_factor,
   data = df_anova) %>% 
  summary()


iris <- as_tibble(iris)
distinct(iris)

## develop iris model
m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)
summary(m_iris)

summary(m_iris)
# draw figure

# visualization -----------------------------------------------------------


n_rep <- 100

df_pred <- iris |> 
  group_by(Species) |> 
reframe(
  Petal.Width = seq(min(Petal.Width),
                    max(Petal.Width),
                    length = n_rep)
)
  

y_pred <- predict(m_iris,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

print(df_pred)
#figure

iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred)) 

#manual interaction syntax
# develop iris model with an interaction
#shortcut
m_iris_int <- lm(Petal.Length ~ Petal.Width * Species,
                 data = iris)
summary(m_iris_int)

# prediction with the interaction model
y_pred_int <- predict(m_iris_int,
                      newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred_int = y_pred_int)

# visualization
iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred_int))