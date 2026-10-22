library(tidyverse)
df_algae <- read_csv("data_src/data_algae.csv")
print(df_algae)
df_algae |> 
  ggplot (aes(x = conductivity,
              y = biomass)) +
  geom_point()
m <- lm(biomass ~ conductivity,
data = df_algae)
summary(m)
coef(m) 

alpha <- coef(m) [1]
beta <- coef(m) [2]
df_algae |> 
  ggplot(aes(x = conductivity,
y = biomass)) +
  geom_point () +
  geom_abline(intercept = alpha,
              slope = beta)

#extract coef
theta <- coef(m)
#extract se
se <- sqrt(diag(vcov(m)))
 #t value
t_value <- theta / se
print(t_value)


# epsolin = eps
#extract residual
eps0 <- df_algae$biomass - ( alpha + beta* df_algae$conductivity)

eps <- resid(m)

plot(eps0 ~ eps)

#visualize error
df_algae <- df_algae %>% 
  mutate(eps = eps)


df_algae %>% 
  ggplot(
    aes(x = conductivity,
        y = biomass)
  ) +
  geom_point() +
  geom_abline(
    intercept = alpha,
    slope = beta
  ) +
  geom_segment(
    aes(
      x = conductivity,
      xend = conductivity,
      y = biomass,
      yend = biomass - eps
    ),
    linetype = "dashed"
  )

y <- df_algae$biomass
ss0 <- sum((y - mean(y))^2)
ss1 <- sum(eps^2)  
rsq <- 1 - ss1 / ss0  

summary(m)
