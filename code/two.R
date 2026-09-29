rm(list= ls())
pacman::p_load(tidyverse,
               patchwork)
library(tidyverse) # call add-in packages everytime you open new R session
df_fl <- read_csv("data_src/data_fish_length.csv")
print(df_fl)
unique(df_fl$lake)
distinct(df_fl, lake)


df_fl_mu <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            sd_l = sd(length)) # summarize with sd()

df_fl %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_jitter(width = 0.1, # scatter width
              height = 0, # scatter height (no scatter with zero)
              alpha = 0.25) + # transparency of data points
  geom_segment(data = df_fl_mu, # switch data frame
               aes(x = lake,
                   xend = lake,
                   y = mu_l - sd_l,
                   yend = mu_l + sd_l)) +
  geom_point(data = df_fl_mu, # switch data frame
             aes(x = lake,
                 y = mu_l),
             size = 2) +
  labs(x = "Lake", # x label
       y = "Fish body length") # y label


x <- df_fl %>%
  filter(lake == "a") %>%  # subset lake a
  pull(length)

y <- df_fl %>%
  filter(lake == "b") %>% # subset lake b
  pull(length)

t.test(x, y, var.equal = TRUE)

# take another look at df_fl_mu
print(df_fl_mu)


v_mu <- df_fl_mu %>% 
  pull(mu_l)

# lake a
print(v_mu[1])
print(v_mu[2])

# difference
v_mu[1] - v_mu[2]

# group mean, variance, and sample size
df_t <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            var_l = var(length), # summarize with sd()
            n = n()) # count number of rows per group

print(df_t)


# pull values as a vector
v_mu <- pull(df_t, mu_l)
v_var <- pull(df_t, var_l)
v_n <- pull(df_t, n)

var_p <- ((v_n[1] - 1)/(sum(v_n) - 2)) * v_var[1] +
  ((v_n[2] - 1)/(sum(v_n) - 2)) * v_var[2]

t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p * ((1 / v_n[1]) + (1 / v_n[2])))

print(t_value)


#get p value
# produce 500 values from -5 to 5 with equal interval
x <- seq(-5, 5, length = 500)

# probability density of t-statistics with df = sum(v_n) - 2
y <- dt(x, df = sum(v_n) - 2)
y1 <- dt(x, df = 10 - 2)

# draw figure
tibble(x,y1 ) %>% 
  ggplot(aes(x =x,
             y = y))+
             
  geom_line() +
  geom_line(aes (y= y1),
             color = "red") + # t_value is the observed t_value
  geom_vline(xintercept = abs(t_value))+
             geom_vline(xintercept = t_value)
             
  labs(y = "Probability density",
       x = "t-statistic") 
# draw entire range
tibble(x, y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_vline(xintercept = t_value,
             color = "salmon") + # t_value is the observed t_value
  geom_vline(xintercept = abs(t_value),
             color = "salmon") + # t_value is the observed t_value
  labs(y = "Probability density",
       x = "t-statistic") 
