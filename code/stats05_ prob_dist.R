#probability distribution

rm(list= ls())
pacman::p_load(tidyverse,
               patchwork)

# continuous --------------------------------------------------------------


#load csv data on R
# load csv data on R
df_h0 <- read_csv("data_src/data_plant_height.csv")
df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width
                 center = 0.5) + # bin's center specification
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean


#draw probability distribution
x <- seq(min(df_h0$height),
max(df_h0$height),
length = 100)
mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)


tibble(y = pd, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line() + # draw lines
  labs(y = "Probability density",
       x = "plant height") # re-label


##pnorm = convert probability density to frequency
# probability of x < 10
p10 <- pnorm(q = 10, mean = mu, sd = sigma)
print(p10)
# probability of x < 20
p20 <- pnorm(q = 20, mean = mu, sd = sigma)
print(p20)
# probability of 10 < x < 20
p20_10 <- p20 - p10
print(p20_10)
x_min <- floor(min(df_h0$height)) # floor takes the integer part of the value
x_max <- ceiling(max(df_h0$height)) # ceiling takes the next closest integer
bin <- seq(x_min, x_max, by = 1) # each bin has 1cm



p <- NULL # empty object for probability
for (i in 1:(length(bin) - 1)) {
  ## p_up < probability upto bin[i+1]
  #p_low <-probability upto bin[i]
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - pnorm(bin[i], mean = mu, sd = sigma)
}


df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))

#combine data
df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width; must match the bin width used for probability
                 center = 0.5) + # bin's center position
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "salmon") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "salmon")


# discrete ----------------------------------------------------------------

df_count <- read_csv("data_src/data_garden_count.csv")
print(df_count)
#histogram
df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # define binwidth
                 center = 0) # relative position of each bin

#poison fit
x <- seq(0, 10, by = 1)
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)
#figure
tibble(y = pm, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") + # draw dashed lines
  geom_point() + # draw points
  labs(y = "Probability",
       x = "Count") # re-label


df_prob <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_count)) # prob x sample size

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # must be divisible number of one; e.g., 0.1, 0.25, 0.5...
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed",
            color = "steelblue"
            ) +
  geom_point(data = df_prob,
             aes(x = x,
                 y = freq),
             color = "steelblue",
             )