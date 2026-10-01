rm(list= ls())
pacman::p_load(tidyverse,
               patchwork)

library(tidyverse)
xs <- rnorm(n=10, mean = 10, sd = 5)
ys <- rnorm(n=10, mean = 12, sd = 5)
t.test(xs, ys, var.equal = TRUE)
xl <- rnorm(n=100, mean = 10, sd = 5)
yl <- rnorm(n=100, mean = 12, sd = 5)
t.test(xl, yl, var.equal = TRUE)
a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

#tibble
df_ab <- tibble(a1 = a1,
                a2 = a2, 
                b1 = b1,
                b2 = b2) |> 
  pivot_longer(
    cols = everything(),
    names_to = "groups",
    values_to = "value"
  )






filter(group %in% c("a1", "a2")) %>%
  ggplot(
    aes(
      x = group,
      y = value
    )
  ) +
  geom_jitter(
    height = 0,
    width = 0.1,
    alpha = 0.5
  ) +
  geom_segment(
    data = df_mu %>%
      filter(group %in% c("a1", "a2")),
    aes(
      y = mu - sig,
      yend = mu + sig
    )
  ) +
  geom_point(
    data = df_mu %>%
      filter(group %in% c("a1", "a2")),
    aes(y = mu),
    size = 2.5
  )

t.test(a1, a2)
t.test(b1, b2)




df_fl <- read_csv("data_src/data_fish_length.csv")
print(df_fl)
mu <- mean(df_fl$length)
sig <- sd(df_fl$length)
x <- rnorm (n = 50 , mean = mu, sd = sig)
y<- rnorm (n = 50 , mean = mu, sd = sig)
v <- t.test(x, y, var.equal = TRUE)$statistics 
 
