## call every single time!
## ctrl+shft+r for line
## ctrl+shft+M for %>% 
## ,, ()+ important

library(tidyverse)

iris %>% 
  ggplot(
    mapping = aes(x= Sepal.Length,
                  y=Sepal.Width)
  )+
geom_point()



ggplot(
  data = iris,
  mapping = aes(x = Sepal.Length,
                y = Sepal.Width)
) +
  geom_point(color = "steelblue")


# line figure -------------------------------------------------------------
df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)

df0 %>% 
  ggplot(
    mapping = aes(x= x,
                  y=y)
  ) +
  geom_line()


# histogram ---------------------------------------------------------------
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram()

iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5)
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(bins = 50)


iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length)) +
  geom_boxplot()


iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot()


iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot(color = "darkgrey")


