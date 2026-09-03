library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)


#filter
filter(iris_sub, Species == "virginica")
filter(iris_sub,Species !="virginica")
filter(iris_sub,Species%in% c("virginica","versicolor"))
filter(iris_sub,Sepal.Width>5,
       Species=="versicolor")
arrange(iris_sub, desc(Sepal.Length))

#select
select(iris_sub, starts_with("Sepal"))
select(iris_sub, ends_with("Width"))
#Mutate
x_max<- nrow(iris_sub)
x<- 1:x_max

iris_sub_row<- mutate(iris_sub, row_id=x)
arrange(iris_sub_row, desc(row_id))
mutate(iris_sub, sl_two_times = 2* Sepal.Length)

mutate(iris_sub, id_name = paste( Species, Sepal.Length))
#pipe %>% 
select(filter(iris_sub, Species == "virginica"), Sepal.Length)

iris_sub %>% 
  filter(Species=="virginica") %>% 
  select(Sepal.Length)  
df_vir_sl<- iris_sub %>% 
  filter(Species == "virginica") %>% 
  select(Sepal.Length)
print(df_vir_sl)
#exercise
iris_pipe <- filter(iris_sub, Species == "setosa") %>% 
  mutate( pw_two_times = 2*Petal.Width)
print(iris_pipe)



df_vir_sl<- iris_sub %>% 
  filter(Species == "setosa") %>% 
  select(Sepal.Length)
print(df_vir_sl)

#Grouping
iris_sub %>% 
  mutate(mu_sl = mean(Sepal.Length)) %>% 
  ungroup()
#summarize
iris_sub %>% 
  summarise(mu_sl = mean(Sepal.Length))

