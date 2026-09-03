# practice
a <- 2
b<- 4L
d<- "aquatic"
a
d
va <- c(1.0,2.3,3)
vb <- c("a","b","c")
vc <- c("1","2","3")
va
vb
vc

va <- c(1.0,2.3,3)
vb <- c(3,2,5.6)
ma<- cbind(va,vb)
ma
mb<- rbind(va,vb)
mb
mc<- matrix(c(1,2,3,4), nrow = 3,ncol = 3)
mc
md<-matrix(c(4,5,6,7),nrow=2,ncol=1)
md


library(tidyverse)
 
set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()
print(iris_sub)
set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()


filter(iris_sub, Species== "virginica")
filter(iris_sub, Species!= "virginica")
filter(iris_sub,Species%in% c("setosa","versicolor"))
filter(iris_sub,Species%in% c("virginica","versicolor"))  
filter(iris_sub, Sepal.Width > 5,
       Species=="versicolor")
arrange(iris_sub, desc(Sepal.Length))
select (iris_sub, starts_with ("Sepal"))
select(iris_sub, starts_with("Sepal"))
select(iris_sub, ends_with("Width"))

# column add --------------------------------------------------------------


x_max<- nrow(iris_sub)
x<- 1:x_max
iris_sub_row<-mutate(iris_sub, row_id=x)
arrange(iris_sub_row, desc(row_id))
mutate(iris_sub, sl_two_times = 2 * Sepal.Length)

# from AI -----------------------------------------------------------------

iris_sub %>% 
  mutate(row_id = row_number()) %>% 
  arrange(desc(row_id))


# restart -----------------------------------------------------------------

mutate(iris_sub, sl_two_times = 2* Sepal.Length)
mutate(iris_sub,id_name= paste(Species, Sepal.Length))
mutate(iris_sub, id_name = paste( Species, Sepal.Length))

# id_name -----------------------------------------------------------------
mutate(iris_sub, id_name = paste( Species, Sepal.Length)) %>% 
ungroup()
mutate(iris_sub, id_name = paste( Species, Sepal.Length))
select(filter(iris_sub, Species == "virginica"), Sepal.Length)
iris_sub %>% 
  filter(Species== "virginica") %>% 
select(Sepal.Length)
df_vir_sl<-iris_sub %>% 
  filter(Species=="setosa") %>% 
select(Sepal.Length)
print(df_vir_sl)
iris_sub %>% 
  mutate(mu_sl= mean(Sepal.Length)) %>% 
  ungroup()
iris_sub %>% 
  mutate(mu_sl = mean(Sepal.Length)) %>% 
  ungroup()

# column add --------------------------------------------------------------

iris_sub %>% 
  mutate(mu_sl = mean(Sepal.Length)) %>% 
  ungroup()
print(iris_sub, width = Inf)

iris_sub %>% 
  summarise(mu_sl = mean(Sepal.Length)) %>% 
ungroup()

# ggplot ------------------------------------------------------------------

library(ggplot2)
library(tidyverse)
iris %>% 
  ggplot(mapping=aes(x=Sepal.Length,
                     y=Sepal.Width)
         )+geom_point(color= "red")
df0<- tibble(x= rep(1:50),3,y=x*2)
df0 %>% 
ggplot(mapping=aes(x=x,y=y))+geom_line()
# histogram ---------------------------------------------------------------
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram()
iris %>% 
  ggplot(aes(x=Sepal.Length))+geom_histogram(binwidth =0.5)
## only x axis needed for histo gram
iris %>% 
  ggplot(aes(x=Species, y= Sepal.Length, fill=Species))+geom_boxplot(color="red")
##x=Species for box plot
