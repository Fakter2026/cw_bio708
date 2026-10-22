library(tidyverse)
head(iris)
#perform at once using lapply() or for Loop
list_lm <- lapply(x= v_sp,
                  function(s) {
                    iris %>%
                      filter(Species == s) %>%
                      lm(Sepal.Width ~ Petal.Width, data = .)
                  })

#for loop version
#list is the object type that can store multiple type of data as a single object

# for example, list[[1]] dataframe, list [[2]] vector, list [[3]] model output etc..
# in list accessing each elememt uses syntax 
fit <- list(NULL)
for ( i in 1: length(v_sp)) {
  fit[[i]] <- iris %>%
    filter(Species == v_sp[i]) %>%
    lm(Sepal.Width ~ Petal. Width, data = .)
}















