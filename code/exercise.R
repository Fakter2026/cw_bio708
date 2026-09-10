 #ctrl+shift+c for # in all line
# Create a new vector z with length 100
# as exp(rnorm(n = 100, mean = 0, sd = 0.1)), and calculate the arithmetic mean, geometric mean, and median of z.

library(tidyverse)

z <- exp(rnorm(100, mean = 0, sd = 0.1))
z 
#arithmetic mean
(mu_z<- (sum (z)/length(z)))
mean(z)

#geometric mean

(mu_z_ge<- prod(z)^(1/length(z)))
#median
(med_z <- median(z))

 
# Draw a histogram of z using functions tibble(), ggplot(), and geom_histogram().

df_z <- tibble(z=z)

(g_hist <- df_z %>% 
  ggplot(aes(x=z)) +
  geom_histogram())
 


# Draw vertical lines of arithmetic mean, geometric mean, and median on the histogram with different colors using a function geom_vline() .
 g_hist + 
   geom_vline(xintercept = mu_z,
              color = "pink")+
   geom_vline(xintercept = mu_z_ge,
              color = "red")+
   geom_vline(xintercept = med_z,
              color = "blue")
   
# Visually compare the values of the central tendency measures with the vertical lines drawn by geom_vline().
# 
# Create a new vector z_rev as -z + max(z) + 0.5, and repeat step 1 – 4.
 z_rev <- -z + max(z) + 0.5
z_rev 
(mu_z_rev <- mean(z_rev))
(mu_z_rev_ge <- prod(z_rev)^(1/length(z_rev)))
(med_z_rev <- median(z_rev))


df_zr <- tibble(z_rev=z_rev)

(g_hist <- df_zr %>% 
    ggplot(aes(x=z_rev)) +
    geom_histogram())

g_hist + 
  geom_vline(xintercept = mu_z_rev,
             color = "yellow")+
  geom_vline(xintercept = mu_z_rev_ge,
             color = "maroon")+
  geom_vline(xintercept = med_z_rev,
             color = "skyblue")



w <- rnorm(100, mean = 10, sd = 1)
 head(w) # show first 10 elements in w

# Using this data, perform the following exercise:
#   
#   Convert the unit of w to “milligram” and create a new vector m.
m <- 1000*w

# Calculate SD and MAD for w and m.
(sd_w <- sqrt(sum((w-mean(w))^2)/length(w)))
(mad_w <- median(abs(w-median(w))))

(sd_m <- sqrt(sum((m-mean(m))^2)/length(m)))
(mad_m<- median(abs(m-median(m))))
# Calculate CV and MAD/Median for w and m.
#should be same
#CV
sd_w/mean(w)
sd_m/mean(m)

#median
(mad_w<- median(w))
(mad_m<- median(m))

