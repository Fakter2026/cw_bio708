x<- c(15.9,15.1,21.9,13.3,24.4)
y<- c(15.9,15.1,21.9,53.3, 24.3)
x
y
#arithmetic mean
(mu<- sum (x)/length(x))
mean(x)
(mu<- sum(y)/length(y))
mean(y)
#geometric mean
mu_x_ge<- prod(x)^(1/length(x))
exp(sum(log(x))/length(x))
mu_y_ge<- prod(y)^(1/length(y))
exp(sum(log(y))/length(y))
mu_x
mu_y

#median
med_x<- median(x)
med_y<-median(y)

# central tendency --------------------------------------------------------
#variance
(var_xsum<-((x- mean(x))^2)/length(x))
(var_ysum<-(y-mean(y))^2)/length(y)
#square root
sqrt(var_x)
sqrt(var_y)

##inter-quantile range
x_l<- quantile(x,0.25)
x_h<- quantile(x,0.75)
(iqr_x<- x_h-x_l)
y_l<- quantile(y,0.25)
y_h<- quantile(y,0.75)
(iqr_y<- y_h-y_l)

#MAD
ad_x<- abs(x-median(x))
median(ad_x)
ad_y<- abs(y-median(y))
median(ad_y)

#coefficient of variation
sd_x<- sqrt(var_x)
cv_x<- sd_x/mu_x

# extra -------------------------------------------------------------------

mycv<- function(v){
 mu<- mean(v)
 s<- sd(v)
 cv<- s/mu
 return(cv)
}
mycv(x)

