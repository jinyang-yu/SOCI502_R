#9.7
#9.7.1
library(dslabs)
data(heights)
male <- heights$height[heights$sex == "Male"]
female <- heights$height[heights$sex == "Female"]
#male: 812; female: 238
#9.7.2
female_percentiles=quantile(female,c(0.1,0.3,0.5,0.7, 0.9),na.rm=TRUE)
male_percentiles=quantile(female,c(0.1,0.3,0.5,0.7, 0.9),na.rm=TRUE)
df=data.frame(female=female_percentiles, male=male_percentiles)
#9.7.3
#Asia
#9.7.4
#Africa
#9.7.5
#10 million
#9.7.6
#b
#9.7.7
#America
#9.7.8
library(dslabs)
data(heights)
x <- heights$height[heights$sex=="Male"]
mean(x<=72) - mean(x<=69)
#0.33

#9.9
#9.9.1
#d
#9.9.2
hi=heights |> ggplot(aes(height)) 
#9.9.3
hi + geom_histogram()
#9.9.4
hi + geom_histogram(binwidth =1)
#9.9.5
heights |> ggplot(aes(height)) + geom_density()
#9.9.6
heights |>ggplot(aes(height, group=sex)) +geom_density()
#9.9.7
heights |>ggplot(aes(height, col=sex)) +geom_density()
#9.9.8
heights |> ggplot(aes(height, fill = sex)) + geom_density(alpha=0.2)