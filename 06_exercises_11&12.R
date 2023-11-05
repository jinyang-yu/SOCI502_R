#11.13
library(dslabs)
library(dplyr)
library(ggplot2)
data(us_contagious_diseases)
#1:d
#2:b
#3:c
#4
dat = us_contagious_diseases |>  
  filter(year == 1967 & disease=="Measles" & !is.na(population)) |>
  mutate(rate = count / population * 10000 * 52 / weeks_reporting)
dat |> ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip() 
state = dat$state
rate = dat$count/dat$population*10000*52/dat$weeks_reporting
state = reorder(state, rate)
print(state)
#5
dat|>mutate(state= reorder(state, rate))|> ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip() 
#6:c
#7
data("murders")
murders |> mutate(rate = total/population*100000)|>mutate(region=reorder(region, rate, FUN=median))|>ggplot(aes(region, rate)) + geom_point() 
#8: a

#11.15
#1
library(tidyverse)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)
the_disease = "Smallpox"
dat = us_contagious_diseases |>
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting>=10) |>
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) |> 
  mutate(state = reorder(state, rate, median, na.rm = TRUE))
dat |> ggplot(aes(year, state, fill = rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
  theme_minimal() +  
  theme(panel.grid = element_blank(), 
        legend.position="bottom", 
        text = element_text(size = 8)) +
  labs(title = the_disease, x = "", y = "")
#2
avg = us_contagious_diseases |>
  filter(disease==the_disease& weeks_reporting>=10) |> group_by(year) |>
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)
dat |> 
  filter(!is.na(rate)) |>
  ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1) +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + ylab("") +
  geom_text(data = data.frame(x = 1950, y = 25), 
            mapping = aes(x, y, label="US average"), 
            color="black") 
#3
us_contagious_diseases |> filter(state=="California" & !weeks_reporting<10) |> 
  group_by(year, disease) |>
  summarize(rate = sum(count)/sum(population)*10000* 52 / weeks_reporting) |>
  ggplot(aes(year, rate,color=disease)) + 
  geom_line()
#4
us_contagious_diseases |> filter(!is.na(population)&!weeks_reporting<10) |> 
  group_by(year, disease) |>
  summarize(rate = sum(count)/sum(population)*10000* 52 / weeks_reporting) |>
  ggplot(aes(year, rate,color=disease)) + 
  geom_line()

#12.3
#1: 25%
#2: b
#3: b.95%
#4: c.69
#5: a.1
#6: d
#7: c.34
#8: a.1%
#9. b.0.15
#10: d

#12.8
#Questions 1-8 are the same as what we did last week
#1: male: 812; female: 238
#2
female_percentiles=quantile(female,c(0.1,0.3,0.5,0.7, 0.9),na.rm=TRUE)
male_percentiles=quantile(female,c(0.1,0.3,0.5,0.7, 0.9),na.rm=TRUE)
df=data.frame(female=female_percentiles, male=male_percentiles)
#3: Asia
#4: Africa
#5: 10 million
#6:b
#7:America
#8
library(dslabs)
data(heights)
x <- heights$height[heights$sex=="Male"]
mean(x<=72) - mean(x<=69)
#answer: 0.33
#9
m=mean(x)
s=sd(x)
print(c(m,s))
pnorm(72, mean = m, sd = s)-pnorm(69, mean = m, sd = s)
#answer: 0.31
#10
y=mean(x<=81) - mean(x<=79)
#0.005
aprox=pnorm(81, mean = m, sd = s)-pnorm(79, mean = m, sd = s)
#0.003
y/aprox
#answer: 1.61 times
#11
1-pnorm(84, mean = 69, sd = 3)
#answer: 2.866516e-07
#12
1000000000*(1-pnorm(84, mean = 69, sd = 3))
#answer: 287
#13
10/287
#answer: 0.03
#14
1000000000*(1-pnorm(81.6, mean = 69, sd = 3))
150/13346
#answer: 0.01
#15: c

#12.10
library(HistData)
data(Galton)
head(Galton)
x <- Galton$child
#1
m=mean(x)
s=sd(x)
me=quantile(x,0.5)
print(c(m,s,me))
#mean=69.3, standard deviation= 3.61, median=69.0
#2
ma=mad(x)
#median absolute deviation=2.97
#3
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
mr=mean(x_with_error)
sr=sd(x_with_error)
mer=quantile(x_with_error,0.5)
mar=mad(x_with_error)
print(c(mr,sr,mer,mar))
mr-m
#answer: 0.83
#4
sr-s
#answer: 20.55
#5
mer-me
#answer: 0
#6
mar-ma
#answer: 0
#7:c
#8
error_avg=function(k) {x[1]=k
  return(mean(x))
}
error_avg(10000)
error_avg(-10000)
