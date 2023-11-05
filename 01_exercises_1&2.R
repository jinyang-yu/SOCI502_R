##2.3
#2.3.1
n=100
n*(n+1)/2
#answer:5050
#2.3.2
n=1000
n*(n+1)/2
#answer:500500
#2.3.3
#answer:b
#2.3.4
log(sqrt(100), 10)
#2.3.5
#answer:c

##2.5
#2.5.1
#answer:c
#2.5.2
library(dslabs)
data(murders)
str(murders)
#answer:state, abb, region, population, total
#2.5.3
a=murders$abb
class(a)
#answer:character
#2.5.4
b=murders[["abb"]]
identical(a,b)
#answer:TRUE
#2.5.5
length(levels(murders$region))
#answer:4
#2.5.6
table(murders$region)

##2.8
#2.8.1
temp=c(35,88,42,84,81,30)
temp2= c(Beijing=35, Lagos=88, Paris=42, Rio_de_Janeiro=84, San_Juan=81, Toronto=30)
#2.8.2
city=c("beijing","lagos","paris","rio de janeiro","san juan","toronto")
#2.8.3
names(temp)=city
#2.8.4
temp[1:3]
#2.8.5
temp[c("paris","san juan")]
#2.8.6
12:73
#2.8.7
seq(1,100,2)
#2.8.8
length(seq(6,55,4/7))
#answer:86
#2.8.9
#answer: numeric
#2.8.10
#answer: integer
#2.8.11
class(a<-1L)
#2.8.12
x <- c("1", "3", "5")
as.integer(x)

##2.10
#2.10.1
pop=(murders$population)
pop=sort(pop)
pop[1]
#answer: 563626
#2.10.2
pop=(murders$population)
index=order(pop)
index[1]
#answer: 51
#2.10.3
which.min(pop)
#2.10.4
states=(murders$state)
states[51]
#answer: "Wyoming"
#2.10.5
ranks=rank(pop)
my_df=data.frame(name=states, rank=ranks)
#2.10.6
ind=order(pop)
states=murders$state[ind]
my_df=data.frame(name=states, rank=sort(ind))
#2.10.7
data("na_example")  
str(na_example)
ind=is.na(na_example)
sum(ind)
#answer:145
#2.10.8
mean(na_example[!isna])
#answer: 2.30

##2.12
#2.12.1
ftemp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", 
          "San Juan", "Toronto")
ctemp =5/9*(ftemp-32)
city_temps <- data.frame(name = city, temperature = ftemp, converttemp=ctemp)
#2.12.2
sum((1/seq(1,100))^2)
#answer: 1.63
#2.12.3
murder_rate=(murders$total)/(murders$population)*100000
aver=mean(murder_rate)
#answer: 2.78

##2.14
#2.14.1
murder_rate=(murders$total)/(murders$population)*100000
low=murder_rate < 1
#2.14.2
ind=which(low)
#2.14.3
murders$state[low]
#2.14.4
northeast=murders$region=="Northeast"
murders$state[low&northeast]
#answer: "Maine"         "New Hampshire" "Vermont"
#2.14.5
aver=mean(murder_rate)
belowa=murder_rate <aver
sum(belowa)
#answer: 27
#2.14.6
index=match(c("AK","MI","IA"), murders$abb)
murders$state[index]
#answer: "Alaska"   "Michigan" "Iowa"  
#2.14.7
actual=c("MA", "ME", "MI","MO","MU") %in% murders$abb
c("MA", "ME", "MI","MO","MU")[actual]
#answer: "MA", "ME", "MI","MO"
#2.14.8
which(!actual)
c("MA", "ME", "MI","MO","MU")[5]
#answer: MU

##2.16
#2.16.1
library(dslabs)
data(murders)
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(log(population_in_millions, 10), total_gun_murders)
# 2.16.2
with(murders,hist(population_in_millions))
#2.16.3
boxplot(population~region, data = murders)
