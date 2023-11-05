##3.6
#3.6.1
#answer: "Not all positives"
#3.6.2
#answer:d.all(!x)
#3.6.3
library(dslabs)
data(murders)
len=nchar(murders$state)
ind=len>8
new_names=murders$abb[ind]
#answer: "CA" "CT" "DC" "LA" "MA" "MN" "MS" "NH" "NJ" "NM" "NC" "ND" "PA" "RI" "SC""SD" "TN" "WA" "WV" "WI"
#3.6.4
sum_n=function(n){sum(1:n)}
sum_n(5000)
#answer: 12502500
#3.6.5
altman_plot=function(x, y){plot(x+y, y-x)}
#3.6.6
#answer: x=-3
#3.6.7
compute_s_n=function(n){
  x=1: n
  sum(x^2)}
compute_s_n(10)
#answer: 385
#3.6.8
s_n = vector("numeric",length= 25)
for(n in 1:25){s_n[n]=compute_s_n(n)}
#3.6.9
n=1:25
s_n=sapply(n, compute_s_n)
#3.6.10
s_n=map_dbl(n, compute_s_n)
#3.6.11
n=1:25
plot(n, s_n)
#3.6.12
n=n
s_n_1=n*(n+1)*(2*n+1)/6
s_n=compute_s_n(n)
identical(s_n_1, s_n)

##4.2
#4.2.1
data(co2)
#answer: d
#4.2.2
data("ChickWeight")
#answer: b
#4.2.3
data("BOD")
#answer: c
#4.2.4
data("BJsales")
data("EuStockMarkets")
data("DNase")
data("Formaldehyde")
data("Orange")
data("UCBAdmissions")
#answer:c.DNase, d.Formaldehyde, e.Orange

##4.4
#4.4.1
library(dplyr)
library(dslabs)
data(murders)
murders= mutate(murders, population_in_millions = population / 10^6)
murders= mutate(murders, rate = total / population*100000)
#4.4.2
murders= mutate(murders, rank = rank(-rate))
#4.4.3
select(murders, state, abb) 
#4.4.4
filter(murders, rank <=5)
#4.4.5
no_south=filter(murders, region!="South")
nrow(no_south)
#answer:34
#4.4.6
murders_nw=filter(murders, region %in% c("Northeast", "West"))
nrow(murders_nw)
#answer: 22
#4.4.7
my_states=filter(murders_nw, rate<1)
#or:
my_states=filter(murders, rate < 1 & region %in% c("Northeast", "West"))
select(my_states, state, rate,rank)

##4.6
#4.6.1
library(dslabs)
data(murders)
murders=mutate(murders, rate =  total / population * 100000, rank = rank(-rate)) |>select(state, rate, rank)
#4.6.2
library(dslabs)
data(murders)
my_states = murders |>
  mutate (rate =  total / population * 100000, rank = rank(-rate))|> 
  filter (rate < 1 & region %in% c("Northeast", "West")) |>
  select (state, rate, rank)

##4.10
#4.10.1
library(NHANES)
data(NHANES)
ref=NHANES|>filter(Gender=="female", AgeDecade==" 20-29")|>summarize(average=mean(BPSysAve, na.rm=TRUE), standard_deviation=sd(BPSysAve,na.rm=TRUE))
#answer: average=108,standard deviation=10.1
#4.10.2
ref_avg=ref|>pull(average)
#4.10.3
ref=NHANES|>filter(Gender=="female", AgeDecade==" 20-29")|>summarize(min_max=quantile(BPSysAve, c(0, 1),na.rm=TRUE))
#answer: min=84, max=179
#4.10.4
ref=NHANES|>filter(Gender=="female")|>group_by(AgeDecade)|>summarize(average=mean(BPSysAve, na.rm=TRUE), standard_deviation=sd(BPSysAve,na.rm=TRUE))
#4.10.5
ref=NHANES|>filter(Gender=="male")|>group_by(AgeDecade)|>summarize(average=mean(BPSysAve, na.rm=TRUE), standard_deviation=sd(BPSysAve,na.rm=TRUE))
#4.10.6
ref=NHANES|>group_by(AgeDecade, Gender)|>summarize(average=mean(BPSysAve, na.rm=TRUE), standard_deviation=sd(BPSysAve,na.rm=TRUE))
#4.10.7
ref=NHANES|>filter(Gender=="male", AgeDecade==" 40-49")|>group_by(Race1)|>summarize(average=mean(BPSysAve, na.rm=TRUE))|>arrange(average)

##4.15
#4.15.1
library(dslabs)
data(murders)
murders
#answer:b
#4.15.2
murders_tibble=as_tibble(murders)
#4.15.3
murders|>group_by(region)
#4.15.4
murders_tibble$population|>log()|>mean()|>exp()
#answer: 3675209
#4.15.5
compute_s_n=function(n) {
  return(tibble(n =n, s_n = n*(n+1)/2, s_n_2=n*(n+1)/2))
}
n=1:100
s_n_df=map_df(n, compute_s_n)
#or:
compute_s_n=function(n) {
return(tibble(n =n, s_n = sum(1:n), s_n_2=sum(1:n)))}
n=1:100
s_n_df=map_df(n, compute_s_n)
