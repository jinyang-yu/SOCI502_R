##13.9
#1: 3/15=1/5
#2: 1-1/5=4/5
#3: 0.17
1/5*(1-2/14)
#4: 0.16
1/5*(1-1/5)
#5: b
#6: 7/15
#7: 0.33
(5/6)^6
#8:
#the Celtics win at least one game in 7 games: 0.97
1-0.6^7
#the Celtics win one of the first four games: 0.87
1-0.6^4
#9: 0.87
B <- 10000
set.seed(1052)
celtic_wins <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games=="win")
})
mean(celtic_wins)
#10: 0.34
n <- 6
outcomes <- c(0,1)
l <- rep(list(outcomes), n)
possibilities <- expand.grid(l)
possibilities
results <- rowSums(possibilities)>=4
mean(results)
#11: 0.34
B <- 10000
set.seed(1136)
cavs_wins <- replicate(B, {
  simulated_games <- sample(c(0,1), 6, replace = TRUE, prob = c(0.5, 0.5))
  sum(simulated_games)>=4
})
mean(cavs_wins)
#12:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}
p <- seq(0.5, 0.95, 0.025)
pr=sapply(p, prob_win)
library(tidyverse)
qplot(p,pr)
#13:
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}
N <- seq(1, 25, 2)
pr=sapply(N, prob_win)
qplot(N,pr)


##13.13
#1: 0.09
m <- 64
s <- 3
pnorm(60, m, s)
#2：0.004
1-pnorm(72, m, s)
#3: 0.68
pnorm(67, m, s)-pnorm(61, m, s)
#4: They don't change
s=s*2.54
m=m*2.54
pnorm(60*2.54, m, s)
1-pnorm(72*2.54, m, s)
pnorm(67*2.54, m, s)-pnorm(61*2.54, m, s)
#5:0.68
rnorm(50000)
pnorm(1)-pnorm(-1)
#6: b
#7: 75.98
qnorm(0.99, 69,3)
#8:
B <- 10000
highest <- replicate(B, {
  simulated_data <- rnorm(10000, 100, 15)
  max(simulated_data)
})
qplot(highest)


##14.10
#1: 1/19
#2:
X<- sample(c(-17,1), n, replace = TRUE, prob=c(1/19, 18/19))
#3: E=0.05
-17*1/19+1*18/19
#4: SE=4.02
abs(1-(-17))*sqrt(18/19*1/19)
#5: S=10
set.seed(1)
n<-10^3
X <- sample(c(-17,1), size = n, replace = TRUE, prob=c(1/19, 18/19))
S<-sum(X)
S
#6: 52.63
n<-10^3
n * (-17*1/19 + 1 * 18/19)
#7: 127.10
n<-10^3
sqrt(n) * 18 * sqrt(18)/19 
#8: 0.34
mu <- n * 1/19
se <-  sqrt(n) * 18 * sqrt(18)/19 
pnorm(0, mu, se)
#9: E=52.35, SE=126.98
set.seed(1)
n <- 1000
B <- 10000
roulette_winnings <- function(n){
  X <- sample(c(-17,1), size = n, replace = TRUE, prob=c(1/19, 18/19))
  sum(X)
}
S <- replicate(B, roulette_winnings(n))
mean(S)
sd(S)
#10: 0.34
mean(S<0)
#11: b
#12: 
set.seed(1)
n<-10^3
X <- sample(c(-17,1), size = n, replace = TRUE, prob=c(1/19, 18/19))
Y<-mean(X)
Y
#13: 0.05
1/19
#14: 0.08
n=2500
abs(1-(-17))*sqrt(18/19*1/19)/sqrt(n)
#15: 0.26
#Because in Q17 n=2500, here I use n=2500 to be consistent with the result of Q17
n=2500
mu <- 1/19
se <- abs(1-(-17))*sqrt(18/19*1/19)/sqrt(n)
pnorm(0, mu, se)
#16:
set.seed(1)
n <- 2500
B <- 10000
roulette_winnings <- function(n){
  X <- sample(c(-17,1), n, replace = TRUE, prob=c(1/19, 18/19))
  mean(X)
}
Y <- replicate(B, roulette_winnings(n))
mean(Y)
sd(Y)
#17: 0.26
mean(Y<0)
#18:c


##14.12
#1
n <- 10000
loss_per_foreclosure <- -200000
p <- 0.3 
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
S=sum(defaults * loss_per_foreclosure)
#2
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})
qplot(losses)
#3
n*(p*loss_per_foreclosure + (1-p)*0)
#> -6e+08
#4
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))
#> 9165151
#5
- loss_per_foreclosure*p/(1-p)/180000
#> 0.48
#6
l <- loss_per_foreclosure
z <- qnorm(0.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x/180000
#> 0.49
#7:d