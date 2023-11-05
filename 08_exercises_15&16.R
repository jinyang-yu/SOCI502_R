##15.3
#1:E(S)=Np=25p
#2:SE(S)=sqrt(25*p*(1-p))
#3:E(X-bar)=p
#4:SE(X-bar)=sqrt(p*(1-p)/N)=sqrt(p*(1-p)/25)
#5
p= seq(0, 1, length = 100) 
se=sqrt(p*(1-p)/25)
library(tidyverse)
qplot(p, se)
#6
for(N in c(25, 100, 1000)){p= seq(0, 1, length = 100) 
se=sqrt(p*(1-p)/N)
qplot(p, se)}
#7: E(d)=p-(1-p)=2p-1
#8: SE(d)=2*sqrt(p*(1-p)/N)
#9
N=25
p=0.45
2*sqrt(p*(1-p)/N)
#> 0.1989975
#10:c

##15.5
#1
take_sample=function(p,N){x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))}
#2
p=0.45
N=100
B=10000
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})
errors=x_hat-p
#3:c
mean(errors)
hist(errors)
#4
mean(abs(errors))
#> 0.039503
#5
sd(errors)
#> 0.0495019
#6
#> 0.0495019
#7
set.seed(1)
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
X_bar <- mean(X)
sqrt(X_bar*(1-X_bar)/N)
#> 0.04990992
#8:c
N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(N,se)
#9:b
#10:b
#11
set.seed(1)
qqnorm(errors)
#12
p=0.45
N=100
sd=sqrt(p*(1-p)/N)
1 - pnorm(0.5, p, sd)
#> 0.1574393
#13
X_hat <- 0.51
se_hat <- sqrt(X_hat*(1-X_hat)/N)
1 - pnorm(.01, 0, se_hat) + pnorm(-0.01, 0, se_hat)

##15.7
library(dslabs)
data("polls_us_election_2016")
library(tidyverse)
polls = polls_us_election_2016 |> 
  filter(enddate >= "2016-10-31" & state == "U.S.") 
#1
N <- polls$samplesize[1]
X_hat <- polls$rawpoll_clinton[1]/100
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat
ci <- c(X_hat-qnorm(.975)*se_hat,X_hat+qnorm(.975)*se_hat)
ci
#>[1] 0.4492385 0.4907615
#2
library(dplyr)
poll <- polls |>
  mutate(X_hat=polls$rawpoll_clinton/100,se_hat=sqrt(X_hat*(1-X_hat)/polls$samplesize),lower=X_hat - qnorm(.975)*se_hat,upper=X_hat + qnorm(.975)*se_hat)|> select(pollster, enddate, X_hat, se_hat, lower, upper)
#3
poll=poll|> mutate(hit=(lower<0.482 & upper>0.482)) 
#4
poll|> summarize(mean(hit))
#>0.3142857
#5: 0.95
#6
polls <- polls_us_election_2016 |> 
  filter(enddate >= "2016-10-31" & state == "U.S.")  |>
  mutate(d_hat = rawpoll_clinton / 100 - rawpoll_trump / 100)
N <- polls$samplesize[1]
d_hat <- polls$d_hat[1]
X_hat <- polls$rawpoll_clinton[1]/100
sed_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
ci <- c(d_hat - qnorm(0.975)*sed_hat, d_hat + qnorm(0.975)*sed_hat)
#>[1] -0.001522975  0.081522975
#7
poll <- polls |>
  mutate(X_hat=polls$rawpoll_clinton/100,sed_hat=2*sqrt(X_hat*(1-X_hat)/polls$samplesize),lower=d_hat - qnorm(.975)*sed_hat,upper=d_hat + qnorm(.975)*sed_hat)|> select(pollster, enddate, X_hat, sed_hat, lower, upper)
poll=poll|> mutate(hit=(lower<0.021 & upper>0.021)) 
poll
#8
poll|> summarize(mean(hit))
#>0.7714286
#9
polls |> mutate(errors = d_hat - 0.021) |>
  ggplot(aes(x = pollster, y = errors)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
#10
polls |> mutate(errors=d_hat-0.021) |>
  group_by(pollster) |>
  filter(n()>=5) |>
  ggplot(aes(x=pollster,y=errors, color=pollster)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##15.11
#1
two_by_two <- data.frame(won = c("no", "yes"), 
                         important_events = c(9, 8),
                         other_career_matches = c(141, 342))
chisq_test <- two_by_two |> select(-won) |> chisq.test()
#> p-value = 0.06712
#2: c
#3
odds_important <- with(two_by_two, (important_events[2]/sum(important_events)) / (important_events[1]/sum(important_events)))
odds_other <- with(two_by_two, (other_career_matches[2]/sum(other_career_matches)) / (other_career_matches[1]/sum(other_career_matches)))
odds_important/ odds_other
log_or <- log(odds_important/ odds_other)
se <- two_by_two |> select(-won) |>
  summarize(se = sqrt(sum(1/important_events) + sum(1/other_career_matches))) |>
  pull(se)
ci <- log_or + c(-1,1) * qnorm(0.975) * se
#4:c
#5
two_by_two_2 <- two_by_two |> select(-won) |> mutate(important_events = important_events*2, other_career_matches = other_career_matches*2)
chisq.test(two_by_two_2)$p.value
#> p-value = 0.00544228
odds_important <- with(two_by_two_2, (important_events[2]/sum(important_events)) / (important_events[1]/sum(important_events)))
odds_other <- with(two_by_two_2, (other_career_matches[2]/sum(other_career_matches)) / (other_career_matches[1]/sum(other_career_matches)))
odds_important/ odds_other
log_or <- log(odds_important/ odds_other)
se <- two_by_two |> select(-won) |>
  summarize(se = sqrt(sum(1/important_events) + sum(1/other_career_matches))) |>
  pull(se)
ci <- log_or + c(-1,1) * qnorm(0.975) * se

##16.3
library(dslabs)
data(heights)
x <- heights |> filter(sex == "Male") |>
  pull(height)
#1
mean(x)
sd(x)
#>[1] 69.31475 [1] 3.611024
#2
set.seed(1)
N <- 50
X <- sample(x, N, replace = TRUE)
mean(X)
sd(X)
#>X_hat=70.47293 se_hat=3.426742
#3:b
#4
#it's confusing...sigma is the standard deviation of the population, and se_hat is the estimate of sigma
#Here I show the se, because I have already shown the estimate of sigma in question 2, and it has nothing to do with the provided formula:se=sigma/sqrt(N)
#I see here occurs a misuse of the label, this sigma means the estimate of standard error
X_hat <- mean(X)
se_hat <- sd(X)
se <- se_hat/ sqrt(N)
se
#>[1] 0.4846145
#5
ci <- c(qnorm(0.025, mean(X), se), qnorm(0.975, mean(X), se))
#>[1]  22.98158 117.96428
#6
mu <- mean(x)
set.seed(1)
B <- 10000
res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  X_hat <- mean(X)
  se_hat <- sd(X)
  se <- se_hat / sqrt(N)
  interval <- c(qnorm(0.025, mean(X), se) , qnorm(0.975, mean(X), se))
  between(mu, interval[1], interval[2])
})
mean(res)
#>[1] 0.9479
#7
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 
polls %>% ggplot(aes(pollster, spread)) + geom_boxplot() + geom_point()
#8:c
#9:d+bi
#10:d+b1 
#11:sigma1/sqrt(N1)
#12:d+b2
#13:sigma2/sqrt(N2)
#14:b2-b1
#15:sqrt(sigma^2/N2+sigma1^2/N1)
#16
polls %>% group_by(pollster)
sigma <- polls %>% group_by(pollster) %>% summarize(s = sd(spread))
#17:c
#18
res <- polls %>% group_by(pollster) %>% summarize(avg=mean(spread), s = sd(spread), N=n())
res
estimate <- max(res$avg) - min(res$avg)
estimate
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])
se_hat
ci <- c(estimate - qnorm(0.975)*se_hat, estimate + qnorm(0.975)*se_hat)
#19
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])
2 * (1 - pnorm(estimate / se_hat, 0, 1))
#20
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()
var <- polls %>% group_by(pollster) %>% summarize(avg = mean(spread), s = sd(spread))
var

#16.7
#1:a
#2
Pr_1 <- 1/8500
Pr_2 <- 1/100
Pr_1*Pr_2
#3:Pr(two children found dead with no evidence of harm∣mother is a murderer)*Pr(mother is a murderer)/Pr(two children found dead with no evidence of harm)
#4
Pr_1 <- 1/8500
Pr_2 <- 1/100
Pr_B <- Pr_1*Pr_2
Pr_A <- 1/1000000
Pr_BA <- 0.50
Pr_AB <- Pr_BA*Pr_A/Pr_B
Pr_AB
# [1] 0.425
#5:b
#6
library(dplyr)
library(dslabs)
data(polls_us_election_2016)
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
results <- polls %>% summarize(avg = mean(spread),  se = sd(spread)/sqrt(n()))
results
#7:b
#8
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
tau <- 0.01
miu <- 0
B <- sigma^2 / (sigma^2 + tau^2)
B
# [1] 0.342579
#9
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
sqrt(1 / (1 / sigma ^2 + 1 / tau ^2))
# [1] 0.005853024
#10
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
est <- B * mu + (1 - B) * Y
est
# [1] 0.002731286
ci <- c(est - qnorm(0.975) * se, est + qnorm(0.975) * se)
ci
# [1] -0.008740432  0.014203003
#11
exp_value <- B*mu + (1-B)*Y 
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
pnorm(0, exp_value, se)
# [1] 0.3203769
#12
mu <- 0
sigma <- results$se
Y <- results$avg
taus <- seq(0.005, 0.05, len = 100)
p_calc <- function(tau) {
  B <- sigma ^ 2 / (sigma^2 + tau^2)
  se <- sqrt(1 / (1/sigma^2 + 1/tau^2))
  exp_value <- B * mu + (1 - B) * Y
  pnorm(0, exp_value, se)
}
ps <- p_calc(taus)
plot(taus, ps)

#16.9
#1
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 |> 
  filter(state != "U.S." & enddate >= "2016-10-31") |> 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
cis <- polls %>% mutate(X_hat = (spread+1)/2, se = 2*sqrt(X_hat*(1-X_hat)/samplesize), 
                        lower = spread - qnorm(0.975)*se, upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)
#2
add <- results_us_election_2016 |> 
  mutate(actual_spread = clinton/100 - trump/100) |> 
  select(state, actual_spread)
cis <- cis |> 
  mutate(state = as.character(state)) |> 
  right_join(add, by = "state")
p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% summarize(proportion_hits = mean(hit))
p_hits
#3
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(pollster) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n(), grade = grade[1]) %>%
  arrange(desc(proportion_hits))
p_hits
#4
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(state) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n()) %>%
  arrange(desc(proportion_hits)) 
p_hits
#5
p_hits %>% mutate(state = reorder(state, proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) + 
  geom_bar(stat = "identity") +
  coord_flip()
#6
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
head(cis)
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))
tail(errors)
#7
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))
p_hits <- errors %>%  group_by(state) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n())
p_hits %>% mutate(state = reorder(state, proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) + 
  geom_bar(stat = "identity") +
  coord_flip()
#8
hist(errors$error)
median(errors$error)
# [1] 0.037
#9
errors %>% filter(grade %in% c("A+","A","A-","B+") | is.na(grade)) %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(state, error)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_boxplot() + 
  geom_point()
#10
errors %>% filter(grade %in% c("A+","A","A-","B+") | is.na(grade)) %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(state, error)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_boxplot() + 
  geom_point()