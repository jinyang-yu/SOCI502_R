##19.6
#1
library(dslabs)
library(tidyverse)
library(dplyr)
library(generics)
data("research_funding_rates")
research_funding_rates
two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(sum) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)
two_by_two
#2
two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1),
         women = round(women/sum(women)*100, 1)) %>% 
  filter(awarded == "yes")
#3
two_by_two %>% 
  select(-awarded) %>% 
  chisq.test() %>% 
  tidy
#4
dat <- research_funding_rates %>% 
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total") %>%
  mutate(discipline = reorder(discipline, applications, sum))
dat
#5
dat %>% 
  ggplot(aes(discipline, success, size = applications, color = gender)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point()
#6
do_chisq_test <- function(m, x, n, y){
  tab <- tibble(men = c(x, m-x), women = c(y, n-y))
  tidy(chisq.test(tab)) %>% 
    mutate(difference = y/n - x/m)
}

res <- research_funding_rates %>% 
  group_by(discipline) %>%
  do(do_chisq_test(.$applications_men, .$awards_men, 
                    .$applications_women, .$awards_women)) %>%
  ungroup() %>%
  select(discipline, difference, p.value) %>%
  arrange(difference)
res 
#7
res %>% summarize(overall_avg = mean(difference), 
                  se = sd(difference)/sqrt(n()))
res %>% 
  ggplot(aes(sample = scale(difference))) + 
  stat_qq() + geom_abline()
