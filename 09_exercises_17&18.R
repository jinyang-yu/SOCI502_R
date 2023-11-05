##17.5
#1
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1009)
galton_heights <- GaltonFamilies |>
  group_by(family, gender) |>
  sample_n(1)|>
  ungroup()|>
  select(father, mother, gender, childHeight)
galton_heights 
#2
#fathers and daughters
galton_heights |> filter(gender=="female")|> ggplot(aes(father, childHeight)) + 
  geom_point(alpha = 0.5)
#mothers and daughters
galton_heights |> filter(gender=="female")|> ggplot(aes(mother, childHeight)) + 
  geom_point(alpha = 0.5)
#mothers and sons
galton_heights |> filter(gender=="male")|> ggplot(aes(mother, childHeight)) + 
  geom_point(alpha = 0.5)
#fathers and sons
galton_heights |> filter(gender=="male")|> ggplot(aes(father, childHeight)) + 
  geom_point(alpha = 0.5)
#3
#fathers and daughters
galton_heights |> filter(gender=="female")|> summarize(r = cor(father, childHeight)) |> pull(r)
#[1] 0.4626973
#mothers and daughters
galton_heights |> filter(gender=="female")|> summarize(r = cor(mother, childHeight)) |> pull(r)
#[1] 0.2612346
#mothers and sons
galton_heights |> filter(gender=="male")|> summarize(r = cor(mother, childHeight)) |> pull(r)
#[1] 0.3247214
#fathers and sons
galton_heights |> filter(gender=="male")|> summarize(r = cor(father, childHeight)) |> pull(r)
#[1] 0.4391516

##18.4
#1
library(Lahman)
dat <- Batting |> filter(yearID == 2002) |>
  mutate(pa = AB + BB, 
         singles = (H - X2B - X3B - HR) / pa, bb = BB / pa) |>
  filter(pa >= 100) |>
  select(playerID, singles, bb)
avg <- Batting |> filter(yearID >1998 & yearID <2002) |>
  mutate(pa = AB + BB, 
         avg_singles = (H - X2B - X3B - HR) / pa, avg_bb = BB / pa) |>
  filter(pa >= 100) |>
  select(playerID, avg_singles, avg_bb)
#2
dat <- inner_join(dat, avg, by = "playerID")
rdat <- dat |>
  summarise(singles_r = cor(singles,avg_singles ), bb_r = cor(bb, avg_bb ))
rdat
#  singles_r      bb_r
#1 0.4614546 0.6501091
#3
library(ggplot2)
dat |>
  ggplot(aes(singles,avg_singles))+
  geom_point(alpha = 0.5)
dat |>
  ggplot(aes(bb,avg_bb))+
  geom_point(alpha = 0.5)
#4
lm(singles ~ avg_singles , data = dat)
lm(bb ~ avg_bb , data = dat)

##18.6
#1
library(HistData)
data("GaltonFamilies")
set.seed(1)
galton_heights <- GaltonFamilies |>
  group_by(family, gender) |>
  sample_n(1) |>
  ungroup()

cors <- galton_heights |> 
  pivot_longer(father:mother, names_to = "parent", values_to = "parentHeight") |>
  mutate(child = ifelse(gender == "female", "daughter", "son")) |>
  unite(pair, c("parent", "child")) |> 
  group_by(pair) |>
  summarize(cor = cor(parentHeight, childHeight))
library(broom)
galton_heights |> 
  pivot_longer(father:mother, names_to = "parent", values_to = "parentHeight") |>
  mutate(child = ifelse(gender == "female", "daughter", "son")) |>
  unite(pair, c("parent", "child")) |> 
  group_by(pair) |> do(tidy(lm(childHeight ~ parentHeight, data = .))) 
#2
galton_heights |> 
  pivot_longer(father:mother, names_to = "parent", values_to = "parentHeight") |>
  mutate(child = ifelse(gender == "female", "daughter", "son")) |>
  unite(pair, c("parent", "child")) |> 
  group_by(pair) |> do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) 
#3
galton_heights |> 
  pivot_longer(father:mother, names_to = "parent", values_to = "parentHeight") |>
  mutate(child = ifelse(gender == "female", "daughter", "son")) |>
  unite(pair, c("parent", "child")) |> 
  group_by(pair) |> do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) |>
  filter(term == "parentHeight") |>
  select(pair, estimate, conf.low, conf.high)|>
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()
#4
B <- 1000
N <- 50
gh=galton_heights |> 
  pivot_longer(father:mother, names_to = "parent", values_to = "parentHeight") |>
  mutate(child = ifelse(gender == "female", "daughter", "son")) |>
  unite(pair, c("parent", "child")) |> 
  group_by(pair)
lse <- replicate(B, {
  sample_n(gh, N, replace = TRUE) |>
    lm(childHeight ~ parentHeight, data = _) |> 
    coef() 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
#5
Teams |>
  filter(yearID == 1971) |>
  lm(R ~ BB, data = _) |>
  tidy() 
#6
Teams|>filter(yearID %in% 1962:2001)|>
  group_by(yearID) |>
  summarize(tidy(lm(R ~ BB), conf.int = TRUE)) 
#7
res <- Teams |>
  filter(yearID %in% 1961:2001)|>
  group_by(yearID) |>
  summarize(tidy(lm(R ~ BB), conf.int = TRUE)) |>
  ungroup() 
res |>
  filter(term == "BB") |>
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")
#8
res <- Teams |>
  filter(yearID %in% 1961:2001)|>
  group_by(yearID) |>
  summarize(tidy(lm(R ~ BB + HR), conf.int = TRUE)) |>
  ungroup() 
res |>
  filter(term == "BB") |>
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

##18.10
#1
teams_data <- Teams |> 
  filter(yearID %in% 1962:2002) |> 
  mutate(PA=BB+AB,
         BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G,
         OPS = BB / PA + (singles + 2*X2B + 3*X3B + 4*HR)/AB) |>
  select(teamID, yearID, OPS, BB, singles, doubles, triples, HR, R)
fit <- teams_data |>  
  filter(yearID <= 2001) |> 
  lm(R ~ OPS, data = _)
teams_data$R_hat <- predict(fit, newdata = teams_data)
teams_data %>% filter(yearID == 2001) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()
#2
teams_data|>mutate(r = cor(R, OPS)) |>
  ggplot(aes(yearID, r)) + 
  geom_point() 
#3
#weight for BB is AB/PA
#4
Teams |> filter(yearID %in% 1962:2021) |>
  group_by(teamID) |>
  mutate(PA = BB + AB,wt = AB/PA) |>
  ungroup() |>
  ggplot(aes(teamID,wt)) +
  geom_point()
Teams |> filter(yearID %in% 1962:2021) |>
  group_by(yearID) |>
  mutate(PA = BB + AB,wt = AB/PA) |>
  ungroup() |>
  ggplot(aes(yearID,wt)) +
  geom_point()
ops <- Teams |> filter(yearID %in% 1962:2021) |>
  mutate(PA = BB + AB,wt = AB/PA) |>
  select(wt)
ops_hat <- mean(ops$wt)
ops_hat
#0.9127769
#5
dat <- Teams |> filter(yearID %in% 1962:2021) |>
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
#6
R_hat <- predict(dat)
cor(ops,R_hat)
#7
ops <- Batting |> filter(yearID %in% 1962:2021) |>
  mutate(PA = BB + AB,wt = AB/PA) |>
  select(wt)

dat <- Batting |> filter(yearID %in% 1962:2021) |>
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
R_hat <- predict(dat)
plot(ops$wt,R_hat)
#8
dat <- Batting |> filter(yearID %in% 1962:2021) |>
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G,PA = BB + AB,
         ops = AB/PA,
         diff = ops-R) |>
  select(playerID,diff)
dat <- dat[order(-dat$diff),]
max(dat$diff,na.rm=TRUE)