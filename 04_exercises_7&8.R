#8.15
library(dplyr)
library(ggplot2)
library(dslabs)
data(murders)
data(heights)
library(ggthemes)
library(extrafont)
extrafont::font_import()
#8.15.1
p = murders |> ggplot()
class(p)
#answer: "gg"     "ggplot"
#8.15.2
#answer:b
#8.15.3
p = heights |> ggplot()
#8.15.4
class(p)
#answer: "gg"     "ggplot"
#8.15.5
#answer:c
#8.15.6
murders|>ggplot(aes(x =population, y=total)) + geom_point()+theme(text = element_text(family = "Arial"))
#8.15.7
murders |> ggplot(aes(total,population )) +geom_point()
#8.15.8
#answer:a
#8.15.9
p=murders |> ggplot(aes(population, total, label = abb))+geom_point()+geom_label()
#8.15.10
#answer:c
#8.15.11
p + geom_point(size = 3, color ="blue")
#8.15.12
#answer:b
#8.15.13
p=murders |> ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(col=region), size = 3)
#8.15.14
p = murders |> 
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label() 
p=p+scale_x_log10() 
#8.15.15
p=p+scale_y_log10() 
#8.15.16
p=p+ggtitle("Gun Murders data")