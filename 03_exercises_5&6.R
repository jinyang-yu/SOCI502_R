##5.3
#5.3.1
path = system.file("extdata", package = "dslabs") 
files = list.files(path)
#not all files are csv
#files that ends with csv: c("carbon_emissions.csv","fertility-two-countries-example.csv", "life-expectancy-and-fertility-two-countries-example.csv","murders.csv","olive.csv","ssa-death-probability.csv")
fullpath=file.path(path, files)
library(readr)
for(i in c(2,3,5,6,7,9)){dat=read_csv(fullpath[i])}
#5.3.2
#what does the "skip the header" mean?
#Skip the first column?
dat=read_csv(fullpath[7], col_select=c(Region,Area,palmitic,palmitoleic,stearic,oleic,linoleic,linolenic,arachidic,eicosenoic))
#Or skip the first row?
dat=read_csv(fullpath[7], skip=1)
#5.3.3
names(dat)
read_lines(fullpath[7],n_max = 1)

##6.2
#6.2.1
library(data.table)
library(dslabs)
data(murders)
murders = setDT(murders)
murders[, rate := total / population * 100000]
#6.2.2
murders[, rank := rank(-rate)]
#6.2.3
murders[, .(state, abb)] 
#6.2.4
murders[rank <= 5]
#6.2.5
no_south = murders[region!= "South"]
nrow(no_south)
#answer: 34
#6.2.6
murders_nw = murders[region%in% c("Northeast", "West")]
nrow(murders_nw)
#answer: 22
#6.2.7
my_states=murders_nw[rate<1]
#or:
my_states= murders[region%in% c("Northeast", "West") & rate<1]
my_states[, .(state, rate, rank)]

#6.5
library(NHANES)
data(NHANES)
#6.5.1
nhanes = setDT(NHANES)
ref=nhanes[AgeDecade==" 20-29", .(average = mean(BPSysAve, na.rm=TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE))]
#6.5.2
ref=nhanes[AgeDecade==" 20-29", .(min_max=quantile(BPSysAve,c(0, 1),na.rm=TRUE))]
#answer: min=84, max=179
#6.5.3
ref=nhanes[Gender == "female", .(average = mean(BPSysAve, na.rm=TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE)), by = AgeDecade]
#6.5.4
ref=nhanes[Gender == "male", .(average = mean(BPSysAve, na.rm=TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE)), by = AgeDecade]
#6.5.5
ref=nhanes[Gender=="male"&AgeDecade==" 40-49", .(average=mean(BPSysAve, na.rm=TRUE)), by= Race1]
ref[order(average)]







