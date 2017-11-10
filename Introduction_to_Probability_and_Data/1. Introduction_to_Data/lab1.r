library(dplyr)
library(ggplot2)
library(statsr)

# Dataset 1: Dr. Arbuthnot's Baptism Records
# load data
source("http://www.openintro.org/stat/data/arbuthnot.R")
#data(arbuthnot)
# dimension of data frame
dim(arbuthnot)
# name of cols
names(arbuthnot)
# access a single column of df
# dataframe $ column
arbuthnot$boys
# plot of the # of girls baptized per year
ggplot(data = arbuthnot, aes(x = year, y = girls)) +
  geom_point()
# syntax of ggplot --> ?ggplot
# add new var to the df
arbuthnot <- arbuthnot %>%
  mutate(total = boys + girls)
# total # of baptism per year
ggplot(data = arbuthnot, aes(x = year, y = total)) +
  geom_line()
# line plot & scatter plot
ggplot(data = arbuthnot, aes(x = year, y = total)) +
  geom_line() +
  geom_point()
# plot of the proportion of boys born over time
ggplot(data = arbuthnot, aes(x = year, y = boys/total)) +
  geom_line() +
  geom_point()
# boys outnumber girls in each year --> return boolean
arbuthnot <- arbuthnot %>%
  mutate(more_boys = boys > girls)

# Dataset 2: Present birth records
source("http://www.openintro.org/stat/data/present.R")
# What years are included in this dataset?
range(present$year)
# plot prop_boys over time
present <- present %>% mutate(total = boys + girls)
present <- present %>% mutate(prop_boys = boys/total)
ggplot(data = present, aes(x = year, y = prop_boys)) + 
    geom_line()
# create a var called more_boys
present <- present %>% mutate(more_boys = boys > girls)
# plot boy-to-girl ratio over time
present <- present %>% mutate(boy_to_girl_ratio = boys/girls)
ggplot(data = present, aes(x = year, y = boy_to_girl_ratio)) + geom_line()
# plot total_birth over time
ggplot(data = present, aes(x = year, y = total)) + geom_line()