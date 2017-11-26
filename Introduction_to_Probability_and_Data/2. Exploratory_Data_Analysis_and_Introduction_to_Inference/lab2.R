library(statsr)
library('devtools')
library('curl')
library('ggplot2')
library('dplyr')

data('nycflights')
# column name of dataframe 
names(nycflights)
# quick view of dataframe (structure)
str(nycflights)
# Distribution of departure delays of all 
# flights with a histogram
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram()
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 15)
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 150)
# departure delays of flights headed to RDU
rdu_flights <- nycflights %>% 
  filter(dest == 'RDU')
ggplot(data = rdu_flights, aes(x = dep_delay)) + 
  geom_histogram()
# summary statistics for a single numerical variable:
# mean, median, sd, var, IQR, range, min, max
rdu_flights %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())
# filter based on multiple criteria
# flights headed to San Francisco (SFO) in February
sfo_feb_flights <- nycflights %>%
  filter(dest == "SFO", month == 2)

# Question 1
dim(sfo_feb_flights)[1]
# Answer: 68

# Question 2: 
ggplot(data = sfo_feb_flights, aes(x = arr_delay)) + 
  geom_histogram()

sfo_feb_flights %>%
  summarise(mean_dd = mean(arr_delay), sd_dd = sd(arr_delay), 
            median_dd = median(arr_delay),n = n())
# Answer: No flight is delayed > 2h

# Question 3
# summary stats for each origin airport
rdu_flights %>%
  group_by(origin) %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())

sfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(median = median(arr_delay), IQR = IQR(arr_delay))
# Answer: DL and UA

# Question 4
# To find the highest averagea delay we need to "arrange" these average delays in "descending" order
nycflights %>%
  group_by(month) %>%
  summarise(mean_dd = mean(dep_delay)) %>%
  arrange(desc(mean_dd))
# Answer: July

# Question 5
nycflights %>%
  group_by(month) %>%
  summarise(median_dd = median(dep_delay)) %>%
  arrange(desc(median_dd))
# Answer: December

# Question 6
# in the data frame month is stored as a numerical variable (numbers 1 - 12). 
# Therefore we can force R to treat this variable as categorical var.
# factor(month)
ggplot(nycflights, aes(x = factor(month), y = dep_delay)) +
  geom_boxplot()
# Answer: Median would be more reliable as the distribution of delays is skewed.

# Question 7
# classifying each flight as "on time" or "delayed"
# mutate: create a new variable
nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"))

nycflights %>%
  group_by(origin) %>%
  summarise(ot_dep_rate = sum(dep_type == "on time") / n()) %>%
  arrange(desc(ot_dep_rate))
# segmented bar plot
ggplot(data = nycflights, aes(x = origin, fill = dep_type)) +
  geom_bar()
# Answer: LGA

# Question 8
nycflights <- nycflights %>%
  mutate(avg_speed = distance / (air_time/60))

nycflights %>%
  select(avg_speed, tailnum) %>%
  arrange(desc(avg_speed))
# Answer: N666DN

# Question 9
ggplot(data = nycflights, aes(x = avg_speed, y = distance)) +
  geom_point()
# Answer: There is an overall postive association between distance and average speed.

# Qusetion 10
nycflights <- nycflights %>%
  mutate(arr_type = ifelse(arr_delay <= 0, "on time", "delayed"))

flights_dep_delay <- nycflights %>%
  filter(dep_type == "delayed")

flights_dep_delay_arr_ot <- nycflights %>%
  filter(dep_type == "delayed", arr_type == "on time")

dim(flights_dep_delay_arr_ot)[1] / dim(flights_dep_delay)[1]
# Answer: 0.1833639