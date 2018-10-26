library(tidyverse)

library(lubridate)
library(nycflights13)

# 16.2.4.3
d1 <- "January 1, 2010"
mdy(d1)
d2 <- "2015-Mar-07"
ymd(d2)
d3 <- "06-Jun-2017"
dmy(d3)
d4 <- c("August 19 (2015)", "July 1 (2015)")
mdy(d4)
d5 <- "12/30/14" # Dec 30, 2014
mdy(d5)

# 16.3.4.1

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>% 
  mutate(time = hour(dep_time) * 100 + minute(dep_time),
    month = as.factor(month(dep_time))) %>%
  ggplot(aes(x = time, y = ..density.., colour = month)) +
  geom_freqpoly(binwidth = 100)

# 16.3.4.2

flights_dt %>% 
  mutate(diff = dep_time - sched_dep_time) %>%
  mutate(diff = as.numeric(diff/60)) %>%
  mutate(discrepancy = dep_delay - diff) %>%
  select(dep_time, sched_dep_time, diff, dep_delay, discrepancy) %>%
  filter(discrepancy != 0 & discrepancy != 1440)

# 16.3.4.3

flights_dt %>%
  mutate(dur = as.numeric(arr_time - dep_time), 
         diff = dur - air_time) %>%
  select(arr_time, dep_time, dur, air_time, diff, origin, dest)

# 16.3.4.4

flights_dt %>% 
  mutate(time = hour(sched_dep_time) * 60 + minute(sched_dep_time)) %>%
  group_by(time) %>%
  mutate(mean_del = mean(dep_delay, na.rm = T)) %>%
  ggplot(aes(x = time, y = mean_del)) +
  geom_point() + 
  geom_smooth()

# 16.3.4.5

flights_dt %>% 
  mutate(week_day = wday(sched_dep_time)) %>%
  group_by(week_day) %>%
  summarise(mean_dep_del = mean(dep_delay, na.rm = T), 
         mean_arr_del = mean(arr_delay, na.rm = T)) %>%
  arrange(mean_arr_del)
  
# 16.3.4.6

ggplot(flights_dt) +
  geom_histogram(aes(x = minute(sched_dep_time)), binwidth = 1)

ggplot(diamonds) +
  geom_histogram(aes(x = carat %% 1 * 100), binwidth = 1)

# 16.3.4.7

flights_dt %>% 
  mutate(minute = minute(sched_dep_time), 
         early = dep_delay < 0) %>%
  group_by(minute) %>%
  summarise(prop_on_time = mean(early, na.rm = T)) %>%
  ggplot(aes(x = minute, y = prop_on_time)) +
    geom_point()

flights_dt %>% 
  mutate(minute = minute(sched_dep_time) %% 10, 
         early = dep_delay < 0) %>%
  group_by(minute) %>%
  summarise(prop_on_time = mean(early, na.rm = T)) %>%
  ggplot(aes(x = minute, y = prop_on_time)) +
  geom_point()

# 16.4.5.3

x <- dmy("01-12-2014") + months(1:12)

y <- today() - day(today() - 1) + months(c((-month(today()) + 1):(12-month(today()))))
y <- floor_date(today(), unit = "year") + months(0:11)

# 16.4.5.4
how_old <- function(dob) {
  x <- round((dob %--% today()) / years(1), 2)
  paste0("You are ", x, " years old.")
}

dob <- ymd("1990-04-28")
how_old(dob)



















