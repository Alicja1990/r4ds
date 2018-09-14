library(nycflights13)
library(tidyverse)

# ! dplyr overwrites some base functions, so you need to type eg. stats::lag() or stats::filter()

sqrt(2) ^ 2 == 2 # gives FALSE, because these are number approximations
near(sqrt(2) ^ 2,  2)

nov_dec <- filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))

arrange(flights, year, month, day)

select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, starts_with("dep"), starts_with("arr"))
select(flights, ends_with("time"), ends_with("delay"))
select(flights, matches("^(del|arr)_(time|delay)$"))
select(flights, contains("TIME")) # bye default contains() ignores case letters

mutate(flights, gain = dep_delay - arr_delay, speed = distance / air_time * 60, 
       gain_per_hour = gain / hours) # use transmute() to only keep new columns

# Cumulative functions
cumsum()
cummean()

mutate(flights, min = as.numeric(str_sub(as.character(dep_time), -2, -1)), 
       hou = as.numeric(str_sub(as.character(dep_time), -4, -3)),
       min_from_midnight = hou * 60 + min)
x <- mutate(flights, del_rank = min_rank(desc(dep_delay)))
filter(x, del_rank <= 10)

not_cancelled <- flights %>% filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% group_by(dest) %>% summarise(n = n())
not_cancelled %>% group_by(tailnum) %>% summarise(plane_dist = sum(distance))

cancelled_delayed <- flights %>% 
  mutate(cancelled = is.na(dep_delay) | is.na(arr_delay)) %>%
  group_by(year, month, day) %>% 
  summarise(no_canc = mean(cancelled), mean_del = mean(arr_delay, na.rm = T))

daleyed_carriers <- flights %>% 
  group_by(carrier) %>% 
  summarise(mean_delay = mean(cancelled), mean_del = mean(arr_delay, na.rm = T))

worst_carriers <- flights %>% 
  group_by(carrier) %>%
  summarise(mean_delay = mean(arr_delay, na.rm = T)) %>%
  arrange(desc(mean_delay)) %>%
  slice(1:10)
  
worst_airports <- flights %>% 
  group_by(carrier, dest) %>%
  summarise(n(), mean = mean(arr_delay, na.rm = T))

worst_plane <- flights %>% 
  group_by(tailnum) %>%
  summarise(mean = mean(arr_delay, na.rm = T)) %>%
  arrange(desc(mean))

best_time <- flights %>%
  group_by(hour) %>%
  summarise(mean = mean(arr_delay, na.rm = T)) %>%
  arrange(mean)

delay_for_dest <- flights %>%
  group_by(dest) %>%
  summarise(total_delay = sum(arr_delay, na.rm = T))

delay_prop <- flights %>%
  group_by(dest) %>%
  mutate(prop = arr_delay / sum(arr_delay, na.rm = T))
  
dest_2_carriers <- flights %>%
  group_by(dest) %>%
  mutate(n_carrier = n_distinct(carrier)) %>%
  filter(n_carrier >= 2)

# How many flights of the plane before first delay bigger than 1 hour
before_delay <- flights %>%
  arrange(tailnum, year, month, day, hour, minute) %>%
  select(tailnum, year, month, day, hour, minute, dep_delay) %>%
  group_by(tailnum) %>%
  mutate(late = dep_delay > 60) %>%
  mutate(before = cumsum(late)) %>%
  filter(before < 1) %>%
  count(sort = T)

  
  
  
  
  
  