library(tidyverse)
library(Lahman)
library(babynames)
library(nasaweather)
library(fueleconomy)
library(nycflights13)

Lahman::Batting %>%
  arrange(playerID, yearID, stint)

babynames %>%
  group_by(year, name, sex) %>%
  filter(n() >1)

atmos %>%
  group_by(year, month, lat, long) %>%
  filter(n() >1) %>%
  nrow()

vehicles %>%
  group_by(id) %>%
  filter(n() > 1) %>%
  nrow()

diamonds %>%
  distinct() %>%
  nrow()
nrow(diamonds)

# Mutate join

flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)

flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")
# same as:
flights2 %>%
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

# Inner and outter joins
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

x %>% 
  inner_join(y, by = "key") # only keeps observations, that appear in both tables
left_join()
right_join()
full_join()

by = NULL # when "by" is not specified, there is a natural join, which means, that it uses all the variable from both tables
by = c("a", "b") # by col a from table 1 and col b in table 2



# 13.4.6.1
library(maps)
flights %>%
  group_by(dest) %>%
  summarise(mean_del = mean(arr_delay, na.rm = T)) %>%
  left_join(airports, c("dest" = "faa")) %>%
  ggplot(aes(lon, lat, color = mean_del)) +
  scale_color_gradient(low = "green", high = "red") +  
  borders("state") +
  geom_point() +
  coord_quickmap()

# 13.4.6.2
flights %>%
  left_join(airports[, c("faa", "lat", "lon")], by = c("origin" = "faa")) %>%
  left_join(airports[, c("faa", "lat", "lon")], by = c("dest" = "faa")) 

# 13.4.6.3
flights %>%
  left_join(planes %>%
              mutate(age = 2013 - year) %>%
              select("tailnum", "age"), by = "tailnum") %>%
  group_by(tailnum) %>%
  summarise(mean_del = mean(arr_delay, na.rm = T), age = mean(age, na.rm = T)) %>%
  ggplot(aes(x = age, y = mean_del)) + 
    geom_point()

# 13.4.6.4
flights %>%
  inner_join(weather, by = c("origin" = "origin",
                            "year" = "year",
                            "month" = "month",
                            "day" = "day",
                            "hour" = "hour")) %>%
  group_by(temp) %>%
  summarise(mean_del = mean(dep_delay, na.rm = T)) %>%
  ggplot(aes(x = mean_del, y = temp)) +
  geom_point()

flights %>%
  inner_join(weather, by = c("origin" = "origin",
                             "year" = "year",
                             "month" = "month",
                             "day" = "day",
                             "hour" = "hour")) %>%
  group_by(dewp) %>%
  summarise(mean_del = mean(dep_delay, na.rm = T)) %>%
  ggplot(aes(x = mean_del, y = dewp)) +
  geom_point()

flights %>%
  inner_join(weather, by = c("origin" = "origin",
                             "year" = "year",
                             "month" = "month",
                             "day" = "day",
                             "hour" = "hour")) %>%
  group_by(wind_speed) %>%
  summarise(mean_del = mean(dep_delay, na.rm = T)) %>%
  ggplot(aes(x = mean_del, y = wind_speed)) +
  geom_point()

flights %>%
  inner_join(weather, by = c("origin" = "origin",
                             "year" = "year",
                             "month" = "month",
                             "day" = "day",
                             "hour" = "hour")) %>%
  group_by(precip) %>%
  summarise(mean_del = mean(dep_delay, na.rm = T)) %>%
  ggplot(aes(x = mean_del, y = precip)) +
  geom_point()

flights %>%
  inner_join(weather, by = c("origin" = "origin",
                             "year" = "year",
                             "month" = "month",
                             "day" = "day",
                             "hour" = "hour")) %>%
  group_by(pressure) %>%
  summarise(mean_del = mean(dep_delay, na.rm = T)) %>%
  ggplot(aes(x = mean_del, y = pressure)) +
  geom_point()

flights %>%
  inner_join(weather, by = c("origin" = "origin",
                             "year" = "year",
                             "month" = "month",
                             "day" = "day",
                             "hour" = "hour")) %>%
  group_by(visib) %>%
  summarise(mean_del = mean(dep_delay, na.rm = T)) %>%
  ggplot(aes(x = mean_del, y = visib)) +
  geom_point()

# 13.4.6.5
flights %>%
  filter(year == 2013, month == 6, day == 13) %>%
  group_by(dest) %>%
  summarise(mean_del = mean(arr_delay, na.rm = T)) %>%
  left_join(airports, c("dest" = "faa")) %>%
  ggplot(aes(lon, lat, color = mean_del)) +
  scale_color_gradient(low = "green", high = "red", breaks = c(-20, 0, 20, 40)) +  
  borders("state") +
  geom_point() +
  coord_quickmap()

# 13.5.1.1
flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(carrier, sort = T)

# 13.5.1.2
over_100_flights <- flights %>%
  group_by(tailnum) %>%
  count(tailnum, sort = T) %>%
  filter(n >= 100)
  
flights %>%
  semi_join(over_100_flights, by = "tailnum")
  
# 13.5.1.3
vehicles %>%
  semi_join(common, by = c("make", "model"))

# 13.5.1.4
flights %>%
  group_by(year, month, day, hour) %>%
  summarise(mean_del = mean(dep_delay, na.rm = T)) %>%
  arrange(desc(mean_del))

# 13.5.1.5
common_planes <- flights %>%
  group_by(tailnum) %>%
  summarise(n = n_distinct(carrier)) %>%
  filter(n != 1, !is.na(tailnum))

flights %>%
  semi_join(common_planes, by = "tailnum") %>%
  select(tailnum, carrier) %>%
  distinct()



