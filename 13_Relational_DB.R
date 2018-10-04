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



flights %>%
  group_by(dest) %>%
  summarise(mean_del = mean(arr_delay, na.rm = T)) %>%
  left_join(airports, c("dest" = "faa")) %>%
  ggplot(aes(lon, lat, color = mean_del)) +
  scale_color_gradient(low = "green", high = "red") +  
  borders("state") +
  geom_point() +
  coord_quickmap()

flights %>%
  left_join(airports, by = c())