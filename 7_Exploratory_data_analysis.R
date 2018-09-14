setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Data")
pd <- read.csv("Panel.data.csv", as.is = T)
library(tidyverse)
library(nycflights13)

# Multiple histograms with linechart
ggplot(data = pd, mapping = aes(x = pd$OB, colour = pd$Profil_ad)) +
  geom_freqpoly(binwidth = 0.5) +
  coord_cartesian(xlim = c(6, 10)) # xlim fuction (not argument) works differently, subtracts values

diamonds %>%
  mutate(id = row_number()) %>%
  select(x, y, z, id) %>%
  gather(variable, value, -id)  %>% # zamienia kilka kolumn w pary wg key values, similar to melt
  ggplot(aes(x = value)) +
  geom_density() +
  geom_rug() + # adds a line on the bottom of the chart
  facet_grid(variable ~ .)

diamonds %>%
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 10) + 
  coord_cartesian(xlim = c(100, 5000), ylim = c(0, 3000))

diamonds %>%
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 10) + 
  xlim(300, 1000) +
  ylim(0, 3000)

ggplot(data = pd) + 
  geom_boxplot(aes(x = Profil_ad, y = OB))

ggplot(data = pd) + 
  geom_boxplot(aes(x = Aktywa, y = OB))

ggplot(data = diamonds) + 
  geom_boxplot(aes(x = cut, y = carat))

library(lvplot)

ggplot(data = diamonds) + 
  geom_lv(aes(x = cut, y = price))

pd %>%
  group_by(Profil) %>%
  summarise(m = mean(OB)) %>%
  ggplot() + 
  geom_bar(aes(x = Profil, y = m), stat = "identity")

diamonds %>%
  count(color, cut) %>%
  group_by(color) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop))

diamonds %>%
  count(color, cut) %>%
  group_by(cut) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop))

# Two cat. variables
flights %>%
  group_by(dest, month) %>%
  summarise(av.del = mean(arr_delay, na.rm = T)) %>%
  ggplot(mapping = aes(x = as.factor(month), y = dest)) + 
  geom_tile(aes(fill = av.del))

flights %>%
  group_by(month, dest) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  group_by(dest) %>% 
  filter(n() == 12) %>% # only airports with at least 12 flights per year
  ungroup() %>%
  mutate(dest = reorder(dest, dep_delay)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile()

# Two continuous variables
ggplot(data = diamonds) +
  geom_bin2d(mapping = aes(x = carat, y = price))
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + # cut one of the variables
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))  # or cut_number to cut by the amount of observations

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(color = cut_width(carat, 1)))

ggplot(data = diamonds, mapping = aes(x = carat)) +
  geom_freqpoly(mapping = aes(color = cut_width(price, 5000)))

# Three variables
ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_bin2d() + 
  facet_wrap(~ cut, ncol = 1)

ggplot(diamonds, aes(x = cut_number(carat, 5), y = price, colour = cut)) +
  geom_boxplot()

# Models

library(modelr)
mod <- lm(log(price) ~ log(carat), data = diamonds)
diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))









