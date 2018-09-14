library(tidyverse)

cases <- table2 %>%
  filter(type == "cases")
  
popul <- table2 %>%
  filter(type == "population")

popul$rate <- cases$count/popul$count * 10000  

table4c <- table4a %>%
  mutate(a = `1999` / table4b$`1999` * 10000) %>%
  mutate(b = `2000` / table4b$`2000` * 10000) %>%
  select(-`1999`, -`2000`) %>%
  rename("1999" = a, "2000" = b)

table2 %>% 
  filter(type == "cases") %>%
  ggplot(aes(x = year, y = count)) +
    geom_line(aes(color = country))

people %>%
  mutate(pers_id = c(1, 1, 2, 3, 3)) %>%
  spread(key, value)

table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)

table5 %>% 
  unite(new, century, year)

stocks %>% 
  complete(year, qtr) # complete() finds all unique combinations of the column(s`) values

fill() # takes a column and fills all missing values with the most recent value (or next by "direction" argument)

who %>% 
  group_by(country, year, sex) %>%
  filter(year > 1995) %>%
  summarise(cases = sum(value)) %>%
  ggplot(aes(x = year, y = cases)) +
    geom_line(aes(colour = sex))
  