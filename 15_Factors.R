library(tidyverse)
library(forcats)

# 15.3.1.1
ggplot(gss_cat, aes(rincome)) +
  geom_bar() +
  coord_flip()
#  theme(axis.text.x = element_text(angle = 90))

# 15.3.1.2
gss_cat %>%
  group_by(relig) %>%
  count(sort = T)
gss_cat %>%
  group_by(partyid) %>%
  count(sort = T)

# 15.3.1.3
gss_cat %>%
  filter(!denom %in% c("No answer", "Other", "Don't know", "Not applicable",
                       "No denomination")) %>%
  group_by(relig) %>%
  count(sort = T)
  
gss_cat %>%
  count(relig, denom) %>%
  ggplot(aes(x = relig, y = denom, size = n)) +
  geom_point()



ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

# 15.4.1.1
ggplot(gss_cat, aes(tvhours)) +
  geom_bar()

# 15.4.1.2
levels(gss_cat$marital)
levels(gss_cat$race)
levels(gss_cat$rincome)
levels(gss_cat$partyid)
levels(gss_cat$relig)
levels(gss_cat$marital)


gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
  ))
gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  ))
gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n = Inf)

# 15.5.1.1
gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                Other = c("No answer", "Don't know", "Other party"),
                                Republican = c("Strong republican", "Not str republican"),
                                Independent = c("Ind,near rep", "Independent", "Ind,near dem"),
                                Democrat = c("Not str democrat", "Strong democrat")
  )) %>%
  group_by(year) %>%
  mutate(year_s = n()) %>%
  select(year, partyid, year_s) %>%
  group_by(year, partyid) %>%
  mutate(voters_s = n()) %>%
  unique() %>%
  mutate(prop = voters_s / year_s) %>%
  ggplot() +
    geom_line(mapping = aes(x = year, y = prop, color = partyid))

# 15.5.1.2

gss_cat %>%
  mutate(rincome = fct_collapse(rincome,
                                'Unknown' = c("No answer", "Don't know", "Refused", "Not applicable"),
                                'Lt 5000' = c("$5000 to 5999", "$4000 to 4999", "$3000 to 3999", "$1000 to 2999", "Lt $1000"),
                                '5000-10000' = c("$10000 - 14999", "$8000 to 9999", "$7000 to 7999", "$6000 to 6999"),
                                'Above 10000' = c("$25000 or more", "$20000 - 24999", "$15000 - 19999", "$10000 - 14999")
  ))













  