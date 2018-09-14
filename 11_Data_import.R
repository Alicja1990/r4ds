library(tidyverse)
library(readr)

# Parse number
parse_double("1,23", locale = locale(decimal_mark = ","))
parse_number("$100")
parse_number("It cost $123.45")
parse_number("$123,456,789")
parse_number("123.456.789", locale = locale(grouping_mark = "."))

# Encoding
x1 <- "Alicja Fraś"
guess_encoding(charToRaw(x1))
parse_date("1 lipca 2015", "%d %B %Y", locale = locale("pl"))

pl_locale <- locale(date_format = "%d.%m.%Y", "pl")
parse_date("1 lipca 2015", "%d %B %Y", locale = pl_locale)


d1 <- "January 1, 2010"
parse_date(d1, "%B %d, %Y")

d2 <- "2015-Mar-07"
parse_date(d2, "%Y-%b-%d")

d3 <- "06-Jun-2017"
parse_date(d3, "%d-%b-%Y")

d4 <- c("August 19 (2015)", "July 1 (2015)")
parse_date(d4, "%B %d (%Y)")

d5 <- "12/30/14" # Dec 30, 2014
parse_date(d5, "%m/%d/%y")

t1 <- "1705"
parse_time(t1, "%H%M")
t2 <- "11:15:10.12 PM"
parse_time(t2, "%H:%M:%OS %p")
