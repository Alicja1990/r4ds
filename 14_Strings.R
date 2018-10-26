library(tidyverse)
library(stringr)
library(htmlwidgets)

# 14.3.1.1.2
x <- "\"'\\"
str_view(x, "\"'\\\\")

# 14.3.1.1.3
y <- "\\..\\..\\.."
x <- c(".a.a.a", "asdasfas.b.g.t", ".r.t.ykdjfhksdjhf")
str_view(x, y)

# 14.3.2.1.1
x <- "^\\$\\^\\$$"
y <- c("$^$", "ff$^$", "$%^$")
str_view(y, x)

# 14.3.2.1.2
x <- "^y"
str_view(words, x, match = T)
x <- "x$"
str_view(words, x, match = T)
x <- "^...$"
str_view(words, x, match = T)
x <- "......."
str_view(words, x, match = T)

# 14.3.3.1.1
x <- "^(a|e|i|o|u|y)"
  # albo 
x <- "^[aeiouy]"
str_view(words, x, match = T)
x <- "^[^aeiou]+$"
str_view(words, x, match = T)
x <- "[^e]ed$"
str_view(words, x, match = T)
x <- "i(ng|se)$"
str_view(words, x, match = T)

# 14.3.3.1.2
x <- "cie|[^c]ei"
y <- "cei|[^c]ie"
sum(str_detect(words, x))
sum(str_detect(words, y))

# 14.3.3.1.3
x <- "qu"
y <- "q[^u]"
sum(str_detect(words, x))
sum(str_detect(words, y))

# 14.3.3.1.4
x <- "ou|ise$|ae|oe|yse$"
str_view(words, x, match = T)

# 14.3.3.1.5
x <- "^\\+48\\s\\d{3}\\s\\d{3}\\s\\d{3}"
y <- "+48 504 541 063"
str_view(y, x, match = T)

# 14.3.4.1.3
x <- "^[^aeiouy]{3}"
str_view(words, x, match = T)
x <- "[aeiouy]{3,}"
str_view(words, x, match = T)
x <- "([aeiouy][^aeiouy]){2,}"

# 14.3.5.1.2
x <- "^(.).{0,}\\1$"
str_view(words, x, match = T)
x <- "(..).*\\1"
x <- "(.).*\\1.*\\1"

# 14.3.4.2.1
words[str_detect(words, "^x|x$")]
words[str_detect(words, "^[aeiouy].*[^aeiouy]$")]

a <- str_detect(words, "a")
e <- str_detect(words, "e")
i <- str_detect(words, "i")
o <- str_detect(words, "o")
u <- str_detect(words, "u")
words[a & e & i & o & u]

# 14.3.4.2.2
words[which(str_count(words, "[aeiou]") == max(str_count(words, "[aeiou]")))]
words[which(str_count(words, "[aeiou]")/str_length(words) == max(str_count(words, "[aeiou]")/str_length(words)))]

# 14.4.3.1.1
colours <- c("( red", "orange", "yellow", "green", "blue", "purple)")
colour_match <- str_c(colours, collapse = ")|( ")
has_colour <- str_subset(sentences, colour_match)
matches <- str_extract(has_colour, colour_match)
more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)

# 14.4.3.1.2
first_word <- str_extract(sentences, "^[a-zA-Z]+")

ing_ending <- str_extract(sentences, "\\b[a-zA-Z]*ing\\b")
ing_ending <- unique(ing_ending[!is.na(ing_ending)])

plurals <- str_extract(sentences, "\\b[a-zA-Z]{3,}s\\b")
plurals <- unique(plurals[!is.na(plurals)])

# 14.4.4.1.1
numbers <- c(" one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
numbers <- str_c(numbers, collapse = "| ")
numbers <- str_c("(", numbers, ") ([^ ]+)")
has_number <- str_subset(sentences, numbers)
numbers_extracted <- str_extract(has_number, numbers)

# 14.4.4.1.2
has_contraction <- str_extract(sentences, "([a-zA-Z]+)'([a-zA-Z]+)")
has_contraction <- has_contraction[!is.na(has_contraction)]

# 14.4.5.1.1
x <- c("ksdjha/", "//sdfjdhk", "df/dfsd")
str_replace_all(x, c("\\/" = "\\\\"))

# 14.4.5.1.3
swapped <- str_replace_all(words, "^(.)(.*)(.)$", "\\3\\2\\1")
intersect(swapped, words)

# 14.4.6.1.1
x <- "apples, pears, and bananas"
str_split(x, ", (and )?")

# 14.4.6.1.2
x <- "This is a sentence.  This is another sentence."
str_split(x, boundary("word"))
str_split(x, " ")

# 14.5.1.1
x <- c("ksdjha\\", "\\\\sdfjdhk", "df\\dfsd", "abc")
str_subset(x, "\\\\")
str_subset(x, fixed("\\"))

# 14.5.1.2
str_extract_all(sentences, boundary("word")) %>%
  unlist() %>%
  str_to_lower() %>%
  tibble() %>%
  set_names("word") %>%
  group_by(word) %>%
  count(sort = T)