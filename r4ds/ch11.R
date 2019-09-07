library(tidyverse)
library(stringr)

string1 <- "This is a string"
string2 <- 'To put a "quote" inside a string use single quotes'

#escaping quotes
double_qoute <- "\""
single_quote <- '\''

#escaping backslash
x <- c("\"", "\\")
writeLines(x)
x
writeLines(double_qoute)

#nonenglish
x <- "\u00b5"
x

#character vector
c("one", "two", "three")

#length
str_length(c("a", "R for data science", NA))

#combine
str_c("x", "y")
str_c("x", "y", "z")
str_c("x", "y", sep=", ")

x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")
str_c("prefix-", c("a", "b", "c"), "-suffix")

name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY"
)

#collapsing
str_c(c("x", "y", "z"), collapse = ", ")


#substr
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)
str_sub("a", 1, 5)
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x
