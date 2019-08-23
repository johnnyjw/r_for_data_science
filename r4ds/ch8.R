library(tidyverse)

#read in file
read_csv("heights.csv")

#reading inline text
read_csv("a, b, c
         1, 2, 3
         4, 5, 6")

#using skip
read_csv("a line of metadata
another line of metadata
a, b, c
         1, 2, 3
         4, 5, 6", skip = 2)


#skipping rows that start with a particular character
read_csv("# dont include this row
a, b, c
         1, 2, 3
         4, 5, 6", comment = '#')

#ignoring column headers
read_csv("1,2,3\n4,5,6", col_names = FALSE)

#add col names as separate vector
read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))

#specify value that represents missing
read_csv("a,b,c\n1,2,.", na = ".")

#exercises p128
#1
read_delim("a|b|c\n1|2|3", delim="|")
#2
?read_csv
#3
?read_fwf
#4
read_delim("x,y\n1,'a,b'", delim=',', quote = "\'")
#5
#wrong number of column names
read_csv("a,b\n1,2,3\n4,5,6")
#one row with too few cols one row with too many
read_csv("a,b,c\n1,2\n1,2,3,4")
#misplaced quote
read_csv("a,b\n\"1")
#na and \n confused
read_csv("a,b\n1,2\na,b")
#semicolon delimiter when should be comma
read_csv("a;b\n1;3")


#p129 parse
str(parse_logical(c("TRUE", "FALSE", "NA")))
str(parse_integer(c("1", "2", "3")))
str(parse_date(c("2010-01-01", "1979-10-14")))

parse_integer(c("1", "231", ".", "456"), na = ".")

#problems
x <- parse_integer(c("123", "345", "abc", "123.45"))
x
problems(x)

#parsing through stuff
parse_number("$100")
parse_number("20%")
parse_number("It costs $123.45")

#location specific
parse_number("$123,456,789")
parse_number(
  "123.456.789",
  locale = locale(grouping_mark = ".")
)
parse_number(
  "123'456'789",
  locale = locale(grouping_mark = "'")
)

#strings
#how a string is represented = each hexadecimal number a byte of info
#mapping of hex to character is encoding
charToRaw("Hadley")

#encoding examples
x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"

parse_character(x1,locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))

#guessing encoding
guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))

#p134 Factors
fruit <- c("apple", "banana")

parse_factor(c("apple", "banana", "bananana"), levels = fruit)

#p134 date parse
parse_datetime("2010-10-01T2010")
#no time, then set to midnight
parse_datetime("20101010")

parse_date("2010-10-01")

library(hms)
parse_time("01:10 am")
parse_time("20:10:01")

#formats
parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")

#formats plus locale
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))

#p136 exercises
#1 
?parse_date
#2
parse_number("1,890,345", locale=locale(decimal_mark = ',', grouping_mark = ','))

parse_number("1.890,345", locale=locale(decimal_mark = ','))
parse_number("1'890,345", locale=locale(decimal_mark = ','))

parse_number("1.890,345", locale=locale(grouping_mark = '.'))

#3
johnny <- locale(date_format = "%d/%m/%y")
parse_date("26/07/72", locale=johnny)

#4
jw_uk <- locale(date_format="%d/%m/%y", tz = "GMT")
parse_time('3:00', locale=jw_uk)

#5
?read_csv

#7
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" #dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"

parse_date(d1, "%B %d, %Y")
parse_date(d2, "%Y-%b-%d")
parse_date(d3, "%d-%b-%Y")
parse_date(d4, "%B %d (%Y)")
parse_date(d5, "%m/%d/%y")
parse_time(t1, "%H%M")
parse_time(t2, "%I:%M:%OS %p")

#p138 demonstrate heuristics of readr
guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TRUE", "FALSE"))
guess_parser(c("1", "5", "9"))
guess_parser(c("12,352,561"))

str(parse_guess("2010-10-10"))

#challenging example
challenge <- read_csv(readr_example("challenge.csv"))
problems(challenge)


#so use that specification to read in 
challenge <- read_csv(
  readr_example("challenge.csv"),
    col_types = cols(
      x = col_integer(),
      y = col_character()
  )
)

#then fix...
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_character()
  )
)

#but look ...
tail(challenge)
#actually dates!
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)
tail(challenge)

#change guess
challenge <- read_csv(
  readr_example("challenge.csv"),
  guess_max = 1001
)

#read in all as character
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(.default = col_character())
)

#type_convert
df <- tribble(
  ~x, ~y,
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
  )

df

type_convert(df)

#write
write_csv(challenge, "challenge.csv")
challenge
write_csv(challenge, "challenge-2.csv")
read_csv("challenge-2.csv")

#rds version
write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")
#feather
library(feather)
write_feather(challenge, "challenge.feather")
read_feather("challenge.feather")
