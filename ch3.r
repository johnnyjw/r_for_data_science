library(tidyverse)
library(nycflights13)

#a tibble
flights


#testing dplyr functions p46
filter(flights, month == 1, day == 1)

(dec25 <- filter(flights, month == 12, day == 25))

#47 tricks
#remember ==
filter(flights, month = 1)

#floating points errors
sqrt(2) ^ 2 == 2

1/49*49 == 1

#use near
near(sqrt(2) ^ 2, 2)
near(1/49*49, 1)

#combining
filter(flights, month == 11 | month == 12)
#or
nov_dec <- filter(flights, month %in% c(11, 12))

#p48 don't forget demorgans law
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

#p48 na can be confusing
NA > 5
10 == NA
NA + 10
NA / 2
NA == NA

#use is.na
x <- NA
is.na(x)

df <- tibble(x = c(1, NA, 3))
filter(df, x>1)

filter(df, is.na(x) | x > 1)


#p49 exs
#1
filter(flights, dep_delay >= 120)
filter(flights, dest %in% c('IAH', 'HOU'))
unique(flights$carrier)
filter(flights, carrier %in% c('UA', 'AA', 'DL'))

filter(flights, month %in% c(7, 8, 9))
filter(flights, dep_delay < 5, arr_delay >= 120)

filter(flights, dep_delay >= 60, arr_delay - dep_delay <= -30)
filter(flights, dep_time < 600)
