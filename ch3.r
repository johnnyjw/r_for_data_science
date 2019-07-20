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

#2
?filter
filter(flights, between(month, 7, 9))

#3
filter(flights, is.na(dep_time))

#p50 arrange
arrange(flights, year, month, day)

arrange(flights, desc(arr_delay))

#sorting with na
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))

#exercises p 51
#1 missing values to top
arrange(df, desc(is.na(x)))

#2
arrange(flights, desc(arr_delay))
arrange(flights, dep_delay)

#3
flights$air_time
flights$distance
arrange(flights, desc(distance/air_time))

#4
arrange(flights, desc(air_time))
arrange(flights, air_time)

#p52 select
select(flights, year, month, day)

select(flights, year : day)

select(flights, -(year : day))

rename(flights, tail_num = tailnum)

#order
select(flights, time_hour, air_time, everything())

#exercises p54
#1
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, starts_with("arr"), starts_with("dep"))
select(flights, matches("^(arr|dep)"))
#2
select(flights, dep_time, year, dep_time)

#3
vars <- c(
  "year", "month", "day", "dep_delay", "arr_delay"
)

select(flights, one_of(vars))

#4
select(flights, contains("TIME"))
?contains
select(flights, contains("TIME", ignore.case = FALSE))

#p54 mutate
flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time
                      )

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
       )

#bottom var relies on upper two
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time /  60,
       gain_per_hour = gain / hours
)

#only keep new variables with transmute
transmute(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time /  60,
       gain_per_hour = gain / hours
)

#p56 modular attributes
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
          )

#offsets
(x <- 1:10)
lag(x)
lead(x)
#cumulative
cumsum(x)
cummean(x)
#rank 
y <- c(1, 2, 3, NA, 3, 4, 2)
min_rank(y)
min_rank(desc(y))
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)

#p58 exercises
#1
transmute(flights,
          dep_time,
          dep_time_mpm = (dep_time %/% 100)* 60 + dep_time %% 100)

#2
transmute(flights,
          air_time,
          ari_time_min = (air_time %/% 100)* 60 + air_time %% 100,
          dep_time,
          dep_time_mpm = (dep_time %/% 100)* 60 + dep_time %% 100,
          arr_time,
          arr_time_mpm = (arr_time %/% 100)* 60 + arr_time %% 100,
          dur = arr_time_mpm - dep_time_mpm)

transmute(flights,
          air_time,
          sched_dep_time,
          sched_arr_time)
