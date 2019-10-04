library(tidyverse)
library(lubridate)
library(nycflights13)

today()
now()

#inputting from strings
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")

#input unquoted numbers
ymd(20170131)

#date time inputs
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")

#force dt by adding timezone
ymd(20170131, tz="UTC")

#from columns
flights %>% 
  select(year, month, day, hour, minute)

flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time){
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(
      year, month, day, sched_dep_time
    ),
    sched_arr_time = make_datetime_100(
      year, month, day, sched_arr_time
    )
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt

flights_dt %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400)  #86400 seconds in a day

flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 600) #600s=6 min

#converting date to datetime and vice versa
as_datetime(today())

as_date(now())

###unix offset (time from 1 jan 1970)
as_datetime(60 * 60 * 10)
as_date(365 * 10 + 2)

#p 243 exercises
#1
ymd(c("2010-10-10", "bananas"))
#2
?today
#3
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" #dec 30, 2014

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)

#getting components
datetime <- ymd_hms("2016-07-08 12:34:56")

year(datetime)
month(datetime)
mday(datetime)

yday(datetime)
wday(datetime)

###returning
month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = FALSE)

flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

#strange delay pattern vs time past hour
flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(
    minute, avg_delay)) +
    geom_line()
  
sched_dep <- flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n())

ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()
###a bias for simple times affects actual departure times

ggplot(sched_dep, aes(minute, n)) +
  geom_line()

#rounding
flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
    geom_line()

#modifying date time
(datetime <- ymd_hms("2016-07-08 12:34:56"))

year(datetime) <- 2020
datetime

month(datetime) <- 1
datetime

hour(datetime) <- hour(datetime) + 1
datetime

update(datetime, year = 2020, month = 2, mday = 2, hour = 2)

ymd("2015-02-01") %>% 
  update(mday = 30)

ymd("2015-02-01") %>% 
  update(hour = 400)

flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  ggplot(aes(dep_hour)) +
    geom_freqpoly(binwidth = 300)

##exs p 248
#1
flights_dt %>% 
  mutate(hour = hour(dep_time),
         month = factor(month(dep_time))) %>%
  group_by(month, hour) %>% 
  summarize(n = n()) %>%  
  ggplot(aes(hour, n)) +
    geom_smooth(aes(colour=month, linetype=month),
                se = FALSE)


flights_dt %>% 
  mutate(hour = hour(dep_time),
         month = month(dep_time)) %>% 
  group_by(month) %>% 
  summarize(count = n())

#2
flights_dt %>% 
  mutate(calc_dep_delay = as.integer(dep_time - sched_dep_time)/60,
         diff = abs(dep_delay - calc_dep_delay)) %>%
  summarize(diff = mean(diff))

flights_dt %>% 
  mutate(calc_dep_delay = as.integer(dep_time - sched_dep_time)/60,
         diff = abs(dep_delay - calc_dep_delay)) %>%
  filter(diff > 0)

## Not consistent - delays into the next day not properly represented

#3
flights_dt %>% 
  mutate(calc_air_time = as.integer(arr_time - dep_time),
         diff = abs(air_time - calc_air_time)) %>%
  filter(! is.na(diff)) %>% 
  group_by(dest) %>% 
  summarize(diff = mean(diff))


flights_dt %>% 
  mutate(calc_air_time = as.integer(arr_time - dep_time),
         sd = as.integer(sched_arr_time - sched_dep_time),
         diff = abs(air_time - calc_air_time)) %>%
  filter(! is.na(diff)) %>% 
  select(dep_time, arr_time, air_time, calc_air_time, diff, sd)
#not a close relationship between airtime and calculated airtime

flights_dt %>% 
  mutate(calc_air_time = as.integer(arr_time - dep_time),
         diff = abs(air_time - calc_air_time)) %>%
  filter(! is.na(diff)) %>%
  ggplot(aes(air_time, calc_air_time)) +
  geom_point()

#4
#should use sched dep time for delay over time of day as the delay is based on this
flights_dt %>% 
  mutate(hour = hour(sched_dep_time)) %>% 
  ggplot(aes(hour, dep_delay)) +
  geom_bar(stat = "summary", fun.y = "mean")

#5
flights_dt %>% 
  mutate(wkday = wday(sched_dep_time, label=TRUE)) %>% 
  ggplot(aes(wkday, dep_delay)) +
  geom_bar(stat = "summary", fun.y = "mean")
#sat best day

#6
diamonds %>% 
  ggplot(aes(carat)) +
  geom_bar()

flights_dt %>% 
  mutate(mins = minute(sched_dep_time)) %>% 
  ggplot(aes(mins)) +
  geom_bar()

# both are focussed on particular whole numbers