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

#7
flights_dt %>% 
  mutate(dep_min = as.numeric(minute(dep_time)),
         sched_dep_min = as.numeric(minute(sched_dep_time))) %>% 
  filter((dep_min<30 & dep_min>=20) |
           (dep_min<=59 & dep_min>=50)) %>% 
  filter(dep_delay<0) %>% 
  ggplot(aes(sched_dep_min)) +
  geom_histogram()
  
#p249 durations
#how old is hadley?
h_age <- today() - ymd(19791014)
h_age

#convert to lubridate duration
as.duration(h_age)

dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(1)
dyears(1)

#add multiply etc
2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)

#durations in calculations
tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)
tomorrow
last_year

####where this falls down
one_pm <- ymd_hms(
  "2016-03-12 13:00:00",
  tz = "America/New_York"
)
one_pm
one_pm + ddays(1)

###using periods instead
one_pm
one_pm + days(1)

#making periods
seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1)


#operations with periods
10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)

#adding periods to times
ymd("2016-01-01") + dyears(1)
ymd("2016-01-01") + years(1)

####fix after midnight
flights_dt %>% 
  filter(arr_time < dep_time) %>% 
  select(arr_time, dep_time)

flights_dt <- flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  )

flights_dt %>% 
  filter(overnight, arr_time < dep_time)

#Intervals
years(1) / days(1)

next_year <- today() + years(1)
next_year

(today() %--% next_year) / ddays(1)

(today() %--% next_year) / days(1)

#exs p 253
#3
base <-  ymd("2015-01-01")
month_vec <- base %>%
  update(month = 1:12)
month_vec

base2 <-  today()
month_vec2 <- base2 %>%
  update(mday = 1, month = 1:12)
month_vec2

#4 birthday function
birth <- function(bdate){
  as.numeric(today() - bdate) %/% 365
}

birth(ymd("1972-07-26"))

#5
(today()  %--% (today() + years(1)))/months(1)


##timezones
Sys.timezone()

length(OlsonNames())
head(OlsonNames())

#timezones
(x1 <- ymd_hms("2015-06-01 12:00:00", tz="America/New_York"))
(x2 <- ymd_hms("2015-06-01 18:00:00", tz="Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz="Pacific/Auckland"))
x1-x2
x1-x3

x4 <- c(x1, x2, x3)
x4

#change timezone displayed but NOT the point in time
x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a
x4a-x4

#keep the base number the same but change the point in time
x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b
x4b - x4
