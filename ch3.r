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

#3
transmute(flights,
          dep_time
          dep_time_mpm = (dep_time %/% 100)* 60 + dep_time %% 100,
          sched_dep_time,
          sched_dep_time_mpm = (sched_dep_time %/% 100)* 60 + sched_dep_time %% 100,
          dep_delay,
          calc_dep_delay = dep_time_mpm - sched_dep_time_mpm)

#4
transmute(flights,
          year,
          month,
          day,
          carrier,
          flight,
          arr_delay,
          delay_rank = min_rank(desc(arr_delay))) %>%
  arrange(delay_rank)

#5
1:3 + 1:10

1:10 + 21:30

#p59 group summaries
summarize(flights, delay = mean(dep_delay, na.rm = TRUE))

#grouped
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay =mean(dep_delay, na.rm = TRUE))

#using pipe
by_dest <- group_by(flights, dest)
delay <- summarize(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
                   )
delay <- filter(delay, count > 20, dest != "HNL")

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

#doing same thing with a pipe
delays <- flights %>%
  group_by(dest) %>%
  summarize(count = n(),
            dist = mean(distance, na.rm = TRUE),
            delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL")

#61 not using na.rm - will apply na rules and you get a lot of na's!
flights %>%
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay))

flights %>%
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay, na.rm = TRUE))

#can filter out the cancelled flights first
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay))

#63 planes example
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

#scatterplotting number of flights vs average delay
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

#filter out low nums and incororate ggplot2 into deplyr flows
delays %>%
  filter(n > 25) %>%
  ggplot(mapping = aes(x = n, y = delay)) +
    geom_point(alpha = 1/10)

#p65 another example of a n vs variation, this time batting scores
batting <- as_tibble(Lahman::Batting)

batters <- batting %>%
  group_by(playerID) %>%
  summarize(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>%
  filter(ab > 100)%>%
  ggplot(mapping = aes(x = ab, y = ba)) +
    geom_point() +
    geom_smooth(se = FALSE)

#if you sort on batting average without filtering small at bat numbers
#then you just get flukies
batters %>%
  arrange(desc(ba))

#aggregating using a logical subset (arr_delay[arr_delay > 0])
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    #average delay
    avg_delay = mean(arr_delay),
    #av positive delay:
    avg_delay2 = mean(arr_delay[arr_delay > 0])
  )

#p67 - spread
not_cancelled %>%
  group_by(dest) %>%
  summarize(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))

#68 quantiles
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    first = min(dep_time),
    last = max(dep_time)
  )

#measures of position
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
  )

#if you filter on ranks you get all the variables and first and last on separate rows
not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%
  filter(r %in% range(r))

#counts
not_cancelled %>%
  group_by(dest) %>%
  summarize(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))

# dplyr's count 
not_cancelled %>%
  count(dest)

# with a weight
not_cancelled %>%
  count(tailnum, wt=distance)

#counts and proportions of lovigal values
#how many flights left before 5am?  (departures delayed from previous day)
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(n_early = sum(dep_time < 500))

# proportion of flights delayed more than an hour
not_cancelled %>%
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))

#multiple variables
daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year <- summarise(per_month, flights = sum(flights)))

#p 72 ungrouping
daily %>%
  ungroup() %>%
  summarize(flights = n())

#exercises p 72
flights
#1
#average
not_cancelled %>% 
  group_by(carrier, flight) %>% 
  summarise(av_delay = mean(dep_delay),
            n = n()) %>%
  filter(n > 3) %>%
  arrange(desc(av_delay))
#median
not_cancelled %>% 
  group_by(carrier, flight) %>% 
  summarise(av_delay = median(dep_delay),
            n = n()) %>%
  filter(n > 3) %>%
  arrange(desc(av_delay))
#proportion
not_cancelled %>% 
  group_by(carrier, flight) %>% 
  summarise(over_five = mean(dep_delay>5),
            n = n()) %>%
  filter(n > 3) %>%
  arrange(desc(over_five))
#greatest variation in lateness
not_cancelled %>% 
  group_by(carrier, flight) %>% 
  summarise(min_delay = min(dep_delay),
            max_delay = max(dep_delay),
            n = n()) %>%
  mutate(del_range = max_delay-min_delay) %>%
  filter(n > 3) %>%
  arrange(desc(del_range))

#always late
not_cancelled %>% 
  group_by(carrier, flight) %>% 
  summarise(min_delay = min(dep_delay),
            n = n()) %>%
  filter(n > 3,
         min_delay > 5) %>%
  arrange(desc(min_delay))

#2
not_cancelled %>% 
  count(dest)

not_cancelled %>% 
  group_by(dest) %>% 
  summarize(n = n())

not_cancelled %>% 
  count(tailnum, wt = distance)

not_cancelled %>% 
  group_by(tailnum) %>% 
  summarize(n = sum(distance))

#4 support cancelled flights
cancelled <- flights %>%
  filter(is.na(dep_delay) | is.na(arr_delay))

cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(av_delay = mean(dep_delay)) %>% 
  arrange(desc(av_delay))

#5
not_cancelled %>%
  group_by(carrier) %>% 
  summarize(av_delay = mean(dep_delay)) %>% 
  arrange(desc(av_delay))

not_cancelled %>%
  group_by(carrier, dest) %>% 
  summarize(av_delay = mean(dep_delay),
            n = n()) %>% 
  arrange(desc(av_delay))

#6
not_cancelled %>%
  group_by(tailnum) %>% 
  arrange(tailnum, year, month, day) %>% 
  mutate(cumdelay = cumsum(arr_delay>60)) %>% 
  summarize(nodelay = sum(ifelse(cumdelay==0, 1, 0)),
         n = n()) %>%
  arrange(desc(nodelay))

cummy$cumdelay

#7
?count

#p73 grouped mutates
flights_sml %>% 
  group_by(year, month, day) %>% 
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

#p75 exercises
vignette("window-functions")

?lag
a <- 1:10
b <- c('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b')
c <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
gg <- tibble(a=c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
             b=c('a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b'),
             c=1:10)
gg %>% 
  group_by(b) %>% 
  mutate(d = lag(c),
         e = cumsum(c),
         f = min_rank(c))
#2 plane with worst on time record
not_cancelled %>% 
  group_by(tailnum) %>% 
  summarize(late_prop = mean(arr_delay > 5),
            n = n()) %>% 
  filter(n>10) %>% 
  mutate(late_rank = min_rank(desc(late_prop))) %>% 
  arrange(late_rank)

#3 dep delay by time
not_cancelled %>% 
  group_by(sched_dep_time) %>% 
  summarize(late_prop = mean(dep_delay > 5),
            n = n()) %>%
  filter(n>10) %>% 
  mutate(late_rank = min_rank(late_prop)) %>% 
  arrange(late_rank)

#4
not_cancelled %>%
  group_by(dest) %>% 
  mutate(act_delay = ifelse(arr_delay<0,0,arr_delay),
         tot_delay = sum(act_delay),
         prop_delay = arr_delay/tot_delay) %>% 
  select(year, month, day, flight, dest, arr_delay, tot_delay, prop_delay) %>% 
  arrange(desc(prop_delay))

#5 delay
not_cancelled %>% 
  arrange(year, month, day, dep_time) %>% 
  transmute(year, month, day, dep_time,
            dep_delay,
            prev_dep = lag(dep_delay)) %>% 
  arrange(desc(dep_delay)) %>% 
  filter(dep_delay < 120)

# 6
colnames(not_cancelled)
not_cancelled %>% 
  group_by(dest) %>% 
  transmute(air_time,
            mean_air = mean(air_time),
            big_gap = mean_air - air_time) %>%
  filter(air_time < 100)
  arrange(desc(big_gap))

# 7
not_cancelled %>% 
  group_by(dest, carrier) %>% 
  summarize(mean_del = mean(arr_delay)) %>%
  mutate(n_carr = n()) %>% 
  filter(n_carr>1) %>% 
  arrange(dest, mean_del)
