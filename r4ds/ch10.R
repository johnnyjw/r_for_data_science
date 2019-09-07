library(tidyverse)
library(nycflights13)
library(lubridate)

airlines
airports
planes
weather

#testing that a primary key actually refers to unique obs
planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)

#something with no primary key (although the above didnt have one either)
flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)

#exs p177
#1 surrogate key
flights %>% mutate(key_row = row_number()) %>% 
  count(year, month, day, flight, key_row) %>% 
  filter(n > 1)

#2a
head(Lahman::Batting)

Lahman::Batting %>% 
  count(playerID, yearID, stint) %>% 
  filter(n > 1)

#2b 
library(babynames)
head(babynames::babynames)

babynames::babynames %>% 
  count(year, sex, name) %>% 
  filter(nn > 1)

#2c
head(nasaweather::atmos)

nasaweather::atmos %>% 
  count(lat, long, year, month) %>% 
  filter(n > 1)

#2d
head(fueleconomy::vehicles)

fueleconomy::vehicles %>% 
  count(id) %>% 
  filter(n > 1)

#2e
head(ggplot2::diamonds)

ggplot2::diamonds %>% 
  count(x, y, z, carat, clarity, cut, price, color, depth, table) %>% 
  filter(n>1)

#2e no key....do the rownym

#3
head(Lahman::Batting)
head(Lahman::Master)
head(Lahman::Salaries)
head(Lahman::Fielding)
head(Lahman::Pitching)


#mutating joins
flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

flights2 %>% 
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")

#result similar to a matching mutate
flights2 %>% select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

#demonstration of joins
x <- tribble(
  ~key,   ~val_x,
  1,       "x1",
  2,       "x2",
  3,       "x3")


y <- tribble(
  ~key, ~val_y,
  1,    "y1",
  2,    "y2",
  4,    "y3"
)

##joins
x %>% 
  inner_join(y, by = "key")

x %>% 
  left_join(y, by = "key")

x %>% 
  right_join(y, by = "key")

x %>% 
  full_join(y, by = "key")


#duplicate keys
x <- tribble(
  ~key,   ~val_x,
  1,       "x1",
  2,       "x2",
  2,       "x3",
  1,       "x4")

y <- tribble(
  ~key, ~val_y,
  1,    "y1",
  2,    "y2"
)

left_join(x, y, by = "key")

#many to many
x <- tribble(
  ~key,   ~val_x,
  1,       "x1",
  2,       "x2",
  2,       "x3",
  3,       "x4")

y <- tribble(
  ~key, ~val_y,
  1,    "y1",
  2,    "y2",
  2,    "y3",
  3,    "y4"
)

left_join(x, y, by = "key")

####demonstrating by argument
## null
flights2 %>% 
  left_join(weather)
##by charactor vector
flights2 %>% 
  left_join(planes, by = "tailnum")

##named character vector
flights2 %>%
  left_join(airports, c("dest" = "faa"))

flights2 %>%
  left_join(airports, c("origin" = "faa"))

###p186 exercises
#1
flights_ex <- flights %>%
  group_by(dest) %>% 
  summarize(delay = mean(arr_delay, na.rm = TRUE))

airports %>% 
  inner_join(flights, c("faa" = "dest")) %>% 
  group_by(lon, lat) %>% 
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(delay > 0) %>% 
  ggplot(aes(lon, lat, colour=delay )) +
    borders("state") +
    geom_point() +
    coord_quickmap()

airports %>% 
  semi_join(flights_ex, c("faa" = "dest"))

#2
airports2 <- airports %>%
  select(faa, lat, lon)

flights_data <- flights2 %>% 
  left_join(airports2, by=c("origin" = "faa")) %>% 
  rename(origin.lat=lat, orign.lon=lon) %>% 
  left_join(airports2, by=c("dest" = "faa")) %>% 
  rename(dest.lat=lat, dest.lon=lon)

#3 
flights %>% 
  group_by(tailnum) %>% 
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>% 
  left_join(planes) %>% 
  ggplot(aes(year, delay)) +
    geom_point()

#4
colnames(weather)

flights %>% 
  group_by(origin, year, month, day, hour) %>% 
  summarize(delay = mean(dep_delay)) %>% 
  left_join(weather) %>% 
  ggplot(aes(temp, delay)) +
    geom_boxplot(aes(group = cut_number(temp, 20)))

flights %>% 
  group_by(origin, year, month, day, hour) %>% 
  summarize(delay = mean(dep_delay)) %>% 
  left_join(weather) %>% 
  ggplot(aes(precip, delay)) +
  geom_point()

flights %>% 
  group_by(origin, year, month, day, hour) %>% 
  summarize(delay = mean(dep_delay)) %>% 
  left_join(weather) %>% 
  ggplot(aes(visib, delay)) +
    geom_boxplot(aes(group = cut_width(visib, 3)))

flights %>% 
  group_by(origin, year, month, day, hour) %>% 
  summarize(delay = mean(dep_delay)) %>% 
  left_join(weather) %>% 
  ggplot(aes(wind_speed, delay)) +
  geom_boxplot(aes(group = cut_number(wind_speed, 10)))

#5 
flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  filter(date >= "2013-06-01" & date <= "2013-06-30") %>% 
  group_by(date) %>% 
  summarize(delay = mean(dep_delay, na.rm=TRUE)) %>% 
  ggplot(aes(date, delay)) +
    geom_point()
###storms on 13 June 2013

#188 filtering joins
top_dest <- flights %>% 
  count(dest, sort = TRUE) %>% 
  head(10)

top_dest

flights %>% 
  filter(dest %in% top_dest$dest)

#but simpler way
flights %>% 
  semi_join(top_dest)

###anti join
flights %>% 
  anti_join(planes, by = "tailnum") %>% 
  count(tailnum, sort = TRUE)

#p191 exercises
#1
flights %>%
  filter(is.na(tailnum))

flights %>%
  filter(is.na(tailnum) & !(is.na(sched_dep_time)))

no_match <- flights %>%
  filter(!(is.na(tailnum))) %>% 
  anti_join(planes, by="tailnum") %>% 
  group_by(tailnum) %>% 
  summarise(count = n()) %>% 
  arrange(count)
  
#2
hundred <- flights %>% 
  group_by(tailnum) %>% 
  summarize(n = n()) %>% 
  filter(n >= 100)

flights %>%
  semi_join(hundred, by="tailnum")


#3
top20 <- fueleconomy::common %>% 
  arrange(desc(n)) %>% 
  head(20)

fueleconomy::vehicles %>% 
  semi_join(top20, by=c('make', 'model'))

#4 choosing the worst 48 hours
worst <- flights %>% 
  mutate(hour = sched_dep_time %/% 100) %>% 
  group_by(year, month, day, hour) %>% 
  summarize(av_del = mean(dep_delay)) %>% 
  arrange(desc(av_del)) %>% 
  head(48)

slowies <- weather %>% 
  semi_join(worst, by=c('year', 'month', 'day', 'hour'))

best <- flights %>% 
  mutate(hour = sched_dep_time %/% 100) %>% 
  group_by(year, month, day, hour) %>% 
  summarize(av_del = mean(dep_delay)) %>% 
  arrange(av_del) %>% 
  head(48)

fasties <- weather %>% 
  semi_join(best, by=c('year', 'month', 'day', 'hour'))

###some relationship with windspeed, humidity, precipitation, gusts

#5
anti_join(flights, airports, by  = c("dest" = "faa"))
###this shows flights for which the destination is not in the airports list
anti_join(airports, flights, by = c("faa" = "dest"))
####this shows arports on the list who are not used as a destination

#6 is there a relationship between tailnum and airline - is each plane by a single airline
dual <- flights %>% 
  select(tailnum, carrier) %>% 
  unique() %>% 
  group_by(tailnum) %>% 
  mutate(carrier_ct = n()) %>% 
  ungroup() %>% 
  filter(carrier_ct > 1) %>% 
  arrange(tailnum, carrier) %>% 
  left_join(airlines)

####it is in general true that there is a relationship between plane and airline, but there are some examples of 
### more than one airline that flies with the same plane


###p192 set operations
df1 <- tribble(
  ~x,   ~y,
  1, 1,
  2, 1
)

df2 <- tribble(
  ~x,  ~y,
  1,   1,
  1,  2
  )

dplyr::intersect(df1, df2)
dplyr::union(df1, df2)

dplyr::setdiff(df1, df2)
dplyr::setdiff(df2, df1)
