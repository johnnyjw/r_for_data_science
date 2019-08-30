library(tidyverse)
library(nycflights13)

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