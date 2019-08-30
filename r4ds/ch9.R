library(tidyverse)

#tidy data - table 1 is tidy
table1
table2
table3
table4a
table4b

#easily working with tidy data
table1 %>% 
  mutate(rate = cases / population * 10000)

table1 %>% 
  count(year, wt =cases)

library(ggplot2)
ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country))

#exs p 151
#2
#rate for table 2
table2rate <- table2 %>%
  filter(type == 'population') %>% 
  select(country, year,
         population = count)

tb2cas <- table2 %>% 
  filter(type == 'cases')
tb2casVec <- tb2cas$count

table2rate <- table2rate %>% 
  mutate(cases = tb2casVec,
         rate = cases / population * 10000)

#rate for tables 4a 4b
caseDate <- table4a %>% 
  select(country,
         cases = `1999`) %>% 
  mutate(year = 1999) %>% 
  union_all(
    table4a %>% 
      select(country,
             cases = `2000`) %>% 
      mutate(year = 2000)
  )

popDate <- table4b %>% 
  select(country,
         population = `1999`) %>% 
  mutate(year = 1999) %>% 
  union_all(
    table4b %>% 
      select(country,
             population = `2000`) %>% 
      mutate(year = 2000)
  )

comb <- caseDate %>% 
  inner_join(popDate, by=c("country", "year")) %>% 
  mutate(rate = cases / population * 10000)

#3
table2rate <- table2 %>%
  filter(type == 'population') %>% 
  select(country, year,
         population = count)

tb2cas <- table2 %>% 
  filter(type == 'cases')
tb2casVec <- tb2cas$count

table2rate <- table2rate %>% 
  mutate(cases = tb2casVec)

ggplot(table2rate, aes(year, cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country))

#gathering
table4a

tidy4a <- table4a %>% 
  gather(`1999`, `2000`, key="year", value="cases")

tidy4b <- table4b %>% 
  gather(`1999`, `2000`, key="year", value="population")

left_join(tidy4a, tidy4b)

#spreading
table2

spread(table2, key = type, value = count)

#exs page 156
table4a %>% 
  gather(1999, 2000, key = "year", value = "cases")

table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")

#3
people <- tribble(
  ~name,             ~key,     ~value,
  #----------------/--------/-------
  "Phillip Woods",  "age",      45,
  "Phillip Woods",  "height",   186,
  "Phillip Woods",  "age",      50,
  "Jessica Cordero",  "age",      37,
  "Jessica Cordero",  "height",  156
)

spread(people, key=key, value=value)
individual <- c(1, 1, 2, 1, 1)
people2 <- people %>% 
  mutate(individual = individual)

spread(people2, key=key, value=value)

#4
preg <- tribble(
  ~pregnant,  ~male, ~female,
  "yes",    NA,     10,
  "no",  20,    12
)

preg2 <- 
  preg %>% gather(male, female, key=sex, value=count)

spread(preg2, key=pregnant, value=count)

#p157 separate
table3

table3 %>% 
  separate(rate, into = c("cases", "population"))

table3 %>% 
  separate(rate, into = c("cases", "population"), sep = '/')

table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)

table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)

#unite
table5

table5 %>% 
  unite(new, century, year)

table5 %>% 
  unite(new, century, year, sep = '')

# p160 exercises
#1
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra="drop")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill = "right")

#2
?separate

#3
?extract

#missing values
stocks <- tibble(
  year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr  = c(1,    2,    3,   4,     2,    3,    4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)

stocks %>% 
  spread(year, return) %>% 
  gather(year, return, `2015`, `2016`)

#get rid of na's
stocks %>% 
  spread(year, return) %>% 
  gather(year, return, `2015`, `2016`, na.rm = TRUE)

#complete
stocks %>% 
  complete(year, qtr)

#fill
treatment <- tribble(
  ~ person,          ~ treatment,    ~ response,
  "Derrick Whitmore",   1,           7,
  NA,                   2,          3,
  NA,                   3,          10,
  "Katherine Burke",    1,           4
)

treatment %>% 
  fill(person)

#p 163 exercises
#1
?spread
stocks %>% 
  spread(year, return, fill = 9999)

stocks %>% 
  complete(year, qtr, fill=list(year=9999, qtr=1234, return=99911))

#2
?fill
treatment %>% 
  fill(person, .direction = "up")


####p163 case study
who

who1 <- who %>% 
  gather(
    new_sp_m014:newrel_f65, key = "key",
    value = "cases",
    na.rm = TRUE
  )
who1

who1 %>% 
  count(key)

who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2

#split up key
who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")
who3

#drop new iso2 iso 3
who3 %>% 
  count(new)

who4 <- who3 %>% 
  select(-new, -iso2, -iso3)

who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1)

who5

####all combined
who %>% 
  gather(
    new_sp_m014:newrel_f65, key = "key",
    value = "cases",
    na.rm = TRUE
  ) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>% 
  separate(key, c("new", "type", "sexage"), sep = "_") %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

#exs p 168
#1 without na.rm
who %>% 
  gather(
    new_sp_m014:newrel_f65, key = "key",
    value = "cases"
  ) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>% 
  separate(key, c("new", "type", "sexage"), sep = "_") %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

#2 without mutate
who %>% 
  gather(
    new_sp_m014:newrel_f65, key = "key",
    value = "cases",
    na.rm = TRUE
  ) %>% 
  separate(key, c("new", "type", "sexage"), sep = "_") %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

#3 confirming redundancy of iso2 and  iso3 by counting distinct row combinations
who %>% select(country) %>% unique() %>% count()
who %>% select(country, iso2, iso3) %>% unique() %>% count()
#negative control
who %>% select(country, iso2, iso3, year) %>% unique() %>% count()

#4 total number of cases
who1 <- who %>%
  gather(
    new_sp_m014:newrel_f65, key = "key",
    value = "cases",
    na.rm = TRUE
  ) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>% 
  separate(key, c("new", "type", "sexage"), sep = "_") %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1) %>% 
  group_by(country, year, sex) %>% 
  summarize(cases = sum(cases))

top10 <- who1 %>% 
  group_by(country, sex) %>% 
  summarise(total = sum(cases)) %>% 
  arrange(desc(total)) %>% 
  ungroup() %>% 
  group_by(sex) %>% 
  mutate(rank = min_rank(desc(total))) %>% 
  filter(rank < 11) %>% 
  select(country, sex)

who2 <- who1 %>% 
  inner_join(top10)

ggplot(who2) +
  geom_line(aes(year, cases, colour = country)) + 
  facet_wrap(~ sex, nrow = 2) +
  coord_cartesian(xlim = c(1995,2015))

