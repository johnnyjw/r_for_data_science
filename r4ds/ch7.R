library(tidyverse)
library(dplyr)

#convert into a tibble
as_tibble(iris)

#create from scratch
tibble(
  x = 1:5,
  y = 1,
  z = x ^ 2 + y
)

#column names with non valid names - nonsyntactic names
tb <- tibble(
  `:)` = "smile",
  ` ` = "space",
  `2000` = "number"
)

tb

#p121 Tribble - transposed tibble
tribble(
  ~x, ~y, ~z,
  #--/--/----
  "a", 2, 3.6,
  "b", 1, 8.5
)

# Showing printing tibbles p121
tibble(
  a = lubridate::now() + runif(1e3) ** 86400,
  b = lubridate::today() + runif(1e3) ** 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

nycflights13::flights %>% 
  print(n=10, width = Inf)

#built in data viewer
nycflights13::flights %>% 
  View()

#p123 subsetting
df <- tibble(
  x = runif(5),
  y = rnorm(5)
)

df$x
df[["x"]]
df %>%  .$x
df %>%  .[["x"]]

#change
class(df)
class(as.data.frame(df))

#exs p 123
#1
class(mtcars)
print(mtcars)
print(as.tibble(mtcars))

#2
df <- data.frame(abc = 1, xyz = "a")
df_t <- as.tibble(df)
#as a data frame you can list first letter of column, cant with tibbles
df$x
df_t$x
#as a df the below outputs a list, tibble outputs tibble
df[, "xyz"]
df_t[, "xyz"]
#a df output or a tibble
df[, c("abc", "xyz")]
df_t[, c("abc", "xyz")]

#3 
name <- "xyz"

df_t[[name]]

#4
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` *2 + rnorm(length(`1`))
)

#a
annoying$`1`

#b
ggplot(annoying) +
  geom_point(aes(`1`,`2`))

#c
annoying2 <- annoying %>% 
  mutate(`3` = `2` / `1`)
annoying2

#d
annoying2 %>% 
  select(
    one = `1`,
    two = `2`,
    three = `3`
  )

#5 enframe
enframe(annoying)
enframe(1:3)

#6
as.tibble(nycflights13::flights)

options(tibble.width =100)
options(tibble.max_extra_cols = 1)
