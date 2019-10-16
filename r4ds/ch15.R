library(stringr)

###shoulda dunnna function
df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

#abstracting out
x <- df$a
(x - min(x, na.rm = TRUE)) / 
  (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

#min and max are effectively computing the range of the data three times
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])

#combined into a new function
rescale01 <- function(x){
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}


rescale01(c(0, 5, 10))
rescale01(c(1, 2, 3, NA, 5))

#the below is the same as the original but still there is duplication
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

#easy to change - only need to change the function
x <- c(1:10, Inf)
rescale01(x)

rescale01 <- function(x){
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)

#exercises p273
#1
rescale02 <- function(x){
  rng <- range(x, na.rm = FALSE)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale02(c(1, 2, 3, NA, 5))

#2
rescale03 <- function(x){
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  res <- (x - rng[1]) / (rng[2] - rng[1])
  res <- ifelse(res==Inf, 1, res)
  ifelse(res==-Inf, 0, res)
}
rescale03(x)

y <- c(1:10, Inf, -Inf)
rescale03(y)

#3
#a
prop_NA <- function(x){
  mean(is.na(x))
}
listy <- c(1, NA, 2, 3, 4, NA, 1, 2)
prop_NA(listy)

#b
proportion <- function(x){
  x / sum(x, na.rm = TRUE)
}

tot <- c(0, 1, 2, 3, 4, 5)
proportion(tot)

#c
error_proportion <- function(x){
  sd(x, na.rm = TRUE / mean(x, na.rm = TRUE))
}

error_proportion(tot)

#4
variance

calc_var <- function(x){
  n <- length(x)
  m <- mean(x)
  (1/(n - 1)) * sum((x - m)^2)
}

x <- c(2, 3, 4, 2, 3, 4, 4)
calc_var(x)

calc_skew <- function(x){
  n <- length(x)
  v <- var(x)
  m <- mean(x)
  third.moment <- (1/(n - 2)) * sum((x - m)^3)
  third.moment/(var(x)^(3/2))
}
calc_skew(x)

#5
both_na <- function(x, y){
  both_vec <- if_else(
    (is.na(x) & is.na(y)),1,0
  )
  sum(both_vec)
}


x <- c(NA, NA, 2, 3, NA, 1)
y <- c(NA, NA, 1, 2, 3, 4)
both_na(x, y)

#6
x <- 'test'
x <- c('test', 'ch8.R')
file.info(x)$isdir

file.access(x, 4) == 0

#7
foo_foo <- function(x) {
  str_c(x, "Little Bunny Foo Foo,\n")
}

hop <- function(x) {
  str_c(x,"Hopping through the Forest,\n")
}

scoop <- function(x) {
  str_c(x,"Scooping up the field mice\n")
}

boppin <- function(x) {
  str_c(x,"And boppin' 'em on the head.\n\n")
}

bopping <- function(x) {
  str_c(x,"And bopping them on the head.\n\n")
}

fairy_godmother <- function(x) {
  str_c(x, "(Spoken)\nDown came the Good Fairy, and she said,\n\n") %>% 
    foo_foo() %>%
    str_c("I don't want to see you,\n") %>% 
    scoop() %>% 
    bopping()
}

bunny_verse <- function(x) {
  foo_foo(x) %>% 
    hop() %>% 
    scoop() %>% 
    boppin()
}

chances3 <- function(x) {
  str_c(x, "(Spoken)\nI'll give you three chances,\n")
}

chances2 <- function(x) {
  str_c(x, "(Spoken)\nI'll give you two chances,\n")
}

chances1 <- function(x) {
  str_c(x, "(Spoken)\nI'll give you one chance,\n")
}

chances0 <- function(x) {
  str_c(x, "(Spoken)\nI gave you three chances,\n") %>% 
    str_c("And you didn't behave,\nNow you're a goon. POOF!\n")
}

warn <- function(x) {
  str_c(x, "And if you don't behave,\nI'll turn you into a goon!\n\n")
}

comb <- "" %>% 
  bunny_verse() %>% 
  fairy_godmother() %>% 
  chances3() %>% 
  warn() %>% 
  bunny_verse() %>% 
  fairy_godmother() %>% 
  chances2() %>% 
  warn() %>% 
  bunny_verse() %>%
  fairy_godmother() %>% 
  chances1() %>% 
  warn() %>% 
  bunny_verse() %>%
  fairy_godmother() %>% 
  chances0()

cat(comb)

#exercises p273
