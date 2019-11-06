library(stringr)
library(lubridate)
library(tidyverse)

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
#1
f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

f1("flip_you_melonfarmer", "flip")
f1("flip_you_melonfarmer", "flit")

f2 <- function(x){
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}

f2("Whats going on Geezer")

f2(c("doosh", "poosh"))
length(c("doosh", "poosh"))

f3 <- function(x, y) {
  rep(y, length.out = length(x))
}

f3(c("a", "b"), "doosh")

#3
?rnorm
?MASS::mvrnorm()
)

MASS::mvrnorm(20, mu=1, Sigma=1  )

rnorm(20)

#276 Conditional Execution
has_name <- function(x) {
  nms <- names(x)
  if (is.nul(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}

#must be true or false
if (c(TRUE, FALSE)) {}
kf (NA) {}

#using identical
identical(0, 0L)

x <- sqrt(2) ^ 2

identical(x, 2)

###exs p279
#1 if is a statement/control structure, ifelse() is a function
# ifelse can take vectors

g <- 34
ifelse(g > 3, "doosh")

if (g>3) "doosh"  

ifelse(g>333, "doosh", "antidoosh")
if (g>333) "doosh" else "antidoosh"

g <- c(2, 66, 88)
ifelse(g>77, "doosh", "antidoosh")
if (g>333) "doosh" else "antidoosh"

#2
greet <- function(time=lubridate::now()) {
  if (hour(time) < 12) {
    cat("Good Morning")
  }  else if (hour(time) < 5) {
    cat("Good Afternoon")
  } else {
    cat("Good Evening")
  }
}

greet()

#3 fizzbuzz
fizzbuzz <- function (number) {
  if (identical(number %% 3,0) &&
    identical(number %% 5, 0)) {
      cat("fizzbuzz")} 
  else if (identical(number %% 3, 0)) {
    cat("fizz")
  } else if ( identical(number %% 5, 0)) {
    cat("buzz")
  } else cat(number)
}

fizzbuzz(2)
fizzbuzz(15)

#4
?cut

temp_class <- function(temp) {
    cut(temp, breaks = c(-100, 0,10,20,30,100), 
        labels = c("freezing", "cold", "cool", "warm", "hot"))}

temp_class(c(0, 10, 20, 30, 40))

temp_class <- function(temp) {
  cut(temp, breaks = c(-100, 0,10,20,30,100), 
      labels = c("freezing", "cold", "cool", "warm", "hot"),
      right = FALSE)}

temp_class(c(0, 10, 20, 30, 40))

#cut converts into categories so not running through if nested conditionals...more efficent

#5
?switch
switch(5,
       5 = "five",
       10 = "ten"
       )

test_it <- function (num){
  switch(num,
         '5' = "five",
         '10' = "ten",
         stop("not five or ten")
  )
}

switch(2,
       "one",
       "two")
test_it(5)
test_it("6")
#switch works differently if the first arguement is a number
#it then returns the nth arguement related to n


#6 test another switch
test_two <- function(test){
  switch(test,
         a = ,
         b = "ab",
         c = ,
         d = "cd",
         "others")
}

test_two('e')
#switch a and c return the ones below, as this is what happens
# when an implicit return value is not supplied
# if an unnamed value provided, that is default, otherwise nothing

#function arguements
mean_ci <- function(x, conf = 0.95){
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se*qnorm(c(alpha / 2, 1 - alpha / 2))
  }

x <- runif(100)
mean_ci(x)
mean_ci(x, conf = 0.99)

#checking values
wt_mean <- function(x, w, na.rm = FALSE) {
  if (length(x) != length(w)) {
    stop("'x' and 'w' must be the same length ", call = FALSE)
  }
  sum(w * x) / sum(x)
}

wt_mean(c(1,2,3), c(2,3,5))
wt_mean(c(1,2,3), c(2,3))

wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(x)
}

wt_mean(1:6, 6:1, na.rm = "foo")

#dot-dot-dot
commas <- function(...) stringr::str_c(..., collapse =  ",")
commas(letters[1:10])

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep="")
}
rule("Important output")
rule("hate", "that")

x <- c(1,2)
sum(x, na.mr = TRUE)
#the mr is a mistake but not caught

#ex p 284
#1
commas(latters[1:10], collapse = '-')
#2
rule("Title", pad = "-+")
rule_more <- function(..., pad = "-") {
  title <- paste0(...)
  width <- (getOption("width") - (nchar(title)) - 5) / nchar(pad)
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep="")
}
rule_more("Important output", pad = "-+./")
rule_more("Important output" )
#3
?mean
to_mean <- c(1,50,99, 34, 55, 77, 44, 77, 20, 80, 999999)
mean(to_mean)
mean(to_mean, trim=0.1)
#4
?cor

cor(1:10, 2:11, method="spearmen")

#side effect function
show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  invisible(df)
}

show_missings(mtcars)
x <- show_missings(mtcars)
class(x)
dim(x)

mtcars %>% 
  show_missings() %>% 
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings

#enviornment
funcie <- function(x){
  x + y
}

y <- 100
funcie(10)
y <- 1000
funcie(10)
