library(tidyverse)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

#do a loop to output media of columns
output <- vector("double", ncol(df))  #output
for (i in seq_along(df)) {            #sequence
  output[[i]] <- median(df[[i]])      #body
}
output

####if vector is 0, seq_along can cope
y <- vector("double", 0)
seq_along(y)
1:length(y)

#exercises p316
mtcars
#1A
output <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  output[[i]] <- mean(mtcars[[i]])
}
output

#b
output <- vector("double", ncol(nycflights13::flights))
for (i in seq_along(nycflights13::flights)) {
  output[[i]] <- typeof(nycflights13::flights[[i]])
}
output

#c
output <- vector("double", ncol(iris))
for (i in seq_along(iris)) {
  output[[i]] <- length(unique(iris[[i]]))
}
output

#d

for (i in c(-10, 0, 10, 100)) {
  vec <- rnorm(10, mean=i)
  cat(vec, "\n")
}

#2
#a
out <- str_c(letters, collapse="")

out <- ""
for (x in letters) {
  out <- str_c(out, x)
}
out

#b
x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))
sd
sd <- sd(x)
sd

#c
x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i-1] + x[i]
}
out
x
?runif
out2 <- cumsum(x)
out2

#3
#a 
humps <- function(){
  for (countdown in c("five", "four", "three", "two", "one", "no")){
    for (i in 1:3) {
      if (countdown == "one"){
        cat("Alice the camel has ", countdown, " hump\n", sep="" )
      } else {
        cat("Alice the camel has ", countdown, " humps\n", sep="" )
      }
    }
    if (countdown == "no"){
      cat("Now Alice is a horse\n\n")
    } else {
      cat("So go, Alice, Go\n\n")
    }
  }
}

humps()

#b
bed <- function(n){
  for (countdown in n:1){
    
    if (countdown == 1){
      cat("There was one in the bed and the little one said ")
      cat("'Good night!\n\n")
    } else {
      cat("There were ", countdown,
          " in the bed and the little one said ",
          sep="")
      cat("'Roll Over, Roll Over'\n")
      cat("And they all rolled over and one fell out!\n\n")
    }
  }
}

bed(5)

#c
beer <- function(n){
  for (countdown in n:0){
    if (countdown == 0){
      cat("No more bottles of beer on the wall, no more bottles of beer.\n",
          "Go to the store and buy some more, 99 bottles of beer on the wall.\n\n",
          sep="")
    }
    else if (countdown == 1){
      cat("1 bottle of beer on the wall, 1 bottle of beer.\n",
          "Take one down and pass it around, no more bottles of beer on the wall.\n\n",
          sep="")
        
    } else {
      cat(countdown, " bottles of beer on the wall, ",
          countdown, " bottles of beer.\n",
          "Take one down and pass it around, ", 
          countdown - 1,
          sep=""
          )
      if (countdown == 2) {
        cat(" bottle of beer on the wall.\n\n")
      } else {
        cat(" bottles of beer on the wall.\n\n")
      }
    }
  }
}

beer(5)

#4 testing pre existing vector or extending vector
listy <- list()
for (i in 1:10000){
  listy[[i]] <- rnorm(100)
}

#preexisting
start_time <- Sys.time()
output <- vector("double", length(listy))
for (i in seq_along(listy)) {
  output[[i]] <- mean(listy[[i]])
}

end_time <- Sys.time()

end_time - start_time

#extending
start_time <- Sys.time()
output <- vector("double", 0)
for (i in seq_along(listy)) {
  output <- c(output, mean(listy[[i]]))
}
end_time <- Sys.time()

end_time - start_time


#for loop variations
#modifying existing object
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

rescale01 <- function(x){
  rng <- range(x, na.rm = TRUE)
  (x - rng[1] / (rng[2] - rng[1]))
}

#apply directly to function
for (i in seq_along(df)){
  df[[i]] <- rescale01(df[[i]])
}

#looping patterns
results <- vector("list", length)

#unknown output length
means <- c(0, 1, 2)

output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)

###better way
out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)
str(unlist(out))

#example of while loop
flip <- function() sample(c("T", "H"), 1)

flips <- 0
nheads <- 0

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips

##exs p 321
#2
veccie <- 1:14
for (nm in names(veccie)){
  cat(nm, "\n", sep="")
}

names(veccie) <- c("one", NA, "Three", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
for (nm in names(veccie)){
  cat(veccie[nm], "\n", sep="")
}
names(veccie) <- c("one", NA, "Three", "Three", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
for (nm in names(veccie)){
  cat(veccie[nm], "\n", sep="")
}

#3
show_mean <- function(df){
  for (i in seq_along(df)){
    if (is.numeric(df[[i]])){
      cat(str_pad(str_c(names(df[i]), ":"), 15, 'right'), round(mean(df[[i]]),2), "\n", sep="")
    }
  
  }

}


show_mean(iris)

?round


#4
mtcars_two <- mtcars

trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
    }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}

mtcars_two
mtcars

#p322 For loops and functional programming
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

###computing mean of every column using a for loop
output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output

####extract mean out of function
col_mean <- function(df){
  output <- vector("double", length(df))
  for (i in seq_along(df)){
    output[i] <- mean(df[[i]])
  }
  output
}

col_mean(df)

###throw in a few more summary stats
col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)){
    out[i] <- fun(df[[i]])
  }
  out
}

col_summary(df, median)

#p324 exs
#1
?apply
for (i in names(mtcars)){
  cat(i, "\n", sep="")
}
for (i in row.names(mtcars)){
  cat(i, "\n", sep="")
}

#2
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10),
  e = c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b")
)

col_summary <- function(df, fun) {
  is_num <- vector("logical", length(df))
  for (i in seq_along(df)) {
    is_num[i] <- is.numeric(df[[i]])
  }
  out <- vector("double", length(df[is_num]))
  for (i in seq_along(df)[is_num]){
    out[i] <- fun(df[[i]])
  }
  out
}
col_summary(df, median)

###functional programming
map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

df %>% map_dbl(mean)
df %>% map_dbl(median)
df %>% map_dbl(sd)

df %>% map_dbl(mean)
#passing arguements
df %>% map_dbl(mean, trim=0.5)

#preserving names
z <- list(x = 1:3, y = 4:5)
map_int(z, length)
?map()

#shortcuts
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))
#or
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

#and output
models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)

models %>% 
  map(summary) %>% 
  map_dbl("r.squared")

#the wierdness of sapply
x1 <- list(
  c(0.27, 0.33, 0.56, 0.91),
  c(0.33, 0.92, 0.81, 0.23), 
  c(0.22, 0.33, 0.11, 0.14)
)
x2 <- list(
  c(0.27, 0.33, 0.56, 0.91),
  c(0.33, 0.92, 0.21, 0.23), 
  c(0.22, 0.33, 0.11, 0.84)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()

x2 %>% sapply(threshold) %>% str()

###exs p 328
#1
#a
mtcars_two %>% map(mean)
#b
nycflights13::flights %>% map(typeof)
#c
iris %>% map(function(x) length(unique(x)))
#d
c(-10, 0, 10, 100) %>% map(function(x) rnorm(10, x))

#2
mtcars %>% 
  map(is.factor) %>% 
  flatten_lgl()

#3
goulet <- 1:10
g2 <- goulet %>% map(function(x) x + 10L)
g3 <- goulet %>% map_int(function(x) x + 10L)

map(1:5, runif)
?runif

#4
map(-2:2, rnorm, n=5)
map_dbl(-2:2, rnorm, n=5)

#5
lin_mod <- function(x){
  lm(mpg ~ wt, data = x)
}

mtcars %>% 
  split(.$cyl) %>%  map(lin_mod)

mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))

###testing out safely
safe_log <- safely(log)
str(safe_log(10))
str(safe_log("a"))

x <- list(1, 10, "a")
y <- x %>%
  map(safely(log))
str(y)

#transpose that
y <- y %>% transpose()
y

#use ok values
is_ok <- y$error %>% map_lgl(is_null) 
x[!is_ok] 
y$result[is_ok] %>% flatten_dbl()

#possibly
x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))

#quietly
x <- list(1, -1)
x %>% map(quietly(log)) %>% str()

#mapping over multiple arguements
#single argument
mu <- list(5, 10, -3)
mu %>% 
  map(rnorm, n = 5) %>% 
  str()

#multiple, could iterate over indices
sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()

#or simpler with map2
map2(mu, sigma, rnorm, n = 5) %>% str()

#using pmap
n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>% 
  pmap(rnorm) %>% 
  str()

#using named arguments
args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% 
  pmap(rnorm) %>% 
  str()
