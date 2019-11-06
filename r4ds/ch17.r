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

trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
    }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
