library(tidyverse)

#vector characteristics
typeof(letters)
typeof(1:10)

x <- list("a", "b", 1:10)
length(x)

#constructing a logical vector
1:10 %% 3 == 0

c(TRUE, FALSE, FALSE, NA)

#numeric
typeof(1)
typeof(1L)
1.5L

#doubles are approximations
x <- sqrt(2) ^ 2
x
x - 2

#special numbers
c(1, 0, -1) / 0

#character strings - demonstrating global string pool

x <- "This is a reasonably long string"
pryr::object_size(x)

y <- rep(x, 1000)
pryr::object_size(y)

#types of NA
NA
NA_integer_
NA_real_
NA_character_

#exs p296
#1
is.finite(0/0)
!is.infinite(0/0)
#2
dplyr::near

#5
typeof(readr::parse_integer(c("2", "4")))

#implicit coersion
x <- sample(20, 100, replace = TRUE)
y <- x > 10
sum(y)
mean(y)

#vectors with multiple types
typeof(c(TRUE, 1L))
typeof(c(1L, 1.5))
typeof(c(1.5, "a"))

#vector recycling
sample(10) + 100
runif(10) > 0.5

1:10 + 1:2

#gives a warning
1:10 + 1:3

#in the tidyverse vector recycling (except for scalars)
#throws erors
tibble(x = 1:4, y = 1:2)
tibble(x = 1:4, y = rep(1:2, 2))

c(x=1, y=2, z=4)
set_names(1:3, c("x", "y", "z"))

#subsetting
x <- c("one", "two", "three", "four", "five")
x[c(1, 3, 5)]
x[c(1, 1, 5, 5, 2)]
x[c(-1, -3, -5)]
x[c(1, -1)]
x[0]

#logical
x <- c(10, 3, NA, 5, 0, 1, NA)

x[!is.na(x)]
x[x %% 2 == 0]

#subset named vector
x <- c(abc = 1, def = 2, xyz = 3)
x[c("abc", "xyz")]

#do nothing
x[]

#exs p 302
#1
mean(is.na(x))
x <- c(-1, 0, 1)
y <- x/0
sum(!is.finite(x))
y
#2
?is.vector
?is.atomic
?setNames
?purrr::set_names

#4
#a
x <- c(1, 2, 3, 4, 5, NA, 7)
x[length(x)]
#b
x[seq(2, length(x), 2)]
#c
x[-length(x)]
#d
x[(x %% 2 == 0) & (!is.na(x))]
#5
?which
x <- c(-1, 0, NA, 1, 3)
y <- x/0
x[-which(x > 0)]
x[x <= 0]
y[-which(y > 0)]
y[y <= 0]
y
y[c(2,3)]
y[c(-1,-4,-5)]
y >= NaN

#6
x <- c(1, 2, 3, 4, 5)
x[9]
x[c(4,9)]
x = c(gg=1, hh=2)
x
x['tt']
x['gg']

#lists
x <- list(1, 2, 3)
x
str(x)
x_names <- list(one=1, two=2, three=3)
str(x_names)

y <- list("a", 1L, 1.5, TRUE)
str(y)
z <- list(list(1,2), list(3, 4))
str(z)

x1 <- list(c(1,2), c(3, 4))
str(x1)
x2 <- list(list(1,2), list(3,4))
str(x2)
x3 <- list(1, list(2, list(3)))
str(x3)


#subsetting
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))

str(a[1:2])
str(a[4])
str(a[[4]])
str(a$a)
str(a[["a"]])

str(a[[4]][1])
str(a[[4]][[1]])

#ex p307
#2
x <- tibble(a=c(1,2,3), b=c("one", "two", "three"))
x
x[1]
x[2]
x[[1]]
str(x[[1]])
x$a
x[,1]
x[1,]
x[1][1]
x[[1]][1]

#attributes
x <- 1:10
attr(x, "greeting")
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye"
attributes(x)

#generic function...oo code
as.Date
methods("as.Date")
getS3method("as.Date", "default")
getS3method("as.Date", "numeric")

#FACTORS
x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
attributes(x)

#dates
x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
attributes(x)

#date time
x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
typeof(x)
attributes(x)

#date time posixlt
y <- as.POSIXlt(x)
typeof(y)
attributes(y)

#tibbles
tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb)
attributes(tb)

df <- data.frame(x = 1:5, y=5:1)
typeof(df)
attributes(df)

#exercises p 312
#1
hms::hms(3600)
attributes(hms::hms(3600))
typeof(hms::hms(3600))

#2
tibble(x=1:2, y=1:5)

#32
tibble(x=list(1:5), y=1:5)
