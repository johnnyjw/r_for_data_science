library(magrittr)

diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>% 
  dplyr::mutate(price_per_carat = price / carat)

pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds, diamonds2)

###similar sizes as share identical columns....
###but what if you change a column?....
diamonds$carat[1] <- NA
pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds, diamonds2)

##### pipes dont work well with things using current environment
## eg assign
assign("x", 10)
x
"x" %>% assign(100)
x

###can set environment
env <- environment()
"x" %>% assign(100, envir = env)
x

##eg lazy evaluation
tryCatch(stop("!"), error = function(e) "An error")

stop("!") %>% 
  tryCatch(error = function(e) "An error")

#other magrittr tools
rnorm(100) %>% 
  matrix(ncol = 2) %>% 
  plot() %>% 
  str()

rnorm(100) %>% 
  matrix(ncol = 2) %T>% 
  plot() %>% 
  str()

#explode
mtcars %$%
  cor(disp, mpg)

#assignment
mtcars %<>% transform(cyl = cyl * 2)
