library(tidyverse)
library(modelr)
options(na.action = na.warn)

ggplot(sim1, aes(x, y)) +
  geom_point()

#randomly generate models
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) +
  geom_abline(
    aes(intercept = a1, slope = a2),
    data = models, alpha = 1/4
) +
  geom_point()

#model y prediction (linear slope)
model1 <- function(a, data) {
  a[1] + data$x * a[2]
}

model1(c(7, 1.5), sim1)

#rms'ing it
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^2))
}
measure_distance(c(7, 1.5), sim1)

#helper function to run modelling across lots of models
sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

#overlay top 10 best models
ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(models, rank(dist) <= 10)
  )

#show models and distance
ggplot(models, aes(a1, a2)) +
  geom_point(
    data = filter(models, rank(dist) <= 10),
    size = 4, color = "red"
  ) +
  geom_point(aes(color = -dist))

##doing a grid version
grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(
    data = filter(grid, rank(dist) <= 10),
    size = 4, color = "red"
  ) +
  geom_point(aes(color = -dist))

#overlay back onto data
ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid, rank(dist) <= 10)
  )

#optimisation of model using Newton-Raphson
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])

##using lm
sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

#exercises p 353
#1
sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)
sim1a_mod <- lm(y ~ x, data = sim1a)
sim1a_coef <- coef(sim1a_mod)

ggplot(sim1a, aes(x, y)) +
  geom_point() +
  geom_abline(intercept = sim1a_coef[1], slope = sim1a_coef[2])
#outliers do affect the slope

#2
#ame ing it
#model y prediction (linear slope)
make_prediction <- function(a, data) {
  a[1] + data$x * a[2]
}

measure_distance <- function(mod, data) {
  diff <- data$y - make_prediction(mod, data)
  mean(abs(diff))
}
best <- optim(c(0, 0), measure_distance, data = sim1a)

ggplot(sim1a, aes(x, y)) +
  geom_point() +
  geom_abline(intercept = sim1a_coef[1], slope = sim1a_coef[2]) +
  geom_abline(intercept = best$par[1], slope = best$par[2], colour="red",size=2)

#3
#a third dimension - where one vaue is essentially dependent on another and will be different dependent on what one of the two (x^0) variables is allocated
#so an infinite number of combinations with the two values

#visualising models
#grid of values
grid <- sim1 %>% 
  data_grid(x)
grid
sim1

grid <- grid %>% 
  add_predictions(sim1_mod)
grid

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(
    aes(y = pred),
    data = grid,
    color = "red",
    size = 1
  )

##residuals
sim1_res <- sim1 %>%
  add_residuals(sim1_mod)
sim1_res

ggplot(sim1_res, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)

#recreating plot with residuals
ggplot(sim1_res, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

#ex pe 358
#1
loess_mod <- loess(y ~ x, data = sim1)
grid <- sim1 %>% 
  add_predictions(loess_mod)
grid
ggplot(grid) +
  geom_point(aes(x, y)) +
  geom_line(aes(x, pred, color='red')) +
  geom_smooth(aes(x, y, color="blue"))
###it looks like the prediction with loess is the same as geom_smooth

linear_mod <- lm(y ~ x, data = sim1)
grid2 <- sim1 %>% 
  add_predictions(linear_mod)
grid2
ggplot(grid2) +
  geom_point(aes(x, y)) +
  geom_line(aes(x, pred, color='red')) +
  geom_smooth(aes(x, y, color="blue"))

#2
?add_predictions
?gather_predictions

grid3 <- sim1 %>% 
  spread_predictions(loess_mod, linear_mod)
grid3

grid4 <- sim1 %>% 
  gather_predictions(loess_mod, linear_mod)
grid4

#3
?geom_ref_line
ggplot(grid2) +
  geom_point(aes(x, y)) +
  geom_line(aes(x, pred, color='red')) +
  geom_ref_line(h = 15)

grid2a <- grid2 %>% 
  add_residuals(loess_mod)
grid2a
ggplot(grid2a) +
  geom_point(aes(x, resid)) +
  geom_ref_line(h=0)

#4
ggplot(grid2a) +
  geom_freqpoly(aes(resid))

#formulas
#model matrix shows the conversion of formula to function
df <- tribble(
  ~y,   ~x1,    ~x2,
  4,      2,     5,
  5,      1,     6
)
model_matrix(df, y ~ x1)

#explicity drop column using -1
model_matrix(df, y ~ x1 - 1)
model_matrix(df, y~x1 + x2)

df <- tribble(
  ~ sex,  ~ response,
  "male",   1,
  "female", 2,
  "male",   1
)
model_matrix(df, response ~ sex)

#sim2 dataset
ggplot(sim2) +
  geom_point(aes(x, y))

mod2 <- lm(y ~ x, data = sim2)
grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid

#visualise predictions
ggplot(sim2, aes(x)) +
  geom_point(aes(y = y)) +
  geom_point(
    data = grid,
    aes(y = pred),
    color = "red",
    size = 4
  )

#you cannot make predictions about levels that you didn't observe
tibble(x= "e") %>% 
  add_predictions(mod2)

#continuous + categorical
ggplot(sim3, aes(x1, y)) +
  geom_point(aes(color = x2))

sim3

#independent model
mod1 <- lm(y ~ x1 + x2, data = sim3)
#interaction model
mod2 <- lm(y ~ x1 * x2, data = sim3)

grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2)
grid

ggplot(sim3, aes(x1, y, color = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~ model)

#map residuals
sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, color = x2)) +
  geom_point() +
  facet_grid(model ~ x2)

#continuous interaction
#independent model
mod1 <- lm(y ~ x1 + x2, data = sim4)
#interaction model
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5)) %>% 
  gather_predictions(mod1, mod2)
grid

###seq range interesting values
seq_range(c(0.0123, 0.923423), n = 5)
seq_range(c(0.0123, 0.923423), n = 5, pretty=TRUE)

x1 <- rcauchy(100)
seq_range(x1, n = 5)
seq_range(x1, n = 5, trim = 0.1)
seq_range(x1, n = 5, trim = 0.25)
seq_range(x1, n = 5, trim = 0.5)

x2 <- c(0, 1)
seq_range(x2, n = 5)
seq_range(x2, n = 5, expand = 0.1)
seq_range(x2, n = 5, expand = 0.25)
seq_range(x1, n = 5, expand = 0.5)

#visualize: not so wonderful
ggplot(grid, aes(x1, x2)) +
  geom_tile(aes(fill = pred)) +
  facet_wrap(~ model)
#better - take slices (groups)
ggplot(grid, aes(x1, pred, color = x2, group = x2)) +
  geom_line() +
  facet_wrap(~ model)

ggplot(grid, aes(x2, pred, color = x1, group = x1)) +
  geom_line() +
  facet_wrap(~ model)

#transformations
df <- tribble(
  ~y, ~x,
  1,  1, 
  2,  2, 
  3,  3
)

#interprets as multi variables, which are actually the same and weeds out redundancies
model_matrix(df, y ~ x^2 + x)
#interprets as transformation
model_matrix(df, y ~ I(x^2) + x)

#poly function
model_matrix(df, y ~ poly(x, 2))

#natural splines
library(splines)
model_matrix(df, y ~ ns(x, 2))

#example of approximating non-linear system
sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

ggplot(sim5, aes(x, y)) +
  geom_point()

#fit to data
mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) +
  geom_point() +
  geom_line(data = grid, color = "red") +
  facet_wrap(~ model)
# extrapolation outside data range is very bad!

#exs p 371
#1 sim2 without intercept
#sim2 dataset
ggplot(sim2) +
  geom_point(aes(x, y))

mm1 <- model_matrix(sim2, y ~ x)
mm1a <- model_matrix(sim2, y ~ x - 1)

mod2a <- lm(y ~ x - 1, data = sim2)
grid <- sim2 %>% 
  data_grid(x) %>% 
  gather_predictions(mod2, mod2a)
grid

#visualise predictions
ggplot(sim2, aes(x)) +
  geom_point(aes(y = y)) +
  geom_point(
    data = grid,
    aes(y = pred),
    color = "red",
    size = 4
  )
#a very marginal impact on intercept on the categorical stuff

#2 model matrix investigations
