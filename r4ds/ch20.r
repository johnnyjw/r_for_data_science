library(modelr)
library(tidyverse)

library(gapminder)
gapminder

#exploratory plot
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
    geom_line(alpha = 1/3)

###model factors that might mask subtle influences
nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + 
    geom_line() +
    ggtitle("Full data =")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>% 
  ggplot(aes(year, pred)) +
    geom_line() +
    ggtitle("Linear trrend +")

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) +
    geom_hline(yintercept = 0, color = "white", size = 3) +
    geom_line() +
    ggtitle("Remaining pattern")

by_country <- gapminder %>%
  group_by(country, continent) %>% 
  nest()

by_country
#looking at the nnested data
by_country$data[[1]]

#country model fitting function
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

models <- map(by_country$data, country_model)

#better to put straight into df
by_country <- by_country %>%
  mutate(model = map(data, country_model))
by_country

by_country %>% 
  filter(continent == "Europe")
by_country %>% 
  arrange(continent, country)

#all the models produced - now need to add residuals
by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country

resids <- unnest(by_country, resids)
resids

resids %>% 
  ggplot(aes(year, resid)) +
    geom_line(aes(group = country), alpha = 1/3) +
    geom_smooth(se = FALSE)

#facet by country
resids %>% 
  ggplot(aes(year, resid, group = country)) +
    geom_line(alpha = 1/3) +
    facet_wrap(~continent)

#broom::glance
broom::glance(nz_mod)

by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)

#drop other list columns
glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
glance

#look at the ones not fitting so well
glance %>% 
  arrange(r.squared)

#look at africa, they are the worst-fitting models
glance %>% 
  ggplot(aes(continent, r.squared)) +
    geom_jitter(width = 0.5)

#focus on particularly bad fit
bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>%
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, color = country)) +
    geom_line()

#Exs 409
#1
normalit<-function(m){
  (m - mean(m))/(mean(m))
}

ny_gapminder <- gapminder %>% 
  mutate(norm_year = normalit(year))

ny_by_country <- ny_gapminder %>%
  group_by(country, continent) %>% 
  nest()

#country model fitting function
ny_country_model <- function(df) {
  lm(lifeExp ~ I(poly(norm_year,3)), data = df)
}

#modelling
ny_by_country <- ny_by_country %>%
  mutate(model = map(data, ny_country_model))
ny_by_country

#all the models produced - now need to add residuals
ny_by_country <- ny_by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
ny_by_country

ny_glance <- ny_by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
ny_glance %>% 
  arrange(r.squared)

glance %>% 
  arrange(r.squared)

ny_glance_rw <- ny_by_country %>% 
  filter(country=='Rwanda') %>% 
  mutate(predict = map2(data, model, add_predictions)) %>% 
  unnest(predict, .drop = TRUE)
ny_glance_rw

ny_glance_rw %>% 
  ggplot(aes(year, lifeExp)) +
  geom_point() + 
  geom_smooth(aes(year, pred))

# Much improved r2 with polynomial
ny_glance %>% 
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)

ny_glance_rw %>% 
  ggplot(aes(norm_year, lifeExp)) +
  geom_point() + 
  geom_smooth(aes(norm_year, pred))

ny_glance_rw
model_matrix(ny_glance_rw, lifeExp ~ I(poly(norm_year,3)))

#get the coefficients
regressionny <- ny_by_country %>% 
  mutate(regressions = map(model, broom::tidy)) %>% 
  unnest(regressions)

regressionny %>% 
  filter(country=='Rwanda')

ny_glance_rw <- ny_glance_rw %>% 
  mutate(c = 41.5,
         x = -2.74*norm_year,
         x2 = (norm_year^2)*0.290,
         x3 = (norm_year^3)*12
  )

# looking at coefficients
ny_glance_rw %>% 
  ggplot(aes(norm_year, lifeExp)) +
  geom_point()+
  geom_line(aes(norm_year, c, color='red')) +
  geom_line(aes(norm_year, x, color='green')) +
  geom_line(aes(norm_year, x2)) +
  geom_line(aes(norm_year, 3))

#look at africa, they are the worst-fitting models
glance %>% 
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)

#2 ggbeeswarm
library(ggbeeswarm)
glance %>% 
  ggplot(aes(continent, r.squared)) +
  geom_quasirandom(aes(color=continent))

#3 back on unnest
nodrop <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)

nodrop %>% 
  filter(r.squared < 0.25) %>% 
  unnest(data, .drop = TRUE) %>% 
  ggplot(aes(year, lifeExp, color = country)) +
  geom_line()

#p 409 looking at lists in columns
data.frame(x = list(1:3, 3:5))
# forcing them into lists with I
data.frame(x = I(list(1:3, 3:5)),
          y = c("1, 2", "3, 4, 5"))
#tibble does it nicer
tibble(x = list(1:3, 3:5),
       y = c("1, 2", "3, 4, 5"))

#nesting
#with grouped data frame
gapminder %>% 
  group_by(country, continent) %>% 
  nest()
#with ungrouped data frame
gapminder %>%
  nest(year:gdpPercap)

#vectorized functions
df <- tribble(
  ~x1,
  "a, b, c",
  "d, e, f"
  )

df %>% 
  mutate(x2 = stringr::str_split(x1, ","))

df %>% 
  mutate(x2 = stringr::str_split(x1, ",")) %>% 
  unnest()

#using map
sim <- tribble(
  ~f,         ~params,
  "runif",  list(min = -1, max = -1),
  "rnorm",  list(sd = 5),
  "rpois",  list(lambda = 10)
)

sim %>% 
  mutate(sims = invoke_map(f, params, n = 10))

# Vectorised Functions
#summaraize doesnt work with quantile
mtcars %>% 
  group_by(cyl) %>%
  summarize(q = quantile(mpg))

# wrap in a list!
mtcars %>% 
  group_by(cyl) %>%
  summarize(q = list(quantile(mpg)))

# with the quantile function, to unnest you also need the probabilities
probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>% 
  group_by(cyl) %>%
  summarize(p = list(probs), q = list(quantile(mpg))) %>% 
  unnest()

#from named list
x <- list(
  a=1:3,
  b=3:4,
  c=6:11
)
df <- enframe(x)
df

# then iterate using map2
df %>% 
  mutate(
    smry = map2_chr(
      name,
      value,
      ~ stringr::str_c(.x, ": ", .y[1])
    )
  )

#ex p 414
#3
mtcars %>% 
  group_by(cyl) %>%
  summarize(q = list(quantile(mpg))) %>% 
  unnest()

mtcars %>% 
  group_by(cyl) %>%
  summarize(q = list(quantile(mpg), names=TRUE)) %>% 
  unnest()


quantile((mtcars$mpg), names=FALSE)


#4
mtcars %>% 
  group_by(cyl) %>% 
  summarize_each(funs(list))


