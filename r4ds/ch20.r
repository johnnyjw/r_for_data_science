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
