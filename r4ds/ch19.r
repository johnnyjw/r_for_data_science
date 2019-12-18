library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)

###why are low-quality diamonds more expensive?
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
#color j (slightly yellow) is worst
ggplot(diamonds, aes(color, price)) + geom_boxplot()
#clarity I1 (inclusions visible to naked eye)
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

#price and carat
#a confounding variable - weight?
#weight the most important factor for price
#lower quality diamonds tend to be larger
ggplot(diamonds, aes(carat, price)) +
  geom_hex(bins = 50)

#focus on diamonds smaller than 2.5 carats
#log transform carat and price to make more linear
diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price),
         lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) +
  geom_hex(bins = 50)

#fit model
mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

#back-transform the predictions, undoing the log
grid <- diamonds2 %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) +
  geom_hex(bins = 50) +
  geom_line(data = grid, color = "red", size = 1)

#residuals
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) +
  geom_hex(bins = 50)
#not much pattern

#but can now plot price residuals against quality factors
ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
#color j (slightly yellow) is worst
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
#clarity I1 (inclusions visible to naked eye)
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

#and the lowest quality now at the bottom

#make model more complex
mod_diamond2 <- lm(
  lprice ~ lcarat + color + cut + clarity,
  data = diamonds2
)

grid <- diamonds2 %>% 
  data_grid(cut, .model = mod_diamond2) %>% 
  add_predictions(mod_diamond2)
grid

ggplot(grid, aes(cut, pred)) +
  geom_point()

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")

ggplot(diamonds2, aes(lcarat, lresid2)) +
  geom_hex(bins = 50)

diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)

#exs page 384
#3 very high and very low residuals
#the above filter where lresidual is above 1
d2r <- diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)
# a lot of fair cut diamonds overpriced and two underpriced, which look like errors.

#4 mod2 is not bad as a model. A large improvement over mod.
# I would use it to give a guide and a large deviation between price and prediction
# I would question.
summary(mod_diamond)
summary(mod_diamond2)

#nyc flights daily
daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())
daily

ggplot(daily, aes(date, n)) +
  geom_line()

#distribution of flight numbers by day of week.
daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(wday, n)) +
  geom_boxplot()

#a strong daily pattern.  Remove using a model
mod <- lm(n ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red", size = 4)

daily <- daily %>% 
  add_residuals(mod)
daily %>% 
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line()

#splitting up into days of the week to visualize causes of failure
ggplot(daily, aes(date, resid, color = wday)) +
  geom_ref_line(h = 0) +
  geom_line()

#some big residuals in some observations
daily %>% 
  filter(resid < -100)

#and long term trends
daily %>% 
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line(color = "grey50") +
  geom_smooth(se = FALSE, span = 0.20)

#seasonal saturday effect
daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n)) +
  geom_point() +
  geom_line() +
  scale_x_date(
    NULL,
    date_breaks = "1 month",
    date_labels = "%b"
  )

###school term
term <- function(date) {
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall"))
}

daily <- daily %>% 
  mutate(term = term(date))

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, color = term)) +
  geom_point(alpha = 1/3) +
  geom_line() +
  scale_x_date(
    NULL,
    date_breaks = "1 month",
    date_labels = "%b"
  )

daily %>% 
  ggplot(aes(wday, n, color = term)) +
  geom_boxplot()

#so a bit of variation between terms.  Try fitting day-of-week for term.
mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha=0.75)

#model improved, but not as much as hoped

grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red") +
  facet_wrap(~ term)

#some seasons have lots of outliers that are impacting on the model.
#perhaps try a model that is robust over outliers
mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) +
  geom_hline(yintercept = 0, size = 2, color = "white") +
  geom_line()
