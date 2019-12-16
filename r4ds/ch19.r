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
