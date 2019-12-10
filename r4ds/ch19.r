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