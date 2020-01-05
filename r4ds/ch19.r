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

#bundling computed variables into a function
compute_vars <- function(data) {
  data %>%
    mutate(
      term = term(date),
      wday = wday(date, label = TRUE)
    )
}

#OR stick it into the model
wday2 <- function(x) wday(x, label=TRUE)
mod3 <- lm(n ~ wday2(date) * term(date), data = daily)

#introducing random splines to fit not completely linear models
library(splines)
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(date, pred, color = wday)) +
     geom_line() +
     geom_point()

#ex p394 
#3
daily2 <- daily %>% 
  mutate(wday_char = as.character(wday)) %>% 
  mutate(term_char = as.character(term)) %>% 
  mutate(wday_term = 
           ifelse(wday == 'Sat',
                  paste(wday_char, "-", term_char, sep=""),
                  wday_char)
         ) %>% 
  mutate(wday_term_fct = as.factor(wday_term))

mod1 <- lm(n ~ wday * term, data = daily2)
mod2 <- lm(n ~ wday_term_fct, data = daily2)
mod3 <- lm(n ~ wday, data = daily2)

daily2 %>% 
  gather_residuals(with_term = mod1, with_wday_term = mod2, wday_only = mod3) %>% 
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha=1)
## so performs a little better in spring and a little worse in late summer

#4
make_wday_term <- function(wday, term){
  wday_char <- as.character(wday)
  term_char <- as.character(term)
  if (wday_char == 'Sat') paste(wday_char, "-", term_char, sep="")
  else wday_char
}


#get holiday dates in
holiday_former <- function(date) 
  {
  if (month(date) == 1 && mday(date) == 1) 1
  else if (month(date) == 7 && mday(date) == 4) 1
  else if (month(date) == 12 && mday(date) == 25) 1
  #mlk day
  else if (month(date) == 1 && mday(date) %in% seq(15, 21) && wday(date) == 2) 1
  #memorial day
  else if (month(date) == 5 && mday(date) %in% seq(25, 31) && wday(date) == 2) 1
  #labor day
  else if (month(date) == 9 && mday(date) %in% seq(1, 7) && wday(date) == 2) 1
  #thanksgiving day
  else if (month(date) == 11 && mday(date) %in% seq(24, 30) && wday(date) == 5) 1
  else 0
}

daily2 <- daily %>%
  mutate(wday_term = map2_chr(wday, term, make_wday_term)) %>%
  mutate(wday_term_fct = as.factor(wday_term)) %>%
  mutate(holiday = map_dbl(date, holiday_former)) %>% 
  mutate(wday_holiday = ifelse(holiday == 1, "Holiday", wday_term)) %>% 
  mutate(wday_holiday_fct = as.factor(wday_holiday))

mod1 <- lm(n ~ wday, data = daily2)
mod2 <- lm(n ~ wday_term_fct, data = daily2)
mod3 <- lm(n ~ wday_holiday_fct, data = daily2)

daily2 %>% 
  gather_residuals(wday = mod1, with_wday_term = mod2, wday_holiday_fct = mod3) %>% 
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha=1)
###some improvement on the big residuals, but some induced overestimates. 

#5
daily2 <- daily2 %>% 
  mutate(month = month(date, label=TRUE))

mod1 <- lm(n ~ wday, data = daily2)
mod2 <- lm(n ~ wday * term, data = daily2)
mod3 <- lm(n ~ wday * month, data = daily2)

daily2 %>% 
  gather_residuals(wday = mod1, wday_term = mod2, wday_month = mod3) %>% 
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha=1)
#### averages across month, but term borders and holidays occur within months

#6
#The incoporation of the splines on weekday using * makes predictions closer
#just splitting date into 5 does not do much
mod <- MASS::rlm(n ~ wday + ns(date, 5), data = daily)

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(date, pred, color = wday)) +
  geom_line() +
  geom_point()

#7
#break down distance of evening flights
flights %>% 
  filter(sched_dep_time >= 2000) %>% 
  mutate(date = make_date(year, month, day)) %>% 
  mutate(wday = wday(date, label = TRUE)) %>% 
  ggplot() +
    geom_bar(aes(wday, distance),stat = "summary", fun.y = "mean")
###ever so slightly more on sunday than weekdays

#8 
library(forcats)
levels(daily2$wday)
changed_levels <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")

reorder_days <- function(wday) fct_reorder(wday, changed_levels)

flights %>% 
  filter(sched_dep_time >= 2000) %>% 
  mutate(date = make_date(year, month, day)) %>% 
  mutate(wday = wday(date, label = TRUE)) %>% 
  mutate(wday = fct_relevel(wday, changed_levels)) %>% 
  ggplot() +
  geom_bar(aes(wday, distance),stat = "summary", fun.y = "mean")
  

