library(tidyverse)
library(forcats)

#example month
x1 <- c("Dec", "Apr", "Jan", "Mar")
#typo
x2 <- c("Dec", "Apr", "Jam", "Mar")
#sort
sort(x1)

#first create list of valid levels
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1

sort(y1)

y2 <- factor(x2, levels = month_levels)
y2
y2 <- parse_factor(x2, levels = month_levels)

y1 <- factor(x1)
y1

f1 <- factor(x1, levels = unique(x1))
f1
f2 <- x1 %>% factor() %>% fct_inorder()
f2

levels(f2)

#the sample data used for chapter
library(forcats)
gss_cat

gss_cat %>% 
  count(race)
ggplot(gss_cat, aes(race)) +
  geom_bar()

#show all levels
ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

###exs p227
#1
gss_cat$rincome
head(gss_cat, 5)
levels(gss_cat$rincome)
ggplot(gss_cat, aes(rincome)) +
  geom_bar()

#2
ggplot(gss_cat, aes(relig)) +
  geom_bar()
ggplot(gss_cat, aes(partyid)) +
  geom_bar()

ggplot(gss_cat, aes(relig, denom)) +
  geom_point()

#3
gss_cat %>% 
  select(relig, denom) %>% 
  unique() %>% 
  group_by(relig) %>% 
  summarize(n = n())

#modifying factor order
relig <- gss_cat %>% 
  group_by(relig) %>% 
  summarize(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
ggplot(relig, aes(tvhours, relig)) + geom_point()

ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

#in a mutate step
relig %>% 
  mutate(relig = fct_reorder(relig, tvhours)) %>% 
  ggplot(aes(tvhours, relig)) +
    geom_point()

#age across income level
rincome <- gss_cat %>% 
  group_by(rincome) %>% 
  summarize(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
ggplot(
  rincome,
  aes(age, fct_reorder(rincome, age))
  ) +
    geom_point()

#fct relevel
ggplot(
  rincome,
  aes(age, fct_relevel(rincome, "Not applicable"))
) +
  geom_point()

#fct_reorder2
by_age <- gss_cat %>% 
  filter(!is.na(age)) %>% 
  group_by(age, marital) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, color = marital)) +
  geom_line(na.rm = TRUE)

ggplot(
  by_age,
  aes(age, prop, color = fct_reorder2(marital, age, prop))
) +
  geom_line()+
  labs(color = "marital")

gss_cat %>% 
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>% 
  ggplot(aes(marital)) +
    geom_bar()

ggplot(gss_cat, aes(tvhours)) +
  geom_histogram()
##p232 exercises
#1
##tvhours is skewed, perhaps median is better
#2 
head(gss_cat)
levels(gss_cat$marital)
#there is kind of an order but not really
head(gss_cat)
levels(gss_cat$race)
#arbitrary
levels(gss_cat$rincome)
#partly principled
levels(gss_cat$partyid)
#arbitrary
levels(gss_cat$relig)
#arbitrary
levels(gss_cat$denom)
#arbitrary

#3
levels(rincome$rincome)
levels(fct_relevel(rincome$rincome, "Not applicable"))
# moving not applicable to the front puts it at the bottom
# of the plot as the order of the y axis is going upwards from front

#modifying factor levels
gss_cat %>% count(partyid)

gss_cat %>% 
  mutate(partyid = fct_recode(partyid,
    "Republican, strong" = "Strong republican",
    "Republican, weak" = "Not str republican",
    "Independent, near rep" = "Ind,near rep",
    "Independent, near dem" = "Ind,near dem",
    "Democrat, strong" = "Not str democrat",
    "Democrat, weak" = "Strong democrat"
                              )) %>% 
  count(partyid)

gss_cat %>% 
  mutate(partyid = fct_recode(partyid,
      "Republican, strong"    = "Strong republican",
      "Republican, weak"      = "Not str republican",
      "Independent, near rep" = "Ind,near rep",
      "Independent, near dem" = "Ind,near dem",
      "Democrat, strong"      = "Not str democrat",
      "Democrat, weak"        = "Strong democrat",
      "Other"                 = "No answer",
      "Other"                 = "Don't know",
      "Other"                 = "Other party"
  )) %>% 
  count(partyid)

#fct collapse
gss_cat %>% 
  mutate(partyid = fct_collapse(partyid,
        other = c("No answer", "Don't know", "Other party"),
        rep = c("Strong republican", "Not str republican"),
        ind = c("Ind,near rep", "Ind,near dem", "Independent"),
        dem = c("Not str democrat", "Strong democrat")
                                )) %>% 
  count(partyid)

#fct_lump
gss_cat %>% 
  mutate(relig = fct_lump(relig)) %>% 
  count(relig)

#use n= arguement
gss_cat %>% 
  mutate(relig = fct_lump(relig, n=10)) %>% 
  count(relig, sort = TRUE) %>% 
  print(n = Inf)

#exs p 235
head(gss_cat)
#1
gss_cat %>% 
  mutate(partyid = fct_collapse(partyid,
    other = c("No answer", "Don't know", "Other party"),
    rep = c("Strong republican", "Not str republican"),
    ind = c("Ind,near rep", "Ind,near dem", "Independent"),
    dem = c("Not str democrat", "Strong democrat")
  )) %>% 
  group_by(year, partyid) %>% 
  summarize(n_party = n()) %>% 
  group_by(year) %>% 
  mutate(pro_p = n_party / sum(n_party)) %>% 
  ggplot(aes(year, pro_p, colour = partyid)) +
    geom_line()

#2
levels(gss_cat$rincome)
gss_cat %>% 
  count(rincome)
#collapse under 10000 nad combine now answer dont know refused
gss_cat %>% 
  mutate(rincome = fct_collapse(rincome,
    unknown = c("No answer", "Don't know", "Refused"),
    "under$10000" = c("$8000 to 9999", "$7000 to 7999",
                     "$6000 to 6999", "$5000 to 5999",
                     "$4000 to 4999", "$3000 to 3999",
                     "$1000 to 2999", "Lt $1000")
                                )) %>% 
  count(rincome)
