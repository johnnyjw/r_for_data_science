library(tidyverse)

#p84
ggplot(data  =diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>% 
  count(cut)

#85
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>% 
  count(cut_width(carat, 0.5))
#86
smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

#87
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

#88 old faithful
ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_histogram(binwidth = 0.25)

#88-89 looking at outliers - only shows up as abnormal widths of y
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

#looking at low outliers by zooming to small values
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  arrange(y)

unusual

#p90 exercises
#1
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = x), binwidth = 0.5)
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = z), binwidth = 0.5)
#2
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = price))
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = price), binwidth = 5000)
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = price), binwidth = 100)

#3
diamonds %>% 
  filter(carat == 0.99) %>% 
  count()
diamonds %>% 
  filter(carat == 1.00) %>% 
  count()

#4 coord cartesian vs xlim/ylim
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y)) +
  coord_cartesian(ylim = c(0, 50))

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y)) +
  ylim( c(0, 50))

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y)) +
  coord_cartesian(ylim = c(0, 5))

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y)) +
  ylim( c(0, 5))

#91 dealing with dodgy values
#could delete entire row
diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))
#replace with missing values
diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

#plotting including missing values
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point()
#get rid of the warning using na.rm
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

#92 looking at info about cancellations
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) +
    geom_freqpoly(
      mapping = aes(color = cancelled),
      binwidth = 1/4
    )

#exercises p93
#
hmm <- data.frame(
  a = c('a', 'a', 'a', 'b', 'b', 'b', NA),
  b = c(1,1,1,2,2,2,NA)
)

ggplot(data=hmm, mapping=aes(x=a)) +
  geom_bar()

ggplot(data=hmm, mapping=aes(x=b)) +
  geom_histogram(binwidth=1)

hmm %>% 
  summarise(b_mean = mean(b),
         b_sum = sum(b),
         b_mean_narm = mean(b, na.rm = TRUE),
         b_sum_narm = mean(b, na.rm = TRUE))

#p94 disparities in populations mask differences in trends
ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(data=diamonds) +
  geom_bar(mapping = aes(x = cut))

#display density instead
ggplot(
  data = diamonds,
    mapping = aes(x = price, y = ..density..)
) +
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

#boxplot p 96
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

#p97 reordering
ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  )

#horizontal
ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  ) +
  coord_flip()


#exercises page 99
#1 change cancelled/noncancelled visualization
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(x = sched_dep_time, y = ..density..)) +
  geom_freqpoly(
    mapping = aes(color = cancelled),
    binwidth = 1/4
  )

#2
colnames(diamonds)
ggplot(data=diamonds) +
  geom_point(mapping = aes(x=carat, y=price))

colnames(diamonds)
ggplot(data=diamonds) +
  geom_freqpoly(mapping = aes(x = carat, y = ..density.., colour=cut))

#3
require(ggstance)
require(lvplot)
ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  ) +
  coord_flip()

ggplot(data = mpg) +
  geom_boxploth(
    mapping = aes(
      y = reorder(class, hwy, FUN = median),
      x = hwy
    )
)

ggplot(data=diamonds) +
  geom_lv(
    mapping=aes(y=price, x=cut)
  )

#5
ggplot(data=diamonds) +
  geom_violin(mapping = aes(x=cut, y=price))

