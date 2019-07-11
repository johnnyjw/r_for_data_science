library('tidyverse')

mpg

#p 5
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# exercises page 6
#1
ggplot(data = mpg)
#2
mtcars
nrow(mtcars)
ncol(mtcars)
#3 
?mpg
#4
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cyl, y = hwy))
#5
ggplot(data = mpg) +
  geom_point(mapping = aes(x = class, y = drv))

#a third dimension
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# p9 or with size - but not so useful for discrete values
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

#transpancy
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

#plot shape
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
#but only 6 shapes at a time

#p10 setting a characteristic value outside mappings
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), colour = "blue")

# exercises page 12
#1 - when you put the value inside the mappings
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = "blue"))
#2
?mpg
str(mpg)
#3
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = displ))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = displ))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = displ))
#4
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class, colour = class))
#5
?geom_point
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color="blue", size=20, stroke = 1)
#6
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))

#p 14 facets
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

#exercises p14
#1
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ displ, nrow = 2)
#2
ggplot(data = mpg) +
  geom_point(mapping = aes(x = drv, y = cyl))

#3
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)