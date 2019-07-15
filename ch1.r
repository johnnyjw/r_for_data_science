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

#exercises p15
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

#4
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap( ~ class, nrow = 2)

#5
?facet_wrap

#p 16 running different geoms
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) 

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy)) 

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv)) 

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv)) 

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv),
              show.legend = FALSE) 

#two geoms
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy)) 

#clean up 2 geoms
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth() 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth() 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(
    data = filter(mpg, class == 'subcompact'),
    se = FALSE
  )

#exercises page 20
#2
ggplot(
  data = mpg,
  mapping = aes(x = displ, y = hwy, colour = drv)
) +
  geom_point() +
  geom_smooth(se = FALSE)

#5
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = mpg, 
    mapping = aes(x = displ, y = hwy)
    ) +
  geom_smooth(
    data = mpg, 
    mapping = aes(x = displ, y = hwy)
    )

#6.1
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se = FALSE)

#6.2
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(
    mapping = aes(group = drv),
    se = FALSE
    )

#6.3
ggplot(data = mpg, mapping = aes(x = displ, y = hwy,
                                 colour = drv)) +
  geom_point() +
  geom_smooth(
    se = FALSE
  )

#6.4
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(
    se = FALSE
  )

#6.5
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(colour = drv)) +
  geom_smooth(
    mapping = aes(linetype = drv),
    se = FALSE
  )

#6.6
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = drv))

#22 stat transforms
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))
#same as
ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut))

#24 override default
demo <- tribble(
  ~a,       ~b,
  "bar_1",  20,
  "bar_2",  30,
  "bar_3", 40
)

ggplot(data = demo) +
  geom_bar(
    mapping = aes(x = a, y = b), stat="identity"
  )

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop.., group=1))

?geom_bar

#stat_summary
ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

#ex p 26
#1
?stat_summary

?geom_pointrange

ggplot(data = diamonds) +
  geom_pointrange(
    stat = "summary",
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

#2
ggplot(data = demo) +
  geom_col(
    mapping = aes(x = a, y = b)
  )

#4
?stat_smooth

#5
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop.. ))

#p27 bar and color
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, colour = cut ))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut ))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity ))

#28 non stacked bars
#identity not very useful
ggplot(data = diamonds,
       mapping = aes(x = cut, fill = clarity )) +
  geom_bar(alpha = 1/5, position = "identity")