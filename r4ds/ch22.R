library(tidyverse)
library(modelr)
library(xlsx)
library(readxl)
library(ggrepel)
library(lubridate)
library(gridExtra)

#442
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = paste(
      "Fuel efficiency generally decreases with",
      "engine size"
    )
  )


#443 subtitle caption
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = paste(
      "Fuel efficiency generally decreases with",
      "engine size"
    ),
    subtitle = paste(
      "Two seaters (sports cars) are an exception",
      "because of their light weight"
    ),
    caption = "Data from fueleconomy.gov"
  )

#444 replace axes
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    colour = "Car type"
  )

#symbols
df <- tibble(
  x = runif(10),
  y = runif(10)
)

ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i == 1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )


# ex p 445
# 1
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = paste(
      "Fuel efficiency generally decreases with",
      "engine size"
    ),
    subtitle = paste(
      "Two seaters (sports cars) are an exception",
      "because of their light weight"
    ),
    caption = "Data from fueleconomy.gov",
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    colour = "Car type"
  )

# 2
men_in_cars <- mpg %>%
  mutate(TwoSeater = if_else(class=='2seater', 1, 0))

mod <- lm(hwy ~ displ * TwoSeater, data=men_in_cars)

men_in_cars2 <- men_in_cars %>% 
  add_predictions(mod)

# without 2 seater
men_in_cars2 %>% 
  filter(class != '2seater') %>% 
ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  geom_smooth(aes(displ, pred), linetype='dashed') +
  labs(
    title = paste(
      "Fuel efficiency generally decreases with",
      "engine size"
    ),
    subtitle = paste(
      "Two seaters (sports cars) are an exception",
      "because of their light weight - have been removed in this plot.\n",
      "Dashed lines are model predictions."
    ),
    caption = "Data from fueleconomy.gov",
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    colour = "Car type"
  )

# with 2 seater
men_in_cars2 %>% 
  ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  geom_smooth(aes(displ, pred), linetype='dashed') +
  labs(
    title = paste(
      "Fuel efficiency generally decreases with",
      "engine size"
    ),
    subtitle = paste(
      "Two seaters (sports cars) are an exception",
      "because of their light weight - have been included in this plot.\n",
      "Dashed lines are model predictions."
    ),
    caption = "Data from fueleconomy.gov",
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    colour = "Car type"
  )


# 3 - sample plot
ret_sur_sum <- read_excel('./r4ds/ReturnedSurveysSummary.xlsx')

ret_sur_sum %>% 
  ggplot(aes(Country, `S1Q2 - How many adult/adolescent (12 years and older) SCD patients do you see each year at your site?`)) +
  geom_col() +
  labs(
    title = 'Sum of Estimated Number of adult/adolescent (12 years and older) SCD patients seen each year at sites per Country',
    subtitle = 'Section 1, Question 2',
    caption = "Report produced 28-Oct-2020",
    x = 'Country',
    y = 'Patient Total'
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# 445 annotations
best_in_class <- mpg %>% 
  group_by(class) %>% 
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_text(aes(label = model), data = best_in_class)

# try 2
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_label(aes(label = model), 
            data = best_in_class,
            nudge_y = 2,
            alpha = 0.5)
# try 3
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  ggrepel::geom_label_repel(
    aes(label = model),
    data = best_in_class)

# directly on plot
class_avg <- mpg %>% 
  group_by(class) %>% 
  summarize(
    displ = median(displ),
    hwy = median(hwy)
  )

ggplot(mpg, aes(displ, hwy, color = class)) +
  ggrepel::geom_label_repel(aes(label = class),
                            data = class_avg,
                            size = 6,
                            label.size = 0, 
                            segment.color = NA) +
  geom_point() +
  theme(legend.position = "none")  
  
  
  
  ggrepel::geom_label_repel(aes(label = class),
                            data = class_avg,
                            size = 6,
                            label.size = 0, 
                            segment.color = NA) +
  geom_point() +
  theme(legend.position = "none")
  
  # single label
  label <- mpg %>% 
    summarize(
      displ = max(displ),
      hwy = max(hwy),
      label = paste(
        "Increasing engine size is \nrelated to",
        "decreasing fuel economy."
      )
    )

  ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_text(
      aes(label = label),
      data = label,
      vjust = "top",
      hjust = "right"
    )

  
  #labels on border? apply inf value
  label <- tibble(
    displ = Inf,
    hwy = Inf,
    label = paste(
      "Increasing engine size is \nrelated to",
      "decreasing fuel economy."
    )
  )
  
  ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_text(
      aes(label = label),
      data = label,
      vjust = "top",
      hjust = "right"
    )

#writelines
"Increasing engine size is related to decreasing fuel economy." %>% 
stringr::str_wrap(width = 40) %>% 
  writeLines()


# ex 451
# 1
four_labels <- tibble(
  displ = c(Inf, Inf, -Inf, -Inf),
  hwy = c(Inf, -Inf, Inf, -Inf),
  label = c("Top Right", "Bottom Right", "Top Left", "Bottom Left")
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(
    aes(label = label),
    data = four_labels,
    vjust = c("top", "bottom", "top", "bottom"),
    hjust = c("right", "right", "left", "left")
  )

# 2
?annotate
max_disp <- max(mpg$displ)
max_hwy <- max(mpg$hwy)


ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  annotate("text", x=max_disp, y=max_hwy, label = "doosh")

# 3  faceting with labels
#  label tibble also has to have the same facet variable
?geom_text
class_avg <- mpg %>% 
  group_by(class) %>% 
  summarize(
    displ = median(displ),
    hwy = median(hwy)
  )


ggplot(mpg, aes(displ, hwy)) +
  ggrepel::geom_label_repel(aes(label = class),
                            data = class_avg,
                            size = 6,
                            label.size = 0, 
                            segment.color = NA) +
  geom_point() +
  facet_wrap(vars(class))

# 4

?geom_label
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_label(aes(label = model,
                 alpha = 0.8,
                 fill=factor(class)), 
             data = best_in_class,
             nudge_y = 2)


ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_label(aes(label = model,
                 alpha = 0.8,
                 color=factor(class)), 
             data = best_in_class,
             nudge_y = 2)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_label(aes(label = model,
                 alpha = 0.8,
                 color=factor(class),
                 family = "serif"), 
             data = best_in_class,
             nudge_y = 2)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_label(aes(label = model,
                 alpha = 0.8,
                 color=factor(class),
                 fontface = 'bold'), 
             data = best_in_class,
             nudge_y = 2)

# 5 arrow
? arrow

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_segment(aes(2.5, 20, xend = 3.5, yend = 20),
               arrow = arrow(length = unit(0.03, "npc"))) +
  # angle - change from 30 default
  geom_segment(aes(2.5, 25, xend = 3.5, yend = 33),
               arrow = arrow(angle = 60,
                             length = unit(0.09, "npc"))) +
  # ends both - and type closed
  geom_segment(aes(3.5, 40, xend = 4.5, yend = 40),
               arrow = arrow(length = unit(0.03, "npc"),
                             ends = "both",
                             type = "closed"))


# 451 scales
#these two the same
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete()

# 452 breaks/labels
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  scale_y_continuous(breaks = seq(15, 40, by = 5))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)

# 453 presidents
presidential %>% 
  mutate(id = 33 + row_number()) %>% 
  ggplot(aes(start, id)) +
    geom_point() +
    geom_segment(aes(xend = end, yend = id)) +
    scale_x_date(
      NULL,
      breaks = presidential$start,
      date_labels = "'%y"
    )

# 454 legend position
base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right")

base + theme(legend.position = "none")

# 455 legend controls
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(
      nrow = 1,
      override.aes = list(size = 4)
    )
  )

# 456 replacing scales
ggplot(diamonds, aes(carat, price)) +
  geom_bin2d()

# log transform
ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d()

# labels not nice...do this instead
ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() +
  scale_x_log10() +
  scale_y_log10()

# 456 color scales
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv)) +
  scale_color_brewer(palette = "Set1")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_color_brewer(palette = "Set1")

# manual define 459
presidential %>% 
  mutate(id = 33 + row_number()) %>% 
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(
    values = c(Republican = "red", Democratic = "blue")
  )

#color viridis
df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)


ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis() +
  coord_fixed()

# ex 460 #1
ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_color_gradient(low = "white", high = "red") +
  coord_fixed()

?scale_color_gradient
ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed()

# answer: should have used scale_fill_gradient rather than scale color!

# 2
?labs
?scale_x_continuous

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  scale_x_continuous("melon farmer") +
  labs(y="doosh")

# 3 
# a
presidential %>% 
  mutate(id = 33 + row_number()) %>% 
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(
    values = c(Republican = "red", Democratic = "blue")
  ) +
  scale_x_date(
    NULL,
    breaks = presidential$start,
    date_labels = "'%y"
  )
# 3b
presidential %>% 
  mutate(id = 33 + row_number(),
         x_label = id + 0.25) %>%
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(
    values = c(Republican = "red", Democratic = "blue")
  ) +
  scale_x_date(
    NULL,
    breaks = presidential$start,
    date_labels = "'%y"
  ) +
  scale_y_continuous("US President Number",
                     breaks = seq(33, 45, by = 1),
                     minor_breaks = NULL) +
  ggrepel::geom_label_repel(
    aes(label = name)
  )

breaks_df <- tibble(
  number = seq(0, 76, by = 4)) %>% 
    mutate(date_seq = ymd("1945-01-20") + dyears(number))



presidential %>% 
  mutate(id = 33 + row_number(),
         y_label = id + 0.35) %>%
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(
    values = c(Republican = "red", Democratic = "blue")
  ) +
  scale_x_date(
    "Years covered by Presidency",
    breaks = breaks_df$date_seq,
    date_labels = "'%y"
  ) +
  scale_y_continuous("US President Number",
                     breaks = seq(33, 45, by = 1),
                     minor_breaks = NULL) +
  guides(
    color = guide_legend(
      nrow = 2,
      override.aes = list(size = 4,
                          shape = "circle")
    )
  ) +
  geom_label(
    aes(start,
        y_label,
        label = name),
    show.legend = FALSE
  )

# 4
ggplot(diamonds, aes(carat, price)) +
  geom_point(aes(color = cut), alpha = 1/20) +
  guides(
    color = guide_legend(
      override.aes = list(size = 4,
                          alpha = 1/2)
    ))


# 461 zooming
ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5,7), ylim = c(10, 30))

mpg %>% 
  filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>% 
    ggplot(mapping = aes(displ, hwy)) +
      geom_point(aes(color = class)) +
      geom_smooth() 

suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class=="compact")

p1 <- ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point()
p2 <- ggplot(compact, aes(displ, hwy, color = drv)) +
  geom_point()

grid.arrange(p1, p2, ncol=2)

#share scales across multiple plots
x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv))

p1 <- ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale
p2 <- ggplot(compact, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

grid.arrange(p1, p2, ncol=2)

# themes p 462
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_bw()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_light()

# 464 saving
ggsave('my_plot.pdf')

# 466
fig.width = 6
out.width = .7
ggplot(mpg, aes(displ, hwy)) +
  geom_point()
ggsave('my_plot_6.pdf')

fig.width = 4
ggplot(mpg, aes(displ, hwy)) +
  geom_point()
ggsave('my_plot_4.pdf')

