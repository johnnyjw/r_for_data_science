library(tidyverse)
library(modelr)
library(xlsx)
library(readxl)

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
