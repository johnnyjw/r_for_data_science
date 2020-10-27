rmarkdown::render(
  "./r4ds/21d_yaml_params.Rmd",
  params = list(my_class = "suv")
)

library(purrr)
#p437
reports <- tibble(
  class = unique(mpg$class),
  filename = stringr::str_c("fuel_economy-", class, ".html"),
  params = purrr::map(class, ~ list(my_class = .))
)
reports


reports %>% 
  select(output_file = filename, params) %>% 
  purrr::pwalk(rmarkdown::render, input = "./r4ds/21d_yaml_params.Rmd")
