# trelliscope-examples3

```{r}
# remotes::install_github("trelliscope/trelliscope")

library(trelliscope)
library(tidyverse)
library(gapminder)

disp <- (ggplot(aes(year, lifeExp), data = gapminder) +
  geom_point() +
  facet_trelliscope(~ continent + country)) |>
  build_panels() |>
  mutate(
    mean_lifeexp = purrr::map_dbl(data, ~ mean(.x$lifeExp)),
    min_lifeexp = purrr::map_dbl(data, ~ min(.x$lifeExp)),
    mean_gdp = purrr::map_dbl(data, ~ mean(.x$gdpPercap)),
    wiki_link = paste0("https://en.wikipedia.org/wiki/", country)
  ) |>
  trelliscope(name = "life expectancy", path = "gapminder") |>
  write_panels(width = 800, height = 500, format = "svg") |>
  add_meta_defs(
    meta_number("mean_gdp",
      label = "Mean of annual GDP per capita (US$, inflation-adjusted)",
      digits = 2),
    meta_href("wiki_link", label = "Wikipedia country page")
  ) |>
  add_meta_labels(
    mean_lifeexp = "Mean of annual life expectancies",
    min_lifeexp = "Lowest observed annual life expectancy"
  ) |>
  set_labels(c("country", "continent", "wiki_link")) |>
  set_layout(nrow = 3, ncol = 5) |>
  set_sort(c("continent", "mean_lifeexp"), dir = c("asc", "desc")) |>
  set_filters(
    filter_string("continent", values = "Africa"),
    filter_range("mean_lifeexp", max = 50)
  ) |>
  add_view(
    name = "Countries with high life expectancy (min >= 60)",
    filter_range("min_lifeexp", min = 60),
    state_sort("min_lifeexp", dir = "desc")
  ) |>
  add_inputs(
    input_text(name = "comments", label = "Comments about this panel",
      width = 100, height = 6),
    input_radio(name = "looks_correct",
      label = "Does the data look correct?", options = c("no", "yes"))
  ) |>
  write_display()
```
