# trelliscope-examples3

This repository contains examples of trelliscope displays matching those found [here]() but using the latest Trelliscope R package.

One-time R setup:

```r
remotes::install_github("trelliscope/trelliscope")
install.packages("tidyverse")
install.packages("gapminder")
```

Each session setup:

```r
# load_all("../trelliscope")
library(trelliscope)
library(tidyverse)
library(gapminder)

# we'll place all displays in a temporary directory:
path <- tempfile()
dir.create(path)
```

## "Bells and whistles" display

This display, found in the directory "gapminder_bells", shows many of the features all in one. These features include:

#### Pre-specified views

A user that generates a display can pre-specify views that they think a viewer might be interested in. This can help viewers navigate to interesting states of the display without understanding all of the controls. When these views are present, an extra icon in the left sidebar appears called "Views". Currently views just update the window location hash to whatever in specified in the view. This could be made more elegant.

#### User inputs

Currently annotations made by the user are only stored in local storage and there is an interface where you can download all of the annotations or have an email drafted that will send them to someone (this was a specific request from a user but we could make the user experience for this more general).

#### Cognostic groups

Displays can have so many cognostics to the point that it is difficult to find a cognostic you might want to filter or sort on. You can now specify cognostic "groups" that will help visually organize how the cognostics are shown in the sort and filter sidebars. Below I'm using `auto_cog = TRUE` which will analyze what is being plotted and create relavent statistical summaries and place them into groups.

```{r}
library(trelliscope)
library(tidyverse)
library(gapminder)

# mix up dates so we can test sorting
scramble <- function(x)
  x + sample(-10:10, length(x), replace = TRUE)
cent <- readr::read_csv("country_centroids.csv")

p <- (ggplot(aes(year, lifeExp), data = gapminder) +
  geom_point() +
  theme_minimal() +
  facet_panels(~ continent + country)) |>
  nest_panels()

set.seed(1234)
stats <- gapminder |>
  mutate(
    yeardate = as.Date(paste0(year, "-01-01")),
  ) |>
  group_by(country, continent) |>
  summarise(
    mean_lifeexp = mean(lifeExp),
    min_lifeexp = min(lifeExp),
    mean_gdp = mean(gdpPercap),
    test = log10(mean(gdpPercap)),
    start_dt = scramble(min(yeardate)),
    end_dt = scramble(max(yeardate)),
    start_dttm = as.POSIXct(start_dt),
    end_dttm = as.POSIXct(end_dt),
    wiki_link = paste0("https://en.wikipedia.org/wiki/", country[1]),
    .groups = "drop"
  ) %>%
  left_join(select(cent, -country), by = c(country = "name"))

x <- left_join(p, stats, by = c("country", "continent"))

xtr <- x |>
  as_trelliscope_df(name = "life expectancy", path = "gapminder_bells") |>
  write_panels(width = 800, height = 500, format = "svg") |>
  add_meta_defs(
    # meta_currency("mean_gdp",
    #   label = "Mean of annual GDP per capita (US$, inflation-adjusted)",
    #   tags = c("statistics", "GDP")),
    meta_href("wiki_link", label = "Wikipedia country page"),
    meta_geo("geo_centroid", latvar = "latitude", longvar = "longitude")
  ) |>
  add_meta_labels(
    mean_lifeexp = "Mean of annual life expectancy",
    min_lifeexp = "Lowest observed annual life expectancy"
  ) |>
  add_meta_tags(
    mean_lifeexp = c("statistics", "life expectancy"),
    min_lifeexp = c("statistics", "life expectancy"),
    country = "geography",
    continent = "geography"
  ) |>
  set_default_labels(c("country", "continent", "wiki_link")) |>
  set_default_layout(nrow = 3, ncol = 5) |>
  set_default_sort(c("continent", "mean_lifeexp"), dir = c("asc", "desc")) |>
  set_default_filters(
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
  add_input_email("johndoe123@fakemail.net") |>
  write_trelliscope()

view_trelliscope(xtr)
```

## Multiple dislays and related displays

This display can be found in the "gapminder_reldisp" folder.

You can have multiple displays in a single app instance. In this case, when the app opens, a list of displays to choose from will appear. I'm not sure how often this feature is used.

On this same topic, there is the notion of "related displays", where if you create multiple displays on the same partitioning of the same dataset in the same output path, you have the option in the UI to view the two different displays side-by-side.

For example, we can create two displays with the gapminder data, one with life expectancy vs. time and another with GBP vs. time:

```r
x1 <- (ggplot(gapminder, aes(year, lifeExp)) +
  geom_point() +
  xlim(1948, 2011) + ylim(10, 95) + theme_minimal() +
  facet_panels(~ country + continent)) |>
  nest_panels() |>
  as_trelliscope_df(
    name = "gapminder_life_expectancy",
    desc = "life expectancy vs. year by country using Gapminder data",
    path = "gapminder_reldisp"
  ) |>
  write_panels(width = 800, height = 500, format = "svg") |>
  write_trelliscope()

x2 <- (ggplot(gapminder, aes(year, log10(gdpPercap))) +
  geom_point() +
  xlim(1948, 2011) + ylim(2.35, 5.1) + theme_minimal() +
  facet_panels(~ country + continent)) |>
  nest_panels() |>
  as_trelliscope_df(
    name = "gapminder_gdp",
    desc = "GDP vs. year by country using Gapminder data",
    path = "gapminder_reldisp"
  ) |>
  write_panels(width = 800, height = 500, format = "svg") |>
  write_trelliscope()

view_trelliscope(x1)
view_trelliscope(x2)
```

When the display opens, choose either display to view. Then click the folder icon with a plus sign that can be found in the top left toolbar, and choose the second display. Now each display will be shown side-by-side for each country. Note that when using related diplays, the panel layout is forced to one row and one column.

The app knows which panels from the two different displays to show based on its panel "signature", which is an md5 hash of the partitioning variables for the displays. For example, since both displays above have the same partitioning, we should expect a panel in each of the displays to have the same signature, e.g. md5 hash of Africa/Tanzania, etc.

## Panels that are not raster images

```r
library(visNetwork)

nnodes <- 100
nnedges <- 1000

nodes <- data.frame(
  id = 1:nnodes,
  label = 1:nnodes, value = rep(1, nnodes))
edges <- data.frame(
  from = sample(1:nnodes, nnedges, replace = T),
  to = sample(1:nnodes, nnedges, replace = T)) |>
    group_by(from, to) |>
    summarise(value = n())

network_plot <- function(id, hide_select = TRUE) {
  style <- ifelse(hide_select,
    "visibility: hidden; position: absolute", "")

  visNetwork(nodes, edges) |>
    visIgraphLayout(layout = "layout_in_circle") |>
    visNodes(fixed = TRUE,
      label = id,
      scaling = list(
        min = 20, max = 50,
        label = list(min = 35, max = 70,
          drawThreshold = 1, maxVisible = 100))) |>
    visEdges(scaling = list(min = 5, max = 30)) |>
    visOptions(highlightNearest = list(enabled = TRUE, degree = 0,
      hideColor = "rgba(200,200,200,0.2)"),
      nodesIdSelection = list(selected = as.character(id), style = style))
}

nodedat <- edges |>
  group_by(from) |>
  summarise(n_nodes = n(), tot_conns = sum(value)) |>
  rename(id = from) |>
  arrange(-n_nodes) |>
  mutate(panel = map_plot(id, network_plot))

network_plot(1)

x3 <- nodedat |>
  arrange(-n_nodes) |>
  as_trelliscope_df(name = "connections",
    path = "network_nonraster") |>
  write_panels(width = 500, height = 500) |>
  set_default_layout(nrow = 2, ncol = 4) |>
  write_trelliscope()

view_trelliscope(x3)
```

## Image panels

```r
load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_7261/datasets/pokemon.Rdata"))


pokemon |>
  mutate(evolves_from_species_id = as.integer(evolves_from_species_id))


pk <- pokemon |>
  mutate(panel = img_panel(url_image)) |>
  as_trelliscope_df(name = "pokemon", path = "pokemon") |>
  set_default_layout(nrow = 3, ncol = 6) |>
  write_trelliscope()

view_trelliscope(pk)
```

## Missing values

```r
gapminder2 <- gapminder
gapminder2$country[c(2, 456, 78, 23, 789, 132)] <- NA
gapminder2$continent[c(543, 567, 12, 89, 7, 234)] <- NA

p <- (ggplot(aes(year, lifeExp), data = gapminder2) +
  geom_point() +
  theme_minimal() +
  facet_panels(~ continent + country)) |>
  nest_panels()

stats <- gapminder2 |>
  mutate(
    yeardate = as.Date(paste0(year, "-01-01")),
  ) |>
  group_by(country, continent) |>
  summarise(
    mean_lifeexp = mean(lifeExp),
    min_lifeexp = min(lifeExp),
    mean_gdp = mean(gdpPercap),
    test = log10(mean(gdpPercap)),
    start_dt = scramble(min(yeardate)),
    end_dt = scramble(max(yeardate)),
    start_dttm = as.POSIXct(start_dt),
    end_dttm = as.POSIXct(end_dt),
    wiki_link = paste0("https://en.wikipedia.org/wiki/", country[1]),
    .groups = "drop"
  ) %>%
  left_join(select(cent, -country), by = c(country = "name"))

x <- left_join(p, stats, by = c("country", "continent"))

xtr <- x |>
  as_trelliscope_df(name = "life expectancy", path = "gapminder_na") |>
  write_panels(width = 800, height = 500, format = "svg") |>
  add_meta_defs(
    meta_href("wiki_link", label = "Wikipedia country page")
  ) |>
  set_default_labels(c("country", "continent", "wiki_link")) |>
  set_default_layout(nrow = 3, ncol = 5) |>
  set_default_sort(c("continent", "mean_lifeexp"), dir = c("asc", "desc")) |>
  set_default_filters(
    filter_range("mean_lifeexp", max = 50)
  ) |>
  write_trelliscope()

view_trelliscope(xtr)
```

## With built JS library


