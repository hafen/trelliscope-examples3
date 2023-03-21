# https://api.nasa.gov
# Account Email: rhafen@gmail.com
# Account ID: f0490df5-2784-486b-99ba-f15fb7440d12
# EbExakEjT5ZQdc9Bgb5mYVf9qWAKvHlRbW4udNQ6
# Hourly Limit: 1,000 requests per hour
# so wait 4 seconds between requests...
library(httr)

api_key <- "EbExakEjT5ZQdc9Bgb5mYVf9qWAKvHlRbW4udNQ6"

rovers <- c("curiosity", "opportunity", "perseverance", "spirit")
rvdat <- list()
for (rv in rovers) {
  url <- paste0("https://api.nasa.gov/mars-photos/api/v1/manifests/", rv,
    "?api_key=", api_key)
  tmp <- httr::GET(url)
  rvdat[[rv]] <- httr::content(tmp)
}

for (x in rvdat) {
  nm <- tolower(x[[1]]$name)
  message(nm)
  sols <- sapply(x[[1]]$photos, function(x) x$sol)
  res <- vector(mode = "list", length = length(sols))
  for (ii in cli::cli_progress_along(sols)) {
    url <- paste0("https://api.nasa.gov/mars-photos/api/v1/rovers/", nm,
      "/photos?sol=", sols[ii], "&api_key=", api_key)
    tmp <- httr::GET(url)
    tmp2 <- httr::content(tmp)
    if (!is.null(tmp2$error$code))
      stop(tmp2$error$code)
    res[[ii]] <- dplyr::bind_rows(lapply(tmp2$photos, function(p) {
      dplyr::tibble(
        id = p$id,
        sol = p$sol,
        camera_id = p$camera$id,
        camera_name = p$camera$name,
        camera_full_name = p$camera$full_name,
        img_src = p$img_src,
        earth_date = p$earth_date,
        rover_name = p$rover$name
      )
    }))
    Sys.sleep(3)
  }
  res2 <- dplyr::bind_rows(res)
  readr::write_rds(res2, paste0("mars-rover/data/", nm, ".rds"))
}

ff <- list.files("mars-rover/data", full.names = TRUE,
  pattern = "^cur|^opp|^per|^spi")

all <- dplyr::bind_rows(lapply(ff, readr::read_rds))

readr::write_rds(all, "mars-rover/data/all.rds")





load_all("../trelliscope")
library(dplyr)

d <- readr::read_rds("mars-rover/data/all.rds")

d2 <- d %>%
  filter(nchar(img_src) != 63) %>%
  mutate(
    img_src = img_panel(img_src, aspect_ratio = 1),
    earth_date = as.Date(earth_date),
    camera = camera_full_name
  ) %>%
  select(-camera_name, -camera_full_name) %>%
  as_trelliscope_df(
    name = "Mars Rover Photos",
    description = "Image data gathered by NASA's Curiosity, Opportunity, and Spirit rovers on Mars",
    path = "mars",
    key_cols = "id"
  ) %>%
  set_default_layout(ncol = 5) %>%
  set_default_filters(
    filter_range("sol", min = 3000),
    filter_string("rover_name", values = "Curiosity")
  ) %>%
  set_default_labels(c("rover_name", "camera", "earth_date"))

write_trelliscope(d2)
view_trelliscope(d2)
