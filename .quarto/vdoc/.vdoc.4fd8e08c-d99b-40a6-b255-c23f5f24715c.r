#
#
#
#
#
#
#
#
#
#| cache: true
#| message: false
library(tidyverse)
library(purrr)
library(leaflet)
library(rvest)
library(httr2)
library(jsonlite)

raw <- read_json("data/wildfires.geojson")
print(length(raw$features))

features <- tibble(features = raw$features)
features <- features |> unnest_wider(features)
features <- features |> unnest_wider(properties)
features <- features |> unnest_wider(geometry, names_sep = "_")

fires <- features |>
  select(incident, gis_acres, fire_year, agency, state, geometry_coordinates) |>
  mutate(
    gis_acres = as.numeric(gis_acres),
    fire_year = as.integer(fire_year)
  )

first_fire <- raw$features[[1]]
print(names(first_fire))

first_coordinate <- first_fire$geometry$coordinates[[1]][[1]]
print(first_coordinate)
#
#
#
august_fires <- fires |> dplyr::filter(stringr::str_detect(incident, "August"))
print(nrow(august_fires))

largest_fires <- fires |>
  dplyr::arrange(dplyr::desc(gis_acres)) |>
  dplyr::slice_head(n = 10)
print(largest_fires)

ggplot2::ggplot(fires, ggplot2::aes(x = gis_acres)) +
  ggplot2::geom_histogram() +
  ggplot2::scale_x_log10()

yearly_acres <- fires |>
  dplyr::group_by(fire_year) |>
  dplyr::summarise(
    total_acres = sum(gis_acres, na.rm = TRUE),
    .groups = "drop"
  )

ggplot2::ggplot(yearly_acres, ggplot2::aes(x = fire_year, y = total_acres)) +
  ggplot2::geom_line() +
  ggplot2::labs(
    title = "Total Acres Burned by Year",
    x = "Fire year",
    y = "Total acres burned",
    caption = "Source: wildfire incident data"
  )
#
#
#
#
