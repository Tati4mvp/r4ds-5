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

state_acres <- fires |>
  dplyr::group_by(state) |>
  dplyr::summarise(
    total_acres = sum(gis_acres, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(total_acres)) |>
  dplyr::slice_head(n = 10)
print(state_acres)
#
#
#
big_fires <- fires |> dplyr::filter(gis_acres >= 100000)
big_fires <- big_fires |>
  dplyr::mutate(
    lon = purrr::map_dbl(
      geometry_coordinates,
      \(boundary) mean(purrr::map_dbl(boundary[[1]], \(pair) pair[[1]]))
    ),
    lat = purrr::map_dbl(
      geometry_coordinates,
      \(boundary) mean(purrr::map_dbl(boundary[[1]], \(pair) pair[[2]]))
    )
  )
#
#
#
agency_counts <- big_fires |> dplyr::count(agency, name = "number_of_fires")

ggplot2::ggplot(agency_counts, ggplot2::aes(x = agency, y = number_of_fires)) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "Big Fires by Managing Agency",
    x = "Managing agency",
    y = "Number of big fires"
  )
#
#
#
largest_big_fires <- big_fires |>
  dplyr::arrange(dplyr::desc(gis_acres)) |>
  dplyr::slice_head(n = 10)

fire_map <- leaflet::leaflet() |>
  leaflet::addProviderTiles("CartoDB.Positron")
all_longitudes <- numeric()
all_latitudes <- numeric()

for (i in seq_len(nrow(largest_big_fires))) {
  boundary <- largest_big_fires$geometry_coordinates[[i]][[1]]
  longitudes <- purrr::map_dbl(boundary, \(pair) pair[[1]])
  latitudes <- purrr::map_dbl(boundary, \(pair) pair[[2]])
  fire <- largest_big_fires[i, ]
  popup <- paste0(
    "<strong>", fire$incident, "</strong><br>",
    "Year: ", fire$fire_year, "<br>",
    "Acres: ", format(fire$gis_acres, big.mark = ",")
  )

  fire_map <- fire_map |>
    leaflet::addPolygons(
      lng = longitudes,
      lat = latitudes,
      popup = popup
    )
  all_longitudes <- c(all_longitudes, longitudes)
  all_latitudes <- c(all_latitudes, latitudes)
}

fire_map |>
  leaflet::fitBounds(
    lng1 = min(all_longitudes),
    lat1 = min(all_latitudes),
    lng2 = max(all_longitudes),
    lat2 = max(all_latitudes)
  )
#
#
#
imdb_snapshots <- readRDS("data/imdb_snapshots.rds") |>
  tibble::as_tibble()
print(imdb_snapshots)
print(imdb_snapshots |> dplyr::count(snap_year))
#
#
#
rank_changes <- imdb_snapshots |>
  dplyr::filter(snap_year %in% c(2015, 2022)) |>
  dplyr::select(title, year, snap_year, rank) |>
  tidyr::pivot_wider(
    names_from = snap_year,
    values_from = rank,
    names_prefix = "rank_"
  ) |>
  dplyr::filter(!is.na(rank_2015), !is.na(rank_2022)) |>
  dplyr::mutate(
    rank_change = abs(rank_2022 - rank_2015),
    release_decade = (year %/% 10) * 10
  ) |>
  dplyr::arrange(dplyr::desc(rank_change)) |>
  dplyr::slice_head(n = 10)
print(rank_changes)
#
#
#
average_rank_change <- rank_changes |>
  dplyr::group_by(release_decade) |>
  dplyr::summarise(
    average_rank_change = mean(rank_change),
    .groups = "drop"
  ) |>
  dplyr::arrange(release_decade)
print(average_rank_change)
#
#
#
nolan_films <- c(
  "The Dark Knight", "Inception", "Interstellar",
  "The Dark Knight Rises", "Memento", "The Prestige", "Batman Begins"
)

nolan_ranks <- imdb_snapshots |>
  dplyr::filter(title %in% nolan_films) |>
  dplyr::select(title, snap_year, rank) |>
  tidyr::pivot_wider(
    names_from = snap_year,
    values_from = rank,
    names_prefix = "rank_"
  )
print(nolan_ranks)
#
#
#
interstellar_ranks <- imdb_snapshots |>
  dplyr::filter(title == "Interstellar")

ggplot2::ggplot(
  interstellar_ranks,
  ggplot2::aes(x = snap_year, y = rank)
) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "Interstellar Rank by Snapshot Year",
    x = "Snapshot year",
    y = "Rank"
  )
#
#
#
film_snapshot_counts <- imdb_snapshots |>
  dplyr::count(title, name = "snapshot_count")
print(film_snapshot_counts)

snapshot_count_distribution <- film_snapshot_counts |>
  dplyr::count(snapshot_count, name = "film_count")

ggplot2::ggplot(
  snapshot_count_distribution,
  ggplot2::aes(x = snapshot_count, y = film_count)
) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "Distribution of Film Snapshot Counts",
    x = "Number of snapshots",
    y = "Number of films"
  )
#
#
#
snap_urls <- c(
  "https://web.archive.org/web/20150101/https://www.imdb.com/chart/top/",
  "https://web.archive.org/web/20170101/https://www.imdb.com/chart/top/",
  "https://web.archive.org/web/20190101/https://www.imdb.com/chart/top/",
  "https://web.archive.org/web/20210101/https://www.imdb.com/chart/top/",
  "https://web.archive.org/web/20220201012049/https://www.imdb.com/chart/top/"
)
snap_years <- c(2015, 2017, 2019, 2021, 2022)

scrape_snapshot <- function(url, snap_year) {
  page <- httr2::request(url) |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 5) |>
    httr2::req_perform() |>
    httr2::resp_body_html()
  tbl <- page |>
    rvest::html_element("table") |>
    rvest::html_table()
  attr_vals <- page |>
    rvest::html_elements("td strong") |>
    rvest::html_attr("title")

  tbl |>
    dplyr::select(
      rank_title_year = `Rank & Title`,
      rating = `IMDb Rating`
    ) |>
    dplyr::mutate(
      rank_title_year = stringr::str_replace_all(rank_title_year, "\n +", " "),
      number = stringr::str_extract(
        attr_vals,
        "(?<=based on )[0-9,]+"
      ) |>
        readr::parse_number()
    ) |>
    tidyr::separate_wider_regex(
      rank_title_year,
      patterns = c(
        rank = "\\d+", "\\. ",
        title = ".+", " +\\(",
        year = "\\d+", "\\)"
      )
    ) |>
    dplyr::mutate(
      rank = as.integer(rank),
      year = as.integer(year),
      snap_year = snap_year
    ) |>
    dplyr::select(snap_year, rank, title, year, rating, number)
}

imdb_snapshots <- purrr::map2(snap_urls, snap_years, scrape_snapshot) |>
  purrr::list_rbind()
#
#
#
#
