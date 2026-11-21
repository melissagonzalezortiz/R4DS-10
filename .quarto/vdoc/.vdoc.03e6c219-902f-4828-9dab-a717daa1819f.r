#
#
#
#
#
#
#
#
#| message: false

library(tidyverse)
library(rvest)
library(httr2)
library(purrr)
#
#
#
imdb_snapshots <- readRDS("data/imdb_snapshots.rds") |>
  as_tibble()

imdb_snapshots

imdb_snapshots |>
  count(snap_year)
#
#
#
#| cache: true
#| eval: false
snap_urls <- c(
  "https://web.archive.org/web/20150101/https://www.imdb.com/chart/top/",
  "https://web.archive.org/web/20170101/https://www.imdb.com/chart/top/",
  "https://web.archive.org/web/20190101/https://www.imdb.com/chart/top/",
  "https://web.archive.org/web/20210101/https://www.imdb.com/chart/top/",
  "https://web.archive.org/web/20220201012049/https://www.imdb.com/chart/top/"
)
snap_years <- c(2015, 2017, 2019, 2021, 2022)

scrape_snapshot <- function(url, snap_year) {
  page <- request(url) |>
    req_timeout(60) |>
    req_retry(max_tries = 3, backoff = ~ 5) |>
    req_perform() |>
    resp_body_html()

  tbl <- page |>
    html_element("table") |>
    html_table()

  attr_vals <- page |>
    html_elements("td strong") |>
    html_attr("title")

  tbl |>
    select(rank_title_year = `Rank & Title`,
           rating = `IMDb Rating`) |>
    mutate(
      rank_title_year = str_replace_all(rank_title_year, "\n +", " "),
      number = str_extract(attr_vals, "(?<=based on )[0-9,]+") |>
        parse_number()
    ) |>
    separate_wider_regex(
      rank_title_year,
      patterns = c(rank = "\\d+", "\\. ",
                   title = ".+", " +\\(", year = "\\d+", "\\)")
    ) |>
    mutate(rank = as.integer(rank),
           year = as.integer(year),
           snap_year = snap_year) |>
    select(snap_year, rank, title, year, rating, number)
}

imdb_snapshots <- map2(snap_urls, snap_years, scrape_snapshot) |>
  list_rbind()
#
#
#
rank_change_by_film <- imdb_snapshots |>
  filter(snap_year %in% c(2015, 2022)) |>
  select(snap_year, rank, title, year) |>
  pivot_wider(names_from = snap_year,
              values_from = c(rank, year),
              names_sep = "_") |>
  filter(!is.na(rank_2015), !is.na(rank_2022)) |>
  mutate(rank_change = rank_2015 - rank_2022,
         release_decade = floor(year_2015 / 10) * 10)

rank_change_by_film |>
  group_by(release_decade) |>
  summarize(average_rank_change = mean(rank_change, na.rm = TRUE),
            .groups = "drop") |>
  arrange(release_decade)
#
#
#
snapshot_counts <- imdb_snapshots |>
  distinct(title, snap_year) |>
  count(title, name = "snapshot_count")

snapshot_counts |>
  count(snapshot_count, name = "film_count") |>
  arrange(snapshot_count)
#
#
#
rank_change_by_film |>
  mutate(release_decade = paste0(release_decade, "s")) |>
  group_by(release_decade) |>
  summarize(average_rank_change = mean(rank_change),
            .groups = "drop") |>
  mutate(direction = if_else(average_rank_change > 0, "Rank rose", "Rank fell")) |>
  ggplot(aes(x = average_rank_change, y = release_decade, fill = direction)) +
  geom_col() +
  geom_vline(xintercept = 0, linewidth = 0.8) +
  labs(
    title = "IMDb Top 250 Rank Changes by Film Release Decade",
    subtitle = "Average change in rank from the 2015 snapshot to the 2022 snapshot",
    x = "Average rank change (positive = rank rose)",
    y = "Release decade",
    fill = "Direction",
    caption = "190 films appear in both the 2015 and 2022 snapshots."
  ) +
  theme_minimal()
#
#
#
#
