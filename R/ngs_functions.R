load_week_ngs <- function(season, week, type, session) {
  # for testing
  # season <- 2020
  # week <- 5
  # type <- "passing"
  # session <- rvest::session("https://nextgenstats.nfl.com/stats/top-plays/fastest-ball-carriers")

  if (week == 0) {
    cli::cli_ul("Loading {season} overall {type} stats...")
  } else {
    cli::cli_ul("Loading {season} week {week} {type} stats...")
  }

  max_reg <- ifelse(season >= 2021, 18, 17)
  min_post <- max_reg + 1
  max_post <- min_post + 5

  if (week == 0) {
    path <- glue::glue("https://appapi.ngs.nfl.com/statboard/{type}?season={season}&seasonType=REG")
  } else if (week %in% 1:max_reg) {
    path <- glue::glue("https://appapi.ngs.nfl.com/statboard/{type}?season={season}&seasonType=REG&week={week}")
  } else if (week %in% min_post:max_post) {
    path <- glue::glue("https://appapi.ngs.nfl.com/statboard/{type}?season={season}&seasonType=POST&week={week}")
  }

  response <- httr::POST(
    url = path,
    session$config,
    httr::config(referer = session$url),
    httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.86 Safari/537.36"),
    handle = session$handle
  ) |>
    httr::content()

  info <- response |>
    purrr::discard(is.list) |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    dplyr::mutate(week = week)

  teams <- readRDS("R/teams_colors_logos.rds") |>
    dplyr::select(team_id, team_abbr) |>
    dplyr::filter(!team_abbr %in% c("LA", "OAK", "STL", "SD"))

  data <- response |> purrr::pluck("stats")

  if (!is.null(data) & length(data) > 0) {
    stats <- data |>
      purrr::map_dfr(function(x) {
        st <- x |>
          purrr::discard(is.list) |>
          tibble::as_tibble_row()
        pl <- x |> purrr::pluck("player")
        pl <- pl |> purrr::set_names(paste0("player_", names(pl)))
        return(dplyr::bind_cols(st, pl))
      }) |>
      janitor::clean_names() |>
      dplyr::rename(
        team_id = dplyr::first(tidyselect::contains("team_id"))
      ) |>
      dplyr::left_join(teams, by = "team_id") |>
      dplyr::select(-tidyselect::any_of(c("season", "season_type", "week")))

    out <- dplyr::bind_cols(info, stats) |>
      dplyr::select(
        tidyselect::any_of(c("season", "season_type", "week")),
        tidyselect::any_of(c("player_display_name", "player_position", "team_abbr")),
        tidyselect::any_of(get(paste0(type, "_stats"))),
        tidyselect::any_of(player_info)
      ) |>
      dplyr::select(-tidyselect::any_of(c(
        "player_nfl_id", "player_esb_id", "player_current_team_id",
        "player_season", "player_football_name", "player_headshot",
        "player_gsis_it_id", "player_ngs_position"
      )))
  } else {
    out <- tibble::tibble()
  }
 out
}

save_ngs_data <- function(seasons) {
  most_recent <- nflreadr::most_recent_season()
  if (!all(seasons %in% 2016:most_recent)) {
    cli::cli_abort("Please pass valid seasons between 2016 and {most_recent}")
  }
  session <- rvest::session("https://nextgenstats.nfl.com/stats/top-plays/fastest-ball-carriers")
  todo <- expand.grid(season = seasons, type = c("passing", "rushing", "receiving"))
  file_dir <- file.path(tempdir(), "ngs")
  if (!dir.exists(file_dir)) dir.create(file_dir)
  purrr::walk2(todo$season, todo$type, save_ngs_type, session, file_dir = file_dir)
  nflversedata::nflverse_upload(
    list.files(file_dir, pattern = "ngs", full.names = TRUE),
    tag = "ngs-season",
    repo = "nflverse/ngs-data"
  )
  cli::cli_alert_success("{.field DONE}")
}

save_ngs_type <- function(season, type = c("passing", "rushing", "receiving"), session, file_dir) {
  max_week <- nflreadr::load_schedules(seasons = season) |>
    dplyr::filter(!is.na(result)) |>
    dplyr::pull(week) |>
    max()
  ngs <- purrr::map2_dfr(season, seq(0, max_week + 1), load_week_ngs, type, session) |>
    # ngs website returned duplicates in 2022 with slightly different values. So
    # we need to distinct on season, week and player ID.
    dplyr::distinct(season, week, player_gsis_id, .keep_all = TRUE)
  saveRDS(ngs, file.path(file_dir, paste0("ngs_", season, "_", type, ".rds")))
}

combine_ngs_data <- function(type){
  cli::cli_progress_step("Combining {.field {type}} data")
  save <- purrr::map(
    seq(2016, nflreadr::most_recent_season()),
    function(s, t){
      glue::glue("https://github.com/nflverse/ngs-data/releases/download/ngs-season/ngs_{s}_{t}.rds") |>
        nflreadr::rds_from_url()
    }, t = type
  ) |>
    purrr::list_rbind()

  nflversedata::nflverse_save(
    save,
    glue::glue("ngs_{type}"),
    glue::glue("Next Gen Stats weekly {type} data"),
    file_types = c("rds", "parquet", "qs", "csv.gz"),
    release_tag = "nextgen_stats"
  )

  cli::cli_progress_done()
}

passing_stats <- c(
  "avg_time_to_throw",
  "avg_completed_air_yards",
  "avg_intended_air_yards",
  "avg_air_yards_differential",
  "aggressiveness",
  "max_completed_air_distance",
  "avg_air_yards_to_sticks",
  "attempts",
  "pass_yards",
  "pass_touchdowns",
  "interceptions",
  "passer_rating",
  "completions",
  "completion_percentage",
  "expected_completion_percentage",
  "completion_percentage_above_expectation",
  "avg_air_distance",
  "max_air_distance"
)

rushing_stats <- c(
  "efficiency",
  "percent_attempts_gte_eight_defenders",
  "avg_time_to_los",
  "rush_attempts",
  "rush_yards",
  "expected_rush_yards",
  "rush_yards_over_expected",
  "avg_rush_yards",
  "rush_yards_over_expected_per_att",
  "rush_pct_over_expected",
  "rush_touchdowns"
)

receiving_stats <- c(
  "avg_cushion",
  "avg_separation",
  "avg_intended_air_yards",
  "percent_share_of_intended_air_yards",
  "receptions",
  "targets",
  "catch_percentage",
  "yards",
  "rec_touchdowns",
  "avg_yac",
  "avg_expected_yac",
  "avg_yac_above_expectation"
)

player_info <- c(
  # "player_nfl_id",
  # "player_esb_id",
  "player_gsis_id",
  "player_first_name",
  "player_last_name",
  # "player_position_group",
  # "player_current_team_id",
  "player_jersey_number",
  # "player_status",
  # "player_season",
  # "player_football_name",
  # "player_headshot",
  # "player_gsis_it_id",
  # "player_ngs_position",
  # "player_ngs_position_group",
  # "player_suffix",
  "player_short_name"
)
