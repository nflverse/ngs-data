pkgload::load_all()
season <- Sys.getenv("NFLVERSE_UPDATE_SEASON", unset = NA_character_) |> as.integer()
type <- Sys.getenv("NFLVERSE_UPDATE_TYPE", unset = NA_character_)
type <- rlang::arg_match0(type, c("passing", "rushing", "receiving", "combine"))

if (type == "combine"){
  purrr::walk(c("passing", "rushing", "receiving"), combine_ngs_data)
} else {
  save_ngs_data(season, type)
}
