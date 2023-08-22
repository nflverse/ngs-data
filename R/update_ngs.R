source("R/ngs_functions.R")

if(Sys.getenv("NFLVERSE_REBUILD", "false") == "true"){
  seasons_to_update <- 2016:nflreadr::most_recent_season()
} else {
  seasons_to_update <- nflreadr::most_recent_season()
}
save_ngs_data(seasons_to_update)

purrr::walk(c("passing", "rushing", "receiving"), combine_ngs_data)

upload_nflverse()
