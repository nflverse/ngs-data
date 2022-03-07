source("R/ngs_functions.R")

save_ngs_data(most_recent_season())

purrr::walk(c("passing", "rushing", "receiving"), combine_ngs_data)

upload_nflverse()
