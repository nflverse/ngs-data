message <- sprintf("NGS data updated %s (ET)", format(Sys.time(), tz = "America/New_York", usetz = TRUE))

git <- function(..., echo_cmd = TRUE, echo = TRUE, error_on_status = FALSE) {
  callr::run("git", c(...),
             echo_cmd = echo_cmd, echo = echo,
             error_on_status = error_on_status
  )
}

git("commit", "-am", message)
