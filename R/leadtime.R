library(gh)
library(ggplot2)
library(dplyr)
library(lubridate)

calculate_cycle_time_gh <- function(repos, token) {
  cycle_time_data <- data.frame(repo = character(),
                                pull_request = integer(),
                                cycle_time = numeric(),
                                stringsAsFactors = FALSE)

  for (repo in repos) {
    pulls_url <- sprintf("https://api.github.com/repos/%s/pulls", repo)
    pulls <- gh::gh(url = pulls_url, .token = token, state = "closed", per_page = 100)

    for (pull in pulls) {
      if (!is.null(pull$merged_at)) {
        created_at <- as.POSIXct(pull$created_at, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
        merged_at <- as.POSIXct(pull$merged_at, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
        days_elapsed <- as.numeric(difftime(merged_at, created_at, units = "days"))

        cycle_time_data <- rbind(cycle_time_data, data.frame(repo = repo,
                                                             pull_request = pull$number,
                                                             cycle_time = days_elapsed,
                                                             stringsAsFactors = FALSE))
      }
    }
  }

  return(cycle_time_data)
}
