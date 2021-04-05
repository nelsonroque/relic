#' relic

#' @name avg_obs
#' @param .data class: dataframe
#' @param id_var class: string
#' @import tidyverse
#' @examples
#' avg_obs(data, id_var="id")
#' @export
avg_obs <- function(.data, id_var="id") {

  # calculate number of observations per id -----
  obs_n_by_id <- .data %>%
    group_by(!!sym(id_var)) %>%
    summarise(n = n())

  # calculate average observations -----
  obs_summary <- obs_n_by_id %>%
    summarise(mean_obs = mean(n, na.rm=T),
              min_obs = min(n, na.rm=T),
              max_obs = max(n, na.rm=T),
              sd_obs = sd(n, na.rm=T))

  # create export data structure -----
  export <- list(n_obs_per_id = obs_n_by_id, summary = obs_summary)
  return(export)
}
