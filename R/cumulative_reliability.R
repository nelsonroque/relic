#' relic

#' @name cumulative_reliability
#' @import tidyverse
#' @examples
#' cumulative_reliability(data, "response_time", id_var="id", time_var='studyday', timepoint_range = c(0,1))
#' @export
cumulative_reliability <- function(.data, varname, id_var="id", time_var='studyday', timepoint_range = c(0,1)) {
  # set variable name to explore reliability ----
    cur.varname = varname

    # init storage of results ----
    results <- tibble()

    # loop over time ----
    for(timepoint in 1:timepoint_range[2]) {

      # filter data ----
      timepoint_data <- .data %>%
        filter(!!sym(cluster_var) >= timepoint_range[1] & !!sym(cluster_var) <= timepoint)

      # specify model ----
      timepoint_fit <- lme(fixed= formula(paste0(cur.varname," ~ 1")),
                     random= ~ 1|id,
                     data=timepoint_data,
                     na.action=na.exclude)

      # calculate reliability ----
      reliability_results <- calc_reliability(day.fit,avg_obs(day.data, id_var=id_var))

      export_results <- tibble(reliability_results$reliability_summary$reliability_between_person) %>%
        mutate(timepoint = timepoint,
               outcome_variable = varname)

      results <- bind_rows(results, export_results)

      # clear to not have looping overwriting issues
    }

  return(reliability.data)
}
