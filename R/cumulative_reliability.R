#' relic

#' @name cumulative_reliability
#' @import tidyverse
#' @export
cumulative_reliability <- function(.data,
                                   varname=NULL,
                                   formula_s=NULL,
                                   id_var="id",
                                   time_var='studyday',
                                   timepoint_range = c(0,1),
                                   avg_obs = NULL) {

  # set variable name to explore reliability ----
  if(is.null(varname) & is.null(formula_s)) {
    stop("Either of `varname` or `formula_s` parameters are required")
  }

  # init storage of results ----
  results <- tibble()

  # decompose ----
  timepoint_min = timepoint_range[1]
  timepoint_max = timepoint_range[2]

  # loop over time ----
  for(timepoint in timepoint_min:timepoint_max) {

    print(paste0("timepoint: 0 - ", timepoint))

    # filter data ----
    timepoint_data <- .data %>%
      filter(!!sym(time_var) >= timepoint_min & !!sym(time_var) <= timepoint+1)

    # specify model ----
    if(is.null(formula_s)) {
      formula_str = paste0(varname," ~ 1 + (1 |", id_var,")")
    } else {
      formula_str = formula_s
    }
    timepoint_fit <- lme4::lmer(formula = formula(formula_str),
                                data = timepoint_data)

    # calculate avg number of obs
    if(is.null(avg_obs)) {
      obs_n_list = avg_obs(timepoint_data, id_var=id_var)
      avg_obs_calc = obs_n_list$summary$mean_obs
      avg_obs_supplied = F
    } else {
      avg_obs_calc = avg_obs
      avg_obs_supplied = T
    }

    # calculate reliability ----
    reliability_results <- calc_reliability(timepoint_fit, avg_obs_calc)

    # create tidy export -----
    export_results <- tibble(icc_adjusted = reliability_results$icc$icc_pkg_adjusted,
                             icc_conditional = reliability_results$icc$icc_pkg_conditional,
                             reliability_betweenperson = reliability_results$reliability_summary$reliability_between_person,
                             formula = formula_str,
                             timepoint = timepoint,
                             min_timepoint = timepoint_range[1],
                             max_timepoint = timepoint_range[2],
                             outcome_variable = varname,
                             avg_obs = avg_obs_calc,
                             avg_obs_supplied = avg_obs_supplied)

    # bind with rest of results ----
    results <- bind_rows(results, export_results)
  }
  return(results)
}
