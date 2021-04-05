#' relic

#' @name calc_reliability
#' @param .fit class: lme4 fit object
#' @param avg_obs class: numeric
#' @import tidyverse
#' @import reghelper
#' @import broom.mixed
#' @examples
#' calc_reliability(fit, n_obs=10)
#' @export
calc_reliability <- function(fit, avg_obs=NA) {

  # extract random effects
  tidy_effects <- broom.mixed::tidy(fit)

  # intercept and residual
  Residual <- tidy_effects %>% filter(group=="Residual") %>% select(estimate) %>% as.numeric(.)
  Intercept <- tidy_effects %>% filter(term=="(Intercept)") %>% select(estimate) %>% as.numeric(.)

  # calculate ICC
  ICC_manual <- Intercept/(Intercept+Residual)
  ICC_pkg <- reghelper::ICC(fit)

  # calculate reliability -----
  between_reliability <- Intercept/(Intercept+(Residual/avg_obs))

  # create list to export ----
  export <- list(icc = list(manual_icc = ICC_manual,
                            reghelper_icc = ICC_pkg),
                 reliability = list(between_person = between_reliability),
                 reliability_components = list(intercept = Intercept, residual = Residual),
                 avg_obs = avg_obs,
                 tidy_fit = tidy_effects)
  return(export)
}
