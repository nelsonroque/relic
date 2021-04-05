#' relic

#' @name calc_reliability
#' @param fit class: lme4 fit object
#' @param avg_obs class: numeric
#' @import tidyverse
#' @import reghelper
#' @import broom.mixed
#' @import performance
#' @import insight
#' @examples
#' calc_reliability(fit, n_obs=10)
#' @export
calc_reliability <- function(fit, avg_obs=NA) {

  # reference:
  # https://easystats.github.io/performance/reference/icc.html

  if (require("lme4")) {

    # extract random effects ----
    tidy_effects <- broom.mixed::tidy(fit)

    # extract variance components -----
    variance <- as.data.frame(lme4::VarCorr(fit))
    var_total <- variance[is.na(variance$var2), 'vcov']
    var_between <- var_total[1:(length(var_total)-1)]

    # extract intercept and residual
    Residual <- tidy_effects %>% filter(group=="Residual") %>% select(estimate) %>% as.numeric(.)
    Intercept <- tidy_effects %>% filter(term=="(Intercept)") %>% select(estimate) %>% as.numeric(.)

    # calculate ICC using various packages/hand calcs for validating concordance ----
    perf_icc <- performance::icc(fit, by_group = FALSE)
    ICC_manual <- (sum(var_between)/sum(var_total))
    ICC_pkg_adjusted = perf_icc$ICC_adjusted
    ICC_pkg_conditional = perf_icc$ICC_conditional
    ICC_pkg_reghelper <- reghelper::ICC(fit)

    # model stats ----
    model_stats = performance::performance(fit)

    # calculate reliability -----
    between_reliability <- Intercept/(Intercept+(Residual/avg_obs))

    # create list to export ----
    export <- list(icc = list(icc_manual = ICC_manual,
                              icc_pkg_reghelper = ICC_pkg_reghelper,
                              icc_pkg_adjusted = ICC_pkg_adjusted,
                              icc_pkg_conditional = ICC_pkg_conditional,
                              in_words = paste0("Proportion of variance that is between-subjects: ", round(ICC_manual,3))),
                   reliability_summary = list(reliability_between_person = between_reliability),
                   reliability_components = list(intercept = Intercept,
                                                 residual = Residual,
                                                 var_total = var_total,
                                                 var_between = var_between),
                   avg_obs = avg_obs,
                   tidy_model_stats = model_stats,
                   tidy_fit = tidy_effects,
                   tidy_variance = variance)
  } else {
    stop("Error: `lme4` package not installed.")
  }

  return(export)
}
