#' relic

#' @name iterative_reliability
#' @import tidyverse
#' @examples
#' iterative_reliability(data, varname, cluster_var='studyday', id_var="id", min_assessment = 0, max_assessment = 1)
#' @export
iterative_reliability <- function(.data, varname, cluster_var='studyday', id_var="id", min_assessment = 0, max_assessment = 1) {
  # set variable name to explore reliability
    cur.varname = varname

    reliability.data <- data.frame()
    for(DAYS in 1:max_day){
      # filter data
      day.data <- data %>%
        filter(!!sym(cluster_var) >= min_day & !!sym(cluster_var) <= DAYS)
      #print(nrow(day.data))
      #cat(DAYS)

      if(autocorrelated){
        # specify models
        day.fit <- lme(fixed= formula(paste0(cur.varname," ~ 1")),
                       random= ~ 1|id,
                       data=day.data,
                       cor=corAR1(),
                       na.action=na.exclude)
      } else {
        # specify models
        day.fit <- lme(fixed= formula(paste0(cur.varname," ~ 1")),
                       random= ~ 1|id,
                       data=day.data,
                       na.action=na.exclude)
      }

      #print(summary(day.fit))

      cur.export <- calc_reliability(day.fit,avg_obs(day.data, id_var=id_var)) %>%
        mutate(days = DAYS,
               variable = varname)

      reliability.data <- rbind(reliability.data,cur.export)

      # clear to not have looping overwriting issues
      rm(day.data)
      rm(day.fit)

    }
  return(reliability.data)
}
