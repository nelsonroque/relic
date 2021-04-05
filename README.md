# relic
An R Package to compute various reliability statistics in longitudinal time-series data.

## Example

```{r}


# load dependencies -----
require(performance)
require(reghelper)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# install custom package for reliability calculations ----
devtools::install_github("nelsonroque/relic", force=T) # install latest version if `force=T`

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(haven) # for loading SAS files
library(tidyverse)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# set data path ----
data_path = "Smartphone Survey and Cognitive Tests Data/"

# load file -----
ss <- read_sas(paste0(data_path, "b1clean_symbol_trial_26feb20.sas7bdat"), NULL)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# create quick report of observations per `id_var` -----
ss_report <- relic::avg_obs(ss, id_var="id")

# n average observations -----
avg_obs_ss = ss_report$summary$mean_obs

# run simple model
fit <- lme4::lmer(response_time ~ 1 + (1|id), data=ss)

# calculate ICC, reliability with all data
rels <- relic::calc_reliability(fit, avg_obs = avg_obs_ss)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# calculate cumulative reliability

# simple unconditional model ----
cumulative_rels <- relic::cumulative_reliability(ss,
                                   varname = "response_time",
                                   id_var="id",
                                   time_var="studyday",
                                   timepoint_range = c(0,3))

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# conditional models with custom formulas ----
cumulative_rels_cf <- relic::cumulative_reliability(ss,
                                          formula = "response_time ~ correct_response + studyday + (1|id)",
                                          id_var="id",
                                          time_var="studyday",
                                          timepoint_range = c(0,3))


```
