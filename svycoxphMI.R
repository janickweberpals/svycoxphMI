# packages
library("dplyr")
library("survival")
library("mice")
library("MatchThem")
library("survey")

# load example dataset with missing observations
set.seed(42)
data_miss <- survival::lung |> 
  # randomly assign an exposure
  dplyr::mutate(exposure = rbinom(prob = 0.3, size = 1, n = nrow(survival::lung))) |> 
  # create an id variable
  dplyr::mutate(id = paste0("ID", seq(1, nrow(survival::lung), 1)))

# impute data
data_imp <- mice::mice(
  data = data_miss |> dplyr::select(-id),
  m = 10,
  seed = 42,
  print = FALSE
  )

# apply propensity score matching on mids object
data_matched <- MatchThem::matchthem(
  exposure ~ age + sex,
  datasets = data_imp,
  approach = 'within',
  method = 'nearest',
  caliper = 0.2,
  ratio = 1,
  replace = T
  )

# coxph result
coxph_results <- with(
  data = data_matched,
  expr = survival::coxph(formula = survival::Surv(time, status) ~ exposure, robust = TRUE)
  ) |> 
  MatchThem::pool() |> 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)

# svycoxph result => ?
svycoxph_results <- with(
  data = data_matched,
  expr = survey::svycoxph(formula = survival::Surv(time, status) ~ exposure)
  ) |> 
  MatchThem::pool() |> 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)


