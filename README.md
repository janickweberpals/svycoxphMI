# README

# svycoxphMI

## svycoxph in ps-matched datasets after multiple imputation

This is a reproducible example on how to use svycoxph in combination
with multiple imputation and propensity score matching using a `mimids`
object from the MatchThem package.

Load packages:

``` r
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("survival"))
suppressPackageStartupMessages(library("mice"))
suppressPackageStartupMessages(library("MatchThem"))
suppressPackageStartupMessages(library("survey"))
```

Create example dataset:

``` r
# load example dataset with missing observations
set.seed(42)
data_miss <- survival::lung |> 
  # randomly assign an exposure
  dplyr::mutate(exposure = rbinom(prob = 0.3, size = 1, n = nrow(survival::lung))) |> 
  # create an id variable
  dplyr::mutate(id = paste0("ID", seq(1, nrow(survival::lung), 1)))
```

Multiple imputation using `mice:`

``` r
# impute data
data_imp <- mice::mice(
  data = data_miss |> dplyr::select(-id),
  m = 10,
  seed = 42,
  print = FALSE
  )
```

Apply propensity score matching with replacement within in each imputed
dataset:

``` r
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
```


    Matching Observations  | dataset: #1 #2 #3 #4 #5 #6 #7 #8 #9 #10

We now want to compare treatment effect estimates for `exposure` when
computed (a) using `coxph` (survival package) and (b) `svycoxph` (survey
package).

\(a\) `coxph`

``` r
# coxph result
coxph_results <- with(
  data = data_matched,
  expr = survival::coxph(formula = survival::Surv(time, status) ~ exposure, robust = TRUE)
  ) |> 
  MatchThem::pool() |> 
  broom::tidy(exponentiate = TRUE) |> 
  dplyr::select(term, estimate, std.error, p.value)

coxph_results
```

          term estimate std.error  p.value
    1 exposure 1.402722 0.2658144 0.206325

\(b\) `svycoxph` =\> leads to an error

``` r
# coxph result
svycoxph_results <- with(
  data = data_matched,
  expr = survey::svycoxph(formula = survival::Surv(time, status) ~ exposure)
  ) |> 
  MatchThem::pool() |> 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)
```