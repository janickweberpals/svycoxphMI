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
  mutate(exposure = rbinom(prob = 0.3, size = 1, n = nrow(survival::lung))) |> 
  # create an id variable
  mutate(id = paste0("ID", seq(1, nrow(survival::lung), 1)))
```

Multiple imputation using `mice:`

``` r
# impute data
data_imp <- mice(
  data = data_miss |> select(-id),
  m = 10,
  seed = 42,
  print = FALSE
  )
```

Apply propensity score matching with replacement within in each imputed
dataset:

``` r
# apply propensity score matching on mids object
data_matched <- matchthem(
  exposure ~ age + sex,
  datasets = data_imp,
  approach = 'within',
  method = 'nearest',
  caliper = 0.2,
  ratio = 1,
  replace = F
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
  expr = coxph(formula = Surv(time, status) ~ exposure, robust = TRUE)
  ) |> 
  pool() |> 
  tidy(exponentiate = TRUE) |> 
  select(term, estimate, std.error)

coxph_results
```

          term estimate std.error
    1 exposure 1.069502 0.1906643

\(b\) `svycoxph` =\> leads to an error

``` r
# coxph result
svycoxph_results <- with(
  data = data_matched,
  expr = svycoxph(formula = Surv(time, status) ~ exposure)
  ) |> 
  pool() |> 
  tidy(exponentiate = TRUE, conf.int = TRUE) |> 
  select(term, estimate, std.error)
```

    1 - level Cluster Sampling design (with replacement)
    With (76) clusters.
    survey::svydesign(ids = ~subclass, weights = ~weights, data = m.data.i)
    1 - level Cluster Sampling design (with replacement)
    With (76) clusters.
    survey::svydesign(ids = ~subclass, weights = ~weights, data = m.data.i)
    1 - level Cluster Sampling design (with replacement)
    With (76) clusters.
    survey::svydesign(ids = ~subclass, weights = ~weights, data = m.data.i)
    1 - level Cluster Sampling design (with replacement)
    With (76) clusters.
    survey::svydesign(ids = ~subclass, weights = ~weights, data = m.data.i)
    1 - level Cluster Sampling design (with replacement)
    With (76) clusters.
    survey::svydesign(ids = ~subclass, weights = ~weights, data = m.data.i)
    1 - level Cluster Sampling design (with replacement)
    With (76) clusters.
    survey::svydesign(ids = ~subclass, weights = ~weights, data = m.data.i)
    1 - level Cluster Sampling design (with replacement)
    With (76) clusters.
    survey::svydesign(ids = ~subclass, weights = ~weights, data = m.data.i)
    1 - level Cluster Sampling design (with replacement)
    With (76) clusters.
    survey::svydesign(ids = ~subclass, weights = ~weights, data = m.data.i)
    1 - level Cluster Sampling design (with replacement)
    With (76) clusters.
    survey::svydesign(ids = ~subclass, weights = ~weights, data = m.data.i)
    1 - level Cluster Sampling design (with replacement)
    With (76) clusters.
    survey::svydesign(ids = ~subclass, weights = ~weights, data = m.data.i)
    1 - level Cluster Sampling design (with replacement)
    With (76) clusters.
    survey::svydesign(ids = ~subclass, weights = ~weights, data = m.data.i)
    1 - level Cluster Sampling design (with replacement)
    With (76) clusters.
    survey::svydesign(ids = ~subclass, weights = ~weights, data = m.data.i)

``` r
svycoxph_results
```

          term estimate std.error
    1 exposure 1.069502 0.1891482
