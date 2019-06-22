
<!-- README.md is generated from README.Rmd. Please edit that file -->

# truelies

[![Travis build
status](https://travis-ci.org/hughjonesd/truelies.svg?branch=master)](https://travis-ci.org/hughjonesd/truelies)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/hughjonesd/truelies?branch=master&svg=true)](https://ci.appveyor.com/project/hughjonesd/truelies)

`truelies` implements Bayesian methods, described in [Hugh-Jones
(2019)](https://link.springer.com/article/10.1007/s40881-019-00069-x),
for estimating the proportion of liars in coinflip-style experiments,
where subjects report a random outcome and are paid for reporting a
“good” outcome.

For R source for the original paper, see
<https://github.com/hughjonesd/GSV-comment>.

## Installation

``` r
devtools::install_github("hughjonesd/truelies")
```

## Example

If you have 33 out of 50 reports of heads in a coin flip experiment:

``` r
library(truelies)
d1 <- update_prior(heads = 33, N = 50, P = 0.5, prior = dunif)
plot(d1)
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r

dist_mean(d1)
#> [1] 0.3120336

# 95% confidence interval, using hdrcde
dist_hdr(d1, 0.95)
#> [1] 0.060000 0.551066
```
