#' @keywords internal
#' @author David Hugh-Jones
#'
#' Distributions are represented by their density functions, which are
#' one-argument R functions.
#'
#' To estimate the proportion of liars in an experiment, use [update_prior()]
#' followed by [dist_mean()]:
#'
#' ```
#' posterior <- update_prior(heads = 33, N = 50, P = 0.5, prior = dunif)
#' dist_mean(posterior)
#' ```
#'
#' To get confidence intervals for an estimate, use [dist_hdr()]:
#'
#' ```
#' dist_hdr(posterior, conf_level = 0.95)
#' ```
#'
#' To test whether two different samples have the same proportion of
#' liars, use [difference_dist()] followed by [dist_hdr()]:
#'
#' ```
#' p2 <- update_prior(heads = 42, N = 49, P = 0.5, prior = dunif)
#' dd <- difference_dist(posterior, p2)
#' dist_hdr(dd, 0.95, bounds = c(-1, 1))
#' ```
#'
#' To test power for detecting a given proportion of liars, use [power_calc()]:
#'
#' ```
#' power_calc(N = 100, P = 0.5, lambda = 0.2)
#' ```
#' @section Testing the package:
#'
#' To run tests on the package:
#'
#' ```
#' source(system.file("test-statistics.R", package = "truelies"))
#' ```
#'
#' This will take some time and will produce a data frame of test results
#' for different parameter values.
#'
#' @references Hugh-Jones, David (2019). True Lies: Comment on Garbarino,
#'   Slonim and Villeval (2018). Journal of the Economic Science Association.
#'   https://link.springer.com/article/10.1007/s40881-019-00069-x.
#'
#' @docType package
"_PACKAGE"
