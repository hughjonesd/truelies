
# Bayesian ways to calculate the probability of people in a sample reporting heads
# when they observe tails


#' @param lambda Probability of a subject lying
#' @param heads Number of good outcomes reported
#' @param N Total number in sample
#' @param P Probability of *bad* outcome
#' @param prior Prior over lambda. A function which takes a vector of values
#'   between 0 and 1, and returns the probability density.
#'
#' @name basic_params
NULL


#' Calculate the probability of observing `heads`
#' good outcomes out of `N` total outcomes, conditional
#' on subjects lying with prob `lambda` if they see heads.
#'
#' @inherit basic_params params
#' @return A vector of probabilities
#'
#' @noRd
prob_report_given_lambda <- function (lambda, heads, N, P) {
  stopifnot(is_prob(lambda), 0 <= heads, heads <= N, is_prob(P))

  # on average, heads are reported 1-P + P*lambda of the time
  stats::dbinom(heads, N, 1 - P + lambda * P)
}



try_integral <- function(f, a, b, npoints = 100) {
  integ <- stats::integrate(f, a, b, subdivisions = npoints) # numerical integration
  if (integ$message != "OK") stop("stats::integrate failed with message: ", integ$message)

  return(integ$value)
}

#' Calculate posterior of lambdas given prior, and heads reports of good
#' outcome
#'
#' @inherit basic_params params
#' @param npoints How many points to integrate on?
#'
#' @return The probability density of the posterior distribution, as a one-argument
#'   function.
#'
#' @examples
#' posterior <- update_prior(heads = 30, N = 50, P = 0.5, prior = stats::dunif)
#' plot(posterior)
#' @export
update_prior <- function(heads, N, P, prior, npoints = 1e3) {
  stopifnot(heads <= N, heads >= 0, is_prob(P), is.function(prior))

  f <- function (lprime) prior(lprime) * prob_report_given_lambda(lprime, heads, N, P)
  integ <- try_integral(f, 0, 1)
  denominator <- integ

  posterior <- function(lambda) {
    prob_report_given_lambda(lambda, heads, N, P) * prior(lambda)/denominator
  }

  class(posterior) <- c("densityFunction", "function")
  attr(posterior, "limits") <- c(0, 1)

  return(posterior)
}



#' Estimate probability that two samples s1, s2 from 2 independent distributions have s1 > s2
#'
#' @param dist1 Density of distribution 1, as a one-argument function.
#' @param dist2 Density of distribution 2.
#'
#' @return A probability scalar.
#'
#' @examples
#'
#' d1 <- update_prior(30, 50, P = 0.5, prior = stats::dunif)
#' d2 <- update_prior(25, 40, P = 0.5, prior = stats::dunif)
#' compare_dists(d1, d2)
#'
#' @export
compare_dists <- function (dist1, dist2) {
  outer_f <- function (l1) {
    dist1(l1) * dist_prob_within(dist2, 0, l1)
  }
  outer_f <- Vectorize(outer_f)
  prob_1_more <- try_integral(outer_f, 0, 1)

  return(prob_1_more)
}


#' Given dist1 and dist2, find the pdf of dist1 - dist2
#'
#' At the moment this only works when dist1 and dist2 are defined on `[0, 1]`.
#'
#' @param dist1,dist2 Probability density functions
#'
#' @return A probability density function defined on `[-1, 1]`.
#'
#' @examples
#'
#' d1 <- update_prior(30, 50, P = 0.5, prior = stats::dunif)
#' d2 <- update_prior(25, 40, P = 0.5, prior = stats::dunif)
#' dd <- difference_dist(d1, d2)
#'
#' @export
difference_dist <- function (dist1, dist2) {

  # prob l1 - l2 = d is integral from 0 to 1-d of dist1(z+d)*dist2(z)
  # l1 - l2 = -.5; l1 = l2 + .5; integral from .5 to 1 of dist1(z + d)*dist2(z)
  dd_fun <- function (d) {
    stopifnot(d >= -1 && d <= 1)
    dist_prob <- function(x) dist1(x + d) * dist2(x)
    dist_prob <- Vectorize(dist_prob)
    ends <- if (d > 0) c(0, 1 - d) else c(-d, 1)
    try_integral(dist_prob, ends[1], ends[2])
  }

  dd_fun <- Vectorize(dd_fun)
  class(dd_fun) <- c("densityFunction", "function")
  attr(dd_fun, "limits") <- c(-1, 1)

  return(dd_fun)
}


#' Find mean of a probability distribution function
#'
#' @param dist A one-argument function
#' @param l Minimum value
#' @param r Maximum value
#'
#' @return A scalar
#'
#' @examples
#'
#' d1 <- update_prior(10, 40, P = 5/6, prior = stats::dunif)
#' dist_mean(d1)
#'
#' @export
dist_mean <- function (dist, l = 0, r = 1) {
  stopifnot(is.function(dist))

  f <- function (x) x * dist(x)
  EV <- try_integral(f, l, r)

  return(EV)
}

#' Find probability of a probability distribution function within given bounds
#'
#' @inherit dist_mean params
#'
#' @return A scalar between 0 and 1.
#'
#' @examples
#'
#' dist_prob_within(stats::dunif, 0.5, 0.7)
#'
#' d1 <- update_prior(33, 50, P = 0.5, prior = stats::dunif)
#' dist_prob_within(d1, 0.1, 0.2)
#'
#' @noRd
dist_prob_within <- function (dist, l, r) {
  stopifnot(l <= r, is.function(dist))
  try_integral(dist, l, r)
}


#' Find quantiles given a probability distribution function
#'
#' @param dist A one argument function
#' @param probs A vector of probabilities
#' @param bounds A length 2 vector of the bounds of the distribution's support
#'
#' @return A vector of quantiles
#'
#' @examples
#'
#' d1 <- update_prior(33, 50, P = 0.5, prior = stats::dunif)
#' dist_quantile(d1, c(0.025, 0.975))
#'
#' @export
dist_quantile <- function(dist, probs, bounds = c(0, 1)) {
  stopifnot(is_prob(probs), is.function(dist))

  qs <- sapply(probs, function (prob) {
    f <- function (x) (dist_prob_within(dist, bounds[1], x) - prob)^2
    # want x s.t. interval_prob(dist, 0, x) == prob
    res <- stats::optimize(f, bounds)
    res$minimum
  })

  return(qs)
}


#' Compute highest density region for a given confidence level
#'
#' This is a wrapper for `hdrcde::hdr`. The highest density region is the
#' interval that covers `conf_level` of the data and has the highest
#' average density. See:
#'
#' Rob J Hyndman (1996) “Computing and graphing highest density regions”. American Statistician,
#' 50, 120-126.
#'
#'
#' @param dist A one-argument function
#' @param conf_level A scalar between 0 and 1
#' @param bounds A length 2 vector of the bounds of the distribution's support
#'
#' @return A length 2 vector of region endpoints
#'
#' @examples
#'
#' d1 <- update_prior(33, 50, P = 0.5, prior = stats::dunif)
#' dist_hdr(d1, 0.95)
#'
#' @export
dist_hdr <- function (dist, conf_level, bounds = c(0, 1)) {
  stopifnot(is_prob(conf_level), is.function(dist))

  eval_at <- seq(bounds[1], bounds[2], 0.01)
  eval_at_y <- dist(eval_at)
  eval_at <- c(bounds[1] - 0.01, eval_at, bounds[2] + 0.01)
  eval_at_y <- c(0, eval_at_y, 0)
  density_est <- list(x = eval_at, y = eval_at_y)

  # we include prob 0.1 to make sure that result has two columns
  result <- hdrcde::hdr(den = density_est, prob = 100 * conf_level)
  result <- result$hdr[1, ]
  if (length(result) != 2) {
    warning("hdrcde::hdr returned only one endpoint. Guessing which it is.")
    if (isTRUE(all.equal(dist(0), 0))) result <- c(result, 1) else
          if (isTRUE(all.equal(dist(1), 0))) result <- c(0, result) else
          if (all(diff(density_est$y) >= 0)) result <- c(result, 1) else
          if (all(diff(density_est$y) <= 0)) result <- c(0, result)
    if (length(result) == 1) stop(
            "Couldn't figure out which endpoint hdrcde::hdr returned.")
  }
  result[result < bounds[1] ] <- bounds[1] # can happen
  result[result > bounds[2]]  <- bounds[2]

  result
}



#' Calculate power to detect non-zero lying
#'
#' This uses simulations to estimate the power to detect a given level of lying in a
#' sample of size `N` by this package's methods.
#'
#' @inherit basic_params params
#' @param alpha Significance level to use for the null hypothesis
#' @param nsims Number of simulations to run
#'
#' @return Estimated power, a scalar between 0 and 1.
#'
#' @examples
#'
#' power_calc(N = 50, P = 0.5, lambda = 0.2)
#'
#' @export
power_calc <- function (N, P, lambda, alpha = 0.05, prior = stats::dunif,
      nsims = 200) {
  res <- replicate(nsims, {
    R <- stats::rbinom(1, N, (1-P) + P * lambda)
    pstr <- update_prior(R, N, P, prior)
    bounds <- dist_hdr(pstr, 1 - alpha)
    bounds[1] > 0
  })

  mean(res)
}

is_prob <- function (p) all(p >= 0 & p <= 1)


#' Print/plot an object of class `densityFunction`.
#'
#' @param x The object
#' @param ... Unused
#'
#' @export
#'
#' @examples
#'
#' d1 <- update_prior(33, 50, P = 0.5, prior = stats::dunif)
#' d1
#' plot(d1)
#'
#' # show the actual R code (techies only)
#' unclass(d1)
#'
#' @export
print.densityFunction <- function (x, ...) {
  l <- attr(x, "limits")
  cat(sprintf("A probability density function with support on [%s, %s].\n",
        l[1], l[2]))
}


#' @export
#'
#' @rdname print.densityFunction
plot.densityFunction <- function (x, ...) {
  # ... goes first so we can override xlim or ylab
  NextMethod(..., xlim = attr(x, "limits"), ylab = "Density")
}

if (FALSE) {
  N <- 100
  P <- 0.5
  prior <- stats::dunif
  CI <- 0.95
  cov_check <- replicate(1000, {
    lambda <- runif(1)
    heads <- rbinom(1, N, prob = lambda * P + 1 - P)
    posterior <- update_prior(heads, N, P, prior)
    cis <- dist_quantiles(posterior, c(1/2 - CI/2, 1/2 + CI/2))

    cis[1] <= lambda && lambda <= cis[2]
  })
  table(cov_check)
}

