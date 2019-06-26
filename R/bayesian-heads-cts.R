
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


is_prob <- function (p) all(p >= 0 & p <= 1)


try_integral <- function(f, a, b, npoints = 100) {
  integ <- stats::integrate(f, a, b, subdivisions = npoints) # numerical integration
  if (integ$message != "OK") stop("stats::integrate failed with message: ", integ$message)

  return(integ$value)
}


#' Calculate posterior distribution of the proportion of liars
#'
#' @description
#' `update_prior` uses the equation for the posterior:
#'
#' \deqn{
#'   \phi(\lambda | R; N,P) = Pr(R|\lambda; N,P) \phi(\lambda) /
#'     \int Pr(R | \lambda'; N,P) \phi(\lambda') d \lambda'
#' }
#'
#' where \eqn{\phi} is the prior and \eqn{Pr(R | \lambda; N, P)} is the
#' probability of R reports of heads given that people lie with probability
#' \eqn{\lambda}:
#'
#' \deqn{
#'   Pr(R | \lambda; N, P) = binom(N, (1-P) + \lambda P)
#' }
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



#' Calculate probability that one posterior is larger than another
#'
#' @description
#' Given two distributions with density functions \eqn{\phi_1, \phi_2},
#' this calculates:
#'
#' \deqn{
#' \int_0^1 \int_0^{l_1}\phi_1(l_1) \phi_2(l_2) d l_2 d l_1,
#' }
#' the probability that the value of the first distribution is greater.
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


#' Find density of the difference of two distributions
#'
#' Given two probability density functions `dist1` and `dist2`, `difference_dist`
#'  returns the density of ``dist1 - dist2`.
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
#' d2 <- update_prior(32, 40, P = 0.5, prior = stats::dunif)
#' dd <- difference_dist(d1, d2)
#' dist_hdr(dd, 0.95)
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


#' Find mean of a probability density function
#'
#' @param dist A one-argument function returned from [update_prior()]
#' @param l Lower bound of the density's support
#' @param r Upper bound of the density's support
#'
#' @return A scalar
#'
#' @examples
#'
#' d1 <- update_prior(10, 40, P = 5/6, prior = stats::dunif)
#' dist_mean(d1)
#'
#' @export
dist_mean <- function (dist, l = attr(dist, "limits")[1], r = attr(dist, "limits")[2]) {
  stopifnot(is.function(dist))

  f <- function (x) x * dist(x)
  EV <- try_integral(f, l, r)

  return(EV)
}

#' Find probability of a probability density function within given bounds
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


#' Find quantiles given a probability density function
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
dist_quantile <- function(dist, probs, bounds = attr(dist, "limits")) {
  stopifnot(is_prob(probs), is.function(dist))

  qs <- sapply(probs, function (prob) {
    f <- function (x) (dist_prob_within(dist, bounds[1], x) - prob)^2
    # want x s.t. interval_prob(dist, 0, x) == prob
    res <- stats::optimize(f, bounds)
    res$minimum
  })

  return(qs)
}


#' Compute highest density region for a density function
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
dist_hdr <- function (dist, conf_level, bounds = attr(dist, "limits")) {
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


#' Estimate power to detect differences in lying between two samples
#'
#' Using simulations, estimate power to detect differences in lying
#' using [compare_dists()], given values for \eqn{\lambda}, the
#' probability of lying, in each sample.
#'
#' @param N1 N of sample 1
#' @param N2 N of sample 2
#' @inherit basic_params params
#' @param lambda1 Probability of lying in sample 1
#' @param lambda2 Probability of lying in sample 2
#' @param alpha Significance level
#' @param alternative "two.sided", "greater" (sample 1 is greater), or "less". Can be
#'   abbreviated
#' @param nsims Number of simulations to run
#'
#' @return Estimated power, a scalar between 0 and 1.
#'
#' @export
#'
#' @examples
#'
#' power_calc_difference(N1 = 100, P = 0.5, lambda = 0, lambda2 = 0.25)
#'
power_calc_difference <- function(N1, N2 = N1, P, lambda1, lambda2, alpha = 0.05,
      alternative = c("two.sided", "greater", "less"),
      prior = stats::dunif, nsims = 200) {
  alternative <- match.arg(alternative)

  res <- replicate(nsims, {
    R1 <- stats::rbinom(1, N1, (1-P) + P * lambda1)
    R2 <- stats::rbinom(1, N2, (1-P) + P * lambda2)
    pstr1 <- update_prior(R1, N1, P, prior)
    pstr2 <- update_prior(R2, N2, P, prior)
    p1x <- compare_dists(pstr1, pstr2)
    p1x
  })

  switch(alternative,
          greater = mean(res <= alpha),
          less    = mean(res >= 1 - alpha),
          two.sided = mean(res <= alpha/2 | res >= 1 - alpha/2)
        )
}


#' Estimate proportions of liars in multiple samples using empirical Bayes
#'
#' This function creates a prior by fitting a Beta distribution to the `heads/N` vector,
#' using [MASS::fitdistr()]. The prior is then updated using data from each
#' individual sample to give the posterior distributions.
#'
#' @param heads A vector of numbers of the good outcome reported
#' @param N A vector of sample sizes
#' @inherit basic_params params
#'
#' @return A list with two components:
#'
#' * `prior`, the calculated empirical prior (of class `densityFunction`).
#' * `posterior`, a list of posterior distributions (objects of class `densityFunction`).
#'   If `heads` was named, the list will have the same names.
#'
#' @export
#'
#' @examples
#'
#' heads <- c(Baseline = 30, Treatment1 = 38, Treatment2 = 45)
#' N <- c(50, 52, 57)
#' res <- empirical_bayes(heads, N, P = 0.5)
#'
#' compare_dists(res$posteriors$Baseline, res$posteriors$Treatment1)
#' plot(res$prior, ylim = c(0, 4), col = "grey", lty = 2)
#' plot(res$posteriors$Baseline, add = TRUE, col = "blue")
#' plot(res$posteriors$Treatment1, add = TRUE, col = "orange")
#' plot(res$posteriors$Treatment2, add = TRUE, col = "red")
#'
empirical_bayes <- function (heads, ...) UseMethod("empirical_bayes")


#' @name empirical_bayes
#' @export
empirical_bayes.default <- function (heads, N, P) {
  if (! requireNamespace("MASS", quietly = TRUE)) {
    stop("`empirical_bayes` requires the 'MASS' package. ",
      "You can install it by running:\n",
      "  install.packages(\"MASS\")")
  }

  maxlik_ests <- pmax(0, (heads/N - P)/(1 - P))
  params <- MASS::fitdistr(maxlik_ests, dbeta, list(shape1 = 1, shape2 = 1))
  prior <- function (x) dbeta(
          x,
          shape1 = params$estimate[["shape1"]],
          shape2 = params$estimate[["shape2"]]
        )

  result <- list()
  result$prior <- prior
  attr(result$prior, "limits") <- c(0, 1)
  class(result$prior) <- c("densityFunction", "function")
  result$posteriors <- mapply(update_prior, heads = heads, N = N,
          MoreArgs = list(P = P, prior = prior),
          SIMPLIFY = FALSE, USE.NAMES = TRUE
        )

  return(result)
}


#' @name empirical_bayes
#'
#' @details
#' The formula interface allows calling the function directly on experimental data.
#'
#' @param formula A two-sided formula of the form `heads ~ group`. `heads` is
#'   a logical vector  specifying whether the "good" outcome was reported. `group`
#'   specifies the sample.
#' @param data A data frame or matrix. Each row represents one individual.
#' @param subset A logical or numeric vector specifying the subset of data to use
#'
#' @export
#'
#' @examples
#'
#' # starting from raw data:
#' raw_data <- data.frame(
#'         report = sample(c("heads", "tails"),
#'           size = 300,
#'           replace = TRUE,
#'           prob = c(.8, .2)
#'         ),
#'         group = rep(LETTERS[1:10], each = 30)
#'     )
#' empirical_bayes(I(report == "heads") ~ group, data = raw_data, P = 0.5)
empirical_bayes.formula <- function (formula, data, P, subset) {
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$P <- NULL
  mf <- eval(m, parent.frame())
  stopifnot(is.logical(mf[[1]]))
  if (is.factor(mf[[2]])) mf[[2]] <- droplevels(mf[[2]])
  heads <- tapply(mf[[1]], mf[[2]], sum)
  N <- tapply(mf[[1]], mf[[2]], length)
  empirical_bayes(heads = heads, N = N, P = P)
}


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
