library(truelies)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

simulate_reports <- function (N, P, lambda) rbinom(1, N, prob = lambda * P + 1 - P)

test_params <- function(N, P, lambda) {
  res <- replicate(100, {
    heads <- simulate_reports(N = N, P = P, lambda = lambda)
    d <- update_prior(heads = heads, N = N, P = P, prior = dunif)

    c(
      lhat = dist_mean(d),
      ci99 = dist_hdr(d, 0.99),
      ci95 = dist_hdr(d, 0.95),
      ci90 = dist_hdr(d, 0.90)
    )
  })
  res <- as.data.frame(t(res))
  with(res, data.frame(
    bias        = mean(lhat - lambda),
    rmse        = sqrt(mean((lhat - lambda)^2)),
    coverage_90 = mean(ci901 <= lambda & lambda <= ci902),
    coverage_95 = mean(ci951 <= lambda & lambda <= ci952),
    coverage_99 = mean(ci991 <= lambda & lambda <= ci992)
  ))
}

params <- expand.grid(
  lambda = seq(0, 0.8, 0.2),
  P      = seq(0.1, 0.9, 0.2),
  N      = c(50, 100, 500, 2000),
  stringsAsFactors = FALSE
)

results <- pmap_dfr(params, test_params)
results <- cbind(params, results)

res_long <- results %>%
      tidyr::gather("cl", "coverage", starts_with("coverage")) %>%
      tidyr::separate(cl, sep = "_", into = c(NA, "cl"))

ggplot(results, aes(lambda, bias)) + geom_point() + geom_line() +
      facet_grid(P ~ N) +
      ggtitle(label  = "Bias")

ggplot(results, aes(lambda, rmse)) + geom_point() + geom_line() +
      facet_grid(P ~ N, labeller = label_both) +
      ggtitle(label  = "RMSE")

ggplot(res_long, aes(lambda, coverage, colour = cl, group = cl)) +
  geom_point() + geom_line() +
  facet_grid(P ~ N, labeller = label_both) +
  geom_hline(yintercept = c(.90, .95, .99), linetype = 2) +
  ggtitle(label  = "Coverage")
