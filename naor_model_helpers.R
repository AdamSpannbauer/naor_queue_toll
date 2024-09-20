plot_poisson <- function(lambda, n = 1e4, fill = "steelblue") {
  x <- rpois(n, lambda)

  plot_data <- data.frame(x = x) |>
    group_by(x) |>
    summarise(freq = n() / n)

  ggplot(plot_data, aes(x = x, y = freq)) +
    geom_bar(stat = "identity", fill = fill) +
    labs(
      title = bquote("Poisson Distribution with " ~ lambda == .(lambda)),
      x = "Number of Arrivals (x)",
      y = "Freq"
    ) +
    theme_minimal()
}


plot_exponential <- function(mu, n = 1e5, fill = "darkorange") {
  x <- rexp(n, mu)
  ggplot(data.frame(x = x), aes(x = x)) +
    geom_density(fill = fill, color = fill, alpha = 0.7) +
    labs(
      title = bquote("Exponential Distribution with " ~ mu == .(mu)),
      x = "Service Time (x)",
      y = "Freq"
    ) +
    theme_minimal()
}

plot_queue_len_dist <- function(n, rho, fill = "forestgreen") {
  p <- c()
  x <- 0:n
  for (i in x) {
    p_i <- queue_len_pmf(i, n, rho)
    p[i + 1] <- p_i
  }

  e_i = expected_queue_len(n, rho)

  ggplot(data.frame(x = x, p = p), aes(x = x, y = p)) +
    geom_bar(stat = "identity", fill = fill) +
    geom_vline(xintercept = e_i) +
    labs(
      title = "Queue length probabilities and expected queue length",
      subtitle = bquote(rho == frac(lambda, mu) ~ "=" ~ .(rho) ~ "and policy" ~ n == .(n)),
      x = "Queue length",
      y = "p(i)"
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    theme_minimal()
}


utilization_factor <- function(lambda, mu) {
  # \rho = \frac{\lambda}{\mu}
  lambda / mu
}

service_value_ratio <- function(R, C, mu) {
  # v_s = \frac{R\mu}{C}
  R * mu / C
}

queue_len_pmf <- function(i, n, rho) {
  # p_i = \frac{\rho^i(1-\rho)}{1-\rho^{n+1}}
  numer <- rho^i * (1 - rho)
  denom <- 1 - rho^(n + 1)

  numer / denom
}

expected_queue_len <- function(n, rho) {
  # E[i] = \frac{\rho}{1-\rho} - \frac{(n + 1)\rho^{n+1}}{1-\rho^{n+1}}
  first <- rho / (1 - rho)
  second <- (n + 1) * rho^(n + 1) / (1 - rho^(n + 1))

  first - second
}

balking_rate <- function(n, lambda, rho) {
  # \zeta = \lambda p_n
  lambda * queue_len_pmf(n, n, rho)
}

queue_join_rate <- function(n, lambda, rho) {
  # \lambda - \zeta
  lambda - balking_rate(n, lambda, rho)
}

busy_fraction <- function(n, rho) {
  # b = \sum_{i=1}^n p_i = 1 - p_0
  1 - queue_len_pmf(0, n, rho)
}

expected_leaving_station <- function(n, mu, rho) {
  # \mu b = \mu (1-p_0)
  mu * busy_fraction(n, rho)
}

net_gain_i <- function(i, R, C, mu) {
  # G_i = R - (i + 1) C \frac{1}{\mu}
  R - (i + 1) * C / mu
}

expected_total_net_gain <- function(n, R, C, lambda, mu) {
  # P = (\lambda - \zeta)R - CE[i]
  rho <- utilization_factor(lambda, mu)
  queue_join_rate(n, lambda, rho) * R - C * expected_queue_len(n, rho)
}

self_optimized <- function(R, C, mu) {
  # n_s \leq \frac{R \mu}{C} = v_s < n_s + 1
  v_s <- service_value_ratio(R, C, mu)
  floor(v_s)
}

overall_optimized <- function(R, C, lambda, mu) {
  n_s <- self_optimized(R, C, mu)

  max_gain <- -Inf
  n_o <- 0
  candidate_ns <- seq(from = 0, to = n_s, by = 1)
  pb <- txtProgressBar(min = 0, max = length(candidate_ns))
  for (n in candidate_ns) {
    setTxtProgressBar(pb, n + 1)
    n_expected_gain <- expected_total_net_gain(n, R, C, lambda, mu)
    if (n_expected_gain >= max_gain) {
      max_gain <- n_expected_gain
      n_o <- n
    }
  }
  close(pb)

  return(n_o)
}

suggested_toll <- function(n_o, R, C, mu) {
  # R - (C * (n_o + 1)) / mu # min range
  R - C * n_o / mu # max range
}

model_queue_naor <- function(lambda, mu, R, C) {
  rho <- utilization_factor(lambda, mu)

  n_s <- self_optimized(R, C, mu)
  n_o <- overall_optimized(R, C, lambda, mu)

  modeled_queue <- list(
    lambda = lambda,
    mu = mu,
    R = R,
    C = C,
    rho = rho,
    service_value_ratio = service_value_ratio(R, C, mu),
    len_pmf = function(i, n) queue_len_pmf(i, n, rho),
    expected_len = function(n) expected_queue_len(n, rho),
    balk_rate = function(n) balking_rate(n, lambda, rho),
    busy_frac = function(n) busy_fraction(n, rho),
    expected_leaving = function(n) expected_leaving_station(n, mu, rho),
    gain_i = function(i) net_gain_i(i, R, C, mu),
    expected_total_gain = function(n) expected_total_net_gain(n, R, C, lambda, mu),
    self_optimized_n = n_s,
    overall_optimized_n = n_o,
    suggested_toll = suggested_toll(n_o, R, C, mu)
  )

  modeled_queue
}

set_queue_len_policy <- function(modeled_queue, n) {
  og_names <- names(modeled_queue)

  modeled_queue$queue_len_policy <- n

  modeled_queue$len_pmf <- function(i) modeled_queue$len_pmf(i, n)
  modeled_queue$expected_len <- modeled_queue$expected_len(n)
  modeled_queue$balk_rate <- modeled_queue$balk_rate(n)
  modeled_queue$busy_frac <- modeled_queue$busy_frac(n)
  modeled_queue$expected_leaving <- modeled_queue$expected_leaving(n)
  modeled_queue$expected_total_gain <- modeled_queue$expected_total_gain(n)

  modeled_queue[c("queue_len_policy", og_names)]
}


# lambda <- 500
# mu <- 501
# R <- 10
# C <- 8
#
# modeled_queue <- model_queue_naor(lambda, mu, R, C)
#
# toll <- modeled_queue$suggested_toll
# TOLLED_modeled_queue <- model_queue_naor(lambda, mu, R - toll, C)
#
# self_opt_queue <- set_queue_len_policy(modeled_queue, n = modeled_queue$self_optimized_n)
# overall_opt_queue <- set_queue_len_policy(modeled_queue, n = modeled_queue$overall_optimized_n)
#
# TOLLED_self_opt_queue <- set_queue_len_policy(TOLLED_modeled_queue, n = TOLLED_modeled_queue$self_optimized_n)
# TOLLED_overall_opt_queue <- set_queue_len_policy(TOLLED_modeled_queue, n = TOLLED_modeled_queue$overall_optimized_n)
