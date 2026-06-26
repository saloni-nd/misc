# ---------------------------------------------------------------
# Plain-language summary
# ---------------------------------------------------------------
# This model estimates how long a vaccine trial needs to run to reach
# an endpoint of detecting a vaccine efficacy of 50%, under different
# scenarios: 1) where the sample size is changed, and 2) where the
# disease incidence rate is changed.
#
# For simplicity, the model assumes all participants are enrolled
# simultaneously, the chosen incidence rate is constant over time,
# there is no dropout, and the vaccine takes effect from week two.
# It shows how long it would take to detect 50% efficacy at a power
# of 0.8 with a two-sided p-value of 0.05.
#
# It first calculates the number of cases required to distinguish 50%
# efficacy from the null hypothesis, using a one-proportion test (a
# normal approximation to the binomial, commonly used in vaccine
# trials for efficacy estimates) on the share of cases that
# fall in the vaccine arm: 0.5 under the null and 1/3 under 50%
# efficacy. It then estimates the time to accrue those cases using an
# exponential time-to-infection model with a constant monthly hazard.
# ---------------------------------------------------------------

library(tidyverse)
library(scales)

# ---------------------------------------------------------------
# Design parameters
# ---------------------------------------------------------------
ve_target   <- 0.50          # vaccine efficacy we want to detect
rr          <- 1 - ve_target # relative risk in vaccine arm = 0.5
alpha       <- 0.05          # two-sided significance level
power       <- 0.80          # 1 - beta
alloc       <- 0.50          # fraction allocated to the vaccine arm (1:1)

# Efficacy does not start on day one; immunity takes time to develop.
# Until onset, both arms run at the same hazard and those cases carry
# no efficacy signal, so they are excluded from the endpoint count
# (this mirrors how the covid trials handled early post-dose cases).
onset_weeks    <- 2
days_per_month <- 365.25 / 12
onset_months   <- onset_weeks * 7 / days_per_month   # ~0.46 months

# ---------------------------------------------------------------
# Required number of cases (events)
# Among cases accrued under the efficacy regime, the share landing in
# the vaccine arm is 0.5 under the null and rr / (1 + rr) = 1/3 under
# 50% efficacy. We size to separate those two proportions.
# ---------------------------------------------------------------
p0 <- 0.5
p1 <- rr / (1 + rr)

z_a <- qnorm(1 - alpha / 2)
z_b <- qnorm(power)

required_events <- ceiling(
  (z_a * sqrt(p0 * (1 - p0)) + z_b * sqrt(p1 * (1 - p1)))^2 / (p0 - p1)^2
)
# required_events = 69

# ---------------------------------------------------------------
# Time (months) until the required number of *post-onset* cases accrue.
#
# Both arms run at hazard lambda over [0, onset]; after onset the
# vaccine arm hazard drops to lambda * rr. Counting cases from onset:
#
#   placebo post-onset = N(1-alloc) * [exp(-lambda*onset) - exp(-lambda*t)]
#   vaccine post-onset = N(alloc)   * exp(-lambda*onset)
#                                   * [1 - exp(-lambda*rr*(t-onset))]
#
# Solved numerically for total post-onset cases = target. Returns NA if
# the target cannot be reached (post-onset cases asymptote to
# N * exp(-lambda*onset), since some are infected during the wait).
# ---------------------------------------------------------------
time_to_events <- function(N, lambda, target, rr, onset, alloc = 0.5) {
  cap <- N * exp(-lambda * onset)              # max reachable post-onset cases
  if (target >= cap) return(NA_real_)
  post_onset_cases <- function(t) {
    placebo <- N * (1 - alloc) * (exp(-lambda * onset) - exp(-lambda * t))
    vaccine <- N * alloc * exp(-lambda * onset) *
      (1 - exp(-lambda * rr * (t - onset)))
    placebo + vaccine
  }
  uniroot(function(t) post_onset_cases(t) - target,
          interval = c(onset + 1e-6, 1e6))$root
}

# ---------------------------------------------------------------
# Build the grid: smooth curve over sample size, one line per incidence
# ---------------------------------------------------------------
incidence  <- c(0.002, 0.004, 0.006, 0.008, 0.01)
size_grid  <- seq(200, 10000, by = 100)

df <- expand_grid(N = size_grid, lambda = incidence) |>
  mutate(
    months = pmap_dbl(
      list(N, lambda),
      \(N, lambda) time_to_events(N, lambda, required_events, rr, onset_months)
    ),
    incidence_lab = factor(
      lambda * 100,
      levels = c(0.2, 0.4, 0.6, 0.8, 1),
      labels = paste0(c(0.2, 0.4, 0.6, 0.8, 1), "% per month")
    )
  )


# ---------------------------------------------------------------
# Plot: months on x (time reads left to right), sample size on y
# ---------------------------------------------------------------
ggplot(df, aes(months, N, colour = incidence_lab)) +
  #geom_hline(yintercept = 5000, linetype = "dashed", colour = "grey55", linewidth = 0.5) +
  geom_line(linewidth = 0.5) +
  scale_x_continuous(
    breaks = seq(0, 11, 1),
    limits = c(0, 12),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_y_continuous(
    breaks = seq(0, 10000, 1000),
    labels = label_comma()
  ) +
  scale_colour_viridis_d(option = "C", end = 0.9, direction = -1) +
  labs(
    title = "Higher incidence and larger samples both shorten a vaccine trial",
    subtitle = "Sample size",
    x = "Months to reach endpoint",
    y = "",
    colour = "Monthly incidence\n(placebo arm)",
    caption = sprintf(
      paste0(
        "Note: In this model, the endpoint is detecting 50%% efficacy at %.0f%% power, two-sided p < %.2f,\n",
        "the sample size is 1:1 between placebo:vaccine, and for simplicity, it assumes all participants are enrolled simultaneously,\n",
        "incidence rates are constant, there is no dropout, and the vaccine halves the hazard ratio from week two."
      ),
      power * 100, alpha
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.caption = element_text(hjust = 0)
  )

ggsave("vaccine_trial_time_to_endpoint.svg", width = 8, height = 5, dpi = 300)

fixed_N <- 5000

crossings <- tibble(lambda = incidence) |>
  mutate(
    months    = map_dbl(lambda, \(l) time_to_events(fixed_N, l, required_events, rr, onset_months)),
    days      = months * days_per_month,
    incidence = paste0(lambda * 100, "% per month")
  ) |>
  select(incidence, months, days)

crossings

