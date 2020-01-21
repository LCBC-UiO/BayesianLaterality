
data <- data.frame(
  listening = c(-20, -23, -14),
  handedness = "left",
  stringsAsFactors = T
)
predict_asymmetry(data)

parameters = dplyr::tibble(
  dominance = rep(c("left", "right", "none"), each = 2),
  handedness = rep(c("left", "right"), 3),
  mean_li = c(10, 12, -24, -24, 0, 0),
  sd_li = rep(22, 6),
  prob_dominance = c(.65, .87, .20, .04, .15, .09)
)
truncation = c(-100, 100)
icc = NULL


diag(3) *(1-.22) + .22


library(tidyverse)
n <- 100
reps <- 3

data <- tibble(
  ID = 1:n,
  ID_mean = truncnorm::rtruncnorm(n, a = 0, b = 100, mean = 10, sd = 10),
  handedness = "left"
) %>%
  mutate(
    listening = map(ID_mean, ~ rnorm(reps, mean = .x, sd = 10))
  ) %>%
  unnest(listening)

predict_asymmetry(data)
