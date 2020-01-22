#' Predict hemispheric dominance
#'
#' @param data Data frame with the following columns:
#' \itemize{
#' \item \code{listening}: Score between -100 and 100.
#' \item \code{handedness}: \code{"left"} for adextral (non-right-handed) and
#'       \code{"right"} for dextral (right-handed)
#'       }
#'  In addition, an optional column name \code{ID}
#'   can be provided, giving the subject ID. If a subject has multiple
#'   measurements, the posterior based on all measurements is provided. If the
#'   \code{ID} column is missing, each row is assumed to be measured on a
#'   separate subject.
#'
#' @param parameters Data frame in which the first two columns specify combinations
#' of hemispheric dominance and handedness and the last three columns specify
#' the corresponding parameter values. In particular, the columns are defined as follows:
#' \itemize{
#' \item \code{dominance}: character specifying hemispheric dominance.
#' \item \code{handedness}: character specifying handedness.
#' \item \code{mean_li}: mean dichotic listening score.
#' \item \code{sd_li}: standard deviation of dichotic listening score.
#' \item \code{prob_dominance}: probability of hemispheric dominance given handedness.
#' }
#' @param truncation Numeric vector with two elements specifying the lower and upper
#' bounds for truncation of the normal distribution for dichotic listening scores.
#' @param icc Intraclass correlation for repeated measurements on the same individual.
#' Defaults to 0.
#'
#' @return The probability of left or right hemispheric dominance in additional
#'   columns of \code{data}.
#' @export
#' @examples
#' ## Simple test dataset
#' data <- data.frame(
#'           listening = c(-20, -23, -14),
#'           handedness = "left",
#'           stringsAsFactors = FALSE
#'          )
#' ## Compute predictions
#' predict_asymmetry(data)
#'
#' ## More interesting example, with multiple measurements per individual.
#' library(dplyr); library(purrr); library(tidyr); library(truncnorm)
#' ## First we sample test data
#' n <- 100 # number of individuals
#' reps <- 3 # number of measurements per individual
#' ## The distribution of subject means has standard deviation 10, and the
#' ## actual measurements for each subject are distributed with a standard
#' ## deviation of 10 around this mean.
#' set.seed(234)
#' data <- tibble(
#'                ID = factor(1:n),
#'                subject_mean = rtruncnorm(n, a = 0, b = 100, mean = 10, sd = 10),
#'                handedness = "left") %>%
#'   mutate(
#'     listening = map(subject_mean, ~ rtruncnorm(reps, a = -100, b = 100,
#'                     mean = .x, sd = 10))
#'   ) %>%
#'   unnest(listening)
#'
#' predict_asymmetry(data)
#'
#'
predict_asymmetry <- function(data,
                              parameters = dplyr::tibble(
                                dominance = rep(c("left", "right", "none"), each = 2),
                                handedness = rep(c("left", "right"), 3),
                                mean_li = c(10, 12, -24, -24, 0, 0),
                                sd_li = rep(22, 6),
                                prob_dominance = c(.65, .87, .20, .04, .15, .09)
                              ),
                              truncation = c(-100, 100),
                              icc = 0
                              ){

  stopifnot(icc >= -1 && icc <= 1)

  # Check if data contains an ID column
  if(!"ID" %in% colnames(data)) {
    message("No ID column in data, assuming one subject per row.")
    data$ID = as.character(seq(1, nrow(data), by = 1))
  }

  dat1 <- dplyr::select(data, .data$ID, .data$listening, .data$handedness)
  dat1 <- dplyr::inner_join(dat1, parameters, by = "handedness")
  dat1 <- dplyr::select(dat1, .data$ID, .data$handedness, .data$dominance,
                        .data$prob_dominance, .data$mean_li, .data$sd_li, .data$listening)
  dat2 <- tidyr::nest(dat1, df = c(.data$listening, .data$mean_li, .data$sd_li))
  dat3 <- dplyr::mutate(dat2,
                log_prob_listening = purrr::map_dbl(.data$df, function(x) {
                  tmvtnorm::dtmvnorm(x$listening,
                                     mean = x$mean_li,
                                     sigma = x$sd_li^2 * (diag(nrow(x)) * (1 - icc) + icc),
                                     lower = rep(truncation[[1]], nrow(x)),
                                     upper = rep(truncation[[2]], nrow(x)),
                                     log = TRUE
                                     )
                }),
                log_posterior = log(.data$prob_dominance) + .data$log_prob_listening,
                probability = exp(.data$log_posterior)
                )
  dat4 <- dplyr::group_by(dat3, .data$ID)
  dat5 <- dplyr::mutate(dat4, probability = .data$probability / sum(.data$probability))

  dplyr::select(dplyr::ungroup(dat5), .data$ID, .data$handedness,
                .data$dominance, .data$probability)
}

