#' Predict brain asymmetry
#'
#' @param data Data frame with the following columns:
#' \itemize{
#' \item \code{listening}: Score between -100 and 100.
#' \item \code{handedness}: \code{"L"} or \code{"A"} for adextral (non-right-handed) and
#' \code{"R"} or \code{"D"} for dextrals (right-handed)
#' }
#' In additional, an optional column name \code{ID} can be provided, giving the subject ID. If a
#' subject has multiple measurements, the posterior based on all measurements is provided. If the
#' \code{ID} column is missing, each row is assumed to be measured on a separate subject.
#'
#'
#' @param mu Vector of mean dichotic listening scores.
#'
#' @param sigma Vector of standard deviation of dichotic listening scores.
#'
#' @param rho Probability of dextrality given left or right hemispheric dominance.
#'
#' @param mu_sem Vector representing the standard error of the mean of \code{mu}. If provided, a hierarchical model
#' is computed.
#'
#' @return The probability of left or right hemispheric dominance in additional columns of \code{data}.
#' @export
#'
#' @examples
#' library(asymm)
#' library(dplyr)
#'
#' # Three observations of three individuals
#' data <- tibble(handedness = c("A", "D", "D"),
#'                    listening = c(-10, 20, 10))
#'
#' predict_asymmetry(data)
#'
#' # Take into account uncertainty in the parameters for mean speech laterlization scores.
#' predict_asymmetry(data, mu_sem = c(4.6, 7.0, 4.3, 7.0))
#'
#' # Multiple observations
#' data <- tibble(ID = c(1, 1, 2),
#'                    handedness = c("A", "A", "D"),
#'                    listening = c(-10, 20, 10))
#'
#' predict_asymmetry(data)
#'
#' # Plot over a grid
#' grid <- expand.grid(listening = seq(from = -100, to = 100, by = 1), handedness = c("A", "D"))
#' grid$ID <- seq(1, nrow(grid), by = 1)
#' predictions <- merge(grid, predict_asymmetry(grid), by = "ID")
#'
#' library(ggplot2)
#' ggplot(predictions, aes(x = listening, y = LeftDominance, group = handedness, color = handedness)) +
#'   geom_line() +
#'   geom_hline(yintercept = 0.05, color = "gray", lwd = 1) +
#'   geom_hline(yintercept = 0.95, color = "gray", lwd = 1) +
#'   xlab("Speech lateralization score") +
#'   ylab("Probability of left brain asymmetry") +
#'   theme(text = element_text(size = 16),
#'         legend.title = element_blank())
#'
#'  library(tidyr)
#' data <- tibble(
#' listening = numeric(),
#' handedness = character()
#' ) %>%
#'   expand(listening = seq(from = -100, to = 100, by = 1), handedness = c("A", "D")) %>%
#'  mutate(ID = row_number())
#'
#' @importFrom magrittr "%>%"
#'
predict_asymmetry <- function(data,
                              mu = dplyr::tibble(
                                handedness = c("adextral", "dextral"),
                                left_dominant = c(10, 12),
                                bilateral = c(0, 0),
                                right_dominant = c(-24, -24)),
                              sigma = dplyr::tibble(
                                handedness = c("adextral", "dextral"),
                                left_dominant = c(22, 22),
                                bilateral = c(22, 22),
                                right_dominant = c(22, 22)),
                              rho = dplyr::tibble(
                                handedness = c("adextral", "dextral"),
                                left_dominant = c(.65, .88),
                                bilateral = c(.15, .08),
                                right_dominant = c(.20, .04)),
                              mu_sem = NULL){

  minval <- -100.01
  maxval <- 100.01

  # Check if data contains an ID column
  if(!"ID" %in% colnames(data)) {
    message("No ID column in data, assuming one subject per row.")
    data$ID = as.character(seq(1, nrow(data), by = 1))
  }


  mu <- tidyr::pivot_longer(mu, -.data$handedness, names_to = "laterality", values_to = "mu")
  sigma <- tidyr::pivot_longer(sigma, -.data$handedness, names_to = "laterality", values_to = "sigma")
  rho <- tidyr::pivot_longer(rho, -.data$handedness, names_to = "laterality", values_to = "rho")

  # Join in all the probabilities and compute p(x_1 | x_2, alpha)
  data %>%
    dplyr::mutate(
      handedness = dplyr::recode(.data$handedness, L = "adextral",
                                 A = "adextral", D = "dextral", R = "dextral"),
    ) %>%
    dplyr::inner_join(mu, by = "handedness") %>%
    dplyr::inner_join(sigma, by = c("handedness", "laterality")) %>%
    dplyr::group_by(.data$ID, .data$handedness, .data$laterality) %>%
    dplyr::summarise(
      p_x1_cond = sum(log(truncnorm::dtruncnorm(.data$listening, a = minval, b = maxval,
                                             mean = .data$mu, sd = .data$sigma)))
    ) %>%
    dplyr::inner_join(rho, by = c("handedness", "laterality")) %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::mutate(
      p_alpha_x2 = log(.data$rho),
      log_posterior = .data$p_x1_cond + .data$p_alpha_x2,
      posterior = exp(.data$log_posterior - max(.data$log_posterior)),
      posterior = .data$posterior / sum(.data$posterior)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$ID, .data$handedness, .data$laterality, .data$posterior) %>%
    tidyr::pivot_wider(names_from = "laterality", values_from = "posterior", names_prefix = "prob_")

}

