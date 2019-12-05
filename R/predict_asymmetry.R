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
#'
#' # Three observations of three individuals
#' data <- data.frame(handedness = c("A", "D", "D"),
#'                    listening = c(-10, 20, 10),
#'                    stringsAsFactors = FALSE)
#'
#' predict_asymmetry(data)
#'
#' # Take into account uncertainty in the parameters for mean speech laterlization scores.
#' predict_asymmetry(data, mu_sem = c(4.6, 7.0, 4.3, 7.0))
#'
#' # Multiple observations
#' data <- data.frame(ID = c(1, 1, 2),
#'                    handedness = c("A", "A", "D"),
#'                    listening = c(-10, 20, 10),
#'                    stringsAsFactors = FALSE)
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
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'
predict_asymmetry <- function(data,
                              mu = c(10, -24, 12, -24),
                              sigma = rep(22, 4),
                              rho = c(0.20, 0.04),
                              mu_sem = NULL){

  minval <- -100.01
  maxval <- 100.01

  # Check if data contains an ID column
  if(!"ID" %in% colnames(data)) {
    message("No ID column in data, assuming one subject per row.")
    data$ID = as.character(seq(1, nrow(data), by = 1))
  }
  rhovec <- c(1 - rho[[1]], rho[[1]], 1 - rho[[2]], rho[[2]])

  data %>%
    tidyr::expand_grid(dominance = c("Left", "Right")) %>%
    dplyr::mutate(
      handedness = dplyr::recode(.data$handedness, L = 0L, A = 0L, D = 1L, R = 1L),
      ind = (.data$handedness == 0 & .data$dominance == "Left") +
        (.data$handedness == 0 & .data$dominance == "Right") * 2L +
        (.data$handedness == 1 & .data$dominance == "Left") * 3L +
        (.data$handedness == 1 & .data$dominance == "Right") * 4L,
      p_x1_cond = log(truncnorm::dtruncnorm(.data$listening, a = minval, b = maxval,
                                            mean = mu[.data$ind], sd = sigma[.data$ind])),
      p_alpha_x2 = log(rhovec[.data$ind])
      ) %>%
    dplyr::group_by(.data$ID, .data$dominance, .data$p_alpha_x2) %>%
    dplyr::summarise_at(dplyr::vars(.data$p_x1_cond), sum) %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::mutate(
      log_posterior = .data$p_alpha_x2 + .data$p_x1_cond,
      posterior = exp(.data$log_posterior - max(.data$log_posterior)),
      posterior = .data$posterior / sum(.data$posterior)
    ) %>%
    dplyr::select(.data$ID, .data$posterior, .data$dominance) %>%
    tidyr::pivot_wider(names_from = .data$dominance, values_from = .data$posterior) %>%
    dplyr::rename_at(dplyr::vars(.data$Left, .data$Right), ~ paste0(., "Dominance")) %>%
    dplyr::ungroup()
}

