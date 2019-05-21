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
#' predictions <- cbind(grid, predict_asymmetry(grid))
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
                              rho = c(0.31, 0.12),
                              mu_sem = NULL){

  minval <- -100.01
  maxval <- 100.01

  # Check if data contains an ID column
  if(!"ID" %in% colnames(data)) {
    message("No ID column in data, assuming one subject per row.")
    data$ID = as.character(seq(1, nrow(data), by = 1))
  }

  # Split data according to ID
  data %>%
    dplyr::arrange(.data$ID) %>%
    dplyr::mutate(
      handedness = dplyr::recode(.data$handedness, L = 0L, A = 0L, D = 1L, R = 1L)
    ) %>%
    tidyr::nest(.data$listening, .key = "listening_measures") %>%
    purrr::pmap_dfr(function(...) {
      args <- list(...)
      # p(alpha | x2)
      p_alpha_x2 <- log(c(1 - rho[[args$handedness + 1]], rho[[args$handedness + 1]]))
      # p(x1 | x2, alpha)
      p_x1_cond <- purrr::map_dbl(if(args$handedness == 0) c(1, 2) else c(3, 4), function(x) {
        if(is.null(mu_sem)){
          sum(log(truncnorm::dtruncnorm(dplyr::pull(args$listening),
                                     a = minval, b = maxval, mean = mu[x], sigma[x])))
        } else {
          mu_vec <- seq(from = minval, to = maxval, length.out = 1000)
          sum(log(
            matrix(
              truncnorm::dtruncnorm(dplyr::pull(args$listening), a = minval, b = maxval, mean = mu[x], sd = sigma[x]),
              ncol = 1) %*%
                truncnorm::dtruncnorm(mu_vec, a = minval, b = maxval, mean = mu[x], sd = sigma[x])))
        }
      })
      log_posterior <- p_alpha_x2 + p_x1_cond
      posterior <- exp(log_posterior - max(log_posterior))
      posterior <- posterior / sum(posterior)

      dplyr::tibble(
        ID = args$ID,
        LeftDominance = posterior[[1]],
        RightDominance = posterior[[2]]
      )
    })
}

