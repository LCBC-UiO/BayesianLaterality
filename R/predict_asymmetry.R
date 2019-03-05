#' Predict brain asymmetry
#'
#' @param data Data frame with the following columns:
#' \itemize{
#' \item \code{listening}: Score between -100 and 100.
#' \item \code{handedness}: \code{"L"} or \code{"A"} for adextral (non-right-handed) and
#' \code{"R"} or \code{"D"} for dextrals (right-handed)
#' }
#' In additional, an optional column name \code{ID} can be provided, giving the subject ID. If a
#' subject has multiple measurements, the posterior based on all measurements is provided.
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
predict_asymmetry <- function(data,
                              mu = c(10, -24, 12, -24),
                              sigma = rep(22, 4),
                              rho = c(0.31, 0.12),
                              mu_sem = NULL){

  minval <- -100.01
  maxval <- 100.01

  # Check if data contains an ID column
  if(!"ID" %in% colnames(data)) data$ID = as.character(seq(1, nrow(data), by = 1))
  stopifnot("listening" %in% colnames(data) && is.numeric(data$listening))
  stopifnot("handedness" %in% colnames(data))

  if(is.factor(data$handedness)) data$handedness <- as.character(data$handedness)

  # Sort according to ID
  data <- data[order(data$ID), , drop = FALSE]

  # Translate L or A to 0 and R or D to 1
  data$handedness[data$handedness == "L"] <- "A"
  data$handedness[data$handedness == "R"] <- "D"
  data$handedness <- as.integer(factor(data$handedness, levels = c("A", "D"))) - 1L


  # Split data according to ID
  posterior <- lapply(split(data, data$ID), function(subdata){
    stopifnot(length(unique(subdata$handedness)) == 1)
    # p(lambda | x1)
    ind <- ifelse(subdata$handedness[[1]] == 0, 1, 2)
    p_lambda_x2 <- c(1 - rho[[ind]], rho[[ind]])


    # p(x1 | x2, lambda)
    ind <- if(subdata$handedness[[1]] == 0) c(1, 2) else c(3, 4)
    p_x1_cond <- c(
      do.call(prod, list(laterality_dist(subdata$listening, mu[ind[[1]]], sigma[ind[[1]]],
                                         minval, maxval, mu_sem[ind[[1]]]))),
      do.call(prod, list(laterality_dist(subdata$listening, mu[ind[[2]]], sigma[ind[[2]]],
                                         minval, maxval, mu_sem[ind[[2]]])))
      )

    posterior <- p_lambda_x2 * p_x1_cond
    posterior / sum(posterior)
  })

  posterior <- do.call(rbind, posterior)
  colnames(posterior) <- c("LeftDominance", "RightDominance")

  return(cbind(ID = unique(data$ID), as.data.frame(posterior), stringsAsFactors = FALSE))
}

laterality_dist <- function(x1, m, s, a, b, sem){
  if(is.null(sem)){
    truncnorm::dtruncnorm(x1, a = a, b = b, mean = m, sd = s)
  } else {
    mu_vec <- seq(from = a, to = b, length.out = 1000)
    sum(truncnorm::dtruncnorm(x1, a = a, b = b, mean = mu_vec, sd = s) *
      truncnorm::dtruncnorm(mu_vec, a = a, b = b, mean = m, sd = sem))
  }
}
