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
#' @return The probability of left or right hemispheric dominance in additional columns of \code{data}.
#' @export
#'
#' @examples
#' library(asymm)
#' data <- data.frame(handedness = c("A", "D", "D"),
#'                    listening = c(-10, 20, 10),
#'                    stringsAsFactors = FALSE)
#'
#' predict_asymmetry(data)
#'
#'
#' data <- data.frame(ID = c(1, 1, 2),
#'                    handedness = c("A", "A", "D"),
#'                    listening = c(-10, 20, 10),
#'                    stringsAsFactors = FALSE)
#'
predict_asymmetry <- function(data,
                              mu = c(10, -24, 12, -24),
                              sigma = rep(22, 4),
                              rho = c(0.31, 0.12)){

  # Check if data contains an ID column
  if(!"ID" %in% colnames(data)) data$ID = seq(1, nrow(data), by = 1)

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
      do.call(prod, list(truncnorm::dtruncnorm(subdata$listening, a = -100.01, b = 100.01, mean = mu[ind[[1]]], sd = sigma[ind[[1]]]))),
      do.call(prod, list(truncnorm::dtruncnorm(subdata$listening, a = -100.01, b = 100.01, mean = mu[ind[[2]]], sd = sigma[ind[[2]]]))))

    posterior <- p_lambda_x2 * p_x1_cond
    posterior / sum(posterior)
  })

  posterior <- do.call(rbind, posterior)
  colnames(posterior) <- c("LeftDominance", "RightDominance")

  return(as.data.frame(cbind(ID = unique(data$ID), posterior), stringsAsFactors = FALSE))
}
