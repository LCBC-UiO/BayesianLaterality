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
#' @param theta Probability of right hemispheric dominance.
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
#' data <- data.frame(ID = c(1, 2, 2),
#'                    handedness = c("A", "D", "D"),
#'                    listening = c(-10, 20, 10),
#'                    stringsAsFactors = FALSE)
#'
#' predict_asymmetry(data)
#'
predict_asymmetry <- function(data, theta = 0.05,
                              mu = c(10.43, -24.41, 11.52, -24.41),
                              sigma = c(22.8, 28.0, 17.0, 28.0),
                              rho = c(0.56, 0.27)){

  # Check if data contains an ID column
  if(!"ID" %in% colnames(data)) data$ID = seq(1, nrow(data), by = 1)

  # Sort according to ID
  data <- data[order(data$ID), , drop = FALSE]

  # Translate L or A to 0 and R or D to 1
  data$handedness[data$handedness == "L"] <- "A"
  data$handedness[data$handedness == "R"] <- "D"
  data$handedness <- as.integer(factor(data$handedness, levels = c("A", "D"))) - 1L



  # Split data according to ID
  posterior <- lapply(split(data, data$ID), function(subdata){
    # Prior, p(\lambda)
    p_lambda <- c(1 - theta, theta)

    # Loop over the data for each subject
    for(i in seq(from = 1, to = nrow(subdata), by = 1)){
      inds <- probtree[probtree$handedness == subdata$handedness[[i]], ]$index

      # p(x1| x2, \lambda)
      p_x1_cond <- mapply(function(m, s) stats::pnorm(subdata$listening[[i]], mean = m, sd = s), mu[inds], sigma[inds])

      # p(x2 | \lambda)
      p_x2_cond <- (subdata$handedness[[i]] == 0) * (1 - rho) + (subdata$handedness[[i]] == 1) * rho

      # Multiply to get unnormalized posterior
      posterior <- p_lambda * p_x1_cond * p_x2_cond

      # Normalize the posterior
      posterior <- posterior / sum(posterior)

      # Update the prior
      p_lambda <- posterior
    }
    return(posterior)
  })

  posterior <- do.call(rbind, posterior)
  colnames(posterior) <- c("LeftDominance", "RightDominance")

  return(cbind(ID = unique(data$ID), posterior))
}
