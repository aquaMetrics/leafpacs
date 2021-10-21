#' Calculate Confidence of Class
#'
#' @param data Data frame of classification results
#'
#' @return Data frame containing confidence of class
#' @export
#' @importFrom stats pnorm
#'
#' @examples
leafpacs_confidence <- function(data) {
  message("Calculating confidence")
  # Add columns for confidence and classification results ---------------------
  data$CLASS <- c("BLANK")
  data$SE <- 0
  data$trsfdMean <- 0
  data$trsfdError <- 0
  data$Normdist1 <- 0
  data$Normdist2 <- 0
  data$Normdist3 <- 0
  data$Normdist4 <- 0
  data$Bad <- 0
  data$Poor <- 0
  data$Moderate <- 0
  data$Good <- 0
  data$High <- 0

  # Calculate CLASS -----------------------------------------------------------
  for (n in 1:nrow(data)) {
    # if(numeric(data$EQR_LEAFPACS[n])) {
    if (data$EQR_LEAFPACS[n] < 0.2) {
      data$CLASS[n] <- "BAD"
    } else
    if (data$EQR_LEAFPACS[n] < 0.4) {
      data$CLASS[n] <- "POOR"
    } else
    if (data$EQR_LEAFPACS[n] < 0.6) {
      data$CLASS[n] <- "MODERATE"
    } else
    if (data$EQR_LEAFPACS[n] < 0.8) {
      data$CLASS[n] <- "GOOD"
    } else
    if (data$EQR_LEAFPACS[n] >= 0.8) {
      data$CLASS[n] <- "HIGH"
    }
  }

  # SE value ------------------------------------------------------------------
  for (n in 1:nrow(data)) {
    data$SE[n] <- ((0.04 + -2.98 * data$EQR_LEAFPACS_Capped[n] + 2.96 * data$EQR_LEAFPACS_Capped[n]^0.95) / sqrt(1))
  }
  for (n in 1:nrow(data)) {
    data$trsfdMean[n] <- log(data$EQR_LEAFPACS_Capped[n] / (1 - data$EQR_LEAFPACS_Capped[n]))
  }
  for (n in 1:nrow(data)) {
    data$trsfdError[n] <- (data$SE[n]) / (data$EQR_LEAFPACS_Capped[n] * (1 - data$EQR_LEAFPACS_Capped[n]))
  }

  for (n in 1:nrow(data)) {
    data$Normdist1[n] <- pnorm((-1.386 - data$trsfdMean[n]) / (data$trsfdError[n]))
  }
  for (n in 1:nrow(data)) {
    data$Normdist2[n] <- pnorm((-0.405 - data$trsfdMean[n]) / (data$trsfdError[n]))
  }
  for (n in 1:nrow(data)) {
    data$Normdist3[n] <- pnorm((0.405 - data$trsfdMean[n]) / (data$trsfdError[n]))
  }
  for (n in 1:nrow(data)) {
    data$Normdist4[n] <- pnorm((1.386 - data$trsfdMean[n]) / (data$trsfdError[n]))
  }

  for (n in 1:nrow(data)) {
    if (data$EQR_LEAFPACS_Capped[n] > 0.95) {
      data$Bad[n] <- 0
    } else
    if (data$EQR_LEAFPACS_Capped[n] <= 0.056) {
      data$Bad[n] <- 88.1
    } else {
      data$Bad[n] <- round((100 * data$Normdist1[n]), 2)
    }
  }

  for (n in 1:nrow(data)) {
    if (data$EQR_LEAFPACS_Capped[n] > 0.95) {
      data$Poor[n] <- 0
    } else
    if (data$EQR_LEAFPACS_Capped[n] <= 0.056) {
      data$Poor[n] <- 9.6
    } else {
      data$Poor[n] <- round((100 * (data$Normdist2[n] - data$Normdist1[n])), 1)
    }
  }

  for (n in 1:nrow(data)) {
    if (data$EQR_LEAFPACS_Capped[n] > 0.95) {
      data$Moderate[n] <- 0
    } else
    if (data$EQR_LEAFPACS_Capped[n] <= 0.056) {
      data$Moderate[n] <- 2
    } else {
      data$Moderate[n] <- round((100 * (data$Normdist3[n] - data$Normdist2[n])), 2)
    }
  }

  for (n in 1:nrow(data)) {
    if (data$EQR_LEAFPACS_Capped[n] > 0.95) {
      data$Good[n] <- 0
    } else
    if (data$EQR_LEAFPACS_Capped[n] <= 0.056) {
      data$Good[n] <- 0.4
    } else {
      data$Good[n] <- round((100 * (data$Normdist4[n] - data$Normdist3[n])), 2)
    }
  }

  for (n in 1:nrow(data)) {
    if (data$EQR_LEAFPACS_Capped[n] > 0.95) {
      data$High[n] <- 100
    } else
    if (data$EQR_LEAFPACS_Capped[n] <= 0.056) {
      data$High[n] <- 0
    } else {
      data$High[n] <- round((100 * (1 - data$Normdist4[n])), 2)
    }
  }

  return(data)
}
