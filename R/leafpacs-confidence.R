#' Calculate Confidence of Class
#'
#' @param data Data frame of classification results
#'
#' @return Data frame containing confidence of class
#' @export
#' @importFrom stats pnorm
#'
#' @examples
#' data <- data.frame(eqr = c(0.999))
#' confidence <- leafpacs_confidence(data)
leafpacs_confidence <- function(data) {
  message("Calculating confidence...")

  # Add confidence columns ---------------------------------------------------
  data$se <- NA
  data$trsfd_mean <- NA
  data$trsfd_error <- NA
  data$norm_dist_1 <- NA
  data$norm_dist_2 <- NA
  data$norm_dist_3 <- NA
  data$norm_dist_4 <- NA
  data$bad <- NA
  data$poor <- NA
  data$moderate <- NA
  data$good <- NA
  data$high <- NA

  # SE value ------------------------------------------------------------------
  data$se <- 0.04 + -2.98 * data$eqr + 2.96 * data$eqr^0.95 / sqrt(1)
  data$trsfd_mean <- log(data$eqr / (1 - data$eqr))
  data$trsfd_error <- (data$se) / (data$eqr * (1 - data$eqr))
  data$norm_dist_1 <- pnorm((-1.386 - data$trsfd_mean) / (data$trsfd_error))
  data$norm_dist_2 <- pnorm((-0.405 - data$trsfd_mean) / (data$trsfd_error))
  data$norm_dist_3 <- pnorm((0.405 - data$trsfd_mean) / (data$trsfd_error))
  data$norm_dist_4 <- pnorm((1.386 - data$trsfd_mean) / (data$trsfd_error))

  data$bad <- round((100 * data$norm_dist_1), 1)
  data$bad[data$eqr > 0.95] <- 0
  data$bad[data$eqr <= 0.056] <- 88.1

  data$poor <- round((100 * (data$norm_dist_2 - data$norm_dist_1)), 1)
  data$poor[data$eqr > 0.95] <- 0
  data$poor[data$eqr <= 0.056] <- 9.6

  data$moderate <- round((100 * (data$norm_dist_3 - data$norm_dist_2)), 1)
  data$moderate[data$eqr > 0.95] <- 0
  data$moderate[data$eqr <= 0.056] <- 2

  data$good <- round((100 * (data$norm_dist_4 - data$norm_dist_3)), 1)
  data$good[data$eqr > 0.95] <- 0
  data$good[data$eqr <= 0.056] <- 0.4

  data$high <- round((100 * (1 - data$norm_dist_4)), 1)
  data$high[data$eqr > 0.95] <- 100
  data$high[data$eqr <= 0.056] <- 0

  return(data)
}
