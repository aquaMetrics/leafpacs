#' Classify LEAFPACS
#'
#' Using pre-calculated sample metrics and predicted reference values this
#' function calculates the EQRs and classification for each sample.
#'
#' @param data Data frame of predicted and observed macrophyte indices
#'
#' @return Data frame containing classification results
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr mutate
#' @importFrom magrittr `%>%`
#'
#' @examples
#' \dontrun{
#' class <- leafpacs_classify(data)
#' }
leafpacs_classify <- function(data) {
  message("Calculating class")
  # Calculate EQRs ------------------------------------------------------------
  # RMNI EQR
  data <- data %>% mutate(RMNI_EQR = (.data$RMNI - 10) / (.data$REF_RMNI - 10))

  # NTAXA EQR
  data <- data %>%
    mutate(NTAXA_EQR = (.data$RN_A_TAXA / .data$REF_TAXA)) %>%
    mutate(RMNI_EQR_ADJ = .data$RMNI_EQR)

  # NFG EQR
  data <- data %>% mutate(NFG_EQR = (.data$N_RFG / .data$REF_NFG))

  # Algal EQR
  data <- data %>% mutate(ALG_EQR = (.data$RFA_PC - 100) / (.data$REF_ALGAE - 100))

  # Add adjustments to RMNI EQR------------------------------------------------
  for (n in 1:nrow(data)) {
    if (data$RMNI_EQR[n] > 1) {
      data[n, "RMNI_EQR_ADJ"] <- 1
    }
  }

  for (n in 1:nrow(data)) {
    if (data$RMNI_EQR[n] >= 0.85 & data$RMNI_EQR[n] < 1) {
      data[n, "RMNI_EQR_ADJ"] <-
        (((data$RMNI_EQR[n] - 0.85) / (1 - 0.85)) * 0.2 + 0.8)
    }
  }

  for (n in 1:nrow(data)) {
    if (data$RMNI_EQR[n] >= 0.70 & data$RMNI_EQR[n] < 0.85) {
      data[n, "RMNI_EQR_ADJ"] <-
        (((data$RMNI_EQR[n] - 0.7) / (0.85 - 0.7)) * 0.2 + 0.6)
    }
  }

  for (n in 1:nrow(data)) {
    if (data$RMNI_EQR[n] >= 0.52 & data$RMNI_EQR[n] < 0.7) {
      data[n, "RMNI_EQR_ADJ"] <-
        (((data$RMNI_EQR[n] - 0.52) / (0.7 - 0.52)) * 0.2 + 0.4)
    }
  }

  for (n in 1:nrow(data)) {
    if (data$RMNI_EQR[n] >= 0.34 & data$RMNI_EQR[n] < 0.52) {
      data[n, "RMNI_EQR_ADJ"] <-
        (((data$RMNI_EQR[n] - 0.34) / (0.52 - 0.34)) * 0.2 + 0.2)
    }
  }

  for (n in 1:nrow(data)) {
    if (data$RMNI_EQR[n] >= 0.16 & data$RMNI_EQR[n] < 0.34) {
      data[n, "RMNI_EQR_ADJ"] <-
        (((data$RMNI_EQR[n] - 0.16) / (0.34 - 0.16)) * 0.2)
    }
  }

  for (n in 1:nrow(data)) {
    if (data$RMNI_EQR[n] < 0.16) {
      data[n, "RMNI_EQR_ADJ"] <- 0
    }
  }

  # Adjustments to Ntaxa or NFG EQR -------------------------------------------
  for (n in 1:nrow(data)) {
    if (min(data$NTAXA_EQR[n], data$NFG_EQR[n]) >= 0.83) {
      data[n, "Diversity_EQR_ADJ"] <-
        (((min(
          data$NTAXA_EQR[n],
          data$NFG_EQR[n]
        ) - 0.83) / (1 - 0.83)) * 0.2 + 0.8)
    }
  }

  for (n in 1:nrow(data)) {
    if (min(data$NTAXA_EQR[n], data$NFG_EQR[n]) >= 0.66 &
      min(data$NTAXA_EQR[n], data$NFG_EQR[n]) < 0.83) {
      data[n, "Diversity_EQR_ADJ"] <-
        (((min(
          data$NTAXA_EQR[n],
          data$NFG_EQR[n]
        ) - 0.66) / (0.83 - 0.66)) * 0.2 + 0.6)
    }
  }

  for (n in 1:nrow(data)) {
    if (min(data$NTAXA_EQR[n], data$NFG_EQR[n]) >= 0.49 &
      min(data$NTAXA_EQR[n], data$NFG_EQR[n]) < 0.66) {
      data[n, "Diversity_EQR_ADJ"] <-
        (((min(
          data$NTAXA_EQR[n],
          data$NFG_EQR[n]
        ) - 0.49) / (0.66 - 0.49)) * 0.2 + 0.4)
    }
  }

  for (n in 1:nrow(data)) {
    if (min(data$NTAXA_EQR[n], data$NFG_EQR[n]) >= 0.32 &
      min(data$NTAXA_EQR[n], data$NFG_EQR[n]) < 0.49) {
      data[n, "Diversity_EQR_ADJ"] <-
        (((min(
          data$NTAXA_EQR[n],
          data$NFG_EQR[n]
        ) - 0.32) / (0.49 - 0.32)) * 0.2 + 0.2)
    }
  }

  for (n in 1:nrow(data)) {
    if (min(data$NTAXA_EQR[n], data$NFG_EQR[n]) < 0.32) {
      data[n, "Diversity_EQR_ADJ"] <-
        (((min(
          data$NTAXA_EQR[n],
          data$NFG_EQR[n]
        ) - 0.15) / (0.32 - 0.15)) * 0.2)
    }
  }

  # Adjustments to ALGAL EQR ---------------------------------------------------

  for (n in 1:nrow(data)) {
    if (data$ALG_EQR[n] >= 0.975) {
      data[n, "ALG_EQR_ADJ"] <-
        (((data$ALG_EQR[n] - 0.975) / (1 - 0.975)) * 0.2 + 0.8)
    }
  }

  for (n in 1:nrow(data)) {
    if (data$ALG_EQR[n] >= 0.925 & data$ALG_EQR[n] < 0.975) {
      data[n, "ALG_EQR_ADJ"] <-
        (((data$ALG_EQR[n] - 0.925) / (0.975 - 0.925)) * 0.2 + 0.6)
    }
  }

  for (n in 1:nrow(data)) {
    if (data$ALG_EQR[n] >= 0.825 & data$ALG_EQR[n] < 0.925) {
      data[n, "ALG_EQR_ADJ"] <-
        (((data$ALG_EQR[n] - 0.825) / (0.925 - 0.825)) * 0.2 + 0.4)
    }
  }

  for (n in 1:nrow(data)) {
    if (data$ALG_EQR[n] >= 0.625 &
      data$ALG_EQR[n] < 0.825) {
      data[n, "ALG_EQR_ADJ"] <-
        (((data$ALG_EQR[n] - 0.625) / (0.825 - 0.625)) * 0.2 + 0.2)
    }
  }

  for (n in 1:nrow(data)) {
    if (data$ALG_EQR[n] < 0.625) {
      data[n, "ALG_EQR_ADJ"] <-
        ((data$ALG_EQR[n] - 0.625) * 0.2 + 0.8)
    }
  }

  # Combining the ecological ratios for each metric ---------------------------
  for (n in 1:nrow(data)) {
    if (data$Diversity_EQR_ADJ[n] < data$RMNI_EQR_ADJ[n]) {
      data[n, "CompositionDiveristy"] <-
        ((0.5 * data$Diversity_EQR_ADJ[n] + data$RMNI_EQR_ADJ[n])) / 1.5
    } else {
      data[n, "CompositionDiveristy"] <- data$RMNI_EQR_ADJ[n]
    }
  }
  for (n in 1:nrow(data)) {
    data[n, "Z"] <-
      (2 *
        (1 / (exp(log(2600000000) + data$REF_RMNI[n] * log(0.0166)) + 1 / 0.5)))
  }

  for (n in 1:nrow(data)) {
    if (data$CompositionDiveristy[n] < data$ALG_EQR_ADJ[n]) {
      data[n, "EQR_LEAFPACS"] <- data$CompositionDiveristy[n]
    } else {
      data[n, "EQR_LEAFPACS"] <-
        (data$Z[n] * data$ALG_EQR_ADJ[n] +
          data$CompositionDiveristy[n]) / (data$Z[n] + 1)
    }
  }

  # Cap final EQR -------------------------------------------------------------
  data$EQR_LEAFPACS_Capped <- data$EQR_LEAFPACS
  for (n in 1:nrow(data)) {
    if (data$EQR_LEAFPACS[n] > 1) {
      data[n, "EQR_LEAFPACS_Capped"] <- 1
    }
  }

  # Calculate CLASS ----------------------------------------------------------
  for (n in 1:nrow(data)) {
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

  return(data)
}
