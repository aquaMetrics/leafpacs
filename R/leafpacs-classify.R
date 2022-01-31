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
  message("THIS PACKAGE IS A WORK IN PROGRESS PLEASE DON'T USE IN PRODUCTION.")
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
  data <- data %>%
    mutate(ALG_EQR = (.data$RFA_PC - 100) / (.data$REF_ALGAE - 100))

  # Add adjustments to RMNI EQR------------------------------------------------
  data$RMNI_EQR_ADJ[data$RMNI_EQR >= 0.85] <-
    ((data$RMNI_EQR[data$RMNI_EQR >= 0.85]
    - 0.85) / (1 - 0.85)) * 0.2 + 0.8

  data$RMNI_EQR_ADJ[data$RMNI_EQR < 0.85] <-
    ((data$RMNI_EQR[data$RMNI_EQR < 0.85]
    - 0.7) / (0.85 - 0.7)) * 0.2 + 0.6

  data$RMNI_EQR_ADJ[data$RMNI_EQR < 0.7] <-
    ((data$RMNI_EQR[data$RMNI_EQR < 0.7]
    - 0.52) / (0.7 - 0.52)) * 0.2 + 0.4

  data$RMNI_EQR_ADJ[data$RMNI_EQR < 0.52] <-
    ((data$RMNI_EQR[data$RMNI_EQR < 0.52]
    - 0.34) / (0.52 - 0.34)) * 0.2 + 0.2

  data$RMNI_EQR_ADJ[data$RMNI_EQR < 0.34] <-
    ((data$RMNI_EQR_ADJ[data$RMNI_EQR < 0.34]
    - 0.16) / (0.34 - 0.16)) * 0.2

  # Adjustments to Ntaxa or NFG EQR -------------------------------------------
  data$MIN_EQR <- min(data$NTAXA_EQR, data$NFG_EQR)

  data$Diversity_EQR_ADJ[data$MIN_EQR >= 0.83] <-
    ((data$MIN_EQR[data$MIN_EQR >= 0.83]
    - 0.83) / (1 - 0.83)) * 0.2 + 0.8

  data$Diversity_EQR_ADJ[data$MIN_EQR < 0.83] <-
    ((data$MIN_EQR[data$MIN_EQR < 0.83]
    - 0.66) / (0.83 - 0.66)) * 0.2 + 0.6

  data$Diversity_EQR_ADJ[data$MIN_EQR < 0.66] <-
    ((data$MIN_EQR[data$MIN_EQR < 0.66]
    - 0.49) / (0.66 - 0.49)) * 0.2 + 0.4

  data$Diversity_EQR_ADJ[data$MIN_EQR < 0.49] <-
    ((data$MIN_EQR[data$MIN_EQR < 0.49]
    - 0.32) / (0.49 - 0.32)) * 0.2 + 0.2

  data$Diversity_EQR_ADJ[data$MIN_EQR < 0.32] <-
    ((data$MIN_EQR[data$MIN_EQR < 0.32]
    - 0.15) / (0.32 - 0.15)) * 0.2

  # Adjustments to ALGAL EQR ---------------------------------------------------
  data$ALG_EQR_ADJ[data$ALG_EQR >= 0.975] <-
    ((data$ALG_EQR[data$ALG_EQR >= 0.975]
    - 0.975) / (1 - 0.975)) * 0.2 + 0.8

  data$ALG_EQR_ADJ[data$ALG_EQR < 0.975] <-
    ((data$ALG_EQR[data$ALG_EQR < 0.975]
    - 0.925) / (0.975 - 0.925)) * 0.2 + 0.6

  data$ALG_EQR_ADJ[data$ALG_EQR < 0.925] <-
    ((data$ALG_EQR[data$ALG_EQR < 0.925]
    - 0.825) / (0.925 - 0.825)) * 0.2 + 0.4

  data$ALG_EQR_ADJ[data$ALG_EQR < 0.825] <-
    ((data$ALG_EQR[data$ALG_EQR < 0.825]
    - 0.625) / (0.825 - 0.625)) * 0.2 + 0.2

  data$ALG_EQR_ADJ[data$ALG_EQR < 0.625] <-
    (data$ALG_EQR[data$ALG_EQR < 0.625]
    / 0.625) * 0.2

  # Combining the ecological ratios for each metric ---------------------------
  data$CompositionDiveristy <- data$RMNI_EQR_ADJ
  data$CompositionDiveristy[data$Diversity_EQR_ADJ < data$RMNI_EQR_ADJ] <-
    ((0.5 * data$Diversity_EQR_ADJ[data$Diversity_EQR_ADJ < data$RMNI_EQR_ADJ] +
      data$RMNI_EQR_ADJ[data$Diversity_EQR_ADJ < data$RMNI_EQR_ADJ])) / 1.5

  data$Z <-
    (2 * (1 / (exp(log(2600000000) + data$REF_RMNI * log(0.0166)) + 1 / 0.5)))

  data$EQR_LEAFPACS <-
    (data$Z * data$ALG_EQR_ADJ + data$CompositionDiveristy) / (data$Z + 1)

  data$EQR_LEAFPACS[data$CompositionDiveristy < data$ALG_EQR_ADJ] <-
    data$CompositionDiveristy

  # Cap final EQR -------------------------------------------------------------
  data$EQR_LEAFPACS_Capped <- data$EQR_LEAFPACS
  data$EQR_LEAFPACS_Capped[data$EQR_LEAFPACS > 1] <- 1

  # Calculate CLASS ----------------------------------------------------------
  data$CLASS[data$EQR_LEAFPACS >= 0.8] <- "HIGH"
  data$CLASS[data$EQR_LEAFPACS < 0.8] <- "GOOD"
  data$CLASS[data$EQR_LEAFPACS < 0.6] <- "MODERATE"
  data$CLASS[data$EQR_LEAFPACS < 0.4] <- "POOR"
  data$CLASS[data$EQR_LEAFPACS < 0.2] <- "BAD"

  return(data)
}
