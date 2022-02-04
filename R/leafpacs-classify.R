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
#' @importFrom dplyr mutate distinct group_by
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr `%>%`
#' @examples
#' \dontrun{
#' class <- leafpacs_classify(data)
#' }
leafpacs_classify <- function(data) {
  message("Calculating class...")
  message("THIS PACKAGE IS A WORK IN PROGRESS - DON'T USE IN PRODUCTION.")
  # Transform data -----------------------------------------------------------
  data <- data %>%
    select(.data$sample_id, .data$question, .data$response) %>%
    filter(.data$question %in% c("rmni",
                           "rn_a_taxa",
                           "n_rfg",
                           "ref_taxa",
                           "ref_algae",
                           "ref_nfg",
                           "ref_rmni",
                           "rfa_pc"))
    data$response <- as.numeric(data$response)
    data <- data %>% distinct() %>%
    group_by(.data$sample_id) %>%
    pivot_wider(names_from = .data$question, values_from = .data$response) %>%
    ungroup()

  # Calculate EQRs ------------------------------------------------------------
  # RMNI EQR
  data <- data %>% mutate(rmni_eqr = (.data$rmni - 10) / (.data$ref_rmni - 10))

  # NTAXA EQR
  data <- data %>%
    mutate(ntaxa_eqr = (.data$rn_a_taxa / .data$ref_taxa)) %>%
    mutate(rmni_eqr_adj = .data$rmni_eqr)

  # NFG EQR
  data <- data %>% mutate(nfg_eqr = (.data$n_rfg / .data$ref_nfg))

  # Algal EQR
  data <- data %>%
    mutate(alg_eqr = (.data$rfa_pc - 100) / (.data$ref_algae - 100))

  # Adjust RMNI EQR------------------------------------------------
  data$rmni_eqr_adj[data$rmni_eqr >= 0.85] <-
    (data$rmni_eqr[data$rmni_eqr >= 0.85]
    - 0.85) / (1 - 0.85) * 0.2 + 0.8

  data$rmni_eqr_adj[data$rmni_eqr < 0.85] <-
    (data$rmni_eqr[data$rmni_eqr < 0.85]
    - 0.7) / (0.85 - 0.7) * 0.2 + 0.6

  data$rmni_eqr_adj[data$rmni_eqr < 0.7] <-
    (data$rmni_eqr[data$rmni_eqr < 0.7]
    - 0.52) / (0.7 - 0.52)* 0.2 + 0.4

  data$rmni_eqr_adj[data$rmni_eqr < 0.52] <-
    (data$rmni_eqr[data$rmni_eqr < 0.52]
    - 0.34) / (0.52 - 0.34) * 0.2 + 0.2

  data$rmni_eqr_adj[data$rmni_eqr < 0.34] <-
    (data$rmni_eqr[data$rmni_eqr < 0.34]
    - 0.16) / (0.34 - 0.16) * 0.2

  # Adjust Ntaxa or NFG EQR ---------------------------------------------------
  data$min_eqr <- pmin(data$ntaxa_eqr, data$nfg_eqr)
  data$diversity_eqr_adj <- NA
  data$diversity_eqr_adj[data$min_eqr >= 0.83] <-
    ((data$min_eqr[data$min_eqr >= 0.83]
    - 0.83) / (1 - 0.83)) * 0.2 + 0.8

  data$diversity_eqr_adj[data$min_eqr < 0.83] <-
    ((data$min_eqr[data$min_eqr < 0.83]
    - 0.66) / (0.83 - 0.66)) * 0.2 + 0.6

  data$diversity_eqr_adj[data$min_eqr < 0.66] <-
    ((data$min_eqr[data$min_eqr < 0.66]
    - 0.49) / (0.66 - 0.49)) * 0.2 + 0.4

  data$diversity_eqr_adj[data$min_eqr < 0.49] <-
    ((data$min_eqr[data$min_eqr < 0.49]
    - 0.32) / (0.49 - 0.32)) * 0.2 + 0.2

  data$diversity_eqr_adj[data$min_eqr < 0.32] <-
    ((data$min_eqr[data$min_eqr < 0.32]
    - 0.15) / (0.32 - 0.15)) * 0.2

  # Adjust ALGAL EQR ----------------------------------------------------------
  data$alg_eqr_adj <- NA
  data$alg_eqr_adj[data$alg_eqr >= 0.975] <-
    ((data$alg_eqr[data$alg_eqr >= 0.975]
    - 0.975) / (1 - 0.975)) * 0.2 + 0.8

  data$alg_eqr_adj[data$alg_eqr < 0.975] <-
    ((data$alg_eqr[data$alg_eqr < 0.975]
    - 0.925) / (0.975 - 0.925)) * 0.2 + 0.6

  data$alg_eqr_adj[data$alg_eqr < 0.925] <-
    ((data$alg_eqr[data$alg_eqr < 0.925]
    - 0.825) / (0.925 - 0.825)) * 0.2 + 0.4

  data$alg_eqr_adj[data$alg_eqr < 0.825] <-
    ((data$alg_eqr[data$alg_eqr < 0.825]
    - 0.625) / (0.825 - 0.625)) * 0.2 + 0.2

  data$alg_eqr_adj[data$alg_eqr < 0.625] <-
    (data$alg_eqr[data$alg_eqr < 0.625]
    / 0.625) * 0.2

  # Combine EQR for each metric ---------------------------
  data$composition_diversity <- data$rmni_eqr_adj
  data$composition_diversity[data$diversity_eqr_adj < data$rmni_eqr_adj] <-
    ((0.5 * data$diversity_eqr_adj[data$diversity_eqr_adj < data$rmni_eqr_adj] +
      data$rmni_eqr_adj[data$diversity_eqr_adj < data$rmni_eqr_adj])) / 1.5

  data$z <-
    (2 * (1 / (exp(log(2600000000) + data$ref_rmni * log(0.0166)) + 1 / 0.5)))

  data$eqr_leafpacs <-
    (data$z * data$alg_eqr_adj + data$composition_diversity) / (data$z + 1)

  data$eqr_leafpacs[data$composition_diversity < data$alg_eqr_adj] <-
    data$composition_diversity[data$composition_diversity < data$alg_eqr_adj]

  # Cap final EQR -------------------------------------------------------------
  data$eqr <- data$eqr_leafpacs
  data$eqr[data$eqr_leafpacs > 1] <- 1

  # Calculate class ----------------------------------------------------------
  data$class <- NA
  data$class[data$eqr_leafpacs >= 0.8] <- "high"
  data$class[data$eqr_leafpacs < 0.8] <- "good"
  data$class[data$eqr_leafpacs < 0.6] <- "moderate"
  data$class[data$eqr_leafpacs < 0.4] <- "poor"
  data$class[data$eqr_leafpacs < 0.2] <- "bad"

  return(data)
}
