#' Predict LEAFPACS nutrient index, taxa, functional groups, and algae
#'
#' Function to predict LEAFPACS River Macrophyte Nutrient Index (RMNI), Number
#' of Functional Groups and add 0.05 for reference algae.
#'
#' @param data Data frame of river LEAFPACS predictors
#' @param reference_algae Number representing reference algae. Default is 0.05
#'   set by the river LEAFPACS standard.
#'
#' @return Data frame of reference predictions
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr transmute mutate_all
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \dontrun{
#' predictions <- leafpacs_predict(data)
#' }
leafpacs_predict <- function(data, reference_algae = 0.05) {
  message("Calculating predictions...")
  data <- data %>% select(
    .data$sample_id,
    .data$alkalinity,
    .data$dist_from_source,
    .data$source_altitude,
    .data$slope
  )
  data <- data %>% distinct()
  data <- data[complete.cases(data), ]
  data <- data %>% mutate_all(type.convert, as.is = TRUE)
  data$sample_id <- as.character(data$sample_id)
  # Predict reference RMNI
  prediction <-
    transmute(data,
      ref_rmni =
        (5.239 + (1.384 * log10(.data$alkalinity + 1)) +
          (-0.68 * log10(.data$slope + 1)) +
          (0.711 * log10(.data$dist_from_source + 1)) +
          (-1.074 * log10(.data$source_altitude + 1))
        )
    )

  # Calculate Reference Taxa
  prediction$ref_taxa <- (10.026 * exp(log10(data$slope + 1) * -0.426))

  # Calculate Reference Number of Functional Groups
  prediction$ref_nfg <- (6.304 * exp(log10(data$slope + 1) * -0.377))

  # Add Reference Algae
  prediction$ref_algae <- reference_algae

  # Sample ID
  prediction$sample_id <- data$sample_id

  # Transform to standard format --------------------------------------------
  prediction <- prediction %>%
    distinct() %>%
    pivot_longer(
      cols = c(
        .data$ref_taxa,
        .data$ref_algae,
        .data$ref_nfg,
        .data$ref_rmni
      ),
      names_to = "question",
      values_to = "response"
    )

  prediction$response <- as.character(prediction$response)
  return(prediction)
}
