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
#' @importFrom dplyr transmute
#'
#' @examples
#' \dontrun{
#' predictions <- leafpacs_predict(data)
#' }
leafpacs_predict <- function(data, reference_algae = 0.05) {

  # Predict reference RMNI
  prediction <-
    transmute(data,
      REF_RMNI =
        (5.239 + (1.384 * log10(.data$ALKALINITY + 1)) +
          (-0.68 * log10(.data$SLOPE + 1)) +
          (0.711 * log10(.data$DIST_FROM_SOURCE + 1)) +
          (-1.074 * log10(.data$SOURCE_ALTITUDE + 1))
        )
    )

  # Calculate Reference Taxa
  prediction$REF_TAXA <- (10.026 * exp(log10(data$SLOPE + 1) * -0.426))

  # Calculate Reference Number of Functional Groups
  prediction$REF_NFG <- (6.304 * exp(log10(data$SLOPE + 1) * -0.377))

  # Add Reference Algae
  prediction$REF_ALGAE <- reference_algae

  # Sample ID
  prediction$SAMPLE_ID <- data$SAMPLE_ID

  prediction <- inner_join(data, prediction, by = "SAMPLE_ID")

  return(prediction)
}
