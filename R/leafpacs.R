#' Run LEAFPACS indices, prediction and classification
#'
#' This is a wrapper function to run all the utility functions together to give
#' a final output.
#'
#' @param data Data frame of taxonomic or indices results
#'
#' @return Data frame containing indices, predictions and classification
#' @export
#' @importFrom dplyr bind_rows
#' @examples
#' results <- leafpacs(taxa_data)
#' \dontrun{
#' data <- hera::get_data(location_id = 92751)
#' class <- leafpacs(data)
#' }
leafpacs <- function(data) {
  data <- validate(data)
  indices <- leafpacs_indices(data)
  data <- bind_rows(data, indices)
  predictions <- leafpacs_predict(data)
  data <- bind_rows(data, predictions)
  data <- leafpacs_classify(data)
  data <- leafpacs_confidence(data)
  return(data)
}
