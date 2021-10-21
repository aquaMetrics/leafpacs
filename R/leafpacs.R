#' Run LEAFPACS indices, prediction and classification
#'
#' This is a wrapper function to run all the utility functions together to give
#' a final output.
#'
#' @param data Data frame of taxonomic or indices results
#'
#' @return Data frame containing indices, predictions and classification
#' @export
#'
#' @examples
#' \dontrun{
#' results <- leafpacs(data)
#'}
leafpacs <- function(data) {

  # data <- leafpacs:::get_leafpacs()
  data <- validate(data)
  data <- leafpacs_indices(data)
  data <- leafpacs_predict(data)
  data <- leafpacs_classify(data)
  data <- leafpacs_confidence(data)
  message("All done!")
  return(data)
}
