#' Calculate river LEAFPACS indices
#'
#' @param data Data frame of macrophyte taxa and cover values.
#'
#' @return Data frame containing indices
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr inner_join group_by
#' @examples
#' \dontrun{
#' indices <- leafpacs_indices(data)
#' }
leafpacs_indices <- function(data) {
  message("Calculating indices")
  # Join data to macrophyte scores
  # macrophyte_scores <- leafpacs::macrophyte_scores
  # inner_join(data, macrophyte_scores, by = c('taxon' = 'taxon'))

  # Calculate RMNI
  # rmni <- group_by(data, .data$sample_id)

  return(data)
}
