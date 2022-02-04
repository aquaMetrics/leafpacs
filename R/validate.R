#' Validate input data
#'
#' Validate input data for correctness and adds calculated variables such log
#' slope
#'
#' @param data Data frame of macrophyte taxonomic data and/or predictors
#'
#' @return Data frame of validated input data
#' @export
#'
#' @examples
#' \dontrun{
#' data <- validate(1)
#' }
validate <- function(data) {
  message("Validating data...")

  # Function to check for fatal errors i.e. no data entered

  # Function to add calculate variables e.g. log values etc
  data <- calculated_values(data)


  # Function to validate each sample individually (removing invalid)

  # ...
  return(data)
}
