#' Get Environment Agency demo data
#'
#' @return dataframe of demo data for testing
#' @export
#'
#' @examples
#' \dontrun{
#' data <- get_demo_data()
#' }
get_demo_data <- function() {

  # Load data
  indices <- utils::read.csv(system.file("extdat",
                                         "MACP_OPEN_DATA_METRICS.csv",
                                         package = "leafpacs"))

  predictors <- utils::read.csv(system.file("extdat",
                                            "MACP_OPEN_DATA_SITE.csv",
                                            package = "leafpacs"))

  # Tidy data
  data <- inner_join(indices, predictors, by = "SITE_ID")
  data <- select(data, -.data$REPLICATE_CODE)
  data <- data %>% filter(complete.cases(.))

  return(data)
}
