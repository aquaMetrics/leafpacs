#' Convert EA data to standard input
#'
#' @param data Data frame of macrophyte taxa and cover values from EA bulk
#'   download.
#'
#' @return Data frame
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr rename
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#' @importFrom magrittr `%>%`
#' @examples
#' # Load data
#' library(dplyr)
#' indices <- utils::read.csv(system.file("extdat",
#'                                        "MACP_OPEN_DATA_METRICS.csv",
#'                                        package = "leafpacs"
#' ))
#'
#' predictors <- utils::read.csv(system.file("extdat",
#'                                           "MACP_OPEN_DATA_SITE.csv",
#'                                           package = "leafpacs"
#' ))
#' # Tidy data
#' data <- dplyr::inner_join(indices, predictors, by = "SITE_ID")
#' data <- dplyr::select(data, -.data$REPLICATE_CODE)
#' data <- data %>% dplyr::filter(complete.cases(data))
#' data <- leafpacs_convert_ea(data)
leafpacs_convert_ea <- function(data) {


  data <- tibble(data)

  names(data) <- tolower(names(data))
  data <- data %>% rename(
    location_id = .data$site_id,
    date_taken = .data$sample_date
  )

  data <- data %>% select(.data$location_id,
                          .data$sample_id,
                          .data$date_taken,
                          .data$rmni,
                          .data$rn_a_taxa,
                          .data$n_rfg,
                          .data$rfa_pc,
                          .data$alkalinity,
                          .data$source_altitude,
                          .data$dist_from_source,
                          .data$slope)

  data$quality_element <- "River Macrophytes"

  data <- data %>% pivot_longer(
    cols = c(.data$rmni, .data$rn_a_taxa, .data$n_rfg, .data$rfa_pc),
    names_to = "question",
    values_to = "response"
  )

  data$sample_id <- as.character(data$sample_id)
  data$response <- as.character(data$response)
  return(data)

}
