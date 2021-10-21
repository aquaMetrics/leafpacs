# Helper functions -------------------------------------------------
#' @importFrom rlang .data
#' @importFrom dplyr mutate filter inner_join
#' @importFrom magrittr `%>%`
#' @importFrom lubridate month year

# Calculated values ----------------------------------------------------------
calculated_values <- function(data) {

  # Month and Year columns
  data <- data %>%
    mutate(
      SAMPLE_DATE =
        as.Date(paste(substr(.data$SAMPLE_DATE, start = 1, stop = 10)),
          format = "%d/%m/%Y"
        )
    ) %>%
    mutate(YEAR = as.integer(year(.data$SAMPLE_DATE))) %>%
    mutate(MONTH = as.integer(month(.data$SAMPLE_DATE)))

  # Log Slope
  data$logSlope <- log10(data$SLOPE)

  return(data)
}

