#' Calculate river LEAFPACS indices
#'
#' @param data Data frame of macrophyte taxa and cover values.
#'
#' @return Data frame containing indices
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr inner_join group_by summarise n_distinct ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom magrittr `%>%`
#' @examples
#' indices <- leafpacs_indices(taxa_data)
leafpacs_indices <- function(data) {
  message("Calculating indices...")
  message("THIS PACKAGE IS A WORK IN PROGRESS - DON'T USE IN PRODUCTION.")

  if (!"taxon" %in% colnames(data)) {
    return(NULL)
  }
  if (all(is.na(data$taxon))) {
    return(NULL)
  }

  if (nrow(data[data$question == "rmni",]) > 0) {
    return(NULL)
  }


  # Join data to taxa macrophyte scores ------------------------------------
  macrophyte_scores <- utils::read.csv(system.file("extdat",
    "macrophyte-taxa.csv",
    package = "leafpacs"
  ))

  data$taxon[data$taxon == "Cladophora"] <-
    "Cladophora glomerata/Rhizoclonium hieroglyphicum"
  data <- inner_join(data, macrophyte_scores, by = c("taxon" = "Taxon.Name"))
  names(data) <- tolower(names(data))

  # Calculate scores -------------------------------------------------------
  data <- group_by(data, .data$sample_id)
  data$response <- as.numeric(data$response)

  # Calculate algae
  data$cover <- NA
  data$cover[data$response == 1] <- 0.05
  data$cover[data$response == 2] <- 0.5
  data$cover[data$response == 3] <- 1.7
  data$cover[data$response == 4] <- 3.8
  data$cover[data$response == 5] <- 7.5
  data$cover[data$response == 6] <- 17.5
  data$cover[data$response == 7] <- 37.5
  data$cover[data$response == 9] <- 62.5
  data$cover[data$response == 10] <- 87.5

  algae <- data %>%
    group_by(.data$cover) %>%
    summarise(
      n = n_distinct(.data$taxon[.data$algal.taxon == "Y"])
    )
  algae_cover <- sum(algae$cover * algae$n, na.rm = TRUE)

  # Calculate indices
  data <- data %>% summarise(
    rmni = sum(.data$rmni * .data$response, na.rm = TRUE) /
      sum(.data$response[!is.na(.data$rmni)]),
    rn_a_taxa = n_distinct(.data$taxon[.data$aquatic.taxon == "Y"]),
    n_rfg = n_distinct(.data$plant.fn.gp[.data$plant.fn.gp != 0 &
                                           !is.na(.data$plant.fn.gp)]),
    rfa_pc = algae_cover
  )

  data <- pivot_longer(data, -.data$sample_id,
    names_to = "question",
    values_to = "response"
  )

  data$response <- as.character(data$response)
  data  <- ungroup(data)
  return(data)
}
