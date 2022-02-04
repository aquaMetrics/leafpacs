#' Taxanomic macrophyte data
#'
#' A dataset containing one macrophyte sample.
#'
#' @format A data frame with 10 rows and 11 variables:
#' \describe{
#'   \item{location_id}{Unique ID of location sampled}
#'   \item{sample_id}{Unique ID of sample}
#'   \item{date_taken}{Date the sample was taken}
#'   \item{question}{What is question is being asked, in this instance `Percentage Cover Band` of a given taxon}
#'   \item{response}{Value collect in reponse to question}
#'   \item{taxon}{The taxon associated with the question being asked if
#'   applicable}
#'   \item{alkalinity}{alkalinity value}
#'   \item{source_altitude}{Source altitude}
#'   \item{dist_from_source}{Distance from source}
#'   \item{slope}{Slope}
#'   \item{quality_element}{Quality element}
#' }
"taxa_data"
