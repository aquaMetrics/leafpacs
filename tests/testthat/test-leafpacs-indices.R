test_that("leafpacs indices", {
  skip("long running test")
  # Find a suit test sample / site
  # data <- get_demo_data()
  # data <- data[data$question == "rn_a_taxa" & data$response == "1", ]
  # data <- data[data$sample_id == 372276, ]
  # data$location_id

  # Get raw taxonomic data from site
  data <- ead::get_observations("http://environment.data.gov.uk/ecology/site/bio/92751", take = 10000)

  # Filter by sample_id
  sample_id <- strsplit(data$truncated_id, "/|-")
  sample_id <- purrr::map(sample_id, function(x) {
    x[2]
  })
  data$sample_id <- unlist(sample_id)
  data <- data[data$sample_id == 372276, ]

  # Extract taxonomic code
  ultimate_foi_id <- strsplit(data$ultimate_foi_id, "/|-")
  ultimate_foi_id <- purrr::map(ultimate_foi_id, function(x) {
    x[7]
  })

  # Get taxa list
  taxa <- ead::get_taxa()

  # Join taxa lsit
  data$ultimate_foi_id <- unlist(ultimate_foi_id)
  obs <- dplyr::left_join(data, taxa, by = c("ultimate_foi_id" = "notation"))

  # Join parent name if species name doesn't join ------------------------------
  taxa <- dplyr::select(taxa, parentTlik, parentTaxonName)
  taxa <- unique(taxa)
  extra <- obs[is.na(obs$pref_label), ]
  extra <- dplyr::left_join(extra, taxa, by = c("ultimate_foi_id" = "parentTlik"))
  obs <- obs[!is.na(obs$pref_label), ]
  data <- dplyr::bind_rows(obs, extra)
  data$pref_label[is.na(data$pref_label)] <-
    data$parentTaxonName.y[is.na(data$pref_label)]

  # Rename columns to match package naming convention ----------------------------
  data <- data %>% dplyr::rename(
    data_taken = date,
    response = simple_result,
    question = property_id,
    location_id = site_id,
    taxon = pref_label
  )

  names(data) <- tolower(names(data))

  data <- data %>% dplyr::select(
    location_id,
    sample_id,
    data_taken,
    response,
    question,
    taxon
  )

  # Test calculated rmni ----------------------------------------------------
  # Should match pre-calculated rmni from EA database?

  indices <- leafpacs_indices(data)

  expect_equal(
    round(indices$value[indices$index == "rmni_score"], 2),
    as.numeric(data$response[data$question ==
      "http://environment.data.gov.uk/ecology/def/bio/Rmni"])
  )
})
