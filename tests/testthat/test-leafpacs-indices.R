test_that("leafpacs indices", {

  # Find a suit test sample / site
  # data <- get_demo_data()
  # data <- data[data$question == "rn_a_taxa" & data$response == "1", ]
  # data <- data[data$sample_id == 372276, ]
  # data$location_id

  # Get raw taxonomic data from site
  data <- hera::get_data(92751)
  data <- data[data$sample_id == "372276" , ]
  taxa_only <- data[data$question == "percentagecoverband", ]

  indices <- leafpacs_indices(taxa_only)

  # Should match pre-calculated rmni from EA database?
  expect_equal(
    round(as.numeric(indices$response[indices$question == "rmni"]), 2),
    as.numeric(data$response[data$question ==
      "rmni"])
  )
})
