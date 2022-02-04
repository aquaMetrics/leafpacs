test_that("leafpacs function", {

  # Test with indices data
  data <- get_demo_data()
  data <- leafpacs(data)

  # Test with taxa data
  data <- taxa_data
  data <- data[!is.na(data$taxon), ]
  data <- leafpacs(data)

  # Test with web


})
