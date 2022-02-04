test_that("leafpacs classifies", {
  data <- get_demo_data()
  predictions <- leafpacs_predict(data)
  data <- bind_rows(data, predictions)
  data <- leafpacs_classify(data)

  # Test against values from spreadsheet using same sample data
  expect_equal(round(data$rmni_eqr[1], 3), 0.632)
  expect_equal(round(data$rmni_eqr_adj[1], 3), 0.525)
  expect_equal(round(data$ntaxa_eqr[1], 3), 1.134)
  expect_equal(round(data$nfg_eqr[1], 3), 1.244)
  expect_equal(round(data$alg_eqr[1], 3), 0.516)
  expect_equal(round(data$diversity_eqr_adj[1], 3), 1.158)
  expect_equal(round(data$alg_eqr_adj[1], 3), 0.165)

  # Checking small bug is definitely fixed
  data <- data[data$sample_id == 369898, ]
  expect_equal(round(data$eqr[1], 3), 0.030)
})
