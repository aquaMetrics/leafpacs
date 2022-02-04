test_that("test leafpacs prediction", {
  data <- get_demo_data()
  predicted <- leafpacs_predict(data[data$location_id == 159293,] )

  predicted <- as.numeric(predicted$response)
  # Round to 2 decimals to match spreadsheet
  predicted <- round(predicted, 2)

  # Test against values calculated on spreadsheet
  expect_equal(predicted[1:4], c(8.82, 0.05, 5.63, 7.42))

})
