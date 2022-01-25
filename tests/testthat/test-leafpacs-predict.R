test_that("test leafpacs prediction", {
  data <- get_demo_data()
  output <- leafpacs_predict(data[1, ])
  expect_equal(round(output$REF_RMNI, 3), 7.421)
  expect_equal(round(output$REF_TAXA, 2), 8.82)
  expect_equal(round(output$REF_NFG, 2), 5.63)
})
