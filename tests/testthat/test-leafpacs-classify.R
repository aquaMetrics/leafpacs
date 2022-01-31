test_that("leafpacs classifies", {
   demo_data <- get_demo_data()
   data <- leafpacs_predict(demo_data[1, ])
   data <- leafpacs_classify(data)

  expect_equal(round(data$RMNI_EQR, 3), 0.632)
  expect_equal(round(data$RMNI_EQR_ADJ, 3), 0.525)
  expect_equal(round(data$NTAXA_EQR, 3), 1.134)
  expect_equal(round(data$NFG_EQR, 3), 1.244)
  expect_equal(round(data$ALG_EQR, 3), 0.516)
  expect_equal(round(data$Diversity_EQR_ADJ, 3), 1.158)
  expect_equal(round(data$ALG_EQR_ADJ, 3), 0.165)


})
