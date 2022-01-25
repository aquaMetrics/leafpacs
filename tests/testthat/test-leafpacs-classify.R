test_that("leafpacs classifies", {
   data <- get_demo_data()
   output <- leafpacs_predict(data[1, ])
   output <- leafpacs_classify(output)

  expect_equal(round(output$RMNI_EQR, 3), 0.632)
  expect_equal(round(output$RMNI_EQR_ADJ, 3), 0.525)
  expect_equal(round(output$NTAXA_EQR, 3), 1.134)
  expect_equal(round(output$NFG_EQR, 3), 1.244)
  expect_equal(round(output$ALG_EQR, 3), 0.516)
  expect_equal(round(output$Diversity_EQR_ADJ, 3), 1.158)
  expect_equal(round(output$ALG_EQR_ADJ, 3), 0.165)


})
