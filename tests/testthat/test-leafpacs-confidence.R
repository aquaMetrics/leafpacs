test_that("classify", {

  data <- data.frame(eqr = c(0.999), location_id = 1)
  confidence <- leafpacs_confidence(data)
  expect_equal(confidence$high, 100)

  data <- data.frame(eqr = c(0.1))
  confidence <- leafpacs_confidence(data)
  expect_equal(confidence$bad, 83.8)
  expect_equal(confidence$high, 0.0)

  data <- get_demo_data()
  predictions <- leafpacs_predict(data)
  data <- bind_rows(data, predictions)
  data <- leafpacs_classify(data)
  data <- leafpacs_confidence(data)
})
