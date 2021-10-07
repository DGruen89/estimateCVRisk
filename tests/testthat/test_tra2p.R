## Test Tra2°P-Score

test_that("Paper example is correct", {
  expect_identical(
    tra2p_score(
      age = 65,
      chf = 1,
      ah = 1,
      diabetic = 1,
      stroke = 0,
      bypass_surg = 0,
      other_surg = 1,
      egfr = 59,
      smoker = 0),
    c(28.8))})
