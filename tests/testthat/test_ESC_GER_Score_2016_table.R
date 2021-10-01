## Script to ESC_SCORE_GER_2016_table

test_that("Paper example is correct", {
  expect_identical(
    ESC_Score_GER_2016_table(
      sex = c("male","female"),
      age = c(60,40),
      totchol = c(270,195),
      sbp = c(162,135),
      smoker = c(0,1),
      mmol = FALSE),
    c(7,0))})
