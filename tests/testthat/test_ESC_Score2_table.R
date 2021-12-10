## Script to ESC_SCORE_2016_table

test_that("Paper example is correct", {
  expect_identical(
    ESC_Score2_table(
      sex = c("male","male"),
      age = c(60,40),
      totchol = c(4.3,6.1),
      sbp = c(120,180),
      smoker = c(0,1),
      risk = "moderate",
      mmol = TRUE),
    c(8,13))})
