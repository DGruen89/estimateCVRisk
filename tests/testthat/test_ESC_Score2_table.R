## Script to ESC_SCORE2_table

test_that("Paper example is correct", {
  expect_identical(
    ESC_Score2_table(
      sex = c("male","male"),
      age = c(60,40),
      totchol = c(4.3,6.1),
      hdl = c(0.9, 1.7),
      sbp = c(120,180),
      smoker = c(0,1),
      risk = "moderate",
      mmol = TRUE),
    c(7,9))})
