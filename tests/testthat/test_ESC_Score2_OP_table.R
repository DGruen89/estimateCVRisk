## Script to ESC_SCORE_2016_table

test_that("Paper example is correct", {
  expect_identical(
    ESC_Score2_OP_table(
      sex = c("male","female"),
      age = c(73,84),
      totchol = c(7,6),
      sbp = c(165,180),
      smoker = c(1,0),
      risk = "moderate",
      mmol = TRUE),
    c(36,31))})
