## Script to test ESC_SCORE2_table

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


## Script to test ESC_SCORE2_formula

test_that("Paper example is correct", {
  expect_identical(
    ESC_Score2_formula(
      sex = c("male","female"),
      age = c(50,50),
      totchol = c(6.3,6.3),
      hdl = c(1.4, 1.4),
      sbp = c(140,140),
      smoker = c(1,1),
      diabetic = c(0,0),
      risk = "low",
      mmol = TRUE),
    c(6.31,4.33))})
