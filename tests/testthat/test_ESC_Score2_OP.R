## Script to ESC_SCORE2_OP_table

test_that("Paper example is correct", {
  expect_identical(
    ESC_Score2_OP_table(
      sex = c("male","female"),
      age = c(73,84),
      totchol = c(7,6),
      hdl = c(1.2, 2.1),
      sbp = c(165,180),
      smoker = c(1,0),
      risk = "moderate",
      mmol = TRUE),
    c(34,27))})


## Script to test ESC_SCORE2_OP_formula

test_that("Paper example is correct", {
  expect_identical(
    ESC_Score2_OP_formula(
      sex = c("male","female"),
      age = c(75,75),
      totchol = c(5.5,5.5),
      hdl = c(1.3, 1.3),
      sbp = c(140,140),
      smoker = c(1,1),
      diabetic = c(0,0),
      risk = "low",
      mmol = TRUE),
    c(18.56,15.16))})
