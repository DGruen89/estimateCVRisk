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
