## Script to test acc_aha

test_that("Paper example is correct", {
  expect_identical(
    ascvd_acc_aha(
      ethnicity = c("white","aa","white","aa"),
      sex = c("female", "female", "male", "male"),
      age = c(55,55,55,55),
      totchol = c(213,213,213,213),
      hdl = c(50,50,50,50),
      sbp = c(120,120,120,120),
      smoker = c(0,0,0,0),
      diabetic = c(0,0,0,0),
      bp_med = c(0,0,0,0)),
    c(2.05,3.03,5.38,6.07))})


