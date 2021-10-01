## Script to test frs-cvd

test_that("Paper example is correct", {
  expect_identical(
    ascvd_frs_cvd_table(
      sex = c("female","male"),
      age = c(61, 53),
      totchol = c(180,161),
      hdl = c(47, 55),
      sbp = c(124,125),
      bp_med = c(0,1),
      smoker = c(1,0),
      diabetic = c(0,1),
      heart_age = FALSE),
    c(10,15.6))})

test_that("Paper example is correct", {
  expect_identical(
    ascvd_frs_cvd_formula(
      sex = c("female","male"),
      age = c(61, 53),
      totchol = c(180,161),
      hdl = c(47, 55),
      sbp = c(124,125),
      bp_med = c(0,1),
      smoker = c(1,0),
      diabetic = c(0,1)),
    c(10.48, 15.62))})

