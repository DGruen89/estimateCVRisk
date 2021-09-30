## Script to test frs-cvd

test_that("Paper example is correct", {
  expect_equal(
    ascvd_frs_cvd_table(
      sex = "female", 
      age = 61, 
      totchol = 180, 
      hdl = 47, 
      sbp = 124, 
      bp_med = 0, 
      smoker = 1, 
      diabetic = 0, 
      heart_age = FALSE), 
    10)})

test_that("Paper example is correct", {
  expect_equal(
    ascvd_frs_cvd_formula(
      sex = "female", 
      age = 61, 
      totchol = 180, 
      hdl = 47, 
      sbp = 124, 
      bp_med = 0, 
      smoker = 1, 
      diabetic = 0), 
    10.48)})

