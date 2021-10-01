## Script to test frs-chd

test_that("Paper example is correct", {
  expect_identical(
    ascvd_frs_chd_table(
      sex = c("male"),
      age = c(55),
      totchol = c(250),
      hdl = c(39),
      ldl = NA,
      sbp = c(146),
      dbp = c(88),
      smoker = c(1),
      diabetic = c(0),
      chol_cat = "tc"),
    c(31))})

test_that("Paper example is correct", {
  expect_identical(
    ascvd_frs_chd_formula(
      sex = c("male"),
      age = c(55),
      totchol = c(250),
      hdl = c(39),
      ldl = NA,
      sbp = c(146),
      dbp = c(88),
      smoker = c(1),
      diabetic = c(0),
      chol_cat = "tc"),
    c(33.36))})
