## Test for reach-score

test_that("Paper example is correct", {
  expect_identical(
    reach_score_next_cv(
      sex = c("male"),
      age = c(62),
      smoker = c(0),
      diabetic = c(1),
      bmi = c(25),
      vasc = c(1),
      cv_event = c(1),
      chf = c(1),
      af = c(0),
      statin = c(1),
      ass = c(1),
      region_EE_or_ME = TRUE),
    c(11))})

test_that("Paper example is correct", {
  expect_identical(
    reach_score_cv_death(
      sex = c("male"),
      age = c(62),
      smoker = c(0),
      diabetic = c(1),
      bmi = c(25),
      vasc = c(1),
      cv_event = c(1),
      chf = c(1),
      af = c(0),
      statin = c(1),
      ass = c(1),
      region_EE_or_ME = TRUE),
    c(6.2))})

test_that("Paper example is correct", {
  expect_identical(
    reach_score_next_cv_formula(
      sex = c("male"),
      age = c(62),
      smoker = c(0),
      diabetic = c(1),
      bmi = c(25),
      vasc = c(1),
      cv_event = c(1),
      chf = c(1),
      af = c(0),
      statin = c(1),
      ass = c(1),
      region_EE_or_ME = TRUE),
    c(13.54))})


test_that("Paper example is correct", {
  expect_identical(
    reach_score_cv_death_formula(
      sex = c("male"),
      age = c(62),
      smoker = c(0),
      diabetic = c(1),
      bmi = c(25),
      vasc = c(1),
      cv_event = c(1),
      chf = c(1),
      af = c(0),
      statin = c(1),
      ass = c("1"),
      region_EE_or_ME = TRUE),
    c(24.49))})

