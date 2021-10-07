### PRocam_score Testskript

test_that("Paper example is correct", {
  expect_identical(
    procam_score_2002(
      age = c(44,59),
      ldl = c(89,188),
      hdl = c(61,35),
      sbp = c(121,160),
      triglycerides = c(156,98),
      smoker = c(0,1),
      diabetic = c(1,0),
      famMI = c(1,1)),
    c(1.1,30))})



test_that("Paper example is correct", {
  expect_identical(
    procam_score_2007(
      sex = c("female", "male"),
      age = c(44,59),
      ldl = c(89,188),
      hdl = c(61,35),
      sbp = c(121,160),
      triglycerides = c(156,98),
      smoker = c(0,1),
      diabetic = c(1,0),
      famMI = c(1,1)),
    c("0-4%","=30%"))})
