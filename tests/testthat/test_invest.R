## Test Invest-Score

test_that("Paper example is correct", {
  expect_identical(
    invest_score(age = c(65,69,77),
                 ethnicity = c("white","white","nw"),
                 bmi = c(33,25,18),
                 hr = c(79,85,100),
                 sbp = c(120,105,144),
                 mi = c(1,0,1),
                 chf = c(0,0,1),
                 stroke = c(0,1,0),
                 smoker = c(1,0,0),
                 diabetic = c(0,1,1),
                 pad = c(0,0,0),
                 ckd = c(1,0,1)),
    c(16,36,36))})
