###

ascvd_frs_cvd_coefficients <- read.csv("ascvd_frs_cvd_coefficients.csv")

usethis::use_data(ascvd_frs_cvd_coefficients, overwrite = TRUE ,compress = "xz")


## Create Risk Table

table_ascvd_cvd <- list()

table_ascvd_cvd[["risktable_f"]] <- data.frame(points = -2:21, risk = c(0,1,1.2,1.5,1.7,2.0,2.4,2.8,3.3,3.9,4.5,5.3,6.3,7.3,8.6,10.0,11.7,13.7,15.9,18.5,21.5,24.8,28.5,30))
table_ascvd_cvd[["risktable_m"]] <- data.frame(points = -3:18, risk = c(0,1.1,1.4,1.6,1.9,2.3,2.8,3.3,3.9,4.7,5.6,6.7,7.9,9.4,11.2,13.2,15.6,18.4,21.6,25.3,29.4,30))

## Vascular/Heart Age

table_ascvd_cvd[["heart_age_f"]] <- data.frame(points = 0:15, risk = c(30,31,34,36,39,42,45,48,51,55,59,64,68,73,79,80))
table_ascvd_cvd[["heart_age_m"]] <- data.frame(points = -1:17, risk = c(29,30,32,34,36,38,40,42,45,48,51,54,57,60,64,68,72,76,80))

usethis::use_data(table_ascvd_cvd, overwrite = TRUE ,compress = "xz")
