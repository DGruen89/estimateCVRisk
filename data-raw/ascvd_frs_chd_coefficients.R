###

ascvd_frs_chd_coefficients <- read.csv("ascvd_frs_chd_coefficients.csv")

#usethis::use_data(ascvd_frs_cvd_coefficients, overwrite = TRUE ,compress = "xz")



ascvd_frs_chd_coefficients_list <- list()


ascvd_frs_chd_coefficients_list[["women_TC_totchol"]] <- cbind(t(ascvd_frs_chd_coefficients[3,10:14]), class = 1:5)
ascvd_frs_chd_coefficients_list[["women_TC_HDL"]] <- cbind(t(ascvd_frs_chd_coefficients[3,15:19]), class = 1:5)
ascvd_frs_chd_coefficients_list[["women_TC_BP"]] <- cbind(t(ascvd_frs_chd_coefficients[3,20:24]), class = 1:5)
ascvd_frs_chd_coefficients_list[["men_TC_totchol"]] <- cbind(t(ascvd_frs_chd_coefficients[4,10:14]), class = 1:5)
ascvd_frs_chd_coefficients_list[["men_TC_HDL"]] <- cbind(t(ascvd_frs_chd_coefficients[4,15:19]), class = 1:5)
ascvd_frs_chd_coefficients_list[["men_TC_BP"]] <- cbind(t(ascvd_frs_chd_coefficients[4,20:24]), class = 1:5)
ascvd_frs_chd_coefficients_list[["women_LC_LDL"]] <- cbind(t(ascvd_frs_chd_coefficients[1,5:9]), class = 1:5)
ascvd_frs_chd_coefficients_list[["women_LC_HDL"]] <- cbind(t(ascvd_frs_chd_coefficients[1,15:19]), class = 1:5)
ascvd_frs_chd_coefficients_list[["women_LC_BP"]] <- cbind(t(ascvd_frs_chd_coefficients[1,20:24]), class = 1:5)
ascvd_frs_chd_coefficients_list[["men_LC_LDL"]] <- cbind(t(ascvd_frs_chd_coefficients[2,5:9]), class = 1:5)
ascvd_frs_chd_coefficients_list[["men_LC_HDL"]] <- cbind(t(ascvd_frs_chd_coefficients[2,15:19]), class = 1:5)
ascvd_frs_chd_coefficients_list[["men_LC_BP"]] <- cbind(t(ascvd_frs_chd_coefficients[2,20:24]), class = 1:5)

## Create Risk Table

table_ascvd_chd <- list()

### CHD 10 Y Risk LDL
table_ascvd_chd[["risktable_f_ldl"]] <- data.frame(points = -2:17, risk = c(1,2,2,2,3,3,4,5,6,7,8,9,11,13,15,17,20,24,27,32))
table_ascvd_chd[["risktable_m_ldl"]] <- data.frame(points = -3:14, risk = c(1,2,2,3,4,4,6,7,9,11,14,18,22,27,33,40,47,56))

table_ascvd_chd[["risktable_f_tc"]] <- data.frame(points = -2:17, risk = c(1,2,2,2,3,3,4,4,5,6,7,8,10,11,13,15,18,20,24,27))
table_ascvd_chd[["risktable_m_tc"]] <- data.frame(points = -1:14, risk = c(2,3,3,4,5,7,8,10,13,16,20,25,31,37,45,53))


## Average Risk

table_ascvd_chd[["average_risk_f"]] <- data.frame(age_class = 1:9, average_10y_CHD_Risk = c(1,1,2,5,8,12,12,13,14),
                                                  average_10y_hard_CHD_Risk = c(0,0,1,2,3,7,8,8,11),
                                                  low_10y_CHD_Risk = c(0,1,2,3,5,7,8,8,8))
table_ascvd_chd[["average_risk_m"]] <- data.frame(age_class = 1:9, average_10y_CHD_Risk = c(3,5,7,11,14,16,21,25,30),
                                                  average_10y_hard_CHD_Risk = c(1,4,4,8,10,13,20,22,25),
                                                  low_10y_CHD_Risk = c(2,3,4,4,6,7,9,11,14))




#usethis::use_data(table_ascvd_chd, overwrite = TRUE, compress = "xz")
