objects <- objects()

usethis::use_data(ascvd_acc_aha_coefficients, ascvd_frs_chd_coefficients, ascvd_frs_chd_coefficients_list,
                  ascvd_frs_cvd_coefficients, reach_cvdeath_coefficients, reach_nextcv_coefficients,
                  risktable_procam2002, risktable_procam2007_men, risktable_procam2007_women,
                  table_ascvd_chd, table_ascvd_cvd, risktable_invest,esc_score_2_coefficients,
                  esc_score_2_OP_coefficients, overwrite = TRUE ,internal = TRUE)
