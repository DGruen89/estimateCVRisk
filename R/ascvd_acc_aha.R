#' ACC/AHA 2013 ASCVD risk score
#'
#' Computes 10-year risk for hard ASCVD event (defined as first occurrence of
#' non-fatal myocardial infarction (MI), congestive heart disease (CHD) death,
#' or fatal or nonfatal stroke).
#'
#' @param race a character vector indicating the patients race. Value: "white", "aa" (afro american)
#' @param sex a character vector indicating the sex of the person. Values: "female", "male"
#' @param age a numeric vector with the age of persons given as years
#' @param totchol a numeric vector; Cholesterol values given in mg/dL
#' @param hdl a numeric vector; HDL Cholesterol values given in mg/dL
#' @param sbp a numeric vector with the systolic blood pressure of persons given as mmHg
#' @param bp_med a numeric vector with the information if a Patient is on a blood pressure medication. Values: yes = 1; no = 0.
#' @param smoker a numeric vector. Smoker = 1, non-smoker = 0. A smoker was defined as current self-reported smoker.
#' @param diabetic a numeric vector indicating whether a person is diabetic. Values: yes = 1; no = 0.
#' @return Estimated 10-Y Risk for hard ASCVD (percent)
#' @details
#' The ASCVD (atherosclerotic cardiovascular disease) risk score is a national guideline developed by the American College of Cardiology.
#' It is a calculation of your 10-year risk of having a cardiovascular problem, such as a heart attack or stroke.
#' This risk estimate considers age, sex, race, cholesterol levels, blood pressure, medication use, diabetic status, and smoking status.
#' @references
#' Goff, David C., et al. "2013 ACC/AHA guideline on the assessment of
#' cardiovascular risk: a report of the American College of
#' Cardiology/American Heart Association Task Force on Practice
#' Guidelines." Journal of the American College of Cardiology 63.25
#' Part B (2014): 2935-2959.
#' @export
ascvd_accaha_10y <- function(race, sex,
                             age, totchol, hdl, sbp,
                             bp_med, smoker, diabetic) {



  data <- data.frame(id = 1:length(race), race = race, sex = sex, age = age, totchol = totchol,
                     hdl = hdl, sbp = sbp, bp_med = bp_med,
                     smoker = smoker, diabetic = diabetic)


  #utils::data(sysdata, envir = environment())

  # Generate data.frame of coefficients based on input `race` and `sex`
  # vectors. We lose the original order after the merge operation, so will
  # need to re-sort the output based on the original order of `race_sex`.

  data <- merge(data, ascvd_acc_aha_coefficients, by = c("race","sex"), all.x = TRUE)
  data <- data[order(data$id),]

  data$sbp_treated <- ifelse(data$bp_med == 1, sbp, 1)
  data$sbp_untreated <- ifelse(data$bp_med == 0, sbp, 1)

  indv_sum <- log(data$age) * data$ln_age_coef +
    log(data$age)^2 * data$ln_age_squared_coef +
    log(data$totchol) * data$ln_totchol_coef +
    log(data$age) * log(totchol) * data$ln_age_totchol_coef +
    log(data$hdl) * data$ln_hdl_coef +
    log(data$age) * log(hdl) * data$ln_age_hdl_coef +
    log(data$sbp_treated) * data$ln_treated_sbp_coef +
    log(data$sbp_treated) * log(age) * data$ln_age_treated_sbp_coef +
    log(data$sbp_untreated) * data$ln_untreated_sbp_coef +
    log(data$sbp_untreated) * log(age) * data$ln_age_untreated_sbp_coef +
    data$smoker * data$smoker_coef +
    data$smoker * log(data$age) * data$ln_age_smoker_coef +
    data$diabetic * data$diabetic_coef

  risk_score <- round((1 - (data$baseline_survival^
                              exp(indv_sum - data$group_mean))) * 100.000, 2)

  ifelse(risk_score < 1, 1, ifelse(risk_score > 30, 30, risk_score))

  return(risk_score)

  }
