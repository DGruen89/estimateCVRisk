#' ACC/AHA 2013 ASCVD risk score for people aged 40-79 years
#'
#' Computes 10-year risk for hard atherosclerotic cardiovascular disease (ASCVD) event (defined as first occurrence of
#' non-fatal myocardial infarction (MI), congestive heart disease (CHD) death,
#' or fatal or nonfatal stroke).
#'
#' @param ethnicity ethnicity; categorical \[white|aa\]; ("aa"=afro american)
#' @param sex gender; categorical \[female|male\]
#' @param age age; integer \[years\]
#' @param totchol total cholesterol; numeric \[mg/dl\]
#' @param hdl high-density lipoprotein; numeric \[mg/dl\]
#' @param sbp systolic blood pressure; numeric \[mmHg\]
#' @param bp_med information if individual is on a blood pressure medication; numeric \[1|0\]; ("1"=yes;"0"=no)
#' @param smoker information on current self-reported smoking status; numeric \[1|0\]; ("1"=smoker;"0"=non-smoker)
#' @param diabetic diabetic status of individual; numeric \[1|0\]; ("1"=diabetic;"0"=non-diabetic)
#' @return Estimated 10 year risk for hard ASCVD (percent)
#' @usage ascvd_acc_aha(ethnicity = c("white", "aa"), sex, age,
#' totchol, hdl, sbp, bp_med, smoker, diabetic)
#' @details
#' The ASCVD (atherosclerotic cardiovascular disease) risk score is a national guideline developed by the American College of Cardiology.
#' It is a calculation of your 10-year risk of having a cardiovascular problem, such as a heart attack or stroke.
#' This risk estimate considers age, sex, ethnicity, cholesterol levels, blood pressure, medication use, diabetic status, and smoking status.
#' @references
#' Goff, David C., et al. "2013 ACC/AHA guideline on the assessment of
#' cardiovascular risk: a report of the American College of
#' Cardiology/American Heart Association Task Force on Practice
#' Guidelines." Journal of the American College of Cardiology 63.25
#' Part B (2014): 2935-2959.
#' @export
ascvd_acc_aha <- function(ethnicity = c("white", "aa"), sex,age, totchol, hdl, sbp,bp_med, smoker, diabetic) {
  if(missing(ethnicity)){
    ethnicity <- match.arg(ethnicity)
  }

  if (!all(ethnicity %in% c("white", "aa"))) {
    stop("ethnicity must be either 'white' or 'aa'")
  }

  if (!all(sex %in% c("male", "female")) | missing(sex)) {
    stop("sex must be either 'male' or 'female'")
  }

  if (any(!is.numeric(age)) | missing(age)) {
    stop("age must be a valid numeric value")
  }

  if (any(!is.numeric(totchol)) & any(!is.na(totchol))) {
    stop("totchol must be a valid numeric value")
  }

  if (!is.numeric(hdl) | missing(hdl)) {
    stop("hdl must be a valid numeric value")
  }

  if (!is.numeric(sbp) | missing(sbp)) {
    stop("sbp must be a valid numeric value")
  }

  if (!is.numeric(smoker) | !all(smoker %in% c(0,1,NA)) | missing(smoker)) {
    stop("smoker must be either 0 (no) or 1 (yes)")
  }

  if (!is.numeric(diabetic) | !all(diabetic %in% c(0,1,NA)) | missing(diabetic)) {
    stop("diabetic must be either 0 (no) or 1 (yes)")
  }

  if (!is.numeric(bp_med) | !all(bp_med %in% c(0,1,NA)) | missing(bp_med)) {
    stop("bp_med must be either 0 (no) or 1 (yes)")
  }

  if (any(age < 40) | any(age > 79) | any(is.na(age))) {
    warning("Some values are outside the optimal age range (40-79 years). Risk calculation can thus become less accurate.")
  }

  if (any(is.na(age))) {
    warning("age contains NA's. Risk score calculation cannot be performed.")
  }

  if (any(is.na(totchol))) {
    warning("totchol contains NA's. Risk score calculation cannot be performed.")
  }

  if (any(is.na(hdl))) {
    warning("hdl contains NA's. Risk score calculation cannot be performed.")
  }

  if (any(is.na(sbp))) {
    warning("sbp contains NA's. Risk score calculation cannot be performed.")
  }

  if (any(is.na(smoker))) {
    warning("smoker contains NA's. Risk score calculation cannot be performed.")
  }

  if (any(is.na(diabetic))) {
    warning("diabetic contains NA's. Risk score calculation cannot be performed.")
  }

  if (any(is.na(bp_med))) {
    warning("bp_med contains NA's. Risk score calculation cannot be performed.")
  }

  data <- data.frame(id = 1:length(ethnicity), ethnicity = ethnicity, sex = sex, age = age, totchol = totchol,
                     hdl = hdl, sbp = sbp, bp_med = bp_med,
                     smoker = smoker, diabetic = diabetic)


  #utils::data(sysdata, envir = environment())

  # Generate data.frame of coefficients based on input `ethnicity` and `sex`
  # vectors. We lose the original order after the merge operation, so will
  # need to re-sort the output based on the original order of `ethnicity_sex`.

  data <- merge(data, ascvd_acc_aha_coefficients, by = c("ethnicity","sex"), all.x = TRUE)
  data <- data[order(data$id),]

  data$sbp_treated <- ifelse(data$bp_med == 1, data$sbp, 1)
  data$sbp_untreated <- ifelse(data$bp_med == 0, data$sbp, 1)

  indv_sum <- colSums(rbind(log(data$age) * data$ln_age_coef,
    log(data$age)^2 * data$ln_age_squared_coef,
    log(data$totchol) * data$ln_totchol_coef,
    log(data$age) * log(data$totchol) * data$ln_age_totchol_coef,
    log(data$hdl) * data$ln_hdl_coef,
    log(data$age) * log(data$hdl) * data$ln_age_hdl_coef,
    log(data$sbp_treated) * data$ln_treated_sbp_coef,
    log(data$sbp_treated) * log(data$age) * data$ln_age_treated_sbp_coef,
    log(data$sbp_untreated) * data$ln_untreated_sbp_coef,
    log(data$sbp_untreated) * log(data$age) * data$ln_age_untreated_sbp_coef,
    data$smoker * data$smoker_coef,
    data$smoker * log(data$age) * data$ln_age_smoker_coef,
    data$diabetic * data$diabetic_coef), na.rm = F)

  risk_score <- round((1 - (data$baseline_survival^
                              exp(indv_sum - data$group_mean))) * 100.000, 2)

  ifelse(risk_score < 1, 1, ifelse(risk_score > 30, 30, risk_score))

  return(risk_score)

  }
