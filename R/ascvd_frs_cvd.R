#' Framingham risk score (CVD)
#'
#' Computes 10-year risk for specific atherosclerotic cardiovascular disease (CVD) events, ie, coronary heart disease, cerebrovascular disease, peripherial vascular disease
#' and heart failure.
#'
#' @param sex gender; categorical \[female|male\]
#' @param age age; integer \[years\]
#' @param totchol total cholesterol; numeric \[mg/dl\]
#' @param hdl high-density lipoprotein; numeric \[mg/dl\]
#' @param sbp systolic blood pressure; numeric \[mmHg\]
#' @param bp_med information if individual is on a blood pressure medication; numeric \[1|0\]; ("1"=yes;"0"=no)
#' @param smoker information on current self-reported smoking status; numeric \[1|0\]; ("1"=smoker;"0"=non-smoker)
#' @param diabetic diabetic status of individual; numeric \[1|0\]; ("1"=diabetic;"0"=non-diabetic)
#' @param heart_age logical; only available in tabl version. if TRUE a dataframe (n > 1) with 2 columns or a vector (n = 1) with 2 elements is returned, containing the Values for Risk Score and Heart Age
#' @return Estimated 10 year risk for CVD (percent)
#' @aliases ascvd_frs_cvd_formula ascvd_frs_cvd_table
#' @usage
#' ascvd_frs_cvd_formula(sex, age, totchol, hdl, sbp, bp_med, smoker, diabetic)
#' ascvd_frs_cvd_table(sex, age, totchol, hdl, sbp, bp_med,
#' smoker, diabetic, heart_age = FALSE)
#' @details Background: \cr
#' Separate multivariable risk algorithms are commonly used to assess risk of specific atherosclerotic cardiovascular disease (CVD) events,
#' ie, coronary heart disease, cerebrovascular disease, peripheral vascular disease, and heart failure.
#' The present report presents a single multivariable risk function that predicts risk of developing all CVD and of its constituents.\cr
#' Methods and Results:\cr
#' We used Cox proportional-hazards regression to evaluate the risk of developing a first CVD event in 8491 Framingham study participants (mean age, 49 years; 4522 women)
#' who attended a routine examination between 30 and 74 years of age and were free of CVD. Sex-specific multivariable risk functions (“general CVD” algorithms) were derived that incorporated
#' age, total and high-density lipoprotein cholesterol, systolic blood pressure, treatment for hypertension, smoking, and diabetes status.
#' We assessed the performance of the general CVD algorithms for predicting individual CVD events (coronary heart disease, stroke, peripheral artery disease, or heart failure).
#' Over 12 years of follow-up, 1174 participants (456 women) developed a first CVD event. All traditional risk factors evaluated predicted CVD risk (multivariable-adjusted P⬍0.0001).
#' The general CVD algorithm demonstrated good discrimination (C statistic, 0.763 (men) and 0.793 (women) and calibration. Simple adjustments to the general CVD risk algorithms
#' allowed estimation of the risks of each CVD component. Two simple risk scores are presented, 1 based on all traditional risk factors and the other based on non–laboratory-based predictors.\cr
#' Conclusions:\cr
#' A sex-specific multivariable risk factor algorithm can be conveniently used to assess general CVD risk and risk of individual
#' CVD events (coronary, cerebrovascular, and peripheral arterial disease and heart failure). The estimated absolute CVD event rates can be used to quantify risk and to guide preventive care.
#' @references
#' D'Agostino RB Sr, Vasan RS, Pencina MJ, Wolf PA, Cobain M, Massaro JM, Kannel WB.
#' General cardiovascular risk profile for use in primary care: the Framingham Heart Study.
#' Circulation. 2008 Feb 12;117(6):743-53. doi: 10.1161/CIRCULATIONAHA.107.699579. Epub 2008 Jan 22. PMID: 18212285.
#' @export
ascvd_frs_cvd_formula <- function(sex, age, totchol, hdl, sbp, bp_med, smoker, diabetic){

  if (!all(sex %in% c("male", "female")) | missing(sex)) {
    stop("sex must be either 'male' or 'female'")
  }

  if (!is.numeric(age) |  missing(age)) {
    stop("age must be a valid numeric value")
  }

  if (!is.numeric(totchol) | missing(totchol)) {
    stop("totchol must be a valid numeric value")
  }

  if (!is.numeric(hdl) | missing(hdl)) {
    stop("hdl must be a valid numeric value")
  }

  if (!is.numeric(sbp) | missing(sbp)) {
    stop("sbp must be a valid numeric value")
  }

  if (!is.numeric(bp_med) | !all(bp_med %in% c(0,1,NA)) | missing(bp_med)) {
    stop("bp_med must be either 0 (no) or 1 (yes)")
  }

  if (!is.numeric(smoker) | !all(smoker %in% c(0,1,NA)) | missing(smoker)) {
    stop("smoker must be either 0 (no) or 1 (yes)")
  }

  if (!is.numeric(diabetic) | !all(diabetic %in% c(0,1,NA)) | missing(diabetic)) {
    stop("diabetic must be either 0 (no) or 1 (yes)")
  }

  if (any(age < 30) | any(age > 74) | any(is.na(age))) {
    warning("Some values are outside the optimal age range (30-74 years). Risk calculation can thus become less accurate.")
  }

  if (any(is.na(totchol))) {
    warning("totchol contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(hdl))) {
    warning("hdl contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(sbp))) {
    warning("sbp contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(diabetic))) {
    warning("diabetic contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(smoker))) {
    warning("smoker contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(bp_med))) {
    warning("bp_med contains NA's. This can greatly underestimate the risk for individuals")
  }


  data <- data.frame(id = 1:length(sex), sex = sex, age = age, totchol = totchol,
                     hdl = hdl, sbp = sbp, bp_med = bp_med,
                     smoker = smoker, diabetic = diabetic)


  # Generate data.frame of coefficients based on input `race` and `sex`
  # vectors. We lose the original order after the merge operation, so will
  # need to re-sort the output based on the original order of `race_sex`.

  data <- merge(data, ascvd_frs_cvd_coefficients, by = "sex", all.x = TRUE)

  if(nrow(data) > 1) {
  data <- data[order(data$id),]
  }

  data$sbp_treated <- ifelse(data$bp_med == 1, data$sbp, 1)
  data$sbp_untreated <- ifelse(data$bp_med == 0, data$sbp, 1)

  ## Set all NAs to 0 so that a calculation is possible.
  ## Attention, this leads to a strong underestimation of the risk.

  data[is.na(data)] <- 0

  indv_sum <- log(data$age) * data$ln_age_coef +
    log(data$totchol) * data$ln_totchol_coef +
    log(data$hdl) * data$ln_hdl_coef +
    log(data$sbp_treated) * data$ln_treated_sbp_coef +
    log(data$sbp_untreated) * data$ln_untreated_sbp_coef +
    data$smoker * data$smoker_coef +
    data$diabetic * data$diabetic_coef

  risk_score <- round((1 - (data$baseline_survival^
                              exp(indv_sum - data$group_mean))) * 100.000, 2)

  ifelse(risk_score < 1, 1, ifelse(risk_score > 30, 30, risk_score))

  return(risk_score)

}
#' @export
ascvd_frs_cvd_table <- function(sex, age, totchol, hdl, sbp,bp_med, smoker, diabetic, heart_age = FALSE){

  if (!all(sex %in% c("male", "female")) | missing(sex)) {
    stop("sex must be either 'male' or 'female'")
  }

  if (!is.numeric(age) |  missing(age)) {
    stop("age must be a valid numeric value")
  }

  if (!is.numeric(totchol) | missing(totchol)) {
    stop("totchol must be a valid numeric value")
  }

  if (!is.numeric(hdl) | missing(hdl)) {
    stop("hdl must be a valid numeric value")
  }

  if (!is.numeric(sbp) | missing(sbp)) {
    stop("sbp must be a valid numeric value")
  }

  if (!is.numeric(bp_med) | !all(bp_med %in% c(0,1,NA)) | missing(bp_med)) {
    stop("bp_med must be either 0 (no) or 1 (yes)")
  }

  if (!is.numeric(smoker) | !all(smoker %in% c(0,1,NA)) | missing(smoker)) {
    stop("smoker must be either 0 (no) or 1 (yes)")
  }

  if (!is.numeric(diabetic) | !all(diabetic %in% c(0,1,NA)) | missing(diabetic)) {
    stop("diabetic must be either 0 (no) or 1 (yes)")
  }

  if (!is.logical(heart_age)) {
    stop("heart_age must be logical")
  }

  if (any(age < 30) | any(age > 74) | any(is.na(age))) {
    warning("Some values are outside the optimal age range (30-74 years). Risk calculation can thus become less accurate.")
  }

  if (any(is.na(totchol))) {
    warning("totchol contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(hdl))) {
    warning("hdl contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(sbp))) {
    warning("sbp contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(diabetic))) {
    warning("diabetic contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(smoker))) {
    warning("smoker contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(bp_med))) {
    warning("bp_med contains NA's. This can greatly underestimate the risk for individuals")
  }


  data <- data.frame(id = 1:length(sex), sex = sex, age = age, totchol = totchol,
                     hdl = hdl, sbp = sbp, bp_med = bp_med,
                     smoker = smoker, diabetic = diabetic)

  #utils::data(sysdata, envir = environment())

  data$score <- 0

  ## Score Age
  data$score[data$age >= 30 & data$age < 35 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 30 & data$age < 35 & !is.na(data$age) & data$sex == "female"] + 0
  data$score[data$age >= 30 & data$age < 35 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 30 & data$age < 35 & !is.na(data$age) & data$sex == "male"] + 0
  data$score[data$age >= 35 & data$age < 40 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 35 & data$age < 40 & !is.na(data$age) & data$sex == "female"] + 2
  data$score[data$age >= 35 & data$age < 40 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 35 & data$age < 40 & !is.na(data$age) & data$sex == "male"] + 2
  data$score[data$age >= 40 & data$age < 45 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 40 & data$age < 45 & !is.na(data$age) & data$sex == "female"] + 4
  data$score[data$age >= 40 & data$age < 45 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 40 & data$age < 45 & !is.na(data$age) & data$sex == "male"] + 5
  data$score[data$age >= 45 & data$age < 50 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 45 & data$age < 50 & !is.na(data$age) & data$sex == "female"] + 5
  data$score[data$age >= 45 & data$age < 50 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 45 & data$age < 50 & !is.na(data$age) & data$sex == "male"] + 6
  data$score[data$age >= 50 & data$age < 55 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 50 & data$age < 55 & !is.na(data$age) & data$sex == "female"] + 7
  data$score[data$age >= 50 & data$age < 55 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 50 & data$age < 55 & !is.na(data$age) & data$sex == "male"] + 8
  data$score[data$age >= 55 & data$age < 60 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 55 & data$age < 60 & !is.na(data$age) & data$sex == "female"] + 8
  data$score[data$age >= 55 & data$age < 60 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 55 & data$age < 60 & !is.na(data$age) & data$sex == "male"] + 10
  data$score[data$age >= 60 & data$age < 65 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 60 & data$age < 65 & !is.na(data$age) & data$sex == "female"] + 9
  data$score[data$age >= 60 & data$age < 65 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 60 & data$age < 65 & !is.na(data$age) & data$sex == "male"] + 11
  data$score[data$age >= 65 & data$age < 70 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 65 & data$age < 70 & !is.na(data$age) & data$sex == "female"] + 10
  data$score[data$age >= 65 & data$age < 70 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 65 & data$age < 70 & !is.na(data$age) & data$sex == "male"] + 12
  data$score[data$age >= 70 & data$age < 75 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 70 & data$age < 75 & !is.na(data$age) & data$sex == "female"] + 11
  data$score[data$age >= 70 & data$age < 75 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 70 & data$age < 75 & !is.na(data$age) & data$sex == "male"] + 14
  data$score[data$age >= 75 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 75 & !is.na(data$age) & data$sex == "female"] + 12
  data$score[data$age >= 75 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 75 & !is.na(data$age) & data$sex == "male"] + 15

  ## Score HDL
  data$score[data$hdl < 35 & !is.na(data$hdl) & data$sex == "female"] <- data$score[data$hdl < 35 & !is.na(data$hdl) & data$sex == "female"] + 2
  data$score[data$hdl < 35 & !is.na(data$hdl) & data$sex == "male"] <- data$score[data$hdl < 35 & !is.na(data$hdl) & data$sex == "male"] + 2
  data$score[data$hdl >= 35 & data$hdl < 45 & !is.na(data$hdl) & data$sex == "female"] <- data$score[data$hdl >= 35 & data$hdl < 45 & !is.na(data$hdl) & data$sex == "female"] + 1
  data$score[data$hdl >= 35 & data$hdl < 45 & !is.na(data$hdl) & data$sex == "male"] <- data$score[data$hdl >= 35 & data$hdl < 45 & !is.na(data$hdl) & data$sex == "male"] + 1
  data$score[data$hdl >= 45 & data$hdl < 50 & !is.na(data$hdl) & data$sex == "female"] <- data$score[data$hdl >= 45 & data$hdl < 50 & !is.na(data$hdl) & data$sex == "female"] + 0
  data$score[data$hdl >= 45 & data$hdl < 50 & !is.na(data$hdl) & data$sex == "male"] <- data$score[data$hdl >= 45 & data$hdl < 50 & !is.na(data$hdl) & data$sex == "male"] + 0
  data$score[data$hdl >= 50 & data$hdl < 60 & !is.na(data$hdl) & data$sex == "female"] <- data$score[data$hdl >= 50 & data$hdl < 60 & !is.na(data$hdl) & data$sex == "female"] - 1
  data$score[data$hdl >= 50 & data$hdl < 60 & !is.na(data$hdl) & data$sex == "male"] <- data$score[data$hdl >= 50 & data$hdl < 60 & !is.na(data$hdl) & data$sex == "male"] - 1
  data$score[data$hdl >= 60 & !is.na(data$hdl) & data$sex == "female"] <- data$score[data$hdl >= 60 & !is.na(data$hdl) & data$sex == "female"] - 2
  data$score[data$hdl >= 60 & !is.na(data$hdl) & data$sex == "male"] <- data$score[data$hdl >= 60 & !is.na(data$hdl) & data$sex == "male"] - 2

  ## Score totchol
  data$score[data$totchol < 160 & !is.na(data$totchol) & data$sex == "female"] <- data$score[data$totchol < 160 & !is.na(data$totchol) & data$sex == "female"] + 0
  data$score[data$totchol < 160 & !is.na(data$totchol) & data$sex == "male"] <- data$score[data$totchol < 160 & !is.na(data$totchol) & data$sex == "male"] + 0
  data$score[data$totchol >= 160 & data$totchol < 200 & !is.na(data$totchol) & data$sex == "female"] <- data$score[data$totchol >= 160 & data$totchol < 200 & !is.na(data$totchol) & data$sex == "female"] + 1
  data$score[data$totchol >= 160 & data$totchol < 200 & !is.na(data$totchol) & data$sex == "male"] <- data$score[data$totchol >= 160 & data$totchol < 200 & !is.na(data$totchol) & data$sex == "male"] + 1
  data$score[data$totchol >= 200 & data$totchol < 240 & !is.na(data$totchol) & data$sex == "female"] <- data$score[data$totchol >= 200 & data$totchol < 240 & !is.na(data$totchol) & data$sex == "female"] + 3
  data$score[data$totchol >= 200 & data$totchol < 240 & !is.na(data$totchol) & data$sex == "male"] <- data$score[data$totchol >= 200 & data$totchol < 240 & !is.na(data$totchol) & data$sex == "male"] + 2
  data$score[data$totchol >= 240 & data$totchol < 280 & !is.na(data$totchol) & data$sex == "female"] <- data$score[data$totchol >= 240 & data$totchol < 280 & !is.na(data$totchol) & data$sex == "female"] + 4
  data$score[data$totchol >= 240 & data$totchol < 280 & !is.na(data$totchol) & data$sex == "male"] <- data$score[data$totchol >= 240 & data$totchol < 280 & !is.na(data$totchol) & data$sex == "male"] + 3
  data$score[data$totchol >= 280 & !is.na(data$totchol) & data$sex == "female"] <- data$score[data$totchol >= 280 & !is.na(data$totchol) & data$sex == "female"] + 5
  data$score[data$totchol >= 280 & !is.na(data$totchol) & data$sex == "male"] <- data$score[data$totchol >= 280 & !is.na(data$totchol) & data$sex == "male"] + 4


  ## Score SBP not treated / treated
  data$score[data$sbp < 120 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "female"] <- data$score[data$sbp < 120 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "female"] - 1
  data$score[data$sbp < 120 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "female"] <- data$score[data$sbp < 120 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "female"] - 3
  data$score[data$sbp < 120 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "male"] <- data$score[data$sbp < 120 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "male"] - 0
  data$score[data$sbp < 120 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "male"] <- data$score[data$sbp < 120 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "male"] - 2
  data$score[data$sbp >= 120 & data$sbp < 130 & !is.na(data$sbp)& data$bp_med == 1 & data$sex == "female"] <- data$score[data$sbp >= 120 & data$sbp < 130 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "female"] + 2
  data$score[data$sbp >= 120 & data$sbp < 130 & !is.na(data$sbp)& data$bp_med == 0 & data$sex == "female"] <- data$score[data$sbp >= 120 & data$sbp < 130 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "female"] + 0
  data$score[data$sbp >= 120 & data$sbp < 130 & !is.na(data$sbp)& data$bp_med == 1 & data$sex == "male"] <- data$score[data$sbp >= 120 & data$sbp < 130 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "male"] + 2
  data$score[data$sbp >= 120 & data$sbp < 130 & !is.na(data$sbp)& data$bp_med == 0 & data$sex == "male"] <- data$score[data$sbp >= 120 & data$sbp < 130 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "male"] + 0
  data$score[data$sbp >= 130 & data$sbp < 140 & !is.na(data$sbp)& data$bp_med == 1 & data$sex == "female"] <- data$score[data$sbp >= 130 & data$sbp < 140 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "female"] + 3
  data$score[data$sbp >= 130 & data$sbp < 140 & !is.na(data$sbp)& data$bp_med == 0 & data$sex == "female"] <- data$score[data$sbp >= 130 & data$sbp < 140 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "female"] + 1
  data$score[data$sbp >= 130 & data$sbp < 140 & !is.na(data$sbp)& data$bp_med == 1 & data$sex == "male"] <- data$score[data$sbp >= 130 & data$sbp < 140 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "male"] + 3
  data$score[data$sbp >= 130 & data$sbp < 140 & !is.na(data$sbp)& data$bp_med == 0 & data$sex == "male"] <- data$score[data$sbp >= 130 & data$sbp < 140 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "male"] + 1
  data$score[data$sbp >= 140 & data$sbp < 150 & !is.na(data$sbp)& data$bp_med == 1 & data$sex == "female"] <- data$score[data$sbp >= 140 & data$sbp < 150 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "female"] + 5
  data$score[data$sbp >= 140 & data$sbp < 150 & !is.na(data$sbp)& data$bp_med == 0 & data$sex == "female"] <- data$score[data$sbp >= 140 & data$sbp < 150 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "female"] + 2
  data$score[data$sbp >= 150 & data$sbp < 160 & !is.na(data$sbp)& data$bp_med == 1 & data$sex == "female"] <- data$score[data$sbp >= 150 & data$sbp < 160 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "female"] + 6
  data$score[data$sbp >= 150 & data$sbp < 160 & !is.na(data$sbp)& data$bp_med == 0 & data$sex == "female"] <- data$score[data$sbp >= 150 & data$sbp < 160 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "female"] + 4
  data$score[data$sbp >= 140 & data$sbp < 160 & !is.na(data$sbp)& data$bp_med == 1 & data$sex == "male"] <- data$score[data$sbp >= 140 & data$sbp < 160 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "male"] + 4
  data$score[data$sbp >= 140 & data$sbp < 160 & !is.na(data$sbp)& data$bp_med == 0 & data$sex == "male"] <- data$score[data$sbp >= 140 & data$sbp < 160 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "male"] + 2
  data$score[data$sbp >= 160 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "female"] <- data$score[data$sbp >= 160 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "female"] + 7
  data$score[data$sbp >= 160 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "female"] <- data$score[data$sbp >= 160 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "female"] + 5
  data$score[data$sbp >= 160 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "male"] <- data$score[data$sbp >= 160 & !is.na(data$sbp) & data$bp_med == 1 & data$sex == "male"] + 5
  data$score[data$sbp >= 160 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "male"] <- data$score[data$sbp >= 160 & !is.na(data$sbp) & data$bp_med == 0 & data$sex == "male"] + 3



  ## Score smoker
  data$score[data$smoker == 1  & !is.na(data$smoker) & data$sex == "female"] <- data$score[data$smoker == 1  & !is.na(data$smoker) & data$sex == "female"] + 3
  data$score[data$smoker == 1  & !is.na(data$smoker) & data$sex == "male"] <- data$score[data$smoker == 1  & !is.na(data$smoker) & data$sex == "male"] + 4

  ## Score diabetic
  data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$sex == "female"] <- data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$sex == "female"] + 4
  data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$sex == "male"] <- data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$sex == "male"] + 3


  ## Calculate Risk

  ## split by sex
  data_f <- data[data$sex == "female",]
  data_m <- data[data$sex == "male",]

  if(nrow(data_f) == 0) {data_f[1,] <- NA}
  if(nrow(data_m) == 0) {data_m[1,] <- NA}

  ## adjust score values for rsiktable

  data_f$score <- ifelse(data_f$score < -2, -2, data_f$score)
  data_f$score <- ifelse(data_f$score > 21, 21, data_f$score)

  data_f$score_hage <- ifelse(data_f$score < 1, 0, data_f$score)
  data_f$score_hage <- ifelse(data_f$score >= 15, 15, data_f$score)

  data_m$score <- ifelse(data_m$score < -3, -3, data_m$score)
  data_m$score <- ifelse(data_m$score > 18, 18, data_m$score)

  data_m$score_hage <- ifelse(data_m$score < 0, -1, data_m$score)
  data_m$score_hage <- ifelse(data_m$score >= 17, 17, data_m$score)

  data_f$risk <- NA
  data_m$risk <- NA

  data_f$risk_hage <- NA
  data_m$risk_hage <- NA

  data_f$risk <- table_ascvd_cvd[["risktable_f"]]$risk[match(data_f$score, table_ascvd_cvd[["risktable_f"]]$points)]
  data_m$risk <- table_ascvd_cvd[["risktable_m"]]$risk[match(data_m$score, table_ascvd_cvd[["risktable_m"]]$points)]

  data_f$risk_hage <- table_ascvd_cvd[["heart_age_f"]]$risk[match(data_f$score_hage, table_ascvd_cvd[["heart_age_f"]]$points)]
  data_m$risk_hage <- table_ascvd_cvd[["heart_age_m"]]$risk[match(data_m$score_hage, table_ascvd_cvd[["heart_age_m"]]$points)]

  ## combine male and female datasets and order by id

  data <- rbind(data_f, data_m)

  data <- data[!is.na(data$id),]

  data <- data[order(data$id),]

  if(heart_age == FALSE) {

    return(data$risk)

  } else {

    return(data[,c("risk", "risk_hage")])

  }




  }

