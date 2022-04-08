#' ACC/AHA 2013 ASCVD risk score
#'
#' Computes 10-year risk for specific atherosclerotic cardiovascular disease (CVD) events, ie, coronary heart disease, cerebrovascular disease, peripherial vascular disease
#' and heart failure.
#'
#' @param sex a numeric vector indicating the sex of the person. Values: "female", "male"
#' @param age a numeric vector with the age of persons given as years
#' @param totchol a numeric vector; Cholesterol values given in mg/dL
#' @param ldl a numeric vector; LDL Cholesterol values given in mg/dL
#' @param hdl a numeric vector; HDL Cholesterol values given in mg/dL
#' @param sbp a numeric vector with the systolic blood pressure of persons given as mmHg
#' @param dbp Diastolic blood pressure (mm Hg)
#' @param smoker a numeric vector. Smoker = 1, non-smoker = 0. A smoker was defined as current self-reported smoker.
#' @param diabetic a numeric vector indicating whether a person is diabetic. Values: yes = 1; no = 0.
#' @param chol_cat a single character value to specify the use of total cholesterol ("tc") or LDL cholesterol ("ldl")
#' @return Estimated 10-Y Risk for CHD (percent)
#' @aliases ascvd_frs_chd_formula ascvd_frs_chd_table
#' @usage
#' ascvd_frs_chd_formula(sex, age, totchol = NA, hdl, ldl = NA, sbp,
#' dbp, smoker, diabetic, chol_cat = c("tc", "ldl"))
#' ascvd_frs_chd_table(sex, age, totchol = NA, hdl, ldl = NA, sbp,
#' dbp, smoker, diabetic, chol_cat = c("tc", "ldl"))
#' @details Background: \cr
#' The objective of this study was to examine the association of Joint National Committee (JNC-V) blood pressure and National Cholesterol Education Program (NCEP) cholesterol categories
#'  with coronary heart disease (CHD) risk, to incorporate them into coronary prediction algorithms, and to compare the discrimination properties of this approach
#'  with other noncategorical prediction functions.\cr
#' Methods and Results:\cr
#' This work was designed as a prospective, single-center study in the setting of a community-based cohort. The patients were 2489 men and 2856 women 30 to 74 years old at baseline
#' with 12 years of follow-up. During the 12 years of follow-up, a total of 383 men and 227 women developed CHD, which was significantly associated with categories of
#' blood pressure, total cholesterol, LDL cholesterol, and HDL cholesterol (all p < 0.001). Sex-specific prediction equations were formulated to predict CHD risk according to
#' age, diabetes, smoking, JNC-V blood pressure categories, and NCEP total cholesterol and LDL cholesterol categories. The accuracy of this categorical approach was found
#' to be comparable to CHD prediction when the continuous variables themselves were used. After adjustment for other factors, 28% of CHD events in men and 29% in women
#' were attributable to blood pressure levels that exceeded high normal (130/85). The corresponding multivariable-adjusted attributable risk percent associated with
#' elevated total cholesterol (200 mg/dL) was 27% in men and 34% in women.\cr
#' Conclusions:\cr
#' Recommended guidelines of blood pressure, total cholesterol, and LDL cholesterol effectively predict CHD risk in a middle-aged white population sample.
#' A simple coronary disease prediction algorithm was developed using categorical variables, which allows physicians to predict multivariate CHD risk in patients without overt CHD.
#' @references
#' Wilson PW, D'Agostino RB, Levy D, Belanger AM, Silbershatz H, Kannel WB.
#' Prediction of coronary heart disease using risk factor categories.
#' Circulation. 1998 May 12;97(18):1837-47. doi: 10.1161/01.cir.97.18.1837. PMID: 9603539.
#' @export
ascvd_frs_chd_formula <- function(sex, age, totchol = NA, hdl, ldl = NA, sbp, dbp, smoker, diabetic, chol_cat = c("tc", "ldl")){

  chol_cat <- match.arg(chol_cat)

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

  if (any(!is.numeric(ldl)) & any(!is.na(ldl))) {
    stop("ldl must be a valid numeric value")
  }

  if (!is.numeric(sbp) | missing(sbp)) {
    stop("sbp must be a valid numeric value")
  }

  if (!is.numeric(dbp) | missing(dbp)) {
    stop("dbp must be a valid numeric value")
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

  if (any(is.na(age))) {
    warning("age contains NA's. Risk calculation can thus become less accurate")
  }

  if (any(is.na(totchol)) & chol_cat == "tc") {
    warning("totchol contains NA's. Risk calculation can thus become less accurate")
  }

  if (any(is.na(hdl))) {
    warning("hdl contains NA's. Risk calculation can thus become less accurate")
  }

  if (any(is.na(ldl)) & chol_cat == "ldl") {
    warning("ldl contains NA's. Risk calculation can thus become less accurate")
  }

  if (any(is.na(sbp))) {
    warning("sbp contains NA's. Risk calculation can thus become less accurate")
  }

  if (any(is.na(dbp))) {
    warning("dbp contains NA's. Risk calculation can thus become less accurate")
  }

  if (any(is.na(smoker))) {
    warning("smoker contains NA's. Risk calculation can thus become less accurate")
  }

  if (any(is.na(diabetic))) {
    warning("diabetic contains NA's. Risk calculation can thus become less accurate")
  }

  data <- data.frame(id = 1:length(sex), sex = sex, age = age, totchol = totchol,
                     hdl = hdl, ldl = ldl, sbp = sbp, dbp = dbp, smoker = smoker, diabetic = diabetic)


  #utils::data(sysdata, envir = environment())

  ## calculate BP Categories

  data$bp_cat <- NA

  # Optimal
  data$bp_cat <- ifelse(data$sbp < 120 & data$dbp < 80, 1, data$bp_cat)
  # normal
  data$bp_cat <- ifelse(((data$sbp >= 120 & data$sbp < 130) | data$dbp >= 80 & data$dbp < 85), 2, data$bp_cat)
  # high normal
  data$bp_cat <- ifelse(((data$sbp >= 130 & data$sbp < 140) | data$dbp >= 85 & data$dbp < 90), 3, data$bp_cat)
  # hypertension stage 1
  data$bp_cat <- ifelse(((data$sbp >= 140 & data$sbp < 160) | data$dbp >= 90 & data$dbp < 99), 4, data$bp_cat)
  # hypertension stage 2-4
  data$bp_cat <- ifelse((data$sbp >= 160 | data$dbp > 100), 5, data$bp_cat)


  ## calculate TC Categories

  if(chol_cat == "tc"){

  data$tc_cat <- NA

  data$tc_cat <- ifelse(data$totchol < 160, 1, data$tc_cat)
  data$tc_cat <- ifelse(data$totchol >= 160 & data$totchol < 200, 2, data$tc_cat)
  data$tc_cat <- ifelse(data$totchol >= 200 & data$totchol < 240, 3, data$tc_cat)
  data$tc_cat <- ifelse(data$totchol >= 240 & data$totchol < 280, 4, data$tc_cat)
  data$tc_cat <- ifelse(data$totchol >= 280, 5, data$tc_cat)
  }

  ## calculate LDL Categories

  if(chol_cat == "ldl"){

  data$ldl_cat <- NA

  data$ldl_cat <- ifelse(data$ldl < 100, 1, data$ldl_cat)
  data$ldl_cat <- ifelse(data$ldl >= 100 & data$ldl < 130, 2, data$ldl_cat)
  data$ldl_cat <- ifelse(data$ldl >= 130 & data$ldl < 160, 3, data$ldl_cat)
  data$ldl_cat <- ifelse(data$ldl >= 160 & data$ldl < 190, 4, data$ldl_cat)
  data$ldl_cat <- ifelse(data$ldl >= 190, 5, data$ldl_cat)
  }

  ## calculate HDL Categories

  data$hdl_cat <- NA

  data$hdl_cat <- ifelse(data$hdl < 35, 1, data$hdl_cat)
  data$hdl_cat <- ifelse(data$hdl >= 35 & data$hdl < 45, 2, data$hdl_cat)
  data$hdl_cat <- ifelse(data$hdl >= 45 & data$hdl < 50, 3, data$hdl_cat)
  data$hdl_cat <- ifelse(data$hdl >= 50 & data$hdl < 60, 4, data$hdl_cat)
  data$hdl_cat <- ifelse(data$hdl >= 60, 5, data$hdl_cat)


  ### Split by sex

  data_men <- data[data$sex == "male",]
  data_women <- data[data$sex == "female",]

  if(nrow(data_men) == 0) {data_men[1,] <- NA}
  if(nrow(data_women) == 0) {data_women[1,] <- NA}

  ## Create Datasets for formular calculation

  if(chol_cat == "tc"){

    data_men$coef_age <- ascvd_frs_chd_coefficients$coef_age[4]
    data_men$coef_age.2 <- 0 ## no age^2 for men
    data_men$coef_totchol <- ascvd_frs_chd_coefficients_list$men_TC_totchol[data_men$tc_cat, 1]
    data_men$coef_hdl <- ascvd_frs_chd_coefficients_list$men_TC_HDL[data_men$hdl_cat, 1]
    data_men$coef_bp <- ascvd_frs_chd_coefficients_list$men_TC_BP[data_men$bp_cat, 1]
    data_men$coef_diabetic <- ascvd_frs_chd_coefficients$coef_diabetic[4]
    data_men$coef_smoker <- ascvd_frs_chd_coefficients$coef_smoker[4]
    data_men$baseline_survival <- ascvd_frs_chd_coefficients$baseline_survival[4]
    data_men$group_mean_coef <- ascvd_frs_chd_coefficients$group_mean_coef[4]

    data_women$coef_age <- ascvd_frs_chd_coefficients$coef_age[3]
    data_women$coef_age.2 <- ascvd_frs_chd_coefficients$coef_age.2[3]
    data_women$coef_totchol <- ascvd_frs_chd_coefficients_list$women_TC_totchol[data_women$tc_cat, 1]
    data_women$coef_hdl <- ascvd_frs_chd_coefficients_list$women_TC_HDL[data_women$hdl_cat, 1]
    data_women$coef_bp <- ascvd_frs_chd_coefficients_list$women_TC_BP[data_women$bp_cat, 1]
    data_women$coef_diabetic <- ascvd_frs_chd_coefficients$coef_diabetic[3]
    data_women$coef_smoker <- ascvd_frs_chd_coefficients$coef_smoker[3]
    data_women$baseline_survival <- ascvd_frs_chd_coefficients$baseline_survival[3]
    data_women$group_mean_coef <- ascvd_frs_chd_coefficients$group_mean_coef[3]

  }


  if(chol_cat == "ldl"){

    data_men$coef_age <- ascvd_frs_chd_coefficients$coef_age[2]
    data_men$coef_age.2 <- 0 ## no age^2 for men
    data_men$coef_ldl <- ascvd_frs_chd_coefficients_list$men_LC_LDL[data_men$ldl_cat, 1]
    data_men$coef_hdl <- ascvd_frs_chd_coefficients_list$men_LC_HDL[data_men$hdl_cat, 1]
    data_men$coef_bp <- ascvd_frs_chd_coefficients_list$men_LC_BP[data_men$bp_cat, 1]
    data_men$coef_diabetic <- ascvd_frs_chd_coefficients$coef_diabetic[2]
    data_men$coef_smoker <- ascvd_frs_chd_coefficients$coef_smoker[2]
    data_men$baseline_survival <- ascvd_frs_chd_coefficients$baseline_survival[2]
    data_men$group_mean_coef <- ascvd_frs_chd_coefficients$group_mean_coef[2]

    data_women$coef_age <- ascvd_frs_chd_coefficients$coef_age[1]
    data_women$coef_age.2 <- ascvd_frs_chd_coefficients$coef_age.2[1]
    data_women$coef_ldl <- ascvd_frs_chd_coefficients_list$women_LC_LDL[data_women$ldl_cat, 1]
    data_women$coef_hdl <- ascvd_frs_chd_coefficients_list$women_LC_HDL[data_women$hdl_cat, 1]
    data_women$coef_bp <- ascvd_frs_chd_coefficients_list$women_LC_BP[data_women$bp_cat, 1]
    data_women$coef_diabetic <- ascvd_frs_chd_coefficients$coef_diabetic[1]
    data_women$coef_smoker <- ascvd_frs_chd_coefficients$coef_smoker[1]
    data_women$baseline_survival <- ascvd_frs_chd_coefficients$baseline_survival[1]
    data_women$group_mean_coef <- ascvd_frs_chd_coefficients$group_mean_coef[1]

  }


  ### combine Dataset

  data <- rbind(data_women, data_men)

  data <- data[!is.na(data$id),]

  data <- data[order(data$id),]

  data[is.na(data)] <- 0

  ## Calculating individual sums TC

  if(chol_cat == "tc"){
  indv_sum <- data$age * data$coef_age +
    data$age^2 * data$coef_age.2 +
    data$coef_totchol + data$coef_hdl +
    data$coef_bp + data$smoker * data$coef_smoker +
    data$diabetic * data$coef_diabetic
  }

  ## Calculating individual sums LDL

  if(chol_cat == "ldl"){
    indv_sum <- data$age * data$coef_age +
      data$age^2 * data$coef_age.2 +
      data$coef_ldl + data$coef_hdl +
      data$coef_bp + data$coef_smoker +
      data$coef_diabetic
  }


  risk_score <- round((1 - (data$baseline_survival^
                              exp(indv_sum - data$group_mean_coef))) * 100.000, 2)

  return(risk_score)

}
#' @export
ascvd_frs_chd_table <- function(sex, age, totchol = NA, hdl, ldl = NA, sbp, dbp, smoker, diabetic, chol_cat = c("tc", "ldl")){

  chol_cat <- match.arg(chol_cat)

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

  if (any(!is.numeric(ldl)) & any(!is.na(ldl))) {
    stop("ldl must be a valid numeric value")
  }

  if (!is.numeric(sbp) | missing(sbp)) {
    stop("sbp must be a valid numeric value")
  }

  if (!is.numeric(dbp) | missing(dbp)) {
    stop("dbp must be a valid numeric value")
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

  if (any(is.na(age))) {
    warning("age contains NA's. Risk calculation less accurate.")
  }

  if (any(is.na(totchol)) & chol_cat == "tc") {
    warning("totchol contains NA's. Risk calculation less accurate.")
  }

  if (any(is.na(hdl))) {
    warning("hdl contains NA's. Risk calculation less accurate.")
  }

  if (any(is.na(ldl)) & chol_cat == "ldl") {
    warning("ldl contains NA's. Risk calculation less accurate.")
  }

  if (any(is.na(sbp))) {
    warning("sbp contains NA's. Risk calculation less accurate.")
  }

  if (any(is.na(dbp))) {
    warning("dbp contains NA's. Risk calculation less accurate.")
  }

  if (any(is.na(smoker))) {
    warning("smoker contains NA's. Risk calculation less accurate.")
  }

  if (any(is.na(diabetic))) {
    warning("diabetic contains NA's. Risk calculation less accurate.")
  }

  data <- data.frame(id = 1:length(sex), sex = sex, age = age, totchol = totchol,
                      hdl = hdl, ldl = ldl, sbp = sbp, dbp = dbp, smoker = smoker, diabetic = diabetic)

  #utils::data(sysdata, envir = environment())

  data$score <- 0

  ##geht nicht für Personen aelter 74
  ## Score Age
  data$score[data$age >= 30 & data$age < 35 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 30 & data$age < 35 & !is.na(data$age) & data$sex == "female"] - 9
  data$score[data$age >= 30 & data$age < 35 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 30 & data$age < 35 & !is.na(data$age) & data$sex == "male"] - 1
  data$score[data$age >= 35 & data$age < 40 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 35 & data$age < 40 & !is.na(data$age) & data$sex == "female"] - 4
  data$score[data$age >= 35 & data$age < 40 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 35 & data$age < 40 & !is.na(data$age) & data$sex == "male"] + 0
  data$score[data$age >= 40 & data$age < 45 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 40 & data$age < 45 & !is.na(data$age) & data$sex == "female"] + 0
  data$score[data$age >= 40 & data$age < 45 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 40 & data$age < 45 & !is.na(data$age) & data$sex == "male"] + 1
  data$score[data$age >= 45 & data$age < 50 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 45 & data$age < 50 & !is.na(data$age) & data$sex == "female"] + 3
  data$score[data$age >= 45 & data$age < 50 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 45 & data$age < 50 & !is.na(data$age) & data$sex == "male"] + 2
  data$score[data$age >= 50 & data$age < 55 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 50 & data$age < 55 & !is.na(data$age) & data$sex == "female"] + 6
  data$score[data$age >= 50 & data$age < 55 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 50 & data$age < 55 & !is.na(data$age) & data$sex == "male"] + 3
  data$score[data$age >= 55 & data$age < 60 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 55 & data$age < 60 & !is.na(data$age) & data$sex == "female"] + 7
  data$score[data$age >= 55 & data$age < 60 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 55 & data$age < 60 & !is.na(data$age) & data$sex == "male"] + 4
  data$score[data$age >= 60 & data$age < 65 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 60 & data$age < 65 & !is.na(data$age) & data$sex == "female"] + 8
  data$score[data$age >= 60 & data$age < 65 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 60 & data$age < 65 & !is.na(data$age) & data$sex == "male"] + 5
  data$score[data$age >= 65 & data$age < 70 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 65 & data$age < 70 & !is.na(data$age) & data$sex == "female"] + 8
  data$score[data$age >= 65 & data$age < 70 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 65 & data$age < 70 & !is.na(data$age) & data$sex == "male"] + 6
  data$score[data$age >= 70 & data$age < 75 & !is.na(data$age) & data$sex == "female"] <- data$score[data$age >= 70 & data$age < 75 & !is.na(data$age) & data$sex == "female"] + 8
  data$score[data$age >= 70 & data$age < 75 & !is.na(data$age) & data$sex == "male"] <- data$score[data$age >= 70 & data$age < 75 & !is.na(data$age) & data$sex == "male"] + 7

  ## Score HDL
  data$score[data$hdl < 35 & !is.na(data$hdl) & data$sex == "female"] <- data$score[data$hdl < 35 & !is.na(data$hdl) & data$sex == "female"] + 5
  data$score[data$hdl < 35 & !is.na(data$hdl) & data$sex == "male"] <- data$score[data$hdl < 35 & !is.na(data$hdl) & data$sex == "male"] + 2
  data$score[data$hdl >= 35 & data$hdl < 45 & !is.na(data$hdl) & data$sex == "female"] <- data$score[data$hdl >= 35 & data$hdl < 45 & !is.na(data$hdl) & data$sex == "female"] + 2
  data$score[data$hdl >= 35 & data$hdl < 45 & !is.na(data$hdl) & data$sex == "male"] <- data$score[data$hdl >= 35 & data$hdl < 45 & !is.na(data$hdl) & data$sex == "male"] + 1
  data$score[data$hdl >= 45 & data$hdl < 50 & !is.na(data$hdl) & data$sex == "female"] <- data$score[data$hdl >= 45 & data$hdl < 50 & !is.na(data$hdl) & data$sex == "female"] + 1
  data$score[data$hdl >= 45 & data$hdl < 50 & !is.na(data$hdl) & data$sex == "male"] <- data$score[data$hdl >= 45 & data$hdl < 50 & !is.na(data$hdl) & data$sex == "male"] + 0
  data$score[data$hdl >= 50 & data$hdl < 60 & !is.na(data$hdl) & data$sex == "female"] <- data$score[data$hdl >= 50 & data$hdl < 60 & !is.na(data$hdl) & data$sex == "female"] + 0
  data$score[data$hdl >= 50 & data$hdl < 60 & !is.na(data$hdl) & data$sex == "male"] <- data$score[data$hdl >= 50 & data$hdl < 60 & !is.na(data$hdl) & data$sex == "male"] + 0


  ## Score totchol
  if(chol_cat == "tc"){
  data$score[data$totchol < 160 & !is.na(data$totchol) & data$sex == "female"] <- data$score[data$totchol < 160 & !is.na(data$totchol) & data$sex == "female"] - 2
  data$score[data$totchol < 160 & !is.na(data$totchol) & data$sex == "male"] <- data$score[data$totchol < 160 & !is.na(data$totchol) & data$sex == "male"] - 3
  data$score[data$totchol >= 160 & data$totchol < 200 & !is.na(data$totchol) & data$sex == "female"] <- data$score[data$totchol >= 160 & data$totchol < 200 & !is.na(data$totchol) & data$sex == "female"] + 0
  data$score[data$totchol >= 160 & data$totchol < 200 & !is.na(data$totchol) & data$sex == "male"] <- data$score[data$totchol >= 160 & data$totchol < 200 & !is.na(data$totchol) & data$sex == "male"] + 0
  data$score[data$totchol >= 200 & data$totchol < 240 & !is.na(data$totchol) & data$sex == "female"] <- data$score[data$totchol >= 200 & data$totchol < 240 & !is.na(data$totchol) & data$sex == "female"] + 1
  data$score[data$totchol >= 200 & data$totchol < 240 & !is.na(data$totchol) & data$sex == "male"] <- data$score[data$totchol >= 200 & data$totchol < 240 & !is.na(data$totchol) & data$sex == "male"] + 1
  data$score[data$totchol >= 240 & data$totchol < 280 & !is.na(data$totchol) & data$sex == "female"] <- data$score[data$totchol >= 240 & data$totchol < 280 & !is.na(data$totchol) & data$sex == "female"] + 1
  data$score[data$totchol >= 240 & data$totchol < 280 & !is.na(data$totchol) & data$sex == "male"] <- data$score[data$totchol >= 240 & data$totchol < 280 & !is.na(data$totchol) & data$sex == "male"] + 2
  data$score[data$totchol >= 280 & !is.na(data$totchol) & data$sex == "female"] <- data$score[data$totchol >= 280 & !is.na(data$totchol) & data$sex == "female"] + 3
  data$score[data$totchol >= 280 & !is.na(data$totchol) & data$sex == "male"] <- data$score[data$totchol >= 280 & !is.na(data$totchol) & data$sex == "male"] + 3
  }

  ## Score LDL
  if(chol_cat == "ldl"){
    data$score[data$ldl < 100 & !is.na(data$ldl) & data$sex == "female"] <- data$score[data$ldl < 100 & !is.na(data$ldl) & data$sex == "female"] - 2
    data$score[data$ldl < 100 & !is.na(data$ldl) & data$sex == "male"] <- data$score[data$ldl < 100 & !is.na(data$ldl) & data$sex == "male"] - 3
    data$score[data$ldl >= 100 & data$ldl < 130 & !is.na(data$ldl) & data$sex == "female"] <- data$score[data$ldl >= 100 & data$ldl < 130 & !is.na(data$ldl) & data$sex == "female"] + 0
    data$score[data$ldl >= 100 & data$ldl < 130 & !is.na(data$ldl) & data$sex == "male"] <- data$score[data$ldl >= 100 & data$ldl < 130 & !is.na(data$ldl) & data$sex == "male"] + 0
    data$score[data$ldl >= 130 & data$ldl < 160 & !is.na(data$ldl) & data$sex == "female"] <- data$score[data$ldl >= 130 & data$ldl < 160 & !is.na(data$ldl) & data$sex == "female"] + 0
    data$score[data$ldl >= 130 & data$ldl < 160 & !is.na(data$ldl) & data$sex == "male"] <- data$score[data$ldl >= 130 & data$ldl < 160 & !is.na(data$ldl) & data$sex == "male"] + 0
    data$score[data$ldl >= 160 & data$ldl < 190 & !is.na(data$ldl) & data$sex == "female"] <- data$score[data$ldl >= 160 & data$ldl < 190 & !is.na(data$ldl) & data$sex == "female"] + 2
    data$score[data$ldl >= 160 & data$ldl < 190 & !is.na(data$ldl) & data$sex == "male"] <- data$score[data$ldl >= 160 & data$ldl < 190 & !is.na(data$ldl) & data$sex == "male"] + 1
    data$score[data$ldl >= 190 & !is.na(data$ldl) & data$sex == "female"] <- data$score[data$ldl >= 190 & !is.na(data$ldl) & data$sex == "female"] + 2
    data$score[data$ldl >= 190 & !is.na(data$ldl) & data$sex == "male"] <- data$score[data$ldl >= 190 & !is.na(data$ldl) & data$sex == "male"] + 2
    }

  ## Score BP

  ## First categorization
  data$bp_cat <- NA

  # Optimal
  data$bp_cat <- ifelse(data$sbp < 120 & data$dbp < 80, 1, data$bp_cat)
  # normal
  data$bp_cat <- ifelse(((data$sbp >= 120 & data$sbp < 130) | data$dbp >= 80 & data$dbp < 85), 2, data$bp_cat)
  # high normal
  data$bp_cat <- ifelse(((data$sbp >= 130 & data$sbp < 140) | data$dbp >= 85 & data$dbp < 90), 3, data$bp_cat)
  # hypertension stage 1
  data$bp_cat <- ifelse(((data$sbp >= 140 & data$sbp < 160) | data$dbp >= 90 & data$dbp < 99), 4, data$bp_cat)
  # hypertension stage 2-4
  data$bp_cat <- ifelse((data$sbp >= 160 | data$dbp > 100), 5, data$bp_cat)

  ## BP Scoring

  data$score[data$bp_cat == 1 & !is.na(data$bp_cat) & data$sex == "female"] <- data$score[data$bp_cat == 1 & !is.na(data$bp_cat) & data$sex == "female"] - 3
  data$score[data$bp_cat == 1 & !is.na(data$bp_cat) & data$sex == "male"] <- data$score[data$bp_cat == 1 & !is.na(data$bp_cat) & data$sex == "male"] + 0
  data$score[data$bp_cat == 2 & !is.na(data$bp_cat) & data$sex == "female"] <- data$score[data$bp_cat == 2 & !is.na(data$bp_cat) & data$sex == "female"] + 0
  data$score[data$bp_cat == 2 & !is.na(data$bp_cat) & data$sex == "male"] <- data$score[data$bp_cat == 2 & !is.na(data$bp_cat) & data$sex == "male"] + 0
  data$score[data$bp_cat == 3 & !is.na(data$bp_cat) & data$sex == "female"] <- data$score[data$bp_cat == 3 & !is.na(data$bp_cat) & data$sex == "female"] + 0
  data$score[data$bp_cat == 3 & !is.na(data$bp_cat) & data$sex == "male"] <- data$score[data$bp_cat == 3 & !is.na(data$bp_cat) & data$sex == "male"] + 1
  data$score[data$bp_cat == 4 & !is.na(data$bp_cat) & data$sex == "female"] <- data$score[data$bp_cat == 4 & !is.na(data$bp_cat) & data$sex == "female"] + 2
  data$score[data$bp_cat == 4 & !is.na(data$bp_cat) & data$sex == "male"] <- data$score[data$bp_cat == 4 & !is.na(data$bp_cat) & data$sex == "male"] + 2
  data$score[data$bp_cat == 5 & !is.na(data$bp_cat) & data$sex == "female"] <- data$score[data$bp_cat == 5 & !is.na(data$bp_cat) & data$sex == "female"] + 3
  data$score[data$bp_cat == 5 & !is.na(data$bp_cat) & data$sex == "male"] <- data$score[data$bp_cat == 5 & !is.na(data$bp_cat) & data$sex == "male"] + 3


  ## Score Smoker
  data$score[data$smoker == 1  & !is.na(data$smoker) & data$sex == "female"] <- data$score[data$smoker == 1  & !is.na(data$smoker) & data$sex == "female"] + 2
  data$score[data$smoker == 1  & !is.na(data$smoker) & data$sex == "male"] <- data$score[data$smoker == 1  & !is.na(data$smoker) & data$sex == "male"] + 2

  ## Score diabetic
  data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$sex == "female"] <- data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$sex == "female"] + 4
  data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$sex == "male"] <- data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$sex == "male"] + 2


  ## Calculate Risk

  ## split by sex
  data_f <- data[data$sex == "female",]
  data_m <- data[data$sex == "male",]

  if(nrow(data_f) == 0) {data_f[1,] <- NA}
  if(nrow(data_m) == 0) {data_m[1,] <- NA}

  ## adjust score values for rsiktable

  data_f$score <- ifelse(data_f$score < -2, -2, data_f$score)
  data_f$score <- ifelse(data_f$score > 17, 17, data_f$score)

  data_m$score <- ifelse(data_m$score < -3, -3, data_m$score)
  data_m$score <- ifelse(data_m$score > 14, 14, data_m$score)

  data_f$risk <- NA
  data_m$risk <- NA

  if(chol_cat == "tc"){
  data_f$risk <- table_ascvd_chd[["risktable_f_tc"]]$risk[match(data_f$score, table_ascvd_chd[["risktable_f_tc"]]$points)]
  data_m$risk <- table_ascvd_chd[["risktable_m_tc"]]$risk[match(data_m$score, table_ascvd_chd[["risktable_m_tc"]]$points)]
  }

  if(chol_cat == "ldl"){
    data_f$risk <- table_ascvd_chd[["risktable_f_ldl"]]$risk[match(data_f$score, table_ascvd_chd[["risktable_f_ldl"]]$points)]
    data_m$risk <- table_ascvd_chd[["risktable_m_ldl"]]$risk[match(data_m$score, table_ascvd_chd[["risktable_m_ldl"]]$points)]
  }

  ## combine male and female datasets and order by id

  data <- rbind(data_f, data_m)

  data <- data[!is.na(data$id),]

  data <- data[order(data$id),]


    return(data$risk)


  }

