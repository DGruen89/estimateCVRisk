#' Calculate ESC-Score2 OP Table Version
#'
#' @description This function takes necessary parameters to calculate the ESC-Score2 OP Table Version for high and low risk
#'
#' @param sex a character vector indicating the sex of the person. Values: "female", "male"
#' @param age a numeric vector with the age of persons given as years
#' @param totchol a numeric vector; Cholesterol values given in mg/dL or mmol/L. If unit is mg/dL set  the argument mmol to FALSE
#' @param hdl a numeric vector; HDL Cholesterol values given in mg/dL or mmol/L. If unit is mg/dL set  the argument mmol to FALSE
#' @param sbp a numeric vector with the systolic blood pressure of persons given as mmHg
#' @param smoker a numeric vector. Smoker = 1, non-smoker = 0. A smoker was defined as current self-reported smoker.
#' @param risk logical. Choose if which risk chart is used for calculation
#' @param mmol logical. Is Cholesterol given as mmol/L (TRUE) or mg/dL (FALSE).
#' @usage ESC_Score2_OP_table(sex, age, totchol, sbp, smoker, risk = c("low","moderate","high","very high"), mmol = FALSE)
#' @return A vector of calculated risks of persons.
#' @details Aims:
#' The aim of this study was to derive and validate the SCORE2-Older Persons (SCORE2-OP) risk model to estimate 5- and 10-year risk of cardiovascular disease (CVD)
#' in individuals aged over 70 years in four geographical risk regions.\cr
#' Methods and results:\cr
#' Sex-specific competing risk-adjusted models for estimating CVD risk (CVD mortality, myocardial infarction, or stroke) were derived in individuals
#' aged over 65 without pre-existing atherosclerotic CVD from the Cohort of Norway (28 503 individuals, 10 089 CVD events).
#' Models included age, smoking status, diabetes, systolic blood pressure, and total- and high-density lipoprotein cholesterol.
#' Four geographical risk regions were defined based on country-specific CVD mortality rates. Models were recalibrated to each region using region-specific estimated CVD
#' incidence rates and risk factor distributions. For external validation, we analysed data from 6 additional study populations {338 615 individuals, 33 219 CVD validation cohorts,
#' C-indices ranged between 0.63 \[95% confidence interval (CI) 0.61 - 0.65\] and 0.67 (0.64 – 0.69)}.
#' Regional calibration of expected-vs.-observed risks was satisfactory. For given risk factor profiles, there was substantial variation across the four risk regions
#' in the estimated 10-year CVD event risk.\cr
#' Conclusions:\cr
#' The competing risk-adjusted SCORE2-OP model was derived, recalibrated, and externally validated to estimate 5- and 10-year CVD risk in older adults (aged 70 years or older)
#' in four geographical risk regions. These models can be used for communicating the risk of CVD and potential benefit from risk factor treatment
#' and may facilitate shared decision-making between clinicians and patients in CVD risk management in older persons.
#' @references
#' SCORE2-OP working group and ESC Cardiovascular risk collaboration,
#' SCORE2-OP risk prediction algorithms: estimating incident cardiovascular event risk in older persons in four geographical risk regions,
#' European Heart Journal, Volume 42, Issue 25, 1 July 2021, Pages 2455–2467, https://doi.org/10.1093/eurheartj/ehab312
#' @export
ESC_Score2_OP_table <- function(sex, age, totchol, hdl, sbp, smoker, risk = c("low","moderate","high","very high"), mmol = FALSE) {

  risk <- match.arg(risk)

  if (risk != "low" & risk != "moderate" & risk != "high" & risk != "very high"){
    stop("risk must be either 'low', 'moderate', 'high' or 'very high'")
  }

  if (!all(sex %in% c("male", "female")) | missing(sex)) {
    stop("sex must be either 'male' or 'female'")
  }

  if (!is.numeric(age) | any(is.na(age))) {
    stop("age must be a valid numeric value")
  }

  if (!is.numeric(totchol) | any(is.na(totchol))) {
    stop("totchol must be a valid numeric value")
  }

  if (!is.numeric(hdl) | any(is.na(hdl))) {
    stop("hdl must be a valid numeric value")
  }

  if (!is.numeric(sbp) | any(is.na(sbp))) {
    stop("sbp must be a valid numeric value")
  }

  if (!is.numeric(smoker) | !all(smoker %in% c(0,1)) | missing(smoker)) {
    stop("smoker must be either 0 (no) or 1 (yes)")
  }

  if(!is.logical(mmol)){
    stop("mmol must be a single logical value")
  }

  if (any(age < 70) | any(is.na(age))) {
    warning("Some values are outside the optimal age range (< 70 years). Risk calculation can thus become less accurate.")
  }

  ESCdata <- data.frame(age = age, totchol = totchol, hdl = hdl, sex = sex, sbp = sbp, smoker = smoker)

  ESCdata$Score <- NA

  ## calculate non-hdl cholesterol

  ESCdata$non_hdl_chol <- ESCdata$totchol - ESCdata$hdl

  ## defining groups
  ## sex
  female <- (ifelse(ESCdata$sex == 'female', 1, 0))

  # SBP over 170
  SBP_4 <- (ifelse(ESCdata$sbp >= 160, 1, 0))
  # SBP 150-169
  SBP_3 <- (ifelse(ESCdata$sbp >= 140 & ESCdata$sbp < 160, 1, 0))
  # SBP 130-149
  SBP_2 <- (ifelse(ESCdata$sbp >= 120 & ESCdata$sbp < 140, 1, 0))
  # SBP under 130
  SBP_1 <- (ifelse(ESCdata$sbp < 120, 1, 0))

  #age
  age_4 <- (ifelse(ESCdata$age >= 85, 1, 0))
  age_3 <- (ifelse(ESCdata$age < 85 & ESCdata$age >= 80, 1, 0))
  age_2 <- (ifelse(ESCdata$age < 80 & ESCdata$age >= 75, 1, 0))
  age_1 <- (ifelse(ESCdata$age < 75, 1, 0))

  if(mmol == TRUE){
    #total cholesterol
    chol_4 <- (ifelse((ESCdata$non_hdl_chol) >= 6, 1, 0))
    chol_3 <- (ifelse((ESCdata$non_hdl_chol) >= 5 & (ESCdata$non_hdl_chol) < 6, 1, 0))
    chol_2 <- (ifelse((ESCdata$non_hdl_chol) >= 4 & (ESCdata$non_hdl_chol) < 5, 1, 0))
    chol_1 <- (ifelse((ESCdata$non_hdl_chol) < 4.0, 1, 0))
  }

  if(mmol == FALSE){
    #total cholesterol
    chol_4 <- (ifelse((ESCdata$non_hdl_chol*0.0259) >= 6, 1, 0))
    chol_3 <- (ifelse((ESCdata$non_hdl_chol*0.0259) >= 5 & (ESCdata$non_hdl_chol*0.0259) < 6, 1, 0))
    chol_2 <- (ifelse((ESCdata$non_hdl_chol*0.0259) >= 4 & (ESCdata$non_hdl_chol*0.0259) < 5, 1, 0))
    chol_1 <- (ifelse((ESCdata$non_hdl_chol*0.0259) < 4, 1, 0))
  }

  #smoker
  smoker <- (ifelse(ESCdata$smoker == '1', 1, 0))

  ## A - D --> age Group > 65
  #A --> line 1
  A1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  A9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))


  #B --> line 2
  B1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  B9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #C --> line 3
  C1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  C9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #D --> line 4
  D1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  D9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  ## E - H --> age Group 60 - 64
  #E --> line 5
  E1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  E9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #F --> line 6
  F1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  F9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #G --> line 7
  G1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  G9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #H --> line 8
  H1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  H9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  ## I - L --> age Group 55-59
  #I --> line 9
  I1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  I9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #J --> line 10
  J1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  J9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #K --> line 11
  K1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  K9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #L --> line 12
  L1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  L9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  ## M - P --> age Group 50 - 54
  #M --> line 13
  M1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  M5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  M9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  M13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #N --> line 14
  N1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  N5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  N9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  N13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #O --> line 15
  O1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  O5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  O9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  O13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #P --> line 16
  P1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  P5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  P9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  P13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))



  # calculation of ESC Score values
  if(risk == "low") {
  ESCdata$Score   <- (A1*28+A2*29+A3*30+A4*31+A5*31+A6*32+A7*33+A8*34+
                      A9*29+A10*35+A11*42+A12*49+A13*29+A14*35+A15*42+A16*49+
                      B1*26+B2*27+B3*28+B4*29+B5*29+B6*30+B7*31+B8*32+
                      B9*28+B10*33+B11*40+B12*47+B13*27+B14*33+B15*40+B16*47+
                      C1*24+C2*25+C3*26+C4*27+C5*27+C6*28+C7*29+C8*30+
                      C9*26+C10*32+C11*38+C12*45+C13*26+C14*32+C15*38+C16*45+
                      D1*23+D2*24+D3*25+D4*26+D5*25+D6*26+D7*27+D8*28+
                      D9*25+D10*30+D11*36+D12*43+D13*25+D14*30+D15*36+D16*43+
                      E1*20+E2*21+E3*22+E4*23+E5*25+E6*26+E7*28+E8*29+
                      E9*23+E10*27+E11*32+E12*37+E13*26+E14*31+E15*36+E16*41+
                      F1*18+F2*19+F3*20+F4*21+F5*23+F6*24+F7*25+F8*26+
                      F9*21+F10*25+F11*29+F12*34+F13*24+F14*28+F15*33+F16*38+
                      G1*16+G2*17+G3*18+G4*19+G5*20+G6*21+G7*22+G8*23+
                      G9*19+G10*22+G11*26+G12*31+G13*22+G14*25+G15*30+G16*34+
                      H1*15+H2*15+H3*16+H4*17+H5*18+H6*19+H7*20+H8*21+
                      H9*17+H10*20+H11*24+H12*28+H13*19+H14*23+H15*27+H16*31+
                      I1*15+I2*15+I3*16+I4*17+I5*21+I6*22+I7*23+I8*24+
                      I9*19+I10*21+I11*24+I12*27+I13*24+I14*27+I15*31+I16*34+
                      J1*13+J2*13+J3*14+J4*15+J5*18+J6*19+J7*20+J8*21+
                      J9*16+J10*18+J11*21+J12*23+J13*21+J14*23+J15*26+J16*30+
                      K1*11+K2*11+K3*12+K4*13+K5*15+K6*16+K7*17+K8*18+
                      K9*14+K10*15+K11*18+K12*20+K13*18+K14*20+K15*23+K16*26+
                      L1*9+L2*10+L3*10+L4*11+L5*13+L6*14+L7*15+L8*15+
                      L9*12+L10*13+L11*15+L12*17+L13*15+L14*17+L15*19+L16*22+
                      M1*10+M2*11+M3*12+M4*12+M5*17+M6*18+M7*19+M8*20+
                      M9*15+M10*16+M11*18+M12*19+M13*22+M14*24+M15*26+M16*28+
                      N1*9+N2*9+N3*10+N4*10+N5*14+N6*15+N7*16+N8*16+
                      N9*12+N10*13+N11*14+N12*16+N13*18+N14*19+N15*21+N16*23+
                      O1*7+O2*7+O3*8+O4*8+O5*11+O6*12+O7*13+O8*14+
                      O9*10+O10*11+O11*12+O12*13+O13*14+O14*16+O15*17+O16*19+
                      P1*6+P2*6+P3*6+P4*7+P5*9+P6*10+P7*10+P8*11+
                      P9*8+P10*8+P11*9+P12*10+P13*12+P14*13+P15*14+P16*15)
  }

  if(risk == "moderate") {
    ESCdata$Score   <- (A1*37+A2*39+A3*40+A4*42+A5*41+A6*43+A7*44+A8*46+
                          A9*37+A10*45+A11*53+A12*62+A13*37+A14*45+A15*53+A16*61+
                          B1*35+B2*36+B3*38+B4*39+B5*39+B6*40+B7*42+B8*43+
                          B9*36+B10*43+B11*51+B12*59+B13*35+B14*43+B15*51+B16*59+
                          C1*32+C2*34+C3*35+C4*37+C5*36+C6*38+C7*39+C8*41+
                          C9*34+C10*41+C11*49+C12*57+C13*34+C14*41+C15*48+C16*57+
                          D1*30+D2*32+D3*33+D4*34+D5*34+D6*35+D7*37+D8*38+
                          D9*32+D10*39+D11*47+D12*55+D13*32+D14*39+D15*46+D16*55+
                          E1*27+E2*28+E3*30+E4*31+E5*34+E6*35+E7*37+E8*39+
                          E9*30+E10*35+E11*41+E12*47+E13*34+E14*40+E15*46+E16*53+
                          F1*24+F2*25+F3*27+F4*28+F5*30+F6*32+F7*33+F8*35+
                          F9*27+F10*32+F11*37+F12*43+F13*31+F14*36+F15*42+F16*48+
                          G1*21+G2*22+G3*24+G4*25+G5*27+G6*28+G7*30+G8*31+
                          G9*25+G10*29+G11*34+G12*40+G13*28+G14*33+G15*38+G16*44+
                          H1*19+H2*20+H3*21+H4*22+H5*24+H6*25+H7*27+H8*28+
                          H9*22+H10*26+H11*31+H12*36+H13*25+H14*30+H15*35+H16*40+
                          I1*19+I2*20+I3*21+I4*23+I5*27+I6*29+I7*30+I8*32+
                          I9*24+I10*27+I11*31+I12*35+I13*31+I14*35+I15*39+I16*44+
                          J1*16+J2*17+J3*18+J4*19+J5*24+J6*25+J7*26+J8*28+
                          J9*21+J10*23+J11*27+J12*30+J13*27+J14*30+J15*34+J16*38+
                          K1*14+K2*15+K3*15+K4*16+K5*20+K6*21+K7*22+K8*24+
                          K9*17+K10*20+K11*23+K12*26+K13*23+K14*26+K15*29+K16*33+
                          L1*12+L2*12+L3*13+L4*14+L5*17+L6*18+L7*19+L8*20+
                          L9*15+L10*17+L11*19+L12*22+L13*19+L14*22+L15*25+L16*29+
                          M1*13+M2*14+M3*15+M4*16+M5*22+M6*23+M7*25+M8*26+
                          M9*19+M10*21+M11*23+M12*25+M13*28+M14*31+M15*34+M16*36+
                          N1*11+N2*11+N3*12+N4*13+N5*18+N6*19+N7*20+N8*22+
                          N9*15+N10*17+N11*18+N12*20+N13*23+N14*25+N15*28+N16*30+
                          O1*9+O2*9+O3*10+O4*11+O5*15+O6*16+O7*17+O8*18+
                          O9*12+O10*13+O11*15+O12*16+O13*19+O14*20+O15*22+O16*24+
                          P1*7+P2*7+P3*8+P4*8+P5*12+P6*13+P7*13+P8*14+
                          P9*10+P10*11+P11*12+P12*13+P13*15+P14*16+P15*18+P16*20)
  }

  if(risk == "high") {
    ESCdata$Score   <- (A1*53+A2*55+A3*57+A4*58+A5*58+A6*59+A7*61+A8*63+
                          A9*42+A10*49+A11*57+A12*65+A13*41+A14*49+A15*56+A16*65+
                          B1*50+B2*52+B3*54+B4*55+B5*55+B6*56+B7*58+B8*60+
                          B9*40+B10*47+B11*55+B12*63+B13*40+B14*47+B15*54+B16*62+
                          C1*47+C2*49+C3*51+C4*52+C5*52+C6*53+C7*55+C8*57+
                          C9*38+C10*45+C11*53+C12*61+C13*38+C14*45+C15*52+C16*60+
                          D1*44+D2*46+D3*48+D4*50+D5*49+D6*51+D7*52+D8*54+
                          D9*36+D10*43+D11*51+D12*58+D13*36+D14*43+D15*50+D16*58+
                          E1*40+E2*42+E3*44+E4*45+E5*49+E6*51+E7*53+E8*55+
                          E9*34+E10*40+E11*45+E12*51+E13*38+E14*44+E15*50+E16*56+
                          F1*36+F2*38+F3*39+F4*41+F5*44+F6*46+F7*48+F8*50+
                          F9*31+F10*36+F11*42+F12*47+F13*35+F14*40+F15*46+F16*52+
                          G1*32+G2*34+G3*36+G4*37+G5*40+G6*42+G7*44+G8*46+
                          G9*29+G10*33+G11*38+G12*44+G13*32+G14*37+G15*42+G16*48+
                          H1*29+H2*31+H3*32+H4*34+H5*36+H6*38+H7*40+H8*41+
                          H9*26+H10*30+H11*35+H12*40+H13*29+H14*34+H15*39+H16*44+
                          I1*29+I2*31+I3*32+I4*34+I5*41+I6*43+I7*45+I8*47+
                          I9*28+I10*32+I11*35+I12*39+I13*35+I14*39+I15*44+I16*48+
                          J1*25+J2*27+J3*28+J4*29+J5*35+J6*37+J7*39+J8*41+
                          J9*24+J10*27+J11*31+J12*34+J13*31+J14*34+J15*38+J16*43+
                          K1*22+K2*23+K3*24+K4*25+K5*31+K6*32+K7*34+K8*36+
                          K9*21+K10*24+K11*27+K12*30+K13*27+K14*30+K15*34+K16*37+
                          L1*18+L2*19+L3*20+L4*22+L5*26+L6*28+L7*29+L8*31+
                          L9*18+L10*20+L11*23+L12*26+L13*23+L14*26+L15*29+L16*33+
                          M1*21+M2*22+M3*24+M4*25+M5*33+M6*35+M7*37+M8*39+
                          M9*23+M10*25+M11*27+M12*29+M13*33+M14*35+M15*38+M16*41+
                          N1*17+N2*18+N3*19+N4*20+N5*28+N6*29+N7*31+N8*33+
                          N9*19+N10*20+N11*22+N12*24+N13*27+N14*29+N15*32+N16*34+
                          O1*14+O2*15+O3*36+O4*17+O5*23+O6*24+O7*26+O8*27+
                          O9*15+O10*17+O11*18+O12*20+O13*22+O14*24+O15*26+O16*28+
                          P1*11+P2*12+P3*13+P4*14+P5*19+P6*20+P7*21+P8*22+
                          P9*12+P10*14+P11*15+P12*16+P13*18+P14*20+P15*22+P16*23)
  }

  if(risk == "very_high") {
    ESCdata$Score   <- (A1*62+A2*63+A3*64+A4*65+A5*65+A6*66+A7*67+A8*68+
                          A9*49+A10*54+A11*59+A12*64+A13*49+A14*54+A15*59+A16*64+
                          B1*60+B2*61+B3*62+B4*63+B5*63+B6*64+B7*65+B8*66+
                          B9*48+B10*53+B11*58+B12*63+B13*48+B14*53+B15*58+B16*63+
                          C1*58+C2*59+C3*60+C4*61+C5*61+C6*62+C7*63+C8*65+
                          C9*47+C10*52+C11*56+C12*61+C13*47+C14*52+C15*56+C16*61+
                          D1*56+D2*57+D3*58+D4*60+D5*59+D6*60+D7*61+D8*63+
                          D9*46+D10*50+D11*55+D12*60+D13*46+D14*50+D15*55+D16*60+
                          E1*53+E2*54+E3*55+E4*57+E5*59+E6*60+E7*62+E8*63+
                          E9*44+E10*48+E11*52+E12*56+E13*47+E14*51+E15*55+E16*59+
                          F1*50+F2*51+F3*52+F4*54+F5*56+F6*57+F7*59+F8*60+
                          F9*42+F10*46+F11*49+F12*53+F13*45+F14*49+F15*52+F16*56+
                          G1*47+G2*48+G3*49+G4*51+G5*53+G6*54+G7*56+G8*57+
                          G9*40+G10*43+G11*47+G12*51+G13*43+G14*46+G15*50+G16*54+
                          H1*44+H2*45+H3*47+H4*48+H5*50+H6*51+H7*53+H8*54+
                          H9*38+H10*41+H11*45+H12*48+H13*40+H14*44+H15*48+H16*51+
                          I1*44+I2*46+I3*47+I4*48+I5*53+I6*55+I7*56+I8*58+
                          I9*40+I10*42+I11*45+I12*48+I13*45+I14*48+I15*51+I16*54+
                          J1*41+J2*42+J3*43+J4*45+J5*49+J6*51+J7*52+J8*53+
                          J9*37+J10*39+J11*42+J12*44+J13*42+J14*44+J15*47+J16*50+
                          K1*37+K2*39+K3*40+K4*41+K5*46+K6*47+K7*48+K8*49+
                          K9*34+K10*36+K11*39+K12*41+K13*39+K14*41+K15*44+K16*47+
                          L1*34+L2*35+L3*36+L4*37+L5*42+L6*43+L7*44+L8*46+
                          L9*31+L10*33+L11*36+L12*38+L13*36+L14*38+L15*41+L16*43+
                          M1*37+M2*38+M3*39+M4*41+M5*48+M6*49+M7*51+M8*52+
                          M9*35+M10*37+M11*39+M12*40+M13*43+M14*45+M15*47+M16*49+
                          N1*33+N2*34+N3*35+N4*36+N5*43+N6*44+N7*46+N8*47+
                          N9*32+N10*33+N11*35+N12*36+N13*39+N14*41+N15*42+N16*44+
                          O1*29+O2*30+O3*31+O4*32+O5*39+O6*40+O7*41+O8*43+
                          O9*28+O10*30+O11*31+O12*33+O13*35+O14*36+O15*38+O16*40+
                          P1*26+P2*27+P3*28+P4*29+P5*34+P6*36+P7*37+P8*38+
                          P9*25+P10*26+P11*28+P12*29+P13*31+P14*33+P15*34+P16*36)
  }

  # ESC Score value [%]



  return(ESCdata$Score)
}
