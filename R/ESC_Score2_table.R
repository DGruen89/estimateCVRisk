#' Calculate ESC-Score2  Table Version
#'
#' @description This function takes necessary parameters to calculate the ESC-Score2 to estimate 10-year risk of cardiovascular disease in Europe.
#'
#' @param sex a character vector indicating the sex of the person. Values: "female", "male"
#' @param age a numeric vector with the age of persons given as years
#' @param totchol a numeric vector; Cholesterol values given in mg/dL or mmol/L. If unit is mg/dL set  the argument mmol to FALSE
#' @param sbp a numeric vector with the systolic blood pressure of persons given as mmHg
#' @param smoker a numeric vector. Smoker = 1, non-smoker = 0. A smoker was defined as current self-reported smoker.
#' @param risk logical. Choose if which risk chart is used for calculation
#' @param mmol logical. Is Cholesterol given as mmol/L (TRUE) or mg/dL (FALSE).
#' @usage ESC_Score2_table(sex, age, totchol, sbp, smoker, risk = c("low","moderate","high","very high"), mmol = FALSE)
#' @return A vector of calculated risks of persons.
#' @details Aims:\cr
#' The aim of this study was to develop, validate, and illustrate an updated prediction model (SCORE2) to estimate 10-year fatal and non-fatal cardiovascular disease (CVD) risk in individuals
#' without previous CVD or diabetes aged 40–69 years in Europe.\cr
#' Methods and results:\cr
#' We derived risk prediction models using individual-participant data from 45 cohorts in 13 countries (677 684 individuals, 30 121 CVD events).
#' We used sex-specific and competing risk-adjusted models, including age, smoking status, systolic blood pressure, and total- and HDL-cholesterol.
#' We defined four risk regions in Europe according to country-specific CVD mortality, recalibrating models to each region using expected incidences
#' and risk factor distributions. Region-specific incidence was estimated using CVD mortality and incidence data on 10 776 466 individuals.
#' For external validation, we analysed data from 25 additional cohorts in 15 European countries (1 133 181 individuals, 43 492 CVD events).
#' After applying the derived risk prediction models to external validation cohorts, C-indices ranged from 0.67 (0.65 – 0.68) to 0.81 (0.76 – 0.86).
#' Predicted CVD risk varied several-fold across European regions. For example, the estimated 10-year CVD risk for a 50-year-old smoker,
#' with a systolic blood pressure of 140mmHg, total cholesterol of 5.5mmol/L, and HDL-cholesterol of 1.3mmol/L, ranged from 5.9% for men in low-risk countries
#' to 14.0% for men in very high-risk countries, and from 4.2% for women in low-risk countries to 13.7% for women in very high-risk countries\cr
#' Conclusion:\cr
#' SCORE2—a new algorithm derived, calibrated, and validated to predict 10-year risk of first-onset CVD in European populations—enhances
#' the identification of individuals at higher risk of developing CVD across Europe.
#' @references
#' SCORE2 working group and ESC Cardiovascular risk collaboration,
#' SCORE2 risk prediction algorithms: new models to estimate 10-year risk of cardiovascular disease in Europe,
#' European Heart Journal, Volume 42, Issue 25, 1 July 2021, Pages 2439–2454, https://doi.org/10.1093/eurheartj/ehab309
#' @export
ESC_Score2_table <- function(sex, age, totchol, sbp, smoker, risk = c("low","moderate","high","very high"), mmol = FALSE) {

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

  if (!is.numeric(sbp) | any(is.na(sbp))) {
    stop("sbp must be a valid numeric value")
  }

  if (!is.numeric(smoker) | !all(smoker %in% c(0,1)) | missing(smoker)) {
    stop("smoker must be either 0 (no) or 1 (yes)")
  }

  if(!is.logical(mmol)){
    stop("mmol must be a single logical value")
  }

  if (any(age < 40) | any(age > 69) | any(is.na(age))) {
    warning("Some values are outside the optimal age range (40-69 years). Risk calculation can thus become less accurate.")
  }

  ESCdata <- data.frame(age = age, totchol = totchol, sex = sex, sbp = sbp, smoker = smoker)

  ESCdata$Score <- NA

  ## defining groups
  sex
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
  age_6 <- (ifelse(ESCdata$age >= 65, 1, 0))
  age_5 <- (ifelse(ESCdata$age < 65 & ESCdata$age >= 60, 1, 0))
  age_4 <- (ifelse(ESCdata$age < 60 & ESCdata$age >= 55, 1, 0))
  age_3 <- (ifelse(ESCdata$age < 55 & ESCdata$age >= 50, 1, 0))
  age_2 <- (ifelse(ESCdata$age < 50 & ESCdata$age >= 45, 1, 0))
  age_1 <- (ifelse(ESCdata$age < 45, 1, 0))

  if(mmol == TRUE){
    #total cholesterol
    chol_4 <- (ifelse((ESCdata$totchol) >= 6, 1, 0))
    chol_3 <- (ifelse((ESCdata$totchol) >= 5 & (ESCdata$totchol) < 6, 1, 0))
    chol_2 <- (ifelse((ESCdata$totchol) >= 4 & (ESCdata$totchol) < 5, 1, 0))
    chol_1 <- (ifelse((ESCdata$totchol) < 4.0, 1, 0))
  }

  if(mmol == FALSE){
    #total cholesterol
    chol_4 <- (ifelse((ESCdata$totchol*0.0259) >= 6, 1, 0))
    chol_3 <- (ifelse((ESCdata$totchol*0.0259) >= 5 & (ESCdata$totchol*0.0259) < 6, 1, 0))
    chol_2 <- (ifelse((ESCdata$totchol*0.0259) >= 4 & (ESCdata$totchol*0.0259) < 5, 1, 0))
    chol_1 <- (ifelse((ESCdata$totchol*0.0259) < 4, 1, 0))
  }

  #smoker
  smoker <- (ifelse(ESCdata$smoker == '1', 1, 0))

  ## A - D --> age Group > 65
  #A --> line 1
  A1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  A9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))


  #B --> line 2
  B1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  B9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #C --> line 3
  C1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  C9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #D --> line 4
  D1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  D9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  ## E - H --> age Group 60 - 64
  #E --> line 5
  E1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  E9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #F --> line 6
  F1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  F9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #G --> line 7
  G1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  G9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #H --> line 8
  H1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  H9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  ## I - L --> age Group 55-59
  #I --> line 9
  I1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  I9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #J --> line 10
  J1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  J9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #K --> line 11
  K1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  K9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #L --> line 12
  L1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  L9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  ## M - P --> age Group 50 - 54
  #M --> line 13
  M1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  M5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  M9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  M13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #N --> line 14
  N1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  N5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  N9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  N13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #O --> line 15
  O1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  O5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  O9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  O13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #P --> line 16
  P1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  P5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  P9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  P13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  ## Q - T --> age Group 45 - 49
  #Q --> line 17
  Q1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  Q2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  Q3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  Q4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  Q5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  Q6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  Q7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  Q8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  Q9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  Q10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  Q11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  Q12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  Q13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  Q14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  Q15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  Q16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #R --> line 18
  R1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  R2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  R3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  R4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  R5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  R6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  R7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  R8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  R9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  R10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  R11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  R12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  R13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  R14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  R15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  R16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #S --> line 19
  S1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  S2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  S3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  S4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  S5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  S6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  S7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  S8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  S9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  S10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  S11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  S12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  S13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  S14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  S15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  S16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #T --> line 20
  T1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  T2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  T3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  T4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  T5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  T6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  T7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  T8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  T9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  T10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  T11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  T12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  T13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  T14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  T15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  T16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))


  ## U - X --> age Group 40 - 44
  #U --> line 21
  U1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  U2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  U3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  U4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  U5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  U6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  U7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  U8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  U9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  U10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  U11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  U12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  U13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  U14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  U15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  U16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #V --> line 22
  V1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  V2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  V3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  V4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  V5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  V6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  V7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  V8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  V9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  V10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  V11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  V12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  V13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  V14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  V15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  V16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #W --> line 23
  W1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  W2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  W3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  W4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  W5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  W6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  W7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  W8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  W9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  W10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  W11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  W12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  W13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  W14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  W15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  W16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  #X --> line 24
  X1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  X2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  X3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  X4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  X5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  X6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  X7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  X8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  X9 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  X10 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  X11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  X12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  X13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  X14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  X15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  X16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))

  # calculation of ESC Score values
  if(risk == "low") {
  ESCdata$Score   <- (A1*8+A2*8+A3*9+A4*9+A5*12+A6*12+A7*13+A8*13+
                      A9*11+A10*12+A11*12+A12*13+A13*15+A14*16+A15*17+A16*19+
                      B1*7+B2*7+B3*7+B4*7+B5*10+B6*10+B7*11+B8*11+
                      B9*9+B10*10+B11*11+B12*11+B13*13+B14*14+B15*15+B16*16+
                      C1*5+C2*6+C3*6+C4*6+C5*8+C6*9+C7*9+C8*9+
                      C9*8+C10*8+C11*9+C12*10+C13*11+C14*12+C15*13+C16*13+
                      D1*5+D2*5+D3*5+D4*5+D5*7+D6*7+D7*7+D8*8+
                      D9*6+D10*7+D11*7+D12*8+D13*9+D14*10+D15*11+D16*11+
                      E1*6+E2*6+E3*7+E4*7+E5*10+E6*10+E7*11+E8*11+
                      E9*8+E10*9+E11*10+E12*11+E13*13+E14*14+E15*15+E16*17+
                      F1*5+F2*5+F3*5+F4*6+F5*8+F6*8+F7*9+F8*9+
                      F9*7+F10*8+F11*8+F12*9+F13*10+F14*11+F15*13+F16*14+
                      G1*4+G2*4+G3*4+G4*5+G5*6+G6*7+G7*7+G8*8+
                      G9*6+G10*6+G11*7+G12*8+G13*9+G14*10+G15*10+G16*11+
                      H1*3+H2*3+H3*4+H4*4+H5*5+H6*6+H7*6+H8*6+
                      H9*5+H10*5+H11*6+H12*6+H13*7+H14*8+H15*9+H16*10+
                      I1*4+I2*5+I3*5+I4*5+I5*8+I6*8+I7*9+I8*10+
                      I9*7+I10*7+I11*8+I12*9+I13*10+I14*12+I15*13+I16*15+
                      J1*3+J2*4+J3*4+J4*4+J5*6+J6*7+J7*7+J8*8+
                      J9*5+J10*6+J11*7+J12*8+J13*9+J14*10+J15*11+J16*12+
                      K1*3+K2*3+K3*3+K4*3+K5*5+K6*5+K7*6+K8*6+
                      K9*4+K10*5+K11*5+K12*6+K13*7+K14*8+K15*9+K16*10+
                      L1*2+L2*2+L3*3+L4*3+L5*4+L6*4+L7*5+L8*5+
                      L9*4+L10*4+L11*4+L12*5+L13*6+L14*6+L15*7+L16*8+
                      M1*3+M2*4+M3*4+M4*4+M5*6+M6*7+M7*7+M8*8+
                      M9*5+M10*6+M11*7+M12*8+M13*9+M14*10+M15*11+M16*13+
                      N1*3+N2*3+N3*3+N4*3+N5*5+N6*5+N7*6+N8*6+
                      N9*4+N10*5+N11*5+N12*6+N13*7+N14*8+N15*9+N16*10+
                      O1*2+O2*2+O3*2+O4*3+O5*4+O6*4+O7*5+O8*5+
                      O9*3+O10*4+O11*4+O12*5+O13*6+O14*6+O15*7+O16*8+
                      P1*2+P2*2+P3*2+P4*2+P5*3+P6*3+P7*4+P8*4+
                      P9*3+P10*3+P11*3+P12*4+P13*4+P14*5+P15*6+P16*7+
                      Q1*2+Q2*3+Q3*3+Q4*3+Q5*5+Q6*5+Q7*6+Q8*7+
                      Q9*4+Q10*5+Q11*6+Q12*6+Q13*7+Q14*8+Q15*10+Q16*11+
                      R1*2+R2*2+R3*2+R4*3+R5*4+R6*4+R7*5+R8*5+
                      R9*3+R10*4+R11*4+R12*5+R13*6+R14*7+R15*8+R16*9+
                      S1*1+S2*2+S3*2+S4*2+S5*3+S6*3+S7*4+S8*4+
                      S9*2+S10*3+S11*3+S12*4+S13*4+S14*5+S15*6+S16*7+
                      T1*1+T2*1+T3*1+T4*1+T5*2+T6*2+T7*3+T8*3+
                      T9*2+T10*2+T11*3+T12*3+T13*3+T14*4+T15*5+T16*5+
                      U1*2+U2*2+U3*2+U4*3+U5*4+U6*4+U7*5+U8*6+
                      U9*3+U10*4+U11*5+U12*5+U13*6+U14*7+U15*8+U16*10+
                      V1*1+V2*2+V3*2+V4*2+V5*3+V6*3+V7*4+V8*4+
                      V9*2+V10*3+V11*3+V12*4+V13*5+V14*5+V15*6+V16*8+
                      W1*1+W2*1+W3*1+W4*1+W5*2+W6*3+W7*3+W8*3+
                      W9*2+W10*2+W11*3+W12*3+W13*3+W14*4+W15*5+W16*6+
                      X1*1+X2*1+X3*1+X4*1+X5*2+X6*2+X7*2+X8*2+
                      X9*1+X10*2+X11*2+X12*2+X13*3+X14*3+X15*4+X16*5)
  }

  if(risk == "moderate") {
    ESCdata$Score   <- (A1*10+A2*10+A3*11+A4*12+A5*15+A6*16+A7*17+A8*18+
                          A9*14+A10*15+A11*17+A12*18+A13*20+A14*22+A15*23+A16*25+
                          B1*8+B2*9+B3*9+B4*9+B5*13+B6*13+B7*14+B8*15+
                          B9*12+B10*13+B11*14+B12*15+B13*17+B14*18+B15*20+B16*21+
                          C1*7+C2*7+C3*7+C4*8+C5*10+C6*11+C7*12+C8*12+
                          C9*10+C10*11+C11*12+C12*13+C13*14+C14*15+C15*17+C16*18+
                          D1*5+D2*6+D3*6+D4*6+D5*9+D6*9+D7*9+D8*10+
                          D9*8+D10*9+D11*10+D12*10+D13*12+D14*13+D15*14+D16*15+
                          E1*7+E2*8+E3*8+E4*9+E5*12+E6*13+E7*14+E8*15+
                          E9*11+E10*12+E11*13+E12*15+E13*17+E14*18+E15*20+E16*22+
                          F1*6+F2*6+F3*7+F4*7+F5*10+F6*11+F7*11+F8*12+
                          F9*9+F10*10+F11*11+F12*12+F13*14+F14*15+F15*17+F16*18+
                          G1*5+G2*5+G3*5+G4*6+G5*8+G6*8+G7*9+G8*10+
                          G9*7+G10*8+G11*9+G12*10+G13*11+G14*13+G15*14+G16*15+
                          H1*4+H2*4+H3*4+H4*5+H5*6+H6*7+H7*7+H8*8+
                          H9*6+H10*7+H11*7+H12*8+H13*9+H14*10+H15*11+H16*12+
                          I1*5+I2*6+I3*6+I4*7+I5*10+I6*11+I7*11+I8*12+
                          I9*9+I10*10+I11*11+I12*12+I13*14+I14*16+I15*17+I16*20+
                          J1*4+J2*4+J3*5+J4*5+J5*8+J6*8+J7*9+J8*10+
                          J9*7+J10*8+J11*9+J12*10+J13*11+J14*13+J15*14+J16*16+
                          K1*3+K2*3+K3*4+K4*4+K5*6+K6*7+K7*7+K8*8+
                          K9*5+K10*6+K11*7+K12*8+K13*9+K14*10+K15*11+K16*13+
                          L1*3+L2*3+L3*3+L4*3+L5*5+L6*5+L7*6+L8*6+
                          L9*4+L10*5+L11*6+L12*6+L13*7+L14*8+L15*9+L16*10+
                          M1*4+M2*4+M3*5+M4*5+M5*8+M6*8+M7*9+M8*10+
                          M9*7+M10*8+M11*9+M12*10+M13*11+M14*13+M15*15+M16*17+
                          N1*3+N2*3+N3*4+N4*4+N5*6+N6*6+N7*7+N8*8+
                          N9*5+N10*6+N11*7+N12*8+N13*9+N14*10+N15*12+N16*14+
                          O1*2+O2*2+O3*3+O4*3+O5*5+O6*5+O7*6+O8*6+
                          O9*4+O10*5+O11*5+O12*6+O13*7+O14*8+O15*9+O16*11+
                          P1*2+P2*2+P3*2+P4*2+P5*3+P6*4+P7*4+P8*5+
                          P9*3+P10*4+P11*4+P12*5+P13*5+P14*6+P15*7+P16*8+
                          Q1*3+Q2*3+Q3*3+Q4*4+Q5*6+Q6*7+Q7*8+Q8*9+
                          Q9*5+Q10*6+Q11*7+Q12*8+Q13*9+Q14*11+Q15*13+Q16*15+
                          R1*2+R2*2+R3*3+R4*3+R5*5+R6*5+R7*6+R8*6+
                          R9*4+R10*5+R11*5+R12*6+R13*7+R14*8+R15*10+R16*12+
                          S1*2+S2*2+S3*2+S4*2+S5*3+S6*4+S7*4+S8*5+
                          S9*3+S10*4+S11*4+S12*5+S13*5+S14*7+S15*8+S16*9+
                          T1*1+T2*1+T3*1+T4*2+T5*3+T6*3+T7*3+T8*4+
                          T9*2+T10*3+T11*3+T12*4+T13*4+T14*5+T15*6+T16*7+
                          U1*2+U2*2+U3*3+U4*3+U5*5+U6*5+U7*6+U8*7+
                          U9*4+U10*5+U11*6+U12*7+U13*8+U14*9+U15*11+U16*13+
                          V1*1+V2*2+V3*2+V4*2+V5*3+V6*4+V7*5+V8*5+
                          V9*3+V10*4+V11*4+V12*5+V13*6+V14*7+V15*8+V16*10+
                          W1*1+W2*1+W3*1+W4*2+W5*3+W6*3+W7*3+W8*4+
                          W9*2+W10*3+W11*3+W12*4+W13*4+W14*5+W15*6+W16*8+
                          X1*1+X2*1+X3*1+X4*1+X5*2+X6*2+X7*2+X8*3+
                          X9*2+X10*2+X11*2+X12*3+X13*3+X14*4+X15*5+X16*6)
  }

  if(risk == "high") {
    ESCdata$Score   <- (A1*15+A2*16+A3*17+A4*18+A5*26+A6*27+A7*29+A8*30+
                          A9*17+A10*18+A11*20+A12*22+A13*25+A14*28+A15*30+A16*32+
                          B1*12+B2*13+B3*14+B4*14+B5*21+B6*22+B7*23+B8*24+
                          B9*14+B10*15+B11*16+B12*18+B13*21+B14*23+B15*25+B16*27+
                          C1*10+C2*10+C3*11+C4*11+C5*16+C6*17+C7*18+C8*19+
                          C9*11+C10*12+C11*13+C12*15+C13*17+C14*19+C15*20+C16*22+
                          D1*8+D2*8+D3*8+D4*9+D5*13+D6*14+D7*14+D8*15+
                          D9*9+D10*10+D11*11+D12*12+D13*14+D14*15+D15*17+D16*18+
                          E1*11+E2*11+E3*12+E4*13+E5*20+E6*21+E7*23+E8*25+
                          E9*13+E10*14+E11*16+E12*18+E13*20+E14*23+E15*25+E16*28+
                          F1*8+F2*9+F3*9+F4*10+F5*15+F6*16+F7*18+F8*19+
                          F9*10+F10*11+F11*13+F12*14+F13*16+F14*18+F15*20+F16*23+
                          G1*6+G2*7+G3*7+G4*8+G5*12+G6*13+G7*14+G8*15+
                          G9*8+G10*9+G11*10+G12*11+G13*13+G14*15+G15*16+G16*18+
                          H1*5+H2*5+H3*6+H4*6+H5*9+H6*10+H7*11+H8*11+
                          H9*6+H10*7+H11*8+H12*9+H13*10+H14*12+H15*13+H16*15+
                          I1*7+I2*8+I3*9+I4*10+I5*15+I6*16+I7*18+I8*20+
                          I9*9+I10*11+I11*12+I12*14+I13*16+I14*19+I15*21+I16*24+
                          J1*5+J2*6+J3*7+J4*7+J5*11+J6*12+J7*14+J8*15+
                          J9*7+J10*8+J11*10+J12*11+J13*13+J14*15+J15*17+J16*19+
                          K1*4+K2*4+K3*5+K4*5+K5*8+K6*9+K7*10+K8*11+
                          K9*6+K10*6+K11*7+K12*9+K13*10+K14*11+K15*13+K16*15+
                          L1*3+L2*3+L3*4+L4*4+L5*6+L6*7+L7*8+L8*8+
                          L9*4+L10*5+L11*6+L12*7+L13*8+L14*9+L15*10+L16*12+
                          M1*5+M2*5+M3*6+M4*7+M5*11+M6*13+M7*14+M8*16+
                          M9*7+M10*8+M11*10+M12*11+M13*13+M14*15+M15*18+M16*21+
                          N1*3+N2*4+N3*4+N4*5+N5*8+N6*9+N7*10+N8*12+
                          N9*5+N10*6+N11*7+N12*9+N13*10+N14*12+N15*14+N16*16+
                          O1*3+O2*3+O3*3+O4*4+O5*6+O6*7+O7*8+O8*9+
                          O9*4+O10*5+O11*5+O12*6+O13*7+O14*9+O15*10+O16*12+
                          P1*2+P2*2+P3*2+P4*3+P5*4+P6*5+P7*6+P8*6+
                          P9*3+P10*3+P11*4+P12*5+P13*6+P14*7+P15*8+P16*9+
                          Q1*3+Q2*4+Q3*4+Q4*5+Q5*8+Q6*10+Q7*11+Q8*13+
                          Q9*5+Q10*6+Q11*8+Q12*9+Q13*10+Q14*13+Q15*15+Q16*18+
                          R1*2+R2*3+R3*3+R4*4+R5*6+R6*7+R7*8+R8*9+
                          R9*4+R10*5+R11*6+R12*7+R13*8+R14*9+R15*11+R16*14+
                          S1*2+S2*2+S3*2+S4*2+S5*4+S6*5+S7*6+S8*6+
                          S9*3+S10*3+S11*4+S12*5+S13*6+S14*7+S15*8+S16*10+
                          T1*1+T2*1+T3*2+T4*2+T5*3+T6*3+T7*4+T8*5+
                          T9*2+T10*2+T11*3+T12*4+T13*4+T14*5+T15*6+T16*7+
                          U1*2+U2*3+U3*3+U4*4+U5*6+U6*7+U7*9+U8*10+
                          U9*4+U10*5+U11*6+U12*7+U13*8+U14*10+U15*13+U16*16+
                          V1*1+V2*2+V3*2+V4*2+V5*4+V6*5+V7*6+V8*7+
                          V9*3+V10*3+V11*4+V12*5+V13*6+V14*7+V15*9+V16*11+
                          W1*1+W2*1+W3*1+W4*2+W5*3+W6*4+W7*4+W8*5+
                          W9*2+W10*2+W11*3+W12*4+W13*4+W14*5+W15*7+W16*8+
                          X1*1+X2*1+X3*1+X4*1+X5*2+X6*2+X7*3+X8*3+
                          X9*1+X10*2+X11*2+X12*3+X13*3+X14*4+X15*5+X16*6)
  }

  if(risk == "very_high") {
    ESCdata$Score   <- (A1*27+A2*28+A3*30+A4*31+A5*41+A6*42+A7*44+A8*46+
                          A9*26+A10*28+A11*30+A12*32+A13*36+A14*39+A15*42+A16*44+
                          B1*22+B2*23+B3*24+B4*26+B5*34+B6*36+B7*37+B8*39+
                          B9*22+B10*24+B11*26+B12*27+B13*31+B14*33+B15*36+B16*38+
                          C1*18+C2*19+C3*20+C4*21+C5*28+C6*30+C7*31+C8*33+
                          C9*18+C10*20+C11*21+C12*23+C13*26+C14*28+C15*30+C16*33+
                          D1*15+D2*16+D3*16+D4*17+D5*23+D6*24+D7*26+D8*27+
                          D9*15+D10*17+D11*18+D12*19+D13*22+D14*24+D15*26+D16*28+
                          E1*20+E2*21+E3*22+E4*24+E5*33+E6*35+E7*37+E8*39+
                          E9*20+E10*23+E11*25+E12*27+E13*31+E14*33+E15*36+E16*40+
                          F1*16+F2*17+F3*18+F4*19+F5*27+F6*29+F7*30+F8*32+
                          F9*17+F10*19+F11*20+F12*22+F13*25+F14*28+F15*31+F16*33+
                          G1*12+G2*13+G3*14+G4*15+G5*22+G6*23+G7*25+G8*26+
                          G9*14+G10*15+G11*17+G12*18+G13*21+G14*23+G15*25+G16*28+
                          H1*10+H2*11+H3*11+H4*12+H5*17+H6*18+H7*20+H8*21+
                          H9*11+H10*12+H11*14+H12*15+H13*17+H14*19+H15*21+H16*23+
                          I1*14+I2*15+I3*17+I4*18+I5*26+I6*28+I7*31+I8*33+
                          I9*16+I10*18+I11*20+I12*23+I13*25+I14*28+I15*32+I16*35+
                          J1*11+J2*12+J3*13+J4*14+J5*21+J6*23+J7*24+J8*26+
                          J9*13+J10*14+J11*16+J12*18+J13*21+J14*23+J15*26+J16*29+
                          K1*8+K2*9+K3*10+K4*11+K5*16+K6*18+K7*19+K8*21+
                          K9*10+K10*11+K11*13+K12*15+K13*17+K14*19+K15*21+K16*24+
                          L1*7+L2*7+L3*8+L4*9+L5*13+L6*14+L7*15+L8*16+
                          L9*8+L10*9+L11*10+L12*12+L13*13+L14*15+L15*17+L16*19+
                          M1*10+M2*11+M3*12+M4*14+M5*21+M6*23+M7*25+M8*28+
                          M9*12+M10*14+M11*16+M12*19+M13*21+M14*24+M15*28+M16*31+
                          N1*8+N2*9+N3*9+N4*11+N5*16+N6*18+N7*19+N8*22+
                          N9*10+N10*11+N11*13+N12*15+N13*17+N14*19+N15*22+N16*25+
                          O1*6+O2*6+O3*7+O4*8+O5*12+O6*13+O7*15+O8*17+
                          O9*7+O10*9+O11*10+O12*12+O13*13+O14*15+O15*17+O16*20+
                          P1*4+P2*5+P3*5+P4*6+P5*9+P6*10+P7*11+P8*13+
                          P9*6+P10*7+P11*8+P12*9+P13*10+P14*12+P15*14+P16*16+
                          Q1*7+Q2*8+Q3*9+Q4*10+Q5*16+Q6*18+Q7*21+Q8*23+
                          Q9*9+Q10*11+Q11*13+Q12*16+Q13*17+Q14*20+Q15*24+Q16*28+
                          R1*5+R2*6+R3*7+R4*8+R5*12+R6*14+R7*15+R8*17+
                          R9*7+R10*8+R11*10+R12*12+R13*13+R14*16+R15*18+R16*22+
                          S1*4+S2*4+S3*5+S4*6+S5*9+S6*10+S7*12+S8*13+
                          S9*5+S10*6+S11*8+S12*9+S13*10+S14*12+S15*14+S16*17+
                          T1*3+T2*3+T3*4+T4*4+T5*7+T6*8+T7*9+T8*10+
                          T9*4+T10*5+T11*6+T12*7+T13*8+T14*9+T15*11+T16*13+
                          U1*5+U2*6+U3*7+U4*8+U5*13+U6*15+U7*17+U8*19+
                          U9*7+U10*9+U11*11+U12*13+U13*14+U14*17+U15*20+U16*24+
                          V1*4+V2*4+V3*5+V4*6+V5*9+V6*11+V7*12+V8*14+
                          V9*5+V10*6+V11*8+V12*10+V13*11+V14*13+V15*16+V16*19+
                          W1*3+W2*3+W3*3+W4*4+W5*7+W6*8+W7*9+W8*10+
                          W9*4+W10*5+W11*6+W12*7+W13*8+W14*10+W15*12+W16*14+
                          X1*2+X2*2+X3*2+X4*3+X5*5+X6*6+X7*6+X8*7+
                          X9*3+X10*4+X11*4+X12*5+X13*6+X14*7+X15*9+X16*11)
  }

  # ESC Score value [%]



  return(ESCdata$Score)
}
