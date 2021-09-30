#' Calculate ESC-Score 2016 Table Version
#'
#' @description This function takes necessary parameters to calculate the ESC-Score 2016 Table Version for high and low risk
#'
#' @param totchol a numeric vector; Cholesterol values given in mg/dL or mmol/L. If unit is mg/dL set  the argument mmol to FALSE
#' @param sex a numeric vector indicating the sex of the person. Values: "female" = 1, "male" = 0
#' @param age a numeric vector with the age of persons given as years
#' @param sbp a numeric vector with the systolic blood pressure of persons given as mmHg
#' @param smoker a numeric vector. Smoker = 1, non-smoker = 0. A smoker was defined as current self-reported smoker.
#' @param mmol logical. Is Cholesterol given as mmol/L (TRUE) or mg/dL (FALSE).
#' @param risk logical. Choose if which risk chart is used for calculation
#' @usage ESC_Score_2016_table(totchol, sex, age, sbp, smoker, risk = c("low","high"), mmol = FALSE)
#' @return A vector of calculated risks of persons.
#' @details The SCORE risk assessment is derived from a large dataset of prospective European studies and predicts fatal atherosclerotic CVD events over a ten year period.
#'This risk estimation is based on the following risk factors: sex, age, smoker, systolic blood pressure and total cholesterol.
#'The threshold for high risk based on fatal cardiovascular events is defined as "higher than 5%", instead of the previous "higher than 20%" using a composite coronary endpoint.
#'This SCORE model has been calibrated according to each European country’s mortality statistics. In other words, if used on the entire population aged 40-65, it will predict the exact number of fatal CVD-events that eventually will occur after 10 years.
#'The relative risk chart may be used to show younger people at low total risk that, relative to others in their age group, their risk may be many times higher than necessary. This may help to motivate decisions about avoidance of smoking, healthy nutrition and exercise, as well as flagging those who may become candidates for medication. This chart refers to relative risk, not percentage risk.
#'You can read more about the SCORE project in European Heart Journal, 2003, 24; 987-1003.
#' @references
#' Piepoli MF,et al. 2016 European Guidelines on cardiovascular disease prevention in clinical practice: The Sixth Joint Task Force of the European Society of Cardiology and Other Societies on Cardiovascular Disease Prevention in Clinical Practice (constituted by representatives of 10 societies and by invited experts):
#' Developed with the special contribution of the European Association for Cardiovascular Prevention & Rehabilitation (EACPR).
#' Eur J Prev Cardiol. 2016 Jul;23(11):NP1-NP96. doi: 10.1177/2047487316653709. Epub 2016 Jun 27. PMID: 27353126.
#' @export
ESC_Score_2016_table <- function(totchol, sex, age, sbp, smoker, risk = c("low","high"), mmol = FALSE) {


  ESCdata <- data.frame(age = age, totchol = totchol, sex = sex, sbp = sbp, smoker = smoker)

  ESCdata$Score <- NA

  ## defining groups
  sex
  female <- (ifelse(ESCdata$sex == 'female', 1, 0))

  # SBP over 170
  SBP_4 <- (ifelse(ESCdata$sbp > 170, 1, 0))
  # SBP 150-169
  SBP_3 <- (ifelse(150 < ESCdata$sbp & ESCdata$sbp <= 170, 1, 0))
  # SBP 130-149
  SBP_2 <- (ifelse(130 < ESCdata$sbp & ESCdata$sbp <= 150, 1, 0))
  # SBP under 130
  SBP_1 <- (ifelse(ESCdata$sbp <= 130, 1, 0))

  #age
  age_5 <- (ifelse(ESCdata$age <= 65 & ESCdata$age >= 62.5, 1, 0))
  age_4 <- (ifelse(ESCdata$age < 62.5 & ESCdata$age >= 57.5, 1, 0))
  age_3 <- (ifelse(ESCdata$age < 57.5 & ESCdata$age >= 52.5, 1, 0))
  age_2 <- (ifelse(ESCdata$age < 52.5 & ESCdata$age >= 45, 1, 0))
  age_1 <- (ifelse(ESCdata$age < 45 & ESCdata$age >= 40, 1, 0))

  if(mmol == TRUE){
    #total cholesterol
    chol_5 <- (ifelse((ESCdata$totchol) >= 7.5, 1, 0))
    chol_4 <- (ifelse(6.5 <= (ESCdata$totchol) & (ESCdata$totchol) < 7.5, 1, 0))
    chol_3 <- (ifelse(5.5 <= (ESCdata$totchol) & (ESCdata$totchol) < 6.5, 1, 0))
    chol_2 <- (ifelse(4.5 <= (ESCdata$totchol) & (ESCdata$totchol) < 5.5, 1, 0))
    chol_1 <- (ifelse((ESCdata$totchol) < 4.5, 1, 0))
  }

  if(mmol == FALSE){
    #total cholesterol
    chol_5 <- (ifelse((ESCdata$totchol*0.0259) >= 7.5, 1, 0))
    chol_4 <- (ifelse(6.5 <= (ESCdata$totchol*0.0259) & (ESCdata$totchol*0.0259) < 7.5, 1, 0))
    chol_3 <- (ifelse(5.5 <= (ESCdata$totchol*0.0259) & (ESCdata$totchol*0.0259) < 6.5, 1, 0))
    chol_2 <- (ifelse(4.5 <= (ESCdata$totchol*0.0259) & (ESCdata$totchol*0.0259) < 5.5, 1, 0))
    chol_1 <- (ifelse((ESCdata$totchol*0.0259) < 4.5, 1, 0))
  }

  #smoker
  smoker <- (ifelse(ESCdata$smoker == '1', 1, 0))

  ## A - D --> age Group 65
  #A --> line 1
  A1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  A6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  A11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  A16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #B --> line 2
  B1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  B6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  B11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  B16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #C --> line 3
  C1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  C6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  C11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  C16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #D --> line 4
  D1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  D6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  D11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  D16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  ## E - H --> age Group 60
  #E --> line 5
  E1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  E6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  E11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  E16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #F --> line 6
  F1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  F6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  F11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  F16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #G --> line 7
  G1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  G6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  G11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  G16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #H --> line 8
  H1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  H6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  H11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  H16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  ## I - L --> age Group 55
  #I --> line 9
  I1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  I6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  I11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  I16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #J --> line 10
  J1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  J6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  J11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  J16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #K --> line 11
  K1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  K6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  K11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  K16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #L --> line 12
  L1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  L6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  L11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  L16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  ## M - P --> age Group 50
  #M --> line 13
  M1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  M5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  M6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  M10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  M11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  M15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  M16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  M20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #N --> line 14
  N1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  N5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  N6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  N10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  N11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  N15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  N16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  N20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #O --> line 15
  O1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  O5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  O6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  O10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  O11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  O15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  O16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  O20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #P --> line 16
  P1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  P5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  P6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  P10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  P11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  P15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  P16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  P20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))


  ## Q - T --> age Group 40
  #Q --> line 17
  Q1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  Q2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  Q3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  Q4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  Q5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  Q6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  Q7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  Q8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  Q9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  Q10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  Q11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  Q12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  Q13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  Q14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  Q15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  Q16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  Q17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  Q18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  Q19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  Q20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #R --> line 18
  R1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  R2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  R3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  R4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  R5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  R6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  R7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  R8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  R9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  R10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  R11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  R12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  R13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  R14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  R15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  R16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  R17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  R18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  R19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  R20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #S --> line 19
  S1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  S2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  S3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  S4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  S5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  S6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  S7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  S8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  S9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  S10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  S11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  S12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  S13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  S14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  S15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  S16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  S17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  S18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  S19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  S20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #P --> line 20
  T1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  T2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  T3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  T4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  T5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  T6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  T7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  T8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  T9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  T10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  T11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  T12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  T13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  T14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  T15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  T16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  T17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  T18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  T19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  T20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  # calculation of ESC Score values
  if(risk == "low") {
  ESCdata$Score   <- (A1*4+A2*5+A3*6+A4*6+A5*7+A6*9+A7*9+A8*11+A9*12+A10*14+
                                     A11*8+A12*9+A13*10+A14*12+A15*14+A16*15+A17*17+A18*20+A19*23+A20*26+
                                     B1*3+B2*3+B3*4+B4*4+B5*5+B6*6+B7*6+B8*7+B9*8+B10*10+
                                     B11*5+B12*6+B13*7+B14*8+B15*10+B16*10+B17*12+B18*14+B19*16+B20*19+
                                     C1*2+C2*2+C3*2+C4*3+C5*3+C6*4+C7*4+C8*5+C9*6+C10*7+
                                     C11*4+C12*4+C13*5+C14*6+C15*7+C16*7+C17*8+C18*9+C19*11+C20*13+
                                     D1*1+D2*1+D3*2+D4*2+D5*2+D6*3+D7*3+D8*3+D9*4+D10*4+
                                     D11*2+D12*3+D13*3+D14*4+D15*5+D16*5+D17*5+D18*6+D19*8+D20*9+
                                     E1*3+E2*3+E3*3+E4*4+E5*4+E6*5+E7*5+E8*6+E9*7+E10*8+
                                     E11*5+E12*6+E13*7+E14*8+E15*9+E16*10+E17*11+E18*13+E19*15+E20*18+
                                     F1*2+F2*2+F3*2+F4*2+F5*3+F6*3+F7*4+F8*4+F9*5+F10*5+
                                     F11*3+F12*4+F13*5+F14*5+F15*6+F16*7+F17*8+F18*9+F19*11+F20*13+
                                     G1*1+G2*1+G3*1+G4*2+G5*2+G6*2+G7*2+G8*3+G9*3+G10*4+
                                     G11*2+G12*3+G13*3+G14*4+G15*4+G16*5+G17*5+G18*6+G19*7+G20*9+
                                     H1*1+H2*1+H3*1+H4*1+H5*1+H6*1+H7*2+H8*2+H9*2+H10*3+
                                     H11*2+H12*2+H13*2+H14*3+H15*3+H16*3+H17*4+H18*4+H19*5+H20*6+
                                     I1*1+I2*1+I3*2+I4*2+I5*2+I6*3+I7*3+I8*3+I9*4+I10*4+
                                     I11*3+I12*4+I13*4+I14*5+I15*6+I16*6+I17*7+I18*8+I19*10+I20*12+
                                     J1*1+J2*1+J3*1+J4*1+J5*1+J6*2+J7*2+J8*2+J9*3+J10*3+
                                     J11*2+J12*2+J13*3+J14*3+J15*4+J16*4+J17*5+J18*6+J19*7+J20*8+
                                     K1*1+K2*1+K3*1+K4*1+K5*1+K6*1+K7*1+K8*1+K9*2+K10*2+
                                     K11*1+K12*2+K13*2+K14*2+K15*3+K16*3+K17*3+K18*4+K19*5+K20*6+
                                     L1*0+L2*0+L3*1+L4*1+L5*1+L6*1+L7*1+L8*1+L9*1+L10*1+
                                     L11*1+L12*1+L13*1+L14*2+L15*2+L16*2+L17*2+L18*3+L19*3+L20*4+
                                     M1*1+M2*1+M3*1+M4*1+M5*1+M6*1+M7*1+M8*2+M9*2+M10*2+
                                     M11*2+M12*2+M13*3+M14*3+M15*4+M16*4+M17*4+M18*5+M19*6+M20*7+
                                     N1*0+N2*0+N3*1+N4*1+N5*1+N6*1+N7*1+N8*1+N9*1+N10*1+
                                     N11*1+N12*1+N13*2+N14*2+N15*2+N16*2+N17*3+N18*3+N19*4+N20*5+
                                     O1*0+O2*0+O3*0+O4*0+O5*0+O6*1+O7*1+O8*1+O9*1+O10*1+
                                     O11*1+O12*1+O13*1+O14*1+O15*2+O16*2+O17*2+O18*2+O19*3+O20*3+
                                     P1*0+P2*0+P3*0+P4*0+P5*0+P6*0+P7*0+P8*0+P9*1+P10*1+
                                     P11*1+P12*1+P13*1+P14*1+P15*1+P16*1+P17*1+P18*2+P19*2+P20*2+
                                     Q1*0+Q2*0+Q3*0+Q4*0+Q5*0+Q6*0+Q7*0+Q8*0+Q9*0+Q10*0+
                                     Q11*0+Q12*1+Q13*1+Q14*1+Q15*1+Q16*1+Q17*1+Q18*1+Q19*2+Q20*2+
                                     R1*0+R2*0+R3*0+R4*0+R5*0+R6*0+R7*0+R8*0+R9*0+R10*0+
                                     R11*0+R12*0+R13*0+R14*1+R15*1+R16*1+R17*1+R18*1+R19*1+R20*1+
                                     S1*0+S2*0+S3*0+S4*0+S5*0+S6*0+S7*0+S8*0+S9*0+S10*0+
                                     S11*0+S12*0+S13*0+S14*0+S15*0+S16*0+S17*1+S18*1+S19*1+S20*1+
                                     T1*0+T2*0+T3*0+T4*0+T5*0+T6*0+T7*0+T8*0+T9*0+T10*0+
                                     T11*0+T12*0+T13*0+T14*0+T15*0+T16*0+T17*0+T18*0+T19*1+T20*1)
  }

  if(risk == "high") {
    ESCdata$Score <- (A1*7+A2*8+A3*9+A4*10+A5*12+A6*13+A7*15+A8*17+A9*19+A10*22+
                                             A11*14+A12*16+A13*19+A14*22+A15*26+A16*26+A17*30+A18*35+A19*41+A20*47+
                                             B1*5+B2*5+B3*6+B4*7+B5*8+B6*9+B7*10+B8*12+B9*13+B10*16+
                                             B11*9+B12*11+B13*13+B14*15+B15*16+B16*18+B17*21+B18*25+B19*29+B20*34+
                                             C1*3+C2*3+C3*4+C4*5+C5*6+C6*6+C7*7+C8*8+C9*9+C10*11+
                                             C11*6+C12*8+C13*9+C14*11+C15*13+C16*13+C17*15+C18*17+C19*20+C20*24+
                                             D1*2+D2*2+D3*3+D4*3+D5*4+D6*4+D7*5+D8*5+D9*6+D10*7+
                                             D11*4+D12*5+D13*6+D14*7+D15*9+D16*9+D17*10+D18*12+D19*14+D20*17+
                                             E1*4+E2*4+E3*5+E4*6+E5*7+E6*8+E7*9+E8*10+E9*11+E10*13+
                                             E11*9+E12*11+E13*13+E14*15+E15*18+E16*18+E17*21+E18*24+E19*28+E20*33+
                                             F1*3+F2*3+F3*3+F4*4+F5*5+F6*5+F7*6+F8*7+F9*8+F10*9+
                                             F11*6+F12*7+F13*9+F14*10+F15*12+F16*12+F17*14+F18*17+F19*20+F20*24+
                                             G1*2+G2*2+G3*2+G4*3+G5*3+G6*3+G7*4+G8*5+G9*5+G10*6+
                                             G11*4+G12*5+G13*6+G14*7+G15*9+G16*8+G17*10+G18*12+G19*14+G20*17+
                                             H1*1+H2*1+H3*2+H4*2+H5*2+H6*2+H7*3+H8*3+H9*4+H10*4+
                                             H11*3+H12*3+H13*4+H14*5+H15*6+H16*6+H17*7+H18*8+H19*10+H20*12+
                                             I1*2+I2*2+I3*3+I4*3+I5*4+I6*4+I7*5+I8*5+I9*6+I10*7+
                                             I11*6+I12*7+I13*8+I14*10+I15*12+I16*12+I17*13+I18*16+I19*19+I20*22+
                                             J1*1+J2*2+J3*2+J4*2+J5*3+J6*3+J7*3+J8*4+J9*4+J10*5+
                                             J11*4+J12*5+J13*6+J14*7+J15*8+J16*8+J17*9+J18*11+J19*13+J20*16+
                                             K1*1+K2*1+K3*1+K4*1+K5*2+K6*2+K7*2+K8*2+K9*3+K10*3+
                                             K11*3+K12*3+K13*4+K14*5+K15*6+K16*5+K17*6+K18*8+K19*9+K20*11+
                                             L1*1+L2*1+L3*1+L4*1+L5*1+L6*1+L7*1+L8*2+L9*2+L10*2+
                                             L11*2+L12*2+L13*3+L14*3+L15*4+L16*4+L17*4+L18*5+L19*6+L20*8+
                                             M1*1+M2*1+M3*1+M4*2+M5*2+M6*2+M7*2+M8*3+M9*3+M10*4+
                                             M11*4+M12*4+M13*5+M14*6+M15*7+M16*7+M17*8+M18*10+M19*12+M20*14+
                                             N1*1+N2*1+N3*1+N4*1+N5*1+N6*1+N7*2+N8*2+N9*2+N10*3+
                                             N11*2+N12*3+N13*3+N14*4+N15*5+N16*5+N17*6+N18*7+N19*8+N20*10+
                                             O1*0+O2*1+O3*1+O4*1+O5*1+O6*1+O7*1+O8*1+O9*1+O10*2+
                                             O11*2+O12*2+O13*2+O14*3+O15*3+O16*3+O17*4+O18*5+O19*6+O20*7+
                                             P1*0+P2*0+P3*1+P4*1+P5*1+P6*1+P7*1+P8*1+P9*1+P10*1+
                                             P11*1+P12*1+P13*2+P14*2+P15*2+P16*2+P17*3+P18*3+P19*4+P20*5+
                                             Q1*0+Q2*0+Q3*0+Q4*0+Q5*0+Q6*0+Q7*0+Q8*0+Q9*1+Q10*1+
                                             Q11*1+Q12*1+Q13*1+Q14*2+Q15*2+Q16*2+Q17*2+Q18*3+Q19*3+Q20*4+
                                             R1*0+R2*0+R3*0+R4*0+R5*0+R6*0+R7*0+R8*0+R9*0+R10*0+
                                             R11*1+R12*1+R13*1+R14*1+R15*1+R16*1+R17*2+R18*2+R19*2+R20*3+
                                             S1*0+S2*0+S3*0+S4*0+S5*0+S6*0+S7*0+S8*0+S9*0+S10*0+
                                             S11*0+S12*1+S13*1+S14*1+S15*1+S16*1+S17*1+S18*1+S19*2+S20*2+
                                             T1*0+T2*0+T3*0+T4*0+T5*0+T6*0+T7*0+T8*0+T9*0+T10*0+
                                             T11*0+T12*0+T13*1+T14*1+T15*1+T16*1+T17*1+T18*1+T19*1+T20*1)
  }

  # ESC Score value [%]



  return(ESCdata$Score)
}
