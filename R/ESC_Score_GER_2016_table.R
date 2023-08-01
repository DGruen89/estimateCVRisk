#' Calculate SCORE Germany 2016 table version
#'
#' @description This function takes necessary parameters to calculate the SCORE Germany 2016 table version for people aged 40 -65 years.
#'
#' @param sex a character vector indicating the sex of the person. Values: "female", "male"
#' @param age a numeric vector with the age of persons given in years
#' @param totchol a numeric vector; Cholesterol values given in mg/dL or mmol/L. If unit is mg/dL set  the argument mmol to FALSE
#' @param sbp a numeric vector with the systolic blood pressure of persons given as mmHg
#' @param smoker a numeric vector. Smoker = 1, non-smoker = 0. A smoker was defined as current self-reported smoker.
#' @param mmol is Cholesterol given as mmol/L; logical \[TRUE|FASLE\]
#' @usage ESC_Score_GER_2016_table(sex, age, totchol, sbp, smoker, mmol = FALSE)
#' @return A vector of the calculated risk per record.
#' @details Abstract:\cr
#' Estimation of absolute risk of cardiovascular disease (CVD), preferably with population-specific risk charts, has become a cornerstone of CVD primary prevention.
#' Regular recalibration of risk charts may be necessary due to decreasing CVD rates and CVD risk factor levels. The SCORE risk charts for fatal CVD risk assessment were first calibrated
#' for Ger- many with 1998 risk factor level data and 1999 mortality statistics. We present an update of these risk charts based on the SCORE methodology including estimates of relative
#' risks from SCORE, risk factor levels from the German Health Interview and Examination Survey for Adults 2008–11 (DEGS1) and official mortality statistics from 2012. Competing risks methods
#' were applied and estimates were independently validated. Updated risk charts were calculated based on cholesterol, smoking, systolic blood pressure risk factor levels, sex and 5-year age-groups.
#' The absolute 10-year risk estimates of fatal CVD were lower according to the updated risk charts compared to the first calibration for Germany. In a nationwide sample of 3062 adults
#' aged 40–65 years free of major CVD from DEGS1, the mean 10-year risk of fatal CVD estimated by the updated charts was lower by 29% and the estimated proportion of high
#' risk people (10-year risk > = 5%) by 50% compared to the older risk charts. This recalibration shows a need for regular updates of risk charts according to changes in mortality and
#' risk factor levels in order to sustain the identification of people with a high CVD risk.
#' @references
#' Rücker V, Keil U, Fitzgerald AP, Malzahn U, Prugger C, Ertl G, Heuschmann PU, Neuhauser H.
#' Predicting 10-Year Risk of Fatal Cardiovascular Disease in Germany: An Update Based on the SCORE-Deutschland Risk Charts.
#' PLoS One. 2016 Sep 9;11(9):e0162188. doi: 10.1371/journal.pone.0162188. PMID: 27612145; PMCID: PMC5017762.
#' @export
ESC_Score_GER_2016_table <- function(sex, age, totchol, sbp, smoker, mmol = FALSE) {

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

  if (any(age < 40) | any(age > 65) | any(is.na(age))) {
    warning("Some values are outside the optimal age range (40-65 years). Risk calculation can thus become less accurate.")
  }

  ESCdata <- data.frame(age = age, totchol = totchol, sex = sex, sbp = sbp, smoker = smoker)

  ESCdata$Score <- NA

  ## defining groups
  #sex
  female <- (ifelse(ESCdata$sex == 'female', 1, 0))

  #SBP
  # SBP over 170
  SBP_4 <- (ifelse(ESCdata$sbp > 170, 1, 0))
  # SBP 150-169
  SBP_3 <- (ifelse(150< ESCdata$sbp & ESCdata$sbp <= 170, 1, 0))
  # SBP 130-149
  SBP_2 <- (ifelse(130< ESCdata$sbp & ESCdata$sbp <= 150, 1, 0))
  # SBP under 130
  SBP_1 <- (ifelse(ESCdata$sbp <= 130, 1, 0))

  #age
  age_6 <- (ifelse(ESCdata$age >= 65, 1, 0))
  age_5 <- (ifelse(60 <= ESCdata$age & ESCdata$age < 65, 1, 0))
  age_4 <- (ifelse(55 <= ESCdata$age & ESCdata$age < 60, 1, 0))
  age_3 <- (ifelse(50 <= ESCdata$age & ESCdata$age < 55, 1, 0))
  age_2 <- (ifelse(45 <= ESCdata$age & ESCdata$age < 50, 1, 0))
  age_1 <- (ifelse(ESCdata$age < 45, 1, 0))

  #total cholesterol
  #chol_5 <- (ifelse((ESCdata$totchol) >= 290, 1, 0))
  #chol_4 <- (ifelse(250 <= (ESCdata$totchol) & (ESCdata$totchol) < 290, 1, 0))
  #chol_3 <- (ifelse(210 <= (ESCdata$totchol) & (ESCdata$totchol) < 250, 1, 0))
  #chol_2 <- (ifelse(170 <= (ESCdata$totchol) & (ESCdata$totchol) < 210, 1, 0))
  #chol_1 <- (ifelse((ESCdata$totchol) < 170, 1, 0))


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

  smoker <- (ifelse(is.na(ESCdata$smoker), 0, ESCdata$smoker))
  #smoker2 <- (ifelse(ESCdata$smoker == '1', 1, 0)) ## NA's werden nicht auf 0 gesetzt

  ## A - D --> age > 65
  #A --> line 1
  A1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  A6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  A11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  A16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #B --> line 2
  B1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  B6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  B11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  B16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #C --> line 3
  C1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  C6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  C11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  C16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #D --> line 4
  D1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  D6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  D11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  D16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_6 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  ## E - H --> age 56 - 60
  #E --> line 5
  E1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  E6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  E11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  E16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #F --> line 6
  F1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  F6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  F11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  F16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #G --> line 7
  G1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  G6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  G11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  G16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #H --> line 8
  H1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  H6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  H11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  H16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_5 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  ## I - L --> age 53 - 57
  #I --> line 9
  I1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  I6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  I11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  I16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #J --> line 10
  J1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  J6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  J11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  J16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #K --> line 11
  K1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  K6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  K11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  K16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #L --> line 12
  L1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  L6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  L11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  L16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_4 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  ## M - P --> age 48 - 52
  #M --> line 13
  M1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  M5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  M6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  M10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  M11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  M15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  M16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  M17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  M18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  M19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  M20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #N --> line 14
  N1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  N5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  N6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  N10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  N11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  N15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  N16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  N17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  N18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  N19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  N20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #O --> line 15
  O1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  O5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  O6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  O10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  O11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  O15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  O16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  O17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  O18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  O19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  O20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #P --> line 16
  P1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  P5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  P6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  P10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  P11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  P15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  P16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  P17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  P18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  P19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  P20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))


  ## Q - T --> age < 47
  #Q --> line 17
  Q1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  Q2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  Q3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  Q4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  Q5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  Q6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  Q7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  Q8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  Q9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  Q10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  Q11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  Q12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  Q13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  Q14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  Q15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  Q16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  Q17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  Q18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  Q19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  Q20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #R --> line 18
  R1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  R2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  R3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  R4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  R5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  R6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  R7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  R8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  R9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  R10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  R11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  R12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  R13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  R14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  R15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  R16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  R17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  R18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  R19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  R20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #S --> line 19
  S1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  S2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  S3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  S4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  S5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  S6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  S7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  S8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  S9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  S10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  S11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  S12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  S13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  S14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  S15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  S16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  S17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  S18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  S19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  S20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #P --> line 20
  T1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  T2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  T3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  T4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  T5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  T6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  T7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  T8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  T9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  T10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  T11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  T12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  T13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  T14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  T15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  T16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  T17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  T18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  T19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  T20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  ## U - X --> age < 47
  #U --> line 21
  U1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  U2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  U3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  U4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  U5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  U6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  U7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  U8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  U9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  U10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  U11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  U12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  U13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  U14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  U15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  U16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  U17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  U18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  U19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  U20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #V --> line 22
  V1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  V2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  V3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  V4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  V5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  V6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  V7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  V8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  V9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  V10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  V11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  V12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  V13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  V14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  V15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  V16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  V17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  V18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  V19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  V20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #W --> line 23
  W1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  W2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  W3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  W4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  W5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  W6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  W7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  W8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  W9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  W10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  W11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  W12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  W13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  W14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  W15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  W16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  W17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  W18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  W19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  W20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #X --> line 24
  X1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  X2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  X3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  X4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  X5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  X6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  X7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  X8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  X9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  X10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  X11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  X12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  X13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  X14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  X15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  X16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  X17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  X18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  X19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  X20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))


  # calculation of ESC Score values
  ESCdata$ESC_Score_value_2016 <- (A1*4+A2*5+A3*6+A4*7+A5*8+A6*8+A7*9+A8*11+A9*13+A10*15+
                                     A11*9+A12*11+A13*13+A14*15+A15*18+A16*18+A17*21+A18*25+A19*29+A20*34+
                                     B1*3+B2*3+B3*4+B4*5+B5*5+B6*5+B7*7+B8*8+B9*9+B10*11+
                                     B11*7+B12*8+B13*9+B14*11+B15*13+B16*13+B17*15+B18*18+B19*21+B20*25+
                                     C1*2+C2*2+C3*3+C4*3+C5*4+C6*4+C7*5+C8*5+C9*6+C10*8+
                                     C11*5+C12*5+C13*6+C14*8+C15*9+C16*9+C17*11+C18*13+C19*15+C20*18+
                                     D1*1+D2*2+D3*2+D4*2+D5*3+D6*3+D7*3+D8*4+D9*4+D10*5+
                                     D11*3+D12*4+D13*4+D14*5+D15*6+D16*6+D17*8+D18*9+D19*11+D20*13+
                                     E1*2+E2*2+E3*3+E4*4+E5*4+E6*4+E7*5+E8*6+E9*7+E10*8+
                                     E11*6+E12*7+E13*8+E14*10+E15*11+E16*11+E17*13+E18*16+E19*19+E20*22+
                                     F1*1+F2*2+F3*2+F4*2+F5*3+F6*3+F7*4+F8*4+F9*5+F10*6+
                                     F11*4+F12*5+F13*6+F14*7+F15*8+F16*8+F17*9+F18*11+F19*13+F20*16+
                                     G1*1+G2*1+G3*1+G4*2+G5*2+G6*2+G7*2+G8*3+G9*3+G10*4+
                                     G11*3+G12*3+G13*4+G14*5+G15*6+G16*6+G17*7+G18*8+G19*9+G20*11+
                                     H1*1+H2*1+H3*1+H4*1+H5*1+H6*1+H7*2+H8*2+H9*2+H10*3+
                                     H11*2+H12*2+H13*3+H14*3+H15*4+H16*4+H17*5+H18*5+H19*7+H20*8+
                                     I1*1+I2*1+I3*2+I4*2+I5*2+I6*2+I7*3+I8*3+I9*4+I10*5+
                                     I11*3+I12*4+I13*5+I14*6+I15*7+I16*7+I17*8+I18*10+I19*12+I20*14+
                                     J1*1+J2*1+J3*1+J4*1+J5*2+J6*2+J7*2+J8*2+J9*3+J10*3+
                                     J11*2+J12*3+J13*3+J14*4+J15*5+J16*5+J17*6+J18*7+J19*8+J20*10+
                                     K1*1+K2*1+K3*1+K4*1+K5*1+K6*1+K7*1+K8*2+K9*2+K10*2+
                                     K11*2+K12*2+K13*2+K14*3+K15*3+K16*3+K17*4+K18*5+K19*6+K20*7+
                                     L1*0+L2*0+L3*1+L4*1+L5*1+L6*1+L7*1+L8*1+L9*1+L10*2+
                                     L11*1+L12*1+L13*2+L14*2+L15*2+L16*2+L17*3+L18*3+L19*4+L20*5+
                                     M1*1+M2*1+M3*1+M4*1+M5*1+M6*1+M7*2+M8*2+M9*2+M10*3+
                                     M11*2+M12*2+M13*3+M14*3+M15*4+M16*4+M17*4+M18*5+M19*6+M20*7+
                                     N1*0+N2*1+N3*1+N4*1+N5*1+N6*1+N7*1+N8*1+N9*2+N10*2+
                                     N11*1+N12*1+N13*2+N14*2+N15*3+N16*3+N17*3+N18*4+N19*4+N20*5+
                                     O1*0+O2*0+O3*0+O4*1+O5*1+O6*1+O7*1+O8*1+O9*1+O10*1+
                                     O11*1+O12*1+O13*1+O14*1+O15*2+O16*2+O17*2+O18*3+O19*3+O20*4+
                                     P1*0+P2*0+P3*0+P4*0+P5*0+P6*0+P7*1+P8*1+P9*1+P10*1+
                                     P11*1+P12*1+P13*1+P14*1+P15*1+P16*1+P17*1+P18*2+P19*2+P20*2+
                                     Q1*0+Q2*0+Q3*1+Q4*1+Q5*1+Q6*1+Q7*1+Q8*1+Q9*1+Q10*2+
                                     Q11*1+Q12*1+Q13*2+Q14*2+Q15*2+Q16*2+Q17*3+Q18*3+Q19*4+Q20*5+
                                     R1*0+R2*0+R3*0+R4*0+R5*1+R6*1+R7*1+R8*1+R9*1+R10*1+
                                     R11*1+R12*1+R13*1+R14*1+R15*2+R16*2+R17*2+R18*2+R19*3+R20*3+
                                     S1*0+S2*0+S3*0+S4*0+S5*0+S6*0+S7*0+S8*1+S9*1+S10*1+
                                     S11*1+S12*1+S13*1+S14*1+S15*1+S16*1+S17*1+S18*2+S19*2+S20*2+
                                     T1*0+T2*0+T3*0+T4*0+T5*0+T6*0+T7*0+T8*0+T9*0+T10*1+
                                     T11*0+T12*0+T13*1+T14*1+T15*1+T16*1+T17*1+T18*1+T19*1+T20*2+
                                     U1*0+U2*0+U3*0+U4*0+U5*0+U6*0+U7*1+U8*1+U9*1+U10*1+
                                     U11*1+U12*1+U13*1+U14*1+U15*1+U16*1+U17*1+U18*2+U19*2+U20*2+
                                     V1*0+V2*0+V3*0+V4*0+V5*0+V6*0+V7*0+V8*0+V9*1+V10*1+
                                     V11*0+V12*0+V13*1+V14*1+V15*1+V16*1+V17*1+V18*1+V19*1+V20*2+
                                     W1*0+W2*0+W3*0+W4*0+W5*0+W6*0+W7*0+W8*0+W9*0+W10*0+
                                     W11*0+W12*0+W13*0+W14*0+W15*1+W16*1+W17*1+W18*1+W19*1+W20*1+
                                     X1*0+X2*0+X3*0+X4*0+X5*0+X6*0+X7*0+X8*0+X9*0+X10*0+
                                     X11*0+X12*0+X13*0+X14*0+X15*0+X16*0+X17*0+X18*1+X19*1+X20*1)

  # ESC Score value [%]



  return(ESCdata$ESC_Score_value_2016)
}
