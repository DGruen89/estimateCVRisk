#' Calculate ESC-Score OP Table Version
#'
#' @description This function takes necessary parameters to calculate the ESC-Score Older People (OP) Table Version for high and low risk
#
#' @param sex a character vector indicating the sex of the person. Values: "female", "male"
#' @param age a numeric vector with the age of persons given as years
#' @param totchol a numeric vector; Cholesterol values given in mg/dL or mmol/L. If unit is mg/dL set  the argument mmol to FALSE
#' @param sbp a numeric vector with the systolic blood pressure of persons given as mmHg
#' @param smoker a numeric vector. Smoker = 1, non-smoker = 0. A smoker was defined as current self-reported smoker.
#' @param risk logical. Choose if which risk chart is used for calculation
#' @param mmol logical. Is Cholesterol given as mmol/L (TRUE) or mg/dL (FALSE).
#' @usage ESC_Score_OP_table(sex, age, totchol, sbp, smoker, risk = c("low","high"), mmol = FALSE)
#' @return A vector of the calculated risk per record.
#' @details Abstract: \cr
#' Aims Estimation of cardiovascular disease risk, using SCORE (Systematic Coronary Risk Evaluation) is recommended by European guidelines on cardiovascular disease prevention.
#' Risk estimation is inaccurate in older people. We hypothesized that this may be due to the assumption, inherent in current risk estimation systems, that risk factors function similarly
#' in all age groups. We aimed to derive and validate a risk estimation function, SCORE O.P., solely from data from individuals aged 65 years and older.
#' Methods and results 20,704 men and 20,121 women, aged 65 and over and without pre-existing coronary disease, from four representative, prospective studies of the general population were included.
#' These were Italian, Belgian and Danish studies (from original SCORE dataset) and the CONOR (Cohort of Norway) study. The variables which remained statistically significant
#' in Cox proportional hazards model and were included in the SCORE O.P. model were: age, total cholesterol, high-density lipoprotein cholesterol, systolic blood pressure, smoking status and diabetes.
#'SCORE O.P. showed good discrimination; area under receiver operator characteristic curve (AUROC) 0.74 (95% confidence interval: 0.73 to 0.75). Calibration was also reasonable,
#'Hosmer-Lemeshow goodness of fit test: 17.16 (men), 22.70 (women). Compared with the original SCORE function extrapolated to the ≥65 years age group discrimination improved,
#'p = 0.05 (men), p < 0.001 (women). Simple risk charts were constructed. On simulated external validation, performed using 10-fold cross validation, AUROC was 0.74 and predicted/observed ratio
#' was 1.02. Conclusion SCORE O.P. provides improved accuracy in risk estimation in older people and may reduce excessive use of medication in this vulnerable population.
#' @references
#' Cooney MT, et al. Cardiovascular risk estimation in older persons: SCORE O.P. Eur J Prev Cardiol. 2016 Jul;23(10):1093-103. doi: 10.1177/2047487315588390. Epub 2015 Jun 3. PMID: 26040999.
#' @export
ESC_Score_OP_table <- function(sex, age, totchol, sbp, smoker, risk = c("low","high"), mmol = FALSE) {

  risk <- match.arg(risk)

  if (risk != "low" & risk != "high"){
    stop("risk must be either 'low' or 'high'")
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

  ESCdata <- data.frame(age = age, totchol = totchol, sex = sex, sbp = sbp, smoker = smoker, mmol = FALSE)

  ESCdata$ESC_Score_value_OP <- 0

  ## defining groups
  ## sex
  female <- (ifelse(ESCdata$sex == 'female', 1, 0))

  # SBP over 170
  SBP_4 <- (ifelse(ESCdata$sbp > 170, 1, 0))
  # SBP 150-169
  SBP_3 <- (ifelse(150< ESCdata$sbp & ESCdata$sbp <= 170, 1, 0))
  # SBP 130-149
  SBP_2 <- (ifelse(130< ESCdata$sbp & ESCdata$sbp <= 150, 1, 0))
  # SBP under 130
  SBP_1 <- (ifelse(ESCdata$sbp <= 130, 1, 0))

  #age
  age_3 <- (ifelse(ESCdata$age <= 80 & ESCdata$age >= 72.5, 1, 0))
  age_2 <- (ifelse(ESCdata$age < 72.5 & ESCdata$age >= 67.5, 1, 0))
  age_1 <- (ifelse(ESCdata$age < 67.5 & ESCdata$age >= 65, 1, 0))

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

  ## A - D --> age Group 75
  #A --> line 1
  A1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  A6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  A11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  A16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  A17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  A18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  A19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  A20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #B --> line 2
  B1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  B6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  B11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  B16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  B17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  B18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  B19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  B20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #C --> line 3
  C1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  C6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  C11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  C16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  C17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  C18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  C19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  C20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #D --> line 4
  D1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  D6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  D11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  D16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  D17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  D18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  D19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  D20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_3 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  ## E - H --> age Group 70
  #E --> line 5
  E1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  E6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  E11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  E16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  E17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  E18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  E19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  E20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #F --> line 6
  F1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  F6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  F11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  F16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  F17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  F18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  F19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  F20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #G --> line 7
  G1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  G6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  G11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  G16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  G17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  G18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  G19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  G20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #H --> line 8
  H1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  H6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  H11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  H16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  H17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  H18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  H19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  H20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_2 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  ## I - L --> age Group 65
  #I --> line 9
  I1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  I6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  I11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  I16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  I17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  I18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  I19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  I20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_4 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #J --> line 10
  J1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  J6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  J11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  J16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  J17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  J18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  J19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  J20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_3 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #K --> line 11
  K1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  K6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  K11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  K16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  K17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  K18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  K19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  K20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_2 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  #L --> line 12
  L1 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L2 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L3 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L4 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L5 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  L6 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L7 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L8 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L9 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L10 <- ((ifelse(female == '1', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))

  L11 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L12 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L13 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L14 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L15 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '0', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))
  L16 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_1 == '1', 1, 0)))
  L17 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_2 == '1', 1, 0)))
  L18 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_3 == '1', 1, 0)))
  L19 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_4 == '1', 1, 0)))
  L20 <- ((ifelse(female == '0', 1, 0))*(ifelse(smoker == '1', 1, 0))*(ifelse(age_1 == '1', 1, 0))*(ifelse(SBP_1 == '1', 1, 0))*(ifelse(chol_5 == '1', 1, 0)))



  # calculation of ESC Score values
  if(risk == "low") {
    ESCdata$ESC_Score_value_OP   <- (A1*12+A2*12+A3*13+A4*13+A5*14+A6*19+A7*19+A8*20+A9*21+A10*22+
                                         A11*17+A12*18+A13*20+A14*22+A15*24+A16*28+A17*30+A18*32+A19*35+A20*39+
                                         B1*10+B2*11+B3*11+B4*12+B5*12+B6*16+B7*17+B8*18+B9*19+B10*20+
                                         B11*15+B12*16+B13*17+B14*19+B15*21+B16*25+B17*26+B18*29+B19*31+B20*35+
                                         C1*9+C2*9+C3*10+C4*10+C5*11+C6*14+C7*15+C8*16+C9*16+C10*17+
                                         C11*13+C12*14+C13*15+C14*17+C15*19+C16*22+C17*23+C18*25+C19*28+C20*31+
                                         D1*8+D2*8+D3*8+D4*9+D5*9+D6*12+D7*13+D8*14+D9*14+D10*15+
                                         D11*11+D12*12+D13*13+D14*15+D15*16+D16*19+D17*21+D18*22+D19*25+D20*27+
                                         E1*6+E2*6+E3*6+E4*6+E5*7+E6*9+E7*9+E8*10+E9*10+E10*11+
                                         E11*9+E12*10+E13*11+E14*12+E15*14+E16*16+E17*17+E18*19+E19*21+E20*23+
                                         F1*5+F2*5+F3*5+F4*6+F5*6+F6*8+F7*8+F8*9+F9*9+F10*10+
                                         F11*8+F12*9+F13*10+F14*11+F15*12+F16*14+F17*15+F18*16+F19*18+F20*20+
                                         G1*4+G2*4+G3*5+G4*5+G5*5+G6*7+G7*7+G8*8+G9*8+G10*8+
                                         G11*7+G12*8+G13*8+G14*9+G15*10+G16*12+G17*13+G18*14+G19*16+G20*18+
                                         H1*4+H2*4+H3*4+H4*4+H5*4+H6*6+H7*6+H8*7+H9*7+H10*7+
                                         H11*6+H12*7+H13*7+H14*8+H15*9+H16*10+H17*11+H18*13+H19*14+H20*16+
                                         I1*3+I2*3+I3*3+I4*3+I5*3+I6*4+I7*4+I8*5+I9*5+I10*5+
                                         I11*5+I12*5+I13*6+I14*7+I15*8+I16*9+I17*9+I18*10+I19*12+I20*13+
                                         J1*2+J2*2+J3*2+J4*3+J5*3+J6*4+J7*4+J8*4+J9*4+J10*5+
                                         J11*4+J12*5+J13*5+J14*6+J15*7+J16*7+J17*8+J18*9+J19*10+J20*11+
                                         K1*2+K2*2+K3*2+K4*2+K5*2+K6*3+K7*3+K8*4+K9*4+K10*4+
                                         K11*4+K12*4+K13*4+K14*5+K15*6+K16*6+K17*7+K18*8+K19*9+K20*10+
                                         L1*2+L2*2+L3*2+L4*2+L5*2+L6*3+L7*3+L8*3+L9*3+L10*3+
                                         L11*3+L12*4+L13*4+L14*4+L15*5+L16*6+L17*6+L18*7+L19*8+L20*9
                                         )
  }

  if(risk == "high") {
    ESCdata$ESC_Score_value_OP <- (A1*18+A2*19+A3*20+A4*21+A5*22+A6*28+A7*29+A8*31+A9*33+A10*35+
                                       A11*23+A12*26+A13*30+A14*33+A15*38+A16*38+A17*42+A18*46+A19*52+A20*57+
                                       B1*16+B2*16+B3*17+B4*18+B5*19+B6*25+B7*26+B8*28+B9*29+B10*31+
                                       B11*21+B12*23+B13*26+B14*30+B15*34+B16*34+B17*37+B18*42+B19*46+B20*52+
                                       C1*14+C2*14+C3*15+C4*16+C5*17+C6*22+C7*23+C8*24+C9*26+C10*27+
                                       C11*18+C12*20+C13*23+C14*26+C15*30+C16*30+C17*33+C18*37+C19*42+C20*47+
                                       D1*12+D2*13+D3*13+D4*14+D5*15+D6*19+D7*20+D8*22+D9*23+D10*24+
                                       D11*16+D12*18+D13*20+D14*23+D15*26+D16*26+D17*29+D18*33+D19*37+D20*42+
                                       E1*9+E2*9+E3*10+E4*10+E5*11+E6*14+E7*15+E8*16+E9*17+E10*18+
                                       E11*13+E12*15+E13*17+E14*20+E15*23+E16*22+E17*25+E18*29+E19*32+E20*37+
                                       F1*7+F2*8+F3*8+F4*9+F5*9+F6*12+F7*13+F8*14+F9*15+F10*16+
                                       F11*12+F12*13+F13*15+F14*17+F15*20+F16*20+F17*22+F18*25+F19*29+F20*33+
                                       G1*7+G2*7+G3*7+G4*8+G5*8+G6*11+G7*11+G8*12+G9*13+G10*14+
                                       G11*10+G12*11+G13*13+G14*15+G15*17+G16*17+G17*19+G18*22+G19*25+G20*29+
                                       H1*6+H2*6+H3*6+H4*7+H5*7+H6*9+H7*10+H8*11+H9*11+H10*12+
                                       H11*9+H12*10+H13*11+H14*13+H15*15+H16*15+H17*17+H18*19+H19*22+H20*25+
                                       I1*4+I2*4+I3*5+I4*5+I5*5+I6*7+I7*7+I8*8+I9*8+I10*9+
                                       I11*7+I12*8+I13*10+I14*11+I15*13+I16*13+I17*14+I18*17+I19*19+I20*22+
                                       J1*4+J2*4+J3*4+J4*4+J5*4+J6*6+J7*6+J8*7+J9*7+J10*8+
                                       J11*6+J12*7+J13*8+J14*10+J15*11+J16*11+J17*13+J18*14+J19*17+J20*19+
                                       K1*3+K2*3+K3*3+K4*4+K5*4+K6*5+K7*5+K8*6+K9*6+K10*7+
                                       K11*6+K12*6+K13*7+K14*8+K15*10+K16*10+K17*11+K18*13+K19*15+K20*17+
                                       L1*3+L2*3+L3*3+L4*3+L5*3+L6*4+L7*5+L8*5+L9*5+L10*6+
                                       L11*5+L12*5+L13*6+L14*7+L15*9+L16*8+L17*10+L18*11+L19*13+L20*15
                                       )
  }

  # ESC Score value [%]



  return(ESCdata$ESC_Score_value_OP)
}
