#' Calculate Procam Score (Version 2002 and 2007)
#'
#' @description This function takes necessary parameters to calculate the PROCAM-Risk-Score table version for the Risk of acute coronary events based on the 10-year follow-up of the prospective cardiovascular Münster (PROCAM) study.
#' Functions are available for PROCAM I (2002) and PROCAM II (2007) Score
#'
#' @param sex gender; categorical \[female|male\]
#' @param age age; integer \[years\]
#' @param ldl low-density lipoprotein; numeric \[mg/dl\]
#' @param hdl high-density lipoprotein; numeric \[mg/dl\]
#' @param sbp systolic blood pressure; numeric \[mmHg\]
#' @param triglycerides triglycerides; numeric \[mg/dl\]
#' @param smoker information on current self-reported smoking status; numeric \[1|0\]; ("1"=smoker;"0"=non-smoker)
#' @param diabetic diabetic status of individual; numeric \[1|0\]; ("1"=diabetic;"0"=non-diabetic)
#' @param famMI family history of premature myocardial infarction; numeric \[1|0\]; ("1"=yes;"0"=no)
#' @usage
#' procam_score_2002(age, ldl, hdl, sbp, triglycerides, smoker, diabetic, famMI)
#' procam_score_2007(sex, age, ldl, hdl, sbp, triglycerides, smoker, diabetic, famMI)
#' @return A vector of calculated risk per record in %. Procam_2002 returns a numeric vectore, while Procam_2007 returns a character vector
#' @aliases procam_score_2002 procam_score_2007
#' @details
#' The 2002-Model:
#' \itemize{
#'  \item{based on Cox-Model}
#'  \item{based on a German Kohort}
#'  \item{Score only precise for men 35-65}
#'  \item{Risk for women is 4 times lower then the risk of men}
#'  }
#'  The 2007-Model:
#'  \itemize{
#'   \item{based on Weibull-Model}
#'   \item{includes men an women with age range from 20 - 75 years}
#'  }
#' Abstract:\cr
#' Background: \cr
#' The absolute risk of an acute coronary event depends on the totality of risk factors exhibited by an individual, the so-called global risk profile.
#' Although several scoring schemes have been suggested to calculate this profile, many omit information on important variables such as family history of coronary heart disease or LDL cholesterol.\cr
#' Methods and Results:\cr
#' Based on 325 acute coronary events occurring within 10 years of follow-up among 5389 men 35 to 65 years of age at recruitment into the Prospective Cardiovascular Muenster (PROCAM) study,
#' we developed a Cox proportional hazards model using the following 8 independent risk variables, ranked in order of importance: age, LDL cholesterol, smoking, HDL cholesterol, systolic blood pressure,
#' family history of premature myocardial infarction, diabetes mellitus, and triglycerides. We then derived a simple point scoring system based on the beta-coefficients of this model.
#' The accuracy of this point scoring scheme was comparable to coronary event prediction when the continuous variables themselves were used.\cr
#' The scoring system accurately predicted observed coronary events with an area under the receiver-operating characteristics curve of 82.4% compared with 82.9% for the Cox model with continuous variables.
#' Conclusions:\cr
#' Our scoring system is a simple and accurate way of predicting global risk of myocardial infarction in clinical practice and will therefore allow more accurate targeting of preventive therapy.
#' @references
#' Assmann G, Cullen P, Schulte H. Simple scoring scheme for calculating the risk of acute coronary events based on the 10-year follow-up of the prospective cardiovascular Münster (PROCAM) study.
#' Circulation. 2002 Jan 22;105(3):310-5. doi: 10.1161/hc0302.102575. Erratum in: Circulation 2002 Feb 19;105(7):900. PMID: 11804985.
#' @import stats
#' @export
procam_score_2002 <- function(age, ldl, hdl, sbp, triglycerides, smoker, diabetic, famMI){

  if (!is.numeric(age) |  missing(age)) {
    stop("age must be a valid numeric value")
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

  if (any(!is.numeric(triglycerides)) & any(!is.na(triglycerides))) {
    stop("triglycerides must be a valid numeric value")
  }

  if (!is.numeric(smoker) | !all(smoker %in% c(0,1,NA)) | missing(smoker)) {
    stop("smoker must be either 0 (no) or 1 (yes)")
  }

  if (!is.numeric(diabetic) | !all(diabetic %in% c(0,1,NA)) | missing(diabetic)) {
    stop("diabetic must be either 0 (no) or 1 (yes)")
  }

  if (!is.numeric(famMI) | !all(famMI %in% c(0,1,NA)) | missing(famMI)) {
    stop("famMI must be either 0 (no) or 1 (yes)")
  }

  if (any(age < 35) | any(age > 65) | any(is.na(age))) {
    warning("Some values are outside the optimal age range (35-65 years). Risk calculation can thus become less accurate.")
  }

  if (any(is.na(hdl))) {
    warning("hdl contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(ldl))) {
    warning("ldl contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(sbp))) {
    warning("sbp contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(triglycerides))) {
    warning("triglycerides contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(smoker))) {
    warning("smoker contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(famMI))) {
    warning("famMI contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(diabetic))) {
    warning("diabetic contains NA's. This can greatly underestimate the risk for individuals")
  }



  data <- data.frame(age = age, ldl = ldl, hdl = hdl, triglycerides = triglycerides, smoker = smoker, diabetic = diabetic, famMI = famMI, sbp = sbp)

  data$score <- 0
  data$age <- round(data$age)
  data$ldl <- round(data$ldl)
  data$hdl <- round(data$hdl)
  data$triglycerides <- round(data$triglycerides)
  data$sbp <- round(data$sbp)


  ## Score Age
  data$score[data$age >= 35 & data$age <= 39 & !is.na(data$age)] <- data$score[data$age >= 35 & data$age <= 39 & !is.na(data$age)] + 0
  data$score[data$age >= 40 & data$age <= 44 & !is.na(data$age)] <- data$score[data$age >= 40 & data$age <= 44 & !is.na(data$age)] + 6
  data$score[data$age >= 45 & data$age <= 49 & !is.na(data$age)] <- data$score[data$age >= 45 & data$age <= 49 & !is.na(data$age)] + 11
  data$score[data$age >= 50 & data$age <= 54 & !is.na(data$age)] <- data$score[data$age >= 50 & data$age <= 54 & !is.na(data$age)] + 16
  data$score[data$age >= 55 & data$age <= 59 & !is.na(data$age)] <- data$score[data$age >= 55 & data$age <= 59 & !is.na(data$age)] + 21
  data$score[data$age > 60 & !is.na(data$age)] <- data$score[data$age >= 60 & !is.na(data$age)] + 26

  ## Score LDL
  data$score[data$ldl < 100 & !is.na(data$ldl)] <- data$score[data$ldl < 100 & !is.na(data$ldl)] + 0
  data$score[data$ldl >= 100 & data$ldl <= 129 & !is.na(data$ldl)] <- data$score[data$ldl >= 100 & data$ldl <= 129 & !is.na(data$ldl)] + 5
  data$score[data$ldl >= 130 & data$ldl <= 159 & !is.na(data$ldl)] <- data$score[data$ldl >= 130 & data$ldl <= 159 & !is.na(data$ldl)] + 10
  data$score[data$ldl >= 160 & data$ldl <= 189 & !is.na(data$ldl)] <- data$score[data$ldl >= 160 & data$ldl <= 189 & !is.na(data$ldl)] + 14
  data$score[data$ldl >= 190 & !is.na(data$ldl)] <- data$score[data$ldl >= 190 & !is.na(data$ldl)] + 20

  ## Score hdl
  data$score[data$hdl < 35 & !is.na(data$hdl)] <- data$score[data$hdl < 35 & !is.na(data$hdl)] + 11
  data$score[data$hdl >= 35 & data$hdl <= 44 & !is.na(data$hdl)] <- data$score[data$hdl >= 35 & data$hdl <= 44 & !is.na(data$hdl)] + 8
  data$score[data$hdl >= 45 & data$hdl <= 54 & !is.na(data$hdl)] <- data$score[data$hdl >= 45 & data$hdl <= 54 & !is.na(data$hdl)] + 5
  data$score[data$hdl >= 55 & !is.na(data$hdl)] <- data$score[data$hdl >= 55 & !is.na(data$hdl)] + 0

  ## Score triglycerides
  data$score[data$triglycerides < 100 & !is.na(data$triglycerides)] <- data$score[data$triglycerides < 100 & !is.na(data$triglycerides)] + 0
  data$score[data$triglycerides >= 100 & data$triglycerides <= 149 & !is.na(data$triglycerides)] <- data$score[data$triglycerides >= 100 & data$triglycerides <= 149 & !is.na(data$triglycerides)] + 2
  data$score[data$triglycerides >= 150 & data$triglycerides <= 199 & !is.na(data$triglycerides)] <- data$score[data$triglycerides >= 150 & data$triglycerides <= 199 & !is.na(data$triglycerides)] + 3
  data$score[data$triglycerides >= 200 & !is.na(data$triglycerides)] <- data$score[data$triglycerides >= 200 & !is.na(data$triglycerides)] + 4

  ## Score smoker
  data$score[data$smoker == 1 & !is.na(data$smoker)] <- data$score[data$smoker == 1 & !is.na(data$smoker)] + 8

  ## Score diabetic
  data$score[data$diabetic == 1 & !is.na(data$diabetic)] <- data$score[data$diabetic == 1 & !is.na(data$diabetic)] + 6

  ## Score famMI
  data$score[data$famMI == 1 & !is.na(data$famMI)] <- data$score[data$famMI == 1 & !is.na(data$famMI)] + 4

  ## Score sys BP
  data$score[data$sbp < 120 & !is.na(data$sbp)] <- data$score[data$sbp < 120 & !is.na(data$sbp)] + 0
  data$score[data$sbp >= 120 & data$sbp <= 129] <- data$score[data$sbp >= 120 & data$sbp <= 129 & !is.na(data$sbp)] + 2
  data$score[data$sbp >= 130 & data$sbp <= 139] <- data$score[data$sbp >= 130 & data$sbp <= 139 & !is.na(data$sbp)] + 3
  data$score[data$sbp >= 140 & data$sbp <= 159] <- data$score[data$sbp >= 140 & data$sbp <= 159 & !is.na(data$sbp)] + 5
  data$score[data$sbp >= 160 & !is.na(data$sbp)] <- data$score[data$sbp >= 160 & !is.na(data$sbp)] + 8


  data$score <- ifelse(data$score > 60, 60, data$score)
  data$score <- ifelse(data$score < 20, 20, data$score)

  ## 10 year Risk

  data$risk <- NA

  data$risk <- risktable_procam2002$risk[match(data$score, risktable_procam2002$points)]

  data$risk[!complete.cases(data$risk)] <- NA

  return(data$risk)

}
#' @export
procam_score_2007 <- function(sex, age, ldl, hdl, sbp, triglycerides, smoker, diabetic, famMI){

  if (!all(sex %in% c("male", "female")) | missing(sex)) {
    stop("sex must be either 'male' or 'female'")
  }

  if (!is.numeric(age) |  missing(age)) {
    stop("age must be a valid numeric value")
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

  if (any(!is.numeric(triglycerides)) & any(!is.na(triglycerides))) {
    stop("triglycerides must be a valid numeric value")
  }

  if (!is.numeric(smoker) | !all(smoker %in% c(0,1,NA)) | missing(smoker)) {
    stop("smoker must be either 0 (no) or 1 (yes)")
  }

  if (!is.numeric(diabetic) | !all(diabetic %in% c(0,1,NA)) | missing(diabetic)) {
    stop("diabetic must be either 0 (no) or 1 (yes)")
  }

  if (!is.numeric(famMI) | !all(famMI %in% c(0,1,NA)) | missing(famMI)) {
    stop("diabetic must be either 0 (no) or 1 (yes)")
  }

  if (any(age < 20) | any(age > 75) | any(is.na(age))) {
    warning("Some values are outside the optimal age range (20-75 years). Risk cannot be calculated exactly.")
  }

  if (any(is.na(hdl))) {
    warning("hdl contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(ldl))) {
    warning("ldl contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(sbp))) {
    warning("sbp contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(triglycerides))) {
    warning("triglycerides contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(smoker))) {
    warning("smoker contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(famMI))) {
    warning("famMI contains NA's. This can greatly underestimate the risk for individuals")
  }

  if (any(is.na(diabetic))) {
    warning("diabetic contains NA's. This can greatly underestimate the risk for individuals")
  }


  data <- data.frame(age = age, ldl = ldl, hdl = hdl, triglycerides = triglycerides, smoker = smoker, diabetic = diabetic, famMI = famMI, sbp = sbp, sex = sex)

  data$score <- 0
  data$age <- round(data$age)
  data$age[data$age > 75] <- 75
  data$age[data$age < 25 & data$sex == "male"] <- "20-24"
  data$age[data$age < 34 & data$sex == "female"] <- "20-33"
  data$ldl <- round(data$ldl)
  data$hdl <- round(data$hdl)
  data$triglycerides <- round(data$triglycerides)
  data$sbp <- round(data$sbp)



  ## Score LDL
  data$score[data$ldl <= 100 & !is.na(data$ldl)] <- data$score[data$ldl <= 100 & !is.na(data$ldl)] + 0
  data$score[data$ldl > 100 & data$ldl <= 105 & !is.na(data$ldl)] <- data$score[data$ldl > 100 & data$ldl <= 105 & !is.na(data$ldl)] + 1
  data$score[data$ldl > 105 & data$ldl <= 110 & !is.na(data$ldl)] <- data$score[data$ldl > 105 & data$ldl <= 110 & !is.na(data$ldl)] + 2
  data$score[data$ldl > 110 & data$ldl <= 115 & !is.na(data$ldl)] <- data$score[data$ldl > 110 & data$ldl <= 115 & !is.na(data$ldl)] + 3
  data$score[data$ldl > 115 & data$ldl <= 120 & !is.na(data$ldl)] <- data$score[data$ldl > 115 & data$ldl <= 120 & !is.na(data$ldl)] + 4
  data$score[data$ldl > 120 & data$ldl <= 125 & !is.na(data$ldl)] <- data$score[data$ldl > 120 & data$ldl <= 125 & !is.na(data$ldl)] + 5
  data$score[data$ldl > 125 & data$ldl <= 130 & !is.na(data$ldl)] <- data$score[data$ldl > 125 & data$ldl <= 130 & !is.na(data$ldl)] + 6
  data$score[data$ldl > 130 & data$ldl <= 135 & !is.na(data$ldl)] <- data$score[data$ldl > 130 & data$ldl <= 135 & !is.na(data$ldl)] + 7
  data$score[data$ldl > 135 & data$ldl <= 140 & !is.na(data$ldl)] <- data$score[data$ldl > 135 & data$ldl <= 140 & !is.na(data$ldl)] + 8
  data$score[data$ldl > 140 & data$ldl <= 145 & !is.na(data$ldl)] <- data$score[data$ldl > 140 & data$ldl <= 145 & !is.na(data$ldl)] + 9
  data$score[data$ldl > 145 & data$ldl <= 150 & !is.na(data$ldl)] <- data$score[data$ldl > 145 & data$ldl <= 150 & !is.na(data$ldl)] + 10
  data$score[data$ldl > 150 & data$ldl <= 155 & !is.na(data$ldl)] <- data$score[data$ldl > 150 & data$ldl <= 155 & !is.na(data$ldl)] + 11
  data$score[data$ldl > 155 & data$ldl <= 160 & !is.na(data$ldl)] <- data$score[data$ldl > 155 & data$ldl <= 160 & !is.na(data$ldl)] + 12
  data$score[data$ldl > 160 & data$ldl <= 165 & !is.na(data$ldl)] <- data$score[data$ldl > 160 & data$ldl <= 165 & !is.na(data$ldl)] + 13
  data$score[data$ldl > 165 & data$ldl <= 170 & !is.na(data$ldl)] <- data$score[data$ldl > 165 & data$ldl <= 170 & !is.na(data$ldl)] + 14
  data$score[data$ldl > 170 & data$ldl <= 175 & !is.na(data$ldl)] <- data$score[data$ldl > 170 & data$ldl <= 175 & !is.na(data$ldl)] + 15
  data$score[data$ldl > 175 & data$ldl <= 180 & !is.na(data$ldl)] <- data$score[data$ldl > 175 & data$ldl <= 180 & !is.na(data$ldl)] + 16
  data$score[data$ldl > 180 & data$ldl <= 185 & !is.na(data$ldl)] <- data$score[data$ldl > 180 & data$ldl <= 185 & !is.na(data$ldl)] + 17
  data$score[data$ldl > 185 & data$ldl <= 190 & !is.na(data$ldl)] <- data$score[data$ldl > 185 & data$ldl <= 190 & !is.na(data$ldl)] + 18
  data$score[data$ldl > 190 & data$ldl <= 195 & !is.na(data$ldl)] <- data$score[data$ldl > 190 & data$ldl <= 195 & !is.na(data$ldl)] + 19
  data$score[data$ldl > 195 & !is.na(data$ldl)] <- data$score[data$ldl > 195 & !is.na(data$ldl)] + 20

  ## Score hdl
  data$score[data$hdl <= 35 & !is.na(data$hdl)] <- data$score[data$hdl <= 35 & !is.na(data$hdl)] + 11
  data$score[data$hdl > 35 & data$hdl <= 37 & !is.na(data$hdl)] <- data$score[data$hdl > 35 & data$hdl <= 37 & !is.na(data$hdl)] + 10
  data$score[data$hdl > 37 & data$hdl <= 39 & !is.na(data$hdl)] <- data$score[data$hdl > 37 & data$hdl <= 39 & !is.na(data$hdl)] + 9
  data$score[data$hdl > 39 & data$hdl <= 41 & !is.na(data$hdl)] <- data$score[data$hdl > 39 & data$hdl <= 41 & !is.na(data$hdl)] + 8
  data$score[data$hdl > 41 & data$hdl <= 43 & !is.na(data$hdl)] <- data$score[data$hdl > 41 & data$hdl <= 43 & !is.na(data$hdl)] + 7
  data$score[data$hdl > 43 & data$hdl <= 45 & !is.na(data$hdl)] <- data$score[data$hdl > 43 & data$hdl <= 45 & !is.na(data$hdl)] + 6
  data$score[data$hdl > 45 & data$hdl <= 47 & !is.na(data$hdl)] <- data$score[data$hdl > 45 & data$hdl <= 47 & !is.na(data$hdl)] + 5
  data$score[data$hdl > 47 & data$hdl <= 49 & !is.na(data$hdl)] <- data$score[data$hdl > 47 & data$hdl <= 49 & !is.na(data$hdl)] + 4
  data$score[data$hdl > 49 & data$hdl <= 51 & !is.na(data$hdl)] <- data$score[data$hdl > 49 & data$hdl <= 51 & !is.na(data$hdl)] + 3
  data$score[data$hdl > 51 & data$hdl <= 53 & !is.na(data$hdl)] <- data$score[data$hdl > 51 & data$hdl <= 53 & !is.na(data$hdl)] + 2
  data$score[data$hdl > 53 & data$hdl <= 55 & !is.na(data$hdl)] <- data$score[data$hdl > 53 & data$hdl <= 55 & !is.na(data$hdl)] + 1
  data$score[data$hdl > 55 & !is.na(data$hdl)] <- data$score[data$hdl > 55 & !is.na(data$hdl)] + 0

  ## Score triglycerides (mg/dL)
  data$score[data$triglycerides < 100 & !is.na(data$triglycerides)] <- data$score[data$triglycerides < 100 & !is.na(data$triglycerides)] + 0
  data$score[data$triglycerides >= 100 & data$triglycerides <= 149 & !is.na(data$triglycerides)] <- data$score[data$triglycerides >= 100 & data$triglycerides <= 149 & !is.na(data$triglycerides)] + 2
  data$score[data$triglycerides >= 150 & data$triglycerides <= 199 & !is.na(data$triglycerides)] <- data$score[data$triglycerides >= 150 & data$triglycerides <= 199 & !is.na(data$triglycerides)] + 3
  data$score[data$triglycerides >= 200 & !is.na(data$triglycerides)] <- data$score[data$triglycerides >= 200 & !is.na(data$triglycerides)] + 4

  ## Score smoker
  data$score[data$smoker == 1  & !is.na(data$smoker)] <- data$score[data$smoker == 1  & !is.na(data$smoker)] + 12

  ## Score diabetic
  data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$sex == "male" & !is.na(data$sex)] <- data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$sex == "male" & !is.na(data$sex)] + 9
  data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$sex == "female" & !is.na(data$sex)] <- data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$sex == "female" & !is.na(data$sex)] + 11


  ## Score famMI
  data$score[data$famMI == 1 & !is.na(data$famMI)] <- data$score[data$famMI == 1 & !is.na(data$famMI)] + 5

  ## Score sys BP
  data$score[data$sbp < 110 & !is.na(data$sbp)] <- data$score[data$sbp < 110 & !is.na(data$sbp)] + 0
  data$score[data$sbp >= 110 & data$sbp <= 119 & !is.na(data$sbp)] <- data$score[data$sbp >= 110 & data$sbp <= 119 & !is.na(data$sbp)] + 1
  data$score[data$sbp > 119 & data$sbp <= 129 & !is.na(data$sbp)] <- data$score[data$sbp > 119 & data$sbp <= 129 & !is.na(data$sbp)] + 2
  data$score[data$sbp > 129 & data$sbp <= 139 & !is.na(data$sbp)] <- data$score[data$sbp > 129 & data$sbp <= 139 & !is.na(data$sbp)] + 3
  data$score[data$sbp > 139 & data$sbp <= 149 & !is.na(data$sbp)] <- data$score[data$sbp > 139 & data$sbp <= 149 & !is.na(data$sbp)] + 4
  data$score[data$sbp > 149 & data$sbp <= 159 & !is.na(data$sbp)] <- data$score[data$sbp > 149 & data$sbp <= 159 & !is.na(data$sbp)] + 5
  data$score[data$sbp > 159 & data$sbp <= 169 & !is.na(data$sbp)] <- data$score[data$sbp > 159 & data$sbp <= 169 & !is.na(data$sbp)] + 6
  data$score[data$sbp > 169 & data$sbp <= 179 & !is.na(data$sbp)] <- data$score[data$sbp > 169 & data$sbp <= 179 & !is.na(data$sbp)] + 7
  data$score[data$sbp > 179 & !is.na(data$sbp)] <- data$score[data$sbp > 179 & !is.na(data$sbp)] + 8

  ## 10 year Risk

  #utils::data(sysdata, envir = environment())

data$risk <- NA

for(i in 1:nrow(data)){

  if(is.na(data$age[i])){
    next
    }
  if(is.na(data$sex[i])){
    next
    }

  if(data$sex[i] == "male"){

    data$risk[i] <- ifelse(is.null(risktable_procam2007_men[[as.character(data$age[i])]][data$score[i] == risktable_procam2007_men[[as.character(data$age[i])]][,1],2]), NA,
                           risktable_procam2007_men[[as.character(data$age[i])]][data$score[i] == risktable_procam2007_men[[as.character(data$age[i])]][,1],2])

  }

  if(data$sex[i] == "female") {

    data$risk[i] <- ifelse(is.null(risktable_procam2007_women[[as.character(data$age[i])]][data$score[i] == risktable_procam2007_women[[as.character(data$age[i])]][,1],2]),NA,
                           risktable_procam2007_women[[as.character(data$age[i])]][data$score[i] == risktable_procam2007_women[[as.character(data$age[i])]][,1],2])

  }

}



  return(data$risk)

}



