#' Calculate INVEST-Score
#'
#' This function takes clinical parameters to calculate the INVEST-Score. A risk score for predicting important adverse events in patients with hypertension and chronic stable coronary artery disease (CAD)
#'
#' @param age age; integer \[years\]
#' @param ethnicity ethnicity; categorical \[white|nw\]; ("nw"= nonwhite)
#' @param bmi body mass index; numeric \[kg/m^2\]
#' @param hr heart rate; numeric \[beats/minute\]
#' @param sbp systolic blood pressure; numeric \[mmHg\]
#' @param mi information of individual had a prior myocardial infarction (MI); numeric \[1|0\]; ("1"=yes;"0"=no)
#' @param chf information if individual has a congestive heart failure (CHF); numeric \[1|0\]; ("1"=yes;"0"=no)
#' @param stroke information if individual had a stroke; numeric \[1|0\]; ("1"=yes;"0"=no)
#' @param smoker a numeric vector. Smoker = 1, non-smoker = 0. A smoker was defined as current self-reported smoker.
#' @param diabetic diabetic status of individual; numeric \[1|0\]; ("1"=diabetic;"0"=non-diabetic)
#' @param pad information if individual has a peripheral arterial disease (PAD); numeric \[1|0\]; ("1"=yes;"0"=no)
#' @param ckd information if individual has a chronic kidney disease (CKD); numeric \[1|0\]; ("1"=yes;"0"=no)
#' @usage invest_score(age = NA, ethnicity = NA, bmi = NA, hr = NA, sbp = NA, mi = NA, chf = NA, stroke = NA, smoker = NA, diabetic = NA, pad = NA, ckd = NA)
#' @details
#' The function takes 12 clinical variables to calculate a risk score (%) to predict future adverse Events (Mortality all-cause, Non-fatal myocardial infarction, non-fatal stroke)
#' Patients can be divided into the following categories based on their individual risk score:
#' \itemize{
#'  \item{Low risk}{Risk Score <= 4}
#'  \item{Intermediate risk}{Risk Score 5 to 6}
#'  \item{High risk}{Risk Score >= 12}
#'  }
#' BACKGROUND:\cr
#' It is difficult to accurately determine prognosis of patients with hypertension and chronic stable coronary artery disease (CAD).
#' Our aim was to construct a risk score for predicting important adverse events in this population.\cr
#' METHODS and Results:\cr
#' Patients with hypertension and chronic stable CAD enrolled in the INternational VErapamil‐SR/Trandolapril STudy (INVEST) comprised the study cohort.
#' Candidate predictor variables were obtained from patients with at least 1 postbaseline visit.
#' Patients were divided into development (n=18 484) and validation cohorts (n=2054). Cox regression model identified predictors of the primary outcome:
#' all‐cause mortality, myocardial infarction, or stroke at a mean follow‐up of 2.3 years. The hazard ratio of each variable was rounded to the nearest
#' integer to construct score weights. A score 0 to 4 defined low‐risk, 5 to 6 intermediate‐risk and ≥7 high‐risk. The following variables were retained in the final model:
#' age, residence, body mass index, on‐treatment heart rate and BP, prior myocardial infarction, heart failure, stroke/transient ischemic attack, smoking, diabetes, peripheral arterial disease, and chronic kidney disease.
#' The primary outcome occurred in 2.9% of the low‐risk group, 6.5% of the intermediate‐risk group, and 18.0% of the high‐risk group (P for trend <0.0001).
#' The model was good at discriminating those who had an event versus those who did not (C‐statistic=0.75). The model performed well in a validation cohort (C‐statistic=0.77).\cr
#' CONCLUSIONS: Readily available clinical variables can rapidly stratify patients with hypertension and chronic stable CAD into useful risk categories.
#' @references
#' Bavry AA, et al. Simple integer risk score to determine prognosis of patients with hypertension and chronic stable coronary artery disease. J Am Heart Assoc. 2013 Aug 15;2(4):e000205. doi: 10.1161/JAHA.113.000205. PMID: 23948642; PMCID: PMC3828777.
#' @return A vector of the calculated risk per record.
#' @export
invest_score <- function(age = NA, ethnicity = NA, bmi = NA, hr = NA, sbp = NA, mi = NA, chf = NA, stroke = NA, smoker = NA, diabetic = NA, pad = NA, ckd = NA){

  if (any(!is.na(age)) & any(!is.numeric(age))) {
    stop("age must be a valid value. Numeric or NA")
  }

  if (!all(ethnicity %in% c("white","nw",NA))) {
    stop("chf must be either white (North American residence), nw (nonwhite) or NA (missing)")
  }

  if (any(!is.na(bmi)) & any(!is.numeric(bmi))) {
    stop("bmi must be a valid value. Numeric or NA")
  }

  if (any(!is.na(hr)) & any(!is.numeric(hr))) {
    stop("hr must be a valid value. Numeric or NA")
  }

  if (any(!is.na(sbp)) & any(!is.numeric(sbp))) {
    stop("sbp must be a valid value. Numeric or NA")
  }

  if (!all(mi %in% c(0,1,NA))) {
    stop("mi must be either 0 (no), 1 (yes) or NA (missing)")
  }

  if (!all(chf %in% c(0,1,NA))) {
    stop("chf must be either 0 (no), 1 (yes) or NA (missing)")
  }

  if (!all(stroke %in% c(0,1,NA))) {
    stop("stroke must be either 0 (no), 1 (yes) or NA (missing)")
  }

  if (!all(smoker %in% c(0,1,NA))) {
    stop("smoker must be either 0 (no), 1 (yes) or NA (missing)")
  }

  if (!all(diabetic %in% c(0,1,NA))) {
    stop("diabetic must be either 0 (no), 1 (yes) or NA (missing)")
  }

  if (!all(pad %in% c(0,1,NA))) {
    stop("pad must be either 0 (no), 1 (yes) or NA (missing)")
  }

  if (!all(ckd %in% c(0,1,NA))) {
    stop("ckd must be either 0 (no), 1 (yes) or NA (missing)")
  }

  if (any(is.na(age))) {
    warning("No or only partial data for age provided. This results in an underestimation of the score")
  }

  if (any(is.na(ethnicity))) {
    warning("No or only partial data for ethnicity provided. This results in an underestimation of the score")
  }

  if (any(is.na(bmi))) {
    warning("No or No or only partial data for BMI provided. This results in an underestimation of the score")
  }

  if (any(is.na(hr))) {
    warning("No or only partial data for heart rate provided. This results in an underestimation of the score")
  }

  if (any(is.na(sbp))) {
    warning("No or only partial data for systolic blood preasure provided. This results in an underestimation of the score")
  }

  if (any(is.na(mi))) {
    warning("No or only partial data for myocardial infarction provided. This results in an underestimation of the score")
  }

  if (any(is.na(chf))) {
    warning("No or only partial data for congestive heart failure provided. This results in an underestimation of the score")
  }

  if (any(is.na(stroke))) {
    warning("No or only partial data for stroke/tia provided. This results in an underestimation of the score")
  }

  if (any(is.na(smoker))) {
    warning("No or only partial data for smoker provided. This results in an underestimation of the score")
  }

  if (any(is.na(diabetic))) {
    warning("No or only partial data for diabetic provided. This results in an underestimation of the score")
  }

  if (any(is.na(pad))) {
    warning("No or only partial data for peripheral arterial disease. This results in an underestimation of the score")
  }

  if (any(is.na(ckd))) {
    warning("No or only partial data for chronic kidney disease provided. This results in an underestimation of the score")
  }


  data <- data.frame(age = age, ethnicity = ethnicity, bmi = bmi, hr = hr, sbp = sbp, mi = mi, chf = chf, stroke = stroke, smoker = smoker, diabetic = diabetic, pad = pad, ckd = ckd)

  data$riskscore <- 0

  ### age
  data$riskscore[is.na(data$age)] <- data$riskscore[is.na(data$age)]
  data$riskscore[data$age < 65 & !is.na(data$age)] <- data$riskscore[data$age < 65 & !is.na(data$age)] + 0
  data$riskscore[data$age >= 65 & data$age < 75 & !is.na(data$age)] <- data$riskscore[data$age >= 65 & data$age < 75 & !is.na(data$age)] + 2
  data$riskscore[data$age >= 75 & !is.na(data$age)] <- data$riskscore[data$age >= 75 & !is.na(data$age)] + 3

  ### ethnicity
  data$riskscore[data$ethnicity == "white" & !is.na(data$ethnicity)] <- data$riskscore[data$ethnicity == "white" & !is.na(data$ethnicity)] + 2

  ### BMI
  data$riskscore[is.na(data$bmi)] <- data$riskscore[is.na(data$bmi)]
  data$riskscore[data$bmi < 20 & !is.na(data$bmi)] <- data$riskscore[data$bmi < 20 & !is.na(data$bmi)] + 2
  data$riskscore[data$bmi >= 20 & data$bmi < 30 & !is.na(data$bmi)] <- data$riskscore[data$bmi >= 20 & data$bmi < 30 & !is.na(data$bmi)] + 1
  data$riskscore[data$bmi > 30 & !is.na(data$bmi)] <- data$riskscore[data$bmi > 30 & !is.na(data$bmi)] + 0

  ### heart rate
  data$riskscore[is.na(data$hr)] <- data$riskscore[is.na(data$hr)]
  data$riskscore[data$hr >= 85 & !is.na(data$hr)] <- data$riskscore[data$hr >= 85 & !is.na(data$hr)] + 1
  data$riskscore[data$hr < 85 & !is.na(data$hr)] <- data$riskscore[data$hr < 85 & !is.na(data$hr)] + 0

  ### sbp
  data$riskscore[is.na(data$sbp)] <- data$riskscore[is.na(data$sbp)]
  data$riskscore[data$sbp < 110 & !is.na(data$sbp)] <- data$riskscore[data$sbp < 110 & !is.na(data$sbp)] + 2
  data$riskscore[data$sbp < 110 & data$sbp > 140 & !is.na(data$sbp)] <- data$riskscore[data$sbp < 110 & data$sbp > 140 & !is.na(data$sbp)] + 0
  data$riskscore[data$sbp >= 140 & !is.na(data$sbp)] <- data$riskscore[data$sbp >= 140 & !is.na(data$sbp)] + 1

  ### myocardial infarction
  data$riskscore[is.na(data$mi)] <- data$riskscore[is.na(data$mi)]
  data$riskscore[data$mi == 1 & !is.na(data$mi)] <- data$riskscore[data$mi == 1 & !is.na(data$mi)] + 1
  data$riskscore[data$mi == 0 & !is.na(data$mi)] <- data$riskscore[data$mi == 0 & !is.na(data$mi)]

  ### congestive heart failure
  data$riskscore[is.na(data$chf)] <- data$riskscore[is.na(data$chf)]
  data$riskscore[data$chf == 1 & !is.na(data$chf)] <- data$riskscore[data$chf == 1 & !is.na(data$chf)] + 2
  data$riskscore[data$chf == 0 & !is.na(data$chf)] <- data$riskscore[data$chf == 0 & !is.na(data$chf)]

  ### stroke
  data$riskscore[is.na(data$stroke)] <- data$riskscore[is.na(data$stroke)]
  data$riskscore[data$stroke == 1 & !is.na(data$stroke)] <- data$riskscore[data$stroke == 1 & !is.na(data$stroke)] + 2
  data$riskscore[data$stroke == 0 & !is.na(data$stroke)] <- data$riskscore[data$stroke == 0 & !is.na(data$stroke)]

  ### smoker
  data$riskscore[is.na(data$smoker)] <- data$riskscore[is.na(data$smoker)]
  data$riskscore[data$smoker == 1 & !is.na(data$smoker)] <- data$riskscore[data$smoker == 1 & !is.na(data$smoker)] + 1
  data$riskscore[data$smoker == 0 & !is.na(data$smoker)] <- data$riskscore[data$smoker == 0 & !is.na(data$smoker)]

  ### diabetic
  data$riskscore[is.na(data$diabetic)] <- data$riskscore[is.na(data$diabetic)]
  data$riskscore[data$diabetic == 1 & !is.na(data$diabetic)] <- data$riskscore[data$diabetic == 1 & !is.na(data$diabetic)] + 2
  data$riskscore[data$diabetic == 0 & !is.na(data$diabetic)] <- data$riskscore[data$diabetic == 0 & !is.na(data$diabetic)]

  ###  peripheral arterial disease
  data$riskscore[is.na(data$pad)] <- data$riskscore[is.na(data$pad)]
  data$riskscore[data$pad == 1 & !is.na(data$pad)] <- data$riskscore[data$pad == 1 & !is.na(data$pad)] + 1
  data$riskscore[data$pad == 0 & !is.na(data$pad)] <- data$riskscore[data$pad == 0 & !is.na(data$pad)]

  ###  chronic kidney disease
  data$riskscore[is.na(data$ckd)] <- data$riskscore[is.na(data$ckd)]
  data$riskscore[data$ckd == 1 & !is.na(data$ckd)] <- data$riskscore[data$ckd == 1 & !is.na(data$ckd)] + 2
  data$riskscore[data$ckd == 0 & !is.na(data$ckd)] <- data$riskscore[data$ckd == 0 & !is.na(data$ckd)]


  data$riskscore <- ifelse(data$riskscore > 12, 12, data$riskscore)

  data$risk <- risktable_invest$risk[match(data$riskscore, risktable_invest$points)]

  return(data$risk)

}
