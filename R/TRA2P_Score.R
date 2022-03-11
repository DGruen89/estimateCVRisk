#' Calculate TRA2P-Score
#'
#' This function takes clinical predictors of recurrent atherothrombosis to calculate the TRA2P-Score
#'
#' @param age a numeric vector with the age of persons given in years.
#' @param chf a numeric vector indicating the presence of a congestive heart failure. Values: yes = 1; no = 0.
#' @param ah a numeric vector indicating the presence of arterial hyperthorphy. Values: yes = 1; no = 0.
#' @param diabetic a numeric vector indicating whether a person is diabetic. Values: yes = 1; no = 0.
#' @param stroke  numeric vector indicating whether a person has had a stroke. Values: yes = 1; no = 0.
#' @param bypass_surg numeric vector indicating whether a person has undergone a bypass surgery. Values: yes = 1; no = 0.
#' @param other_surg numeric vector indicating whether a person has other vascular disease (peripheral). Values: yes = 1; no = 0.
#' @param egfr a numeric vector; eGFR values given in mL x min^−1 x 1.73 m^−2.
#' @param smoker a numeric vector. Smoker = 1, non-smoker = 0. A smoker was defined as current self-reported smoker.
#' @usage tra2p_score(age, chf = NA, ah = NA, diabetic = NA, stroke = NA,
#' bypass_surg = NA, other_surg = NA, egfr = NA, smoker = NA)
#' @details
#' The function takes nine independent baseline clinical atherothrombotic risk indicators to calculate a score (%) according to the 3-year risk of cardiovascular death, MI or ischemic stroke.
#' The score can be divided into the following categories:
#' \itemize{
#'  \item{Low risk}{Score < 5%}
#'  \item{Intermediate risk}{Score 5% to < 15%}
#'  \item{High risk}{Score >= 15%}
#'  }
#' BACKGROUND:\cr
#' Patients with stable ischemic heart disease and previous myocardial infarction (MI) vary in their risk for recurrent cardiovascular events.
#' Atherothrombotic risk assessment may be useful to identify high- risk patients who have the greatest potential to benefit from more intensive secondary preventive therapy
#' such as treatment with vorapaxar.\cr
#' METHODS:\cr
#' We identified independent clinical indicators of atherothrombotic risk among 8598 stable, placebo-treated patients with a previous MI followed up for 2.5 years (median)
#' in TRA 2°P-TIMI 50 (Thrombin Receptor Antagonist in Secondary Prevention of Atherothrombotic Ischemic Events–TIMI 50).
#' The efficacy and safety of vorapaxar (SCH 530348; MK-5348) were assessed by baseline risk among patients with previous MI without prior stroke or transient ischemic attack for whom
#' there is a clinical indication for vorapaxar. End points were cardiovascular death, MI, or ischemic stroke and GUSTO (Global Use of Strategies to Open Occluded Coronary Arteries) severe bleeding.\cr
#' RESULTS:\cr
#' The 9 independent risk predictors were age, diabetes mellitus, hypertension, smoking, peripheral arterial disease, previous stroke, previous coronary bypass grafting, heart failure, and renal dysfunction. A simple integer-based scheme using these predictors showed a strong graded relationship with the rate of cardiovascular death/MI/ischemic stroke and the individual components (P for trend <0.001 for all). High-risk patients (≥3 risk indicators; 20% of population) had a 3.2% absolute risk reduction in cardiovascular disease/MI/ischemic stroke with vorapaxar, and intermediate-risk patients (1–2 risk indicators; 61%) had a 2.1% absolute risk reduction (P<0.001 each), translating to a number needed to treat of 31 and 48. Bleeding increased across risk groups (P for trend<0.01); however, net clinical outcome was increasingly favorable with vorapaxar across risk groups. Fatal bleeding or intracranial hemorrhage was 0.9% with both treatments in high-risk patients.
#' CONCLUSIONS: Stratification of baseline atherothrombotic risk can assist with therapeutic decision making for vorapaxar use for secondary prevention after MI.
#' @references
#' Bohula EA, et al. Atherothrombotic Risk Stratification and the Efficacy and Safety of Vorapaxar in Patients With Stable Ischemic Heart Disease and Previous Myocardial Infarction.
#' Circulation. 2016 Jul 26;134(4):304-13. doi: 10.1161/CIRCULATIONAHA.115.019861. PMID: 27440003.
#' @return A vector of the calculated risk per record.
#' @export
tra2p_score <- function(age, chf = NA, ah = NA, diabetic = NA, stroke = NA, bypass_surg = NA, other_surg = NA, egfr = NA, smoker = NA){

  if (!is.numeric(age) |  missing(age)) {
    stop("age must be a valid numeric value")
  }

  if (!all(chf %in% c(0,1,NA))) {
    stop("smoker must be either 0 (no), 1 (yes) or NA (missing)")
  }

  if (any(is.na(chf))) {
    warning("No or some values for chf not provided. This results in an underestimation of the score")
  }

  if (!all(ah %in% c(0,1,NA))) {
    stop("ah must be either 0 (no), 1 (yes) or NA (missing)")
  }

  if (any(is.na(ah))) {
    warning("No or some values for ah not provided. This results in an underestimation of the score")
  }

  if (!all(diabetic %in% c(0,1,NA))) {
    stop("diabetic must be either 0 (no), 1 (yes) or NA (missing)")
  }

  if (any(is.na(diabetic))) {
    warning("No or some values for diabetic not provided. This results in an underestimation of the score")
  }

  if (!all(stroke %in% c(0,1,NA))) {
    stop("stroke must be either 0 (no), 1 (yes) or NA (missing)")
  }

  if (any(is.na(stroke))) {
    warning("No or some values for stroke not provided. This results in an underestimation of the score")
  }

  if (!all(bypass_surg %in% c(0,1,NA))) {
    stop("bypass_surg must be either 0 (no), 1 (yes) or NA (missing)")
  }

  if (any(is.na(bypass_surg))) {
    warning("No or some values for bypass_surg not provided. This results in an underestimation of the score")
  }

  if (!all(other_surg %in% c(0,1,NA))) {
    stop("other_surg must be either 0 (no), 1 (yes) or NA (missing)")
  }

  if (any(is.na(other_surg))) {
    warning("No or some values for other_surg not provided. This results in an underestimation of the score")
  }

  if (!all(smoker %in% c(0,1,NA))) {
    stop("other_surg must be either 0 (no), 1 (yes) or NA (missing)")
  }

  if (any(is.na(smoker))) {
    warning("No or some values for other_surg not provided. This results in an underestimation of the score")
  }

  if (any(!is.na(egfr)) & any(!is.numeric(egfr))) {
    stop("egfr must be a valid value. Numeric or NA")
  }

  if (any(is.na(egfr))) {
    warning("No or some values for BMI not provided. This results in an underestimation of the score")
  }

  data <- data.frame(chf = chf, ah = ah, age = age, diabetic = diabetic, stroke = stroke, bypass_surg = bypass_surg, other_surg = other_surg, egfr = egfr, smoker = smoker)

  data$points <- 0

  data$points[data$chf == 1 & !is.na(data$chf)] <- data$points[data$chf == 1 & !is.na(data$chf)] + 1
  data$points[data$ah == 1 & !is.na(data$ah)] <- data$points[data$ah == 1 & !is.na(data$ah)] + 1
  data$points[data$age >= 75 & !is.na(data$age)] <- data$points[data$age >= 75 & !is.na(data$age)] + 1
  data$points[data$diabetic == 1 & !is.na(data$diabetic)] <- data$points[data$diabetic == 1 & !is.na(data$diabetic)] + 1
  data$points[data$stroke == 1 & !is.na(data$stroke)] <- data$points[data$stroke == 1 & !is.na(data$stroke)] + 1
  data$points[data$bypass_surg == 1 & !is.na(data$bypass_surg)] <- data$points[data$bypass_surg == 1 & !is.na(data$bypass_surg)] + 1
  data$points[data$other_surg == 1 & !is.na(data$other_surg)] <- data$points[data$other_surg == 1 & !is.na(data$other_surg)] + 1
  data$points[data$egfr < 60 & !is.na(data$egfr)] <- data$points[data$egfr < 60 & !is.na(data$egfr)] + 1
  data$points[data$smoker == 1 & !is.na(data$smoker)] <- data$points[data$smoker == 1 & !is.na(data$smoker)] + 1

  table_tra2p <- data.frame(risk = c(3.5,6.8,9.9,14.5,21.8,28.8,45.3,58.6),
                            points = c(0:7))

  data$points <- ifelse(data$points > 7, 7, data$points)

  data$risk <- table_tra2p$risk[match(data$points, table_tra2p$points)]

  return(data$risk)

}
