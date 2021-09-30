#' Calculate Procam Score (Version 2002 and 2007)
#'
#' @description This function takes necessary parameters to calculate the PROCAM-Risk-Score Table Version for the Risk of acute Coronary Events based on the 10-year follow-up of the Prospective Cardiovascular Münster (PROCAM) study
#'
#' @param age a numeric vector with the age of persons given as years
#' @param ldl  a numeric vector; LDL Cholesterol values given in mg/dL
#' @param hdl a numeric vector; HDL Cholesterol values given in mg/dL
#' @param sbp a numeric vector with the systolic blood pressure of persons given as mmHg
#' @param triglycerides a numeric vector with the information whether a Patient is taking antihypertensive medication. Values: yes = 1; no = 0.
#' @param smoker a numeric vector. Smoker = 1, non-smoker = 0. A smoker was defined as current self-reported smoker.
#' @param diabetic a numeric vector indicating whether a person is diabetic. Values: yes = 1; no = 0.
#' @param famMI a numeric vector indicating family history of premature myocardial infarction. Values: yes = 1; no = 0.
#' @usage procam_score_2002(age, ldl, hdl, triglycerides, smoker, diabetic, famMI, sbp)
#' @return A vector of calculated risk per record in %.
#' @details Abstract:
#' Background: The absolute risk of an acute coronary event depends on the totality of risk factors exhibited by an individual, the so-called global risk profile. Although several scoring schemes have been suggested to calculate this profile, many omit information on important variables such as family history of coronary heart disease or LDL cholesterol.
#' Methods and Results: Based on 325 acute coronary events occurring within 10 years of follow-up among 5389 men 35 to 65 years of age at recruitment into the Prospective Cardiovascular Muenster (PROCAM) study, we developed a Cox proportional hazards model using the following 8 independent risk variables, ranked in order of importance: age, LDL cholesterol,
#' smoking, HDL cholesterol, systolic blood pressure, family history of premature myocardial infarction, diabetes mellitus, and triglycerides. We then derived a simple point scoring system based on the beta-coefficients of this model. The accuracy of this point scoring scheme was comparable to coronary event prediction when the continuous variables themselves were used.
#' The scoring system accurately predicted observed coronary events with an area under the receiver-operating characteristics curve of 82.4% compared with 82.9% for the Cox model with continuous variables.
#' Conclusions: Our scoring system is a simple and accurate way of predicting global risk of myocardial infarction in clinical practice and will therefore allow more accurate targeting of preventive therapy.
#' @references
#' Assmann G, Cullen P, Schulte H. Simple scoring scheme for calculating the risk of acute coronary events based on the 10-year follow-up of the prospective cardiovascular Münster (PROCAM) study.
#' Circulation. 2002 Jan 22;105(3):310-5. doi: 10.1161/hc0302.102575. Erratum in: Circulation 2002 Feb 19;105(7):900. PMID: 11804985.
#' @import stats
#' @export
procam_score_2002 <- function(age, ldl, hdl, triglycerides, smoker, diabetic, famMI, sbp){

  data <- data.frame(age = age, ldl = ldl, hdl = hdl, triglycerides = triglycerides, smoker = smoker, diabetic = diabetic, famMI = famMI, sbp = sbp)

  if(sum(is.na(data) !=0)){

    print("Dataframe contains NAs. Entries with missing values have a lower accuracy and underestimate the 10-year-risk")

  }

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
  data$score[data$age >= 60 & data$age <= 65 & !is.na(data$age)] <- data$score[data$age >= 60 & data$age <= 65 & !is.na(data$age)] + 26

  ## Score LDL
  data$score[data$ldl < 100] <- data$score[data$ldl < 100 & !is.na(data$ldl)] + 0
  data$score[data$ldl >= 100 & data$ldl <= 129 & !is.na(data$ldl)] <- data$score[data$ldl >= 100 & data$ldl <= 129 & !is.na(data$ldl)] + 5
  data$score[data$ldl >= 130 & data$ldl <= 159 & !is.na(data$ldl)] <- data$score[data$ldl >= 130 & data$ldl <= 159 & !is.na(data$ldl)] + 10
  data$score[data$ldl >= 160 & data$ldl <= 189 & !is.na(data$ldl)] <- data$score[data$ldl >= 160 & data$ldl <= 189 & !is.na(data$ldl)] + 14
  data$score[data$ldl >= 190] <- data$score[data$ldl >= 190] + 20

  ## Score hdl
  data$score[data$hdl < 35] <- data$score[data$hdl < 35 & !is.na(data$hdl)] + 11
  data$score[data$hdl >= 35 & data$hdl <= 44 & !is.na(data$hdl)] <- data$score[data$hdl >= 35 & data$hdl <= 44 & !is.na(data$hdl)] + 8
  data$score[data$hdl >= 45 & data$hdl <= 54 & !is.na(data$hdl)] <- data$score[data$hdl >= 45 & data$hdl <= 54 & !is.na(data$hdl)] + 5
  data$score[data$hdl >= 55] <- data$score[data$hdl >= 55 & !is.na(data$hdl)] + 0

  ## Score triglycerides
  data$score[data$triglycerides < 100] <- data$score[data$triglycerides < 100 & !is.na(data$triglycerides)] + 0
  data$score[data$triglycerides >= 100 & data$triglycerides <= 149 & !is.na(data$triglycerides)] <- data$score[data$triglycerides >= 100 & data$triglycerides <= 149 & !is.na(data$triglycerides)] + 2
  data$score[data$triglycerides >= 150 & data$triglycerides <= 199 & !is.na(data$triglycerides)] <- data$score[data$triglycerides >= 150 & data$triglycerides <= 199 & !is.na(data$triglycerides)] + 3
  data$score[data$triglycerides >= 200] <- data$score[data$triglycerides >= 200 & !is.na(data$triglycerides)] + 4

  ## Score smoker
  data$score[data$smoker == 1] <- data$score[data$smoker == 1  & !is.na(data$smoker)] + 8

  ## Score diabetic
  data$score[data$diabetic == 1] <- data$score[data$diabetic == 1  & !is.na(data$diabetic)] + 6

  ## Score famMI
  data$score[data$famMI == 1] <- data$score[data$famMI == 1 & !is.na(data$famMI)] + 4

  ## Score sys BP
  data$score[data$sbp < 120] <- data$score[data$sbp < 120 & !is.na(data$sbp)] + 0
  data$score[data$sbp >= 120 & data$sbp <= 129] <- data$score[data$sbp >= 120 & data$sbp <= 129 & !is.na(data$sbp)] + 2
  data$score[data$sbp >= 130 & data$sbp <= 139] <- data$score[data$sbp >= 130 & data$sbp <= 139 & !is.na(data$sbp)] + 3
  data$score[data$sbp >= 140 & data$sbp <= 159] <- data$score[data$sbp >= 140 & data$sbp <= 159 & !is.na(data$sbp)] + 5
  data$score[data$sbp >= 160] <- data$score[data$sbp >= 160 & !is.na(data$sbp)] + 8

  ## 10 year Risk

  risktable <- data.frame(points = 20:60, risk = c(1,1.1,1.2,1.3,1.4,1.6,1.7,1.8,1.9,2.3,2.4,2.8,2.9,3.3,3.5,4.0,4.2,4.8,5.1,5.7,6.1,7.0,7.4,
                                                   8.0,8.8,10.2,10.5,10.7,12.8,13.2,15.5,16.8,17.5,19.6,21.7,22.2,23.8,25.1,28.0,29.4,30.0))
  data$risk <- NA

  data$risk <- risktable$risk[match(data$score, risktable$points)]

  data$risk[!complete.cases(data$risk)] <- NA

  return(data$risk)

}


#' @export
procam_score_2007 <- function(sex, age, ldl, hdl, triglycerides, smoker, diabetic, famMI, sbp){

  data <- data.frame(age = age, ldl = ldl, hdl = hdl, triglycerides = triglycerides, smoker = smoker, diabetic = diabetic, famMI = famMI, sbp = sbp, sex = sex)

  if(sum(is.na(data) !=0)){

    print("Dataframe contains NAs. Entries with missing values have a lower accuracy and underestimate the 10-year-risk")

  }

  data$score <- 0
  data$age <- round(data$age)
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
  data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$gender == 1 & !is.na(data$gender)] <- data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$gender == 1 & !is.na(data$gender)] + 9
  data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$gender == 1 & !is.na(data$gender)] <- data$score[data$diabetic == 1  & !is.na(data$diabetic) & data$gender == 0 & !is.na(data$gender)] + 11


  ## Score famMI
  data$score[data$famMI == 1 & !is.na(data$famMI)] <- data$score[data$famMI == 1 & !is.na(data$famMI)] + 4

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

    data$risk[i] <- risktable_procam2007_men[[as.character(data$age[i])]][data$score[i] == risktable_procam2007_men[[as.character(data$age[i])]][,1],2]

  }

  if(data$sex[i] == "female") {

    data$risk[i] <- risktable_procam2007_women[[as.character(data$age[i])]][data$score[i] == risktable_procam2007_women[[as.character(data$age[i])]][,1],2]

  }

}



  return(data$risk)

}



