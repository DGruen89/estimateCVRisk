#' Calculate Reach-Score_Death / Reach-Score_next_CV
#'
#' This function takes necessary parameters to calculate the Reach-Score. You can choose between a calculation based on a score sheet or based on a cox model (formula).
#'
#' @param sex a character vector indicating the sex of the person. Values: "female", "male"
#' @param age a numeric vector with the age of persons given as years
#' @param bmi a numeric vector; Body Mass Index in kg/m^2
#' @param diabetic a numeric vector indicating whether a person is diabetic. Values: yes = 1; no = 0.
#' @param smoker a numeric vector. A smoker was defined as >= 5 cigarettes per day on average within the last month. Smoker = 1, non-smoker = 0.
#' @param vasc a numeric vector; Number of vascular beds involved in previously diagnosed vascular disease. Number from 0 to 3
#' @param cv_event a numeric vector; cardiovascular event in past year. 1 = yes, 0 = no
#' @param chf a numeric vector indicating whether a person had a Congestive heart failure. Values: yes = 1; no = 0.
#' @param af numeric vector; Atrial fibrillation. 1 = yes, 0 = no
#' @param statin a numeric vector; indicating whether a person is on a statin therapy. 1 = yes, 0 = no
#' @param asa information if individual is on a acetylsalicylic acid medication; numeric ("1"=yes;"0"=no)
#' @param region_EE_or_ME a logical vector; Geographical region membership in East Europe or Middel East
#' @param region_jap_aust a logical vector; Geographical region membership in Japan or Australia
#' @aliases reach_score_next_cv reach_score_cv_death reach_score_cv_death_formula reach_score_next_cv_formula
#' @usage
#' reach_score_next_cv(sex, age, bmi=NA, diabetic=NA, smoker=NA,
#' vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, asa=NA,
#' region_EE_or_ME = FALSE, region_jap_aust = FALSE)
#' reach_score_cv_death(sex, age, bmi=NA, diabetic=NA, smoker=NA,
#' vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, asa=NA,
#' region_EE_or_ME = FALSE, region_jap_aust = FALSE)
#' reach_score_cv_death_formula(sex, age, bmi=NA, diabetic=NA, smoker=NA,
#' vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, asa=NA,
#' region_EE_or_ME = FALSE, region_jap_aust = FALSE)
#' reach_score_next_cv_formula(sex, age, bmi=NA, diabetic=NA, smoker=NA,
#' vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, asa=NA,
#' region_EE_or_ME = FALSE, region_jap_aust = FALSE)
#' @details
#' A risk model to predict secondary cardiovascular events and cardiovascular death in outpatients with established atherothrombotic disease.
#' Tradiotional risk factors, burden of disease, lack of treatment, and geographic location all are related to an increase risk of subsequent
#' cardiovascular morbidity and cardiovascular mortality. \cr
#' \cr
#' Abstract: \cr
#' Background:\cr
#' Prediction models for cardiovascular events and cardiovascular death in patients with established cardiovascular disease are not generally available.\cr
#' Methods: \cr
#' Participants from the prospective REduction of Atherothrombosis for Continued Health (REACH) Registry provided a global outpatient population with known cardiovascular disease at entry.
#' Cardiovascular prediction models were estimated from the 2-year follow-up data of 49,689 participants from around the world.\cr
#' Results:\cr
#' A developmental prediction model was estimated from 33,419 randomly selected participants (2394 cardiovascular events with 1029 cardiovascular deaths) from the pool of 49,689.
#' The number of vascular beds with clinical disease, diabetes, smoking, low body mass index, history of atrial fibrillation, cardiac failure,
#' and history of cardiovascular event(s) <1 year before baseline examination increased risk of a subsequent cardiovascular event.
#' Statin (hazard ratio 0.75; 95% confidence interval, 0.69-0.82) and acetylsalicylic acid therapy (hazard ratio 0.90; 95% confidence interval, 0.83-0.99)
#' also were significantly associated with reduced risk of cardiovascular events. The prediction model was validated
#' in the remaining 16,270 REACH subjects (1172 cardiovascular events, 494 cardiovascular deaths). Risk of cardiovascular death was similarly estimated with the same set of risk factors.
#' Simple algorithms were developed for prediction of overall cardiovascular events and for cardiovascular death.\cr
#' Conclusions:\cr
#' This study establishes and validates a risk model to predict secondary cardiovascular events and cardiovascular death in outpatients with established atherothrombotic disease.
#' Traditional risk factors, burden of disease, lack of treatment, and geographic location all are related to an increased risk of subsequent cardiovascular morbidity and cardiovascular mortality.
#'
#' \itemize{
#'  \item{Reduction of Atherothrombosis for Continued Health (REACH) Registry}
#'  \item{established in 2003}
#'  \item{Patients from all over the world}
#'  \item{applicable for patients with age >= 45}
#'  \item{Study enrolled between Dec. 2003 nad June 2004}
#'  \item{20-month risk prediction for cardiovascular death and next cardiovascular event}
#'  \item{applicable for Patients with knwon atherothrombotic disease or at high risk to developing atherothrombosis}
#'  }
#' @references Wilson. Peter W. F., et al. "An International Model to Predict Recurrent Cardiovascular Disease." The American Journal of Medicine (2012) 125, 695-703.
#' @return A vector of the calculated risk per record.
#' @export
reach_score_next_cv <- function(sex, age, bmi=NA, diabetic=NA, smoker=NA, vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, asa=NA, region_EE_or_ME = FALSE, region_jap_aust = FALSE){

    if (!all(sex %in% c("male", "female")) | missing(sex)) {
        stop("sex must be either 'male' or 'female'")
    }

    if (!is.numeric(age) |  missing(age)) {
        stop("age must be a valid numeric value")
    }

    if (any(!is.na(bmi)) & any(!is.numeric(bmi))) {
        stop("bmi must be a valid value. Numeric or NA")
    }

    if (!all(diabetic %in% c(0,1,NA))) {
        stop("diabetic must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(smoker %in% c(0,1,NA))) {
        stop("smoker must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(vasc %in% c(1,2,3,NA))) {
        stop("vasc must be either 1,2,3 or NA (missing)")
    }

    if (!all(cv_event %in% c(0,1,NA))) {
        stop("cv_event must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(af %in% c(0,1,NA))) {
        stop("af must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(statin %in% c(0,1,NA))) {
        stop("statin must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(asa %in% c(0,1,NA))) {
        stop("asa must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (all(!is.logical(region_EE_or_ME)) | all(!is.logical(region_jap_aust))) {
        stop("region must be either TRUE or FALSE")
    }

    if (any(age < 45) | any(is.na(age))) {
        warning("Some age values are below the optimal age range. Risk calculation can thus become less accurate.")
    }

    if (any(is.na(bmi))) {
        warning("bmi contains NA's. This can greatly underestimate the risk for individuals")
    }

    if (any(is.na(diabetic))) {
        warning("diabetic contains NA's. This can greatly underestimate the risk for individuals")
    }

    if (any(is.na(smoker))) {
        warning("smoker contains NA's. This can greatly underestimate the risk for individuals")
    }

    if (any(is.na(cv_event))) {
        warning("cv_event contains NA's. This can greatly underestimate the risk for individuals")
    }

    if (any(is.na(af))) {
        warning("af contains NA's. This can greatly underestimate the risk for individuals")
    }

    if (any(is.na(statin))) {
        warning("statin contains NA's. This can greatly underestimate the risk for individuals")
    }

    if (any(is.na(asa))) {
        warning("asa contains NA's. This can greatly underestimate the risk for individuals")
    }



    data <- data.frame(sex = sex, age = age, smoker = smoker, diabetic = diabetic, bmi = bmi, vasc = vasc, cv_event = cv_event, chf = chf, af = af,
                       statin = statin, asa = asa, region_EE_or_ME = region_EE_or_ME, region_jap_aust = region_jap_aust)

    data$riskscore <- 0

    #NEXT CV EVENT

    ### Points for sex (0=male / 1=female)

    data$riskscore[is.na(data$sex)] <- data$riskscore[is.na(data$sex)]
    data$riskscore[data$sex == "female" & !is.na(data$sex)] <- data$riskscore[data$sex == "female" & !is.na(data$sex)] + 0
    data$riskscore[data$sex == "male" & !is.na(data$sex)] <- data$riskscore[data$sex == "male" & !is.na(data$sex)] + 1

    ### age

    data$riskscore[is.na(data$age)] <- data$riskscore[is.na(data$age)]
    data$riskscore[data$age < 25 & !is.na(data$age)] <- data$riskscore[data$age < 25 & !is.na(data$age)] + 0
    data$riskscore[data$age >= 25 & data$age < 30 & !is.na(data$age)] <- data$riskscore[data$age >= 25 & data$age < 30 & !is.na(data$age)] + 1
    data$riskscore[data$age >= 30 & data$age < 35 & !is.na(data$age)] <- data$riskscore[data$age >= 30 & data$age < 35 & !is.na(data$age)] + 2
    data$riskscore[data$age >= 35 & data$age < 40 & !is.na(data$age)] <- data$riskscore[data$age >= 35 & data$age < 40 & !is.na(data$age)] + 3
    data$riskscore[data$age >= 40 & data$age < 45 & !is.na(data$age)] <- data$riskscore[data$age >= 40 & data$age < 45 & !is.na(data$age)] + 4
    data$riskscore[data$age >= 45 & data$age < 50 & !is.na(data$age)] <- data$riskscore[data$age >= 45 & data$age < 50 & !is.na(data$age)] + 5
    data$riskscore[data$age >= 50 & data$age < 55 & !is.na(data$age)] <- data$riskscore[data$age >= 50 & data$age < 55 & !is.na(data$age)] + 6
    data$riskscore[data$age >= 55 & data$age < 60 & !is.na(data$age)] <- data$riskscore[data$age >= 55 & data$age < 60 & !is.na(data$age)] + 7
    data$riskscore[data$age >= 60 & data$age < 65 & !is.na(data$age)] <- data$riskscore[data$age >= 60 & data$age < 65 & !is.na(data$age)] + 8
    data$riskscore[data$age >= 65 & data$age < 70 & !is.na(data$age)] <- data$riskscore[data$age >= 65 & data$age < 70 & !is.na(data$age)] + 9
    data$riskscore[data$age >= 70 & data$age < 75 & !is.na(data$age)] <- data$riskscore[data$age >= 70 & data$age < 75 & !is.na(data$age)] + 10
    data$riskscore[data$age >= 75 & data$age < 80 & !is.na(data$age)] <- data$riskscore[data$age >= 75 & data$age < 80 & !is.na(data$age)] + 11
    data$riskscore[data$age >= 80 & data$age < 85 & !is.na(data$age)] <- data$riskscore[data$age >= 80 & data$age < 85 & !is.na(data$age)] + 12
    data$riskscore[data$age >= 85 & !is.na(data$age)] <- data$riskscore[data$age >= 85 & !is.na(data$age)] + 13


    ### BMI

    data$riskscore[is.na(data$bmi)] <- data$riskscore[is.na(data$bmi)]
    data$riskscore[data$bmi < 20 & !is.na(data$bmi)] <- data$riskscore[data$bmi < 20 & !is.na(data$bmi)] + 2
    data$riskscore[data$bmi >= 20 & !is.na(data$bmi)] <- data$riskscore[data$bmi >= 20 & !is.na(data$bmi)] + 0



    ### Raucher (Ex Raucher unter 6 Monate)


    data$riskscore[is.na(data$smoker)] <- data$riskscore[is.na(data$smoker)]
    data$riskscore[data$smoker == 1 & !is.na(data$smoker)] <- data$riskscore[data$smoker == 1 & !is.na(data$smoker)] + 2
    data$riskscore[data$smoker == 0 & !is.na(data$smoker)] <- data$riskscore[data$smoker == 0 & !is.na(data$smoker)]


    ### diabeticetes
    data$riskscore[is.na(data$diabetic)] <- data$riskscore[is.na(data$diabetic)]
    data$riskscore[data$diabetic == 1 & !is.na(data$diabetic)] <- data$riskscore[data$diabetic == 1 & !is.na(data$diabetic)] + 2
    data$riskscore[data$diabetic == 0 & !is.na(data$diabetic)] <- data$riskscore[data$diabetic == 0 & !is.na(data$diabetic)]


    ### Number of Vascular Beds

    data$riskscore[is.na(data$vasc)] <- data$riskscore[is.na(data$vasc)]
    data$riskscore[data$vasc == 1 & !is.na(data$vasc)] <- data$riskscore[data$vasc == 1 & !is.na(data$vasc)] + 2
    data$riskscore[data$vasc == 2 & !is.na(data$vasc)] <- data$riskscore[data$vasc == 2 & !is.na(data$vasc)] + 4
    data$riskscore[data$vasc == 3 & !is.na(data$vasc)] <- data$riskscore[data$vasc == 3 & !is.na(data$vasc)] + 6


    ### CV Event im letzten jahr

    data$riskscore[is.na(data$cv_event)] <- data$riskscore[is.na(data$cv_event)]
    data$riskscore[data$cv_event == 1 & !is.na(data$cv_event)] <- data$riskscore[data$cv_event == 1 & !is.na(data$cv_event)] + 2
    data$riskscore[data$cv_event == 0 & !is.na(data$cv_event)] <- data$riskscore[data$cv_event == 0 & !is.na(data$cv_event)]


    ### Congestive Heart failure Herzinsuffizienz
    data$riskscore[is.na(data$chf)] <- data$riskscore[is.na(data$chf)]
    data$riskscore[data$chf == 1 & !is.na(data$chf)] <- data$riskscore[data$chf == 1 & !is.na(data$chf)] + 3
    data$riskscore[data$chf == 0 & !is.na(data$chf)] <- data$riskscore[data$chf == 0 & !is.na(data$chf)]



    ### Atrial fibrillation /Vorhofflimmern
    data$riskscore[is.na(data$af)] <- data$riskscore[is.na(data$af)]
    data$riskscore[data$af == 1 & !is.na(data$af)] <- data$riskscore[data$af == 1 & !is.na(data$af)] + 2
    data$riskscore[data$af == 0 & !is.na(data$af)] <- data$riskscore[data$af == 0 & !is.na(data$af)]


    ### Statine therapy
    data$riskscore[is.na(data$statin)] <- data$riskscore[is.na(data$statin)]
    data$riskscore[data$statin == 1 & !is.na(data$statin)] <- data$riskscore[data$statin == 1 & !is.na(data$statin)] - 2
    data$riskscore[data$statin == 0 & !is.na(data$statin)] <- data$riskscore[data$statin == 0 & !is.na(data$statin)]


    ### ASS therapy
    data$riskscore[is.na(data$asa)] <- data$riskscore[is.na(data$asa)]
    data$riskscore[data$asa == 1 & !is.na(data$ass)] <- data$riskscore[data$asa == 1 & !is.na(data$ass)] - 1
    data$riskscore[data$asa == 0 & !is.na(data$ass)] <- data$riskscore[data$asa == 0 & !is.na(data$ass)]


    ### Eastern Europe or Middle East

    data$riskscore[data$region_EE_or_ME == TRUE] <- data$riskscore[data$region_EE_or_ME == TRUE] + 2
    data$riskscore[data$region_EE_or_ME == FALSE] <- data$riskscore[data$region_EE_or_ME == FALSE]


    ### Japan or Australia

    data$riskscore[data$region_jap_aust == TRUE] <- data$riskscore[data$region_jap_aust == TRUE] - 2
    data$riskscore[data$region_jap_aust == FALSE] <- data$riskscore[data$region_jap_aust == FALSE]


    ## Risikoscore > 29 immer auf 29 setzen, da keine weiteren Risikowerte bekannt

    data$riskscore[data$riskscore > 29] <- 29
    data$riskscore[data$riskscore < 0] <- 0

    #### Tabelle Next CV Event basierend auf Risikoscore

    df <- data.frame(interger_risk_score = c(0:29),
                     risk_20month = c(0,1,1.2,1.4,1.6,1.9,2.2,2.5,3,3.5,4,4.7,5.4,6.3,7.3,8.5,9.8,11,13,15,17,20,23,26,30,34,38,43,48,50))

    ### Rückgabe des 20M Sterberisiko auf Basis des Riskscores

    data$score <- df[match(data$riskscore, df[,1]),2]


    return(data$score)

}
#' @export
reach_score_cv_death <- function(sex, age, bmi=NA, diabetic=NA, smoker=NA, vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, asa=NA, region_EE_or_ME = FALSE, region_jap_aust = FALSE){


    if (!all(sex %in% c("male", "female")) | missing(sex)) {
        stop("sex must be either 'male' or 'female'")
    }

    if (!is.numeric(age) |  missing(age)) {
        stop("age must be a valid numeric value")
    }

    if (any(!is.na(bmi)) & any(!is.numeric(bmi))) {
        stop("bmi must be a valid value. Numeric or NA")
    }

    if (!all(diabetic %in% c(0,1,NA))) {
        stop("diabetic must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(smoker %in% c(0,1,NA))) {
        stop("smoker must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(vasc %in% c(0,1,2,3,NA))) {
        stop("vasc must be either 1,2,3 or NA (missing)")
    }

    if (!all(cv_event %in% c(0,1,NA))) {
        stop("cv_event must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(af %in% c(0,1,NA))) {
        stop("af must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(statin %in% c(0,1,NA))) {
        stop("statin must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(asa %in% c(0,1,NA))) {
        stop("asa must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (all(!is.logical(region_EE_or_ME)) | all(!is.logical(region_jap_aust))) {
        stop("region must be either TRUE or FALSE")
    }

    if (any(age < 45) | any(is.na(age))) {
        warning("Some age values are below the optimal age range. Risk cannot be calculated exactly.")
    }

    if (any(is.na(bmi))) {
        warning("No or some values for BMI not provided. This results in an underestimation of the score")
    }

    if (any(is.na(diabetic))) {
        warning("No or some values for diabetic status not provided. This results in an underestimation of the score")
    }

    if (any(is.na(smoker))) {
        warning("No or some values for smoker not provided. This results in an underestimation of the score")
    }

    if (any(is.na(vasc))) {
        warning("No or some values for vasc not provided. This results in an underestimation of the score")
    }

    if (any(is.na(cv_event))) {
        warning("No or some values for cv_event not provided. This results in an underestimation of the score")
    }

    if (any(is.na(af))) {
        warning("No or some values for af not provided. This results in an underestimation of the score")
    }

    if (any(is.na(statin))) {
        warning("No or some values for statin not provided. This results in an underestimation of the score")
    }

    if (any(is.na(asa))) {
        warning("No or some values for asa not provided. This results in an underestimation of the score")
    }



    data <- data.frame(sex = sex, age = age, smoker = smoker, diabetic = diabetic, bmi = bmi, vasc = vasc, cv_event = cv_event, chf = chf, af = af,
                       statin = statin, asa = asa, region_EE_or_ME = region_EE_or_ME, region_jap_aust = region_jap_aust)

    data$riskscore <- 0

    #CV Death

    ### Points for sex (0=male / 1=female)

    data$riskscore[is.na(data$sex)] <- data$riskscore[is.na(data$sex)]
    data$riskscore[data$sex == "female" & !is.na(data$sex)] <- data$riskscore[data$sex == "female" & !is.na(data$sex)] + 0
    data$riskscore[data$sex == "male" & !is.na(data$sex)] <- data$riskscore[data$sex == "male" & !is.na(data$sex)] + 1

    ### age

    data$riskscore[is.na(data$age)] <- data$riskscore[is.na(data$age)]
    data$riskscore[data$age < 25 & !is.na(data$age)] <- data$riskscore[data$age < 25 & !is.na(data$age)] + 0
    data$riskscore[data$age >= 25 & data$age < 30 & !is.na(data$age)] <- data$riskscore[data$age >= 25 & data$age < 30 & !is.na(data$age)] + 1
    data$riskscore[data$age >= 30 & data$age < 35 & !is.na(data$age)] <- data$riskscore[data$age >= 30 & data$age < 35 & !is.na(data$age)] + 2
    data$riskscore[data$age >= 35 & data$age < 40 & !is.na(data$age)] <- data$riskscore[data$age >= 35 & data$age < 40 & !is.na(data$age)] + 3
    data$riskscore[data$age >= 40 & data$age < 45 & !is.na(data$age)] <- data$riskscore[data$age >= 40 & data$age < 45 & !is.na(data$age)] + 4
    data$riskscore[data$age >= 45 & data$age < 50 & !is.na(data$age)] <- data$riskscore[data$age >= 45 & data$age < 50 & !is.na(data$age)] + 5
    data$riskscore[data$age >= 50 & data$age < 55 & !is.na(data$age)] <- data$riskscore[data$age >= 50 & data$age < 55 & !is.na(data$age)] + 6
    data$riskscore[data$age >= 55 & data$age < 60 & !is.na(data$age)] <- data$riskscore[data$age >= 55 & data$age < 60 & !is.na(data$age)] + 7
    data$riskscore[data$age >= 60 & data$age < 65 & !is.na(data$age)] <- data$riskscore[data$age >= 60 & data$age < 65 & !is.na(data$age)] + 8
    data$riskscore[data$age >= 65 & data$age < 70 & !is.na(data$age)] <- data$riskscore[data$age >= 65 & data$age < 70 & !is.na(data$age)] + 9
    data$riskscore[data$age >= 70 & data$age < 75 & !is.na(data$age)] <- data$riskscore[data$age >= 70 & data$age < 75 & !is.na(data$age)] + 10
    data$riskscore[data$age >= 75 & data$age < 80 & !is.na(data$age)] <- data$riskscore[data$age >= 75 & data$age < 80 & !is.na(data$age)] + 11
    data$riskscore[data$age >= 80 & data$age < 85 & !is.na(data$age)] <- data$riskscore[data$age >= 80 & data$age < 85 & !is.na(data$age)] + 12
    data$riskscore[data$age >= 85 & !is.na(data$age)] <- data$riskscore[data$age >= 85 & !is.na(data$age)] + 13


    ### BMI

    data$riskscore[is.na(data$bmi)] <- data$riskscore[is.na(data$bmi)]
    data$riskscore[data$bmi < 20 & !is.na(data$bmi)] <- data$riskscore[data$bmi < 20 & !is.na(data$bmi)] + 2
    data$riskscore[data$bmi >= 20 & !is.na(data$bmi)] <- data$riskscore[data$bmi >= 20 & !is.na(data$bmi)] + 0



    ### Raucher (Ex Raucher unter 6 Monate)


    data$riskscore[is.na(data$smoker)] <- data$riskscore[is.na(data$smoker)]
    data$riskscore[data$smoker == 1 & !is.na(data$smoker)] <- data$riskscore[data$smoker == 1 & !is.na(data$smoker)] + 1
    data$riskscore[data$smoker == 0 & !is.na(data$smoker)] <- data$riskscore[data$smoker == 0 & !is.na(data$smoker)] + 0


    ### Diabetes
    data$riskscore[is.na(data$diabetic)] <- data$riskscore[is.na(data$diabetic)]
    data$riskscore[data$diabetic == 1 & !is.na(data$diabetic)] <- data$riskscore[data$diabetic == 1 & !is.na(data$diabetic)] + 2
    data$riskscore[data$diabetic == 0 & !is.na(data$diabetic)] <- data$riskscore[data$diabetic == 0 & !is.na(data$diabetic)] + 0


    ### Number of Vascular Beds

    data$riskscore[is.na(data$vasc)] <- data$riskscore[is.na(data$vasc)] + 0
    data$riskscore[data$vasc == 0 & !is.na(data$vasc)] <- data$riskscore[data$vasc == 0 & !is.na(data$vasc)] + 0
    data$riskscore[data$vasc == 1 & !is.na(data$vasc)] <- data$riskscore[data$vasc == 1 & !is.na(data$vasc)] + 1
    data$riskscore[data$vasc == 2 & !is.na(data$vasc)] <- data$riskscore[data$vasc == 2 & !is.na(data$vasc)] + 2
    data$riskscore[data$vasc == 3 & !is.na(data$vasc)] <- data$riskscore[data$vasc == 3 & !is.na(data$vasc)] + 3


    ### CV Event im letzten jahr

    data$riskscore[is.na(data$cv_event)] <- data$riskscore[is.na(data$cv_event)]
    data$riskscore[data$cv_event == 1 & !is.na(data$cv_event)] <- data$riskscore[data$cv_event == 1 & !is.na(data$cv_event)] + 1
    data$riskscore[data$cv_event == 0 & !is.na(data$cv_event)] <- data$riskscore[data$cv_event == 0 & !is.na(data$cv_event)] + 0


    ### Congestive Heart failure Herzinsuffizienz
    data$riskscore[is.na(data$chf)] <- data$riskscore[is.na(data$chf)] + 0
    data$riskscore[data$chf == 1 & !is.na(data$chf)] <- data$riskscore[data$chf == 1 & !is.na(data$chf)] + 4
    data$riskscore[data$chf == 0 & !is.na(data$chf)] <- data$riskscore[data$chf == 0 & !is.na(data$chf)] + 0



    ### Atrial fibrillation /Vorhofflimmern
    data$riskscore[is.na(data$af)] <- data$riskscore[is.na(data$af)] + 0
    data$riskscore[data$af == 1 & !is.na(data$af)] <- data$riskscore[data$af == 1 & !is.na(data$af)] + 2
    data$riskscore[data$af == 0 & !is.na(data$af)] <- data$riskscore[data$af == 0 & !is.na(data$af)] + 0


    ### Statine therapy
    data$riskscore[is.na(data$statin)] <- data$riskscore[is.na(data$statin)]
    data$riskscore[data$statin == 1 & !is.na(data$statin)] <- data$riskscore[data$statin == 1 & !is.na(data$statin)] - 1
    data$riskscore[data$statin == 0 & !is.na(data$statin)] <- data$riskscore[data$statin == 0 & !is.na(data$statin)] + 0


    ### ASS therapy
    data$riskscore[is.na(data$asa)] <- data$riskscore[is.na(data$asa)]
    data$riskscore[data$asa == 1 & !is.na(data$asa)] <- data$riskscore[data$asa == 1 & !is.na(data$asa)] - 1
    data$riskscore[data$asa == 0 & !is.na(data$asa)] <- data$riskscore[data$asa == 0 & !is.na(data$asa)] + 0


    ### Eastern Europe or Middle East

    data$riskscore[data$region_EE_or_ME == TRUE] <- data$riskscore[data$region_EE_or_ME == TRUE] + 1
    data$riskscore[data$region_EE_or_ME == FALSE] <- data$riskscore[data$region_EE_or_ME == FALSE] + 0


    ### Japan or Australia

    data$riskscore[data$region_jap_aust == TRUE] <- data$riskscore[data$region_jap_aust == TRUE] - 3
    data$riskscore[data$region_jap_aust == FALSE] <- data$riskscore[data$region_jap_aust == FALSE] + 0


    ## Risikoscore >= 26 immer auf 26 setzen und 0-8 auf 8 setzen, da keine weiteren Risikowerte bekannt

    data$riskscore[data$riskscore > 26] <- 26
    data$riskscore[data$riskscore <= 8] <- 8

    #### Tabelle CV death basierend auf Risikoscore

    df <- data.frame(interger_risk_score = c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
                     CV_death = c(0,1.1,1.4,1.8,2.3,3,3.8,4.9,6.2,7.9,10,13,16,20,25,30,37,45,50))

    ### Auswahl ob Anzeige in Prozent oder dezimal

    data$score <- df[match(data$riskscore, df[,1]),2]

    return(data$score)

}
#' @export
reach_score_cv_death_formula <- function(sex, age, bmi=NA, diabetic=NA, smoker=NA, vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, asa=NA, region_EE_or_ME = FALSE, region_jap_aust = FALSE){


    if (!all(sex %in% c("male", "female")) | missing(sex)) {
        stop("sex must be either 'male' or 'female'")
    }

    if (!is.numeric(age) |  missing(age)) {
        stop("age must be a valid numeric value")
    }

    if (any(!is.na(bmi)) & any(!is.numeric(bmi))) {
        stop("bmi must be a valid value. Numeric or NA")
    }

    if (!all(diabetic %in% c(0,1,NA))) {
        stop("diabetic must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(smoker %in% c(0,1,NA))) {
        stop("smoker must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(vasc %in% c(1,2,3,NA))) {
        stop("vasc must be either 1,2,3 or NA (missing)")
    }

    if (!all(cv_event %in% c(0,1,NA))) {
        stop("cv_event must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(af %in% c(0,1,NA))) {
        stop("af must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(statin %in% c(0,1,NA))) {
        stop("statin must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(asa %in% c(0,1,NA))) {
        stop("asa must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (all(!is.logical(region_EE_or_ME)) | all(!is.logical(region_jap_aust))) {
        stop("region must be either TRUE or FALSE")
    }

    if (any(age < 45) | any(is.na(age))) {
        warning("Some age values are below the optimal age range. Risk cannot be calculated exactly.")
    }

    if (any(is.na(bmi))) {
        warning("No or some values for BMI not provided. This results in an underestimation of the score")
    }

    if (any(is.na(diabetic))) {
        warning("No or some values for diabetic status not provided. This results in an underestimation of the score")
    }

    if (any(is.na(smoker))) {
        warning("No or some values for smoker not provided. This results in an underestimation of the score")
    }

    if (any(is.na(vasc))) {
        warning("No or some values for vasc not provided. This results in an underestimation of the score")
    }

    if (any(is.na(cv_event))) {
        warning("No or some values for cv_event not provided. This results in an underestimation of the score")
    }

    if (any(is.na(af))) {
        warning("No or some values for af not provided. This results in an underestimation of the score")
    }

    if (any(is.na(statin))) {
        warning("No or some values for statin not provided. This results in an underestimation of the score")
    }

    if (any(is.na(asa))) {
        warning("No or some values for asa not provided. This results in an underestimation of the score")
    }



    data <- data.frame(sex = sex, age = age, smoker = smoker, diabetic = diabetic, bmi = bmi, vasc = vasc, cv_event = cv_event, chf = chf, af = af,
                       statin = statin, asa = asa, region_EE_or_ME = region_EE_or_ME, region_jap_aust = region_jap_aust)


    ## factorize BMI

    data$bmi <- ifelse(data$bmi < 20, 1, 0)
    data$sex[data$sex == "female"] <- 0
    data$sex[data$sex == "male"] <- 1
    data$sex <- as.numeric(data$sex)
    data$region_EE_or_ME[data$region_EE_or_ME == TRUE] <- 1
    data$region_EE_or_ME[data$region_EE_or_ME == FALSE] <- 0
    data$region_jap_aust[data$region_jap_aust == TRUE] <- 1
    data$region_jap_aust[data$region_jap_aust == FALSE] <- 0

    ## set all NAs to 0
    data[is.na(data)] <- 0


    sum_coefs <- data$sex * reach_cvdeath_coefficients$sex_coef +
        data$age * reach_cvdeath_coefficients$age_coef +
        data$smoker * reach_cvdeath_coefficients$smoker_coef +
        data$diabetic * reach_cvdeath_coefficients$diab_coef +
        data$bmi * reach_cvdeath_coefficients$bmi_coef +
        data$vasc * reach_cvdeath_coefficients$vasc_coef +
        data$cv_event * reach_cvdeath_coefficients$cv_event_coef +
        data$chf * reach_cvdeath_coefficients$chf_coef +
        data$af * reach_cvdeath_coefficients$af_coef +
        data$statin * reach_cvdeath_coefficients$statin_coef +
        data$asa * reach_cvdeath_coefficients$asa_coef +
        data$region_EE_or_ME * reach_cvdeath_coefficients$region_EE_or_ME_coef +
        data$region_jap_aust * reach_cvdeath_coefficients$region_jap_aust_coef

    ## Calcualtion of Risk

    cv_death <- round((1 - (reach_cvdeath_coefficients$baseline_surv^exp(sum_coefs - reach_cvdeath_coefficients$group_mean_coef)))*100,2)

    cv_death <- ifelse(cv_death < 1, 1, ifelse(cv_death > 30, 30, cv_death))

    return(cv_death)

}
#' @export
reach_score_next_cv_formula <- function(sex, age, bmi=NA, diabetic=NA, smoker=NA, vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, asa=NA, region_EE_or_ME = FALSE, region_jap_aust = FALSE){


    if (!all(sex %in% c("male", "female")) | missing(sex)) {
        stop("sex must be either 'male' or 'female'")
    }

    if (!is.numeric(age) |  missing(age)) {
        stop("age must be a valid numeric value")
    }

    if (any(!is.na(bmi)) & any(!is.numeric(bmi))) {
        stop("bmi must be a valid value. Numeric or NA")
    }

    if (!all(diabetic %in% c(0,1,NA))) {
        stop("diabetic must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(smoker %in% c(0,1,NA))) {
        stop("smoker must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(vasc %in% c(1,2,3,NA))) {
        stop("vasc must be either 1,2,3 or NA (missing)")
    }

    if (!all(cv_event %in% c(0,1,NA))) {
        stop("cv_event must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(af %in% c(0,1,NA))) {
        stop("af must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(statin %in% c(0,1,NA))) {
        stop("statin must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (!all(asa %in% c(0,1,NA))) {
        stop("asa must be either 0 (no), 1 (yes) or NA (missing)")
    }

    if (all(!is.logical(region_EE_or_ME)) | all(!is.logical(region_jap_aust))) {
        stop("region must be either TRUE or FALSE")
    }

    if (any(age < 45) | any(is.na(age))) {
        warning("Some age values are below the optimal age range. Risk cannot be calculated exactly.")
    }

    if (any(is.na(bmi))) {
        warning("No or some values for BMI not provided. This results in an underestimation of the score")
    }

    if (any(is.na(diabetic))) {
        warning("No or some values for diabetic status not provided. This results in an underestimation of the score")
    }

    if (any(is.na(smoker))) {
        warning("No or some values for smoker not provided. This results in an underestimation of the score")
    }

    if (any(is.na(vasc))) {
        warning("No or some values for vasc not provided. This results in an underestimation of the score")
    }

    if (any(is.na(cv_event))) {
        warning("No or some values for cv_event not provided. This results in an underestimation of the score")
    }

    if (any(is.na(af))) {
        warning("No or some values for af not provided. This results in an underestimation of the score")
    }

    if (any(is.na(statin))) {
        warning("No or some values for statin not provided. This results in an underestimation of the score")
    }

    if (any(is.na(asa))) {
        warning("No or some values for asa not provided. This results in an underestimation of the score")
    }



    data <- data.frame(sex = sex, age = age, smoker = smoker, diabetic = diabetic, bmi = bmi, vasc = vasc, cv_event = cv_event, chf = chf, af = af,
                       statin = statin, asa = asa, region_EE_or_ME = region_EE_or_ME, region_jap_aust = region_jap_aust)

    ## factorize BMI

    data$bmi <- ifelse(data$bmi < 20, 1, 0)
    data$sex[data$sex == "male"] <- 1
    data$sex[data$sex == "female"] <- 0
    data$sex <- as.numeric(data$sex)
    data$region_EE_or_ME[data$region_EE_or_ME == TRUE] <- 1
    data$region_EE_or_ME[data$region_EE_or_ME == FALSE] <- 0
    data$region_jap_aust[data$region_jap_aust == TRUE] <- 1
    data$region_jap_aust[data$region_jap_aust == FALSE] <- 0

    ## set all NAs to 0
    data[is.na(data)] <- 0
    sum_coefs <- data$sex * reach_nextcv_coefficients$sex_coef +
        data$age * reach_nextcv_coefficients$age_coef +
        data$smoker * reach_nextcv_coefficients$smoker_coef +
        data$diabetic * reach_nextcv_coefficients$diabetic_coef +
        data$bmi * reach_nextcv_coefficients$bmi_coef +
        data$vasc * reach_nextcv_coefficients$vasc_coef +
        data$cv_event * reach_nextcv_coefficients$cv_event_coef +
        data$chf * reach_nextcv_coefficients$chf_coef +
        data$af * reach_nextcv_coefficients$af_coef +
        data$statin * reach_nextcv_coefficients$statin_coef +
        data$asa * reach_nextcv_coefficients$asa_coef +
        data$region_EE_or_ME * reach_nextcv_coefficients$region_EE_or_ME_coef +
        data$region_jap_aust * reach_nextcv_coefficients$region_jap_aust_coef

    ## Calcualtion of Risk

    cv_death <- round((1 - (reach_nextcv_coefficients$baseline_surv^exp(sum_coefs - reach_nextcv_coefficients$group_mean)))*100,2)

    cv_death <- ifelse(cv_death < 1, 1, ifelse(cv_death > 30, 30, cv_death))

    return(cv_death)

}
