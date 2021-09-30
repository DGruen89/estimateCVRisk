#' Calculate Reach-Score_Death / Reach-Score_next_CV
#'
#' This function takes necessary parameters to calculate the Reach-Score. You can choose between a calculation based on a score sheet or based on a cox model (formula).
#'
#' @param sex a numeric vector indicating the sex of the person. Values: "female" = 1, "male" = 0
#' @param age numeric; Age of person
#' @param bmi numeric; Body Mass Index in kg/m^2
#' @param diabetic numeric; Diabetes Mellitus, 1 = yes, 0 = no
#' @param smoker numeric; current Smoker (1) was defined as >= 5 cigarettes per day on average within the last month. Other (0)
#' @param vasc numeric; Number of vascular beds involved in previously diagnosed vascular disease. Number from 1 to 3
#' @param cv_event numeric; cardiovascular event in past year. 1 = yes, 0 = no
#' @param chf numeric; Cognestive heart failure. 1 = yes, 0 = no
#' @param af numeric; Atrial fibrillation. 1 = yes, 0 = no
#' @param statin numeric; Statin therapy. 1 = yes, 0 = no
#' @param ass numeric; ASS therapy. 1 = yes, 0 = no.
#' @param region_EE_or_ME numeric; Geographical region membership in East Europe or Middel East
#' @param region_jap_aust numeric; Geographical region membership in Japan or Australia
#' @usage
#' reach_score_next_cv(sex=NA, age=NA, bmi=NA, diabetic=NA, smoker=NA, vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, ass=NA,region_EE_or_ME=NA, region_jap_aust = NA)
#' reach_score_death(sex=NA, age=NA, bmi=NA, diabetic=NA, smoker=NA, vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, ass=NA, region_EE_or_ME = NA, region_jap_aust = NA)
#' reach_score_cv_death_formula(sex=NA, age=NA, smoker=NA, diabetic=NA, bmi=NA, vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, ass=NA, region_EE_or_ME=NA, region_jap_aust = NA)
#' reach_score_next_cv_formula(sex=NA, age=NA, smoker=NA, diabetic=NA, bmi=NA, vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA,ass=NA, region_EE_or_ME=NA, region_jap_aust = NA)
#' @details A risk model to predict secondary cardiovascular events and cardiovascular death in outpatients with established atherothrombotic disease.
#' Tradiotional risk factors, burden of disease, lack of treatment, and geographic location all are related to an increase risk of subsequent cardiovascular morbidity and cardiovascular mortality.
#' @references Wilson. Peter W. F., et al. "An International Model to Predict Recurrent Cardiovascular Disease." The American Journal of Medicine (2012) 125, 695-703.
#' @return A vector of the calculated risk per record.
#' @aliases
#' @export
reach_score_next_cv <- function(sex=NA, age=NA, bmi=NA, diabetic=NA, smoker=NA, vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, ass=NA, region_EE_or_ME = NA, region_jap_aust = NA){


    data <- data.frame(sex = sex, age = age, smoker = smoker, diabetic = diabetic, bmi = bmi, vasc = vasc, cv_event = cv_event, chf = chf, af = af,
                       statin = statin, ass = ass, region_EE_or_ME = region_EE_or_ME, region_jap_aust = region_jap_aust)

    data$riskscore <- 0

    #NEXT CV EVENT

    ### Points for sex (0=male / 1=female)

    data$riskscore[is.na(data$sex)] <- data$riskscore[is.na(data$sex)]
    data$riskscore[data$sex == 1] <- data$riskscore[data$sex == 1] + 0
    data$riskscore[data$sex == 0] <- data$riskscore[data$sex == 0] + 1

    ### age

    data$riskscore[is.na(data$age)] <- data$riskscore[is.na(data$age)]
    data$riskscore[data$age < 25] <- data$riskscore[data$age < 25] + 0
    data$riskscore[data$age >= 25 & data$age < 30] <- data$riskscore[data$age >= 25 & data$age < 30] + 1
    data$riskscore[data$age >= 30 & data$age < 35] <- data$riskscore[data$age >= 30 & data$age < 35] + 2
    data$riskscore[data$age >= 35 & data$age < 40] <- data$riskscore[data$age >= 35 & data$age < 40] + 3
    data$riskscore[data$age >= 40 & data$age < 45] <- data$riskscore[data$age >= 40 & data$age < 45] + 4
    data$riskscore[data$age >= 45 & data$age < 50] <- data$riskscore[data$age >= 45 & data$age < 50] + 5
    data$riskscore[data$age >= 50 & data$age < 55] <- data$riskscore[data$age >= 50 & data$age < 55] + 6
    data$riskscore[data$age >= 55 & data$age < 60] <- data$riskscore[data$age >= 55 & data$age < 60] + 7
    data$riskscore[data$age >= 60 & data$age < 65] <- data$riskscore[data$age >= 60 & data$age < 65] + 8
    data$riskscore[data$age >= 65 & data$age < 70] <- data$riskscore[data$age >= 65 & data$age < 70] + 9
    data$riskscore[data$age >= 70 & data$age < 75] <- data$riskscore[data$age >= 70 & data$age < 75] + 10
    data$riskscore[data$age >= 75 & data$age < 80] <- data$riskscore[data$age >= 75 & data$age < 80] + 11
    data$riskscore[data$age >= 80 & data$age < 85] <- data$riskscore[data$age >= 80 & data$age < 85] + 12
    data$riskscore[data$age >= 85] <- data$riskscore[data$age >= 85] + 13


    ### BMI

    data$riskscore[is.na(data$bmi)] <- data$riskscore[is.na(data$bmi)]
    data$riskscore[data$bmi < 20] <- data$riskscore[data$bmi < 20] + 2
    data$riskscore[data$bmi >= 20] <- data$riskscore[data$bmi >= 20] + 0



    ### Raucher (Ex Raucher unter 6 Monate)


    data$riskscore[is.na(data$smoker)] <- data$riskscore[is.na(data$smoker)]
    data$riskscore[data$smoker == 1] <- data$riskscore[data$smoker == 1] + 2
    data$riskscore[data$smoker == 0] <- data$riskscore[data$smoker == 0]


    ### diabeticetes
    data$riskscore[is.na(data$diabetic)] <- data$riskscore[is.na(data$diabetic)]
    data$riskscore[data$diabetic == 1] <- data$riskscore[data$diabetic == 1] + 2
    data$riskscore[data$diabetic == 0] <- data$riskscore[data$diabetic == 0]


    ### Number of Vascular Beds

    data$riskscore[is.na(data$vasc)] <- data$riskscore[is.na(data$vasc)]
    data$riskscore[data$vasc == 1] <- data$riskscore[data$vasc == 1] + 2
    data$riskscore[data$vasc == 2] <- data$riskscore[data$vasc == 2] + 4
    data$riskscore[data$vasc == 3] <- data$riskscore[data$vasc == 3] + 6


    ### CV Event im letzten jahr

    data$riskscore[is.na(data$cv_event)] <- data$riskscore[is.na(data$cv_event)]
    data$riskscore[data$cv_event == 1] <- data$riskscore[data$cv_event == 1] + 2
    data$riskscore[data$cv_event == 0] <- data$riskscore[data$cv_event == 0]


    ### Congestive Heart failure Herzinsuffizienz
    data$riskscore[is.na(data$chf)] <- data$riskscore[is.na(data$chf)]
    data$riskscore[data$chf == 1] <- data$riskscore[data$chf == 1] + 3
    data$riskscore[data$chf == 0] <- data$riskscore[data$chf == 0]



    ### Atrial fibrillation /Vorhofflimmern
    data$riskscore[is.na(data$af)] <- data$riskscore[is.na(data$af)]
    data$riskscore[data$af == 1] <- data$riskscore[data$af == 1] + 2
    data$riskscore[data$af == 0] <- data$riskscore[data$af == 0]


    ### Statine therapy
    data$riskscore[is.na(data$statin)] <- data$riskscore[is.na(data$statin)]
    data$riskscore[data$statin == 1] <- data$riskscore[data$statin == 1] - 2
    data$riskscore[data$statin == 0] <- data$riskscore[data$statin == 0]


    ### ASS therapy
    data$riskscore[is.na(data$ass)] <- data$riskscore[is.na(data$ass)]
    data$riskscore[data$ass == 1] <- data$riskscore[data$ass == 1] - 1
    data$riskscore[data$ass == 0] <- data$riskscore[data$ass == 0]


    ### Eastern Europe or Middle East

    data$riskscore[is.na(data$region_EE_or_ME)] <- data$riskscore[is.na(data$region_EE_or_ME)]
    data$riskscore[data$region_EE_or_ME == 1] <- data$riskscore[data$region_EE_or_ME == 1] + 2
    data$riskscore[data$region_EE_or_ME == 0] <- data$riskscore[data$region_EE_or_ME == 0]


    ### Japan or Australia

    data$riskscore[is.na(data$region_jap_aust)] <- data$riskscore[is.na(data$region_jap_aust)]
    data$riskscore[data$region_jap_aust == 1] <- data$riskscore[data$region_jap_aust == 1] - 2
    data$riskscore[data$region_jap_aust == 0] <- data$riskscore[data$region_jap_aust == 0]


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
reach_score_cv_death <- function(sex=NA, age=NA, bmi=NA, diabetic=NA, smoker=NA, vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, ass=NA, region_EE_or_ME = NA, region_jap_aust = NA){

    data <- data.frame(sex = sex, age = age, smoker = smoker, diabetic = diabetic, bmi = bmi, vasc = vasc, cv_event = cv_event, chf = chf, af = af,
                       statin = statin, ass = ass, region_EE_or_EM = region_EE_or_ME, region_jap_aust = region_jap_aust)

    data$riskscore <- 0

    #CV Death

    ### Points for sex (0=male / 1=female)

    data$riskscore[is.na(data$sex)] <- data$riskscore[is.na(data$sex)]
    data$riskscore[data$sex == 1] <- data$riskscore[data$sex == 1] + 1
    data$riskscore[data$sex == 0] <- data$riskscore[data$sex == 0] + 0

    ### age

    data$riskscore[is.na(data$age)] <- data$riskscore[is.na(data$age)]
    data$riskscore[data$age < 25] <- data$riskscore[data$age < 25] + 0
    data$riskscore[data$age >= 25 & data$age < 30] <- data$riskscore[data$age >= 25 & data$age < 30] + 1
    data$riskscore[data$age >= 30 & data$age < 35] <- data$riskscore[data$age >= 30 & data$age < 35] + 2
    data$riskscore[data$age >= 35 & data$age < 40] <- data$riskscore[data$age >= 35 & data$age < 40] + 3
    data$riskscore[data$age >= 40 & data$age < 45] <- data$riskscore[data$age >= 40 & data$age < 45] + 4
    data$riskscore[data$age >= 45 & data$age < 50] <- data$riskscore[data$age >= 45 & data$age < 50] + 5
    data$riskscore[data$age >= 50 & data$age < 55] <- data$riskscore[data$age >= 50 & data$age < 55] + 6
    data$riskscore[data$age >= 55 & data$age < 60] <- data$riskscore[data$age >= 55 & data$age < 60] + 7
    data$riskscore[data$age >= 60 & data$age < 65] <- data$riskscore[data$age >= 60 & data$age < 65] + 8
    data$riskscore[data$age >= 65 & data$age < 70] <- data$riskscore[data$age >= 65 & data$age < 70] + 9
    data$riskscore[data$age >= 70 & data$age < 75] <- data$riskscore[data$age >= 70 & data$age < 75] + 10
    data$riskscore[data$age >= 75 & data$age < 80] <- data$riskscore[data$age >= 75 & data$age < 80] + 11
    data$riskscore[data$age >= 80 & data$age < 85] <- data$riskscore[data$age >= 80 & data$age < 85] + 12
    data$riskscore[data$age >= 85] <- data$riskscore[data$age >= 85] + 13


    ### BMI

    data$riskscore[is.na(data$bmi)] <- data$riskscore[is.na(data$bmi)]
    data$riskscore[data$bmi < 20] <- data$riskscore[data$bmi < 20] + 2
    data$riskscore[data$bmi >= 20] <- data$riskscore[data$bmi >= 20] + 0



    ### Raucher (Ex Raucher unter 6 Monate)


    data$riskscore[is.na(data$smoker)] <- data$riskscore[is.na(data$smoker)]
    data$riskscore[data$smoker == 1] <- data$riskscore[data$smoker == 1] + 1
    data$riskscore[data$smoker == 0] <- data$riskscore[data$smoker == 0]


    ### Diabetes
    data$riskscore[is.na(data$diabetic)] <- data$riskscore[is.na(data$diabetic)]
    data$riskscore[data$diabetic == 1] <- data$riskscore[data$diabetic == 1] + 2
    data$riskscore[data$diabetic == 0] <- data$riskscore[data$diabetic == 0]


    ### Number of Vascular Beds

    data$riskscore[is.na(data$vasc)] <- data$riskscore[is.na(data$vasc)]
    data$riskscore[data$vasc == 1] <- data$riskscore[data$vasc == 1] + 1
    data$riskscore[data$vasc == 2] <- data$riskscore[data$vasc == 2] + 2
    data$riskscore[data$vasc == 3] <- data$riskscore[data$vasc == 3] + 3


    ### CV Event im letzten jahr

    data$riskscore[is.na(data$cv_event)] <- data$riskscore[is.na(data$cv_event)]
    data$riskscore[data$cv_event == 1] <- data$riskscore[data$cv_event == 1] + 1
    data$riskscore[data$cv_event == 0] <- data$riskscore[data$cv_event == 0]


    ### Congestive Heart failure Herzinsuffizienz
    data$riskscore[is.na(data$chf)] <- data$riskscore[is.na(data$chf)]
    data$riskscore[data$chf == 1] <- data$riskscore[data$chf == 1] + 4
    data$riskscore[data$chf == 0] <- data$riskscore[data$chf == 0]



    ### Atrial fibrillation /Vorhofflimmern
    data$riskscore[is.na(data$af)] <- data$riskscore[is.na(data$af)]
    data$riskscore[data$af == 1] <- data$riskscore[data$af == 1] + 2
    data$riskscore[data$af == 0] <- data$riskscore[data$af == 0]


    ### Statine therapy
    data$riskscore[is.na(data$statin)] <- data$riskscore[is.na(data$statin)]
    data$riskscore[data$statin == 1] <- data$riskscore[data$statin == 1] - 1
    data$riskscore[data$statin == 0] <- data$riskscore[data$statin == 0]


    ### ASS therapy
    data$riskscore[is.na(data$ass)] <- data$riskscore[is.na(data$ass)]
    data$riskscore[data$ass == 1] <- data$riskscore[data$ass == 1] - 1
    data$riskscore[data$ass == 0] <- data$riskscore[data$ass == 0]


    ### Eastern Europe or Middle East

    data$riskscore[is.na(data$region_EE_or_ME)] <- data$riskscore[is.na(data$region_EE_or_ME)]
    data$riskscore[data$region_EE_or_ME == 1] <- data$riskscore[data$region_EE_or_ME == 1] + 1
    data$riskscore[data$region_EE_or_ME == 0] <- data$riskscore[data$region_EE_or_ME == 0]


    ### Japan or Australia

    data$riskscore[is.na(data$region_jap_aust)] <- data$riskscore[is.na(data$region_jap_aust)]
    data$riskscore[data$region_jap_aust == 1] <- data$riskscore[data$region_jap_aust == 1] - 3
    data$riskscore[data$region_jap_aust == 0] <- data$riskscore[data$region_jap_aust == 0]


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
reach_score_cv_death_formula <- function(sex=NA, age=NA, smoker=NA, diabetic=NA, bmi=NA, vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, ass=NA, region_EE_or_ME=NA, region_jap_aust = NA){

    #utils::data(sysdata, envir = environment())

    data <- data.frame(sex = sex, age = age, smoker = smoker, diabetic = diabetic, bmi = bmi, vasc = vasc, cv_event = cv_event, chf = chf, af = af,
                       statin = statin, ass = ass, region_EE_or_ME = region_EE_or_ME, region_jap_aust = region_jap_aust)


    ## factorize BMI

    data$bmi <- ifelse(data$bmi < 20, 1, 0)

    ## set all NAs to 0
    data[is.na(data)] <- 0


    sum_coefs <- with(data, sex * sex_coef + age * age_coef + smoker * smoker_coef + diabetic * diabetic_coef + bmi * bmi_coef + vasc * vasc_coef + cv_event * cv_event_coef + chf * chf_coef +
                          af * af_coef + statin * statin_coef + ass * ass_coef + region_EE_or_ME * region_EE_or_ME_coef + region_jap_aust * region_jap_aust_coef)


    ## Calcualtion of Risk

    cv_death <- round((1 - (reach_cvdeath_coefficients$baseline_surv^exp(sum_coefs - reach_cvdeath_coefficients$baseline_surv)))*100,2)

    cv_death <- ifelse(cv_death < 1, 1, ifelse(cv_death > 30, 30, cv_death))

    return(cv_death)

}
#' @export
reach_score_next_cv_formula <- function(sex=NA, age=NA, smoker=NA, diabetic=NA, bmi=NA, vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, ass=NA, region_EE_or_ME=NA, region_jap_aust = NA){

    #utils::data(sysdata, envir = environment())

    data <- data.frame(sex = sex, age = age, smoker = smoker, diabetic = diabetic, bmi = bmi, vasc = vasc, cv_event = cv_event, chf = chf, af = af,
                       statin = statin, ass = ass, region_EE_or_ME = region_EE_or_ME, region_jap_aust = region_jap_aust)

    ## factorize BMI

    data$bmi <- ifelse(data$bmi < 20, 1, 0)

    ## set all NAs to 0
    data[is.na(data)] <- 0

    sum_coefs <- with(data, sex * sex_coef + age * age_coef + smoker * smoker_coef + diabetic * diabetic_coef + bmi * bmi_coef + vasc * vasc_coef + cv_event * cv_event_coef + chf * chf_coef +
                          af * af_coef + statin * statin_coef + ass * ass_coef + region_EE_or_ME * region_EE_or_ME_coef + region_jap_aust * region_jap_aust_coef)

    ## Calcualtion of Risk

    cv_death <- round((1 - (reach_nextcv_coefficients$baseline_surv^exp(sum_coefs - reach_nextcv_coefficients$group_mean)))*100,2)

    cv_death <- ifelse(cv_death < 1, 1, ifelse(cv_death > 30, 30, cv_death))

    return(cv_death)

}
