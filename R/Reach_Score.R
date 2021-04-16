#' Calculate Reach-Score_Death / Reach-Score_next_CV
#'
#' This function takes necessary parameters to calculate the Reach-Score
#'
#' @param gender Geschlecht REDCap-ID:
#' @param age Alter REDCap-ID: varid_1891
#' @param bmi Body Mass Index (BMI) REDCap-ID: varid_2265
#' @param diab Diabetes Mellitus REDCap-ID: varid_558
#' @param smoker Raucher REDCap-ID: varid_561
#' @param vasc Anzahl der Gefäßbetten mit klinischer Erkrankung (Zerebrovaskuläre Krankheit (TIA, Schlaganfall), koronare Herzkrankheit, pAVK)
#' @param cv_event Kardiovaskuläres Event (Myokardinfarkt und zerebrovaskulären Ereignis oder kardiovaskulärer Tod) im letzten Jahr
#' @param chf Kategorie Ejektionsfraktion des LV (allgemein)  REDCap-ID: varid_1743
#' @param af Vorhofflimmern-/flattern REDCap-ID: varid_576
#' @param statin Statin REDCap-ID: varid_693
#' @param ass ASS REDCap-ID: varid_695
#' @param calc_cv_event logisch; wenn TRUE, werden 'CV_event' und 'vasc' automatisch berechnet. 'khk', 'mvcad', 'pavk', 'stroke', 'date_mi', 'date_invest', 'mi', müssen der Funktion zuätzlich übergeben werden
#' @param khk Koronare Herzkrankheit REDCap-ID: varid_569
#' @param mvcad Mehrgefäßerkrankung REDCap-ID: varid_1244
#' @param pavk PAVK REDCap-ID: varid_610
#' @param stroke Schlaganfall/TIA REDCap-ID: varid_613
#' @param date_mi Datum Myokardinfarkt REDCap-ID varid_2397
#' @param date_invest Datum der Untersuchung/Befragung REDCap-ID: varid_547
#' @param mi Z.n. Myokardinfarkt REDCap-ID: varid_570
#' @param redcap_data logisch; wenn TRUE benutzt die Funktion den ihr in 'data' übergebenen Dataframe und führt die Berechnung durch. Einzelne Varibalen müssen nicht als Argumente angegeben werden.
#' @param data Datensatz mit allen Variablen aus RedCap die für die Berechung benötigt werden. Muss angegben werden wenn 'redcap_data' TRUE.
#' @usage
#' reach_score_next_cv(gender=NA, age=NA, bmi=NA, diab=NA, smoker=NA, vasc=NA, cv_event=NA, chf=NA, af=NA,
#' statin=NA, ass=NA, calc_cv_event = FALSE, khk = NA, mvcad = NA, pavk = NA,  stroke = NA, date_mi = NA,
#' date_invest = NA, mi = NA, redcap_data = TRUE, data = NULL)
#'
#' reach_score_death_new(gender=NA, age=NA, bmi=NA, diab=NA, smoker=NA, vasc=NA, cv_event=NA, chf=NA, af=NA,
#' statin=NA, ass=NA, calc_cv_event = FALSE, khk = NA, mvcad = NA, pavk = NA,  stroke = NA, date_mi = NA,
#' date_invest = NA, mi = NA, redcap_data = TRUE, data = NULL)
#' @details Prognosemodell entwickelt zur Sekundärprävention für Patienten mit etablierten kardiovaskulären Erkrankungen, zur Vorhersage von kardiovaskulären Ereignissen oder Tod über einen Zeitraum von 20 Monaten (Wilson et al., 2012).
#' Der Score wird verwendet, indem jedem Risikoindikator Punkte zugewiesen, die Punkte addiert oder subtrahiert und das prozentuale Risiko für das nächste kardiovaskuläre Ereignis oder den kardiovaskulären Tod bestimmt werden.
#' Der Score wurde aufgeteilt in zwei einzelne Scores, die zum einen das kardiovaskuläre Risiko und den Tod nach 20 Monaten berechnen. Das prozentuale Risiko unterscheidet sich in beiden Scores.
#'
#' @note
#' Die Funktion unterstützt mehrere Varianten für die Berechung der Scores:
#'
#' \itemize{
#'   \item{Variante 1 (calc_cv_event = FALSE & redcap_data = FALSE):}{
#'
#'   Alle Argumente von 'gender' bis 'ass' müssen der Funktion übergeben werden. 'cv_event' und 'vasc' muss manuell berechnet werden, da keine entsprechenden Felder in REDCap vorhanden sind.
#'   }
#'   \item{Variante 2 (calc_cv_event = TRUE & redcap_data = FALSE):}{
#'
#'   'cv_event' und 'vasc' werden von der Funktion berechnet. Zusätzlich müssen aber die Argumente 'khk', 'mvcad', 'pavk', 'stroke', 'date_mi', 'date_invest', 'mi' der Funktion übergeben werden.
#'   }
#'   \item{Variante 3 (calc_cv_event = FALSE & redcap_data = TRUE):}{
#'
#'   Wenn 'redcap_data' TRUE und ein Dataframe mit allen benötigten Variablen aus REDCAp (alle in den Argumenten beschriebenen varids) übergeben wird, berechnet die Funktion die Scores ohne das weitere Argumente angegeben werden müssen.
#'   }
#'  }
#' @return A vector of the calculated risk per record.
#' @export
reach_score_next_cv <- function(gender=NA, age=NA, bmi=NA, diab=NA, smoker=NA, vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, ass=NA, calc_cv_event = FALSE, khk = NA, mvcad = NA, pavk = NA,  stroke = NA, date_mi = NA, date_invest = NA, mi = NA, redcap_data = FALSE, data = NULL){

  if(redcap_data == TRUE) {

    gender = data$varid_549
    age = data$varid_1891
    bmi = data$varid_2265
    diab = data$varid_558
    smoker = data$varid_561
    chf = data$varid_1743
    af = data$varid_576
    statin = data$varid_693
    ass = data$varid_695
    khk = data$varid_569
    mvcad = data$varid_1244
    pavk = data$varid_610
    stroke = data$varid_613
    date_mi = data$varid_2397
    date_invest = data$varid_547
    mi = data$varid_570

  }

  if(calc_cv_event == TRUE | redcap_data == TRUE) {

    KHKges <- NA

    ### KHK zusammenführen nach und vor Angiogramm

    KHKges[khk == 0 & mvcad == 2] <- 0
    KHKges[khk == 0 & mvcad == 3] <- 1
    KHKges[khk == 0 & mvcad == 4] <- 1
    KHKges[khk == 0 & mvcad == 5] <- 1
    KHKges[khk == 0 & mvcad == 6] <- 1
    KHKges[khk == 0 & mvcad == 7] <- 1
    KHKges[khk == 1 & mvcad == 2] <- 0
    KHKges[khk == 1 & mvcad == 3] <- 1
    KHKges[khk == 1 & mvcad == 4] <- 1
    KHKges[khk == 1 & mvcad == 5] <- 1
    KHKges[khk == 1 & mvcad == 6] <- 1
    KHKges[khk == 1 & mvcad == 7] <- 1

    KHKges <- ifelse(is.na(mvcad), khk, KHKges)

    ### erstllen der Spalte cv_event fÃ¼r die Funktion. cv_event 1 wenn ... oder ... 1, sonst 0


    #Erstellen Tabelle für Vasc Beds
    vasc[KHKges ==1 & pavk==1 & stroke==1] <- 3
    vasc[KHKges == 0  & pavk == 1 & stroke == 1] <- 2
    vasc[KHKges == 1 & pavk == 0 & stroke == 1] <- 2
    vasc[KHKges == 1 & pavk == 1 & stroke == 0] <- 2
    vasc[KHKges == 0 & pavk == 0 & stroke == 1] <- 1
    vasc[KHKges == 0 & pavk == 1 & stroke == 0] <- 1
    vasc[KHKges == 1 & pavk == 0 & stroke == 0] <- 1
    vasc[KHKges == 0 & pavk == 0 & stroke == 0] <- 0

    #CV Event im letzten Jahr - Zeitraum erstellen für Myokardinfarkt
    date_mi <-as.Date(date_mi)

    cv_eventpastyear <- ifelse((as.Date(date_invest) - as.Date(date_mi)) <= 365 , 1, 0 ) # | (as.Date(data$varid_547) - as.Date(data$varid_614)) <= 365

    #erstellen der Spalte cv_event fuer die Funktion. cv_event 1 wenn ... oder ... 1, sonst 0

    cv_event <- ifelse(mi == 1 | stroke == 1, 1, 0)

    cv_event <- ifelse((cv_eventpastyear == 1 & cv_event == 1) | stroke == 1, 1, 0)


  }

  risk_all <- c()

  for (i in 1:length(gender)) {

    riskscore <- 0

    #NEXT CV EVENT

    ### Punkte fÃ¼r Geschlecht (2=männlich / 3=weiblich)
    if(is.na(gender[i])) {riskscore <- riskscore}
    else if(gender[i] == 2) {riskscore <- riskscore +1}
    else if(gender[i] == 3) {riskscore <- riskscore +0}
    else if(gender[i] == 88) {riskscore <- riskscore +0}
    else if(gender[i] == 99) {riskscore <-riskscore +0}


    ### age
    if(is.na(age[i])) {riskscore <- riskscore}
    else if(age[i] < 25) {riskscore <- riskscore +0}
    else if(age[i] >= 25 & age[i] < 30) {riskscore <- riskscore +1}
    else if(age[i] >= 30 & age[i] < 35) {riskscore <- riskscore +2}
    else if(age[i] >= 35 & age[i] < 40) {riskscore <- riskscore +3}
    else if(age[i] >= 40 & age[i] < 45) {riskscore <- riskscore +4}
    else if(age[i] >= 45 & age[i] < 50) {riskscore <- riskscore +5}
    else if(age[i] >= 50 & age[i] < 55) {riskscore <- riskscore +6}
    else if(age[i] >= 55 & age[i] < 60) {riskscore <- riskscore +7}
    else if(age[i] >= 60 & age[i] < 65) {riskscore <- riskscore +8}
    else if(age[i] >= 65 & age[i] < 70) {riskscore <- riskscore +9}
    else if(age[i] >= 70 & age[i] < 75) {riskscore <- riskscore +10}
    else if(age[i] >= 75 & age[i] < 80) {riskscore <- riskscore +11}
    else if(age[i] >= 80 & age[i] < 85) {riskscore <- riskscore +12}
    else if(age[i] >= 85) {riskscore <- riskscore +13}



    ### Punkte fÃ¼r BMI
    if(is.na(bmi[i])) {riskscore <- riskscore}
    else if(bmi[i] > 20) {riskscore <- riskscore +0}
    else if(bmi[i] <= 20) {riskscore <- riskscore +2}
    else if(bmi[i] == 88) {riskscore <- riskscore +0}
    else if(bmi[i] == 99) {riskscore <-riskscore +0}




    ### Raucher (Ex Raucher unter 6 Monate)
    if(is.na(smoker[i])) {riskscore <- riskscore}
    else if(smoker[i] == 1) {riskscore <- riskscore +2}
    else if(smoker[i] == 0) {riskscore <- riskscore +0}
    else if(smoker[i] == 88) {riskscore <- riskscore +0}
    else if(smoker[i] == 99) {riskscore <-riskscore +0}
    else if(smoker[i] == 2) {riskscore <- riskscore +0}


    ### Diabetes
    if(is.na(diab[i])) {riskscore <- riskscore}
    else if(diab[i] == 1) {riskscore <- riskscore +2}
    else if(diab[i] == 0) {riskscore <- riskscore +0}
    else if(diab[i] == 88) {riskscore <- riskscore +0}
    else if(diab[i] == 99) {riskscore <-riskscore +0}


    ### Number of Vascular Beds

    ###varid_569 = KHK varid_610= PAVK  varid_613=schlaganfall/TIA


    if(is.na(vasc[i])) {riskscore <- riskscore}
    else if(vasc[i] == 1) {riskscore <- riskscore +2}
    else if(vasc[i] == 2) {riskscore <- riskscore +4}
    else if(vasc[i] == 3) {riskscore <- riskscore +6}
    else if(vasc[i] == 88) {riskscore <- riskscore +0}
    else if(vasc[i] == 99) {riskscore <-riskscore +0}



    ### CV Event im letzten jahr
    if(is.na(cv_event[i])) {riskscore <- riskscore}
    else if(cv_event[i] == 1) {riskscore <- riskscore +2}
    else if(cv_event[i] == 0) {riskscore <- riskscore +0}

    ### Congestive Heart failure Herzinsuffizienz
    if(is.na(chf[i])) {riskscore <- riskscore}
    else if(chf[i] == 2) {riskscore <- riskscore +3}
    else if(chf[i] == 3) {riskscore <- riskscore +3}
    else if(chf[i] == 4) {riskscore <- riskscore +0}
    else if(chf[i] == 5) {riskscore <- riskscore +0}
    else if(chf[i] == 88) {riskscore <- riskscore +0}
    else if(chf[i] == 99) {riskscore <-riskscore +0}


    ### Atrial fibrillation /Vorhofflimmern
    if(is.na(af[i])) {riskscore <- riskscore}
    else if(af[i] == 1) {riskscore <- riskscore +2}
    else if(af[i] == 0) {riskscore <- riskscore +0}
    else if(af[i] == 88) {riskscore <- riskscore +0}
    else if(af[i] == 99) {riskscore <-riskscore +0}


    ### Statine therapy
    if(is.na(statin[i])) {riskscore <- riskscore}
    else if(statin[i] == 1) {riskscore <- riskscore -2}
    else if(statin[i] == 0) {riskscore <- riskscore + 0}
    else if(statin[i] == 88) {riskscore <- riskscore +0}
    else if(statin[i] == 99) {riskscore <-riskscore +0}


    ### ASS therapy
    if(is.na(ass[i])) {riskscore <- riskscore}
    else if(ass[i] == 1) {riskscore <- riskscore -1}
    else if(ass[i] == 0) {riskscore <- riskscore + 0}
    else if(ass[i] == 88) {riskscore <- riskscore +0}
    else if(ass[i] == 99) {riskscore <-riskscore +0}


    ## Risikoscore > 29 immer auf 29 setzen, da keine weiteren Risikowerte bekannt

    if(riskscore >29){riskscore <- 29}
    if(riskscore <0){riskscore <- 0}

    #### Tabelle Next CV Event basierend auf Risikoscore

    df <- data.frame(interger_risk_score = c(0:29),
                     risk_20month = c(0,1,1.2,1.4,1.6,1.9,2.2,2.5,3,3.5,4,4.7,5.4,6.3,7.3,8.5,9.8,11,13,15,17,20,23,26,30,34,38,43,48,50))

    ### RÃ¼ckgabe des 20M Sterberisiko auf Basis des Riscscores

    risk <-as.numeric(df[df[,1] == riskscore,2])

    risk_all <- append(risk_all, risk)

  }

  return(risk_all)

}
#' @export
reach_score_death <- function(gender=NA, age=NA, bmi=NA, diab=NA, smoker=NA, vasc=NA, cv_event=NA, chf=NA, af=NA, statin=NA, ass=NA, calc_cv_event = FALSE, khk = NA, mvcad = NA, pavk = NA,  stroke = NA, date_mi = NA, date_invest = NA, mi = NA, redcap_data = FALSE, data = NULL){


  if(redcap_data == TRUE) {

    gender = data$varid_549
    age = data$varid_1891
    bmi = data$varid_2265
    diab = data$varid_558
    smoker = data$varid_561
    chf = data$varid_1743
    af = data$varid_576
    statin = data$varid_693
    ass = data$varid_695
    khk = data$varid_569
    mvcad = data$varid_1244
    pavk = data$varid_610
    stroke = data$varid_613
    date_mi = data$varid_2397
    date_invest = data$varid_547
    mi = data$varid_570

  }

  if(calc_cv_event == TRUE | redcap_data == TRUE) {

    KHKges <- NA

    ### KHK zusammenführen nach und vor Angiogramm

    KHKges[khk == 0 & mvcad == 2] <- 0
    KHKges[khk == 0 & mvcad == 3] <- 1
    KHKges[khk == 0 & mvcad == 4] <- 1
    KHKges[khk == 0 & mvcad == 5] <- 1
    KHKges[khk == 0 & mvcad == 6] <- 1
    KHKges[khk == 0 & mvcad == 7] <- 1
    KHKges[khk == 1 & mvcad == 2] <- 0
    KHKges[khk == 1 & mvcad == 3] <- 1
    KHKges[khk == 1 & mvcad == 4] <- 1
    KHKges[khk == 1 & mvcad == 5] <- 1
    KHKges[khk == 1 & mvcad == 6] <- 1
    KHKges[khk == 1 & mvcad == 7] <- 1

    KHKges <- ifelse(is.na(mvcad), khk, KHKges)

    ### erstllen der Spalte cv_event fÃ¼r die Funktion. cv_event 1 wenn ... oder ... 1, sonst 0


    #Erstellen Tabelle für Vasc Beds
    vasc[KHKges ==1 & pavk==1 & stroke==1] <- 3
    vasc[KHKges == 0  & pavk == 1 & stroke == 1] <- 2
    vasc[KHKges == 1 & pavk == 0 & stroke == 1] <- 2
    vasc[KHKges == 1 & pavk == 1 & stroke == 0] <- 2
    vasc[KHKges == 0 & pavk == 0 & stroke == 1] <- 1
    vasc[KHKges == 0 & pavk == 1 & stroke == 0] <- 1
    vasc[KHKges == 1 & pavk == 0 & stroke == 0] <- 1
    vasc[KHKges == 0 & pavk == 0 & stroke == 0] <- 0

    #CV Event im letzten Jahr - Zeitraum erstellen für Myokardinfarkt
    date_mi <-as.Date(date_mi)

    cv_eventpastyear <- ifelse((as.Date(date_invest) - as.Date(date_mi)) <= 365 , 1, 0 ) # | (as.Date(data$varid_547) - as.Date(data$varid_614)) <= 365

    #erstellen der Spalte cv_event fuer die Funktion. cv_event 1 wenn ... oder ... 1, sonst 0

    cv_event <- ifelse(mi == 1 | stroke == 1, 1, 0)

    cv_event <- ifelse((cv_eventpastyear == 1 & cv_event == 1) | stroke == 1, 1, 0)


  }


  risk_all <- c()

  for (i in 1:length(gender)) {

    riskscore <- 0

    #CV death

    ### Punkte fÃ¼r gender
    if(is.na(gender[i])) {riskscore <- riskscore}
    else if(gender[i] == 2) {riskscore <- riskscore +1}
    else if(gender[i] == 3) {riskscore <- riskscore +0}
    else if(gender[i] == 88) {riskscore <- riskscore +0}
    else if(gender[i] == 99) {riskscore <-riskscore +0}

    ### age
    if(is.na(age[i])) {riskscore <- riskscore}
    else if(age[i] < 25) {riskscore <- riskscore +0}
    else if(age[i] >= 25 & age[i] < 30) {riskscore <- riskscore +1}
    else if(age[i] >= 30 & age[i] < 35) {riskscore <- riskscore +2}
    else if(age[i] >= 35 & age[i] < 40) {riskscore <- riskscore +3}
    else if(age[i] >= 40 & age[i] < 45) {riskscore <- riskscore +4}
    else if(age[i] >= 45 & age[i] < 50) {riskscore <- riskscore +5}
    else if(age[i] >= 50 & age[i] < 55) {riskscore <- riskscore +6}
    else if(age[i] >= 55 & age[i] < 60) {riskscore <- riskscore +7}
    else if(age[i] >= 60 & age[i] < 65) {riskscore <- riskscore +8}
    else if(age[i] >= 65 & age[i] < 70) {riskscore <- riskscore +9}
    else if(age[i] >= 70 & age[i] < 75) {riskscore <- riskscore +10}
    else if(age[i] >= 75 & age[i] < 80) {riskscore <- riskscore +11}
    else if(age[i] >= 80 & age[i] < 85) {riskscore <- riskscore +12}
    else if(age[i] >= 85) {riskscore <- riskscore +13}




    ### Punkte fÃ¼r bmi
    if(is.na(bmi[i])) {riskscore <- riskscore}
    else if(bmi[i] > 20) {riskscore <- riskscore +0}
    else if(bmi[i] <= 20) {riskscore <- riskscore +2}
    else if(bmi[i] == 88) {riskscore <- riskscore +0}
    else if(bmi[i] == 99) {riskscore <- riskscore +0}


    ### Raucher
    if(is.na(smoker[i])) {riskscore <- riskscore}
    else if(smoker[i] == 1) {riskscore <- riskscore +1}
    else if(smoker[i] == 0) {riskscore <- riskscore +0}
    else if(smoker[i] == 2) {riskscore <- riskscore +0}
    else if(smoker[i] == 88) {riskscore <- riskscore +0}
    else if(smoker[i] == 99) {riskscore <- riskscore +0}

    ### Diabetes
    if(is.na(diab[i])) {riskscore <- riskscore}
    else if(diab[i] == 1) {riskscore <- riskscore +2}
    else if(diab[i] == 0) {riskscore <- riskscore +0}
    else if(diab[i] == 88) {riskscore <- riskscore +0}
    else if(diab[i] == 99) {riskscore <- riskscore +0}

    ### Number of Vascular Beds
    if(is.na(vasc[i])) {riskscore <- riskscore}
    else if(vasc[i] == 1) {riskscore <- riskscore +1}
    else if(vasc[i] == 2) {riskscore <- riskscore +2}
    else if(vasc[i] == 3) {riskscore <- riskscore +3}
    else if(vasc[i] == 88) {riskscore <- riskscore +0}
    else if(vasc[i] == 99) {riskscore <-riskscore +0}

    ### CV Event im letzten jahr
    if(is.na(cv_event[i])) {riskscore <- riskscore}
    else if(cv_event[i] == 1) {riskscore <- riskscore +1}
    else if(cv_event[i] == 0) {riskscore <- riskscore +0}
    else if(cv_event[i] == 88) {riskscore <- riskscore +0}
    else if(cv_event[i] == 99) {riskscore <- riskscore +0}

    ### CHF
    if(is.na(chf[i])) {riskscore <- riskscore}
    else if(chf[i] == 2) {riskscore <- riskscore +4}
    else if(chf[i] == 3) {riskscore <- riskscore +4}
    else if(chf[i] == 4) {riskscore <- riskscore +0}
    else if(chf[i] == 5) {riskscore <- riskscore +0}
    else if(chf[i] == 88) {riskscore <- riskscore +0}
    else if(chf[i] == 99) {riskscore <- riskscore +0}


    ### Atrial fibrillation
    if(is.na(af[i])) {riskscore <- riskscore}
    else if(af[i] == 1) {riskscore <- riskscore +2}
    else if(af[i] == 0) {riskscore <- riskscore +0}
    else if(af[i] == 88) {riskscore <- riskscore +0}
    else if(af[i] == 99) {riskscore <- riskscore +0}


    ### Statine therapy
    if(is.na(statin[i])) {riskscore <- riskscore}
    else if(statin[i] == 1) {riskscore <- riskscore -1}
    else if(statin[i] == 0) {riskscore <- riskscore +0}
    else if(statin[i] == 88) {riskscore <- riskscore +0}
    else if(statin[i] == 99) {riskscore <- riskscore +0}


    ### ASS therapy
    if(is.na(ass[i])) {riskscore <- riskscore}
    else if(ass[i] == 1) {riskscore <- riskscore -1}
    else if(ass[i] == 0) {riskscore <- riskscore +0}
    else if(ass[i] == 88) {riskscore <- riskscore +0}
    else if(ass[i] == 99) {riskscore <- riskscore +0}


    ## Risikoscore > 26 immer auf 26 setzen, da keine weiteren Risikowerte bekannt

    if(riskscore >26){riskscore <- 26}
    if(riskscore < 8){riskscore <- 8}


    #### Tabelle CV death basierend auf Risikoscore

    df <- data.frame(interger_risk_score = c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
                     CV_death = c(0,1.1,1.4,1.8,2.3,3,3.8,4.9,6.2,7.9,10,13,16,20,25,30,37,45,50))

    ### Auswahl ob Anzeige in Prozent oder dezimal

    risk <- as.numeric(df[df[,1] == riskscore,2])

    risk_all <- append(risk_all, risk)

  }

  return(risk_all)

}
