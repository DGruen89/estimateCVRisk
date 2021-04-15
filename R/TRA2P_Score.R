#' Calculate TRA2P-Score
#'
#' This function takes necessary parameters to calculate the TRA2P-Score
#'
#' @param CHF Kategorie Ejektionsfraktion des LV (allgemein) REDCap-ID: varid_1743
#' @param AH Arterielle Hypertonie REDCap-ID: varid_559
#' @param Alter Alter REDCap-ID: varid_1891
#' @param Diabetes Diabetes Mellitus REDCap-ID: varid_558
#' @param Schlaganfall Schlaganfall/TIA REDCap-ID: varid_613
#' @param Bypass_OP  	Koronare Bypass-OP REDCap-ID: varid_584
#' @param andere_OP  	Sonstige Gefäss-Operation REDCap-ID: varid_586
#' @param eGFR  	eGFR MDRD REDCap-ID: varid_2629
#' @param Raucher Raucher REDCap-ID: varid_561
#' @param pavk PAVK REDCap-ID: varid_610
#' @param redcap_data logisch; wenn TRUE benutzt die Funktion den ihr in 'data' übergebenen Dataframe und führt die Berechnung durch. Einzelne Varibalen müssen nicht als Argumente angegeben werden.
#' @param data Datensatz mit allen Variablen aus RedCap die für die Berechung benötigt werden. Muss angegben werden wenn 'redcap_data' TRUE.
#' @usage TRA2P_Score_new_2(CHF = NA, AH = NA, Alter = NA, Diabetes = NA, Schlaganfall = NA, Bypass_OP = NA, andere_OP = NA, eGFR = NA, Raucher = NA, pavk = NA, redcap_data = FALSE, data = NULL)
#' @details Der Score sagt ein drei Jahres Risiko für MI, kardiovaskulären Tod und ischämischen Schlaganfall voraus (Bohula et al., 2016).
#' @return A vector of the calculated risk per record.
#' @export
TRA2P_Score <- function(CHF = NA, AH = NA, Alter = NA, Diabetes = NA, Schlaganfall = NA, Bypass_OP = NA, andere_OP = NA, eGFR = NA, Raucher = NA, pavk = NA, redcap_data = FALSE, data = NULL){

  if(redcap_data == TRUE) {

    Alter = data$varid_1891
    CHF = data$varid_1743
    AH = data$varid_559
    Diabetes = data$varid_558
    Schlaganfall = data$varid_613
    Bypass_OP = data$varid_584
    Raucher = data$varid_561
    pavk = data$varid_610
    eGFR = data$varid_2629
    andere_OP = data$varid_586

  }


  score_all <- c()

  for (i in 1:length(CHF)) {

    points <- 0

    if(is.na(CHF[i])) {points <- points}
    else if(CHF[i] == 2) { points <- points +1}
    else if (CHF[i] == 3){ points <- points +1}

    if(is.na(AH[i])) {points <- points}
    else if(AH[i] == 1) { points <- points +1}

    if(is.na(Alter[i])) {points <- points}
    else if(Alter[i] >= 75) { points <- points +1}

    if(is.na(Diabetes[i])) {points <- points}
    else if(Diabetes[i] == 1) { points <- points +1}

    if(is.na(Schlaganfall[i])) {points <- points}
    else if(Schlaganfall[i] == 1) { points <- points +1}

    if(is.na(Bypass_OP[i])) {points <- points}
    else if(Bypass_OP[i] == 1) { points <- points +1}

    if(is.na(andere_OP[i])) {points <- points}
    else if(andere_OP[i] == 1) { points <- points +1}

    if(is.na(eGFR[i])) {points <- points}
    else if(eGFR[i] < 60) {points <- points + 1}

    if(is.na(Raucher[i])) {points <- points}
    else if(Raucher[i] == 1) { points <- points +1}

    if(is.na(pavk[i])) {points <- points}
    else if(pavk[i] == 1) {points <- +1}

    score <- 3.5

    if(points == 1) {score <- 6.8}
    else if(points == 2) {score <- 9.9}
    else if(points == 3) {score <- 14.5}
    else if(points == 4) {score <- 21.8}
    else if(points == 5) {score <- 28.9}
    else if(points == 6) {score <- 45.3}
    else if(points >= 7) {score <- 58.6}

    score_all <- append(score_all, score)

  }

  return(score_all)

}
