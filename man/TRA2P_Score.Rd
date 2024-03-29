% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tra2p_Score.R
\name{tra2p_score}
\alias{tra2p_score}
\title{Calculate TRA2P-Score}
\usage{
tra2p_score(age, chf = NA, ah = NA, diabetic = NA, stroke = NA, bypass_surg = NA, other_surg = NA, egfr = NA, smoker = NA)
}
\arguments{
\item{age}{age; integer [years]}

\item{chf}{information if individual has a congestive heart failure (CHF); numeric [1|0]; ("1"=yes;"0"=no)}

\item{ah}{information if individual has arterial hyperthorphy (AF); numeric [1|0]; ("1"=yes;"0"=no)}

\item{diabetic}{diabetic status of individual; numeric [1|0]; ("1"=diabetic;"0"=non-diabetic)}

\item{stroke}{information if individual had a stroke; numeric [1|0]; ("1"=yes;"0"=no)}

\item{bypass_surg}{information if individual had undergone a bypass surgery; numeric [1|0]; ("1"=yes;"0"=no)}

\item{other_surg}{information if individual had a other vascular disease (peripheral) surgery; numeric [1|0]; ("1"=yes;"0"=no)}

\item{egfr}{estimated glomerular filtration rate (eGFR); numeric [mL x min^−1 x 1.73 m^−2]}

\item{smoker}{information on current self-reported smoking status; numeric [1|0]; ("1"=smoker;"0"=non-smoker)}
}
\value{
A vector of the calculated risk per record.
}
\description{
This function takes clinical predictors of recurrent atherothrombosis to calculate the TRA2P-Score
}
\details{
The function takes nine independent baseline clinical atherothrombotic risk indicators to calculate a score (\%) according to the 3-year risk of cardiovascular death, MI or ischemic stroke.
The score can be divided into the following categories:
\itemize{
\item{Low risk}{Score < 5\%}
\item{Intermediate risk}{Score 5\% to < 15\%}
\item{High risk}{Score >= 15\%}
}
BACKGROUND:\cr
Patients with stable ischemic heart disease and previous myocardial infarction (MI) vary in their risk for recurrent cardiovascular events.
Atherothrombotic risk assessment may be useful to identify high- risk patients who have the greatest potential to benefit from more intensive secondary preventive therapy
such as treatment with vorapaxar.\cr
METHODS:\cr
We identified independent clinical indicators of atherothrombotic risk among 8598 stable, placebo-treated patients with a previous MI followed up for 2.5 years (median)
in TRA 2°P-TIMI 50 (Thrombin Receptor Antagonist in Secondary Prevention of Atherothrombotic Ischemic Events–TIMI 50).
The efficacy and safety of vorapaxar (SCH 530348; MK-5348) were assessed by baseline risk among patients with previous MI without prior stroke or transient ischemic attack for whom
there is a clinical indication for vorapaxar. End points were cardiovascular death, MI, or ischemic stroke and GUSTO (Global Use of Strategies to Open Occluded Coronary Arteries) severe bleeding.\cr
RESULTS:\cr
The 9 independent risk predictors were age, diabetes mellitus, hypertension, smoking, peripheral arterial disease, previous stroke, previous coronary bypass grafting, heart failure, and renal dysfunction. A simple integer-based scheme using these predictors showed a strong graded relationship with the rate of cardiovascular death/MI/ischemic stroke and the individual components (P for trend <0.001 for all). High-risk patients (≥3 risk indicators; 20\% of population) had a 3.2\% absolute risk reduction in cardiovascular disease/MI/ischemic stroke with vorapaxar, and intermediate-risk patients (1–2 risk indicators; 61\%) had a 2.1\% absolute risk reduction (P<0.001 each), translating to a number needed to treat of 31 and 48. Bleeding increased across risk groups (P for trend<0.01); however, net clinical outcome was increasingly favorable with vorapaxar across risk groups. Fatal bleeding or intracranial hemorrhage was 0.9\% with both treatments in high-risk patients.
CONCLUSIONS: Stratification of baseline atherothrombotic risk can assist with therapeutic decision making for vorapaxar use for secondary prevention after MI.
}
\references{
Bohula EA, et al. Atherothrombotic Risk Stratification and the Efficacy and Safety of Vorapaxar in Patients With Stable Ischemic Heart Disease and Previous Myocardial Infarction.
Circulation. 2016 Jul 26;134(4):304-13. doi: 10.1161/CIRCULATIONAHA.115.019861. PMID: 27440003.
}
