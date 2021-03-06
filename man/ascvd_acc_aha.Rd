% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ascvd_acc_aha.R
\name{ascvd_acc_aha}
\alias{ascvd_acc_aha}
\title{ACC/AHA 2013 ASCVD risk score}
\usage{
ascvd_acc_aha(ethnicity = c("white", "aa"), sex, age,
totchol, hdl, sbp, bp_med, smoker, diabetic)
}
\arguments{
\item{ethnicity}{a character vector indicating the patients ethnicity. Value: "white", "aa" (afro american)}

\item{sex}{a character vector indicating the sex of the person. Values: "female", "male"}

\item{age}{a numeric vector with the age of persons given as years}

\item{totchol}{a numeric vector; Cholesterol values given in mg/dL}

\item{hdl}{a numeric vector; HDL Cholesterol values given in mg/dL}

\item{sbp}{a numeric vector with the systolic blood pressure of persons given as mmHg}

\item{bp_med}{a numeric vector with the information if a Patient is on a blood pressure medication. Values: yes = 1; no = 0.}

\item{smoker}{a numeric vector. Smoker = 1, non-smoker = 0. A smoker was defined as current self-reported smoker.}

\item{diabetic}{a numeric vector indicating whether a person is diabetic. Values: yes = 1; no = 0.}
}
\value{
Estimated 10-Y Risk for hard ASCVD (percent)
}
\description{
Computes 10-year risk for hard ASCVD event (defined as first occurrence of
non-fatal myocardial infarction (MI), congestive heart disease (CHD) death,
or fatal or nonfatal stroke).
}
\details{
The ASCVD (atherosclerotic cardiovascular disease) risk score is a national guideline developed by the American College of Cardiology.
It is a calculation of your 10-year risk of having a cardiovascular problem, such as a heart attack or stroke.
This risk estimate considers age, sex, ethnicity, cholesterol levels, blood pressure, medication use, diabetic status, and smoking status.
}
\references{
Goff, David C., et al. "2013 ACC/AHA guideline on the assessment of
cardiovascular risk: a report of the American College of
Cardiology/American Heart Association Task Force on Practice
Guidelines." Journal of the American College of Cardiology 63.25
Part B (2014): 2935-2959.
}
