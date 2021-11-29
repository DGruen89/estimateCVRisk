# estimateCVRisk

Cardiovascular diseases (CVDs) are among the leading causes of death globaly. In 2019 an estimated 17.9 million people died from CVDs, representing 32% of all global deaths ([Source: WHO](https://www.who.int/en/news-room/fact-sheets/detail/cardiovascular-diseases-(cvds))). Of these deaths, 85% were due to heart attack and stroke. Hence, it is important to estimate cardiovascular risk as early as possible e.g. facilitating a timely and personalized treatment. 
To estimate individual CVD risk, guidelines of the respective medical societies recommend diffent risk calculators such as the ASCVD Risk estimator (American College of Cardiology (ACC) / American Heart Association (AHA); https://www.ahajournals.org/doi/10.1161/CIR.0000000000000678) or the SCORE system (European Society of Cardiology (ESC); https://doi.org/10.1093/eurheartj/ehab484).<br/>
The **aim** of this package is to provide a tool for CVD risk calculation in the context of primary and secondary prevention integrating various risk scores for e.g. prospective risk analyses in clinical cohorts or comparisms of different scores in these cohorts. The usage of R provides an easily and fast way to calculate the scores even in large Dataset. 

## About the package

estimateCVRisk is a package which allows you to calculate different risk scores for CVD risk estimation.<br/>
Currently available riskscore functions in the package:<br/><br/>

**Primary Prevention**<br/>
- Systematic COronary Risk Evaluation from the European Society of Cardiology (**ESC-SCORE**) (https://doi.org/10.1016/S0195-668X(03)00114-3)<br/>
- Systematic COronary Risk Evaluation in older persons (**ESC-SCORE O.P.**) (https://doi.org/10.1177/2047487315588390)<br/>
- Systematic COronary Risk Evaluation in a German Cohort (**ESC-SCORE DE**) (https://doi.org/10.1371/journal.pone.0162188)<br/>
- PROCAM I Score (https://doi.org/10.1161/hc0302.102575)<br/> 
- PROCAM II Score (https://doi.org/10.1111/j.1365-2362.2007.01888.x)<br/>
- Atherosclerotic cardiovascular disease score (**ASCVD**) Guideline from ACC/AHA on the Assessment of CV (https://doi.org/10.1161/01.cir.0000437741.48606.98)<br/>
- Framingham Risk Score to assess risk of specific CVD (**FRS-CVD**) (https://doi.org/10.1161/CIRCULATIONAHA.107.699579)<br/>
- Framingham Risk Score to assess risk of CHD (**FRS-CHD**) (https://doi.org/10.1161/01.CIR.97.18.1837)<br/>
- soon available - SCORE2 (https://doi.org/10.1093/eurheartj/ehab309)
- soon available - SCORE2-OP (https://doi.org/10.1093/eurheartj/ehab312)


**Secondary Prevention**<br/>
- REducation of Atherothrombosis for Continued Health (**REACH**) (https://doi.org/10.1016/j.amjmed.2012.01.014)<br/>
- Thrombin Receptor Antagonist in Secondary Prevention of Atherothrombotic Ischemic Events-TIMI 50 (**TRA 2°P-TIMI**)<br/><br/>


## Installation of the package in R

```
# Installation of the GitHub Version in R:

# Step 1: if not already installed you need to install R package "devtools":
install.packages("devtools")

# Step 2: install the package "estimateCVRisk" with:
devtools::install_github("DGruen89/estimateCVRisk")

```

## Usage of the package in R

```
library("estimateCVRisk")

 ascvd_acc_aha(
      race = c("white","aa","white","aa"),
      sex = c("female", "female", "male", "male"),
      age = c(55,55,55,55),
      totchol = c(213,213,213,213),
      hdl = c(50,50,50,50),
      sbp = c(120,120,120,120),
      smoker = c(0,0,0,0),
      diabetic = c(0,0,0,0),
      bp_med = c(0,0,0,0))
```

The function can be passed single values as well as vectors. The output is a vector with the same length as the input vector.
