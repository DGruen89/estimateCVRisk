# estimateCVRisk #

Cardiovascular diseases (CVDs) are among the leading causes of death globaly. In 2019 an estimated 17.9 million people died from CVDs, representing 32% of all global deaths ([Source: WHO](https://www.who.int/en/news-room/fact-sheets/detail/cardiovascular-diseases-(cvds))). Of these deaths, 85% were due to heart attack and stroke. Hence, it is important to estimate cardiovascular risk as early as possible e.g. facilitating a timely and personalized treatment. 
To estimate individual CVD risk, guidelines of the respective medical societies recommend diffent risk calculators such as the ASCVD Risk estimator (American College of Cardiology (ACC) / American Heart Association (AHA); https://doi.org/10.1161/CIR.0000000000000678) or the SCORE system (European Society of Cardiology (ESC); https://doi.org/10.1093/eurheartj/ehab484).<br/>
The **aim** of this package is to provide a tool for CVD risk calculation in the context of primary and secondary prevention integrating various CVD risk scores for e.g. prospective risk analyses in clinical cohorts or comparison of different scores. The implementation as R package provides an easily and fast way to calculate the scores even in large Dataset. 

## About the package ##

**estimateCVRisk** is a package which allows calculation of different scores for CVD risk estimation.<br/>

Currently available risk score functions in the package:<br/><br/>

**Primary Prevention**<br/>
- Systematic COronary Risk Evaluation from the European Society of Cardiology (**ESC-SCORE**) (https://doi.org/10.1016/S0195-668X(03)00114-3)<br/>
- Systematic COronary Risk Evaluation in older persons (**ESC-SCORE O.P.**) (https://doi.org/10.1177/2047487315588390)<br/>
- Systematic COronary Risk Evaluation in a German Cohort (**ESC-SCORE DE**) (https://doi.org/10.1371/journal.pone.0162188)<br/>
- PROCAM I Score (https://doi.org/10.1161/hc0302.102575)<br/> 
- PROCAM II Score (https://doi.org/10.1111/j.1365-2362.2007.01888.x)<br/>
- Atherosclerotic cardiovascular disease score (**ASCVD**) Guideline from ACC/AHA on the Assessment of CV (https://doi.org/10.1161/01.cir.0000437741.48606.98)<br/>
- Framingham Risk Score to assess risk of specific CVD (**FRS-CVD**) (https://doi.org/10.1161/CIRCULATIONAHA.107.699579)<br/>
- Framingham Risk Score to assess risk of CHD (**FRS-CHD**) (https://doi.org/10.1161/01.CIR.97.18.1837)<br/>
- Systematic COronary Risk Evaluation 2 from the European Society of Cardiology (**ESC-SCORE2**) (https://doi.org/10.1093/eurheartj/ehab309)<br/>
- Systematic COronary Risk Evaluation 2 in older persons (**ESC-SCORE2 O.P.**) (https://doi.org/10.1093/eurheartj/ehab312)<br/>
- soon available - 10-Year Coronary Heart Disease Risk Prediction Using Coronary Artery Calcium and Traditional Risk Factors: Derivation in the MESA (Multi-Ethnic Study of Atherosclerosis)(**MESA**) (https://doi.org/10.1016/j.jacc.2015.08.035)<br/>


**Secondary Prevention**<br/>
- REducation of Atherothrombosis for Continued Health (**REACH**) (https://doi.org/10.1016/j.amjmed.2012.01.014)<br/>
- Thrombin Receptor Antagonist in Secondary Prevention of Atherothrombotic Ischemic Events-TIMI 50 (**TRA 2°P-TIMI**) (https://doi.org/10.1161/CIRCULATIONAHA.115.019861)<br/>
- INternational VErapamil‐SR/Trandolapril STudy (**INVEST**) score (https://doi.org/10.1161/JAHA.113.000205)<br/>


## Installation of the package in R ##

Installation of the GitHub Version in R:
```R
# Step 1: if not already installed, you need to install the R package "devtools":
install.packages("devtools")

# Step 2: install the "estimateCVRisk" package with:
devtools::install_github("DGruen89/estimateCVRisk")
```

## Usage of the package in R ##

First, load the the "estimateCVRisk" package with
```R
library("estimateCVRisk")
```

### Functions ###

After loading the package the following functions representing risk estimation for **primary prevention** are available

```R
ascvd_acc_aha()             # ACC/AHA 2013 ASCVD risk score for people aged 40-79 years
ascvd_frs_chd()             # Framingham risk score for coronary heart disease (CHD) for people aged 30-74 years
ascvd_frs_cvd()             # Framingham risk score for specific atherosclerotic cardiovascular disease (CVD) for people aged 30-74 years
ESC_Score_2016_table()      # table based version of the 2016 ESC risk score for people aged 40-65 years in two geographical risk regions.
ESC_Score_GER_2016_table()  # table based version of the 2016 german population specific risk score for people aged 40-65 years
ESC_Score_OP_table()        # table based version of the 2016 ESC risk score specially for people aged 65 years or older 
ESC_Score2_table()          # table based version of the 2021 ESC SCORE2 risk score for people aged 40-69 years
ESC_Score2_formula()        # formula (cox-model) based version of the 2021 ESC SCORE2 risk score for people aged 40-69 years 
ESC_Score2_OP_table()       # table based version of the 2021 ESC SCORE2 risk score for people aged 70 years or older
ESC_Score2_OP_formula()     # formula (cox-model) based version of the 2021 ESC SCORE2 risk score for people aged 70 years or older
procam_score_2002()         # table based version of procam I score for men aged 35-65 years
procam_score_2007()         # table based version of procam II score for people aged 20-75 years
```

and the following functions representing risk estimation for **secondary prevention** are available
```R
reach_score_next_cv()          # table based version of the reach score for risk estimation of cardivascular event in the next 20 month for people aged 45 years or older
reach_score_cv_death()         # table based version of the reach score for risk estimation of cardivascular death in the next 20 month for people aged 45 years or older
reach_score_next_cv_formula()  # formula (cox-model) based version of the reach score for risk estimation of cardivascular death in the next 20 month for people aged 45 years or older
reach_score_cv_death_formula() # formula (cox-model) based version of the reach score for risk estimation of cardivascular event in the next 20 month for people aged 45 years or older
tra2p_score()                  # table based version TRA 2°P-TIMI 50 risk score
invest_score()                 # table based version of the INternational VErapamil‐SR/Trandolapril STudy (INVEST) Score 
```

### Variables ###

The "estimateCVRisk" package defines the **variables** needed for risk estimation as follows:

```R
age             # age; integer [years]
sex             # gender; categorical [female|male]
ethnicity       # ethnicity; categorical [white|aa]; ("aa"=afro american)
bmi             # body mass index; numeric [kg/m^2]
totchol         # total cholesterol; numeric [mg/dl]
hdl             # high-density lipoprotein; numeric [mg/dl]
ldl             # low-density lipoprotein; numeric [mg/dl]
triglycerides   # triglycerides; numeric [mg/dl]
hr              # heart rate; numeric [beats/minute]
sbp             # systolic blood pressure; numeric [mmHg]
dbp             # diastolic blood pressure; numeric [mmHg]
bp_med          # information if individual is on a blood pressure medication; numeric [1|0]; ("1"=yes;"0"=no)
asa             # information if individual is on a acetylsalicylic acid medication; numeric [1|0]; ("1"=yes;"0"=no)
statin          # information if individual is on a statin medication; numeric [1|0]; ("1"=yes;"0"=no)
smoker          # information on current self-reported smoking status; numeric [1|0]; ("1"=smoker;"0"=non-smoker)
diabetic        # diabetic status of individual; numeric [1|0]; ("1"=diabetic;"0"=non-diabetic)
famMI           # family history of premature myocardial infarction; numeric [1|0]; ("1"=yes;"0"=no)
chf             # information if individual has a congestive heart failure (CHF); numeric [1|0]; ("1"=yes;"0"=no)
af              # information if individual has atrial fibrillation (AF); numeric [1|0]; ("1"=yes;"0"=no)
cv_event        # information if individual had a cardiovascular event in the past year; numeric [1|0]; ("1"=yes;"0"=no)
vasc            # Number of vascular beds involved in previously diagnosed vascular disease; numeric [0-3]
famMI           # information of family history of premature myocardial infarction in parents, grandparents or siblings before the age of 60 years; numeric [1|0]; ("1"=yes;"0"=no)
mi              # information of individual had a prior myocardial infarction (MI); numeric [1|0]; ("1"=yes;"0"=no)
ah              # information if individual has arterial hyperthorphy (AF); numeric [1|0]; ("1"=yes;"0"=no)
stroke          # information if individual had a stroke; numeric [1|0]; ("1"=yes;"0"=no)
pad             # information if individual has a peripheral arterial disease (PAD); numeric [1|0]; ("1"=yes;"0"=no)
ckd             # information if individual has a chronic kidney disease (CKD); numeric [1|0]; ("1"=yes;"0"=no)
bypass_surg     # information if individual had undergone a bypass surgery; numeric [1|0]; ("1"=yes;"0"=no)
other_surg      # information if individual had a other vascular disease (peripheral) surgery; numeric [1|0]; ("1"=yes;"0"=no) 
egfr            # estimated glomerular filtration rate (eGFR); numeric [mL x min^−1 x 1.73 m^−2]
region_EE_or_ME # Geographical region membership in East Europe or Middel East; logical [TRUE|FALSE] 
region_jap_aust # Geographical region membership in Japan or Australia; logical [TRUE|FALSE]

```


The different risk estimation functions require specific sets of risk variables as shown in the tables below. Further (optional) variables affecting the output of each function might be needed (e.g. with ```heart_age=TRUE``` set the function ```ascvd_frs_cvd_formula()``` will add the estimated heart age to the estimated risk score as part fo the returned dataframe).

Each risk function can be passed single values as well as vectors. The returned output is a vector with the same length as the input vector (see also section Examples).

Primary Prevention  | age | sex | ethnicity | totchol | hdl | ldl | triglycerides | sbp | dbp | bp_med | smoker | diabetic | famMI | famMI |  
------------------- | --- |-----|-----------|---------|-----|-----|---------------|-----|-----|--------|--------|----------|-------|-------|
ascvd_acc_aha       | x   | x   | x         | x       | x   |     |               | x   |     | x      | x      | x        |       |       |
ascvd_frs_chd       | x   | x   |           | x       | x   | x   |               | x   | x   |        | x      | x        |       |       |
ascvd_frs_cvd       | x   | x   |           | x       | x   |     |               | x   |     | x      | x      | x        |       |       |
ESC_Score*          | x   | x   |           | x       |(x)* |     |               | x   |     |        | x      |          |       |       |
procam_score_2002   | x   |     |           |         | x   | x   | x             | x   |     |        | x      | x        |  x    | x     |
procam_score_2007   | x   | x   |           |         | x   | x   | x             | x   |     |        | x      | x        |  x    | x     |


Secondary Prevention  | age | sex | ethnicity | bmi | hf | sbp | asa | statin | smoker | diabetic | mi | chf | af | cv_event | vasc | ah | stroke | pad | ckd | bypass_surg | other_surg | egfr | region_EE_or_ME | region_jap_aust |        
--------------------- | --- |-----|-----------|-----|----|-----|-----|--------|--------|----------|----|-----|----|----------|------|----|--------|-----|-----|-------------|------------|------|-----------------|-----------------|
Reach_Score           | x   | x   |           | x   |    |     | x   | x      | x      | x        |    | x   | x  | x        | x    |    |        |     |     |             |            |      | x               | x               |
TRA2P_Score           | x   |     |           |     |    |     |     |        | x      | x        |    | x   |    |          |      | x  | x      |     |     | x           | x          | x    |                 |                 |
INVEST_Score          | x   |     | x         | x   | x  | x   |     |        | x      | x        | x  | x   |    |          |      |    | x      | x   | x   |             |            |      |                 |                 |

\* all scores using the same variables [ESC_Score_2016, ESC_Score_GER_2016, ESC_Score_OP, ESC_Score2, ESC_Score2_OP]. ESC_Score2 and ESC_Score2_OP additionally use the variable hdl to calculate the value for non-hdl cholesterol (that is used instead of totchol as in the original ESC-Scores).

## Examples ##

### Calculate CVD risk with the SCORE risk estimation for an individual ###

```R
 ESC_Score2_table(
      sex = "female",
      age = 55,
      totchol = 200, # mg/dL
      hdl = 20, # mg/dL
      sbp = 135,
      smoker = 1,
      risk = "moderate",
      mmol = FALSE)
```

### Calculate CVD risk with the FRS-CHD risk estimation for a group individuals ###

```R
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

### Calculate CVD risk with the TRA 2°P-TIMI for a cohort dataset ###
```R
 whit(data, tra2p_score( 
      age = age,
      chf = chf,
      ah = ah,
      diabetic = diabetic,
      stroke = stroke,
      bypass_surg = bypass_surg,
      other_surg = other_surg,
      egfr = egfr,
      smoker = smoker))
```
