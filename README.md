# estimateCVRisk

Cardiovascular diseases (CVDs) are the leading cause of death globaly. In 2019 an estimated 17.9 million people died from CVDs, representing 32% of all global deaths. Of 
these deaths, 85% were due to heart attack and stroke. It is important to detect cardiovascular disease as early as possible so that so that an early treatment can begin. To determine the risk of CVD US American, European and German guidelines recommend different risk equations. <br/>
**The aim** of this package is to provide a tool for easily calculation of different risk scores for e.g. prospective risk analysis in clinicla kohorts or comparisms of different scores in these kohorts (tools for comparism will be included in future updates of the package). The usage of R provides an easily and fast way to calculate the scores even in large Dataset. 

## about the package

estimateCVRisk is a package which allows you to calculate the most popular riskScore for cardivascular risk available. <br/>
Currently available riskscore functions in the package:
- Systematic COronary Risk Evaluation from the European Society of Cardiology (**ESC-SCORE**) (https://doi.org/10.1016/S0195-668X(03)00114-3)
- Systematic COronary Risk Evaluation in older persons (**ESC-SCORE O.P.**) (https://doi.org/10.1177/2047487315588390)
- Systematic COronary Risk Evaluation in a German Cohort (**ESC-SCORE DE**) (https://doi.org/10.1371/journal.pone.0162188)
- PROCAM I Score (https://doi.org/10.1161/hc0302.102575) 
- PROCAM II Score (https://doi.org/10.1111/j.1365-2362.2007.01888.x)
- Atherosclerotic cardiovascular disease score (**ASCVD**) Guideline from ACC/AHA on the Assessment of CV (https://doi.org/10.1161/01.cir.0000437741.48606.98)
- Framingham Risk Score to assess risk of specific CVD (**FRS-CVD**) (https://doi.org/10.1161/CIRCULATIONAHA.107.699579)
- Framingham Risk Score to assess risk of CHD (**FRS-CHD**) (https://doi.org/10.1161/01.CIR.97.18.1837)
- REducation of Atherothrombosis for Continued Health (**REACH**) (https://doi.org/10.1016/j.amjmed.2012.01.014)
- Thrombin Receptor Antagonist in Secondary Prevention of Atherothrombotic Ischemic Events-TIMI 50 (**TRA 2°P-TIMI**)

available soon: 
- SCORE2 (https://doi.org/10.1093/eurheartj/ehab309 )
- SCORE2-OP (https://doi.org/10.1093/eurheartj/ehab312 )
 

## Usage

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
