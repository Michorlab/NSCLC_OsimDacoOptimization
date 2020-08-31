##### fixedPKparameters #####
# Author: Kamrine Poels
# Description: Returns the fixed PK parameters given different predictors

fixedPKparams = function(weight = 70, albumin = 4, asianOther = 0, asianChinese = 0,
                         asianJapanese = 0, nonAsianCaucasian = 0, healthy = 0, female = 0,
                         concomitant = 0, egfr = 1, bat = 22, food = 0){
  c1_osi = 14.2*(1+0.44)^healthy*(weight/70)^.56
  v1_osi = 986*(albumin/4)^1.33*(weight/70)^.65
  c2_osi = 31.5*(1+.21)^asianOther*(1+.17)^asianChinese*(1+.2)^asianJapanese*(1+.1)^nonAsianCaucasian*
    (1+1.25)^healthy*(weight/70)^.99
  cl_daco = 19.952*(weight/70)^.75*(1-.115)^female*(1+.085)^(asianOther+asianChinese+asianJapanese)*
    (1-.33)^concomitant*(1+.093)^egfr*(1-.046)^(1-egfr)*(1+.002*(bat-22))*(albumin/4)^.543
  ka_daco = .067*(1+.227)^food
  return(data.frame(Clearance1_osi = c1_osi,
                    Volume1_osi = v1_osi,
                    Clearance2_osi = c2_osi,
                    Clearance_daco = cl_daco,
                    Absorption_daco = ka_daco))
}
