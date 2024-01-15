clear
set more off
cd "C:\Users\Ejig\Documents\Applied Economics MS\ECON 672 -Program Analysis and Evaluation\Replication Project\2011356data"

use "C:\Users\Ejig\Documents\Applied Economics MS\ECON 672 -Program Analysis and Evaluation\Replication Project\2011356data\JPEfinal_1-23-2013.dta" 

*Table 1 Summary Statistics:
table () ( country ) (), style(table-1) statistic(mean csp) statistic(mean csp_years) statistic(mean totyrsedu) statistic(mean completed_prim) statistic(mean completed_sec) statistic(mean completed_uni) statistic(mean age) statistic(mean sex) statistic(mean numsibs) statistic(mean mom_highestedu) statistic(mean dad_highestedu) nformat(%9.3g) sformat("(%s)" sd)
 
asdoc tabstat csp csp_years totyrsedu completed_prim completed_sec completed_uni age sex numsibs mom_highestedu dad_highestedu, by(country) stats(sd) 

tabstat csp csp_years totyrsedu completed_prim completed_sec completed_uni age sex numsibs mom_highestedu dad_highestedu, stats(sd) 

tab csp numsibs

*Table 2, differences in means between treated and untreated
asdoc sum totyrsedu if csp==1, replace
asdoc sum totyrsedu if csp==0, append
asdoc sum totyrsedu if csp==0 & treatedhh==1, append
asdoc ttest totyrsedu, by(csp), append
asdoc ttest totyrsedu if treatedhh==1, by(csp), append

asdoc sum completed_prim if csp==1
asdoc sum completed_prim if csp==0
asdoc sum completed_prim if csp==0 & treatedhh==1
asdoc ttest completed_prim, by(csp)
asdoc ttest completed_prim if treatedhh==1, by(csp)

asdoc sum completed_sec if csp==1
asdoc sum completed_sec if csp==0
asdoc sum completed_sec if csp==0 & treatedhh==1
asdoc ttest completed_sec, by(csp)
asdoc ttest completed_sec if treatedhh==1, by(csp)

asdoc sum completed_uni if csp==1
asdoc sum completed_uni if csp==0
asdoc sum completed_uni if csp==0 & treatedhh==1
asdoc ttest completed_uni, by(csp)
asdoc ttest completed_uni if treatedhh==1, by(csp)


*correlation tests: 
corr(aci totyrsedu)
*Weakly negatively correlated!

corr birthorder csp
*Not correlated!

*Differences in means between treated and untreated for employment and leadership
sum employed if csp==1
sum employed if csp==0
sum employed if csp==0 & treatedhh==1
ttest employed, by(csp)
ttest employed if treatedhh==1, by(csp)

sum whitecollar if csp==1
sum whitecollar if csp==0
sum whitecollar if csp==0 & treatedhh==1
ttest whitecollar, by(csp)
ttest whitecollar if treatedhh==1, by(csp)

sum leader_com if csp==1
sum leader_com if csp==0
sum leader_com if csp==0 & treatedhh==1
ttest leader_com, by(csp)
ttest leader_com if treatedhh==1, by(csp)

sum leader_church if csp==1
sum leader_church if csp==0
sum leader_church if csp==0 & treatedhh==1
ttest leader_church, by(csp)
ttest leader_church if treatedhh==1, by(csp)

*****************OLS regression for the four education outcomes:
global covar age age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu

*Total years of education:
reg totyrsedu D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* $covar treatedhh progvillage  momedmissing  dadedmissing, cluster(village_id) 
est sto ols_totyrs

*Primary school completion:
reg completed_prim D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* $covar treatedhh progvillage  momedmissing  dadedmissing, cluster(village_id) 
est sto ols_prim

*Secondary school completion:
reg completed_sec D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* $covar treatedhh progvillage  momedmissing  dadedmissing, cluster(village_id) 
est sto ols_sec

*University school completion: 
reg completed_uni D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* $covar treatedhh progvillage  momedmissing  dadedmissing, cluster(village_id) 
est sto ols_uni

************Difference-in-differences:
*Within household differences:
lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316)
est sto hh_diff

*Within village differences:
lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316) 
est sto vill_diff

*Treatment effect with spillovers:
lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316) 
est sto att_tau

*Covariate test:
test $covar 
test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
test D1aci1316=D2aci1316=D3aci1316 
test D1aci12minus=D2aci12minus=D3aci12minus
test (D1aci1316=D2aci1316=D3aci1316) (D1aci12minus=D2aci12minus=D3aci12minus)

*OLS educational 
outreg2 [ols_prim ols_sec ols_uni hh_diff vill_diff att_tau] using myfile, word


*****************OLS regression for two employment oucomes & two leadership outcomes:
*Formal employment:
reg employed D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* $covar treatedhh progvillage  momedmissing  dadedmissing , cluster(village_id)
est sto ols_emp

*White collar employment:
reg whitecollar D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* $covar treatedhh progvillage  momedmissing  dadedmissing , cluster(village_id)
est sto ols_whitecol

*Community leader:
reg leader_comm D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* $covar treatedhh progvillage  momedmissing  dadedmissing , cluster(village_id)
est sto ols_comm

*Church leader: 
reg leader_church D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* $covar treatedhh progvillage  momedmissing  dadedmissing , cluster(village_id)
est sto ols_church

************Difference-in-differences:
*Within household differences:
lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316)
est sto hh_diff_el

*Within village differences:
lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316) 
est sto vill_diff_el

*Treatment effect with spillovers:
lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316) 
est sto att_tau_el

*Covariate balance test: 
test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
test D1aci1316=D2aci1316=D3aci1316 
test D1aci12minus=D2aci12minus=D3aci12minus
test (D1aci1316=D2aci1316=D3aci1316) (D1aci12minus=D2aci12minus=D3aci12minus)


****************Generate differences for all variables:
gen validobs=1 if (totyrsedu~=. & sex~=. & numsibs~=.)
su validobs

foreach j in totyrsedu completed_prim completed_sec completed_uni employed whitecollar leader_comm leader_church D1aci12minusxT D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316 $covar treatedhh progvillage momedmissing dadedmissing {
   bysort new_hh_id: egen mn`j'=mean(`j'*validobs)
   gen d`j'=`j' - mn`j'
   }

*****************OLS-FE regression for the four education outcomes:
global dcovar dage dage2 dsex dnumsibs dbirthorder doldest dmom_highestedu ddad_highestedu dtreatedhh dprogvillage dmomedmissing ddadedmissing

*Total years of education:
reg dtotyrsedu dD1aci12minusxT dD1aci12minus dD1aci1316 dD2aci* dD3aci* $dcovar , cluster(village_id)
est sto olsfe_dtot

*Primary school completion:
reg dcompleted_prim dD1aci12minusxT dD1aci12minus dD1aci1316 dD2aci* dD3aci* $dcovar , cluster(village_id)
est sto olsfe_dprim

*Secondary school completion:
reg dcompleted_sec dD1aci12minusxT dD1aci12minus dD1aci1316 dD2aci* dD3aci* $dcovar, cluster(village_id) 
est sto olsfe_dsec

*University school completion: 
reg dcompleted_uni dD1aci12minusxT dD1aci12minus dD1aci1316 dD2aci* dD3aci* $dcovar, cluster(village_id)
est sto olsfe_duni

************Difference-in-differences:
*Within household differences:
lincom (dD1aci12minus-dD1aci1316) - (dD3aci12minus-dD3aci1316)
est sto hh_diff_fe

*Within village differences:
lincom (dD2aci12minus-dD2aci1316) - (dD3aci12minus-dD3aci1316) 
est sto vill_diff_fe

*Treatment effect with spillovers:
lincom ((dD1aci12minus+dD1aci12minusxT)-dD1aci1316) - (dD3aci12minus-dD3aci1316) 
est sto att_tau_fe

*Covariate balance test: 
test dD1aci12minus dD1aci1316 dD2aci12minus dD2aci1316 dD3aci12minus dD3aci1316
test dD1aci1316=dD2aci1316=dD3aci1316 
test dD1aci12minus=dD2aci12minus=dD3aci12minus
test (dD1aci1316=dD2aci1316=dD3aci1316) (dD1aci12minus=dD2aci12minus=dD3aci12minus)
   
****************************************OLS-FE for the two employment outcomes & leadership:

*Formal employment:
drop mn* validobs
gen validobs=1 if (employed~=. & sex~=. & numsibs~=.)
su validobs

foreach j in employed /*
            */ D1aci12minusxT D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus /*
            */ D3aci1316 $covar treatedhh progvillage momedmissing dadedmissing {
   bysort new_hh_id: egen mn`j'=mean(`j'*validobs)
   replace d`j'=`j' - mn`j'
   }

reg demployed dD1aci12minusxT dD1aci12minus dD1aci1316 dD2aci* dD3aci* $dcovar , cluster(village_id)
est sto olsfe_demp

************Difference-in-differences:
*Within household differences:
lincom (dD1aci12minus-dD1aci1316) - (dD3aci12minus-dD3aci1316) 
est sto hhfe_diff_el

*Within village difference:
lincom (dD2aci12minus-dD2aci1316) - (dD3aci12minus-dD3aci1316)
est sto vfe_diff_el

*Treatment effect w/ spillovers:
lincom ((dD1aci12minus+dD1aci12minusxT)-dD1aci1316) - (dD3aci12minus-dD3aci1316)
est sto att_fe_el

*Covariate balance test: 
test dD1aci12minus dD1aci1316 dD2aci12minus dD2aci1316 dD3aci12minus dD3aci1316
test dD1aci1316=dD2aci1316=dD3aci1316 
test dD1aci12minus=dD2aci12minus=dD3aci12minus
test (dD1aci1316=dD2aci1316=dD3aci1316) (dD1aci12minus=dD2aci12minus=dD3aci12minus)

*White collar: 
drop mn* validobs
gen validobs=1 if (whitecollar~=. & sex~=. & numsibs~=.)
su validobs

foreach j in whitecollar /*
            */ D1aci12minusxT D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus /*
            */ D3aci1316 $covar treatedhh progvillage momedmissing dadedmissing {
   bysort new_hh_id: egen mn`j'=mean(`j'*validobs)
   replace d`j'=`j' - mn`j'
   }


reg dwhitecollar dD1aci12minusxT dD1aci12minus dD1aci1316 dD2aci* dD3aci* $dcovar, cluster(village_id)
est sto olsfe_dwc

************Difference-in-differences:
*Within household differences:
lincom (dD1aci12minus-dD1aci1316) - (dD3aci12minus-dD3aci1316) 
est sto hhfe_diff_el

*Within village difference:
lincom (dD2aci12minus-dD2aci1316) - (dD3aci12minus-dD3aci1316)
est sto vfe_diff_el

*Treatment effect w/ spillovers:
lincom ((dD1aci12minus+dD1aci12minusxT)-dD1aci1316) - (dD3aci12minus-dD3aci1316)
est sto att_fe_el

*Covariate balance test: 
test dD1aci12minus dD1aci1316 dD2aci12minus dD2aci1316 dD3aci12minus dD3aci1316
test dD1aci1316=dD2aci1316=dD3aci1316 
test dD1aci12minus=dD2aci12minus=dD3aci12minus
test (dD1aci1316=dD2aci1316=dD3aci1316) (dD1aci12minus=dD2aci12minus=dD3aci12minus)

*Community leader:
drop mn* validobs
gen validobs=1 if (leader_comm~=. & sex~=. & numsibs~=.)
su validobs

foreach j in leader_comm /*
            */ D1aci12minusxT D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus /*
            */ D3aci1316 $covar treatedhh progvillage momedmissing dadedmissing {
   bysort new_hh_id: egen mn`j'=mean(`j'*validobs)
   replace d`j'=`j' - mn`j'
   }


reg dleader_comm dD1aci12minusxT dD1aci12minus dD1aci1316 dD2aci* dD3aci* $dcovar, cluster(village_id)
est sto olsfe_dcl

************Difference-in-differences:
*Within household differences:
lincom (dD1aci12minus-dD1aci1316) - (dD3aci12minus-dD3aci1316) 
est sto hhfe_diff_el

*Within village difference:
lincom (dD2aci12minus-dD2aci1316) - (dD3aci12minus-dD3aci1316)
est sto vfe_diff_el

*Treatment effect w/ spillovers:
lincom ((dD1aci12minus+dD1aci12minusxT)-dD1aci1316) - (dD3aci12minus-dD3aci1316)
est sto att_fe_el

*Covariate balance test: 
test dD1aci12minus dD1aci1316 dD2aci12minus dD2aci1316 dD3aci12minus dD3aci1316
test dD1aci1316=dD2aci1316=dD3aci1316 
test dD1aci12minus=dD2aci12minus=dD3aci12minus
test (dD1aci1316=dD2aci1316=dD3aci1316) (dD1aci12minus=dD2aci12minus=dD3aci12minus)

*Church leader:
drop mn* validobs
gen validobs=1 if (leader_church~=. & sex~=. & numsibs~=.)
su validobs

foreach j in leader_church /*
            */ D1aci12minusxT D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus /*
            */ D3aci1316 $covar treatedhh progvillage momedmissing dadedmissing {
   bysort new_hh_id: egen mn`j'=mean(`j'*validobs)
   replace d`j'=`j' - mn`j'
   }

reg dleader_church dD1aci12minusxT dD1aci12minus dD1aci1316 dD2aci* dD3aci* $dcovar , cluster(village_id)
est sto olsfe_dchl

************Difference-in-differences:
*Within household differences:
lincom (dD1aci12minus-dD1aci1316) - (dD3aci12minus-dD3aci1316) 
est sto hhfe_diff_el

*Within village difference:
lincom (dD2aci12minus-dD2aci1316) - (dD3aci12minus-dD3aci1316)
est sto vfe_diff_el

*Treatment effect w/ spillovers:
lincom ((dD1aci12minus+dD1aci12minusxT)-dD1aci1316) - (dD3aci12minus-dD3aci1316)
est sto att_fe_el

*Covariate balance test: 
test dD1aci12minus dD1aci1316 dD2aci12minus dD2aci1316 dD3aci12minus dD3aci1316
test dD1aci1316=dD2aci1316=dD3aci1316 
test dD1aci12minus=dD2aci12minus=dD3aci12minus
test (dD1aci1316=dD2aci1316=dD3aci1316) (dD1aci12minus=dD2aci12minus=dD3aci12minus)

****IV TEST 
reg D1aci12minusxT iv4* if totyrsedu~=. & sex~=. & numsibs~=., cluster(village_id) /* weak IV F-test */
* Note: Cannot reject (at 5% level) assumption that boy & girl coefficients are equal

test (iv4cspaci4minus1boy=iv4cspaci4minus1girl) (iv4cspaci5to81boy=iv4cspaci5to81girl) /*
 */ (iv4cspaci9to121boy=iv4cspaci9to121girl) (iv4cspaci4minus2boy=iv4cspaci4minus2girl) /*
 */ (iv4cspaci5to82boy=iv4cspaci5to82girl) (iv4cspaci9to122boy=iv4cspaci9to122girl) /*
 */ (iv4cspaci4minus3minusboy=iv4cspaci4minus3minusgirl) /*
 */ (iv4cspaci5to83minusboy=iv4cspaci5to83minusgirl) (iv4cspaci9to123minusboy=iv4cspaci9to123minusgirl)

* weak IV F-test
reg D1aci12minusxT iv3* if totyrsedu~=. & sex~=. & numsibs~=., cluster(village_id) 


****************************************IV for the four education outcomes:
*\\\Total years of education:
ivregress gmm totyrsedu (D1aci12minusxT = iv3*) D1aci12minus D1aci1316 D2aci* D3aci* $covar treatedhh progvillage momedmissing dadedmissing, cluster(village_id) first
est sto iv_totyrsedu

estat overid

*Test for monotonicity:
*First stage estimate

reg D1aci12minusxT  D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316 age age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu treatedhh progvillage momedmissing dadedmissing iv3cspaci4minus1 iv3cspaci4minus2 iv3cspaci4minus3minus iv3cspaci5to81 iv3cspaci5to82 iv3cspaci5to83minus iv3cspaci9to121 iv3cspaci9to122 $covar treatedhh progvillage momedmissing dadedmissing
predict dhat

*Average outcome across dhat:
sum totyrsedu, detail
sort totyrsedu
by totyrsedu: egen dhat_mean=mean(dhat)
sort dhat_mean
twoway line totyrsedu dhat_mean


*\\\Secondary school education:
ivregress gmm completed_sec (D1aci12minusxT = iv3*) D1aci12minus D1aci1316 D2aci* D3aci* $covar treatedhh progvillage momedmissing dadedmissing, cluster(village_id)
est sto iv_sec

estat overid

************Difference-in-differences:
*Within household differences:
lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316)
est sto hhiv_sec

*Within village difference:
lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316)
est sto viv_sec

*Treatment effect w/ spillovers:
lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316)
est sto attiv_sec

*Covariate balance test: 
test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
test D1aci1316=D2aci1316=D3aci1316 
test D1aci12minus=D2aci12minus=D3aci12minus
test (D1aci1316=D2aci1316=D3aci1316) (D1aci12minus=D2aci12minus=D3aci12minus)






   /* Following lines test for endogeneity of D1aci12minusxT (control function method) */
   quietly reg D1aci12minusxT iv3* D1aci12minus D1aci1316 D2aci* D3aci* age /*
                 */ age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu /*
                 */ treatedhh progvillage momedmissing dadedmissing, cluster(village_id)
   predict resid, r
   reg `j' D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* $covar treatedhh progvillage momedmissing dadedmissing resid, cluster(village_id)
   drop resid


****************************************IV-GMM-FE for Education outcomes:
drop mn* validobs
gen validobs=1 if (totyrsedu~=. & sex~=. & numsibs~=. & hh_id~=.)
su validobs

foreach j in totyrsedu completed_prim completed_sec completed_uni /*
            */ employed whitecollar leader_comm leader_church /*
            */ D1aci12minusxT D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus /*
            */ D3aci1316 $covar treatedhh progvillage momedmissing dadedmissing {
   bysort new_hh_id: egen mn`j'=mean(`j'*validobs)
   replace d`j'=`j' - mn`j'
   }

foreach j in iv3cspaci4minus1 iv3cspaci4minus2 iv3cspaci4minus3minus iv3cspaci5to81 /*
            */ iv3cspaci5to82 iv3cspaci5to83minus iv3cspaci9to121 iv3cspaci9to122 /* 
            */ iv3cspaci9to123minus {
   bysort new_hh_id: egen mn`j'=mean(`j'*validobs)
   gen d`j'=`j' - mn`j'
   }

* weak IV F-test
reg dD1aci12minusxT div3* if demployed~=. & dsex~=. & dnumsibs~=., cluster(village_id) 

foreach j in dtotyrsedu dcompleted_prim dcompleted_sec dcompleted_uni{
   ivregress gmm `j' (dD1aci12minusxT=div3*) dD1aci12minus dD1aci1316 dD2aci* dD3aci* $dcovar, cluster(village_id)
   outreg2 using IVFE, word 
   estat overid
   lincom (dD1aci12minus-dD1aci1316) - (dD3aci12minus-dD3aci1316) /* within household */
   lincom (dD2aci12minus-dD2aci1316) - (dD3aci12minus-dD3aci1316) /* within village */
   lincom ((dD1aci12minus+dD1aci12minusxT)-dD1aci1316) - (dD3aci12minus-dD3aci1316) /* treatment effect w/ spillovers */
   test dD1aci12minus dD1aci1316 dD2aci12minus dD2aci1316 dD3aci12minus dD3aci1316
   test dD1aci1316=dD2aci1316=dD3aci1316 
   test dD1aci12minus=dD2aci12minus=dD3aci12minus
   test (dD1aci1316=dD2aci1316=dD3aci1316) (dD1aci12minus=dD2aci12minus=dD3aci12minus)
   /* Following lines test for endogeneity of D1aci12minusxT (control function method) */
   quietly reg dD1aci12minusxT div3* dD1aci12minus dD1aci1316 dD2aci* dD3aci* $dcovar, cluster(village_id)
   predict resid, r
   reg `j' dD1aci12minusxT dD1aci12minus dD1aci1316 dD2aci* dD3aci* $dcovar resid, cluster(village_id)
   drop resid
}


********************************************Regression Discontinuity Design:


 *Uganda
cmogram totyrsedu aci if country==1 & aci > 0 & aci < 16, cut(1) scatter line(12) qfitci, 
*Guatemala
cmogram totyrsedu aci if country==2 & aci > 0 & aci < 16, cut(1) scatter line(12) qfitci
*Phillipines
cmogram totyrsedu aci if country==3 & aci > 0 & aci < 16, cut(1) scatter line(12) qfitci
*India
cmogram totyrsedu aci if country==4 & aci > 0 & aci < 16, cut(1) scatter line(12) qfitci
*Kenya
cmogram totyrsedu aci if country==5 & aci > 0 & aci < 16, cut(1) scatter line(12) qfitci
*Bolivia
cmogram totyrsedu aci if country==6 & aci > 0 & aci < 16, cut(1) scatter line(12) qfitci

rddensity aci, c(12) plot
tab aci csp

*******************************
**TABLE 2:
foreach j of numlist 1 2 3 4 5 6  {
   reg totyrsedu D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* age /*
          */ age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu treatedhh /*
          */ progvillage momedmissing dadedmissing if country==`j', cluster(village_id)
   lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316) /* within household */
   lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316) /* within village */
   lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316) /* treatment effect w/ spillovers */
   test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
}


foreach j of numlist 1 2 3 4 5 6  {
   reg completed_prim D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* age /*
          */ age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu treatedhh /*
          */ progvillage momedmissing dadedmissing if country==`j', cluster(village_id)
   lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316) /* within household */
   lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316) /* within village */
   lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316) /* treatment effect w/ spillovers */
   test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
}


foreach j of numlist 1 2 3 4 5 6  {
   reg completed_sec D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* age /*
          */ age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu treatedhh /*
          */ progvillage momedmissing dadedmissing if country==`j', cluster(village_id)
   lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316) /* within household */
   lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316) /* within village */
   lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316) /* treatment effect w/ spillovers */
   test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
}


foreach j of numlist 1 2 3 4 5 6  {
   reg completed_uni D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* age /*
          */ age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu treatedhh /*
          */ progvillage momedmissing dadedmissing if country==`j', cluster(village_id)
   lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316) /* within household */
   lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316) /* within village */
   lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316) /* treatment effect w/ spillovers */
   test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
}


xtset new_hh_id

foreach j of numlist 1 2 3 4 5 6  {
   xtreg totyrsedu D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* age /*
          */ age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu treatedhh /*
          */ progvillage momedmissing dadedmissing if country==`j', fe cluster(village_id)
   lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316) /* within household */
   lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316) /* within village */
   lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316) /* treatment effect w/ spillovers */
   test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
}


foreach j of numlist 1 2 3 4 5 6  {
   xtreg completed_prim D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* age /*
          */ age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu treatedhh /*
          */ progvillage momedmissing dadedmissing if country==`j', fe cluster(village_id)
   lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316) /* within household */
   lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316) /* within village */
   lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316) /* treatment effect w/ spillovers */
   test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
}


foreach j of numlist 1 2 3 4 5 6  {
   xtreg completed_sec D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* age /*
          */ age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu treatedhh /*
          */ progvillage momedmissing dadedmissing if country==`j', fe cluster(village_id)
   lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316) /* within household */
   lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316) /* within village */
   lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316) /* treatment effect w/ spillovers */
   test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
}


foreach j of numlist 1 2 3 4 5 6  {
   xtreg completed_uni D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* age /*
          */ age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu treatedhh /*
          */ progvillage momedmissing dadedmissing if country==`j', fe cluster(village_id)
   lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316) /* within household */
   lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316) /* within village */
   lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316) /* treatment effect w/ spillovers */
   test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
}


* Following is for Table 4.

foreach j of numlist 1 2 3 4 5 6  {
   reg totyrsedu D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* age /*
          */ age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu treatedhh /*
          */ progvillage momedmissing dadedmissing if country==`j' & sex==1, cluster(village_id)
outreg2 using male, se bdec(3) rdec(3)word
   lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316) /* within household */
   lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316) /* within village */
   lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316) /* treatment effect w/ spillovers */
   test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
}

foreach j of numlist 1 2 3 4 5 6  {
   reg totyrsedu D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* age /*
          */ age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu treatedhh /*
          */ progvillage momedmissing dadedmissing if country==`j' & sex==0, cluster(village_id)
outreg2 using female, se bdec(3) rdec(3)word
   lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316) /* within household */
   lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316) /* within village */
   lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316) /* treatment effect w/ spillovers */
   test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
}

xtset new_hh_id

foreach j of numlist 1 2 3 4 5 6  {
   xtreg totyrsedu D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* age /*
          */ age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu treatedhh /*
          */ progvillage momedmissing dadedmissing if country==`j' & sex==1, fe cluster(village_id)
   lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316) /* within household */
   lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316) /* within village */
   lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316) /* treatment effect w/ spillovers */
   test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
}

foreach j of numlist 1 2 3 4 5 6  {
   xtreg totyrsedu D1aci12minusxT D1aci12minus D1aci1316 D2aci* D3aci* age /*
          */ age2 sex numsibs birthorder oldest mom_highestedu dad_highestedu treatedhh /*
          */ progvillage momedmissing dadedmissing if country==`j' & sex==0, fe cluster(village_id)
   lincom (D1aci12minus-D1aci1316) - (D3aci12minus-D3aci1316) /* within household */
   lincom (D2aci12minus-D2aci1316) - (D3aci12minus-D3aci1316) /* within village */
   lincom ((D1aci12minus+D1aci12minusxT)-D1aci1316) - (D3aci12minus-D3aci1316) /* treatment effect w/ spillovers */
   test D1aci12minus D1aci1316 D2aci12minus D2aci1316 D3aci12minus D3aci1316
}
