***** Avgpay analysis panel data version *****
* open dataset

cd "~/Desktop/jotarepos/correq/corr_equilibrium/Data"

***** logit model
use "stata_pool.dta", clear


* drop practice round
drop if round <= 2

* encode ids
encode cluster_id, gen (cluster_subject_id)

gen round_player=session_code+player_code+string(round,"%02.0f")

preserve
keep if game == "BM" 
g regret_10 = (player_avgpay1 - player_avgpay0)
gen negative_regret_1 = 0 
replace negative_regret_1 = 1 if regret_10  < 0
gen negative_regret_10= negative_regret_1 *regret_10

keep game regret information round_player round player_code player_strategy period player_avgpay0 player_avgpay1 session_code cluster_subject_id regret_10 negative_regret_1
save bml_lm.dta, replace
restore


preserve
keep if game == "MV"
bysort session_round_pair_id: g cambio = player_strategy-player_strategy[_n-1]
g switch_dir = cambio
replace switch_dir = 2 if switch_dir == -1 & player_strategy[_n-1]==1
replace switch_dir = 1 if cambio == -2 
replace switch_dir = 2 if switch_dir == -1 & player_strategy[_n-1]==2

g fed_0 = player_avgpay0
g fed_1 = player_avgpay1
g fed_2 = player_avgpay2

replace fed_0 = player_avgpay1 if player_strategy[_n-1]==1 & switch_dir==0
replace fed_1 = player_avgpay2 if player_strategy[_n-1]==1 & switch_dir==1
replace fed_2 = player_avgpay0 if player_strategy[_n-1]==1 & switch_dir==2

replace fed_0 = player_avgpay2 if player_strategy[_n-1]==2 & switch_dir==0
replace fed_1 = player_avgpay0 if player_strategy[_n-1]==2 & switch_dir==1
replace fed_2 = player_avgpay1 if player_strategy[_n-1]==2 & switch_dir==2

keep game regret information session_round_pair_id round_player round player_code player_strategy period player_avgpay0 player_avgpay1 player_avgpay2 session_code cluster_subject_id switch_dir fed_0 fed_1 fed_2
save mv_all.dta, replace
restore

* generate treatment dummies
gen MaxInfo = 0
replace MaxInfo = 1 if information == "H"
gen MV = 0
replace MV = 1 if game == "MV"
gen MaxInfo_MV = MaxInfo * MV

* generate control variables
gen LateGame = 0
replace LateGame = 1 if round >= 7
gen LatePeriod = 0
replace LatePeriod = 1 if period >= 36

g regret_10 = (player_avgpay1 - player_avgpay0)/100

g regret_new = (player_strategy[_n-1]*-2+1)*regret_10 

g regret_hm = 0

replace regret_hm = regret_new if regret_new>0 

gen negative_regret = 0
replace negative_regret = 1 if regret_new  < 0
gen negative_regret_new = negative_regret *regret_new

gen negative_regret_1 = 0 
replace negative_regret_1 = 1 if regret_10  < 0

gen negative_regret_10= negative_regret_1 *regret_10

reg player_strategy1  regret_10 ///
      if game == "BM" & regret==3 & information=="L", cluster(cluster_subject_id)

reg player_strategy1  regret_10 ///
      if game == "BM" & regret==2 & information=="L", cluster(cluster_subject_id)


gen pay_last = player_payoff[_n-1]
logit player_switch regret_new  ///
      if game == "BM" & regret==2 & information=="L", cluster(cluster_id)

logit player_switch regret_new  ///
      if game == "BM" & regret==2 & information=="L", noconstant cluster(cluster_id)

	  
logit player_switch regret_new negative_regret_new ///
      if game == "BM" & regret==2 & information=="L", cluster(cluster_id)

logit player_switch regret_new  ///
      if game == "BM" & regret==3 & information=="L", cluster(cluster_id)

logit player_switch regret_new negative_regret_new ///
      if game == "BM" & regret==3 & information=="L", cluster(cluster_id)

	  
	  
logit player_strategy1  regret_10 negative_regret_10 ///
      if game == "BM" & regret==2 & information=="L", cluster(cluster_subject_id)
test regret_10+negative_regret_10=0

logit player_strategy1  regret_10 negative_regret_10 ///
      if game == "BM" & regret==2 & information=="H", cluster(cluster_subject_id)
test regret_10+negative_regret_10=0


logit player_strategy1  regret_10 negative_regret_10 ///
      if game == "BM" & regret==3 & information=="H", cluster(cluster_subject_id)
test regret_10+negative_regret_10=0





	  
logit player_strategy1  regret_10 ///
      if game == "BM" & regret==2 & information=="L", noconstant cluster(cluster_subject_id)	  
	  


test regret_10+negative_regret_10=0

 logit player_strategy1  regret_10 negative_regret_10 ///
      if game == "BM" & regret==3 & information=="L", cluster(cluster_subject_id)

	  	  
	   
 
reg player_strategy1  regret_10 negative_regret_10 ///
      if game == "BM" & regret==3 & information=="L", cluster(cluster_subject_id)
test regret_10+negative_regret_10=0

reg player_strategy1  regret_10 ///
      if game == "BM" & regret==2 & information=="L", cluster(cluster_subject_id)


reg player_strategy1  regret_10 negative_regret_10 ///
      if game == "BM" & regret==2 & information=="L", cluster(cluster_subject_id)
test regret_10+negative_regret_10=0

reg player_strategy1  regret_10 negative_regret_10 ///
      if game == "BM" & regret==3 & information=="H", cluster(cluster_subject_id)
test regret_10+negative_regret_10=0

reg player_strategy1  regret_10 negative_regret_10 ///
      if game == "BM" & regret==2 & information=="H", cluster(cluster_subject_id)






reg player_strategy1  regret_10 negative_regret_10 period  ///
      if game == "BM" & regret==2 & information=="L", cluster(cluster_subject_id)


reg player_switch regret_new  negative_regret_new  ///
      if game == "BM" & regret==3  & information=="L", cluster(cluster_subject_id)

	  
	  
reg player_strategy1  regret_10 negative_regret_10 ///
      if game == "BM" & regret==3 & information=="H", cluster(cluster_subject_id)
	  
reg player_strategy1  regret_10 negative_regret_10 ///
      if game == "BM" & regret==2 & information=="H", cluster(cluster_subject_id)




eg player_strategy1  player_avgpay1  player_avgpay0 ///
      if game == "BM" & regret==3 & information=="L", cluster(cluster_id)
	  	  	   	  

reg player_strategy1  regret_10 negative_regret_10 ///
      if game == "BM" & regret==3 & information=="L", cluster(cluster_id)

logit player_strategy1  player_avgpay1  player_avgpay0 ///
      if game == "BM" & regret==3 & information=="L", cluster(cluster_id)
	  	  	   	  
logit player_switch regret_new  negative_regret_new  ///
      if game == "BM" & regret==3 & information=="L", cluster(cluster_id)
	  	  
logit player_strategy1  regret_10 negative_regret_10 ///
      if game == "BM" & regret==3 & information=="L", cluster(cluster_id)
restore

	  
 
	  
logit player_strategy1  regret_10 negative_regret_10 ///
      if game == "BM" & regret==3 & information=="L", cluster(cluster_id)


logit player_switch  regret_new    ///
      if game == "BM" & regret==3 & information=="L", noconstant cluster(cluster_id)

logit player_switch  regret_hm  ///
      if game == "BM" & regret==3 & information=="L", cluster(cluster_id)
	  
	  

logit player_strategy1  player_avgpay1  player_avgpay0 ///
      if game == "BM" & regret==3 & information=="L", cluster(cluster_id)


reg player_strategy1  player_avgpay1  player_avgpay0 ///
      if game == "BM" & regret==3 & information=="L" & period>10, cluster(cluster_id)

reg player_strategy1  player_avgpay1  player_avgpay0 ///
      if game == "BM" & regret==2 & information=="L", cluster(cluster_id)


	  
	  

logit player_switch  regret_10 ///
      if game == "BM" & regret==2 & information=="L", noconstant cluster(cluster_id)

logit player_switch  regret_new ///
      if game == "BM" & regret==3 & information=="L",cluster(cluster_id)
scalar m1 = e(ll)	  
	  
	  
logit player_switch  regret_new ///
      if game == "BM" & regret==3 & information=="L", noconstant cluster(cluster_id)
scalar m1 = e(ll)
	  
logit player_switch  regret_hm  ///
      if game == "BM" & regret==3 & information=="L", cluster(cluster_id)
scalar m2 = e(ll)

di "chi2(2) = " 2*(m2-m1)
di "Prob > chi2 = "chi2tail(1, 2*(m2-m1))


logit player_switch  regret_new  ///
      if game == "BM" & regret==2 & information=="L", noconstant cluster(cluster_id) 
scalar m1a = e(ll)

logit player_switch  regret_hm  ///
      if game == "BM" & regret==2 & information=="L", cluster(cluster_id) 
scalar m2a = e(ll)

di "chi2(2) = " 2*(m2a-m1a)
di "Prob > chi2 = "chi2tail(2, 2*(m2a-m1a))


	  
logit player_switch  regret_new ///
      if game == "BM" & regret==3 & information=="H", noconstant cluster(cluster_id)

logit player_switch  regret_new ///
      if game == "BM" & regret==3 & information=="H",  cluster(cluster_id)
	  
	  
	  
logit player_switch  regret_new ///
      if game == "BM" & regret==2 & information=="L", noconstant cluster(cluster_id)
	  
	  
logit player_switch  regret_new ///
      if game == "BM" & regret==2 & information=="H", noconstant cluster(cluster_id)


	  
***** Directional regret data analysis (hist avg) *****
* open dataset
use "stata_pool_dir.dta", clear

* drop practice round
drop if round <= 2

* drop regret mode data
*drop if regret == 3

* encode ids
encode cluster_id, gen (cluster_subject_id)
encode cluster_id_dir, gen (cluster_subject_direction_id)

* generate treatment dummies
gen MaxInfo = 0
replace MaxInfo = 1 if information == "H"
gen MV = 0
replace MV = 1 if game == "MV"
gen MaxInfo_MV = MaxInfo * MV

* generate control variables
gen LateGame = 0
replace LateGame = 1 if round >= 7
gen LatePeriod = 0
replace LatePeriod = 1 if period >= 36

* standardize avgpaydiff
gen avgpaydiff_std = player_avgpaydiff / 100

* add indicator dummy for avgpaydiff_std<0 and intersection term
gen negative_regret = 0
replace negative_regret = 1 if avgpaydiff_std < 0
gen negative_avgpaydiff = negative_regret * avgpaydiff_std

* generate intersection terms regarding avgpaydiff
gen MaxInfo_avgpaydiff = MaxInfo * avgpaydiff_std

gen MaxInfo_avgpaydiff_negative = MaxInfo * avgpaydiff_std*negative_regret

gen MV_avgpaydiff = MV * avgpaydiff_std

gen LateGame_avgpaydiff = LateGame * avgpaydiff_std
gen LatePeriod_avgpaydiff = LatePeriod * avgpaydiff_std

**OLS regressions

reg player_switch_new avgpaydiff_std negative_avgpaydiff ///
	MaxInfo MaxInfo_avgpaydiff MaxInfo_avgpaydiff_negative period if game == "BM" & regret==2, cluster(cluster_subject_id) 
test avgpaydiff_std  + negative_avgpaydiff = 0
test avgpaydiff_std+MaxInfo_avgpaydiff + negative_avgpaydiff+MaxInfo_avgpaydiff_negative=0
outreg2 using "~/Desktop/jotarepos/correq/corr_equilibrium/tables/stata_table", tex nonote se replace nolabel bdec(3)

reg player_switch_new avgpaydiff_std negative_avgpaydiff ///
	MaxInfo MaxInfo_avgpaydiff MaxInfo_avgpaydiff_negative period if game == "BM" & regret==3, cluster(cluster_subject_id) 
test avgpaydiff_std  + negative_avgpaydiff = 0
test avgpaydiff_std+MaxInfo_avgpaydiff + negative_avgpaydiff+MaxInfo_avgpaydiff_negative=0
outreg2 using "~/Desktop/jotarepos/correq/corr_equilibrium/tables/stata_table", tex nonote se append nolabel bdec(3)

reg player_switch_new avgpaydiff_std negative_avgpaydiff ///
	MaxInfo MaxInfo_avgpaydiff MaxInfo_avgpaydiff_negative period if game == "MV" & regret==2, cluster(cluster_subject_id) 
test avgpaydiff_std + negative_avgpaydiff = 0
outreg2 using "~/Desktop/jotarepos/correq/corr_equilibrium/tables/stata_table", tex nonote se append nolabel bdec(3)

reg player_switch_new avgpaydiff_std negative_avgpaydiff ///
	MaxInfo MaxInfo_avgpaydiff MaxInfo_avgpaydiff_negative period if game == "MV" & regret==3, cluster(cluster_subject_id) 
test avgpaydiff_std + negative_avgpaydiff = 0
outreg2 using "~/Desktop/jotarepos/correq/corr_equilibrium/tables/stata_table", tex nonote se append nolabel bdec(3)


