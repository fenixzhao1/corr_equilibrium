***** Directional regret data analysis (hist avg) *****
* open dataset
use "D:\Dropbox\Working Papers\Correlated Equilibrium\data\produce\stata_pool_dir.dta", clear

* drop practice round
drop if round <= 2

* drop regret mode data
drop if regret == 3

* encode ids
encode cluster_id, gen (cluster_subject_id)
encode cluster_id_dir, gen (cluster_subject_direction_id)

* generate treatment dummies
gen MaxInfo = 0
replace MaxInfo = 1 if information == "MaxInfo"
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

* generate intersection terms regarding avgpaydiff
gen MaxInfo_avgpaydiff = MaxInfo * avgpaydiff_std
gen MV_avgpaydiff = MV * avgpaydiff_std
gen LateGame_avgpaydiff = LateGame * avgpaydiff_std
gen LatePeriod_avgpaydiff = LatePeriod * avgpaydiff_std

* add indicator dummy for avgpaydiff_std<0 and intersection term
gen negative_regret = 0
replace negative_regret = 1 if avgpaydiff_std < 0
gen negative_avgpaydiff = negative_regret * avgpaydiff_std

* logit regressions
logit player_switch_new avgpaydiff_std, cluster(cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

logit player_switch_new avgpaydiff_std positive_avgpaydiff, cluster(cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

logit player_switch_new avgpaydiff_std ///
	  MaxInfo MaxInfo_avgpaydiff ///
	  MV MV_avgpaydiff MaxInfo_MV ///
	  LateGame LateGame_avgpaydiff ///    
	  LatePeriod LatePeriod_avgpaydiff, cluster(cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

logit player_switch_new avgpaydiff_std positive_avgpaydiff ///
	  MaxInfo MaxInfo_avgpaydiff ///
	  MV MV_avgpaydiff MaxInfo_MV ///
	  LateGame LateGame_avgpaydiff ///    
	  LatePeriod LatePeriod_avgpaydiff, cluster(cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

* OLS regression
reg player_switch_new avgpaydiff_std negative_avgpaydiff if game == "BM", cluster(cluster_subject_id)
test avgpaydiff_std + negative_avgpaydiff = 0
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

reg player_switch_new avgpaydiff_std negative_avgpaydiff ///
	MaxInfo MaxInfo_avgpaydiff ///
	LateGame LateGame_avgpaydiff ///    
	LatePeriod LatePeriod_avgpaydiff if game == "BM", cluster(cluster_subject_id)
test avgpaydiff_std + negative_avgpaydiff = 0
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

reg player_switch_new avgpaydiff_std negative_avgpaydiff if game == "MV", cluster(cluster_subject_id)
test avgpaydiff_std + negative_avgpaydiff = 0
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

reg player_switch_new avgpaydiff_std negative_avgpaydiff ///
	MaxInfo MaxInfo_avgpaydiff ///
	LateGame LateGame_avgpaydiff ///    
	LatePeriod LatePeriod_avgpaydiff if game == "MV", cluster(cluster_subject_id)
test avgpaydiff_std + negative_avgpaydiff = 0
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)



***** Directional regret data analysis multinomial logit MV (hist avg) *****
* open dataset
use "D:\Dropbox\Working Papers\Correlated Equilibrium\data\produce\stata_pool_dir_mv.dta", clear

* drop practice round
drop if round <= 2

* encode ids
encode cluster_id, gen (cluster_subject_id)
encode cluster_id_dir, gen (cluster_subject_direction_id)

* generate treatment dummies
gen MaxInfo = 0
replace MaxInfo = 1 if information == "MaxInfo"

* generate control variables
gen LateGame = 0
replace LateGame = 1 if round >= 7
gen LatePeriod = 0
replace LatePeriod = 1 if period >= 26

* standardize avgpaydiff
gen avgpaydiff1_std = player_avgpaydiff1 / 100
gen avgpaydiff2_std = player_avgpaydiff2 / 100

* generate intersection terms regarding avgpaydiff
gen MaxInfo_avgpaydiff1 = MaxInfo * avgpaydiff1_std
gen LateGame_avgpaydiff1 = LateGame * avgpaydiff1_std
gen LatePeriod_avgpaydiff1 = LatePeriod * avgpaydiff1_std

gen MaxInfo_avgpaydiff2 = MaxInfo * avgpaydiff2_std
gen LateGame_avgpaydiff2 = LateGame * avgpaydiff2_std
gen LatePeriod_avgpaydiff2 = LatePeriod * avgpaydiff2_std

* add indicator dummy for avgpaydiff_std<0 and intersection term
gen negative_regret1 = 0
replace negative_regret1 = 1 if avgpaydiff1_std < 0
gen negative_avgpaydiff1 = negative_regret1 * avgpaydiff1_std

gen negative_regret2 = 0
replace negative_regret2 = 1 if avgpaydiff2_std < 0
gen negative_avgpaydiff2 = negative_regret2 * avgpaydiff2_std

* logit regressions
mlogit player_switch avgpaydiff1_std avgpaydiff2_std negative_avgpaydiff1 negative_avgpaydiff2 ///
	   ,cluster(cluster_subject_id) base(0)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
	   
mlogit player_switch avgpaydiff1_std avgpaydiff2_std negative_avgpaydiff1 negative_avgpaydiff2 ///
	   MaxInfo MaxInfo_avgpaydiff1 MaxInfo_avgpaydiff2 ///
	   LateGame LateGame_avgpaydiff1 LateGame_avgpaydiff2 ///    
	   LatePeriod LatePeriod_avgpaydiff1 LatePeriod_avgpaydiff2 ///
	   ,cluster(cluster_subject_id) base(0)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)



***** Directional regret data analysis (counterfactual) *****
* open dataset
use "D:\Dropbox\Working Papers\Correlated Equilibrium\data\produce\stata_pool_dir.dta", clear

* drop practice round
drop if round <= 2

* drop regret mode data
drop if regret == 2

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

* generate intersection terms regarding avgpaydiff
gen MaxInfo_avgpaydiff = MaxInfo * avgpaydiff_std
gen MV_avgpaydiff = MV * avgpaydiff_std
gen LateGame_avgpaydiff = LateGame * avgpaydiff_std
gen LatePeriod_avgpaydiff = LatePeriod * avgpaydiff_std

* add indicator dummy for avgpaydiff_std<0 and intersection term
gen negative_regret = 0
replace negative_regret = 1 if avgpaydiff_std < 0
gen negative_avgpaydiff = negative_regret * avgpaydiff_std

* logit regressions
logit player_switch_new avgpaydiff_std, cluster(cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

logit player_switch_new avgpaydiff_std positive_avgpaydiff, cluster(cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

logit player_switch_new avgpaydiff_std ///
	  MaxInfo MaxInfo_avgpaydiff ///
	  MV MV_avgpaydiff MaxInfo_MV ///
	  LateGame LateGame_avgpaydiff ///    
	  LatePeriod LatePeriod_avgpaydiff, cluster(cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

logit player_switch_new avgpaydiff_std positive_avgpaydiff ///
	  MaxInfo MaxInfo_avgpaydiff ///
	  MV MV_avgpaydiff MaxInfo_MV ///
	  LateGame LateGame_avgpaydiff ///    
	  LatePeriod LatePeriod_avgpaydiff, cluster(cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

* OLS regression
reg player_switch_new avgpaydiff_std negative_avgpaydiff if game == "CH", cluster(cluster_subject_id)
test avgpaydiff_std + negative_avgpaydiff = 0
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

reg player_switch_new avgpaydiff_std negative_avgpaydiff ///
	MaxInfo MaxInfo_avgpaydiff ///
	LateGame LateGame_avgpaydiff ///    
	LatePeriod LatePeriod_avgpaydiff if game == "CH", cluster(cluster_subject_id)
test avgpaydiff_std + negative_avgpaydiff = 0
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

reg player_switch_new avgpaydiff_std negative_avgpaydiff if game == "MV", cluster(cluster_subject_id)
test avgpaydiff_std + negative_avgpaydiff = 0
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

reg player_switch_new avgpaydiff_std negative_avgpaydiff ///
	MaxInfo MaxInfo_avgpaydiff ///
	LateGame LateGame_avgpaydiff ///    
	LatePeriod LatePeriod_avgpaydiff if game == "MV", cluster(cluster_subject_id)
test avgpaydiff_std + negative_avgpaydiff = 0
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)



***** Beta estimation testing using CH data and counterfactual *****
* open dataset
use "D:\Dropbox\Working Papers\Correlated Equilibrium\data\produce\stata_pool_dir.dta", clear

* drop practice round
drop if round <= 2

* keep counterfactual and CH data
keep if regret == 3 & game == "CH"

* encode ids
encode cluster_id, gen (cluster_subject_id)
encode cluster_id_dir, gen (cluster_subject_direction_id)
encode player_code, gen (subject_id)

* generate treatment dummies
gen H = 0
replace H = 1 if information == "H"

* add indicator dummy for regret<0 and intersection term
rename player_avgpaydiff player_regret
gen negative_regret_dummy_ = 0
replace negative_regret_dummy = 1 if player_regret < 0
gen negative_regret = negative_regret_dummy * player_regret

* generate intersection terms regarding avgpaydiff
gen H_regret = H * player_regret
gen H_negative_regret = H * negative_regret

* logit regressions
xi:logit player_switch_new player_regret negative_regret ///
      H_regret H_negative_regret ///
      i.subject_id, cluster(cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

logit player_switch_new player_regret negative_regret ///
      H_regret H_negative_regret, cluster(cluster_subject_id)

xi:logit player_switch_new player_regret ///
         H_regret ///
         i.subject_id, cluster(cluster_subject_id)
	  
logit player_switch_new player_regret ///
      H_regret, cluster(cluster_subject_id)

* keep low information
keep if H == 0
xi:logit player_switch_new player_regret i.subject_id, cluster(cluster_subject_id) noconstant 
logit player_switch_new player_regret, cluster(cluster_subject_id) noconstant 












