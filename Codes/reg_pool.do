***** Avgpay analysis panel data version *****
* open dataset

cd "~/Desktop/jotarepos/correq/corr_equilibrium/Data"

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


