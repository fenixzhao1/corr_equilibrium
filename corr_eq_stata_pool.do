***** data preparation *****
* open dataset
use "D:\Dropbox\Working Papers\Correlated Equilibrium\data\produce\stata_pool.dta", clear

* drop practice round
drop if round <= 2

* encode ids
encode session_round_pair_id, gen (cluster_pair_id)
encode player_code, gen (cluster_subject_id)
encode cluster_id, gen (cluster_pair_subject_id)

* generate treatment dummies
gen MaxInfo = 0
replace MaxInfo = 1 if information == "MaxInfo"

* generate control variables
gen LateGame = 0
replace LateGame = 1 if round >= 7

gen LatePeriod = 0
replace LatePeriod = 1 if period >= 26

* generate intersection between info treatment and avgpay
gen MaxInfo_avgpay0 = MaxInfo * player_avgpay0_standard
gen MaxInfo_avgpay1 = MaxInfo * player_avgpay1_standard
gen MaxInfo_avgpay2 = MaxInfo * player_avgpay2_standard

* generate intersection between learning and avgpay
gen LateGame_avgpay0 = LateGame * player_avgpay0_standard
gen LateGame_avgpay1 = LateGame * player_avgpay1_standard
gen LateGame_avgpay2 = LateGame * player_avgpay2_standard

gen LatePeriod_avgpay0 = LatePeriod * player_avgpay0_standard
gen LatePeriod_avgpay1 = LatePeriod * player_avgpay1_standard
gen LatePeriod_avgpay2 = LatePeriod * player_avgpay2_standard


***** Avgpay analysis*****
* sort the data and generate lag terms
sort cluster_pair_subject_id period
by cluster_pair_subject_id: gen lag_player_0 = player_strategy0[_n-1]
by cluster_pair_subject_id: gen lag_player_1 = player_strategy1[_n-1]
by cluster_pair_subject_id: gen lag_player_2 = player_strategy2[_n-1]

* regression at pair level
logit player_strategy0 lag_player_0 ///
      player_avgpay0_standard player_avgpay1_standard player_avgpay2_standard ///
	  MaxInfo MaxInfo_avgpay0 MaxInfo_avgpay1 MaxInfo_avgpay2 ///
	  LateGame LateGame_avgpay0 LateGame_avgpay1 LateGame_avgpay2 ///
	  LatePeriod LatePeriod_avgpay0 LatePeriod_avgpay1 LatePeriod_avgpay2 ///
	  if game == "BM", cluster(cluster_pair_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)
	  
logit player_strategy0 lag_player_0 lag_player_1 ///
      player_avgpay0_standard player_avgpay1_standard player_avgpay2_standard ///
	  MaxInfo MaxInfo_avgpay0 MaxInfo_avgpay1 MaxInfo_avgpay2 ///
	  LateGame LateGame_avgpay0 LateGame_avgpay1 LateGame_avgpay2 ///
	  LatePeriod LatePeriod_avgpay0 LatePeriod_avgpay1 LatePeriod_avgpay2 ///
	  if game == "MV", cluster(cluster_pair_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
	  
logit player_strategy1 lag_player_0 lag_player_1 ///
      player_avgpay0_standard player_avgpay1_standard player_avgpay2_standard ///
	  MaxInfo MaxInfo_avgpay0 MaxInfo_avgpay1 MaxInfo_avgpay2 ///
	  LateGame LateGame_avgpay0 LateGame_avgpay1 LateGame_avgpay2 ///
	  LatePeriod LatePeriod_avgpay0 LatePeriod_avgpay1 LatePeriod_avgpay2 ///
	  if game == "MV", cluster(cluster_pair_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
	  
* regression at subject level
logit player_strategy0 lag_player_0 ///
      player_avgpay0_standard player_avgpay1_standard player_avgpay2_standard ///
	  MaxInfo MaxInfo_avgpay0 MaxInfo_avgpay1 MaxInfo_avgpay2 ///
	  LateGame LateGame_avgpay0 LateGame_avgpay1 LateGame_avgpay2 ///
	  LatePeriod LatePeriod_avgpay0 LatePeriod_avgpay1 LatePeriod_avgpay2 ///
	  if game == "BM", cluster(cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)
	  
logit player_strategy0 lag_player_0 lag_player_1 ///
      player_avgpay0_standard player_avgpay1_standard player_avgpay2_standard ///
	  MaxInfo MaxInfo_avgpay0 MaxInfo_avgpay1 MaxInfo_avgpay2 ///
	  LateGame LateGame_avgpay0 LateGame_avgpay1 LateGame_avgpay2 ///
	  LatePeriod LatePeriod_avgpay0 LatePeriod_avgpay1 LatePeriod_avgpay2 ///
	  if game == "MV", cluster(cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
	  
logit player_strategy1 lag_player_0 lag_player_1 ///
      player_avgpay0_standard player_avgpay1_standard player_avgpay2_standard ///
	  MaxInfo MaxInfo_avgpay0 MaxInfo_avgpay1 MaxInfo_avgpay2 ///
	  LateGame LateGame_avgpay0 LateGame_avgpay1 LateGame_avgpay2 ///
	  LatePeriod LatePeriod_avgpay0 LatePeriod_avgpay1 LatePeriod_avgpay2 ///
	  if game == "MV", cluster(cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

