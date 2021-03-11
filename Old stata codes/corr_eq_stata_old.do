***** Analysis with the orginal dataset *****
** data preparation
* open dataset
use "D:\Dropbox\Working Papers\Correlated Equilibrium\data\produce\stata.dta", clear

* drop practice round
drop if round <= 2

* encode session_round_pair_id
encode session_round_pair_id, gen (individual_id)

* generate treatment dummies
gen MaxInfo = 0
replace MaxInfo = 1 if information == "MaxInfo"

* generate control variables
gen late_game = 0
replace late_game = 1 if round >= 7

gen late_period = 0
replace late_period = 1 if period >= 26

* generate other regret form
gen low_p1_0_regret = p1_strategy_0_regret / 100
gen low_p1_1_regret = p1_strategy_1_regret / 100
gen low_p1_2_regret = p1_strategy_2_regret / 100
gen low_p2_0_regret = p2_strategy_0_regret / 100
gen low_p2_1_regret = p2_strategy_1_regret / 100
gen low_p2_2_regret = p2_strategy_2_regret / 100

* generate intersection between regret terms and info treatment
gen maxinfo_p1_0_regret = MaxInfo * low_p1_0_regret
gen maxinfo_p1_1_regret = MaxInfo * low_p1_1_regret
gen maxinfo_p1_2_regret = MaxInfo * low_p1_2_regret
gen maxinfo_p2_0_regret = MaxInfo * low_p2_0_regret
gen maxinfo_p2_1_regret = MaxInfo * low_p2_1_regret
gen maxinfo_p2_2_regret = MaxInfo * low_p2_2_regret

** regret analysis
* generate lag terms for choice dummies
sort individual_id period
by individual_id: gen lag_p1_0 = p1_strategy_0[_n-1]
by individual_id: gen lag_p1_1 = p1_strategy_1[_n-1]
by individual_id: gen lag_p1_2 = p1_strategy_2[_n-1]
by individual_id: gen lag_p2_0 = p2_strategy_0[_n-1]
by individual_id: gen lag_p2_1 = p2_strategy_1[_n-1]
by individual_id: gen lag_p2_2 = p2_strategy_2[_n-1]

* logit regression for p1 in BM games
logit p1_strategy_0 lag_p1_0 low_p1_0_regret low_p1_1_regret ///
	  MaxInfo maxinfo_p1_0_regret maxinfo_p1_1_regret ///
	  late_game late_period ///
	  if game == "BM", cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

* logit regression for p2 in BM games
logit p2_strategy_0 lag_p2_0 low_p2_0_regret low_p2_1_regret ///
	  MaxInfo maxinfo_p2_0_regret maxinfo_p2_1_regret ///
	  late_game late_period ///
	  if game == "BM", cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

* logit regression for p1 in MV games
logit p1_strategy_0 lag_p1_0 lag_p1_1 ///
      low_p1_0_regret low_p1_1_regret low_p1_2_regret ///
	  MaxInfo maxinfo_p1_0_regret maxinfo_p1_1_regret maxinfo_p1_2_regret ///
	  late_game late_period ///
	  if game == "MV", cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

logit p1_strategy_1 lag_p1_0 lag_p1_1 ///
      low_p1_0_regret low_p1_1_regret low_p1_2_regret ///
	  MaxInfo maxinfo_p1_0_regret maxinfo_p1_1_regret maxinfo_p1_2_regret ///
	  late_game late_period ///
	  if game == "MV", cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

* logit regression for p2 in MV games
logit p2_strategy_0 lag_p2_0 lag_p2_1 ///
      low_p2_0_regret low_p2_1_regret low_p2_2_regret ///
	  MaxInfo maxinfo_p2_0_regret maxinfo_p2_1_regret maxinfo_p2_2_regret ///
	  late_game late_period ///
	  if game == "MV", cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

logit p2_strategy_1 lag_p2_0 lag_p2_1 ///
      low_p2_0_regret low_p2_1_regret low_p2_2_regret ///
	  MaxInfo maxinfo_p2_0_regret maxinfo_p2_1_regret maxinfo_p2_2_regret ///
	  late_game late_period ///
	  if game == "MV", cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
  
** old regret analysis
* Logit regression for BM games
logit p1_strategy_1 MaxInfo low_p1_0_regret low_p1_1_regret ///
      if game == "BM", cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

logit p2_strategy_1 MaxInfo low_p2_0_regret low_p2_1_regret ///
      if game == "BM", cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

* Logit regression for MV games
logit p1_strategy_1 MaxInfo low_p1_0_regret low_p1_1_regret low_p1_2_regret ///
      if game == "MV", cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

logit p1_strategy_2 MaxInfo low_p1_0_regret low_p1_1_regret low_p1_2_regret ///
      if game == "MV", cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

logit p2_strategy_1 MaxInfo low_p2_0_regret low_p2_1_regret low_p2_2_regret ///
      if game == "MV", cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

logit p2_strategy_2 MaxInfo low_p2_0_regret low_p2_1_regret low_p2_2_regret ///
      if game == "MV", cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)



***** Analysis with the pooled-player and original-action dataset *****
** data preparation
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

** Avgpay analysis panel data version
* sort the data and generate lag terms
sort cluster_pair_subject_id period
by cluster_pair_subject_id: gen lag_player_0 = player_strategy0[_n-1]
by cluster_pair_subject_id: gen lag_player_1 = player_strategy1[_n-1]
by cluster_pair_subject_id: gen lag_player_2 = player_strategy2[_n-1]

xtset cluster_pair_subject_id period
quietly xtlogit player_strategy0 lag_player_0 ///
      player_avgpay0_standard player_avgpay1_standard player_avgpay2_standard ///
	  MaxInfo MaxInfo_avgpay0 MaxInfo_avgpay1 MaxInfo_avgpay2 ///
	  LateGame LateGame_avgpay0 LateGame_avgpay1 LateGame_avgpay2 ///
	  LatePeriod LatePeriod_avgpay0 LatePeriod_avgpay1 LatePeriod_avgpay2 ///
	  if game == "BM", re vce(cluster cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)
test player_avgpay0_standard + player_avgpay1_standard = 0

quietly xtlogit player_strategy0 lag_player_0 lag_player_1 ///
      player_avgpay0_standard player_avgpay1_standard player_avgpay2_standard ///
	  MaxInfo MaxInfo_avgpay0 MaxInfo_avgpay1 MaxInfo_avgpay2 ///
	  LateGame LateGame_avgpay0 LateGame_avgpay1 LateGame_avgpay2 ///
	  LatePeriod LatePeriod_avgpay0 LatePeriod_avgpay1 LatePeriod_avgpay2 ///
	  if game == "MV", re vce(cluster cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
test player_avgpay0_standard + player_avgpay1_standard = 0
test player_avgpay0_standard + player_avgpay2_standard = 0
	  
quietly xtlogit player_strategy1 lag_player_0 lag_player_1 ///
      player_avgpay0_standard player_avgpay1_standard player_avgpay2_standard ///
	  MaxInfo MaxInfo_avgpay0 MaxInfo_avgpay1 MaxInfo_avgpay2 ///
	  LateGame LateGame_avgpay0 LateGame_avgpay1 LateGame_avgpay2 ///
	  LatePeriod LatePeriod_avgpay0 LatePeriod_avgpay1 LatePeriod_avgpay2 ///
	  if game == "MV", re vce(cluster cluster_subject_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
test player_avgpay1_standard + player_avgpay0_standard = 0
test player_avgpay1_standard + player_avgpay2_standard = 0

** Avgpay analysis non-panel data
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
