***** data preparation *****
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



***** regret analysis *****
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

	  
	  
***** old regret analysis *****
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
