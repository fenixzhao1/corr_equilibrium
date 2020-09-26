* open dataset
use "D:\Dropbox\Working Papers\Correlated Equilibrium\data\corr_equilibrium\data\pilot_9_13.dta"

* encode session_round_pair_id
encode session_round_pair_id, gen (individual_id)

* generate treatment dummies
gen MaxInfo = 0
replace MaxInfo = 1 if information == "MaxInfo"

* generate other regret form
gen low_p1_0_regret = p1_strategy_0_regret / 100
gen low_p1_1_regret = p1_strategy_1_regret / 100
gen low_p1_2_regret = p1_strategy_2_regret / 100
gen low_p2_0_regret = p2_strategy_0_regret / 100
gen low_p2_1_regret = p2_strategy_1_regret / 100
gen low_p2_2_regret = p2_strategy_2_regret / 100
gen low_p3_0_regret = p3_strategy_0_regret / 100
gen low_p3_1_regret = p3_strategy_1_regret / 100
gen low_p3_2_regret = p3_strategy_2_regret / 100

gen log_p1_0_regret = log(p1_strategy_0_regret)
gen log_p1_1_regret = log(p1_strategy_1_regret)
gen log_p1_2_regret = log(p1_strategy_2_regret)
gen log_p2_0_regret = log(p2_strategy_0_regret)
gen log_p2_1_regret = log(p2_strategy_1_regret)
gen log_p2_2_regret = log(p2_strategy_2_regret)
gen log_p3_0_regret = log(p3_strategy_0_regret)
gen log_p3_1_regret = log(p3_strategy_1_regret)
gen log_p3_2_regret = log(p3_strategy_2_regret)


* Logit regression for BM games
logit p1_strategy_1 MaxInfo low_p1_0_regret low_p1_1_regret ///
      if game == 1, cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

logit p2_strategy_1 MaxInfo low_p2_0_regret low_p2_1_regret ///
      if game == 1, cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

* Logit regression for MV games
logit p1_strategy_1 MaxInfo low_p1_0_regret low_p1_1_regret low_p1_2_regret ///
      if game == 3, cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

logit p1_strategy_2 MaxInfo low_p1_0_regret low_p1_1_regret low_p1_2_regret ///
      if game == 3, cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

logit p2_strategy_1 MaxInfo low_p2_0_regret low_p2_1_regret low_p2_2_regret ///
      if game == 3, cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

logit p2_strategy_2 MaxInfo low_p2_0_regret low_p2_1_regret low_p2_2_regret ///
      if game == 3, cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

* Logit regression for FT games
logit p1_strategy_1 MaxInfo low_p1_0_regret low_p1_1_regret ///
      if game == 2, cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

logit p2_strategy_1 MaxInfo low_p2_0_regret low_p2_1_regret ///
      if game == 2, cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

logit p3_strategy_1 MaxInfo low_p3_0_regret low_p3_1_regret low_p3_2_regret ///
      if game == 2, cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

logit p3_strategy_2 MaxInfo low_p3_0_regret low_p3_1_regret low_p3_2_regret ///
      if game == 2, cluster(individual_id)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
