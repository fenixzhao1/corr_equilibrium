* load data
use "C:\Users\fenix\Dropbox\Working Papers\Correlated Equilibrium\data\corr_equilibrium\Data\stata_pool_mu.dta", clear

* add treatment variables
gen high_information = 0
replace high_information = 1 if information=="H"
gen average_regret = 0
replace average_regret = 1 if regret_info == "A" 
gen high_average = 0
replace high_average = 1 if high_information==1 & average_regret==1

gen A_regret = average_regret * player_avgpaydiff
gen H_info = high_information * player_avgpaydiff
gen A_H = high_average * player_avgpaydiff

* add truncation dummy
gen negative = 0
replace negative = 1 if player_avgpaydiff < 0
gen negative_avgpaydiff = player_avgpaydiff * negative

* CH games mu estimation
reg player_switch player_avgpaydiff A_regret H_info A_H if game=="BM", nocons cluster(session_code)
outreg2 using D:\stata_table, tex nonote se replace nolabel bdec(4)

reg player_switch player_avgpaydiff negative_avgpaydiff A_regret H_info A_H if game=="BM", nocons cluster(session_code)
outreg2 using D:\stata_table, tex nonote se append nolabel bdec(4)

* MU games mu estimation
reg player_switch player_avgpaydiff A_regret H_info A_H if game=="MV", nocons cluster(session_code)
outreg2 using D:\stata_table, tex nonote se append nolabel bdec(4)

reg player_switch player_avgpaydiff negative_avgpaydiff A_regret H_info A_H if game=="MV", nocons cluster(session_code)
outreg2 using D:\stata_table, tex nonote se append nolabel bdec(4)

