capture program drop mv_game_ml
capture program drop mv_game_ml_2
capture program drop mv_game_ml_3
capture program drop mv_game_ml_4
capture program drop mv_game_ml_5
capture program drop mv_game_terco
capture program drop mv_game_terca
capture program drop mv_game_trunco


program define mv_game_ml
args logl beta1 beta2 beta0 beta3
quietly{
replace p1=. 
replace p2=. 

replace p1=exp(`beta0'+`beta1'*regret_10)/(1+exp(`beta0'+`beta1'*regret_10)+exp(`beta3'+`beta2'*regret_20)) if p1==.
replace p2=exp(`beta3'+`beta2'*regret_20)/(1+exp(`beta0'+`beta1'*regret_10)+exp(`beta3'+`beta2'*regret_20)) if p2==.

*replace p1=exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))/(exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))+exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))) if p1==.
*replace p2=exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))/(exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))+exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))) if p2==.

replace `logl'=ln(p1*player_strategy_1+(p2)*(player_strategy_2)+(1-p1-p2)*player_strategy_0)
}
end
*+exp(`beta4'+`beta5'*regret_10*(-negative_regret_1)+`beta6'*regret_20*(-negative_regret_2))

program define mv_game_ml_2
args logl beta1 beta2 beta4 beta5
quietly{
replace p1=. 
replace p2=. 
replace p0=.
replace p1=exp(`beta1'*fed_1)/(exp(`beta1'*fed_1)+exp(`beta2'*fed_2)+exp(`beta5'+`beta4'*fed_0)) if p1==.
replace p2=exp(`beta2'*fed_2)/(exp(`beta1'*fed_1)+exp(`beta2'*fed_2)+exp(`beta5'+`beta4'*fed_0)) if p2==.
replace p0=exp(`beta5'+`beta4'*fed_0)/(exp(`beta1'*fed_1)+exp(`beta2'*fed_2)+exp(`beta5'+`beta4'*fed_0)) if p0==.

replace `logl'=ln(p1*player_strategy_1+(p2)*(player_strategy_2)+(p0)*player_strategy_0)
}
end

program define mv_game_ml_3
args logl beta1 beta2 beta0 beta3
quietly{
replace p1=. 
replace p2=. 
replace p1=exp(`beta0'+`beta1'*regret_10)/(1+exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))+exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))) if p1==.
replace p2=exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))/(1+exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))+exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))) if p2==.

*replace p1=exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))/(exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))+exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))) if p1==.
*replace p2=exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))/(exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))+exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))) if p2==.

replace `logl'=ln(p1*player_strategy_1+(p2)*(player_strategy_2)+(1-p1-p2)*player_strategy_0)
}
end

program define mv_game_ml_4
args logl beta1  beta5
quietly{
replace p1=. 
replace p2=. 
replace p0=.
replace p1=exp(`beta1'*player_avgpay1)/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay2)+exp(`beta5'+`beta1'*player_avgpay0)) if p1==.
replace p2=exp(`beta1'*player_avgpay2)/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay2)+exp(`beta5'+`beta1'*player_avgpay0)) if p2==.
replace p0=exp(`beta5'+`beta1'*player_avgpay0)/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay2)+exp(`beta5'+`beta1'*player_avgpay0)) if p0==.

replace `logl'=ln(p1*player_strategy_1+(p2)*(player_strategy_2)+(p0)*player_strategy_0)
}
end

program define mv_game_ml_5
args logl beta1 
quietly{
replace p1=. 
replace p2=. 
replace p0=.
replace p1=exp(`beta1'*player_avgpay1)/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay2)+exp(`beta1'*player_avgpay0)) if p1==.
replace p2=exp(`beta1'*player_avgpay2)/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay2)+exp(`beta1'*player_avgpay0)) if p2==.
replace p0=exp(`beta1'*player_avgpay0)/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay2)+exp(`beta1'*player_avgpay0)) if p0==.

replace `logl'=ln(p1*player_strategy_1+(p2)*(player_strategy_2)+(p0)*player_strategy_0)
}
end

program define mv_game_terco
args logl beta1 
quietly{
replace p1=. 
replace p2=. 
replace p0=.
replace p1=(exp(`beta1'*player_avgpay1)*0.2)/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay2)+exp(`beta1'*player_avgpay0)) if p1==.
replace p2=(exp(`beta1'*player_avgpay2)*0.2)/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay2)+exp(`beta1'*player_avgpay0)) if p2==.
replace p0=(exp(`beta1'*player_avgpay0)*0.2)/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay2)+exp(`beta1'*player_avgpay0)) if p0==.

replace `logl'=ln(p1*player_strategy_1*(1-jugado_1)+(1-p2-p0)*player_strategy_1*(jugado_1)+(p2)*(player_strategy_2)*(1-jugado_2)+(1-p1-p0)*(jugado_2)*(player_strategy_2)+(p0)*player_strategy_0*(1-jugado_0)+(1-p1-p2)*jugado_0*player_strategy_0)
}
end

program define mv_game_terca
args logl beta1 delta
quietly{
replace p1=. 
replace p2=. 
replace p0=.
replace p1=(exp(`beta1'*player_avgpay1)*`delta')/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay2)+exp(`beta1'*player_avgpay0)) if p1==.
replace p2=(exp(`beta1'*player_avgpay2)*`delta')/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay2)+exp(`beta1'*player_avgpay0)) if p2==.
replace p0=(exp(`beta1'*player_avgpay0)*`delta')/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay2)+exp(`beta1'*player_avgpay0)) if p0==.

replace `logl'=ln(p1*player_strategy_1*(1-jugado_1)+(1-p2-p0)*player_strategy_1*(jugado_1)+(p2)*(player_strategy_2)*(1-jugado_2)+(1-p1-p0)*(jugado_2)*(player_strategy_2)+(p0)*player_strategy_0*(1-jugado_0)+(1-p1-p2)*jugado_0*player_strategy_0)
}
end


program define mv_game_trunco
args logl beta1 beta2
quietly{
replace p10=. 
replace p20=. 
replace p01=.
replace p21=.
replace p02=.
replace p12=.
	
replace p10=exp(`beta1'*(player_avgpay1-player_avgpay0)+`beta2'*(player_avgpay1-player_avgpay0)*du_10)/ ///
	(1+ exp(`beta1'*(player_avgpay1-player_avgpay0)+`beta2'*(player_avgpay1-player_avgpay0)*du_10) +  ///
	exp(`beta1'*(player_avgpay2-player_avgpay0)+`beta2'*(player_avgpay2-player_avgpay0)*du_20)  ///
	)

replace p20=exp(`beta1'*(player_avgpay2-player_avgpay0)+`beta2'*(player_avgpay2-player_avgpay0)*du_20)/ ///
	(1+ exp(`beta1'*(player_avgpay1-player_avgpay0)+`beta2'*(player_avgpay1-player_avgpay0)*du_10) +  ///
	exp(`beta1'*(player_avgpay2-player_avgpay0)+`beta2'*(player_avgpay2-player_avgpay0)*du_20)  ///
	)

replace p01=exp(`beta1'*(player_avgpay0-player_avgpay1)+`beta2'*(player_avgpay0-player_avgpay1)*(1-du_10))/ ///
	(1+ exp(`beta1'*(player_avgpay0-player_avgpay1)+`beta2'*(player_avgpay0-player_avgpay1)*(1-du_10)) +  ///
	exp(`beta1'*(player_avgpay2-player_avgpay1)+`beta2'*(player_avgpay2-player_avgpay1)*(du_21))  ///
	)	
	
replace p21=exp(`beta1'*(player_avgpay2-player_avgpay1)+`beta2'*(player_avgpay2-player_avgpay1)*du_21)/ ///
	(1+ exp(`beta1'*(player_avgpay2-player_avgpay1)+`beta2'*(player_avgpay2-player_avgpay1)*du_21) +  ///
	exp(`beta1'*(player_avgpay0-player_avgpay1)+`beta2'*(player_avgpay0-player_avgpay1)*(1-du_10))  ///
	)
	
replace p02=exp(`beta1'*(player_avgpay0-player_avgpay2)+`beta2'*(player_avgpay0-player_avgpay2)*(1-du_20))/ ///
	(1+ exp(`beta1'*(player_avgpay1-player_avgpay2)+`beta2'*(player_avgpay1-player_avgpay2)*(1-du_21)) +  ///
	exp(`beta1'*(player_avgpay0-player_avgpay2)+`beta2'*(player_avgpay0-player_avgpay2)*(1-du_20))  ///
	)	


replace p12=exp(`beta1'*(player_avgpay1-player_avgpay2)+`beta2'*(player_avgpay1-player_avgpay2)*(1-du_21))/ ///
	(1+ exp(`beta1'*(player_avgpay1-player_avgpay2)+`beta2'*(player_avgpay1-player_avgpay2)*(1-du_21)) +  ///
	exp(`beta1'*(player_avgpay0-player_avgpay2)+`beta2'*(player_avgpay0-player_avgpay2)*(1-du_20))  ///
	)
		
	
	
replace `logl'=ln((p10*player_strategy_1 + p20*player_strategy_2 + (1-p10-p20)*(player_strategy_0))*jugado_0+ (p01*player_strategy_0 + p21*player_strategy_2 + (1-p01-p21)*player_strategy_1)*jugado_1+(p02*player_strategy_0 + p12*player_strategy_1  + (1-p02-p12)*player_strategy_2)*jugado_2)
}
end


use mv_all.dta, clear
gen double p1=.
gen double p2=.
gen double p0=.

gen double p01=.
gen double p02=.
gen double p12=.
gen double p10=.
gen double p21=.
gen double p20=.

g player_strategy_1=player_strategy==1
g player_strategy_2=player_strategy==2
g player_strategy_0=player_strategy==0

gen last_played = player_strategy[_n-1]
replace last_played =. if period==1

g jugado_1=last_played==1
g jugado_2=last_played==2
g jugado_0=last_played==0

g du_10 = 0
g du_20 = 0 
g du_21 = 0 

replace du_10 = 1 if player_avgpay1>player_avgpay0
replace du_20 = 1 if player_avgpay2>player_avgpay0
replace du_21 = 1 if player_avgpay2>player_avgpay1



preserve
drop if last_played==.
keep if information=="L" & regret==3
mat start=(0.15, -0.001)
ml model lf mv_game_trunco /beta1 /beta2, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(on)
restore



preserve
keep if information=="L" & regret==3
mat start=(0.03)
ml model lf mv_game_terco /beta1 , cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(on)
restore

preserve
keep if information=="L" & regret==3
mat start=(0.03, 0.5)
ml model lf mv_game_terca /beta1 /delta , cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(on)
restore


preserve
keep if information=="L" & regret==2
mat start=(0.03, 0.5)
ml model lf mv_game_terca /beta1 /delta , cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(on)
restore




preserve
keep if information=="L" & regret==2
mat start=(0.03)
ml model lf mv_game_ml_5 /beta1 , cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(on)
restore





preserve
keep if information=="L" & regret==3 
mat start=(0.0003, 0.0007)
ml model lf mv_game_ml_4 /beta1 /beta5, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(on)
restore

preserve
keep if information=="L" & regret==2 
mat start=(0.0003, 0.0007)
ml model lf mv_game_ml_4 /beta1 /beta5, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(on)
restore



g player_strategy_1=switch_dir==1
g player_strategy_2=switch_dir==2
g player_strategy_0=switch_dir==0
replace player_strategy_1=. if switch_dir==.
replace player_strategy_2=. if switch_dir==.
replace player_strategy_0=. if switch_dir==.


g regret_10 = (fed_1-fed_0)/100
g negative_regret_1 = 0 
replace negative_regret_1 = 1 if regret_10<0

g regret_20 = (fed_2-fed_0)/100
g negative_regret_2 = 0 
replace negative_regret_2 = 1 if regret_20<0

preserve
keep if information=="L" & regret==2 & switch_dir!=.
mat start=(0.0003, 0.0007, 0.2, 0.7)
ml model lf mv_game_ml /beta1 /beta2 /beta0 /beta3, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore

preserve
keep if information=="L" & regret==3 & switch_dir!=.
mat start=(0.0003, 0.0007, 0.2, 0.7)
ml model lf mv_game_ml /beta1 /beta2 /beta0 /beta3, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore

preserve
keep if information=="H" & regret==2 & switch_dir!=.
mat start=(0.0003, 0.0007, 0.2, 0.7)
ml model lf mv_game_ml /beta1 /beta2 /beta0 /beta3, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore

preserve
keep if information=="H" & regret==3 & switch_dir!=.
mat start=(0.0003, 0.0007, 0.2, 0.7)
ml model lf mv_game_ml /beta1 /beta2 /beta0 /beta3, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore

preserve
keep if information=="L" & regret==2 & switch_dir!=.
mat start=(0.0003, 0.0007, 0.2, 0.7)
ml model lf mv_game_ml_3 /beta1 /beta2 /beta0 /beta3, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore

preserve
keep if information=="L" & regret==3 & switch_dir!=.
mat start=(0.0003, 0.0007, 0.2, 0.7)
ml model lf mv_game_ml_3 /beta1 /beta2 /beta0 /beta3, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore

preserve
keep if information=="H" & regret==2 & switch_dir!=.
mat start=(0.0003, 0.0007, 0.2, 0.7)
ml model lf mv_game_ml_3 /beta1 /beta2 /beta0 /beta3, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore

preserve
keep if information=="H" & regret==3 & switch_dir!=.
mat start=(0.0003, 0.0007, 0.2, 0.7)
ml model lf mv_game_ml_3 /beta1 /beta2 /beta0 /beta3, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore

preserve
keep if information=="L" & regret==3 & switch_dir!=.
mat start=(0.0003, 0.0007, 0.0003, 0.2)
ml model lf mv_game_ml_2 /beta1 /beta2 /beta4 /beta5, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(on)
restore

