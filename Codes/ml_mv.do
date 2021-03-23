capture program drop mv_game_ml
capture program drop mv_game_ml_2

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
args logl beta1 beta2 beta0 beta3
quietly{
replace p1=. 
replace p2=. 

replace p1=exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))/(1+exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))+exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))) if p1==.
replace p2=exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))/(1+exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))+exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))) if p2==.

*replace p1=exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))/(exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))+exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))) if p1==.
*replace p2=exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))/(exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))+exp(`beta3'+`beta2'*regret_20*(1-negative_regret_2))) if p2==.

replace `logl'=ln(p1*player_strategy_1+(p2)*(player_strategy_2)+(1-p1-p2)*player_strategy_0)
}
end
*+exp(`beta4'+`beta5'*regret_10*(-negative_regret_1)+`beta6'*regret_20*(-negative_regret_2))




use mv_all.dta, clear
gen double p1=.
gen double p2=.

g player_strategy_1=switch_dir==1
g player_strategy_2=switch_dir==2
g player_strategy_0=switch_dir==0
replace player_strategy_1=. if switch_dir==.
replace player_strategy_2=. if switch_dir==.
replace player_strategy_0=. if switch_dir==.


g regret_10 = fed_1-fed_0
g negative_regret_1 = 0 
replace negative_regret_1 = 1 if regret_10<0

g regret_20 = fed_2-fed_0
g negative_regret_2 = 0 
replace negative_regret_2 = 1 if regret_20<0

keep if information=="L" & regret==2 & switch_dir!=.
mat start=(0.0003, 0.0007, 0.2, 0.7)
ml model lf mv_game_ml /beta1 /beta2 /beta0 /beta3, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)

ml model lf mv_game_ml_2 /beta1 /beta2 /beta0 /beta3, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)




