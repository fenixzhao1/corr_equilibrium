capture program drop pasado 
capture program drop pasa
capture program drop terco

program define pasado
args logl beta1 

quietly{
replace p1=. 

replace p1=exp(`beta1'*player_avgpay1)/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay0)) if p1==.

replace `logl'=ln(p1*player_strategy+(1-p1)*(1-player_strategy))
}
end

program define pasa
args logl beta1 beta2 beta0 beta3
quietly{
replace p1=. 

replace p1=exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))/(exp(`beta0'+`beta1'*regret_10*(1-negative_regret_1))+exp(`beta3'+`beta2'*regret_10*(-negative_regret_1))) if p1==.

replace `logl'=ln(p1*player_strategy+(1-p1)*(1-player_strategy))
}
end


program define terco
args logl beta1
quietly{
replace p1=. 
replace p2=.
replace p1=(exp(`beta1'*player_avgpay1)*0.2)/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay0)) if p1==.
replace p2=(exp(`beta1'*player_avgpay0)*0.2)/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay0)) if p2==.

replace `logl'=ln(p1*player_strategy*(1-last_played)+(1-p2)*player_strategy*(last_played)+p2*(1-player_strategy)*(last_played)+(1-p1)*(1-player_strategy)*(1-last_played))
}
end


use bml_lm.dta, clear

gen last_played = player_strategy[_n-1]
replace last_played =. if period==1

gen double p1=.
gen double p2=.

drop if last_played ==. 
preserve
keep if information=="L" & regret==2
mat start=(0.03)
ml model lf terco /beta1 , cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore

preserve
keep if information=="L" & regret==3
mat start=(0.03)
ml model lf terco /beta1 , cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore





mat start=(0.20)
ml model lf pasado /beta1, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)

mat start=(0.0003, -0.0002, 0.50, 0.50)
ml model lf pasa /beta1 /beta2 /beta0 /beta3, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)





