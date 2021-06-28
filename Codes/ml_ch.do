cd "~/Desktop/jotarepos/correq/corr_equilibrium/Data"

capture program drop pasado 
capture program drop pasa
capture program drop terco
capture program drop terca
capture program drop trunca_1
capture program drop trunca_2
capture program drop trunca_i


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

program define terca
args logl beta1 delta
quietly{
replace p1=. 
replace p2=.
replace p1=(exp(`beta1'*player_avgpay1)*`delta')/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay0)) if p1==.
replace p2=(exp(`beta1'*player_avgpay0)*`delta')/(exp(`beta1'*player_avgpay1)+exp(`beta1'*player_avgpay0)) if p2==.

replace `logl'=ln(p1*player_strategy*(1-last_played)+(1-p2)*player_strategy*(last_played)+p2*(1-player_strategy)*(last_played)+(1-p1)*(1-player_strategy)*(1-last_played))
}
end


program define trunca_1
args logl beta1 beta2
quietly{
replace p1=. 
replace p2=.
replace p1=(exp(`beta1'*(regret_10)+`beta2'*regret_10*(negative_regret_1)))/(1+exp(`beta1'*(regret_10)+`beta2'*regret_10*(negative_regret_1)) ) if p1==.
replace p2=exp(`beta1'*(-1*regret_10)+`beta2'*-1*regret_10*(1-negative_regret_1))/(1+exp(`beta1'*(-1*regret_10)+`beta2'*-1*regret_10*(1-negative_regret_1)) ) if p2==.

replace `logl'=ln(p1*player_strategy*(1-last_played)+(1-p2)*player_strategy*(last_played)+p2*(1-player_strategy)*(last_played)+(1-p1)*(1-player_strategy)*(1-last_played))
}
end


program define trunca_2
args logl beta1 beta2
quietly{
replace p1=. 
replace p1=exp(`beta1'*(regret_10)+`beta2'*regret_10*(negative_regret_1))/(1+exp(`beta1'*(regret_10)+`beta2'*regret_10*(negative_regret_1)) ) if p1==.
replace `logl'=ln(p1*player_strategy+(1-p1)*(1-player_strategy))
}
end


program define trunca_i
args logl beta1 beta2 delta
quietly{
replace p1=. 
replace p2=.
replace p1= (exp(`beta1'*(regret_10)+`beta2'*regret_10*(negative_regret_1))*`delta')/(1+exp(`beta1'*(regret_10)+`beta2'*regret_10*(negative_regret_1)) ) if p1==.
replace p2= (exp(`beta1'*(-1*regret_10)+`beta2'*-1*regret_10*(1-negative_regret_1))*`delta')/(1+exp(`beta1'*(-1*regret_10)+`beta2'*-1*regret_10*(1-negative_regret_1)) ) if p2==.

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
mat start=(0.3, 0.05)
ml model lf terca /beta1 /delta, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore

preserve
keep if information=="L" & regret==3
mat start=(0.3, 0.05)
ml model lf terca /beta1 /delta, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore



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



**Truncated
preserve
keep if information=="L" & regret==2
mat start=(0.03, -0.05)
ml model lf trunca_1 /beta1 /beta2, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore

preserve
keep if information=="L" & regret==3
mat start=(0.03, -0.05)
ml model lf trunca_1 /beta1 /beta2, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore


preserve
keep if information=="L" & regret==2
mat start=(0.03, -0.05)
ml model lf trunca_2 /beta1 /beta2, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore
preserve
keep if information=="L" & regret==3
mat start=(0.03, -0.05)
ml model lf trunca_2 /beta1 /beta2, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore


preserve
keep if information=="L" & regret==2
mat start=(0.03, -0.05)
ml model lf trunca_i /beta1 /beta2, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore


****
**Truncated Inertia
preserve
keep if information=="L" & regret==2
mat start=(0.03, -0.05, 0.5)
ml model lf trunca_i /beta1 /beta2 /delta, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore

preserve
keep if information=="L" & regret==3
mat start=(0.03, -0.05, 0.5)
ml model lf trunca_i /beta1 /beta2 /delta, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
restore
****









mat start=(0.20)
ml model lf pasado /beta1, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)

mat start=(0.0003, -0.0002, 0.50, 0.50)
ml model lf pasa /beta1 /beta2 /beta0 /beta3, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)
