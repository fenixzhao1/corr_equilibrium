capture program drop pasado 
capture program drop pasa
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



use bml_lm.dta, clear

gen double p1=.

mat start=(0.20)
ml model lf pasado /beta1, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)

mat start=(0.0003, -0.0002, 0.50, 0.50)
ml model lf pasa /beta1 /beta2 /beta0 /beta3, cluster(cluster_subject_id)
ml init start, copy
ml max, trace search(norescale)





