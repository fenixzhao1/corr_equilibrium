***** Avgpay analysis panel data version *****
* open dataset

cd "~/Desktop/jotarepos/correq/corr_equilibrium/Data"

***** logit model
use "mv_all.dta", clear


mlogit switch_dir fed_0 fed_1 fed_2 ///
      if regret==3 & information=="L", cluster(cluster_subject_id )
	  	  	   	  
mlogit switch_dir fed_0 fed_1 fed_2 ///
      if regret==3 & information=="L", rrr cluster(cluster_subject_id )
	  	  	   	  
mlogit switch_dir fed_0 fed_1 fed_2 ///
      if regret==2 & information=="L", cluster(cluster_subject_id )
	  	  	   	  
mlogit switch_dir fed_0 fed_1 fed_2 ///
      if regret==2 & information=="H", cluster(cluster_subject_id )
	  	  	   	  
mlogit switch_dir fed_0 fed_1 fed_2 ///
      if regret==3 & information=="H", cluster(cluster_subject_id )

				  
