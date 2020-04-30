#
# This is an example to show the design for questionnaires dynamically displayed on the mobile backend
# through ad-hoc analysis for decision-making based on mobile user's feedback on live
#
# Any patient related dta has been removed from this example
#

demo the evaluation of model features in order to design the questionnaires
# to collect patient feedback info
Patient data has been either removed or de-identified in this example
Created on Wed Jan 27 10:21:38 2016

@author: Lingtao Cao
"""

#Dynamically assign the question list


#imputing and modelling functions

###############################################################################
# User defined function for inference of missing information
###############################################################################
findCondPop <- function(patient) {
  # Find the conditional population based on known features ofthe patient
  # Args:
  #   patient: Patient data. NA is for unknown feature value.
  # Returns:
  #   Row index of summary table corresponding to conditional population

  idx = 1:nrow(cohort_summary)
  for (f in union(features, required_features)) {
    if (!is.na(patient[[f]])) {
      #print(f)
      idx <- intersect(idx, which(cohort_summary[[f]] == patient[[f]]))
      #print(length(idx))
    }
  }
  return(idx)
}


initEstimate <- function(pop_idx, patient) {
  # Initialize the estimate from the conditional population mean
  # Args:
  #   pop_idx: Row index of cohort_summary table
  #   patient: Patient data. NA is for unknown feature value.
  # Returns:
  #   Range and estimates of features for clustering.
  
  z = qnorm(1-0.5*alpha)
  
  if (length(pop_idx)!=0) {
	num_pop = sum(cohort_summary[pop_idx, 'num_patient'])
	cond_est = colSums(cohort_summary[pop_idx, features]*cohort_summary[pop_idx, 'num_patient'])/num_pop
	bound = z*sqrt(cond_est*(1-cond_est)/num_pop)
   }
   else {
	cond_est = pop_avg
	bound = z*sqrt(cond_est*(1-cond_est)/sum(cohort_summary[, 'num_patient']))
   }
  
  for (name in features) {
    if (!is.na(patient[[name]])) {
      cond_est[[name]] <- patient[[name]]
      bound[[name]] <- 0
    }  
  }

  f_hat = c(patient[,c("bmi_30","bmi_30_35","bmi_35_40","bmi_40")], cond_est)
  f_range = c(list(bmi_30=0.0, bmi_30_35=0.0, bmi_35_40=0.0, bmi_40=0.0), bound)

  return(list(f_hat=f_hat, f_range=f_range))  
}

findSupportClusters <- function(cluster, patient_data, range) {
  # Compute the support clusters
  # Args:
  #   c: data frame for the centroid of clusters. Each row of the data frame
  #      contains the coordinate of the centorid of each cluster. The number
  #      of the row is the number of clusters and the number of columns is the
  #      number of dimensions
  #   patient_data: Feature vector of patient information used in clustering.
  #   range: 99% confidence interval of estimation of feature vector of the 
  #          patient. If 0, then corresponding feature is known.
  # Returns:
  #   Index of support clusters.
  
  # Divide known and unknown dimensions or attributes
  known = list()
  unknown = list()
  unknown_bound = list()
  for (name in names(range)) {
    if (range[[name]] == 0) {
      known[[name]] <- patient_data[[name]]
    }
    else {
      unknown[[name]] <- patient_data[[name]]
      unknown_bound[[name]] <- range[[name]]
    }
  }
  
  #print("Known features:")
  #print(known)
  #print("Unknown features:")
  #print(unknown)
  #print(unknown_bound)
  #return(list(v1=known, v2=unknown, v3=unknown_bound))
  #return(known)

  #compute the distance with known features
  known_dim = cluster[,names(known)]
  cluster$known_dist = rowSums((known_dim - known)^2)
  #print(c)

  support_cluster = list()

  for (name in names(unknown)) {
    min = unknown[[name]] - unknown_bound[[name]]
    sc_min = which.min(sqrt((cluster[[name]]-min)^2 + cluster$known_dist))
    max = unknown[[name]] + unknown_bound[[name]]
    sc_max = which.min(sqrt((cluster[[name]]-max)^2 + cluster$known_dist))
    support_cluster <- c(support_cluster, c(sc_min,sc_max))
    #print(name)
    #print(sc_min)
    #print(sc_max)
  }

  return(unique(unlist(support_cluster)))

}

findSupportBox <- function(cluster, support_cluster, f_hat, f_range) {
  # Compute the support box
  # Args:
  #   cluster: data frame for the centroid of clusters. Each row of the data frame
  #            contains the coordinate of the centorid of each cluster. The number
  #            of the row is the number of clusters and the number of columns is the
  #             number of dimensions
  #   support_cluster: Index of support clusters
  #   f_hat: Feature vector of patient information used in clustering.
  #   f_range: 99% confidence interval of estimation of feature vector of the 
  #            patient. If 0, then corresponding feature is known.
  # Returns:
  #   Coordinate of support boxes for features used for clustering.
  
  # select support clusters
  sc = cluster[support_cluster,]
  max_est = mapply(function(x1,y1) x1[[1]]+y1[[1]], f_hat, f_range)
  min_est = mapply(function(x1,y1) x1[[1]]-y1[[1]], f_hat, f_range)

  box_min = list()
  box_max = list()

  for (name in names(sc)) {
    box_min[[name]] <- min(unlist(c(sc[name], min_est[name])))
    box_max[[name]] <- max(unlist(c(sc[name], max_est[name])))
  }

  return(list(box_min=box_min, box_max=box_max))
}

findGroupCluster <- function(cluster, box) {
  # Find clusters inside the support box
  # Args:
  #   cluster: data frame for the centroid of clusters. Each row of the data frame
  #            contains the coordinate of the centorid of each cluster. The number
  #            of the row is the number of clusters and the number of columns is the
  #            number of dimensions
  #   box: Coordinate of support box. box$box_min has the minimum coordinate and 
  #        box$box_max has the maximum coordinate.
  # Returns:
  #   Index of clusters inside the support box.  

  group_cluster = list()
  for (i in 1:nrow(cluster)) {
    if ((prod(cluster[i,] >= box$box_min)) & (prod(cluster[i,] <= box$box_max))) {
      group_cluster <- c(group_cluster, i)
    }
  }
  
  return(unlist(group_cluster))
}

updateEstimates <- function(cluster, cluster_size, group_cluster, f_hat, f_range) {
  # Update the estimates of features for clustering from group clusters.
  # Args:
  #   cluster: data frame for the centroid of clusters. Each row of the data frame
  #            contains the coordinate of the centorid of each cluster. The number
  #            of the row is the number of clusters and the number of columns is the
  #             number of dimensions
  #   cluster_size: number of population in cluster.
  #   group_cluster: Index of group clusters.
  #   f_hat: Feature vector of patient information used in clustering.
  #   f_range: 99% confidence interval of estimation of feature vector of the 
  #            patient. If 0, then corresponding feature is known.
  # Returns:
  #   Range and estimates of features (probability and binary value) for clustering.

  num = 0
  pos_num = list()
  for (i in group_cluster) {
    num = num + cluster_size[i,]
    if (length(pos_num)==0) {
      pos_num = cluster[i,]*cluster_size[i,]
    }
    else {
      pos_num = pos_num + cluster[i,]*cluster_size[i,]
    }
  }

  f_hat_new = pos_num / num
  z = qnorm(1-0.5*alpha)
  f_range_new = z*sqrt(f_hat_new*(1-f_hat_new)/num)
  f_hat_bin_new = f_hat

  for (name in names(f_range)) {
    if (f_range[[name]] == 0) {
      f_range_new[[name]] <- f_range[[name]]
      f_hat_new[[name]] <- f_hat[[name]]
    } else {
      f_hat_bin_new[[name]] <- as.numeric(f_hat_new[[name]] >= pop_avg[[name]])
    }
  }

  return(list(f_hat=f_hat_new, f_range=f_range_new, f_hat_bin=f_hat_bin_new))
}

#function to compute the predicted confidence interval
logitT <- function(x){ return (as.numeric(exp(x)/(1+exp(x)))) }


#modify the model output
computePredCI5 <- function(patient_org, imputed, coe, covm){

newd0 <- patient_org[,c("age", "bmi_pre", "gender")]
newd0$gender = newd0$gender - 1

to_compute <- imputed[,c(features)]
to_compute <- cbind(intercept=1, newd0, to_compute)
to_compute <- as.matrix(to_compute)

s <- coe %*% t(to_compute)
se <- apply(to_compute, 1, function(x) sqrt(t(as.numeric(x)) %*% covm %*% as.numeric(x)))
td <- list(id=patient_org$explorys_patient_id)
td <- c(td, pred_val=logitT(s), pred_upr=logitT(s+cr*se), pred_lwr=logitT(s-cr*se))
td$pred_range=td$pred_upr-td$pred_lwr
return (td)
}



#user_entry='M,52,150,67,1,0,,,0,1,,,'

#ask required demographic information
#age gender weight height (bmi)
demo_input = function(patient_input, subset_input){
usr.age <- readline(prompt="Please enter your age? ")
usr.age = as.numeric(usr.age)
patient_input$age=usr.age
patient_input[,c("age_60", "age_60_70","age_70_80","age_80")] = 0

if(usr.age<60) {patient_input$age_60=1
} else if (usr.age>=60 & usr.age<70) {patient_input$age_60_70=1
} else if (usr.age>=70 & usr.age<80) {patient_input$age_70_80=1
} else {patient_input$age_80=1}

subset_input[,c("age_60", "age_60_70","age_70_80","age_80")]=patient_input[,c("age_60", "age_60_70","age_70_80","age_80")]

usr.weight <- readline(prompt="Please enter your weight(lb)? ")
usr.weight <- as.numeric(usr.weight)
usr.height <- readline(prompt="Please enter your height(in)? ")
usr.height <- as.numeric(usr.height)
usr.bmi = usr.weight*703/(usr.height)^2
patient_input$bmi_pre=usr.bmi

patient_input[,c("bmi_30", "bmi_30_35","bmi_35_40","bmi_40")] = 0
subset_input[,c("bmi_25","bmi_25_30","bmi_30")] = 0
if(usr.bmi<25) {
	subset_input$bmi_25=1
	patient_input$bmi_30=1
} else if(usr.bmi>=25 & usr.bmi<30) {
	subset_input$bmi_25_30=1
	patient_input$bmi_30=1
} else if (usr.bmi>=30 & usr.bmi<35) {
	subset_input$bmi_30=1
	patient_input$bmi_30_35=1
} else if (usr.bmi>=35 & user.bmi<40) {
	patient_input$bmi_35_40=1
	subset_input$bmi_30=1
} else {
	patient_input$bmi_40=1
	subset_input$bmi_30=1
}

usr.gender <- readline(prompt="Please enter your gender(M/F)? ")
patient_input["gender"] <- ifelse(toupper(usr.gender)=="M", 1, 2)
subset_input[,'gender']=patient_input[,'gender']

return(list(patient_input=patient_input, subset_input=subset_input))  
}

ask_next_question = function(pickup_idx, patient_input){

#print the missing features
usr.feature <- readline(noquote(paste("We'd like to ask you - do you have ", features[pickup_idx], "? (Y/N) ", sep="")))
#update the users input
patient_input[,features[pickup_idx]] = ifelse(toupper(usr.feature)=="Y", 1, 0)
return (patient_input)

}


print_feedback_los = function(tmp){

user_feed$pred_value=tmp$pred_val
user_feed$pred_upper=tmp$pred_upr
user_feed$pred_lower=tmp$pred_lwr
usr <- round(as.numeric(tmp[2:5]),3)

restp = 100-(which((qr_los>usr[1])==T)[1]-1)*5
if(restp<20) { riskw="highly likely"
               appendw = "worse than"
} else if (restp>=20 & restp<40) { riskw="likely"
                                   appendw = "somewhat worse than"
} else if (restp>=40 & restp<60) { riskw="neutral" 
                                   appendw = "about the same as"
} else if (restp>=60 & restp<80) { riskw="unlikely"
                                   appendw = "somewhat better than"
} else { riskw="highly unlikely"
         appendw = "much better than"
}
user_feed$pred_likert = riskw

#provide the user feedback
cat(paste(user_feed$context_statement_los, "\n",
      user_feed$patient_condition, 
	  #"which is ", appendw, " the average condition of the studied population, it is ",
	  "it is ",
	  user_feed$pred_likert, 
	  user_feed$quali_outcome_statement_los, "\n",
	  user_feed$numberic_outcome_statement_los, usr[1]*100, "%.\n", 
	  user_feed$ci_outcome_statement_los, usr[3]*100, "% to ", usr[2]*100, "%.\n", sep=""))	  
	  
}


#imputation and modelling
impute_modelling <- function(patient, fcoe, fcov) {

# Initialize the estimates from the conditional population mean
cond_pop <- findCondPop(patient)
init_est <- initEstimate(cond_pop, patient)
f_hat <- init_est$f_hat
f_range <- init_est$f_range

#fix the missing issue
f_hat[which(is.na(f_hat))]=0
f_range[which(is.na(f_range))]=0

# Infer missing features from clustering
support_cluster <- findSupportClusters(cluster_centroid, f_hat, f_range)
box <- findSupportBox(cluster_centroid, support_cluster, f_hat, f_range)
group_cluster <- findGroupCluster(cluster_centroid, box)

if (is.null(group_cluster)) {
  feature_est = f_hat
  feature_range = f_range
  feature_est_bin = f_hat

  for (name in names(f_range)) {
  if (f_range[[name]] != 0) {
      feature_est_bin[[name]] <- as.numeric(f_hat[[name]] >= pop_avg[[name]])
    }
  }
} else {
  new_est <- updateEstimates(cluster_centroid, cluster_size, group_cluster, f_hat, f_range)
  feature_est = new_est$f_hat
  feature_range = new_est$f_range
  feature_est_bin = new_est$f_hat_bin
}


# compute the predicted confidence interval

feature_est = data.frame(feature_est)
#str(feature_est)
feature_range = data.frame(feature_range)

# using the imputed fixed probability
#compute model
tmp <- computePredCI5(patient, feature_est, fcoe, fcov)

return (tmp)
}




#compute the correlations
compute_phi = function(sdf, f1, f2){
	#print (f1)
	#print (f2)
	if (f1==f2) {
		phi_var =1
	} else {
		df = sdf[,c(f1, f2, 'num_patient')]
		#print (head(df, 1))
		a = as.numeric(sum(df[df[,f1]==0 & df[,f2]==0, 'num_patient']))
		b = as.numeric(sum(df[df[,f1]==0 & df[,f2]==1, 'num_patient']))
		c = as.numeric(sum(df[df[,f1]==1 & df[,f2]==0, 'num_patient']))
		d = as.numeric(sum(df[df[,f1]==1 & df[,f2]==1, 'num_patient']))
		#print(paste("a=", a, " b=", b, " c=", c, " d=", d))
		tt = (a+b)*(c+d)*(a+c)*(b+d)
		if (tt >0) {
			phi_var = (a*d-b*c)/sqrt(tt)
		} else{ 
		    phi_var = 0
		}
		
	}
	return (abs(phi_var))
}

#pv = compute_phi(list_id, "pre_comor_Group3_Endocrine", "pre_comor_Group3_Endocrine")
#pv = compute_phi(list_id, "pre_comor_Group3_Endocrine", "pre_comor_Hypertension")
# compute_phi(list_id, "pre_comor_Anemia", "pre_comor_Rheumatism")


	
get_nextz = function(unknown_features){
  #print ("use z order")
  zlist = zorder[which(zorder %in% unknown_features)]
  #zv = unknown_features[match(z, unknown_features)]
  return (zlist)
}

#get the next feature elements
get_nextlist = function(sdf, unknown_features) {
	a = data.frame(matrix(NA, nrow=length(unknown_features), ncol=0))

	for (f in unknown_features) {	
		clist = sapply(unknown_features, function(x) compute_phi(sdf, f, x))
		cdf = data.frame(clist)
		colnames(cdf) = f
		a= cbind(a, cdf)
	}

	d = colSums(a)
	#print (d)
	next_feature = names(d[which(d %in% max(d))])
	if(length(next_feature)>1){
		#sort multiple next features
		#print (paste((length(next_feature)), "features have equal correlation-sum:"))
		#print (paste(next_feature, collapse=" "))
		zlist = get_nextz(next_feature)
		next_feature = zlist[1]
		#print (paste("the top one is ", next_feature))
	}
	#sort and return the next feature list
	next_list = rownames(a)[order(-a[next_feature])]

	#qlist = paste(next_list, collapse=" ")
	#print (qlist)	
	return (next_list)
}

#get the next data elements
get_next_feature <- function(f_sum, subset_input, unknown_features){

    #pass the patient input and subset the whole cohort
	if(length(unknown_features)>1) {
		a = subset_input[,!is.na(subset_input)]
		t = colnames(subset_input)[!is.na(subset_input)]
		list_id = which(apply(f_sum[,t], 1, function(x) all(x==a)))
	    subset_size = sum(f_sum[list_id,'num_patient'])
		#print (paste("numb of patient in subset =", subset_size))
		subset_df = f_sum[list_id,]
	
		#if there is a subset or sample size is big enough threshold=30
		if (subset_size >= 30) {
			#print("use corr")
			next_list = get_nextlist(subset_df, unknown_features)
		} else {
			next_list = get_nextz(unknown_features)
		}
			
	} else { #if there is only one feature left, then no need to sort
		next_list = unknown_features
		subset_df = f_sum
	}
	
	return (list(f_sum=subset_df, next_feature=next_list[1]))
}


get_nextlist_m12 = function(sdf, unknown_features) {
	k = length(unknown_features)
	a = data.frame(matrix(NA, nrow=k, ncol=k))

	feature_left = unknown_features
	i = 1
	for (f in feature_left) {	
		clist = sapply(feature_left, function(x) compute_phi(sdf, f, x))
		a[i:k,i]=clist
		colnames(a)[i] = f
		#print (a)
		feature_left = feature_left[!is.element(feature_left, f)]
		i = i+1
	}
	a[upper.tri(a)]=a[lower.tri(a)]
	d = colSums(a)
	#print (d)
	next_feature = names(d[which(d %in% max(d))])
	#next_feature = names(d[which(d %in% min(d))])
	if(length(next_feature)>1){
		#sort multiple next features
		#print (paste((length(next_feature)), "features have equal correlation-sum:"))
		#print (paste(next_feature, collapse=" "))
		zlist = get_nextz(next_feature)
		next_feature = zlist[1]
		#print (paste("the top one is ", next_feature))
	}
	#sort and return the next feature list
	#next_list = rownames(a)[order(-a[next_feature])]

	#qlist = paste(next_list, collapse=" ")
	#print (qlist)	
	return (next_feature)
}



#get the next data elements
get_next_feature_m12 <- function(f_sum, subset_input, unknown_features){

	
    #pass the patient input and subset the whole cohort
	if(length(unknown_features)>1) {
		a = subset_input[,!is.na(subset_input)]
		t = colnames(subset_input)[!is.na(subset_input)]
		list_id = which(apply(f_sum[,t], 1, function(x) all(x==a)))
	    subset_size = sum(f_sum[list_id,'num_patient'])
		#print (paste("numb of patient in subset =", subset_size))
		subset_df = f_sum[list_id,]
	
		#if there is a subset or sample size is big enough threshold=30
		if (subset_size >= 30) {
			next_feature = get_nextlist_m12(subset_df, unknown_features)
		} else {
			next_feature = (get_nextz(unknown_features))[1]
		}
			
	} else { #if there is only one feature left, then no need to sort
		next_feature = unknown_features
		subset_df = f_sum
	}
	
	return (list(f_sum=subset_df, next_feature=next_feature))
}


#compute the frequency based on the subset applied with all known features
compute_freq = function(sdf, f1){
	#print (f1)
	df = sdf[,c(f1, 'num_patient')]
	#print (head(df, 1))
	a = as.numeric(sum(df[df[,f1]==0, 'num_patient']))
	b = as.numeric(sum(df[df[,f1]==1, 'num_patient']))
	#print(paste("a=", a, " b=", b))
	return (b/(a+b))
}


get_nextlist_m34 = function(sdf, unknown_features) {
	
	d = sapply(unknown_features, function(x) compute_freq(sdf, x))
	#next_feature = names(d[which(d %in% max(d))])
	next_feature = names(d[which(d %in% min(d))])
	#print (next_feature)
	if(length(next_feature)>1){
		#sort multiple next features
		zlist = get_nextz(next_feature)
		next_feature = zlist[1]
		#print (paste("the top one is ", next_feature))
	}
	#sort and return the next feature list
	#next_list = names(d)[order(-d)]

	return (next_feature)
}



#get the next data elements
get_next_feature_m34 <- function(f_sum, subset_input, unknown_features, known_features){

    #if no feature is known, then assign the first feature ranked by z value
	#this step is unnecessary, will be removed and rerun the test later
	if(length(known_features)==0) {
		next_feature = zorder[1]
		subset_df = f_sum
		
	} else {
		if(length(unknown_features)>1){
			a = subset_input[,!is.na(subset_input)]
			t = colnames(subset_input)[!is.na(subset_input)]
			list_id = which(apply(f_sum[,t], 1, function(x) all(x==a)))
			subset_size = sum(f_sum[list_id,'num_patient'])
			#print (paste("numb of patient in subset =", subset_size))
			subset_df = f_sum[list_id,]
		
			#if there is a subset or sample size is big enough threshold=30
			if (subset_size >= 30) {
				next_feature = get_nextlist_m34(subset_df, unknown_features)
			} else {
				next_feature = (get_nextz(unknown_features))[1]
			}
			
		} else { #if there is only one feature left, then no need to sort
			next_feature = unknown_features
			subset_df = f_sum
		}
	}
	
	return (list(f_sum=subset_df, next_feature=next_feature))
}

#get the next data elements
get_next_feature_m56 <- function(f_sum, subset_input, unknown_features){

	if(length(unknown_features)==9) {
		a = subset_input[,!is.na(subset_input)]
		t = colnames(subset_input)[!is.na(subset_input)]
		list_id = which(apply(f_sum[,t], 1, function(x) all(x==a)))
		f_sum= f_sum[list_id,]
		next_feature = get_nextlist_m34(f_sum, unknown_features)
	} else {

		if(length(unknown_features)>1){
			pre_feature = colnames(subset_input[,features])[!is.na(subset_input[,features])]
			subset_df = f_sum[f_sum[,pre_feature]==subset_input[,pre_feature],]
		    next_feature = get_nextlist_m34(subset_df, unknown_features)
		} else { #if there is only one feature left, then no need to sort
			next_feature = unknown_features
		}
	}
	
	return (list(f_sum=f_sum, next_feature=next_feature))
}


#get the next data elements
get_next_feature_m78 <- function(f_sum, subset_input, unknown_features){

	if(length(unknown_features)==9) {
		a = subset_input[,!is.na(subset_input)]
		t = colnames(subset_input)[!is.na(subset_input)]
		#list_id = which(apply(f_sum[,t], 1, function(x) all(x==a)))
		list_id = as.numeric(which(do.call(paste, freq_sum[,t]) %in% paste(a, collapse=" ")))
		f_sum= freq_sum[list_id,]
		next_feature = get_nextlist_m12(f_sum, unknown_features)
	} else {

		if(length(unknown_features)>1){
			pre_feature = colnames(subset_input[,features])[!is.na(subset_input[,features])]
			subset_df = f_sum[f_sum[,pre_feature]==subset_input[,pre_feature],]
		    next_feature = get_nextlist_m12(subset_df, unknown_features)
		} else { #if there is only one feature left, then no need to sort
			next_feature = unknown_features
		}
	}
	
	return (list(f_sum=f_sum, next_feature=next_feature))
}



get_nextlist_m90 = function(sdf, unknown_features, past_features) {
	
	a = data.frame(matrix(NA, nrow=length(past_features), ncol=length(unknown_features)))

	i = 1
	for (f in unknown_features) {	
		clist = sapply(past_features, function(x) compute_phi(sdf, f, x))
		a[,i]=clist
		colnames(a)[i] = f
		#print (a)
		i = i+1
	}
	d = colSums(a)
	#print (d)
	#next_feature = names(d[which(d %in% max(d))])
	next_feature = names(d[which(d %in% min(d))])
	if(length(next_feature)>1){
		zlist = get_nextz(next_feature)
		next_feature = zlist[1]
	}
	#sort and return the next feature list
	#next_list = names(d)[order(-d)]

	return (next_feature)
}


#get the next data elements
get_next_feature_m90 <- function(f_sum, subset_input, unknown_features, known_features){

	if(length(unknown_features)==9) {
		a = subset_input[,!is.na(subset_input)]
		t = colnames(subset_input)[!is.na(subset_input)]
		list_id = as.numeric(which(do.call(paste, freq_sum[,t]) %in% paste(a, collapse=" ")))
		f_sum= freq_sum[list_id,]
		next_feature = get_nextlist_m34(f_sum, unknown_features)
	} else {

		if(length(unknown_features)>1){
			pre_feature = colnames(subset_input[,features])[!is.na(subset_input[,features])]
			subset_df = f_sum[f_sum[,pre_feature]==subset_input[,pre_feature],]
			if(length(known_features)==1) {
				next_feature = get_nextlist_m34(subset_df, unknown_features)
			} else {
				past_features = known_features[-which(known_features %in% pre_feature)]
				next_feature = get_nextlist_m90(subset_df, unknown_features, past_features)
			}
		} else { #if there is only one feature left, then no need to sort
			next_feature = unknown_features
		}
	}
	
	return (list(f_sum=f_sum, next_feature=next_feature))
}


get_nextlist_m112 = function(sdf, unknown_features, feature_set) {
	k=length(feature_set)
	a = data.frame(matrix(NA, nrow=k, ncol=k))
	feature_left = feature_set
	i = 1
	for (f in feature_left) {	
	    clist = sapply(feature_left, function(x) compute_phi(sdf, f, x))
		a[i:k,i]=clist
		colnames(a)[i] = f
		feature_left = feature_left[!is.element(feature_left, f)]
		i = i+1
	}
	#print(a)
	a[upper.tri(a)]=a[lower.tri(a)]
	d = colSums(a)
	d = d[which(names(d) %in% unknown_features)]
	#print (d)
	#next_feature = names(d[which(d %in% max(d))])
	next_feature = names(d[which(d %in% min(d))])
	if(length(next_feature)>1){
		#sort multiple next features
		zlist = get_nextz(next_feature)
		next_feature = zlist[1]
	}
	#sort and return the next feature list
	#next_list = rownames(a)[order(-a[next_feature])]

	return (next_feature)
}


#get the next data elements
get_next_feature_m112 <- function(f_sum, subset_input, unknown_features){

	if(length(unknown_features)==9) {
		a = subset_input[,!is.na(subset_input)]
		t = colnames(subset_input)[!is.na(subset_input)]
		#list_id = which(apply(f_sum[,t], 1, function(x) all(x==a)))
		list_id = as.numeric(which(do.call(paste, freq_sum[,t]) %in% paste(a, collapse=" ")))
		f_sum= freq_sum[list_id,]
		next_feature = get_nextlist_m112(f_sum, unknown_features, unknown_features)
	} else {

		if(length(unknown_features)>1){
			pre_feature = colnames(subset_input[,features])[!is.na(subset_input[,features])]
			subset_df = f_sum[f_sum[,pre_feature]==subset_input[,pre_feature],]
			feature_set = features[-which(features %in% pre_feature)]
		    next_feature = get_nextlist_m112(subset_df, unknown_features, feature_set)
		} else { #if there is only one feature left, then no need to sort
			next_feature = unknown_features
		}
	}
	
	return (list(f_sum=f_sum, next_feature=next_feature))
}


#start the process
#get the next feature by ranking the z values of the model summary
#sort multiple next features
flist <- question_ranked[1,]$feature_rank
flist <- as.character(flist[[1]])
flist <- strsplit(flist," ")
zorder <- as.character(flist[[1]])

#initialize the user data structure

# 1. Load source data for test
cohort = read.csv("V:/data/LOS_proc_variables.csv", header=TRUE)

# add age group
idx = which(cohort$age < 60)
cohort$age_60 = 0
cohort$age_60[idx] = 1
idx = which(cohort$age >= 60 & cohort$age < 70)
cohort$age_60_70 = 0
cohort$age_60_70[idx] = 1
idx = which(cohort$age >= 70 & cohort$age < 80)
cohort$age_70_80 = 0
cohort$age_70_80[idx] = 1
idx = which(cohort$age >= 80)
cohort$age_80 = 0
cohort$age_80[idx] = 1
idx = which(is.na(cohort$age))
cohort[idx, c("age_60","age_60_70","age_70_80","age_80")] = NA

# bmi group
idx = which(cohort$bmi_pre < 30)
cohort$bmi_30 = 0
cohort$bmi_30[idx] = 1
idx = which(cohort$bmi_pre >= 30 & cohort$bmi_pre < 35)
cohort$bmi_30_35 = 0
cohort$bmi_30_35[idx] = 1
idx = which(cohort$bmi_pre >= 35 & cohort$bmi_pre < 40)
cohort$bmi_35_40 = 0
cohort$bmi_35_40[idx] = 1
idx = which(cohort$bmi_pre >= 40)
cohort$bmi_40 = 0
cohort$bmi_40[idx] = 1
idx = which(is.na(cohort$bmi_pre))
cohort[idx, c("bmi_30","bmi_30_35","bmi_35_40","bmi_40")] = NA

cohort$revision = as.numeric(apply(cohort[,c("revision90","revision_18m")], 1, sum) > 0)
cohort$complication = as.numeric(apply(cohort[,c("complication_3m","complication_12m")], 1, sum) > 0)
cohort$post_recovery = as.numeric(apply(cohort[,c("Ortho_visit_90_360","pain","xray")], 1, sum) > 0)

# select data only with columns used
data = cohort[,c("explorys_patient_id", "Length_of_Stay", "revision", "complication", "post_recovery",
"gender", "age", "age_60", "age_60_70", "age_70_80", "age_80",
"bmi_pre", "bmi_30", "bmi_30_35", "bmi_35_40", "bmi_40",
"antibiotics", "ever_smoker", "pre_comor_Group3_Endocrine","pre_comor_Anemia", 
"pre_comor_Group5_MentalDisorder", "pre_comor_Group6_Nervous", "pre_comor_Hypertension",
"pre_comor_Group8_Respiratory", "pre_comor_Rheumatism",
"insurance_medicare", "insurance_medicaid", "insurance_private", "insurance_other", 
"race_Caucasian", "race_AfricanAmerican", "race_Asian", "race_Latino", "race_Native"   
)]

#extract LOS longer than 1 days
nrow(data)
data <- data[data$Length_of_Stay > 1,]
nrow(data)
data <- data[complete.cases(data),]
nrow(data)

#factorize the data
dat <- apply(data[,c(6, 17:34)], 2, factor)
summary(dat)
length(rownames(dat))
dat = cbind(data[,c(1:2, 35:36, 3:5, 7:16)], dat)
names(dat)

reg_los <- glm(LOS ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat, family=binomial)

cr <<- qnorm(0.975)
coe.los <- as.numeric(reg_los$coefficients)
cov.los <- vcov(reg_los)
#quartile of the whole cohort
qr_los = quantile(reg_los$fitted, probs=seq(0,1,0.05))

library(RJDBC)
smDriver = JDBC(driverClass = "com.vertica.jdbc.Driver", classPath = "C:\\Program Files\\Vertica Systems\\JDBC\\vertica-jdbc-7.1.2-0.jar")
sm = dbConnect(smDriver, "jdbc:vertica://vertica-als-1:5433/db1", "lingtao.cao", "Hau37Eq6")

cohort_summary = dbGetQuery(sm, 
"select *
from sandbox_jj.LOS_cohort_summary_sunhwan")

# replace NA to 0 for age and bmi columns
required_features = c('age_60', 'age_60_70', 'age_70_80', 'age_80', 
			    'gender', 'bmi_30', 'bmi_30_35', 'bmi_35_40', 'bmi_40')

cohort_summary[,required_features] <- apply(cohort_summary[,required_features], 2, 
							  function(x){replace(x, is.na(x), 0)})
							  
features = c("antibiotics", "ever_smoker", "pre_comor_Group3_Endocrine","pre_comor_Anemia", 
		 "pre_comor_Group5_MentalDisorder", "pre_comor_Group6_Nervous", "pre_comor_Hypertension",
		 "pre_comor_Group8_Respiratory", "pre_comor_Rheumatism")
		 
num_features = length(features)

# centroid of 8 clusters
cluster_centroid = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_centers_8_binary_48K.csv", header=TRUE)
# cluster size
cluster_size = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_size_8_binary_48K.csv", header=TRUE)
# cluster assignment
cluster_assignment = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_cluster_binary_48K.csv", header=TRUE)

# Average of features from the entire population
pop_avg = colSums(cohort_summary[, features]*cohort_summary[, 'num_patient'])/sum(cohort_summary[, 'num_patient'])

# alpha level for 99% confidence interval
alpha = 0.01
					


#user feedback semantics 
user_feed <- data.frame(context_statement_los=character(0), patient_condition=character(0), 
quali_outcome_statement_los=character(0), numberic_outcome_statement_los=character(0), ci_outcome_statement_los=character(0),
pred_value=numeric(0), pred_likert=character(0), pred_upper=numeric(0), pred_lower=numeric(0),

context_statement_rev_v1=character(0), context_statement_rev_v2=character(0),
quali_outcome_statement_rev=character(0), numberic_outcome_statement_rev=character(0), ci_outcome_statement_rev=character(0),
pred_value_rev=numeric(0), pred_likert_rev=character(0), pred_upper_rev=numeric(0), pred_lower_rev=numeric(0)
)

user_feed <- as.list(user_feed)

#LOS
user_feed$context_statement_los <- "According to existing studies, the average length of stay after TKR is between 3-4 days. Approximately 7% of patients have to stay 6 days or longer."
user_feed$patient_condition= "Based on your demographic and current health condition, "
user_feed$quali_outcome_statement_los= " that you will have to stay 6 days or longer."
user_feed$numberic_outcome_statement_los= "The probability that you will have to stay 6 days or longer is "
user_feed$ci_outcome_statement_los= "The probability that you will stay at least 6 days or longer ranges from "

#revision
user_feed$context_statement_rev_v1 = "According to existing studies, revision rate within 18 months is around 1.6%."
user_feed$context_statement_rev_v2 = "According to existing studies, two out of 100 patients will be readmitted to the hospital within 18 months for a follow up surgery."
user_feed$quali_outcome_statement_rev = " that you will be readmitted."
user_feed$numberic_outcome_statement_rev = "The probability that you will be readmitted is "
user_feed$ci_outcome_statement_rev = "The probability that you will be readmitted ranges from "



freq_sum = read.csv("H:/My Documents/workf/R_code/freq_summary.csv", header=TRUE)


					
		 
#get the input data structure		

test_dynamic = function(){
 
set.seed(0)
test_patient_id = sample(data$explorys_patient_id, 1)
pid = toString(test_patient_id)
patient_org = data[data$explorys_patient_id %in% test_patient_id,]
#add row ID
rownames(patient_org) = patient_org$explorys_patient_id
patient_input <- patient_org
patient_input[features] <- NA

#get the summary frequency table
subset_input = freq_sum[sample(nrow(freq_sum),1),c(1:17)]
subset_input[features] <- NA

		 
#step 1: ask questions for required field
pt = demo_input(patient_input, subset_input)
patient_input = pt$patient_input
subset_input = pt$subset_input

unknown_features = features

#step 2: process the patient input
while(length(unknown_features)!=0){

#pass the patient input and subset the whole cohort
	if(length(unknown_features)>1) {
		print (paste("Number of unknown features =", length(unknown_features)))
		a = subset_input[,!is.na(subset_input)]
		t = colnames(subset_input)[!is.na(subset_input)]
		#print (dim(freq_sum))
		list_id = which(apply(freq_sum[,t], 1, function(x) all(x==a)))
	    #print (length(list_id))
		subset_size = sum(freq_sum[list_id,'num_patient'])
		print (paste("numb of patient in subset =", subset_size))
		subset_df = freq_sum[list_id,]
		
		#alternative ways
		#subset_input[,colnames(subset_input)[!is.na(subset_input)]]
		#subset_input[,-which(colnames(subset_input) %in% colnames(subset_input)[is.na(subset_input)] )]
		#t <- paste(colnames(subset_input)[is.na(subset_input)], collapse=",")
		#subset(subset_input, select=-c(t))
		
		#if there is a subset or sample size is big enough threshold=30
		if (subset_size >= 30) {
		#if(length(list_id)>0) {
			next_list = get_nextlist(subset_df, unknown_features)
		} else {
			next_list = get_nextz(unknown_features)
		}
			
	} else { #if there is only one feature left, then no need to sort
		next_list = unknown_features
	}
	
	#update freq summary table
	freq_sum = subset_df
			
	print (next_list)
	
#step 3: impute missing and compute the model
	#print(noquote("In this step we will impute the feature(s) of "))
	#print(unknown_features)
	tmp <- impute_modelling(patient_input, coe.los, cov.los)
	print_feedback_los(tmp)

	#ask question for the next feature [1:1] or next 3 features [1:3]
	pickup_idx = which(features %in% next_list[1:1])
	tmp$next_question=features[pickup_idx]
	patient_input = ask_next_question(pickup_idx, patient_input)

	#update the unknown features
	known_features = colnames(patient_input[,features])[!is.na(patient_input[,features])]
	unknown_features = features[! features %in% known_features]
	subset_input[known_features]=patient_input[known_features] 


#repeat from step 2
}


#when all features are known, compute model
tmpfinal <- computePredCI5(patient_input, patient_input, coe.los, cov.los)
print_feedback_los(tmpfinal)
print("end of the test")

return (patient_input)
}






















#test 1k patient and compare the results with the static feature order
#sample the patients
num_test=1000
set.seed(0)
test_id_set = sample(intersect(data$explorys_patient_id, cluster_assignment$X), num_test)
patient_set = data[data$explorys_patient_id %in% test_id_set,]
rownames(patient_set) = patient_set$explorys_patient_id

cr <<- qnorm(0.975)
coe.los <- as.numeric(reg_los$coefficients)
cov.los <- vcov(reg_los)

#generate the demographic mapping
#alternative
# bmi group
dem1 = patient_set[,c("explorys_patient_id","gender","age_60","age_60_70","age_70_80","age_80","bmi_pre")]
idx = which(dem1$bmi_pre < 25)
dem1$bmi_25=0
dem1$bmi_25[idx] = 1
idx = which(dem1$bmi_pre >= 25 & dem1$bmi_pre < 30)
dem1$bmi_25_30 = 0
dem1$bmi_25_30[idx] = 1
idx = which(dem1$bmi_pre >= 30)
dem1$bmi_30 = 0
dem1$bmi_30[idx] = 1
idx = which(is.na(dem1$bmi_pre))
dem1[idx, c("bmi_25","bmi_25_30","bmi_30")] = NA

freq_sum = read.csv("H:/My Documents/workf/R_code/freq_summary.csv", header=TRUE)

#prepare lists to store results
wmp_list <- list(NULL)
length(wmp_list)=num_test

for(j in 1:num_test){
#for(j in 1:1){

#conduct imputation and modelling

#get patient data
patient_org=patient_set[j,]
test_patient_id = patient_org$explorys_patient_id
pid = toString(test_patient_id)

#used to store the results based on fixed probability
rmp <- data.frame(pid=character(0), 
pred_val=numeric(0), pred_upr=numeric(0), pred_lwr=numeric(0), pred_range=numeric(0),
true_value=character(0), diff=numeric(0), diff_pct=numeric(0))


#assign freq summary
f_sum = freq_sum

#assign patient[j,] demographic info
subset_input[,c(1:8)] <- dem1[j,c(2:6,8:10)]


#mark the rest of the features as missing ones
patient = patient_org
unknown_features = features
patient[unknown_features] <- NA
subset_input[unknown_features] <- NA
known_features <- list()

while(length(unknown_features)!=0){

	#get the next feature to ask
	#nf = get_next_feature_m12(f_sum, subset_input, unknown_features)
	#nf = get_next_feature_m34(f_sum, subset_input, unknown_features, known_features)
	#nf = get_next_feature_m56(f_sum, subset_input, unknown_features)
	#nf = get_next_feature_m78(f_sum, subset_input, unknown_features)
	#nf = get_next_feature_m90(f_sum, subset_input, unknown_features, known_features)
	nf = get_next_feature_m112(f_sum, subset_input, unknown_features)
	f_sum = nf$f_sum
	#print (dim(f_sum))
	next_feature = nf$next_feature
	
	#print (next_feature)

	#run the imputation process
	# Initialize the estimates from the conditional population mean
	cond_pop <- findCondPop(patient)
	init_est <- initEstimate(cond_pop, patient)
	f_hat <- init_est$f_hat
	f_range <- init_est$f_range

	# Infer missing features from clustering
	support_cluster <- findSupportClusters(cluster_centroid, f_hat, f_range)
	box <- findSupportBox(cluster_centroid, support_cluster, f_hat, f_range)
	group_cluster <- findGroupCluster(cluster_centroid, box)

	if (is.null(group_cluster)) {
	  feature_est = f_hat
	  feature_range = f_range
	  feature_est_bin = f_hat

	  for (name in names(f_range)) {
	  if (f_range[[name]] != 0) {
		  feature_est_bin[[name]] <- as.numeric(f_hat[[name]] >= pop_avg[[name]])
		}
	  }
	} else {
	  new_est <- updateEstimates(cluster_centroid, cluster_size, group_cluster, f_hat, f_range)
	  feature_est = new_est$f_hat
	  feature_range = new_est$f_range
	  feature_est_bin = new_est$f_hat_bin
	}

	# compute the predicted confidence interval
	feature_est = data.frame(feature_est)
	feature_range = data.frame(feature_range)

	#compute model
	tmp <- computePredCI5(patient_org, feature_est, coe.los, cov.los)

	#append the results
	rmp <- rbind(rmp, data.frame(tmp))
	
	#assign value to the next feature
	#tmp$next_question=next_feature
	patient[,next_feature]=patient_org[,next_feature]

	#update the unknown features
	known_features = colnames(patient[,features])[!is.na(patient[,features])]
	unknown_features = features[! features %in% known_features]
	#update the subset input
	#for method 1-4
	#subset_input[known_features]=patient[known_features] 
	#for method 5-7
	subset_input[,features] <- NA
	subset_input[,next_feature]=patient_org[,next_feature]

} #end of while

#use the original data to get the predicted confidence interval
td <- computePredCI5(patient_org, patient_org, coe.los, cov.los)
td$diff=0
td$diff_pct=0
td$true_value="***"
rmp$diff=rmp$pred_val-td$pred_val
if(td$pred_val!=0) {
	rmp$diff_pct=rmp$diff/td$pred_val*100
} else {
	rmp$range_pct=NA
}

#mark the step when reached to prediction fitted value
for(q in 1:9){
	if(rmp$diff[q]==0) {rmp$true_value[q]="*"} else {rmp$true_value[q]=""}
}

#combine the results
rmp <- rbind(rmp, data.frame(td))

#prepare the data to test
wmp_list[[j]] <- rmp[,c("pred_val","pred_range","diff","diff_pct","true_value")]
names(wmp_list)[j] = pid

#monitor the loop
if(j%%100==0) {print(j)}

} #end of j in number of test





#save the result
delist <- function(wlist){
  return (data.frame(x=rownames(wlist), y0=wlist$pred_val, y1=wlist$pred_range, y2=abs(wlist$diff), y3=abs(wlist$diff_pct)))
}

min_conv <- function(wlist){
	idx = which(wlist$true_value=="*")
	if(length(idx)>0) {
		step_numb=min(idx)
		} else {step_numb=NA}
	return (step_numb)
}

ewmp <- data.frame(x=numeric(0), y0=numeric(0), y1=numeric(0), y2=numeric(0), y3=numeric(0))
for(i in 1:num_test){
#for(i in 1:3){
	ewmp = rbind(ewmp, delist(wmp_list[[i]]))
}

tm12 <- aggregate(ewmp[,2:5], by=list(as.numeric(as.character(ewmp$x))), mean)
colnames(tm12)=c("step","pred_val","pred_range","abs_error","error_pct")

t_rsme <- aggregate(ewmp[,4], by=list(as.numeric(as.character(ewmp$x))), FUN= function(x) sqrt(sum(x^2)))

k1 <- aggregate(ewmp[,5], by=list(as.numeric(as.character(ewmp$x))), median)
k1 <- aggregate(ewmp[,4], by=list(as.numeric(as.character(ewmp$x))), FUN= function(x) sqrt(sum(x^2)/1000.))



colnames(t_static)=c("step","pred_val","pred_range","abs_error","error_pct")

#draw plot
par(mfrow=c(1,2))
matplot(tm1$step, cbind(tm1$error_pct, tm2$error_pct), type="l", col=c("orange","dark green"),
xlab="Questioning Step", ylab="Error ratio compared with fitted prediction",
axes=F, pch=19, font.main=7,
main="Error ratio between imputed prediction and \nfitted prediction across steps\nMethod 1 (orange) vs. Method 2(green)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)


matplot(tm1$step, cbind(tm1$error_pct, tm2$error_pct, tm7$error_pct, tm8$error_pct), type="l", col=c("orange","dark green","black","red"),
xlab="Questioning Step", ylab="Error ratio compared with fitted prediction",
axes=F, pch=19, font.main=7,
main="Error ratio between imputed prediction and \nfitted prediction across steps\nM1 (orange) vs M2(green) vs M7 (black) vs M8 (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(tm1$step, cbind(tm1$error_pct, tm2$error_pct, tm3$error_pct, tm4$error_pct), type="l", col=c("orange","dark green","blue","brown"),
xlab="Questioning Step", ylab="Error ratio compared with fitted prediction",
axes=F, pch=19, font.main=7,
main="Error ratio between imputed prediction and \nfitted prediction across steps\nM1 (orange) vs M2(green) vs M3 (blue) vs M4 (brown)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(tm3$step, cbind(tm3$error_pct, tm4$error_pct, tm5$error_pct, tm6$error_pct), type="l", col=c("blue","brown","chartreuse4","purple"),
xlab="Questioning Step", ylab="Error ratio compared with fitted prediction",
axes=F, pch=19, font.main=7,
main="Error ratio between imputed prediction and \nfitted prediction across steps\nM3 (blue) vs M4 (brown) vs M5 (chartreuse) vs M6(purple)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(tm7$step, cbind(tm7$error_pct, tm8$error_pct, tm5$error_pct, tm6$error_pct), type="l", col=c("black","red","chartreuse4","purple"),
xlab="Questioning Step", ylab="Error ratio compared with fitted prediction",
axes=F, pch=19, font.main=7,
main="Error ratio between imputed prediction and \nfitted prediction across steps\nM7 (black) vs M8 (red) vs M5 (chartreuse) vs M6(purple)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(tm7$step, cbind(tm7$error_pct, tm8$error_pct, tm9$error_pct, tm0$error_pct), type="l", col=c("black","red","chocolate4","cyan4"),
xlab="Questioning Step", ylab="Error ratio compared with fitted prediction",
axes=F, pch=19, font.main=7,
main="Error ratio between imputed prediction and \nfitted prediction across steps\nM7 (black) vs M8 (red) vs M9 (chocolate) vs M10 (cyan)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(t_static$step, cbind(t_static$error_pct, tm1$error_pct, tm2$error_pct, tm3$error_pct, tm4$error_pct, tm5$error_pct, tm6$error_pct, tm7$error_pct, tm8$error_pct, tm9$error_pct, tm0$error_pct), type="l", 
col=c("gold4","orange","dark green","blue","brown","chartreuse4","purple","black","red","chocolate4","cyan4"),
xlab="Questioning Step", ylab="Error ratio compared with fitted prediction",
axes=F, pch=19, font.main=7,
main="Error ratio between imputed prediction and \nfitted prediction across steps\nStatic(gold) vs M1(orange) vs M2(green) vs M3(blue) vs M4(brown)\nvs M5(chartreuse) vs M6(purple) vs M7 (black) vs M8 (red)\nvs M9 (chocolate) vs M10 (cyan)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)



matplot(t_rsme$Group.1, cbind(t_rsme$x, t_rsme2$x, t_rsme3$x), type="l", col=c("orange","dark green","black"),
xlab="Questioning Step", ylab="RSME to the final predicted P",
axes=F, pch=19, 
main="RSME to the predicted P across steps\nMethod 1 (orange) vs. Method 2(green) vs. Method 3 (black)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)


par(mfrow=c(1,2))
matplot(t_static$step, cbind(t_static$error_pct, tm11$error_pct, tm12$error_pct), type="l", col=c("black", "orange","dark green"),
xlab="Questioning Step", ylab="Error ratio compared with fitted prediction",
axes=F, pch=19, font.main=7,
main="Error ratio between imputed prediction and \nfitted prediction across steps\nStatic(black) vs Method 11(orange) vs. Method 12(green)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)