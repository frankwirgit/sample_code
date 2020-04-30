# Coding example in R for data extraction, data re-engineering, 
# missing value imputation, data assembly, feature extraction. 
# and to compare different methods with predicted confidence interval
# The patient data in this example has been de-identified

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

#alternative
# bmi group
idx = which(cohort$bmi_pre < 25)
cohort$bmi_25 = 0
cohort$bmi_25[idx] = 1
idx = which(cohort$bmi_pre >= 25 & cohort$bmi_pre < 30)
cohort$bmi_25_30 = 0
cohort$bmi_25_30[idx] = 1
idx = which(cohort$bmi_pre >= 30)
cohort$bmi_30 = 0
cohort$bmi_30[idx] = 1
idx = which(is.na(cohort$bmi_pre))
cohort[idx, c("bmi_25","bmi_25_30","bmi_30")] = NA


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

#alternative
data = cohort[,c("explorys_patient_id", "Length_of_Stay", "revision", "complication", "post_recovery",
"gender", "age", "age_60", "age_60_70", "age_70_80", "age_80",
"bmi_pre", "bmi_25", "bmi_25_30", "bmi_30",
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

> nrow(data)
[1] 62315
> data <- data[data$Length_of_Stay > 1,]
> nrow(data)
[1] 58925
> data <- data[complete.cases(data),]
> nrow(data)
[1] 47565

data = within(data, {LOS=ifelse(Length_of_Stay>5, 1, 0)})
data = within(data, {LOS4=ifelse(Length_of_Stay>4, 1, 0)})

#factorize the data
dat <- apply(data[,c(6, 17:34)], 2, factor)
summary(dat)
length(rownames(dat))
dat = cbind(data[,c(1:2, 35:36, 3:5, 7:16)], dat)
names(dat)

#calculate the AUC
getROC_AUC = function(probs, true_Y) {
	probsSort = sort(probs, decreasing=TRUE, index.return=TRUE)
	val = unlist(probsSort$x)
	idx = unlist(probsSort$ix)

	roc_y = true_Y[idx];
	stack_x = cumsum(roc_y == 0)/sum(roc_y == 0)
	stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)

	auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
	return(list(stack_x=stack_x, stack_y=stack_y, auc=auc))
}

reg_los <- glm(LOS ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat, family=binomial)
prob = predict(reg_los, type="response")
aList = getROC_AUC(prob, dat$LOS)
auc_los = unlist(aList$auc)
> auc_los
[1] 0.6236503

#compare with LOS longer than 4 days
reg_los4 <- glm(LOS4 ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat, family=binomial)
prob = predict(reg_los4, type="response")
aList = getROC_AUC(prob, dat$LOS4)
unlist(aList$auc)
[1] 0.680885

#compare with adding the race
reg_los_race <- glm(LOS ~ age + bmi_pre + gender 
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat, family=binomial)
prob = predict(reg_los_race, type="response")
aList = getROC_AUC(prob, dat$LOS)
unlist(aList$auc)
[1] 0.6310645

#compare with adding the insurance
reg_los_ins <- glm(LOS ~ age + bmi_pre + gender 
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat, family=binomial)
prob = predict(reg_los_ins, type="response")
aList = getROC_AUC(prob, dat$LOS)
unlist(aList$auc)
[1] 0.6367008

#compare with adding the insurance
reg_los_ri <- glm(LOS ~ age + bmi_pre + gender 
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat, family=binomial)
prob = predict(reg_los_ri, type="response")
aList = getROC_AUC(prob, dat$LOS)
unlist(aList$auc)
[1] 0.642125



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
		 
feature_inorder = c("antibiotics", "pre_comor_Anemia", "pre_comor_Group6_Nervous", 
		 "pre_comor_Group5_MentalDisorder", "pre_comor_Rheumatism", "pre_comor_Group3_Endocrine", 
		 "pre_comor_Group8_Respiratory", "ever_smoker", "pre_comor_Hypertension")
		 
num_features = length(features)

# centroid of 8 clusters
cluster_centroid = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_centers_8_binary_48K.csv", header=TRUE)
# cluster size
cluster_size = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_size_8_binary_48K.csv", header=TRUE)
# cluster assignment
cluster_assignment = read.csv("V:/result/segmentation_without_ortho_visit/new_preop_kmeans_13_cluster_binary_48K.csv", header=TRUE)

# Average of features from the entirepopulation
pop_avg = colSums(cohort_summary[, features]*cohort_summary[, 'num_patient'])/sum(cohort_summary[, 'num_patient'])

# alpha level for 99% confidence interval
alpha = 0.01

cr <<- qnorm(0.975)
coe.los <- as.numeric(reg_los$coefficients)
cov.los <- vcov(reg_los)

coe.rev <- as.numeric(reg_rev$coefficients)
cov.rev <- vcov(reg_rev)
coe.com <- as.numeric(reg_com$coefficients)
cov.com <- vcov(reg_com)
coe.porec <- as.numeric(reg_porec$coefficients)
cov.porec <- vcov(reg_porec)

#check the sign of each coefficients
c <- reg_los$coefficients[5:13]
names(c)=features
d <- names(which(c<0))

#quartile of the whole cohort
qr_los = quantile(reg_los$fitted, probs=seq(0,1,0.05))

qr_rev = quantile(reg_rev$fitted, probs=seq(0,1,0.05))
qr_com = quantile(reg_com$fitted, probs=seq(0,1,0.05))
qr_porec = quantile(reg_porec$fitted, probs=seq(0,1,0.05))

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

initEstimate_old <- function(pop_idx, patient) {
  # Initialize the estimate from the conditional population mean
  # Args:
  #   pop_idx: Row index of cohort_summary table
  #   patient: Patient data. NA is for unknown feature value.
  # Returns:
  #   Range and estimates of features for clustering.
  
  z = qnorm(1-0.5*alpha)

  num_pop = sum(cohort_summary[pop_idx, 'num_patient'])
  cond_est = colSums(cohort_summary[pop_idx, features]*cohort_summary[pop_idx, 'num_patient'])/num_pop
  bound = z*sqrt(cond_est*(1-cond_est)/num_pop)
 
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

computePredCI1 <- function(newd, coe, covm){
s <- coe %*% t(newd)
se <- apply(newd, 1, function(x) sqrt(t(as.numeric(x)) %*% covm %*% as.numeric(x)))
return(list(pred_val=logitT(s), pred_upr=logitT(s+cr*se), pred_lwr=logitT(s-cr*se)))
}

computePredCI2 <- function(patient_org, imputed, coe, covm){

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
if(td$pred_val!=0) {td$range_pct=td$pred_range/td$pred_val*100} else {td$range_pct=NA}
return (td)
}

computePredCI3 <- function(patient_org, imputed, imputed_upper, imputed_lower, coe, covm){

newd0 <- patient_org[,c("age", "bmi_pre", "gender")]
newd0$gender = newd0$gender - 1

to_compute <- imputed[,c(features)]
to_compute <- cbind(intercept=1, newd0, to_compute)
to_compute <- as.matrix(to_compute)

to_compute_upper <- imputed_upper[,c(features)]
to_compute_upper <- cbind(intercept=1, newd0, to_compute_upper)
to_compute_upper <- as.matrix(to_compute_upper)

to_compute_lower <- imputed_lower[,c(features)]
to_compute_lower <- cbind(intercept=1, newd0, to_compute_lower)
to_compute_lower <- as.matrix(to_compute_lower)


s <- coe %*% t(to_compute)
su <- coe %*% t(to_compute_upper)
sl <- coe %*% t(to_compute_lower)
td <- list(id=patient_org$explorys_patient_id)
td <- c(td, pred_val=logitT(s), pred_upr=logitT(su), pred_lwr=logitT(sl))
td$pred_range=td$pred_upr-td$pred_lwr
if(td$pred_val!=0) {td$range_pct=td$pred_range/td$pred_val*100} else {td$range_pct=NA}
return (td)
}

#ask demographic information
#age gender weight height (bmi)
demo_input = function(patient_input){
usr.age <- readline(prompt="Please enter your age? ")
usr.age = as.numeric(usr.age)
patient_input$age=usr.age
patient_input[,c("age_60", "age_60_70","age_70_80","age_80")] = 0

if(usr.age<60) {patient_input$age_60=1
} else if (usr.age>=60 & usr.age<70) {patient_input$age_60_70=1
} else if (usr.age>=70 & usr.age<80) {patient_input$age_70_80=1
} else {patient_input$age_80=1}

usr.weight <- readline(prompt="Please enter your weight(lb)? ")
usr.weight <- as.numeric(usr.weight)
usr.height <- readline(prompt="Please enter your height(in)? ")
usr.height <- as.numeric(usr.height)
usr.bmi = usr.weight*703/(usr.height)^2
patient_input$bmi_pre=usr.bmi

patient_input[,c("bmi_30", "bmi_30_35","bmi_35_40","bmi_40")] = 0
if(usr.bmi<30) {patient_input$bmi_30=1
} else if (usr.bmi>=30 & usr.bmi<35) {patient_input$bmi_30_35=1
} else if (usr.bmi>=35 & user.bmi<40) {patient_input$bmi_35_40=1
} else {patient_input$bmi_40=1}


usr.gender <- readline(prompt="Please enter your gender(M/F)? ")
patient_input["gender"] <- ifelse(toupper(usr.gender)=="M", 1, 2)

return (patient_input)
}


ask_question = function(pickup_idx, patient_input){

#print the missing features
usr.feature <- readline(noquote(paste("We'd like to ask you - do you have ", feature_inorder[pickup_idx], "? (Y/N) ", sep="")))
#update the users input
patient_input[,feature_inorder[pickup_idx]] = ifelse(toupper(usr.feature)=="Y", 1, 0)
return (patient_input)

}


#input user data from the terminal and output the imputation/modelling result

#initialize the user data structure
set.seed(0)
test_patient_id = sample(data$explorys_patient_id, 1)
pid = toString(test_patient_id)
patient_org = data[data$explorys_patient_id %in% test_patient_id,]
#add row ID
rownames(patient_org) = patient_org$explorys_patient_id

patient_input <- patient_org

#randomly assign the PID
idset = c(1:2000)
usr.id=sample(idset, 1)
idset[[usr.id]] <- NA

patient_input$explorys_patient_id = usr.id
rownames(patient_input)=usr.id
patient_input$Length_of_Stay=0
patient_input$revision=0
patient_input$complication=0
patient_input$post_recovery=0
patient_input$LOS=0

#initialize the question list
testp = function(){

patient_input = demo_input(patient_input)

#prepare the imputing list
patient_input[features] <- NA


feature_toask <- feature_inorder

for(i in 1:length(feature_inorder)){

#for(i in 1:3){

patient = patient_input
patient[feature_toask] <- NA

#print(noquote("In this step we will impute the feature(s) of "))
#print(feature_toask)
#imputation and modelling
# 4. Initialize the estimates from the conditional population mean
cond_pop <- findCondPop(patient)
init_est <- initEstimate(cond_pop, patient)
f_hat <- init_est$f_hat
f_range <- init_est$f_range

#fix the missing issue
f_hat[which(is.na(f_hat))]=0
f_range[which(is.na(f_range))]=0

# 5. Infer missing features from clustering
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

#6. compute the predicted confidence interval

feature_est = data.frame(feature_est)
#str(feature_est)
feature_range = data.frame(feature_range)

#6.1 using the imputed fixed probability
tmp <- computePredCI2(patient_input, feature_est, coe.los, cov.los)
usr <- round(as.numeric(tmp[2:6]),3)

#provide the user feedback
print(paste("Based on your input, your risk to have a hospital stay longer than 5 days is ", usr[1]*100, "%", sep=""))
print(paste("And your risk to have a hospital stay longer than 5 days is between ", usr[3]*100, "% and ", usr[2]*100, "%", sep=""))

qr_upr = names(which((qr_los>usr[1])==T)[1])
qr_lwr = names(qr_los[length(which((qr_los>usr[1])==F))])
print(paste("You are in the ", qr_lwr, " - ", qr_upr, " risk group of the studied population who had TKR", sep=""))

restp = 100-(which((qr_los>usr[1])==T)[1]-1)*5
riskp = ifelse(restp<25, "high", ifelse((restp<75 & restp>=25), "medium", "low"))
print(paste(restp, "% of the studied population who had TKR have a higher risk than you do for having a longer hospital stay", sep=""))
print(paste("You are at ", riskp, " risk to have a hospital stay longer than 5 days", sep=""))

#randomly pick up the next future to ask
#pickup_idx = sample(1:length(feature_toask), 1, replace=F)
#ask question in order
pickup_idx = i
tmp$next_question=feature_inorder[pickup_idx]

patient_input = ask_question(pickup_idx, patient_input)

#update question pool
feature_toask <- feature_toask[-1]


}

#the last step is to put the complete user input into the model
tmp <- computePredCI2(patient_input, patient_input, coe.los, cov.los)
usr <- round(as.numeric(tmp[2:6]),3)

#provide the user feedback
print(paste("Based on your input, your risk to have a hospital stay longer than 5 days is ", usr[1]*100, "%", sep=""))
print(paste("And your risk to have a hospital stay longer than 5 days is between ", usr[3]*100, "% and ", usr[2]*100, "%", sep=""))

qr_upr = names(which((qr_los>usr[1])==T)[1])
qr_lwr = names(qr_los[length(which((qr_los>usr[1])==F))])
print(paste("You are in the ", qr_lwr, " - ", qr_upr, " risk group of the studied population who had TKR", sep=""))

restp = 100-(which((qr_los>usr[1])==T)[1]-1)*5
riskp = ifelse(restp<25, "high", ifelse((restp<75 & restp>=25), "medium", "low"))
print(paste(restp, "% of the studied population who had TKR have a higher risk than you do for having a longer hospital stay", sep=""))
print(paste("You are at ", riskp, " risk to have a hospital stay longer than 5 days", sep=""))
print("end of the test")

return (patient_input)
}









#new semantics 
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

#complication

#post recovery


> nrow(data)
[1] 47565
> nrow(subset(data, data$Length_of_Stay>5))
[1] 3352
> 3352/47565*100
[1] 7.047199


#initialize the question list
testp3 = function(){

patient_input = demo_input(patient_input)

#prepare the imputing list
patient_input[features] <- NA


feature_toask <- feature_inorder

for(i in 1:length(feature_inorder)){

#for(i in 1:3){

patient = patient_input
patient[feature_toask] <- NA

#print(noquote("In this step we will impute the feature(s) of "))
#print(feature_toask)
#imputation and modelling
# 4. Initialize the estimates from the conditional population mean
cond_pop <- findCondPop(patient)
init_est <- initEstimate(cond_pop, patient)
f_hat <- init_est$f_hat
f_range <- init_est$f_range

#fix the missing issue
f_hat[which(is.na(f_hat))]=0
f_range[which(is.na(f_range))]=0

# 5. Infer missing features from clustering
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

#6. compute the predicted confidence interval

feature_est = data.frame(feature_est)
#str(feature_est)
feature_range = data.frame(feature_range)

#6.1 using the imputed fixed probability
#tmp <- computePredCI2(patient_input, feature_est, coe.los, cov.los)
#print_feedback_los(tmp)
tmp_rev <- computePredCI2(patient_input, feature_est, coe.rev, cov.rev)
print_feedback_rev(tmp_rev)



#randomly pick up the next future to ask
#pickup_idx = sample(1:length(feature_toask), 1, replace=F)
#ask question in order
pickup_idx = i
tmp$next_question=feature_inorder[pickup_idx]

patient_input = ask_question(pickup_idx, patient_input)

#update question pool
feature_toask <- feature_toask[-1]


}

#the last step is to put the complete user input into the model
#tmp <- computePredCI2(patient_input, patient_input, coe.los, cov.los)
#print_feedback_los(tmp)
tmp_rev <- computePredCI2(patient_input, patient_input, coe.rev, cov.rev)
print_feedback_rev(tmp_rev)
print("end of the test")

return (patient_input)
}



print_feedback_los = function(tmp){

user_feed$pred_value=tmp$pred_val
user_feed$pred_upper=tmp$pred_upr
user_feed$pred_lower=tmp$pred_lwr
usr <- round(as.numeric(tmp[2:6]),3)

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
	  
#qr_upr = names(which((qr_los>usr[1])==T)[1])
#qr_lwr = names(qr_los[length(which((qr_los>usr[1])==F))])
#print(paste("You are in the ", qr_lwr, " - ", qr_upr, " risk group of the studied population who had TKR", sep=""))

#print(paste(restp, "% of the studied population who had TKR have a higher risk than you do for having a longer hospital stay", sep=""))
#print(paste("You are at ", riskp, " risk to have a hospital stay longer than 5 days", sep=""))
	  
}

print_feedback_rev = function(tmp_rev){

user_feed$pred_value_rev=tmp_rev$pred_val
user_feed$pred_upper_rev=tmp_rev$pred_upr
user_feed$pred_lower_rev=tmp_rev$pred_lwr
usr <- round(as.numeric(tmp_rev[2:6]),3)

restp = 100-(which((qr_rev>usr[1])==T)[1]-1)*5
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
user_feed$pred_likert_rev = riskw

cat(paste(user_feed$context_statement_rev_v1, "\n", user_feed$context_statement_rev_v2, "\n",
      user_feed$patient_condition, 
	  #"which is ", appendw, " the average condition of the studied population, it is ",
	  "it is ",
	  user_feed$pred_likert_rev, 
	  user_feed$quali_outcome_statement_rev, "\n",
	  user_feed$numberic_outcome_statement_rev, usr[1]*100, "%.\n", 
	  user_feed$ci_outcome_statement_rev, usr[3]*100, "% to ", usr[2]*100, "%.\n", sep=""))	
	  
}


reg_rev <- glm(revision ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat, family=binomial)
prob = predict(reg_rev, type="response")
aList = getROC_AUC(prob, dat$revision)
auc_rev = unlist(aList$auc)

> auc_rev
[1] 0.6178434

#average of revision



reg_com <- glm(complication ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat, family=binomial)
prob = predict(reg_com, type="response")
aList = getROC_AUC(prob, dat$complication)
unlist(aList$auc)
[1] 0.6258472



reg_porec <- glm(post_recovery ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat, family=binomial)
prob = predict(reg_porec, type="response")
aList = getROC_AUC(prob, dat$post_recovery)
unlist(aList$auc)

[1] 0.6883093

#find missing
sum(is.na(cohort$age))
sapply(data, function(x) sum(is.na(x)))

#test the correlation matrix
#subset the data into gender, age and bim groups
#when implement, the data should be replaced by summary matrix
demog <- data.frame(gender=c(1,2))
age_60 = c(1,0,0,0)
age_60_70 = c(0,1,0,0)
age_70_80 = c(0,0,1,0)
age_80 = c(0,0,0,1)
demog1 <- data.frame(age_60, age_60_70, age_70_80, age_80)
bmi_30 = c(1,0,0,0)
bmi_30_35 = c(0,1,0,0)
bmi_35_40 = c(0,0,1,0)
bmi_40 = c(0,0,0,1)
demog2 <- data.frame(bmi_30, bmi_30_35, bmi_35_40, bmi_40)
demog3 <- merge(x=demog1, y=demog2, by=NULL)
demog <- merge(x=demog, y=demog3, by=NULL)
rm(demog1,demog2,demog3)
rm(bmi_30,bmi_30_35,bmi_35_40,bmi_40)
rm(age_60, age_60_70, age_70_80, age_80)


demo_f = c("gender","age_60","age_60_70","age_70_80","age_80","bmi_30","bmi_30_35","bmi_35_40","bmi_40")

#alternative of demographic groups
demog <- data.frame(gender=c(1,2))
age_60 = c(1,0,0,0)
age_60_70 = c(0,1,0,0)
age_70_80 = c(0,0,1,0)
age_80 = c(0,0,0,1)
demog1 <- data.frame(age_60, age_60_70, age_70_80, age_80)
bmi_25 = c(1,0,0)
bmi_25_30 = c(0,1,0)
bmi_30 = c(0,0,1)
demog2 <- data.frame(bmi_25, bmi_25_30, bmi_30)
demog3 <- merge(x=demog1, y=demog2, by=NULL)
demog <- merge(x=demog, y=demog3, by=NULL)
rm(demog1,demog2,demog3)
rm(bmi_25, bmi_25_30, bmi_30)
rm(age_60, age_60_70, age_70_80, age_80)


demo_f = c("gender","age_60","age_60_70","age_70_80","age_80","bmi_25", "bmi_25_30", "bmi_30")


demog2 = demog
qlist <- list(NULL)
for(j in 1:nrow(demog)){
#for(j in 1:3) {
	#form the subset condition
	print(paste("j=", j))
	str="subset(data, "
	df = demog[j,]
	combine_filter <- function(df, str){
		for (i in 2:length(df)) {
			if (df[,i]==1) {
				str = paste(str, "data$", colnames(df)[i], "==", df[,i], " & ", sep="")
			}
		}
		str=paste(str, "data$gender==", df[,1], ")", sep="")
		return (str)
	}
	
	cond_str = combine_filter(df, str)
	print(cond_str)
	sub <- eval(parse(text=cond_str))
	print(nrow(sub))

	#calculate the phi correlation through the feature pairs

	#clist <- vector("list", length=36)
	#names(clist) <- c("key","freq","corv")
	#dim(combn(feature_inorder, 2))[2]
	count = 1
	clist = list(NULL)
	feature_start = feature_inorder
	for(f in feature_start){
		feature_left = feature_start[!is.element(feature_start, f)]
		#print(paste("f=", f))
		for(u in feature_left){
			clist$key[[count]] <- c(f, u)
			tq = table(sub[,f], sub[,u])
			if(dim(tq)[1]<2 | dim(tq)[2]<2) {
				tq = matrix(c(tq,0,0), ncol=2, nrow=2)
			}
			clist$freq[[count]] <- tq
			clist$corv[[count]] <- abs(phi(tq,7))
			#print(count)
			count = count+1
		}
		feature_start=feature_left
	}

	corv_max =0
	f_first <- NULL
	for (f in rev(feature_inorder)){
		g <- grep(f, clist$key)
		if (length(g)!=0) {
			corv_cur = sum(clist$corv[c(g)], na.rm=T)
		} else{
			corv_cur = 0
		}
		if (corv_cur > corv_max) {
			corv_max = corv_cur
			f_first = f
			#print(paste("f_first=", f, " and sum corv =", corv_max, sep=""))
		}
	}
	g <- grep(f_first, clist$key)
	g1 <- g[order(clist$corv[c(g)], decreasing=T)]
	kd <- clist$key[c(g1)]
	qlist[[j]] <- c(f_first, sapply(kd, function(x) ifelse(x[1]==f_first, x[2],x[1])))
	demog2[j,'feature_rank']=paste(qlist[[j]], collapse=" ")
	print(qlist[[j]])
} #end of j

write.csv(demog2, 'feature_inorder.csv', row.names=FALSE)
write.csv(demog2, 'feature_inorder_3bmi.csv', row.names=FALSE)

#analysis
q <- data.frame(lapply(qlist, as.character), stringsAsFactors=F)
colnames(q)=c(1:32)
s <- as.list(q[1,], drop=T)
as.vector(s, mode='character')

s1 <- as.vector(q[2,], mode='character')
split(seq_along(s1), s1)

s3 <- data.frame(feature_rank=q)

#based on the z value of the regression summary

library(caret)

question_ranked <- data.frame(outcome=character(0), feature_rank=character(0))

rank_predictor <- function(outcome_nm, reg){

t <- varImp(reg, scale=F)
t <- t[-c(1:3),,drop=F]
t <- t[order(-t$Overall), , drop=F]
qranked=paste(gsub("1","",rownames(t)), collapse=" ")
return (rbind(question_ranked, data.frame(outcome=outcome_nm, feature_rank=qranked)))
}

question_ranked = rank_predictor("LOS", reg_los)
question_ranked = rank_predictor("revision", reg_rev)
question_ranked = rank_predictor("complication", reg_com)
question_ranked = rank_predictor("post_recovery", reg_porec)


write.csv(question_ranked, 'feature_inorder_z.csv', row.names=FALSE)

#based on the correlation between outcome and feature list

outcomes = c("LOS","revision","complication","post_recovery")

count=1
olist = list(NULL)
odf = data.frame()
i=1
for(o in outcomes){
	print(paste("o=", o))
	odf[i,"outcome"]=o
	for(u in feature_inorder){
		olist$key[[count]] <- c(o,u)
		tq = table(data[,o], data[,u])
		if(dim(tq)[1]<2 | dim(tq)[2]<2) {
			tq = matrix(c(tq,0,0), ncol=2, nrow=2)
		}
		olist$freq[[count]] <- tq
		corv_cur = abs(phi(tq,7))
		olist$corv[[count]] <- corv_cur
		odf[i,u]= corv_cur
		print(count)
		count = count+1
		
	}
	x = odf[i,2:10]
	odf[i, "feature_rank"] <- paste(colnames(x[1,order(-x[1,])]), collapse=" ")
	i = i+1
}

write.csv(odf, 'feature_inorder_outcome.csv', row.names=FALSE)

#commented out
.f = function(){
for (i in 1:4){
x = odf[i,2:10]
paste(colnames(x[1,order(-x[1,])]), collapse=" ")
}
}

#use apply function
f = as.matrix(feature_inorder)
apply(f, 1, 
	function(t,x){
		#paste("f=",x)
		#apply(f, 1, function(x, t) table(data[,x], data[,t]))
		abs(phi(table(data[,x], data[,t]),7))
	}
	,t="antibiotics"
)

emb_apply <- function(y,feature){

apply(feature, 1, 
	function(x, t){
		abs(phi(table(data[,x], data[,t]),7))
	}
	,t=y
)
}

emb_apply <- function(y,feature){

apply(feature, 1, 
	function(x){
		abs(phi(table(data[,x], data[,y]),7))
	})
}

#doesn't work
#apply(f,1,emb_apply(y,feature), feature=f)
#apply(f,1,emb_pply(y,f))

#doesn't work
for(y in feature_inorder){
	#emb_apply(y, f)
	)
}

#an alternative way to compute
f = as.matrix(feature_inorder)
s <- data.frame(matrix(ncol=0, nrow=9))
for(y in feature_inorder){
	e <- data.frame(apply(f, 1, function(x) abs(phi(table(data[,x], data[,y]),7))))
	s <- cbind(s,e)
}
colnames(s)=feature_inorder
s1 <- as.vector(apply(s,2,sum))
s2 <- s[,which(s1==max(s1)), drop=F]
paste(feature_inorder[order(-s2)], collapse=" ")

#use the cohort summary to get the freq table
> table(data$antibiotics, data$ever_smoker)
   
        0     1
  0 24435  5809
  1 14146  3175
> sum(cohort_summary[pop_idx,'num_patient'])
[1] 3627
> pop_idx = which(cohort_summary[["antibiotics"]]==1 & cohort_summary[["ever_smoker"]]==0)
> sum(cohort_summary[pop_idx,'num_patient'])
[1] 17271
> sum(cohort_summary[,'num_patient'])
[1] 62315
> table(cohort$antibiotics, cohort$ever_smoker)
   
        0     1
  0 34144  7273
  1 17271  3627
> 

b = data.frame(matrix(ncol=2, nrow=0))
colnames(b)=c("demoid","qorder")
for(i in 1:24)  {

a <- demog2[i,]$feature_rank
a <- strsplit(a," ")
a <- as.character(a[[1]])
b <- rbind(b, data.frame(demoid=i, qorder=paste(match(a, feature_inorder), collapse="")))

}

> which(duplicated(b$qorder)==T)
[1]  6 11 22

> c <- substr(b$qorder, 1, 5)
> which(duplicated(c)==T)
 [1]  6 11 14 15 16 17 19 21 22 23 24
> c <- substr(b$qorder, 1, 4)
> which(duplicated(c)==T)
 [1]  6 10 11 13 14 15 16 17 19 20 21 22 23 24
> c <- substr(b$qorder, 1, 3)
> which(duplicated(c)==T)
 [1]  5  6  7 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24

b2 = data.frame(matrix(ncol=2, nrow=0))
colnames(b2)=c("outcome","qorder")
for(i in 1:4)  {

a <- odf[i,]$feature_rank
a <- strsplit(a," ")
a <- as.character(a[[1]])
b2 <- rbind(b2, data.frame(outcome=odf[i,]$outcome, qorder=paste(match(a, feature_inorder), collapse="")))

}

b3 = data.frame(matrix(ncol=2, nrow=0))
colnames(b3)=c("outcome","qorder")
for(i in 1:4)  {

a <- question_ranked[i,]$feature_rank
a <- as.character(a[[1]])
a <- strsplit(a," ")
a <- as.character(a[[1]])
b3 <- rbind(b3, data.frame(outcome=odf[i,]$outcome, qorder=paste(match(a, feature_inorder), collapse="")))

}



#test RMSE

#sample the patients
num_test=1000
set.seed(0)
test_id_set = sample(intersect(data$explorys_patient_id, cluster_assignment$X), num_test)
patient_set = data[data$explorys_patient_id %in% test_id_set,]
rownames(patient_set) = patient_set$explorys_patient_id

cr <<- qnorm(0.975)
coe.los <- as.numeric(reg_los$coefficients)
cov.los <- vcov(reg_los)

#generate the demogrphic mapping
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




#prepare lists to store results
wmp_list <- list(NULL)
length(wmp_list)=num_test

for(j in 1:num_test){
#for(j in 1:3){

#conduct imputation and modeling

#get patient data
patient_org=patient_set[j,]
test_patient_id = patient_org$explorys_patient_id
pid = toString(test_patient_id)

#used to store the results based on fixed probability
rmp <- data.frame(pid=character(0), 
pred_val=numeric(0), pred_upr=numeric(0), pred_lwr=numeric(0), pred_range=numeric(0),
true_value=character(0), diff=numeric(0), diff_pct=numeric(0))

#initialize the question list
#method 1 - for each patient, get the feature list which fits the demo info
a <- dem1[j,c(2:6,8:10)]
list_id = which(apply(demog, 1, function(x) all(x==a)))
flist <- demog2[list_id,]$feature_rank

#method2 - use the feature list which fits the outcome
#flist <- odf[1,]$feature_rank

#method3 - use the feature list which fits the model
#flist <- question_ranked[1,]$feature_rank
#flist <- as.character(flist[[1]])

flist <- strsplit(flist," ")
flist <- as.character(flist[[1]])
feature_toask <- flist

for(i in 1:length(feature_toask)){

#mark the rest of the features as missing ones
patient = patient_org
patient[feature_toask] <- NA

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

#update the feature list to ask next 
feature_toask <- feature_toask[-1]

} #end of i in feature to ask

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

t3p <- aggregate(ewmp[,2:5], by=list(as.numeric(as.character(ewmp$x))), mean)
t_rsme <- aggregate(ewmp[,4], by=list(as.numeric(as.character(ewmp$x))), FUN= function(x) sqrt(sum(x^2)))

k1 <- aggregate(ewmp[,5], by=list(as.numeric(as.character(ewmp$x))), median)
k1 <- aggregate(ewmp[,4], by=list(as.numeric(as.character(ewmp$x))), FUN= function(x) sqrt(sum(x^2)/1000.))

colnames(t3p)=c("step","pred_val","pred_range","abs_error","error_pct")



#draw plot
par(mfrow=c(1,2))
matplot(t$step, cbind(t$error_pct, t2$error_pct, t3$error_pct), type="l", col=c("orange","dark green","black"),
xlab="Questioning Step", ylab="Percentage of error to the final predicted P",
axes=F, pch=19, 
main="Differences of predicted P between using imputed value and \nactual value across steps\nMethod 1 (orange) vs. Method 2(green) vs. Method 3 (black)")
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


#get the summary freq table
library(RJDBC)
smDriver = JDBC(driverClass = "com.vertica.jdbc.Driver", classPath = "C:\\Program Files\\Vertica Systems\\JDBC\\vertica-jdbc-7.1.2-0.jar")
sm = dbConnect(smDriver, "jdbc:vertica://vertica-als-1:5433/db1", "lingtao.cao", "Hau37Eq6")

freq_summary = dbGetQuery(sm, "select *from sandbox_jj.cohort_nona_summary_frank")

#get the 2x2 freq matrix
set.seed(7)
test_patient_id = sample(data$explorys_patient_id, 1)
pid = toString(test_patient_id)
#print the patient ID
paste("test patient ID =", pid)
patient_org = data[data$explorys_patient_id %in% test_patient_id,]
patient = patient_org

required_features = c('age_60', 'age_60_70', 'age_70_80', 'age_80', 
			    'gender', 'bmi_25', 'bmi_25_30', 'bmi_30')
		 
feature_inorder = c("antibiotics", "pre_comor_Anemia", "pre_comor_Group6_Nervous", 
		 "pre_comor_Group5_MentalDisorder", "pre_comor_Rheumatism", "pre_comor_Group3_Endocrine", 
		 "pre_comor_Group8_Respiratory", "ever_smoker", "pre_comor_Hypertension")
		 
known_features = c("antibiotics","ever_smoker")

#b <- apply(patient[,required_features],1,function(x) ifelse(x==0,NA,x))

freq_summary[is.na(freq_summary)] <- 0

ft = c(required_features, known_features)
a <- patient[,ft]
#list_id = which(apply(demog2[,required_features], 1, function(x) all(x==b)))
list_id <- which(apply(freq_summary[,ft], 1, function(x) all(x==b)))
tfreq <- freq_summary[list_id,]

> table(tfreq$pre_comor_Group3_Endocrine, tfreq$pre_comor_Anemia)
   
     0  1
  0 21 11
  1 29 21

write.csv(freq_summary, 'freq_summary.csv', row.names=FALSE)
