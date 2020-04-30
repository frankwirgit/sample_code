# A coding example for doing a step by step QA check
# in order to ensure the data quality 
# the displayed patient data have been de-identified in this sample

library(RJDBC)
smDriver = JDBC(driverClass = "com.vertica.jdbc.Driver", classPath = "C:\\Program Files\\Vertica Systems\\JDBC\\vertica-jdbc-7.2.2-0.jar")
sm = dbConnect(smDriver, "jdbc:vertica://vertica-als-lb.explorys.net:5433/db1", "lingtao.cao", "xxxxxxxxxx")
hip_rep_proc = dbGetQuery(sm, "select * from SANDBOX_JJ.hip_rep_proc_los")



> length(unique(hip_rep_proc$explorys_patient_id))
[1] 4836
> names(hip_rep_proc)
[1] "explorys_patient_id"       "encounter_join_id"         "proc_date"                 "Length_of_Stay"           
[5] "admission_date"            "discharge_date"            "std_discharge_disposition" "concept_name"             
> nrow(hip_rep_proc)
[1] 4836
> which(hip_rep_proc$Length_of_Stay<2)
 [1]  116  739  836  936 1073 1360 1831 2068 2322 2737 3238 3383 3519 3721 3762 3826 3957 4012 4361
> length(which(hip_rep_proc$Length_of_Stay<2))
[1] 19
> 4836-19
[1] 4817
> length(which(hip_rep_proc$discharge_date < hip_rep_proc$proc_date))
[1] 0
> length(which(hip_rep_proc$admission_date > hip_rep_proc$proc_date))
[1] 4570
> length(which(hip_rep_proc$admission_date <= hip_rep_proc$proc_date))
[1] 266


> length(which(hip_rep_proc$admission_date - hip_rep_proc$proc_date > 2))
Error in hip_rep_proc$admission_date - hip_rep_proc$proc_date : 
  non-numeric argument to binary operator
> length(which((hip_rep_proc$admission_date - hip_rep_proc$proc_date) > 2))
Error in hip_rep_proc$admission_date - hip_rep_proc$proc_date : 
  non-numeric argument to binary operator
> class(hip_rep_proc$admission_date)
[1] "character"
> as.Date(hip_rep_proc$admission_date[1:10], format="%Y-%m-%d")
 [1] "2013-12-05" "2013-04-29" "2011-10-24" "2012-02-28" "2010-12-14" "2011-08-16" "2011-06-13" "2012-12-31" "2013-02-04" "2011-08-01"
> length(which((as.Date(hip_rep_proc$admission_date,format="%Y-%m-%d") - as.Date(hip_rep_proc$proc_date,format="%Y-%m-%d")) > 2))
[1] 0
> length(which((as.Date(hip_rep_proc$admission_date,format="%Y-%m-%d") - as.Date(hip_rep_proc$proc_date,format="%Y-%m-%d")) > 1))
[1] 0
> length(which((as.Date(hip_rep_proc$admission_date,format="%Y-%m-%d") - as.Date(hip_rep_proc$proc_date,format="%Y-%m-%d")) ==1))
[1] 1312
> length(which((as.Date(hip_rep_proc$admission_date,format="%Y-%m-%d") - as.Date(hip_rep_proc$proc_date,format="%Y-%m-%d")) <=1))
[1] 4836

cohort = dbGetQuery(sm, "select * from SANDBOX_JJ.hip_rep_demo")

######################### bmi preop #############################
bmi_pre = dbGetQuery(sm, "
select distinct a.explorys_patient_id, a.std_value, timestampdiff(day, a.observation_date, b.proc_date) as pre_days 
from SUPERMART_111_S.v_observation a, SANDBOX_JJ.hip_rep_proc_los b,
(
	select b.explorys_patient_id, max(b.observation_date) as observation_date 
	from SANDBOX_JJ.hip_rep_proc_los c
	join (select * 
			from SUPERMART_111.v_observation 
			where loinc_test_id='39156-5' and std_value is not null
		) b
	on c.explorys_patient_id = b.explorys_patient_id
	where datediff(day, b.observation_date, c.proc_date) > 0 and datediff(day, b.observation_date, c.proc_date) <= 360
	group by b.explorys_patient_id
) e
where a.explorys_patient_id = e.explorys_patient_id
and a.explorys_patient_id = b.explorys_patient_id
and a.observation_date = e.observation_date
and a.loinc_test_id='39156-5' 
and a.std_value is not null
order by a.explorys_patient_id
")


tmp1 = unlist(tapply(as.numeric(bmi_pre$std_value), bmi_pre$explorys_patient_id, median, na.rm=TRUE))
tmp1 = tmp1[order(as.numeric(names(tmp1)))]
tmp2 = unlist(tapply(as.numeric(bmi_pre$pre_days), bmi_pre$explorys_patient_id, median, na.rm=TRUE))
tmp2 = tmp2[order(as.numeric(names(tmp2)))]
tmp = data.frame(explorys_patient_id = unique(as.numeric(bmi_pre$explorys_patient_id)), bmi_pre = NA, bmi_pre_days = NA)
tmp = tmp[order(as.numeric(tmp$explorys_patient_id)),]
tmp$bmi_pre = tmp1
tmp$bmi_pre_days = unlist(tmp2)

> nrow(tmp)
[1] 3862

mydata.preop = merge(mydata.preop, tmp,  by.x = 'explorys_patient_id', by.y = 'explorys_patient_id', all=TRUE)



cohort = cohort[complete.cases(cohort),]
> nrow(cohort)
[1] 4424


#cohort$bmi_pre = as.numeric(cohort$weight)*703/as.numeric(cohort$height)^2
cohort$bmi_pre = as.numeric(cohort$weight)/as.numeric(cohort$height)^2*10000

cohort = cohort[cohort$bmi_pre>=10, ]

> nrow(cohort)
[1] 4419


> table(cohort$gender)

   0    1    2 
   1 1781 2642 

cohort = cohort[cohort$gender>0, ]

> nrow(cohort)
[1] 4418
   
> table(cohort$age)

> table(cohort$ever_smoker)


   0    1 
3247 1171 

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


colSums(cohort[,c(8:11)])/nrow(cohort)
   age_60 age_60_70 age_70_80    age_80 
0.2240833 0.3322770 0.2987777 0.1448619 

>  colSums(cohort[,c(12:15)])/nrow(cohort)
   bmi_30 bmi_30_35 bmi_35_40    bmi_40 
0.4678588 0.2507922 0.1435038 0.1378452 

> length(which(hip_rep_proc$Length_of_Stay>25))
[1] 3

cohort_hip = merge(x=hip_rep_proc, y=cohort, by="explorys_patient_id", all.x=T)

> sum(is.na(cohort_hip$gender))
[1] 418
> 4836-418
[1] 4418

> sum(cohort_hip$bmi_pre>0, na.rm=T)
[1] 4418




#####################################

test = dbGetQuery(sm, "select disorder_code*1 from supermart_111_s.v_diagnosis where disorder_code not like '%V' and disorder_code not like '%E'  order by 1 limit 10")
dbGetQuery(sm, "select distinct disorder_code*1 from supermart_111_s.v_diagnosis where REGEXP_LIKE(disorder_code, '^[0-9]+$') and (disorder_code*1>1 and disorder_code*1<=139) order by 1 limit 146")
grp1 = dbGetQuery(sm, "select disorder_code*1 from supermart_111_s.v_diagnosis where REGEXP_LIKE(disorder_code, '^\\d+(\\.\\d+)?$') and disorder_code*1>139 order by 1 limit 100")

/* cormobidity extraction */
#group 1

grp1 = dbGetQuery(sm, "select distinct a.explorys_patient_id, disorder_code from supermart_111_s.v_diagnosis a, sandbox_jj.hip_rep_proc_los b
where a.explorys_patient_id = b.explorys_patient_id
and a.source_system_type = 'CLINICAL' 
and REGEXP_LIKE(disorder_code, '^\\d+(\\.\\d+)?$') and disorder_code*1>=1 and disorder_code*1<=139
and timestampdiff(day, a.diagnosis_date, b.proc_date) <= 730
order by a.explorys_patient_id, disorder_code")


> length(unique(grp1$disorder_code))
[1] 136
> b = unique(grp1$disorder_code)
> b[order(b)]

length(unique(grp1$explorys_patient_id))
[1] 1046


grp2 = dbGetQuery(sm, "select distinct a.explorys_patient_id, disorder_code from supermart_111_s.v_diagnosis a, sandbox_jj.hip_rep_proc_los b
where a.explorys_patient_id = b.explorys_patient_id
and a.source_system_type = 'CLINICAL' 
and REGEXP_LIKE(disorder_code, '^\\d+(\\.\\d+)?$') and disorder_code*1>=140 and disorder_code*1<=239
and timestampdiff(day, a.diagnosis_date, b.proc_date) <= 730
order by a.explorys_patient_id, disorder_code")


extract_cormogrp <- function(start_icd, end_icd){
	select_query = paste("select distinct a.explorys_patient_id, disorder_code from supermart_111.v_diagnosis a, sandbox_jj.hip_rep_proc_los b where a.explorys_patient_id = b.explorys_patient_id and a.source_system_type = 'CLINICAL' and REGEXP_LIKE(disorder_code, '^\\d+(\\.\\d+)?$') and disorder_code*1>=",
	start_icd, " and disorder_code*1<=", end_icd, " and timestampdiff(day, a.diagnosis_date, b.proc_date) <= 730 order by a.explorys_patient_id, disorder_code", sep="");
	grp = dbGetQuery(sm, select_query)
	return (grp)
}

grp1 <- extract_cormogrp(1, 139)
length(unique(grp1$disorder_code))
140
length(unique(grp1$explorys_patient_id))
[1] 1077

grp2 <- extract_cormogrp(140, 239)
length(unique(grp2$disorder_code))
length(unique(grp2$explorys_patient_id))

-- supermart_111_s (305, 1111), supermart_111 (307, 1138)

grp3 <- extract_cormogrp(240, 279)
Anemia <- extract_cormogrp(280, 289)
grp5 <- extract_cormogrp(290, 319)
grp6 <- extract_cormogrp(320, 389)
HBP  <- extract_cormogrp(401, 405)
grp7 <- extract_cormogrp(390, 459)
grp8 <- extract_cormogrp(460, 519)
grp9 <- extract_cormogrp(520, 579)
grp10 <- extract_cormogrp(580, 629)
Rheumatism <- extract_cormogrp(725, 729)
grp13 <- extract_cormogrp(710, 739)

# pre-op antibiotics
antibiotics = dbGetQuery(sm, "select distinct(explorys_patient_id) from
(
select a.* from supermart_111.v_drug a,  sandbox_jj.hip_rep_proc_los b
where a.explorys_patient_id = b.explorys_patient_id 
and (
lower(a.ingredient_descriptions) like '%cefazolin%'
or lower(a.ingredient_descriptions) like '%cefuroxime%'
or lower(a.ingredient_descriptions) like '%vancomycin%'
)
and timestampdiff(day, a.prescription_date, b.proc_date) >= 0  
and timestampdiff(day, a.prescription_date, b.proc_date) <= 7 
and lower(a.std_order_status) = 'complete'
and a.source_system_type = 'CLINICAL' ) c
")
length(unique(antibiotics$explorys_patient_id))
[1] 2005  -- vs. supermart_111_s [1] 1901


# alcohol within 90 days before surgery
alcohol_pre_90 = dbGetQuery(sm, "select distinct(explorys_patient_id) from
	(
	select a.* from supermart_111.v_habit a,  sandbox_jj.hip_rep_proc_los b
	where a.explorys_patient_id = b.explorys_patient_id 
	and timestampdiff(day, a.contact_date, b.proc_date) > 0
	and timestampdiff(day, a.contact_date, b.proc_date) <= 90
	and a.mapped_question_answer like 'alcohol_y'
	order by a.explorys_patient_id, a.contact_date
	) c
")
length(unique(alcohol_pre_90$explorys_patient_id))
[1] 501

#use cohort (demographics) as the base data to merge
#merge with the proc_los


tmp = merge(x=cohort, y=hip_rep_proc[, c(1, 3:6, 8)], by="explorys_patient_id", all.x=T)
length(unique(tmp$explorys_patient_id))
summary(!is.na(tmp$explorys_patient_id))


#a = alcohol_pre_90[!duplicated(alcohol_pre_90$explorys_patient_id), "explorys_patient_id", drop=F]
#a = alcohol_pre_90[!duplicated(alcohol_pre_90[, "explorys_patient_id"]), ,drop=F]
 

#merge alcohol_pre_90, antibiotics & cormobidity
merge_feature <- function(base_df, df_merge, df_column){
	df1 = df_merge[!duplicated(df_merge$explorys_patient_id), "explorys_patient_id", drop=F]
	df1$v2 = 1;
	df2 = merge(x=df1, y=base_df, by="explorys_patient_id", all.y=T)
	df2= df2[!is.na(df2$explorys_patient_id),]
	df2[is.na(df2$v2),]$v2 = 0
	colnames(df2)[2] = df_column
	return (df2)
}

df = merge_feature(cohort, alcohol_pre_90, "alcohol")
df1 = merge_feature(df, antibiotics, "antibiotics")
df = merge_feature(df1, grp13, "pre_comor_Group13_Musculoskeletal")
df1 = merge_feature(df, Rheumatism, "pre_comor_Rheumatism")
df = merge_feature(df1, grp10, "pre_comor_Group10_Genitourinary")
df1 = merge_feature(df, grp9, "pre_comor_Group9_Digestive")
df = merge_feature(df1, grp8, "pre_comor_Group8_Respiratory")
df1 = merge_feature(df, grp7, "pre_comor_Group7_Circulatory")
df = merge_feature(df1, HBP, "pre_comor_Hypertension")
df1 = merge_feature(df, grp6, "pre_comor_Group6_Nervous")
df = merge_feature(df1, grp5, "pre_comor_Group5_MentalDisorder")
df1 = merge_feature(df, Anemia, "pre_comor_Anemia")
df = merge_feature(df1, grp3, "pre_comor_Group3_Endocrine")
df1 = merge_feature(df, grp2, "pre_comor_Group2_Neoplasm")
df = merge_feature(df1, grp1, "pre_comor_Group1_InfectiousParasitic")

"explorys_patient_id", "Length_of_Stay", "revision", "complication", "post_recovery",
"gender", "age", "age_60", "age_60_70", "age_70_80", "age_80",
"bmi_pre", "bmi_30", "bmi_30_35", "bmi_35_40", "bmi_40",
"antibiotics", "ever_smoker", "pre_comor_Group3_Endocrine","pre_comor_Anemia", 
"pre_comor_Group5_MentalDisorder", "pre_comor_Group6_Nervous", "pre_comor_Hypertension",
"pre_comor_Group8_Respiratory", "pre_comor_Rheumatism",
#merge others
"pre_comor_Group13_Musculoskeletal"
"pre_comor_Rheumatism"
"pre_comor_Group10_Genitourinary"
"pre_comor_Group9_Digestive"
"pre_comor_Group8_Respiratory"
"pre_comor_Group7_Circulatory"
"pre_comor_Hypertension"
"pre_comor_Group6_Nervous"
"pre_comor_Group5_MentalDisorder"
"pre_comor_Anemia"
"pre_comor_Group3_Endocrine"
"pre_comor_Group2_Neoplasm"
"pre_comor_Group1_InfectiousParasitic"

#outcome

#revision
rev = dbGetQuery(sm, "select count(distinct(explorys_patient_id)) from
(
	select a.* from supermart_111_s.v_procedure a, sandbox_jj.hip_rep_proc_los b --, supermart_111_s.v_encounter d
	where a.explorys_patient_id = b.explorys_patient_id
	--and a.explorys_patient_id = d.explorys_patient_id
	--and a.encounter_join_id = d.encounter_join_id
	and a.proc_date > b.proc_date
	and a.proc_date is not null and a.std_order_status = 'Complete' and a.encounter_join_id is not null
	--and d.std_encounter_type in ('SURGERY', 'PROCEDURE', 'HOSPITAL_INPATIENT', 'HOSPITAL_ENCOUNTER', 'HOSPITAL_OUTPATIEN')
	and (
	a.icd9_concept in ('81.55')
	or a.cpt_concept in  ('27486', '27487')
	or a.snomed_id in ('280462001', '179346008', '179405004', '179415005', '442579001', '280462001', '27912008', '16117008', '179353004', '179410000')
	)
) e


################# Have Revision Surgery 3 month after surgery ##############

post_revision = dbGetQuery(sm, "
--select distinct(explorys_patient_id) from
--(
select distinct a.explorys_patient_id, timestampdiff(day,b.proc_date, a.proc_date) as day_id from supermart_111.v_procedure a, sandbox_jj.LOS_proc_zsun b, supermart_111.v_encounter d
where (a.icd9_concept = '81.55' or a.cpt_concept in ('27486', '27487')  or a.snomed_id in ('179346008', '280462001', '29712008', '16117008', '179353004', '179410000') ) 
and a.explorys_patient_id = b.explorys_patient_id 
and timestampdiff(day, b.proc_date, a.proc_date) >0
--and timestampdiff(day, b.proc_date, a.proc_date) <= 90
and a.proc_date is not null and a.std_order_status = 'Complete' and a.encounter_join_id is not null
and a.encounter_join_id = d.encounter_join_id and a.explorys_patient_id = d.explorys_patient_id
and d.std_encounter_type in ('SURGERY', 'PROCEDURE', 'HOSPITAL_INPATIENT', 'HOSPITAL_ENCOUNTER', 'HOSPITAL_OUTPATIEN')
order by a.explorys_patient_id, day_id
--) e
")

postop$revision90 = as.numeric(postop$explorys_patient_id %in% post_revision$explorys_patient_id)

postop$revision_18m = as.numeric(postop$explorys_patient_id %in% post_revision$explorys_patient_id)





diab = dbGetQuery(sm, "
select distinct a.explorys_patient_id from supermart_111_s.v_diagnosis a, sandbox_jj.hip_rep_proc_los b
where a.explorys_patient_id = b.explorys_patient_id
and a.source_system_type = 'CLINICAL' 
and a.disorder_code like '250%'
and timestampdiff(day, a.diagnosis_date, b.proc_date) <= 730
")


------ 90 days before surgery, smoking status

	select count(distinct(explorys_patient_id)) from
	(
	select a.* from supermart_111.v_habit a, sandbox_jj.LOS_proc_zsun b
	where a.explorys_patient_id = b.explorys_patient_id 
	and timestampdiff(day, a.contact_date, b.proc_date) > 0
	and timestampdiff(day, a.contact_date, b.proc_date) <= 90
	and a.mapped_question_answer like 'tobacco_y'
	order by a.explorys_patient_id, a.contact_date
	) c




------- 90 days before surgery, alcohol status
	select count(distinct(explorys_patient_id)) from
	(
	select a.* from supermart_111.v_habit a, sandbox_jj.LOS_proc_zsun b
	where a.explorys_patient_id = b.explorys_patient_id 
	and timestampdiff(day, a.contact_date, b.proc_date) > 0
	and timestampdiff(day, a.contact_date, b.proc_date) <= 90
	and a.mapped_question_answer like 'alcohol_y'
	order by a.explorys_patient_id, a.contact_date
	) c





--------- Pre-op antibiotics
--Orthopeadic: 60min prior to procedure
-- some may take antibiotics 5-7 days before procedure
-- look back for 14 days

select count(distinct(explorys_patient_id)) from
(
select a.* from supermart_111.v_drug a, sandbox_jj.LOS_proc_zsun b
where a.explorys_patient_id = b.explorys_patient_id 
and (
lower(a.ingredient_descriptions) like '%cefazolin%'
or lower(a.ingredient_descriptions) like '%cefuroxime%'
or lower(a.ingredient_descriptions) like '%vancomycin%'
)
and timestampdiff(day, a.prescription_date, b.proc_date) >= 0  
and timestampdiff(day, a.prescription_date, b.proc_date) <= 14 
and lower(a.std_order_status) = 'complete'
and a.source_system_type = 'CLINICAL'
) c


------ Facility type (Encounter type)
select a.std_encounter_type, count(a.std_encounter_type) as freq 
--select count(a.explorys_patient_id)
from supermart_111.v_encounter a, sandbox_jj.LOS_cut25_zsun b
where a.explorys_patient_id = b.explorys_patient_id
and a.encounter_join_id = b.encounter_join_id
and a.std_encounter_type in ('SURGERY', 'PROCEDURE', 'HOSPITAL_INPATIENT', 'HOSPITAL_ENCOUNTER', 'HOSPITAL_OUTPATIEN')
group by a.std_encounter_type
order by freq desc


------ insurance type

select * from supermart_111.v_demographic

select * from supermart_111.v_encounter

select * from supermart_111.v_procedure

select c.std_insurance_type, count(c.std_insurance_type) as freq from
--select count(distinct(explorys_patient_id)) from
(
select distinct a.explorys_patient_id, a.std_insurance_type from supermart_111.v_demographic a, sandbox_jj.LOS_cut25_zsun b
where a.explorys_patient_id = b.explorys_patient_id
and a.std_insurance_type is not null and source_system_type = 'CLINICAL'
and a.std_insurance_type <> 'UNKNOWN'
) c
group by c.std_insurance_type
order by freq desc

-- number of patients with multiple insurance type (do not have insurance effective date and expire date)
select count(*) from
(
select explorys_patient_id, count(explorys_patient_id) as freq from
(
select distinct a.explorys_patient_id, a.std_insurance_type from supermart_111.v_demographic a, sandbox_jj.LOS_cut25_zsun b
where a.explorys_patient_id = b.explorys_patient_id
and a.std_insurance_type is not null and source_system_type = 'CLINICAL'
and a.std_insurance_type <> 'UNKNOWN'
) c
group by explorys_patient_id
order by freq desc
) d
where freq > 1

-- length of stay == 1 means out patient

select count(*) from sandbox_jj.LOS_cut25_zsun
where Length_of_Stay = 1


------ diabetes
select count(*) from
(
select distinct a.explorys_patient_id from supermart_111.v_diagnosis a, sandbox_jj.los_proc_zsun b
where a.explorys_patient_id = b.explorys_patient_id
and a.source_system_type = 'CLINICAL' 
and a.disorder_code like '250%'
and timestampdiff(day, a.diagnosis_date, b.proc_date) <= 730
) c



---- Revision rate

select count(distinct(explorys_patient_id)) from
(
	select a.* from supermart_111.v_procedure a, sandbox_jj.LOS_cut25_zsun b--, supermart_111.v_encounter d
	where a.explorys_patient_id = b.explorys_patient_id
	--and a.explorys_patient_id = d.explorys_patient_id
	--and a.encounter_join_id = d.encounter_join_id
	and a.proc_date > b.proc_date
	and a.proc_date is not null and a.std_order_status = 'Complete' and a.encounter_join_id is not null
	--and d.std_encounter_type in ('SURGERY', 'PROCEDURE', 'HOSPITAL_INPATIENT', 'HOSPITAL_ENCOUNTER', 'HOSPITAL_OUTPATIEN')
	and (
	a.icd9_concept in ('81.55')
	or a.cpt_concept in  ('27486', '27487')
	or a.snomed_id in ('280462001', '179346008', '179405004', '179415005', '442579001', '280462001', '27912008', '16117008', '179353004', '179410000')
	)
) e

select * from sandbox_jj.LOS_cut25_zsun

select * from sandbox_jj.base6_zsun







--------------------------------- POSTOPERATIVE COMPLICATION ---------------------------------------
select count(distinct(explorys_patient_id)) from 
(
select a.* from supermart_111.v_diagnosis a, sandbox_jj.LOS_proc_zsun b
where a.explorys_patient_id = b.explorys_patient_id
and a.source_system_type = 'CLINICAL'
and timestampdiff(day, b.proc_date, a.diagnosis_date) >= 0
and timestampdiff(day, b.proc_date, a.diagnosis_date) <= 90
--and a.snomed_ids in ('433084008', '433085009', '213121005')
--and a.disorder_code like '998.%' 
and (a.disorder_code like '996.4%' or a.disorder_code like '996.5%' or a.disorder_code like '996.6%' or a.disorder_code like '996.7%')
--or a.disorder_code like '997.9%' or a.disorder_code like '998.%' or a.disorder_code like '999.6%'  or a.disorder_code like '999.7%' or a.disorder_code = '999.9')
) c


------- postop complication
select distinct a.explorys_patient_id from supermart_111.v_diagnosis a, sandbox_jj.LOS_proc_zsun b
where a.explorys_patient_id = b.explorys_patient_id
and a.source_system_type = 'CLINICAL'
and timestampdiff(day, b.proc_date, a.diagnosis_date) >= 0
and timestampdiff(day, b.proc_date, a.diagnosis_date) <= 90
and (a.disorder_code like '996.4%' or a.disorder_code like '996.5%' or a.disorder_code like '996.6%' 
or a.disorder_code like '996.7%')


------ postop orthopedic visit
-- 90-360 days


select a.explorys_patient_id, count(distinct(date(a.encounter_date))) as freq from supermart_111.v_encounter a, sandbox_jj.LOS_proc_zsun b,  supermart_111.v_encounter_provider c
--select a.*  from supermart_111.v_encounter a, sandbox_jj.LOS_proc_zsun b,  supermart_111.v_encounter_provider c
where a.encounter_date is not null and a.std_encounter_status = 'COMPLETED' and a.source_system_type = 'CLINICAL' and a.std_encounter_type = 'OFFICE_VISIT'
and a.encounter_closed = 'Y'
and a.explorys_patient_id = b.explorys_patient_id
and c.explorys_patient_id = b.explorys_patient_id and b.encounter_join_id = c.encounter_join_id
and timestampdiff(day, b.discharge_date, a.encounter_date) >=90 and timestampdiff(day, b.discharge_date, a.encounter_date) <= 360
and (
c.specialty_name_1 = 'Orthopaedics' --or c.specialty_name_2 = 'Orthopaedics' or c.specialty_name_3 = 'Orthopaedics' or c.specialty_name_4 = 'Orthopaedics' or c.specialty_name_5 = 'Orthopaedics'
)
--and a.explorys_patient_id = '183006610'
--and a.explorys_patient_id = '183011763'
--order by a.encounter_date
group by a.explorys_patient_id
order by freq desc




------- postop pain medicine (90 days - 360 days after procedure date)

select count(distinct(explorys_patient_id)) from
(
select a.* from supermart_111.v_drug a, sandbox_jj.LOS_proc_zsun b
where a.explorys_patient_id = b.explorys_patient_id 
and ( lower(a.ingredient_descriptions) like '%capsaisin%'
--lower(a.ingredient_descriptions) like '%codeine%'
--or lower(a.ingredient_descriptions) like '%fentanyl%'
--or lower(a.ingredient_descriptions) like '%hydrocodone%'
--or lower(a.ingredient_descriptions) like '%hydromorphone%'
--or lower(a.ingredient_descriptions) like '%morphine%'
--or lower(a.ingredient_descriptions) like '%oxycodone%'
)
and b.proc_date < a.prescription_date
and timestampdiff(day, b.proc_date, a.prescription_date) >= 360
and timestampdiff(day, b.proc_date, a.prescription_date) <= 540 
and lower(a.std_order_status) = 'complete'
and a.source_system_type = 'CLINICAL'
) c


---- preop pain medicine
select count(distinct(explorys_patient_id)) from
(
select a.* from supermart_111.v_drug a, sandbox_jj.LOS_proc_zsun b
--select distinct(a.ingredient_descriptions) from supermart_111.v_drug a, sandbox_jj.LOS_proc_zsun b
where a.explorys_patient_id = b.explorys_patient_id 
and ( lower(a.ingredient_descriptions) like '%capsaisin%'
--lower(a.ingredient_descriptions) like '%codeine%'
--or lower(a.ingredient_descriptions) like '%fentanyl%'
--or lower(a.ingredient_descriptions) like '%hydrocodone%'
--or lower(a.ingredient_descriptions) like '%hydromorphone%'
--or lower(a.ingredient_descriptions) like '%morphine%'
--or lower(a.ingredient_descriptions) like '%oxycodone%'
)
and a.prescription_date < b.proc_date
and timestampdiff(day, a.prescription_date, b.proc_date) > 30  
and timestampdiff(day, a.prescription_date, b.proc_date) <= 210 
and lower(a.std_order_status) = 'complete'
and a.source_system_type = 'CLINICAL'
) c


---------------------- REVISION KNEE SURGERY ------------------

select distinct(explorys_patient_id) from
(
select a.* from supermart_111.v_procedure a, sandbox_jj.LOS_proc_zsun b, supermart_111.v_encounter d
where (a.icd9_concept = '81.55' or a.cpt_concept in ('27486', '27487')  or a.snomed_id in ('179346008', '280462001', '29712008', '16117008', '179353004', '179410000') ) 
and a.explorys_patient_id = b.explorys_patient_id and a.proc_date > b.proc_date
and a.proc_date is not null and a.std_order_status = 'Complete' and a.encounter_join_id is not null
and a.encounter_join_id = d.encounter_join_id and a.explorys_patient_id = d.explorys_patient_id
and d.std_encounter_type in ('SURGERY', 'PROCEDURE', 'HOSPITAL_INPATIENT', 'HOSPITAL_ENCOUNTER', 'HOSPITAL_OUTPATIEN')
) e


--------------------------------- POSTOP KNEE PAIN DIAGNOSIS 90-360 DAYS -------------------------
select COUNT(distinct(explorys_patient_id)) from
(
select a.* from supermart_111.v_diagnosis a, sandbox_jj.LOS_proc_zsun b
where a.disorder_code like '719.%6'
and a.explorys_patient_id = b.explorys_patient_id
and timestampdiff(day, b.proc_date, a.diagnosis_date) >= 180 
--and timestampdiff(day, b.proc_date, a.diagnosis_date) <= 360 
and a.diagnosis_date is not null and a.status = 'Complete' and a.encounter_record_id_hash is not null
and source_system_type = 'CLINICAL'
) e





------------------- postop X-ray 90-360 days ------------------


select count(*) from
(
select distinct a.explorys_patient_id from supermart_111.v_procedure a, sandbox_jj.LOS_proc_zsun b
where a.snomed_id is not null and a.std_order_status = 'Complete' and a.source_system_type = 'CLINICAL'
and a.explorys_patient_id = b.explorys_patient_id
and (a.snomed_id in ('74016001') or icd9_concept = '88.27')
and timestampdiff(day, b.proc_date, a.proc_date) >90 and timestampdiff(day, b.proc_date, a.proc_date) <= 360
) e



