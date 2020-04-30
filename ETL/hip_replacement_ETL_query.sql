--- SQL example to pull pre-defined cohort (specific patient clusters) from the explorys database
--- Those are the eligible patients who will participate the hip replacement studies
--- The patient data displayed in this example has been de-identifed for the data security purpose

----------------------------- COHORT DEFINITION --------------------


select * from sandbox_jj.hip_rep_base where explorys_patient_id=86
encounter_join_id = 52265685

drop table sandbox_jj.hip_rep_base0;

create table sandbox_jj.hip_rep_base0 as
(
select a.*, d.std_encounter_type from supermart_111_s.v_procedure a, supermart_111_s.v_encounter d
where a.icd9_concept = '81.51'
and a.proc_date is not null and a.std_order_status = 'Complete' and a.encounter_join_id is not null
and a.encounter_join_id = d.encounter_join_id and a.explorys_patient_id = d.explorys_patient_id
and d.std_encounter_type in ('SURGERY', 'PROCEDURE', 'HOSPITAL_INPATIENT', 'HOSPITAL_ENCOUNTER', 'HOSPITAL_OUTPATIEN')
);
select count(*) from sandbox_jj.hip_rep_base0  --10138
select count (distinct explorys_patient_id) from sandbox_jj.hip_rep_base0  --7490
select count (distinct explorys_patient_id) from sandbox_jj.hip_rep_base  --13898
select count (distinct explorys_patient_id) from sandbox_jj.hip_rep_base where icd_version='ICD10'  -- 92
select count (distinct explorys_patient_id) from sandbox_jj.hip_rep_base where icd_version='ICD9'  -- 13821

select count (distinct explorys_patient_id) from (
select distinct a.explorys_patient_id from sandbox_jj.hip_rep_base a 
inner join sandbox_jj.hip_rep_base0 b on a.explorys_patient_id = b.explorys_patient_id and a.icd_version='ICD10'
) c;

-- 7404

drop table sandbox_jj.hip_rep_base;
create table sandbox_jj.hip_rep_base as
(
select a.*, d.std_encounter_type from supermart_111.v_procedure a, supermart_111.v_encounter d
where ((a.icd_code = '81.51' and icd_version = 'ICD9') or (a.icd_code in ('0SR90J9','0SR90JA','0SR90JZ','0SRB0J9','0SRB0JA','0SRB0JZ') and icd_version = 'ICD10' ))
and a.proc_date is not null and a.std_order_status = 'Complete' and a.encounter_join_id is not null
and a.encounter_join_id = d.encounter_join_id and a.explorys_patient_id = d.explorys_patient_id
and d.std_encounter_type in ('SURGERY', 'PROCEDURE', 'HOSPITAL_INPATIENT', 'HOSPITAL_ENCOUNTER', 'HOSPITAL_OUTPATIEN')
);

select a.explorys_patient_id, a.encounter_join_id, d.encounter_join_id, d.std_encounter_type from supermart_111.v_procedure a, supermart_111.v_encounter d
where ((a.icd_code = '81.51' and icd_version = 'ICD9') or (a.icd_code in ('0SR90J9','0SR90JA','0SR90JZ','0SRB0J9','0SRB0JA','0SRB0JZ') and icd_version = 'ICD10' ))
and a.proc_date is not null and a.std_order_status = 'Complete' and a.encounter_join_id is not null
and a.encounter_join_id = d.encounter_join_id and a.explorys_patient_id = d.explorys_patient_id
and d.std_encounter_type in ('SURGERY', 'PROCEDURE', 'HOSPITAL_INPATIENT', 'HOSPITAL_ENCOUNTER', 'HOSPITAL_OUTPATIEN')
and a.explorys_patient_id=86

select distinct explorys_patient_id from sandbox_jj.hip_rep_base where icd_version='ICD10'
select count(*) from sandbox_jj.hip_rep_base  --22632
select * from sandbox_jj.hip_rep_base order by 1 limit 10

-- first surgery
drop table sandbox_jj.hip_rep_first;
create table sandbox_jj.hip_rep_first as
(
select a.* from sandbox_jj.hip_rep_base a,
(
select explorys_patient_id, min(proc_date) as op_date from sandbox_jj.hip_rep_base
group by explorys_patient_id
) b
where a.explorys_patient_id = b.explorys_patient_id and a.proc_date = b.op_date
);

select explorys_patient_id, min(proc_date) as op_date from sandbox_jj.hip_rep_base where explorys_patient_id=86

select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_first  --13898
select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_first0  -- 7490

select * from sandbox_jj.hip_rep_first where explorys_patient_id=86

-- between 2010 and 2014
drop table sandbox_jj.hip_rep_201014;
create table sandbox_jj.hip_rep_201014 as
(
select * from sandbox_jj.hip_rep_first
where year(proc_date) >= 2010 and year(proc_date) <= 2014
);

select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_201014  -- 9835
select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_201014_0  --5423

select * from sandbox_jj.hip_rep_201014 where explorys_patient_id=86

-- age at first knee surgery 18+
drop table sandbox_jj.hip_rep_18yr;
create table sandbox_jj.hip_rep_18yr as
(
select a.* from sandbox_jj.hip_rep_201014 a, supermart_111.v_demographic c
where a.explorys_patient_id = c.explorys_patient_id and year(a.proc_date) - c.birth_year >= 18 
);


select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_18yr -- 9676

select * from sandbox_jj.hip_rep_18yr where explorys_patient_id=86

drop table sandbox_jj.hip_rep_18yr;
create table sandbox_jj.hip_rep_18yr as
(
select a.* from sandbox_jj.hip_rep_201014 a, supermart_111_s.v_demographic c
where a.explorys_patient_id = c.explorys_patient_id and year(a.proc_date) - c.birth_year >= 18 
);
select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_18yr --9832


select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_18yr0  --5421

select count(distinct(explorys_patient_id)) from 
(select distinct explorys_patient_id from sandbox_jj.hip_rep_18yr 
where explorys_patient_id in (select distinct explorys_patient_id from sandbox_jj.hip_rep_18yr0)) c;  -- 5356


---- link to admission table

select count(distinct(d.explorys_patient_id)) from supermart_111.v_admission b, supermart_111.v_encounter c, sandbox_jj.hip_rep_18yr d
where d.explorys_patient_id = b.explorys_patient_id 
and b.explorys_patient_id = c.explorys_patient_id 
and d.encounter_join_id = b.encounter_join_id
and b.encounter_join_id = c.encounter_join_id
and c.std_encounter_type in ('SURGERY', 'PROCEDURE', 'HOSPITAL_INPATIENT', 'HOSPITAL_ENCOUNTER', 'HOSPITAL_OUTPATIENT')
and b.admission_date is not null 
and b.discharge_date is not null
and date(d.proc_date) <= date(b.discharge_date)
and timestampdiff(day, b.admission_date, d.proc_date) >= -1


drop table sandbox_jj.hip_rep_adm;
create table sandbox_jj.hip_rep_adm as
(
--select count(distinct(d.explorys_patient_id)) from  supermart_111.v_admission b, supermart_111.v_encounter c, sandbox_jj.hip_rep_18yr d
select distinct d.explorys_patient_id, d.proc_date, d.encounter_join_id, b.admission_date, b.discharge_date, b.std_discharge_disposition 
from  supermart_111.v_admission b, supermart_111.v_encounter c, sandbox_jj.hip_rep_18yr d
where d.explorys_patient_id = b.explorys_patient_id 
and b.explorys_patient_id = c.explorys_patient_id 
and d.encounter_join_id = b.encounter_join_id
and b.encounter_join_id = c.encounter_join_id
and c.std_encounter_type in ('SURGERY', 'PROCEDURE', 'HOSPITAL_INPATIENT', 'HOSPITAL_ENCOUNTER', 'HOSPITAL_OUTPATIENT')
and b.admission_date is not null 
and b.discharge_date is not null
--and b.std_discharge_disposition is not null
--and c.std_encounter_status = 'COMPLETED' -- use this will reduce the size a lot
and date(d.proc_date) <= date(b.discharge_date)
and timestampdiff(day, b.admission_date, d.proc_date) >= -1
);

select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_adm -- 5167
select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_adm0  --4873

select count (distinct explorys_patient_id) from (
select distinct a.explorys_patient_id from sandbox_jj.hip_rep_adm a 
inner join sandbox_jj.hip_rep_adm0 b on a.explorys_patient_id = b.explorys_patient_id
) c;

-- 3841


---------------------------PATIENT LINK TO ADMISSION TABLE, ARE THEY STILL HAVE MULTIPLE RECORDS IN ADMISSION TABLE? (yes)------------------------------------

select count(*) from
(
select explorys_patient_id, count(explorys_patient_id ) as freq from sandbox_jj.hip_rep_adm
group by explorys_patient_id
order by freq desc
) a
where freq > 1 -- 421

-- remove patients with more than one admission records
drop table sandbox_jj.hip_rep_adm_unique;
create table sandbox_jj.hip_rep_adm_unique as
(
	select * from sandbox_jj.hip_rep_adm
	where explorys_patient_id in
	(
	select explorys_patient_id from
	(
	select explorys_patient_id, count(explorys_patient_id ) as freq from sandbox_jj.hip_rep_adm
	group by explorys_patient_id
	order by freq desc
	) a
	where freq = 1
	)
);

select count(*) from
(
select explorys_patient_id, count(explorys_patient_id ) as freq from sandbox_jj.hip_rep_adm0
group by explorys_patient_id
order by freq desc
) a
where freq > 1 -- 35

-- remove patients with more than one admission records
drop table sandbox_jj.hip_rep_adm_unique0;
create table sandbox_jj.hip_rep_adm_unique0 as
(
	select * from sandbox_jj.hip_rep_adm0
	where explorys_patient_id in
	(
	select explorys_patient_id from
	(
	select explorys_patient_id, count(explorys_patient_id ) as freq from sandbox_jj.hip_rep_adm0
	group by explorys_patient_id
	order by freq desc
	) a
	where freq = 1
	)
);

select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_adm_unique -- 4746 ?? include ICD10 but having less patient having one admission records
select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_adm_unique0 -- 4838


select count (distinct explorys_patient_id) from (
select distinct a.explorys_patient_id from sandbox_jj.hip_rep_adm_unique a 
inner join sandbox_jj.hip_rep_adm_unique0 b on a.explorys_patient_id = b.explorys_patient_id
) c;
-- 3798


-- stay with the original list with ICD9 only

---- LOS based on admission date
--drop table sandbox_jj.hip_rep_adm_los
create table sandbox_jj.hip_rep_adm_los as
(
  select a.explorys_patient_id, a.encounter_join_id, a.proc_date, 
  timestampdiff(day, a.admission_date, a.discharge_date) + 1 as Length_of_Stay, 
  a.admission_date, a.discharge_date, a.std_discharge_disposition, b.concept_name
  from sandbox_jj.hip_rep_adm_unique a, xref.snomed b
  where a.std_discharge_disposition = b.concept_id
  order by a.explorys_patient_id
);

select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_adm_los  --4836
--drop table sandbox_jj.hip_rep_proc_los
---- LOS based on proc date

create table sandbox_jj.hip_rep_proc_los as
(
  select a.explorys_patient_id, a.encounter_join_id, a.proc_date, 
  timestampdiff(day, a.proc_date, a.discharge_date) + 1 as Length_of_Stay, 
  a.admission_date, a.discharge_date, a.std_discharge_disposition, b.concept_name
  from sandbox_jj.hip_rep_adm_unique a, xref.snomed b
  where a.std_discharge_disposition = b.concept_id
  order by a.explorys_patient_id
);

select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_proc_los --4836

select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_proc_los where Length_of_Stay > 1  --4817


-- get the demographic information age, gender, weight, height and BMI, ever smoker and alcohol

-- merge the information
select distinct a.explorys_patient_id from sandbox_jj.hip_rep_proc_los

b.age, b.gender
from sandbox_jj.hip_rep_demo2

---- age, gender race etc.
drop table sandbox_jj.hip_rep_demo2;
create table sandbox_jj.hip_rep_demo2 as
(
	select distinct a.explorys_patient_id, (year(a.proc_date) - b.birth_year) as age, b.std_gender as gender, 
	b.std_language as language, b.std_religion as religion, b.std_ethnicity as ethnicity, b.std_race as race, b.std_insurance_type as insurance
	from sandbox_jj.hip_rep_proc_los a
	inner join supermart_111_s.v_demographic b
	on a.explorys_patient_id = b.explorys_patient_id
	order by a.explorys_patient_id
);

select * from sandbox_jj.hip_rep_demo2 order by 1 limit 30

drop table sandbox_jj.hip_rep_demo1;
create table sandbox_jj.hip_rep_demo1 as
(
	select distinct explorys_patient_id, age, gender from sandbox_jj.hip_rep_demo2
	order by explorys_patient_id
)
select count(distinct explorys_patient_id) from sandbox_jj.hip_rep_demo1  --4836


-- check the frequency
select gender, count(*) as freq_gender from 
(select distinct explorys_patient_id, gender from sandbox_jj.hip_rep_demo2) a
group by gender order by gender

gender	freq_gender  --total 4836
0	1
1	1957
2	2878

select age, count(*) as freq_gender from 
(select distinct explorys_patient_id, age from sandbox_jj.hip_rep_demo2) a
group by age order by age

select sum(freq_gender) from
(select age, count(*) as freq_gender from 
(select distinct explorys_patient_id, age from sandbox_jj.hip_rep_demo2) a
group by age order by age) b


age	freq_gender  -- total 4836
22	1
24	1
25	1
26	3
27	1
29	3
30	1
31	1
32	3
33	2
35	3
36	3
37	1
38	8
39	4
40	8
41	4
42	13
43	13
44	16
45	17
46	20
47	32
48	28
49	30
50	42
51	55
52	69
53	53
54	102
55	101
56	96
57	117
58	104
59	148
60	139
61	136
62	153
63	157
64	175
65	171
66	182
67	175
68	170
69	167
70	157
71	157
72	147
73	177
74	145
75	139
76	132
77	127
78	128
79	127
80	104
81	98
82	84
83	88
84	64
85	90
86	57
87	50
88	26
89	7
90	3


---- gender check
/*
drop table sandbox_jj.hip_rep_demo2;
create table sandbox_jj.hip_rep_demo2 as
(
select distinct a.explorys_patient_id, a.std_gender, d.std_value from supermart_111.v_demographic a, sandbox_jj.tkr_cohort_union_zsun b,  xref.std_code_value d
where a.explorys_patient_id = b.explorys_patient_id
and d.attribute = 'gender' and d.std_code = a.std_gender
) c
group by c.std_value
order by freq desc
*/

---- body weight and height
drop table sandbox_jj.hip_rep_demo_tmp1;
create table sandbox_jj.hip_rep_demo_tmp1 as
(
    select distinct c.explorys_patient_id, a.std_value as weight, a.observation_date, c.proc_date from sandbox_jj.hip_rep_proc_los c 
	inner join supermart_111_s.v_observation a on a.explorys_patient_id = c.explorys_patient_id
	where a.loinc_test_id = '29463-7' and a.std_value is not null and a.std_report_status = 'Complete' and a.source_system_type = 'CLINICAL' and 
	c.proc_date >=  a.observation_date
);

select * from sandbox_jj.hip_rep_demo_tmp1 order by 1 limit 30
select * from sandbox_jj.hip_rep_demo_tmp1 where explorys_patient_id='8452' order by observation_date

drop table sandbox_jj.hip_rep_demo_tmp2;
create table sandbox_jj.hip_rep_demo_tmp2 as
(
	select a.* from sandbox_jj.hip_rep_demo_tmp1 a inner join
	(select distinct explorys_patient_id, max(observation_date) as max_obs from sandbox_jj.hip_rep_demo_tmp1 
	 group by explorys_patient_id order by explorys_patient_id) b 
	on a.explorys_patient_id = b.explorys_patient_id and a.observation_date = b.max_obs
)

select count(distinct explorys_patient_id) from sandbox_jj.hip_rep_demo_tmp2 --4456

-- dedup the records having the same observation date
drop table sandbox_jj.hip_rep_demo_tmp3;
create table sandbox_jj.hip_rep_demo_tmp3 as
(
select explorys_patient_id, weight, observation_date, proc_date from 
(select explorys_patient_id, weight, observation_date, proc_date, 
row_number() over (partition by explorys_patient_id) as row_numb from sandbox_jj.hip_rep_demo_tmp2) b 
where row_numb=1
order by explorys_patient_id
)

select count(distinct explorys_patient_id) from sandbox_jj.hip_rep_demo_tmp3 --4456

drop table sandbox_jj.hip_rep_demo_tmp1;
drop table sandbox_jj.hip_rep_demo_tmp2;


drop table sandbox_jj.hip_rep_demo_tmp1;
create table sandbox_jj.hip_rep_demo_tmp1 as
(
    select distinct c.explorys_patient_id, a.std_value as height, a.observation_date, c.proc_date from sandbox_jj.hip_rep_proc_los c 
	inner join supermart_111_s.v_observation a on a.explorys_patient_id = c.explorys_patient_id
	where a.loinc_test_id = '8302-2' and a.std_value is not null and a.std_report_status = 'Complete' and a.source_system_type = 'CLINICAL' and 
	c.proc_date >=  a.observation_date
);


drop table sandbox_jj.hip_rep_demo_tmp2;
create table sandbox_jj.hip_rep_demo_tmp2 as
(
	select a.* from sandbox_jj.hip_rep_demo_tmp1 a inner join
	(select distinct explorys_patient_id, max(observation_date) as max_obs from sandbox_jj.hip_rep_demo_tmp1 
	 group by explorys_patient_id order by explorys_patient_id) b 
	on a.explorys_patient_id = b.explorys_patient_id and a.observation_date = b.max_obs
)

select count(distinct explorys_patient_id) from sandbox_jj.hip_rep_demo_tmp2 --4453

drop table sandbox_jj.hip_rep_demo_tmp4;
create table sandbox_jj.hip_rep_demo_tmp4 as
(
select explorys_patient_id, height, observation_date, proc_date from 
(select explorys_patient_id, height, observation_date, proc_date, 
row_number() over (partition by explorys_patient_id) as row_numb from sandbox_jj.hip_rep_demo_tmp2) b 
where row_numb=1
order by explorys_patient_id
)

select count(distinct explorys_patient_id) from sandbox_jj.hip_rep_demo_tmp4 --4453


drop table sandbox_jj.hip_rep_demo_tmp1;
drop table sandbox_jj.hip_rep_demo_tmp2;
drop table sandbox_jj.hip_rep_demo_tmp;
create table sandbox_jj.hip_rep_demo_tmp as
(
select distinct a.explorys_patient_id, a.weight, b.height from 
sandbox_jj.hip_rep_demo_tmp3 a, sandbox_jj.hip_rep_demo_tmp4 b
where a.explorys_patient_id = b.explorys_patient_id
order by a.explorys_patient_id
);
select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_demo_tmp  -- 4424

select * from sandbox_jj.hip_rep_demo_tmp order by 1 limit 30


-- for checking purpose only: remove the requirement of test done before the surgery date
-- 4790 (weight)
select count(distinct(b.explorys_patient_id)) from sandbox_jj.hip_rep_proc_los b inner join
supermart_111_s.v_observation a on a.explorys_patient_id = b.explorys_patient_id
where a.loinc_test_id = '29463-7' and std_value is not null and std_report_status = 'Complete' and source_system_type = 'CLINICAL';

-- 4798 (height)
select count(distinct(b.explorys_patient_id)) from sandbox_jj.hip_rep_proc_los b inner join
supermart_111_s.v_observation a on a.explorys_patient_id = b.explorys_patient_id
where a.loinc_test_id = '8302-2' and std_value is not null and std_report_status = 'Complete' and source_system_type = 'CLINICAL';


---- ever smoker

create table sandbox_jj.hip_rep_smoker as
(
	select distinct(b.explorys_patient_id) as explorys_patient_id, 'v_habit' as source
	from supermart_111_s.v_habit a, sandbox_jj.hip_rep_proc_los b
	where a.explorys_patient_id = b.explorys_patient_id 
	and a.contact_date < b.proc_date
	and a.mapped_question_answer like 'tobacco_y'
	order by b.explorys_patient_id
);

select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_smoker where source='v_habit'  --510

insert into sandbox_jj.hip_rep_smoker
select distinct(b.explorys_patient_id) as explorys_patient_id, 'v_diag' as source
from supermart_111.v_diagnosis a, sandbox_jj.hip_rep_proc_los b
where a.explorys_patient_id = b.explorys_patient_id 
and a.diagnosis_date < b.proc_date
and a.disorder_code in ('V15.82', '305.1')
order by b.explorys_patient_id;


select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_smoker where source='v_diag'  --1022


select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_smoker  --1215
select count(*) from sandbox_jj.hip_rep_smoker  --1532



-- combine age gender weight height and smoking status
create table sandbox_jj.hip_rep_demo as 
(

	select c.*, '0' as ever_smoker from
	(
		select a.*, b.weight, b.height from sandbox_jj.hip_rep_demo1 a left outer join sandbox_jj.hip_rep_demo_tmp b
		on a.explorys_patient_id = b.explorys_patient_id
		order by a.explorys_patient_id
	) c

)
update sandbox_jj.hip_rep_demo set ever_smoker = '1' where explorys_patient_id in (select distinct explorys_patient_id from sandbox_jj.hip_rep_smoker)

select count(*) from sandbox_jj.hip_rep_demo where ever_smoker = '1'  --1215

select count(distinct(explorys_patient_id)) from sandbox_jj.hip_rep_demo  --4836

select * from from sandbox_jj.hip_rep_demo order by 1 limit 30
drop table sandbox_jj.hip_rep_demo1;
drop table sandbox_jj.hip_rep_demo_tmp;

select * from sandbox_jj.hip_rep_demo where (weight>0 and height is NULL) or (height>0 and weight is NULL)









---- check the subset based on the TKR data filtered out  -- 1568
select count(distinct(a.explorys_patient_id)) from sandbox_jj.hip_rep_adm_unique a
inner join sandbox_jj.base6_icd9_zsun b
on a.explorys_patient_id = b.explorys_patient_id  






























