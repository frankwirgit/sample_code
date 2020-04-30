# An example for data examination before data engineering and data mining process

#===========================================================
#review inpatient data

#Employee Status
> count(inpd[,2])
x  freq
1  Active 15806
2   COBRA   115
3 Unknown   188

#Relationship - drop, the same as #Plan Relationship
> count(inpd[,3])
x freq
1 Dependent 3512
2  Employee 8674
3    Spouse 3923

#Eligibility Source
> count(inpd[,7])
x  freq
1 Created Eligibility   242
2    Plan Eligibility 15867

#Product drop - the same as Plan Product
> count(inpd[,8])
x  freq
1    CDHP   710
2     HMO   860
3     POS 11947
4     PPO  2168
5 Unknown   424

#Plan Description
> count(inpd[,10])
x freq
1                Aetna Basic Plan  396
2             Aetna Choice POS II 6212
3        Aetna Choice POS II Gold  188
4      Aetna Choice POS II Silver   78
5           Aetna Consumer Choice 5571
6  Aetna Consumer Choice Out-Area    1
7                       Aetna HMO  135
8  BCBS Blue Card PPO New England  156
9      BCBS of KC Consumer Choice    1
10  CIGNA Choice Fund HRA OA Plus  314
11                       Coventry 1085
12         DP Aetna Choice POS II    3
13                   DP Aetna HMO    2
14  Horizon BCBS Blue Card PPO CA  301
15  Horizon BCBS Blue Card PPO IL  155
16  Horizon BCBS Blue Card PPO MI   96
17  Horizon BCBS Blue Card PPO NJ   47
18  Horizon BCBS Blue Card PPO PA  138
19   Horizon BCBS Consumer Choice  712
20                            N/A  204
21         NQ Aetna Choice POS II    3
22    NQ Aetna Choice POS II Gold    1
23     United Healthcare Consumer  110
24           UPMC Consumer Choice  112
25               UPMC Health Plan   88

#Benefit Package - drop all is NA
> count(inpd[,11])
x  freq
1 N/A 16109

#Coverage Tier
> count(inpd[,12])
x freq
1 Employee + One 2536
2  Employee Only 3948
3         Family 9625

#Funding Arrangement
> count(inpd[,13])
x  freq
1   B   110
2 N/A 15999

#Medical Carrier
> count(inpd[,14])
x  freq
1                AETNA 12723
2                 BCBS  1619
3                Cigna   325
4 Coventry Health Plan  1106
5    United Healthcare   113
6                 UPMC   223

#Rx Carrier
> count(inpd[,15])
x  freq
1                AETNA 12726
2                 BCBS  1619
3                Cigna   325
4   Coventry Drug Plan    21
5 Coventry Health Plan  1085
6    United Healthcare   110
7                 UPMC   223

#Mental Health Carrier - drop, all is NA

#Claim Vendor
> count(inpd[,17])
x  freq
1           Aetna 12721
2   Cigna Medical   325
3        Coventry  1108
4 Horizon Medical  1619
5             UHC   113
6    UPMC Medical   223

#External Medical Indicator - drop, all is No

#External Pharmacy Indicator - drop, all is No

#Quest Detailed benefit Plantype
> count(inpd[,20])
x  freq
1 HMO   860
2 HRA   314
3 HSA   386
4 N/A   438
5 POS 11943
6 PPO  2168

#Detailed Coverage Tier
> count(inpd[,21])
x freq
1                Domestic Partner    4
2        EE + DP + DPs Child(ren)   23
3        EE + EEs Child(ren) + DP  182
4     EE + EEs Child+DP+DPs Child   25
5           Employee + Child(ren) 2939
6               Employee + Family 6228
7               Employee + Spouse 2396
8                   Employee Only 3879
9  Employee+Domestic Partner (DP)  119
10                            N/A  314

#Quest Employment Status
> count(inpd[,22])
x  freq
1              Active 13202
2    Leave of Absence  2027
3      Leave With Pay   379
4                 N/A   314
5          Terminated   141
6 Terminated With Pay    46

#Detailed Relationship - ??
> count(inpd[,23])
x freq
1                   Daughter 1874
2     Domestic Partner Adult  123
3  Domestic Partner Daughter    6
4       Domestic Partner Son    5
5                   Employee 8525
6         ExDomestic Partner    1
7                   ExSpouse    9
8                 Grandchild   63
9                        N/A  314
10               Other Child    8
11   Qualified Tax Dependent   10
12                       Son 1459
13                    Spouse 3712

8915    OVZV-AWPT-QDYN-AA    Dependent Female               Daughter
9483    OVZV-AWPT-QDYN-AA    Dependent Female             Grandchild
727     OWMF-MLGF-ODYV-QA     Employee Female                    N/A
3842    OWMF-MLGF-ODYV-QA     Employee Female               Employee
399     OWYU-QVYV-YDYU-GA     Employee Female               Employee
7923    OWYU-QVYV-YDYU-GA     Employee Female                    N/A
7928    OXSU-MDQH-ZDYU-OA     Employee Female               Employee
10400   OXSU-MDQH-ZDYU-OA     Employee Female                    N/A
1849    PDON-QXMD-ZDYW-BA       Spouse Female Domestic Partner Adult
9496    PDON-QXMD-ZDYW-BA       Spouse Female                 Spouse



#Detailed Plan Name
> count(inpd[,24])
x freq
1                Aetna Basic Plan  396
2             Aetna Choice POS II 6102
3        Aetna Choice POS II Gold  188
4      Aetna Choice POS II Silver   78
5           Aetna Consumer Choice 5571
6  Aetna Consumer Choice Out-Area    1
7                       Aetna HMO  135
8  BCBS Blue Card PPO New England  156
9      BCBS of KC Consumer Choice    1
10  CIGNA Choice Fund HRA OA Plus  314
11                       Coventry 1085
12         DP Aetna Choice POS II    3
13                   DP Aetna HMO    2
14  Horizon BCBS Blue Card PPO CA  301
15  Horizon BCBS Blue Card PPO IL  155
16  Horizon BCBS Blue Card PPO MI   96
17  Horizon BCBS Blue Card PPO NJ   47
18  Horizon BCBS Blue Card PPO PA  138
19   Horizon BCBS Consumer Choice  712
20                            N/A  314
21         NQ Aetna Choice POS II    3
22    NQ Aetna Choice POS II Gold    1
23     United Healthcare Consumer  110
24           UPMC Consumer Choice  112
25               UPMC Health Plan   88


#Ethnicity - remove N/A cases in double records for the same patient
> count(inpd[,25])
x freq
1 AMIND  117
2 ASIAN 1178
3 BLACK 3285
4 HISPA 1486
5   N/A 2372
6 NSPEC   39
7 PACIF   22
8 WHITE 7610

#Shift
> count(inpd[,26])
x  freq
1            Any   154
2            Day 12512
3        Evening  1590
4            N/A   314
5          Night  1513
6 Not Applicable    26

#Full/Part Time
x  freq
1 Full Time 15786
2       N/A   314
3   On-call     1
4 Part Time     8

#set it as NA
9648    ZDOF-ZUMT-PDYV-BO     Employee   Male      Part Time
13739   ZDOF-ZUMT-PDYV-BO     Employee   Male      Full Time

#Regular/Temp
x  freq
1       N/A   314
2   Regular 15793
3 Temporary     2

dn = names(inpd)[1:28]
demo = inpd[,c(dn)]
> nrow(demo)
[1] 16109
> demo = unique(demo)
> nrow(demo)
[1] 11771
> dt = unique(demo[,c(1,3)])
> nrow(dt)
[1] 9794
> ck = df[df$freq>1,]
> nrow(ck)
[1] 0


dt = unique(demo[,c(1,3,5,26)])
> nrow(dt)
[1] 9867
df = count(dt[,c(1:3)])
> nrow(df)
[1] 9794

ck = df[df$freq>1,]
> nrow(ck)
[1] 73
c = dt[dt$Encrypted.Member.ID %in% ck$Encrypted.Member.ID,]
c = c[order(c$Encrypted.Member.ID),]

rownames(c) <- 1:nrow(c)


#uses the admission date >=2013
dn=names(inpd)[29:62]
demo = inpd[,c(dn)]
#Admission.Date
> count(demo[,2])
x freq
1 2010   12
2 2011   24
3 2012  605
4 2013 4536
5 2014 3984
6 2015 3761
7 2016 3187

> demo = demo[demo$Admission.Date>="2013",]
> count(demo[,2])
x freq
1 2013 4536
2 2014 3984
3 2015 3761
4 2016 3187

#Service.Type
x freq
1                     Maternity 3009
2                       Medical 6538
3 Mental Health/Substance Abuse 1289
4                     Non-Acute  715
5                      Surgical 3917

#DRG.Type
> count(demo[,7])
x  freq
1  Medical 10546
2 Surgical  4922




#DRG
a = "K565    Intestinal adhesions w obst (postprocedural) (postinfection)"
b <- strsplit(a, " ")[[1]][1]

a = inpd$DRG
b <- strsplit(a, " ")
c = sapply(1:length(a), function(x) b[[x]][1])

#Submitted.ICD.Code.Version
x  freq
1 ICD-10  4235
2  ICD-9 11233

#"Discharge.Status
x  freq
1   Dis home health org  1243
2     Dis Intermed Care     7
3    Dis IP Rehab Facil   188
4        Dis Medcr LTCH    37
5   Dis Medcr Swing Bed     1
6   Dis other type inst    11
7     Dis Short Term IP   195
8   Dis Skilled Nursing   260
9     Exp - not recover   108
10  Expired in facility     1
11    Expired TBD State     4
12       Hospice - Home    31
13  Hospice - Med Facil    25
14  IP Admit (Medcr OP)     1
15  Left against advice    86
16  Other Dis TBD State     1
17 Resrv Natl Assigment    69
18    Routine discharge 11308
19   Still Patient - OP   384
20 Still Patient TBD St     6
21              Unknown  1502

#"Discretionary"
> count(demo[,27])
x  freq
1  No 13644
2 Yes  1824

#Supply.Sensitive.Procedures
x  freq
1                                      All Other 14363
2                                   Back surgery   150
3                         Carotid artery surgery    19
4                                Cholecystectomy   168
5         Coronary artery bypass grafting (CABG)    56
6                        Hip replacement surgery   518
7        Lower extremity arterial bypass surgery    45
8 Percutaneous coronary artery angioplasty (PCI)   149


#C.Section
x  freq
1  No 14527
2 Yes   941

#Network.Indicator
x  freq
1     In-Network 14839
2 Out-of-Network   629


#Hospital.Type
x  freq
1          Acute 13626
2        Hospice     3
3          Other  1161
4 Rehabilitation   449
5            SNF   229

#Provider.Type
x  freq
1 Clinics, groups, associations, partnersh    13
2                   Independent Laboratory     5
3                    Independent Radiology     3
4                   Institutional Provider 14284
5               Non-physician professional    12
6                                    Other   337
7                                 Pharmacy     1
8                        Physicians and DO   813

#Plan.Provider.Type
x  freq
1           ACUTE REHABILITATION FACILITY   119
2               ACUTE SHORT TERM HOSPITAL 10466
3                  AMBULATORY SURGICENTER     3
4                      CHILDRENS HOSPITAL   354
5                   CLINICAL PSYCHOLOGIST     4
6                DIAGNOSIS TESTING CENTER     1
7                      DOCTOR OF MEDICINE     1
8  DRUG AND ALCOHOL REHABILITATION CENTER     1
9                  EXTENDED CARE FACILITY     2
10           FREESTANDING BIRTHING CENTER     7
11                 GENERAL ACUTE HOSPITAL   108
12                               HOSPITAL   302
13           INTENSIVE OUTPATIENT PROGRAM     6
14 MULTI BEHAVIORAL HEALTH PROVIDER GROUP     5
15                   MULTI PROVIDER GROUP     1
16                 OTHER MEDICAL PROVIDER     1
17          PARTIAL HOSPITAL/DAY PROGRAMS    16
18                               PHARMACY     1
19                              PHYSICIAN    16
20   PSYCHIATRIC HOSPITAL, ACUTE AND LONG   441
21         RESIDENTIAL TREATMENT FACILITY   192
22               SKILLED NURSING FACILITY   273
23                     SPECIALTY HOSPITAL     1
24               SUBSTANCE ABUSE FACILITY   266
25                            UNAVAILABLE  2878
26                              UNDEFINED     3


