# An ETL example to process user demographic data

import pandas as pd
import numpy as np

#src="/watson/quest_eh/data/original_files/2017MAY04/"
#dst="/watson/quest_eh/workspace/lcao/data/"

print pd.options.display.width
print pd.options.display.max_colwidth

pd.set_option('display.width', 150)
pd.set_option('max_colwidth', 50) 


#eligibility checking
#f = "2013-2016EligibilityFiles/2016.Eligibility.csv"
f = "data/2016.Eligibility.csv"
e = pd.read_csv(f,dtype=object, usecols=[0,1,6])
e.columns.values[2]="year"
el = e['Encrypted Member ID'].tolist()
#list(e)

#process claims
#read in claims
#f = "inpt_0518Y17/inpt_claims_y13-16.csv"
#f = "data/inpt_claims_y13-16.csv"
#cols=[0,3,21]+range(29,45)
f = "data/outpt_claims_y13-16.csv"
cols=[0,3,21]+range(30,46)
#cols=[0,3,21]+range(31,43)
#cols=[0,3,21,22]
inp = pd.read_csv(f, dtype=object).ix[:,cols]


inp = inp.drop_duplicates()
#inp = inp.loc[(inp['Admission.Date']=="2016"),]
inp = inp.loc[(inp['Service.Date']=="2016"),]
inp = inp[inp['Encrypted.Member.ID'].isin(el)]


#list(inp)
#inp.shape
dt = inp.ix[:,range(3,19)]
#dt = inp.ix[:,range(3,15)]
db = dt.copy()
for col in dt:
    db[col]=dt[col].str.replace('\.','').copy()

#db.shape

db0 = inp.ix[:,0]
db1 = db.ix[:,range(0,5)]
db2 = db.ix[:,range(5,10)]
db3 = db.ix[:,range(10,13)]
db4 = db.ix[:,range(13,16)]
#db5 = db.iloc[:,[10]]
#type(db5)



#gender
#f = "inpt_0518Y17/inpt_member_gender_race.csv"
#f = "data/inpt_member_gender_race.csv"
f = "data/outpt_member_gender_race.csv"
ng = pd.read_csv(f,dtype=object, usecols=[0,1])
ng = pd.merge(inp.iloc[:,[0]],ng,on=['Encrypted.Member.ID'],how='left').drop_duplicates()
ng.shape


#age
inp['age'] = pd.to_numeric(inp['Age'])
inp.loc[inp['age']==999,'age']=-1
ag = inp.loc[:,['Encrypted.Member.ID','age']].drop_duplicates()
ag.shape

def check_code(df0,df,codelist):
	import numpy as np
	bol = np.column_stack([df[col].str.contains(codelist, na=False) for col in df])
	mem = df0.loc[bol.any(axis=1)]
	return(mem)


#smoking
smk1 = check_code(db0,db1,"^3051|^98984")
smk2 = check_code(db0,db2,"^F17")
smk = pd.concat([smk1,smk2]).drop_duplicates()


#quit_current,quit_attempt
quit_attempt = check_code(db0,db4,"^HZ80|^HZ90").drop_duplicates()


#overweight
over1 = check_code(db0,db1,"^27802")
over2 = check_code(db0,db2,"^E663")
over = pd.concat([over1,over2]).drop_duplicates()

#obese
ob1 = check_code(db0,db1,"^27800|^27801")
ob2 = check_code(db0,db2,"^E660|^E669")
ob = pd.concat([ob1,ob2]).drop_duplicates()

#alcohol_depend
alc1 = check_code(db0,db1,"^29181|^303")
alc2 = check_code(db0,db2,"^F10|^Z7141")
alc = pd.concat([alc1,alc2]).drop_duplicates()

#alcohol_treat
list(db2)
list(db3)
list(db4)
alt1 = check_code(db0,db2,"^Z7141")
alt2 = check_code(db0,db3,"^946[123789]")
alt3 = check_code(db0,db4,"^HZ2Z")
alt = pd.concat([alt1,alt2,alt3]).drop_duplicates()

#mh_osteo
osteo1 = check_code(db0,db1,"^7330")
osteo2 = check_code(db0,db2,"^M80|^M81")
osteo = pd.concat([osteo1,osteo2]).drop_duplicates()

#mh_mlsk"
mlsk1 = check_code(db0,db1,"^7[1-3][0-9]")
mlsk2 = check_code(db0,db2,"^M[0-9][0-9]")
mlsk = pd.concat([mlsk1,mlsk2]).drop_duplicates()

#mh_ihd
ihd = check_code(db0,db1,"^41[0-4]").drop_duplicates()

#mh_avd_others
avd = check_code(db0,db1,"^44[0-9]").drop_duplicates()

#mh_diabetes
diab1 = check_code(db0,db1,"^250")
diab2 = check_code(db0,db2,"^E0[8-9]|^E1[0-3]")
diab = pd.concat([diab1,diab2]).drop_duplicates()


#mh_gdiabetes
gdia1 = check_code(db0,db1,"^6488")
gdia2 = check_code(db0,db2,"^O998")
gdia = pd.concat([gdia1,gdia2]).drop_duplicates()

#mh_htn
htn1 = check_code(db0,db1,"^401|^405")
htn2 = check_code(db0,db2,"^I1[1-6]")
htn = pd.concat([htn1,htn2]).drop_duplicates()


#mh_depression
dep1 = check_code(db0,db1,"^2962|^2963|^311")
dep2 = check_code(db0,db2,"^F32|^F33")
dep = pd.concat([dep1,dep2]).drop_duplicates()



#mh_mental_other
men1 = check_code(db0,db1,"^29[^6]|^296[^2^3]|^30|^31[^1]")
men2 = check_code(db0,db2,"^F0[1-9]|^F[^0^3][0-9]|^F3[^2^3]")
men = pd.concat([men1,men2]).drop_duplicates()


#mh_ckd
ckd1 = check_code(db0,db1,"^585|V4511")
ckd2 = check_code(db0,db2,"^N18|^Z49")
ckd3 = check_code(db0,db3,"^3995",)
ckd = pd.concat([ckd1,ckd2,ckd3]).drop_duplicates()


#fh_chd
chd1 = check_code(db0,db1,"^V1749")
chd2 = check_code(db0,db2,"Z8249")
chd = pd.concat([chd1,chd2]).drop_duplicates()

#fh_diabetes
dia1 = check_code(db0,db1,"^V180")
dia2 = check_code(db0,db2,"Z833")
dia = pd.concat([dia1,dia2]).drop_duplicates()

#fh_breastc
bcs1 = check_code(db0,db1,"^V163")
bcs2 = check_code(db0,db2,"Z803")
bcs = pd.concat([bcs1,bcs2]).drop_duplicates()

#fh_colonc
col1 = check_code(db0,db1,"^V160")
col2 = check_code(db0,db2,"Z800")
col = pd.concat([col1,col2]).drop_duplicates()

#fh_prostatec
pro1 = check_code(db0,db1,"^V1642")
pro2 = check_code(db0,db2,"Z8042")
pro = pd.concat([pro1,pro2]).drop_duplicates()


#save the result of inp
ing = ng
iag=ag
ismk=smk
iquitt=quit_attempt
iover = over
iob = ob
ialc = alc
ialt = alt
iosteo = osteo
imlsk= mlsk
iihd = ihd
iavd = avd
idiab = diab
igdia = gdia
ihtn = htn
idep = dep
imen = men
ickd = ckd
ichd = chd
idia = dia
ibcs = bcs
icol = col
ipro = pro


#save the result of outp
ong = ng
oag=ag
osmk=smk
oquitt=quit_attempt
oover = over
oob = ob
oalc = alc
oalt = alt
oosteo = osteo
omlsk= mlsk
oihd = ihd
oavd = avd
odiab = diab
ogdia = gdia
ohtn = htn
odep = dep
omen = men
ockd = ckd
ochd = chd
odia = dia
obcs = bcs
ocol = col
opro = pro


#for prof claims
f = "data/pfp16.csv"
inp = pd.read_csv(f, dtype=object)

f = "data/prof_claims_member_gender_race.csv"
ng = pd.read_csv(f,dtype=object, usecols=[0,1])
ng = pd.merge(inp.iloc[:,[0]],ng,on=['Encrypted.Member.ID'],how='left').drop_duplicates()
ng.shape
png = ng

inp['age'] = pd.to_numeric(inp['Age'])
inp.loc[inp['age']==999,'age']=-1
ag = inp.loc[:,['Encrypted.Member.ID','age']].drop_duplicates()
ag.shape
pag = ag

dt = inp.ix[:,range(3,15)]
db = dt.copy()
for col in dt:
    db[col]=dt[col].str.replace('\.','').copy()

db.shape

db0 = inp.ix[:,0]
db1 = db.ix[:,range(0,5)]
db2 = db.ix[:,range(5,10)]
db5 = db.iloc[:,[10]]

#quit_current,quit_attempt
pquitt = check_code(db0,db5,"99406|99407").drop_duplicates()

alt1 = check_code(db0,db2,"^Z7141")
alt2 = check_code(db0,db5,"99408|99409")
palt = pd.concat([alt1,alt2]).drop_duplicates()

men1 = check_code(db0,db1,"^29[^6]|^296[^2^3]|^30|^31[^1]")
men2 = check_code(db0,db2,"^F0[1-9]|^F[^0^3][0-9]|^F3[^2^3]")
pmen = pd.concat([men1,men2]).drop_duplicates()


ckd1 = check_code(db0,db1,"^585|V4511")
ckd2 = check_code(db0,db2,"^N18|^Z49")
ckd3 = check_code(db0,db5,"^9093[5-9]|^909[4-9][0-9]",)
pckd = pd.concat([ckd1,ckd2,ckd3]).drop_duplicates()


#fh_chd
chd1 = check_code(db0,db1,"^V1749")
chd2 = check_code(db0,db2,"Z8249")
pchd = pd.concat([chd1,chd2]).drop_duplicates()

#fh_diabetes
dia1 = check_code(db0,db1,"^V180")
dia2 = check_code(db0,db2,"Z833")
pdia = pd.concat([dia1,dia2]).drop_duplicates()

#fh_breastc
bcs1 = check_code(db0,db1,"^V163")
bcs2 = check_code(db0,db2,"Z803")
pbcs = pd.concat([bcs1,bcs2]).drop_duplicates()

#fh_colonc
col1 = check_code(db0,db1,"^V160")
col2 = check_code(db0,db2,"Z800")
pcol = pd.concat([col1,col2]).drop_duplicates()

#fh_prostatec
pro1 = check_code(db0,db1,"^V1642")
pro2 = check_code(db0,db2,"Z8042")
ppro = pd.concat([pro1,pro2]).drop_duplicates()


#for rx
f = "data/rx016.csv"
rx = pd.read_csv(f, dtype=object)
rx.shape
f = "data/rx_member_gender_race.csv"
ng = pd.read_csv(f,dtype=object, usecols=[0,1])
ng = pd.merge(rx.iloc[:,[0]],ng,on=['Encrypted.Member.ID'],how='left').drop_duplicates()
ng.shape
rng = ng

rx.loc[rx['Age']=="90+",'Age']='91'
rx['age'] = pd.to_numeric(rx['Age'])
rx.loc[rx['age']==999,'age']=-1
ag = rx.loc[:,['Encrypted.Member.ID','age']].drop_duplicates()
ag.shape
rag = ag

#read in NDC
#f = "rx_0519Y17/NDC_Mapping.csv"
f = "data/NDC_Mapping.csv"
ndcm = pd.read_csv(f, dtype=object)

def get_rxmem(rx,ndcs,druglist):
	n = ndcs.iloc[:,1].str.contains(druglist, case=False, na=False)
	ndrug = ndcs[n==True]['NDC'].tolist()
	rmem = rx[rx['NDC'].isin(ndrug)]['Encrypted.Member.ID'].drop_duplicates()
	return(rmem)

ndcs = ndcm.iloc[:,[0,3]]
rquit = get_rxmem(rx,ndcs,'NICOTINE')

ndcs = ndcm.iloc[:,[0,8]]
rdiab = get_rxmem(rx,ndcs,'diabetes')

rhtn = get_rxmem(rx,ndcs,'hypertension')

rdep = get_rxmem(rx,ndcs,'depression')

rmen = get_rxmem(rx,ndcs,'anxiety|adhd|bipolar|schizophrenia')

#merge the same feature in claims
def fcomb(dlist,fname):
	df = pd.DataFrame(pd.concat(dlist).drop_duplicates())
	df[fname]=1
	return(df)
 
smk = fcomb([ismk,osmk], "smoking")
quitt = fcomb([iquitt,oquitt,pquitt,rquit],"quit_attempt")
quitt['quit_current'] = quitt['quit_attempt']

over = fcomb([iover,oover],"overweight")
ob = fcomb([iob,oob],"obese")
alc = fcomb([ialc,oalc],"alcohol_depend")
alt = fcomb([ialt,oalt,palt],"alcohol_treat")
osteo = fcomb([iosteo,oosteo],"mh_osteo")
mlsk = fcomb([imlsk,omlsk],"mh_mlsk")
ihd = fcomb([iihd,oihd],"mh_ihd")
avd = fcomb([iavd,oavd],"mh_avd_others")

diab = fcomb([idiab,odiab,rdiab],"mh_diabetes")
gdia = fcomb([igdia,ogdia],"mh_gdiabetes")
htn = fcomb([ihtn,ohtn,rhtn],"mh_htn")
dep = fcomb([idep,odep,rdep],"mh_depression")
men = fcomb([imen,omen,pmen,rmen],"mh_mental_other")
ckd = fcomb([ickd,ockd,pckd],"mh_ckd")
chd = fcomb([ichd,ochd,pchd],"fh_chd")
dia = fcomb([idia,odia,pdia],"fh_diabetes")
bcs = fcomb([ibcs,obcs,pbcs],"fh_breastc")
col = fcomb([icol,ocol,pcol],"fh_colonc")
pro = fcomb([ipro,opro,ppro],"fh_prostatec")
#pro.shape

#merge all features in claims	
flist = [smk,quitt,over,ob,alc,alt,osteo,mlsk,ihd,avd,diab,gdia,htn,dep,men,ckd,chd,dia,bcs,col,pro]
fclm = reduce(lambda left,right: pd.merge(left,right,on=['Encrypted.Member.ID'],how='outer'), flist)
fclm = fclm.fillna(0)
fclm.shape

#gender
flist = [ing,ong,png,rng]
cng = reduce(lambda left,right: pd.merge(left,right,on=['Encrypted.Member.ID','Gender'],how='outer'), flist).drop_duplicates()
cng['gender']='M'
cng.loc[cng['Gender']=='Female','gender']='F'
cng=cng.drop(['Gender'],1)
cng.shape

#age
flist = [iag,oag,pag,rag]
cag = reduce(lambda left,right: pd.merge(left,right,on=['Encrypted.Member.ID','age'],how='outer'), flist).drop_duplicates()
#cag.shape
cag = cag.groupby(['Encrypted.Member.ID'], as_index=False).agg({'age':'max'})
cag.loc[cag['age'] == -1,'age'] = np.NaN
cag.shape

#combine
e['Encrypted.Member.ID']=e['Encrypted Member ID']
e=e[['Encrypted.Member.ID','Relationship','year']]
flist = [e,cng,cag]
tmp = reduce(lambda left,right: pd.merge(left,right,on=['Encrypted.Member.ID'],how='right'), flist).drop_duplicates()
tmp.shape

fclm = pd.merge(tmp,fclm,on=['Encrypted.Member.ID'],how='left')
fclm.shape
#fclm.drop_duplicates().shape
#fclm1 = fclm2[fclm2['Relationship']=="Employee"]
#fclm1.shape
fclm.to_csv('data/fclm16.csv', index=False, encoding='utf-8')
#fclm1 = pd.read_csv('data/fclm16.csv',dtype=object)
#fclm1.shape
####################################################

#process lab data
#read in lab
#f = "lab_0517Y17/lab_y16.csv"
f = "data/lab_y16.csv"
lab = pd.read_csv(f,dtype=object, usecols=[0,4,7,8])
lab = lab[lab['Encrypted.Member.ID'].isin(el)]

def lab_feature(LOINC, df, fname):
	ff = df.loc[df['Quest.LOINC.Code']==LOINC,['Encrypted.Member.ID','Quest.Result']]
	ff[fname]=pd.to_numeric(ff['Quest.Result'])
	return(ff.ix[:,[0,2]])

bmif = lab_feature("39156-5",lab,"bmi")
fbmi = bmif.groupby(['Encrypted.Member.ID'],as_index=False).agg({'bmi':'mean'})

fbmi['overweight'] = ((fbmi['bmi']>=25) & (fbmi['bmi']<30)).astype('int')
fbmi['obese'] = (fbmi['bmi']>=30).astype('int')

heif = lab_feature("3137-7",lab,"height")
weif = lab_feature("3142-7",lab,"weight")
fhei = heif.groupby(['Encrypted.Member.ID'],as_index=False).agg({'height':'mean'})
fwei = weif.groupby(['Encrypted.Member.ID'],as_index=False).agg({'weight':'mean'})

wcf = lab_feature("8280-0",lab,"wc")
fwc = wcf.groupby(['Encrypted.Member.ID'],as_index=False).agg({'wc':'mean'})
fwc.shape

flist=[fhei,fwei,fbmi,fwc]
tmp = reduce(lambda left,right: pd.merge(left,right,on=['Encrypted.Member.ID'],how='outer'), flist).drop_duplicates()
tmp.shape

#flab = fbmi.merge(fhei.merge(fwei, on='Encrypted.Member.ID',how="outer"), on='Encrypted.Member.ID',how="outer")


#gender
#f = "lab_0517Y17/lab_member_gender.csv"
f = "data/lab_member_gender.csv"
lng = pd.read_csv(f,dtype=object).loc[:,['Encrypted.Member.ID','Quest.Gender']]

lng['gender']='M'
lng.loc[lng['Quest.Gender']=='Female','gender']='F'
lng=lng.drop(['Quest.Gender'],1)
lng.shape

#age
lab['age'] = pd.to_numeric(lab['Age'])
lab.loc[pd.isnull(lab['age']),'age']=-1
lag = lab.loc[:,['Encrypted.Member.ID','age']]
lag = lag.groupby(['Encrypted.Member.ID'],as_index=False).agg({'age':'max'})
lag.loc[lag['age'] == -1,'age'] = np.NaN
lag.shape

#combine
flist = [e,lng,lag]
ga = reduce(lambda left,right: pd.merge(left,right,on=['Encrypted.Member.ID'],how='right'), flist).drop_duplicates()
#ga.shape
#list(ga)
flab = pd.merge(ga,tmp,on=['Encrypted.Member.ID'],how='left')
#flab.shape
flab['large_wc'] = (((flab['wc']>=35) & (flab['gender']=="F")) | ((flab['wc']>=40) & (flab['gender']=="M"))).astype('int')

#list(flab)
flab.to_csv('data/flab16.csv', index=False, encoding='utf-8')
#flab1 = pd.read_csv('data/flab16.csv',dtype=object)
#flab1.shape
####################################################

#select type questions
#f = "HRA_Y16/HRA_Select_processed_Y16.csv"
f = "data/HRA_Select_processed_Y16.csv"
hra = pd.read_csv(f,dtype=object)
hra = hra[hra['Encrypted.Member.ID'].isin(el)]
hra.shape

#fill in type questions
f = "data/HRA_Fillin_processed_Y16.csv"
hra2 = pd.read_csv(f,dtype=object)
hra2 = hra2[hra2['Encrypted.Member.ID'].isin(el)]
hra2.shape

#question list
f = "data/HRA_question_Y16.csv"
qlist = pd.read_csv(f,dtype=object)

qid = ['2','840','80','83','84','85','827','86','831','87','832','3000','1900','2100','2200','2300','2000','2600','2700','49','129','837','838']
hs = hra[hra['Question.ID'].isin(qid)]
#hs.shape
#for y in hs.columns:
#    print hs[y].dtype

def hra_feature(Qid,Aid,df,fname):
    ff = df.loc[pd.to_numeric(df['Question.ID'])==Qid,:]
    f1 = ff.ix[ff['Answer.ID'].str.contains(Aid, na=False),[0]]
    f1[fname]=1
    return(f1)



def hra_feature2(Qid, df, fname):
	ff = df.loc[pd.to_numeric(df['Question.ID'])==Qid,:]
	ff.columns.values[3]=fname
	return(ff.iloc[:,[0,3]])
	
	
def hra_feature3(Qid, Aid, df, fname):
    ff = df.loc[pd.to_numeric(df['Question.ID'])==Qid, :]
    f1 = ff.copy()
    f1[fname]=(ff['Answer.ID']==Aid).astype('int')
    return(f1.iloc[:,[0,3]])

#ff = hs.loc[(pd.to_numeric(hs['Question.ID'])==2) & hs['Answer.ID']=='3',:]
#ff.shape
r1 = hra_feature(2,'3',hs,"race_african")
r2 = hra_feature(2,'4',hs,"race_asian")
r3 = hra_feature(2,'5',hs,"race_white")
r4 = hra_feature(2,'6',hs,"race_hisp")
r5 = hra_feature(2,'7',hs,"race_native")
flist = [r1,r2,r3,r4,r5]
r = reduce(lambda left,right: pd.merge(left,right,on=['Encrypted.Member.ID'],how='outer'), flist).drop_duplicates()
#r.shape


df = hs[hs['Question.ID']=='840']
df['smoking']=(df['Answer.ID']=='3558').astype('int')
df.shape
df1 = hs[hs['Question.ID']=='80']
df2 = df1.copy()
df2['quit_attempt']=1-(df1['Answer.ID']=='299').astype('int')
df2['quit_current']=(df1['Answer.ID']=='295').astype('int')
df2.shape

smk = pd.merge(df.iloc[:,[0,3]], df2.iloc[:,[0,3,4]], how='outer', on=['Encrypted.Member.ID'])


qo = ['83','84','85','827','86','831','87','832','49','129','837','838']
qlo = qlist[qlist['Question.ID'].isin(qo)].iloc[:,[1,2,3]]
ho = hs[hs['Question.ID'].isin(qo)]

hq = pd.merge(ho, qlo, how='left', on=['Question.ID','Answer.ID'])


o1 = hra_feature2(83,hq,"diet_fruitveg")
o2 = hra_feature2(84,hq,"diet_fiber")
o3 = hra_feature2(85,hq,"diet_highfat")
o4 = hra_feature2(827,hq,"diet_redmeat")
o5 = hra_feature2(86,hq,"aero_perwk")
o6 = hra_feature2(831,hq,"aero_duration")
o7 = hra_feature2(87,hq,"strength_perwk")
o8 = hra_feature2(832,hq,"strength_duration")
o9 = hra_feature2(49,hq,"pros_hs")
o10 = hra_feature2(129,hq,"sr-goal-weight")
o11 = hra_feature2(837,hq,"sr-goal-ex")
o12 = hra_feature2(838,hq,"sr-goal-diet")

flist = [o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12]
o = reduce(lambda left,right: pd.merge(left,right,on=['Encrypted.Member.ID'],how='outer'), flist).drop_duplicates()
#o.shape

t1 = hra_feature3(3000,'4087',hs,"mh_osteo")
t2 = hra_feature3(1900,'4076',hs,"mh_ihd")
t3 = hra_feature3(2100,'4078',hs,"mh_diabetes")
t4 = hra_feature3(2200,'4079',hs,"mh_diabetes")
t5 = hra_feature3(2300,'4080',hs,"mh_gdiabetes")
t6 = hra_feature3(2600,'4083',hs,"mh_htn")
t7 = hra_feature3(2000,'4077',hs,"mh_depression")
t8 = hra_feature3(2700,'4084',hs,"mh_ckd")

t34 = pd.concat([t3,t4]).drop_duplicates()
#t34.shape
t34 = t34.groupby(['Encrypted.Member.ID'],as_index=False).agg({'mh_diabetes':'max'})
#t34.shape


flist = [t1,t2,t34,t5,t6,t7,t8]
t = reduce(lambda left,right: pd.merge(left,right,on=['Encrypted.Member.ID'],how='outer'), flist).drop_duplicates()
#t.shape


#fill-in question ID=147,149,150
qid = ['147','149','150']
hf = hra2[hra2['Question.ID'].isin(qid)]
#hf.shape


b1 = hf.ix[hf['Question.ID']=='147',[0,2]]
b1.columns.values[1]="height"
b2 = hf.ix[hf['Question.ID']=='149',[0,2]]
b2.columns.values[1]="weight"
b3 = hf.ix[hf['Question.ID']=='150',[0,2]]
b3.columns.values[1]="bmi"

b3['overweight'] = ((b3['bmi']>=25) & (b3['bmi']<30)).astype('int')
b3['obese'] = (b3['bmi']>=30).astype('int')


flist = [b1,b2,b3]
b = reduce(lambda left,right: pd.merge(left,right,on=['Encrypted.Member.ID'],how='outer'), flist).drop_duplicates()
#b.shape
#list(b)

flist = [r,smk,o,t,b]
fhra = reduce(lambda left,right: pd.merge(left,right,on=['Encrypted.Member.ID'],how='outer'), flist).drop_duplicates()
#fhra.shape


#age
#f = "HRA_Y16/HRA_age_status_Y16.csv"
f = "data/HRA_age_status_Y16.csv"
hage = pd.read_csv(f,dtype=object).iloc[:,[0,2]]
hage = hage[hage['Encrypted.Member.ID'].isin(el)]
hage['age']=pd.to_numeric(hage['Age'])
hage=hage.drop(['Age'],1)
#hage.shape
tmp = pd.merge(e,hage,on=['Encrypted.Member.ID'],how='right')
#tmp.shape

fhra = pd.merge(tmp,fhra,on=['Encrypted.Member.ID'],how='left')
#fhra.shape
#list(fhra)
fhra.to_csv('data/fhra16.csv', index=False, encoding='utf-8')
#fhra1 = pd.read_csv('data/fhra16.csv',dtype=object)
#fhra1.shape
####################################################

#apply the total merging logic
#age
t1 = flab.loc[:,['Encrypted.Member.ID','age']]
t2 = fhra.loc[:,['Encrypted.Member.ID','age']]
t3 = fclm.loc[:,['Encrypted.Member.ID','age']]
flist = [t1,t2,t3]
t = reduce(lambda left,right: pd.merge(left,right,on=['Encrypted.Member.ID'],how='outer'), flist)
t.loc[pd.isnull(t['age']),'age']=-1
#t.shape

tt = t.groupby(['Encrypted.Member.ID'],as_index=False).agg({'age':'max'})
tt.loc[tt['age'] == -1,'age'] = np.NaN
#tt.shape


#gender
t1 = flab.loc[:,['Encrypted.Member.ID','gender']]
t2 = fclm.loc[:,['Encrypted.Member.ID','gender']]
tt1 = pd.merge(t1,t2,on=['Encrypted.Member.ID','gender'],how='outer')
#tt1.shape

#hra features
hl = ['Encrypted.Member.ID',"race_african","race_asian","race_white","race_hisp","race_native",
"diet_fruitveg","diet_fiber","diet_highfat","diet_redmeat","aero_perwk",
"aero_duration","strength_perwk","strength_duration",'pros_hs','sr-goal-weight','sr-goal-ex','sr-goal-diet']
tt2 =  fhra.loc[:,hl]
#tt2.shape

#list(flab)

#smoking
s1 = fhra[fhra.smoking.isin([0,1])].loc[:,['Encrypted.Member.ID',"smoking"]]
s2 = fclm[fclm.smoking.isin([0,1])].loc[:,['Encrypted.Member.ID',"smoking"]]
s = pd.merge(s1,s2,on=['Encrypted.Member.ID','smoking'],how='outer')
tt31 = s.groupby(['Encrypted.Member.ID'],as_index=False).agg({'smoking':'max'}).drop_duplicates()
#tt31.shape

#quitt
q1 = fhra[(fhra.quit_current.isin([0,1])) | (fhra.quit_attempt.isin([0,1]))].loc[:,['Encrypted.Member.ID',"quit_attempt","quit_current"]]
q2 = fclm[fclm.quit_current.isin([0,1])].loc[:,['Encrypted.Member.ID',"quit_attempt","quit_current"]]
q = pd.merge(q1,q2,on=['Encrypted.Member.ID',"quit_attempt","quit_current"],how='outer')
tt32 = q.groupby(['Encrypted.Member.ID'],as_index=False).agg({'quit_attempt':'max','quit_current':'max'}).drop_duplicates()
tt32.shape


#weight height bmi
t1 = flab.loc[flab['bmi']>0,['Encrypted.Member.ID',"height","weight","bmi","overweight","obese"]]
t2 = fhra.loc[fhra['bmi']>0,['Encrypted.Member.ID',"height","weight","bmi","overweight","obese"]]
t3 = fclm.loc[((fclm['overweight']==1) | (fclm['obese']==1)),['Encrypted.Member.ID',"overweight","obese"]]
#t3.shape

#lab is over hra
t12 = t2[(~t2['Encrypted.Member.ID'].isin(t1['Encrypted.Member.ID']))]
#t12.shape
type(t1)
type(t12)

t112 = pd.DataFrame(pd.concat([t1,t12])).drop_duplicates()
t112.shape

t123 = t3[(~t3['Encrypted.Member.ID'].isin(t112['Encrypted.Member.ID']))]
tt4=pd.merge(t112,t123,on=list(t123),how='outer').drop_duplicates()
#tt4.shape

tt42=flab.loc[:,['Encrypted.Member.ID',"wc","large_wc"]]
#tt42.shape


#claim features
cl = ['Encrypted.Member.ID',"alcohol_depend","alcohol_treat","mh_mlsk","mh_avd_others","mh_mental_other","fh_chd","fh_diabetes","fh_breastc","fh_colonc","fh_prostatec"]
tt5 = fclm.loc[:,cl]
tt5.shape


#other features
fncomm = ["Encrypted.Member.ID","mh_osteo", "mh_ihd","mh_diabetes","mh_gdiabetes","mh_htn","mh_depression","mh_ckd"]
d1 = fhra.loc[:,fncomm]
#d1.shape
d1 = d1[d1['mh_osteo'].notnull()]
#d1.shape


d2 = fclm.loc[:,fncomm]
d2.shape

#d = pd.DataFrame(pd.concat([d1,d2])).drop_duplicates()
#d.shape

d = pd.merge(d1,d2,on=fncomm,how='outer').drop_duplicates()
#d.shape
d = d.fillna(0)

#fncomm.remove("Encrypted.Member.ID")
#del fncomm[-1]
tt6 = d.groupby(['Encrypted.Member.ID'],as_index=False)[fncomm[1:len(fncomm)]].max()
#tt6.shape


#combine all
flist = [tt,tt1,tt2,tt31,tt32,tt4,tt42,tt5,tt6]


tta = reduce(lambda left,right: pd.merge(left,right,on=['Encrypted.Member.ID'],how='outer'), flist).drop_duplicates()
tta = pd.merge(e,tta,on=['Encrypted.Member.ID'],how='right')
#tta.shape
#list(tta)
tta.to_csv('data/f16.csv', index=False, encoding='utf-8')

#f16 = pd.read_csv('data/f16.csv',dtype=object)
#f16.shape

tta2 = tta[tta['Relationship']=="Employee"]
#tta2.shape
#list(tta2)
tta2.to_csv('data/f16_employee.csv', index=False, encoding='utf-8')

#relationship & year
#writer = ExcelWriter('f16.xlsx')
#ff.to_excel(writer,'demo_lifestyle')
#writer.save()


#
#db0.merge(pd.DataFrame(data = [smk.values] * len(smk), columns = smk.index), left_index=True, right_index=True)




f = "data/HRA_question_Y15.csv"
qlist = pd.read_csv(f,dtype=object)
qo = ['83','84','85','827','86','831','87','832']
qlo = qlist[qlist['Question.ID'].isin(qo)]
qlo = qlist.loc[qlist['Question.ID'].isin(qo),:]
qlo = qlist[qlist['Question.ID'].isin(qo)].iloc[:,[1,2,3]]

n = ndcm.iloc[:,3].str.contains('NICOTINE', case=False, na=False)
nquit = ndcm.ix[n==True,'NDC']

nquit = ndcm[n==True].iloc[:,[0,3]]
nquit[:5]

ndcquit = nquit['NDC'].tolist()
quitd = ndcm[ndcm['NDC'].isin(ndcquit)]['Dosage']


a=[0,3,21]+range(29,45)
inp = pd.read_csv('data/inpt_claims_y13-16.csv', dtype=object).ix[:,a]
list(inp)

inp = inp.drop_duplicates()
inp = inp.loc[(inp['Admission.Date']=="2014"),]

el = e['Encrypted Member ID'].tolist()
inp = inp[inp['Encrypted.Member.ID'].isin(el)]

import re
str="7456.67"
re.sub("\.","",str)
ck = pd.DataFrame(inp['ICD.9.Procedure.3'])
ck[:6]

n = inp['ICD.9.Procedure.3'].str.contains('\.', na=False)
n1 = inp.ix[n==True,]
n1
b = inp['ICD.9.Procedure.3'].str.replace('\.','')
b[n==True]
#inp.ix[0:5,:]

#type(inp)
#inp.shape
type(inp)
for y in inp.columns:
    print inp[y].dtype

inp.shape
df = inp.ix[:,range(4,10)]
list(df)
for col in df
    print inp[col].index
    
df[:6]

#ck = df['Secondary.Diagnosis.2'].str.contains("^43", na=False) 
#ck[:6]
mask = np.column_stack([df[col].str.contains("^58", na=False) for col in df])
mask[:6]
k = inp.loc[mask.any(axis=1),'Encrypted.Member.ID']
dd = inp.ix[:,0]
k = dd.loc[mask.any(axis=1)]
k1 =k
k2 =k
k3 = pd.DataFrame(pd.concat([k1,k2]).drop_duplicates())
k3['smoking']=1
dt = inp.ix[:,range(14,18)]
db = dt.copy()
dt[n==True]
for col in dt:
    db[col]=dt[col].str.replace('\.','').copy()

db[n==True]

inp0 = data.frame(sapply(4:19, function(x) gsub("\\.","",inp[,x])))
inp0 <- data.frame(lapply(inp0, as.character), stringsAsFactors=FALSE)
inp0 = cbind(inp[,1,drop=F],inp0)



#DX
dx = outp.iloc[,range(30,40)].drop_duplicates()

#for col in dx:
#    m = re.match(r'\S+',dx[col]) #m = re.search(r'^\S+',dx[col])
#    if m:
#        t1[col]=m.group(0)
#    else:
#        t1[col]=""

t1 = dx.copy()
t2 = dx.copy()

for col in dx:
    t1[col]=dx[col].str.split().str.get(0).copy()
    #t1[col]=dx[col].str.extract('(^\S+).*').str.strip() #print pd.show_versions() works for pandas 0.18.0
    
#df.loc[df.make == '', 'make'] = df.id.str.split().str.get(0)


#convert column to string
df['movie_title'] = df['movie_title'].astype(str)

#but it remove numbers in names of movies too
df['titles'] = df['movie_title'].str.extract('([a-zA-Z ]+)', expand=False).str.strip()
df['titles1'] = df['movie_title'].str.split('(', 1).str[0].str.strip()
df['titles2'] = df['movie_title'].str.replace(r'\([^)]*\)', '').str.strip()
print df
          movie_title      titles      titles1      titles2
0  Toy Story 2 (1995)   Toy Story  Toy Story 2  Toy Story 2
1    GoldenEye (1995)   GoldenEye    GoldenEye    GoldenEye
2   Four Rooms (1995)  Four Rooms   Four Rooms   Four Rooms
3   Get Shorty (1995)  Get Shorty   Get Shorty   Get Shorty
4      Copycat (1995)     Copycat      Copycat      Copycat




for col in dx:
    t2[col]=dx[col].str.replace(r'^\S+\s*','').copy()
    

s1 = "  test a single line"
df = pd.DataFrame(np.nan, index=[0], columns=['make'])
df.loc[:,'make'] = s1

dt = df.copy()
df['make'].str.split().str.get(0)

df['make'].str.replace(r'^\s*\S+\s*','')
b = df['make'].str.lstrip()

re.sub(r'^\S+\s*','',s1.lstrip())
re.sub(r'^\s*\S+\s*','',s1)


dt.loc[:,'make']="  test    a single string"
dt['make'].str.lstrip().str.replace(r'^\S*\s*','')



m = re.match(r'\s*\S*'," this is a test")
m.group(0)

#re.sub(r"[^0-9]+", "", dx[col]).copy()
   
  
 #test counts
 list(e)
e2 = e
e2 = e2.rename(columns={'year':'year'})
qf = e2.groupby(['Encrypted Member ID','Relationship','year']).size().reset_index(name="freq")
qf = e2.groupby(['Encrypted Member ID','Relationship'])["year"].count().reset_index(name="freq")
qf = e2.groupby(['Encrypted Member ID','Relationship'],as_index=False).agg({"year": "count"})


	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	