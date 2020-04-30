# An ETL example to process inpatient data

import pandas as pd
#import numpy as np
#import re

src="/watson/quest_eh/data/original_files/2017MAY04/"
dst="/watson/quest_eh/workspace/lcao/data/"

#process inpatient claims

fo = "inpatient.claims.2013to2016.csv"
inp0 = pd.read_csv(src+fo,dtype=object)

src2="/watson/quest_eh/data/original_files/2017JUN05/UpdatedTable2-AND-Q42016data/"
f = "InpatientClaims.2016Q4.incurred.csv"
inpq4 = pd.read_csv(src2+f,dtype=object)

inp = pd.concat([inp0,inpq4], ignore_index=True).drop_duplicates()

#create gender race mapping - Encrypted Member ID, Gender, Ethnicity, 
k = inp[inp['Ethnicity'].notnull()].iloc[:,[0,4,24]].drop_duplicates()
k.sort_values(by='Encrypted.Member.ID', ascending=True)
k.to_csv(dst+'inpt_member_gender_race.csv', index=False, encoding='utf-8')


#DRG mapping
drg = inp.loc[:,['DRG']]

d1 = drg.copy()
d2 = drg.copy()
for col in drg:
    d1[col]=drg[col].str.split().str.get(0)

for col in drg:
    d2[col]=drg[col].str.replace(r'^\s*\S+\s*','')

d2.columns.values[0]="DRG.Detail"
inp.loc[:,'DRG']=d1
drgm = pd.concat([d1,d2],axis=1).drop_duplicates()
drgm.to_csv(dst+"inpt_DRG_total.csv", index=False, encoding='utf-8')

#DX mapping
dx = inp.iloc[:,range(28,39)]

t1 = dx.copy()
t2 = dx.copy()
for col in dx:
    t1[col]=dx[col].str.split().str.get(0)

for col in dx:
    t2[col]=dx[col].str.replace(r'^\s*\S+\s*','')

t2.columns=dx.columns+".Detail"
   
d = pd.concat([inp.loc[:,['Submitted.ICD.Code.Version']],t1, t2], axis=1)
    

dx9 = d.iloc[d['Submitted.ICD.Code.Version']=="ICD-9",range(1,6)+range(11,16)]
dxd9 = pd.DataFrame()
for i in range(0,5):
    dx1 = dx9.iloc[:,[i,i+5]]
    dx1.columns=["ICD9DX","ICD9DX.Detail"]
    dxd9 = pd.concat([dxd9,dx1]).drop_duplicates()
dxd9.to_csv(dst+"inpt_claims_ICD9.csv", index=False, encoding='utf-8')

dx10 = d.iloc[d['Submitted.ICD.Code.Version']=="ICD-10",range(6,11)+range(16,21)]
dxd10 = pd.DataFrame()
for i in range(0,5):
    dx1 = dx10.iloc[:,[i,i+5]]
    dx1.columns=["ICD10DX","ICD10DX.Detail"]
    dxd10 = pd.concat([dxd10,dx1]).drop_duplicates()
dxd10.to_csv(dst+"inpt_claims_ICD10.csv", index=False, encoding='utf-8')

inp.iloc[:,range(28,39)]=t1

#PCS mapping
pc = inp.iloc[:,range(39,45)]

r1 = pc.copy()
r2 = pc.copy()
for col in pc:
    r1[col]=pc[col].str.split().str.get(0)

for col in pc:
    r2[col]=pc[col].str.replace(r'^\s*\S+\s*','')

r2.columns=pc.columns+".Detail"

p = pd.concat([r1,r2],axis=1)

p1 = pd.DataFrame()
for i in range(0,3):
    pcs = p.iloc[:,[i,i+6]]
    pcs.columns=["ICD9PCS","ICD9PCS.Detail"]
    p1 = pd.concat([p1,pcs]).drop_duplicates()
p1.to_csv(dst+"inpt_ICD9_PCS.csv", index=False, encoding='utf-8')

p2 = pd.DataFrame()
for i in range(3,6):
    pcs = p.iloc[:,[i,i+6]]
    pcs.columns=["ICD10PCS","ICD10PCS.Detail"]
    p2 = pd.concat([p2,pcs]).drop_duplicates()
p2.to_csv(dst+"inpt_ICD10_PCS.csv", index=False, encoding='utf-8')

inp.iloc[:,range(39,45)]=r1

cols=range(0,3)+range(5,8)+[9]+range(11,15)+[16]+range(19,24)+range(25,57)+range(58,81)+range(83,100)

#create the data
#remove plan relationship, plan product, benefit package, mental health carrier, external medical indicator, external pharmacy indicator, extra claim vendor
#member months, subscriber months, 
inp = inp.iloc[:,cols].drop_duplicates()

inp.to_csv(dst+"inpt_claims_y13-16.csv", index=False, encoding='utf-8')


#optional - update the missing age 
hage = inp.iloc[((inp.Age!="999") & (inp.Age!="") & (inp.Age!="90+")),[0,3,20]]
hage['age'] = pd.to_numeric(hage.Age)
refage = hage.groupby(['Encrypted.Member.ID'],as_index=False).agg({'Paid.Date':'max','age':'max'})
refage['age16'] = refage.age + 2016 - pd.to_numeric(refage.Paid.Date)
refg=refage[['Encrypted.Member.ID','age16']]

amem = refg['Encrypted.Member.ID'].tolist()

noage = inp[((inp.Age=="999") | (inp.Age=="")) & (inp['Encrypted.Member.ID'].isin(amem))]
noage = pd.merge(noage,refg,on=['Encrypted.Member.ID'],how='left')

noage['Age'] = (noage['age16'] - 2016 + pd.to_numeric(noage['Paid.Date'])).astype(str)
noage=noage.drop(['age16'],1)

withage = inp[(((inp.Age!="999") & (inp.Age!="") & (inp['Encrypted.Member.ID'].isin(amem))) | (~inp['Encrypted.Member.ID'].isin(amem)))]

inp1 = pd.concat([withage, noage]).drop_duplicates()
inp1.to_csv(dst+"inpt_claims_y13-16.csv", index=False, encoding='utf-8')

