# An ETL example to process outpatient data

import pandas as pd
#import numpy as np
#import re

src="/watson/quest_eh/data/original_files/2017MAY04/"
dst="/watson/quest_eh/workspace/lcao/data/"

#process outpatient claims

fo = "outpatient.claims.2013to2016.csv"
outp0 = pd.read_csv(src+fo,dtype=object)

src2="/watson/quest_eh/data/original_files/2017JUN05/UpdatedTable2-AND-Q42016data/"
f = "OutpatientClaims.2016Q4.incurred.csv"
outpq4 = pd.read_csv(src2+f,dtype=object)

outp = pd.concat([outp0,outpq4], ignore_index=True).drop_duplicates()

#create gender race mapping - Encrypted Member ID, Gender, Ethnicity, 
k = outp[outp['Ethnicity'].notnull()].iloc[:,[0,4,24]].drop_duplicates()
k.sort_values(by='Encrypted.Member.ID', ascending=True)
k.to_csv(dst+'outpt_member_gender_race.csv', index=False, encoding='utf-8')

cols=range(0,3)+range(5,8)+[9]+range(11,15)+[16]+range(19,24)+range(25,55)+range(56,78)+range(80,93)

#create the data
#remove plan relationship, plan product, benefit package, mental health carrier, external medical indicator, external pharmacy indicator, extra claim vendor
#member months, subscriber months, 
outp = outp.iloc[:,cols].drop_duplicates()

#DX mapping
dx = outp.iloc[:,range(30,40)]

t1 = dx.copy()
t2 = dx.copy()
for col in dx:
    t1[col]=dx[col].str.split().str.get(0)

for col in dx:
    t2[col]=dx[col].str.replace(r'^\s*\S+\s*','')

t2.columns=dx.columns+".Detail"
   
d = pd.concat([outp.loc[:,['Submitted.ICD.Code.Version']],t1, t2], axis=1)
    

dx9 = d.iloc[d['Submitted.ICD.Code.Version']=="ICD-9",range(1,6)+range(11,16)]
dxd9 = pd.DataFrame()
for i in range(0,5):
    dx1 = dx9.iloc[:,[i,i+5]]
    dx1.columns=["ICD9DX","ICD9DX.Detail"]
    dxd9 = pd.concat([dxd9,dx1]).drop_duplicates()
dxd9.to_csv(dst+"outpt_claims_ICD9.csv", index=False, encoding='utf-8')

dx10 = d.iloc[d['Submitted.ICD.Code.Version']=="ICD-10",range(6,11)+range(16,21)]
dxd10 = pd.DataFrame()
for i in range(0,5):
    dx1 = dx10.iloc[:,[i,i+5]]
    dx1.columns=["ICD10DX","ICD10DX.Detail"]
    dxd10 = pd.concat([dxd10,dx1]).drop_duplicates()
dxd10.to_csv(dst+"outpt_claims_ICD10.csv", index=False, encoding='utf-8')

outp.iloc[:,range(30,40)]=t1

#PCS mapping
pc = outp.iloc[:,range(40,46)]

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
p1.to_csv(dst+"outpt_ICD9_PCS.csv", index=False, encoding='utf-8')

p2 = pd.DataFrame()
for i in range(3,6):
    pcs = p.iloc[:,[i,i+6]]
    pcs.columns=["ICD10PCS","ICD10PCS.Detail"]
    p2 = pd.concat([p2,pcs]).drop_duplicates()
p2.to_csv(dst+"outpt_ICD10_PCS.csv", index=False, encoding='utf-8')

outp.iloc[:,range(40,46)]=r1

outp.to_csv(dst+"outpt_claims_y13-16.csv", index=False, encoding='utf-8')


#optional - update the missing age 
hage = outp.iloc[((outp.Age!="999") & (outp.Age!="") & (outp.Age!="90+")),[0,3,20]]
hage['age'] = pd.to_numeric(hage.Age)
refage = hage.groupby(['Encrypted.Member.ID'],as_index=False).agg({'Paid.Date':'max','age':'max'})
refage['age16'] = refage.age + 2016 - pd.to_numeric(refage.Paid.Date)
refg=refage[['Encrypted.Member.ID','age16']]

amem = refg['Encrypted.Member.ID'].tolist()

noage = outp[((outp.Age=="999") | (outp.Age=="")) & (outp['Encrypted.Member.ID'].isin(amem))]
noage = pd.merge(noage,refg,on=['Encrypted.Member.ID'],how='left')

noage['Age'] = (noage['age16'] - 2016 + pd.to_numeric(noage['Paid.Date'])).astype(str)
noage=noage.drop(['age16'],1)

withage = outp[(((outp.Age!="999") & (outp.Age!="") & (outp['Encrypted.Member.ID'].isin(amem))) | (~outp['Encrypted.Member.ID'].isin(amem)))]

outp1 = pd.concat([withage, noage]).drop_duplicates()
outp1.to_csv(dst+"outpt_claims_y13-16.csv", index=False, encoding='utf-8')

