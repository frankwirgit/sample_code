# An ETL example to process drugs/RX data

import pandas as pd
#import numpy as np
#import re

src="/watson/quest_eh/data/original_files/2017MAY04/"
dst="/watson/quest_eh/workspace/lcao/data/"

#process professional claims

fo = "pharmacy.claims.2014and2016.csv"
rx1 = pd.read_csv(src+fo,dtype=object)

fo = "pharmacy.claims.2013and2015.csv"
rx2 = pd.read_csv(src+fo,dtype=object)

src2="/watson/quest_eh/data/original_files/2017JUN05/UpdatedTable2-AND-Q42016data/"
f = "PharmacyClaims.2016Q4.incurred.csv"
rxq4 = pd.read_csv(src2+f,dtype=object)

rx = pd.concat([rx1,rx2,rxq4], ignore_index=True).drop_duplicates()

#create gender race mapping - Encrypted Member ID, Gender, Ethnicity, 
k = rx[rx['Ethnicity'].notnull()].iloc[:,[0,4,24]].drop_duplicates()
k.sort_values(by='Encrypted.Member.ID', ascending=True)
k.to_csv(dst+'rx_member_gender_race.csv', index=False, encoding='utf-8')

#create the data
#remove plan relationship, plan product, benefit package, mental health carrier, external medical indicator, external pharmacy indicator, extra claim vendor
#member months, subscriber months, 

#NDC mapping
ndc = rx.iloc[:,(range(30,35)+[37,41])].drop_duplicates()
ndc.iloc[:,'NDC']=ndc['NDC'].str.split().str.get(0)

rx.iloc[:,'NDC']=ndc['NDC']

ndc = ndc.drop_duplicates()
ndc.to_csv(dst+"rx_NDC.csv", index=False, encoding='utf-8')



#DAW mapping
daw = rx.ix[:,[36]]

r1 = daw.copy()
r2 = dx.copy()
for col in cpt:
    r1[col]=cpt[col].str.split().str.get(0)

for col in cpt:
    r2[col]=cpt[col].str.replace(r'^\s*\S+\s*','')

r2.columns=cpt.columns+".Detail"

p1 = pd.concat([r1,r2,axis=1).drop_duplicates()
p1.to_csv(dst+"rx_DAW.csv", index=False, encoding='utf-8')


rx.iloc[:,[36]]=r1
rx.to_csv(dst+"rx_y13-16.csv", index=False, encoding='utf-8')


#extract the related columns
cols=range(0,3)+range(5,8)+[9]+range(11,15)+[16]+range(19,24)+range(25,31)+range(35,37)+range(38,40)+[45]+range(46,67)+[68,71]+range(74,87)
rx = rx.iloc[:,cols].drop_duplicates()



#optional - update the missing age 
hage = rx.iloc[((rx.Age!="999") & (rx.Age!="") & (rx.Age!="90+")),[0,3,20]]
hage['age'] = pd.to_numeric(hage.Age)
refage = hage.groupby(['Encrypted.Member.ID'],as_index=False).agg({'Paid.Date':'max','age':'max'})
refage['age16'] = refage.age + 2016 - pd.to_numeric(refage.Paid.Date)
refg=refage[['Encrypted.Member.ID','age16']]

amem = refg['Encrypted.Member.ID'].tolist()

noage = rx[((rx.Age=="999") | (rx.Age=="")) & (rx['Encrypted.Member.ID'].isin(amem))]
noage = pd.merge(noage,refg,on=['Encrypted.Member.ID'],how='left')

noage['Age'] = (noage['age16'] - 2016 + pd.to_numeric(noage['Paid.Date'])).astype(str)
noage=noage.drop(['age16'],1)

withage = rx[(((rx.Age!="999") & (rx.Age!="") & (rx['Encrypted.Member.ID'].isin(amem))) | (~rx['Encrypted.Member.ID'].isin(amem)))]

rx1 = pd.concat([withage, noage]).drop_duplicates()
rx1.to_csv(dst+"rx_y13-16.csv", index=False, encoding='utf-8')

