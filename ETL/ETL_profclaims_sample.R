# An ETL example to process the professional claims

options(width=200)
library(plyr)
#library(dplyr)
src="/watson/quest_eh/data/original_files/2017MAY04/"
dst="/watson/quest_eh/workspace/lcao/data/"


f1 = "professional.claims.2013.csv"
pc1 = read.csv(paste(src, f1, sep=""), header=T, stringsAsFactors = FALSE)

k1 = unique(pc1[,c(1,5,25)])
dx1 = unique(pc1[,c(38,40:49)])
cpt1 = unique(pc1[,c(50:51)])

pc1 = unique(pc1[,c(1:3,6:8,10,12:15,17,20:24,26:54,56,58:79,82:95)])


src2="/watson/quest_eh/data/original_files/2017MAY17/Added_files_5.11.2017/"
f2 = "professional.claims.2014.csv"
pc2 = read.csv(paste(src2, f2, sep=""), header=T, stringsAsFactors = FALSE)


k2 = unique(pc2[,c(1,5,25)])
dx2 = unique(pc2[,c(38,40:49)])
cpt2 = unique(pc2[,c(50:51)])

pc2 = unique(pc2[,c(1:3,6:8,10,12:15,17,20:24,26:54,56,58:79,82:95)])

f3 = "professional.claims.2015.csv"
pc3 = read.csv(paste(src2, f3, sep=""), header=T, stringsAsFactors = FALSE)


k3 = unique(pc3[,c(1,5,25)])
dx3 = unique(pc3[,c(38,40:49)])
cpt3 = unique(pc3[,c(50:51)])

pc3 = unique(pc3[,c(1:3,6:8,10,12:15,17,20:24,26:54,56,58:79,82:95)])

f4 = "professional.claims.2016.csv"
pc4 = read.csv(paste(src2, f4, sep=""), header=T, stringsAsFactors = FALSE)


k4 = unique(pc4[,c(1,5,25)])
dx4 = unique(pc4[,c(38,40:49)])
cpt4 = unique(pc4[,c(50:51)])

pc4 = unique(pc4[,c(1:3,6:8,10,12:15,17,20:24,26:54,56,58:79,82:95)])



src3="/watson/quest_eh/data/original_files/2017JUN05/UpdatedTable2-AND-Q42016data/"
f = "ProfessionalClaims.2016Q4.incurred.csv"
library(data.table)
pcq4 = fread(paste(src3, f, sep=""), header=T)
pcq4 = data.frame(pcq4)
kq4 = unique(pcq4[,c(1,5,25)])
dxq4 = unique(pcq4[,c(38,40:49)])
cptq4 = unique(pcq4[,c(50:51)])

pcq4 = unique(pcq4[,c(1:3,6:8,10,12:15,17,20:24,26:54,56,58:79,82:95)])

pc = unique(rbind(pc1,pc2,pc3,pc4,pcq4))




pcold = unique(rbind(pc1,pc2,pc3,pc4))

nrow(pc1)
nrow(pc2)
nrow(pc3)
nrow(pc4)
nrow(pcq4)
nrow(pc)


> nrow(pc1)
[1] 792690
> nrow(pc2)
[1] 797853
> nrow(pc3)
[1] 802588
> nrow(pc4)
[1] 754873
> nrow(pcq4)
[1] 198211

> nrow(pc)
[1] 3286155



#demo gender race
k = unique(rbind(k1,k2,k3,k4))
> nrow(k)
[1] 95498
> length(unique(k[,1]))
[1] 93890
> nrow(unique(k[,c(1,2)]))
[1] 93890


#k = unique(pc[,c(1,5,25)])

m = count(k[,c(1,2)])
t1 = m[m$freq==1,]
k1 = k[k$Encrypted.Member.ID %in% t1$Encrypted.Member.ID,]

t2 = m[m$freq>1,]
k2 = k[k$Encrypted.Member.ID %in% t2$Encrypted.Member.ID,]
> nrow(k2)
[1] 3216
k2 = k2[order(k2$Encrypted.Member.ID),]
k2 = k2[k2$Ethnicity!="N/A",]
> nrow(k2)
[1] 1608

> nrow(k1)
[1] 92282

nrow(k1)+nrow(k2)
k = rbind(k1,k2)
k = k[order(k$Encrypted.Member.ID),]
> nrow(k)
[1] 93890

write.csv(k, paste(dst, "prof_claims_member_gender_race.csv", sep=""), row.names=FALSE)


f = "prof_0520Y17/prof_claims_member_gender_race.csv"
mem = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
k0 = k[!(k$Encrypted.Member.ID %in% unique(mem$Encrypted.Member.ID)), ]
mem0 = rbind(mem, k0)
write.csv(mem0, paste(dst, "prof_claims_member_gender_race.csv", sep=""), row.names=FALSE)




#DX
dx=unique(rbind(dx1,dx2,dx3,dx4))
rm(dx1,dx2,dx3,dx4)

#dx = unique(pc[,c(38,40:49)])
t1 = data.frame(sapply(2:11, function(x) gsub("(^\\S+).*", "\\1", dx[, x])))
names(t1)=names(dx)[-1]

t2 = data.frame(sapply(2:11, function(x) gsub("^\\S+\\s*", "", dx[, x]))) 
names(t2) = paste0(names(dx)[-1],".Detail")


d = cbind(dx[,1,drop=F], t1, t2)
dx9 = d[d$Submitted.ICD.Code.Version=="ICD-9",c(2:6,12:16)]



dx90 = dx9[,c(1,6)]
names(dx90)=c("ICD9DX","ICD9DX.Detail")

dx91 = dx9[,c(2,7)]
names(dx91)=c("ICD9DX","ICD9DX.Detail")

dx92 = dx9[,c(3,8)]
names(dx92)=c("ICD9DX","ICD9DX.Detail")

dx93 = dx9[,c(4,9)]
names(dx93)=c("ICD9DX","ICD9DX.Detail")

dx94 = dx9[,c(5,10)]
names(dx94)=c("ICD9DX","ICD9DX.Detail")

dxd9 = unique(rbind(dx90, dx91, dx92, dx93, dx94))


> nrow(dxd9)
[1] 11397
> length(unique(dxd9[,1]))
[1] 11396

> dxd9[dxd9$ICD9DX=="179",]
ICD9DX          ICD9DX.Detail
9192      179 MALIG NEOPL UTERUS NOS
248977    179                       
> which(dxd9$ICD9DX=="179" & dxd9$ICD9DX.Detail=="")
[1] 8965
> dxd91 = dxd9[-c(8965),]
> nrow(dxd91)
[1] 11396

write.csv(dxd91, paste(dst, "prof_claims_ICD9.csv", sep=""), row.names=FALSE)

dx10 = d[d$Submitted.ICD.Code.Version=="ICD-10",c(7:11,17:21)]
dx00 = dx10[,c(1,6)]
names(dx00)=c("ICD10DX","ICD10DX.Detail")

dx11 = dx10[,c(2,7)]
names(dx11)=c("ICD10DX","ICD10DX.Detail")

dx12 = dx10[,c(3,8)]
names(dx12)=c("ICD10DX","ICD10DX.Detail")

dx13 = dx10[,c(4,9)]
names(dx13)=c("ICD10DX","ICD10DX.Detail")

dx14 = dx10[,c(5,10)]
names(dx14)=c("ICD10DX","ICD10DX.Detail")

dxd1 = unique(rbind(dx00, dx11, dx12, dx13, dx14))
> nrow(dxd1)
[1] 18631
> length(unique(dxd1[,1]))
[1] 18631

write.csv(dxd1, paste(dst, "prof_claims_ICD10.csv", sep=""), row.names=FALSE)

f = "prof_0520Y17/prof_claims_ICD10.csv"
icd = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)

d0 = dxd1[!(dxd1$ICD10DX %in% unique(icd$ICD10DX)), ]
icd0 = rbind(icd, d0)

write.csv(icd0, paste(dst, "prof_claims_ICD10.csv", sep=""), row.names=FALSE)




#CPT
cpt=unique(rbind(cpt1,cpt2,cpt3,cpt4))
rm(cpt1,cpt2,cpt3,cpt4)


#cpt = unique(pc[,c(50:51)])

cm = unique(cpt[,2,drop=F])
cpt=cpt[,-c(2),drop=F]

cpt$CPT = gsub("(^\\S+).*", "\\1", cpt$Procedure.Code)
cpt$CPT.Detail = gsub("^\\S+\\s*", "", cpt$Procedure.Code)
cpt=unique(cpt[,-c(1)])
write.csv(cpt, paste(dst, "prof_claims_CPT.csv", sep=""), row.names=FALSE)


cm = unique(cptq4[,2,drop=F])
cpt=cptq4[,-c(2),drop=F]

cpt$CPT = gsub("(^\\S+).*", "\\1", cpt$Procedure.Code)
cpt$CPT.Detail = gsub("^\\S+\\s*", "", cpt$Procedure.Code)
cpt=unique(cpt[,-c(1)])


f = "prof_0520Y17/prof_claims_CPT.csv"
old = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
d0 = cpt[!(cpt$CPT %in% unique(old$CPT)), ]
cpt0 = rbind(old, d0)
write.csv(cpt0, paste(dst, "prof_claims_CPT.csv", sep=""), row.names=FALSE)

cm$CM = gsub("(^\\S+).*", "\\1", cm$Procedure.Modifier)
cm$CM.Detail = gsub("^\\S+\\s*", "", cm$Procedure.Modifier)
cm=unique(cm[,-c(1)])
write.csv(cm, paste(dst, "prof_claims_CM.csv", sep=""), row.names=FALSE)

f = "prof_0520Y17/prof_claims_CM.csv"
old = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
d0 = cm[!(cm$CM %in% unique(old$CM)), ]
[0]



pc = unique(pc[,c(1:3,6:8,10,12:15,17,20:24,26:54,56,58:79,82:95)])
t = data.frame(sapply(32:41, function(x) gsub("(^\\S+).*", "\\1", pc[, x])))
names(t)=names(pc)[32:41]
pc[,c(32:41)]=t
pc$Procedure.Code = gsub("(^\\S+).*", "\\1", pc$Procedure.Code)
pc$Procedure.Modifier = gsub("(^\\S+).*", "\\1", pc$Procedure.Modifier)
> nrow(pc)
[1] 3286155

pc = unique(pc)
nrow(pc)
> nrow(pc)
[1] 3285938




t = data.frame(sapply(32:41, function(x) gsub("(^\\S+).*", "\\1", pcq4[, x])))
names(t)=names(pcq4)[32:41]
pcq4[,c(32:41)]=t

pcq4$Procedure.Code = gsub("(^\\S+).*", "\\1", pcq4$Procedure.Code)
pcq4$Procedure.Modifier = gsub("(^\\S+).*", "\\1", pcq4$Procedure.Modifier)

#write.csv(pc, paste(dst, "prof_claims_16Q4.csv", sep=""), row.names=FALSE)


pf = "prof_0520Y17/prof_claims_y13-16.csv"
pfold = read.csv(paste(dst, pf, sep=""), header=T, stringsAsFactors = FALSE)




> pf = "prof_0520Y17/prof_claims_y13-16.csv"
> pfold = read.csv(paste(dst, pf, sep=""), header=T, stringsAsFactors = FALSE)
> nrow(pfold)
[1] 3148004
> nrow(pc)
[1] 198211
> pf0 = rbind(pfold, pc)
nrow(pf0)
> nrow(pf0)
[1] 3346215
> pf01 = unique(pf0)
nrow(pf01)
write.csv(pf01, paste(dst, "prof_claims_y13-16.csv", sep=""), row.names=FALSE)
> nrow(pf01)
[1] 3285938



#update the missing age 
b = unique(pc[pc$Age!="999" & pc$Age!="" & pc$Age!="90+",c(1,4,21)])
b$age = as.integer(b$Age)
> nrow(b)
[1] 360968


t = aggregate(b[,c(3:4)], b[,1,drop=F], FUN=max)
> nrow(t)
[1] 93471


t$age16 = as.integer(t$age) + 2016 - as.integer(t$Paid.Date)
t = t[,-c(2:3)]

  
a = pc[pc$Encrypted.Member.ID %in% t$Encrypted.Member.ID & (pc$Age=="999" | pc$Age==""),]
> nrow(a)
[1] 8334


fcom = intersect(names(t),names(a))
d = merge(x=a, y=t, by=fcom, all.x=T)
d$Age = as.character(d$age16 - (2016-as.integer(d$Paid.Date)))
#drop age16
d = d[,c(-84)]
> nrow(d)
[1] 8334

c = pc[!(pc$Encrypted.Member.ID %in% t$Encrypted.Member.ID) | (pc$Encrypted.Member.ID %in% t$Encrypted.Member.ID & pc$Age!="999" & pc$Age!=""),]
> nrow(pc)-nrow(d)
[1] 3277604
> nrow(c)
[1] 3277604


pc0 = rbind(c, d)
> nrow(pc0)
[1] 3285938

> nrow(unique(pc0))
[1] 3285938

write.csv(pc0, paste(dst, "prof_claims_y13-16.csv", sep=""), row.names=FALSE)


# professional claims Yes/No data fields
#  "Regular.Temp" "Discretionary" "C.Section" "Network.Indicator" 


#cross check the age
> options(width=200)
> library(plyr)
> src="/watson/quest_eh/data/original_files/2017MAY04/"
> dst="/watson/quest_eh/workspace/lcao/data/"
> f1 = "lab_0517Y17/lab_member_noage.csv"
> m1 = read.csv(paste(dst, f1, sep=""), header=T, stringsAsFactors = FALSE)

p1 = "prof_0520Y17/prof_claims_y13-16.csv"
p = read.csv(paste(dst, p1, sep=""), header=T, stringsAsFactors = FALSE)

r1 = "rx_0519Y17/rx_y13-16.csv"
r = read.csv(paste(dst, r1, sep=""), header=T, stringsAsFactors = FALSE)

