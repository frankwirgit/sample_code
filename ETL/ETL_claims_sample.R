# An ETL example to process claims

options(width=200)
library(plyr)
src="/watson/quest_eh/data/original_files/2017MAY04/"
dst="/watson/quest_eh/workspace/lcao/data/"

#set the year
yr="2014"
fyr="14"
#eligibility
f = "2013-2016EligibilityFiles/"
e = read.csv(paste(src, f, yr, ".Eligibility.csv", sep=""), header=T, stringsAsFactors = FALSE)[,c(1,2,7)]
names(e)[3]="year"
#nrow(e)

#claims

#"Encrypted.Member.ID","Age","Admission.Date",ICD9, ICD10 dx and procedure code & CPT


f = "inpt_0518Y17/inpt_claims_y13-16.csv"
inp = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE,colClasses=("ICD.9.Procedure.3"="character"))[,c(1,4,22,30:45)]
#> nrow(inp)
inp = unique(inp)
#> nrow(inp)
inp = inp[inp$Admission.Date==yr,]
#> nrow(inp)
inp=inp[inp$Encrypted.Member.ID %in% e$Encrypted.Member.ID,]
#> nrow(inp)


inp0 = data.frame(sapply(4:19, function(x) gsub("\\.","",inp[,x])))
inp0 <- data.frame(lapply(inp0, as.character), stringsAsFactors=FALSE)
inp0 = cbind(inp[,1,drop=F],inp0)


f = "outpt_0531Y17/outpt_claims_y13-16.csv"
outp = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE,colClasses=("ICD.9.Procedure.2"="character"))[,c(1,4,22,31:46)]
#> nrow(outp)
outp = unique(outp)
#> nrow(outp)

outp = outp[outp$Service.Date==yr,]
#> nrow(outp)

outp=outp[outp$Encrypted.Member.ID %in% e$Encrypted.Member.ID,]
#nrow(outp)





outp0 = data.frame(sapply(4:19, function(x) gsub("\\.","",outp[,x])))
outp0 <- data.frame(lapply(outp0, as.character), stringsAsFactors=FALSE)
outp0 = cbind(outp[,1,drop=F],outp0)


f = "prof_0520Y17/prof_claims_y13-16.csv"
pfp = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)[,c(1,4,22,32:43)]
#> nrow(pfp)

pfp = unique(pfp)
#> nrow(pfp)
pfp = pfp[pfp$Service.Date==yr,]

#> nrow(pfp)
pfp=pfp[pfp$Encrypted.Member.ID %in% e$Encrypted.Member.ID,]
#> nrow(pfp)

write.csv(pfp, paste(dst, "pfp", fyr, ".csv", sep=""), row.names=FALSE)
#pfp = read.csv(paste(dst, "feature_Y",fyr,"/pfp", fyr, ".csv", sep=""), header=T, stringsAsFactors = FALSE)

pfp0 = data.frame(sapply(4:14, function(x) gsub("\\.","",pfp[,x])))
pfp0 <- data.frame(lapply(pfp0, as.character), stringsAsFactors=FALSE)
pfp0 = cbind(pfp[,1,drop=F],pfp0)



f = "rx_0519Y17/rx_y13-16.csv"
rx = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)[,c(1,4,22,23)]

#> nrow(rx)

rx = unique(rx)
#> nrow(rx)

rx = rx[rx$Service.Date==yr,]
#> nrow(rx)
rx=rx[rx$Encrypted.Member.ID %in% e$Encrypted.Member.ID,]
#> nrow(rx)


rx0 = rx

write.csv(rx0, paste(dst, "rx0", fyr, ".csv", sep=""), row.names=FALSE)
#rx0 = read.csv(paste(dst, "rx0", fyr, ".csv", sep=""), header=T, stringsAsFactors = FALSE)

f = "rx_0519Y17/NDC_Mapping.csv"
ndcm = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)



claim_feature <- function(df,dx9,mdx9,dx10,mdx10,pcs9,mpcs9,pcs10,mpcs10,cpt,mcpt,fname){
  #initial dataframe
  f9 = df[0,1,drop=F]
  f10=p9=p10=fc=f9
  
  if(dx9){
    a = unique(c(grep(mdx9,df$X1),grep(mdx9,df$X2),grep(mdx9,df$X3),grep(mdx9,df$X4),grep(mdx9,df$X5)))
    a = a[!is.na(a)]
    if(length(a)>0){
      f9 = df[a,1,drop=F]
    }
  }
  if(dx10){
    a = unique(c(grep(mdx10,df$X6),grep(mdx10,df$X7),grep(mdx10,df$X8),grep(mdx10,df$X9),grep(mdx10,df$X10)))
    a = a[!is.na(a)]
    if(length(a)>0){
      f10 = df[a,1,drop=F]
    }
  }
  if(pcs9){
    a = unique(c(grep(mpcs9,df$X11),grep(mpcfs9,df$X12),grep(mpcs9,df$X13)))
    a = a[!is.na(a)]
    if(length(a)>0){
      p9 = df[a,1,drop=F]
    }
  }
  if(pcs10){
    a = unique(c(grep(mpcs10,df$X14),grep(mpcs10,df$X15),grep(mpcs10,df$X16)))
    a = a[!is.na(a)]
    if(length(a)>0){
      p10 = df[a,1,drop=F]
    }
  }
  if(cpt){
    a = unique(c(grep(mcpt,df$X11)))
    a = a[!is.na(a)]
    if(length(a)>0){
      fc = df[a,1,drop=F]
    }
  }
  
  
  ff = unique(rbind(f9,f10,p9,p10,fc))
  if(nrow(ff)>0){
    ff$name=1
    names(ff)[2]=fname
  }
  
  return(ff)
  
}


#add professional claims
#add ICD10 Z720
ismk = claim_feature(inp0,1,"^3051|^98984",1,"^F17",0,NA,0,NA,0,NA,"smoking")
osmk = claim_feature(outp0,1,"^3051|^98984",1,"^F17",0,NA,0,NA,0,NA,"smoking")
#psmk = claim_feature(pfp0,1,"^3051|^98984",1,"^F17",0,NA,0,NA,0,NA,"smoking")
smk = unique(rbind(ismk,osmk))

#> nrow(ismk)
#> nrow(osmk)
#> nrow(psmk)
#> nrow(smk)


a = grep("NICOTINE",ndcm[,4],ignore.case = T)
nquit = ndcm[a,c(1,4)]
#nrow(nquit)


iquit = claim_feature(inp0,0,NA,0,NA,0,NA,1,"^HZ80|^HZ90",0,NA,"quit_current")
oquit = claim_feature(outp0,0,NA,0,NA,0,NA,1,"^HZ80|^HZ90",0,NA,"quit_current")
#S9075, S9453 HcPCS
pquit = claim_feature(pfp0,0,NA,0,NA,0,NA,0,NA,1,"99406|99407","quit_current")
rquit = unique(rx0[rx0$NDC %in% nquit$NDC,1,drop=F])
rquit$quit_current=1

quit = unique(rbind(iquit,oquit,pquit,rquit))
#> nrow(iquit)
#> nrow(oquit)
#> nrow(pquit)
#> nrow(rquit)
#> nrow(quit)


quit$quit_attempt=quit$quit_current
#quit$smoking=1



#merge them
fcom = intersect(names(quit),names(smk))
sqmk = merge(x=smk, y=quit, by=fcom, all=T)
sqmk[is.na(sqmk)]=0

#> nrow(sqmk)
#> nrow(quit)
#> nrow(smk)


#Q - overwieght and obese, no based bmi
iover = claim_feature(inp0,1,"^27802",1,"^E663",0,NA,0,NA,0,NA,"overweight")
oover = claim_feature(outp0,1,"^27802",1,"^E663",0,NA,0,NA,0,NA,"overweight")
over = unique(rbind(iover,oover))
#> nrow(over)
#> nrow(oover)
#> nrow(over)




iob = claim_feature(inp0,1,"^27800|^27801",1,"^E660|^E669",0,NA,0,NA,0,NA,"obese")
oob = claim_feature(outp0,1,"^27800|^27801",1,"^E660|^E669",0,NA,0,NA,0,NA,"obese")
ob = unique(rbind(iob,oob))
#> nrow(iob)
#> nrow(oob)
#> nrow(ob)


#Q: qualified for both obese and overweight
ck = intersect(ob[,1],over[,1])
#length(ck)


fcom = intersect(names(over),names(ob))
ovb = merge(x=over, y=ob, by=fcom, all=T)
ovb[which(ovb$obese==1),]$overweight=0
ovb[which(ovb$overweight==1),]$obese=0

#> nrow(ovb)
#> nrow(over)
#> nrow(ob)




ialc = claim_feature(inp0,1,"^29181|^303",1,"^F10|^Z7141",0,NA,0,NA,0,NA,"alcohol_depend")
oalc = claim_feature(outp0,1,"^29181|^303",1,"^F10|^Z7141",0,NA,0,NA,0,NA,"alcohol_depend")
#palc = claim_feature(pfp0,1,"^29181|^303",1,"^F10|^Z7141",0,NA,0,NA,0,NA,"alcohol_depend")
alc = unique(rbind(ialc,oalc))

#> nrow(ialc)
#> nrow(oalc)
#> nrow(palc)
#> nrow(alc)




ialt = claim_feature(inp0,0,NA,1,"^Z7141",1,"^9461|^9462|^9463|^9467|^9468|^9469",1,"^HZ2Z",0,NA,"alcohol_treat")
oalt = claim_feature(outp0,0,NA,1,"^Z7141",1,"^9461|^9462|^9463|^9467|^9468|^9469",1,"^HZ2Z",0,NA,"alcohol_treat")
palt = claim_feature(pfp0,0,NA,1,"^Z7141",0,NA,0,NA,1,"99408|99409","alcohol_treat")
alt = unique(rbind(ialt,oalt,palt))

#> nrow(ialt)
#> nrow(oalt)
#> nrow(palt)
#> nrow(alt)



#alt$alcohol_depend=1
fcom = intersect(names(alc),names(alt))
alct = merge(x=alc, y=alt, by=fcom, all=T)
alct[is.na(alct)]=0

#> nrow(alct)
#> nrow(alc)
#> nrow(alt)



iosteo = claim_feature(inp0,1,"^7330",1,"^M80|^M81",0,NA,0,NA,0,NA,"mh_osteo")
oosteo = claim_feature(outp0,1,"^7330",1,"^M80|^M81",0,NA,0,NA,0,NA,"mh_osteo")
osteo = unique(rbind(iosteo,oosteo))

#> nrow(iosteo)
#> nrow(oosteo)
#> nrow(osteo)

imlsk = claim_feature(inp0,1,"^7[1-3][0-9]",1,"^M[0-9][0-9]",0,NA,0,NA,0,NA,"mh_mlsk")
omlsk = claim_feature(outp0,1,"^7[1-3][0-9]",1,"^M[0-9][0-9]",0,NA,0,NA,0,NA,"mh_mlsk")
mlsk = unique(rbind(imlsk,omlsk))

#> nrow(imlsk)
#> nrow(omlsk)
#> nrow(mlsk)



iihd = claim_feature(inp0,1,"^41[0-4]",0,NA,0,NA,0,NA,0,NA,"mh_ihd")
oihd = claim_feature(outp0,1,"^41[0-4]",0,NA,0,NA,0,NA,0,NA,"mh_ihd")
ihd = unique(rbind(iihd,oihd))

#> nrow(iihd)
#> nrow(oihd)
#> nrow(ihd)



iavd = claim_feature(inp0,1,"^44[0-9]",0,NA,0,NA,0,NA,0,NA,"mh_avd_others")
oavd = claim_feature(outp0,1,"^44[0-9]",0,NA,0,NA,0,NA,0,NA,"mh_avd_others")
avd = unique(rbind(iavd,oavd))

#> nrow(iavd)
#> nrow(oavd)
#> nrow(avd)


fcom = names(sqmk)[1]
tmp1 = Reduce(function(x, y) merge(x, y, by=fcom, all=TRUE), list(sqmk,ovb,alct,osteo,mlsk,ihd,avd))
tmp1[is.na(tmp1)]=0

#> nrow(tmp1)


a = grep("diabetes",ndcm[,9],ignore.case = T)
ndiab = ndcm[a,c(1,9)]
#nrow(ndiab)


idiab= claim_feature(inp0,1,"^250",1,"^E0[8-9]|^E1[0-3]",0,NA,0,NA,0,NA,"mh_diabetes")
odiab = claim_feature(outp0,1,"^250",1,"^E0[8-9]|^E1[0-3]",0,NA,0,NA,0,NA,"mh_diabetes")
rdiab = unique(rx0[rx0$NDC %in% ndiab$NDC,1,drop=F])
rdiab$mh_diabetes=1
diab = unique(rbind(idiab,odiab,rdiab))
#> nrow(idiab)
#> nrow(odiab)
#> nrow(rdiab)
#> nrow(diab)




igdia= claim_feature(inp0,1,"^6488",1,"^O998",0,NA,0,NA,0,NA,"mh_gdiabetes")
ogdia = claim_feature(outp0,1,"^6488",1,"^O998",0,NA,0,NA,0,NA,"mh_gdiabetes")
gdia = unique(rbind(igdia,ogdia))
#> nrow(igdia)
#> nrow(ogdia)
#> nrow(gdia)



a = grep("hypertension",ndcm[,9],ignore.case = T)
nhtn = ndcm[a,c(1,9)]
#nrow(nhtn)

ihtn= claim_feature(inp0,1,"^401|^405",1,"^I1[1-6]",0,NA,0,NA,0,NA,"mh_htn")
ohtn = claim_feature(outp0,1,"^401|^405",1,"^I1[1-6]",0,NA,0,NA,0,NA,"mh_htn")
rhtn = unique(rx0[rx0$NDC %in% nhtn$NDC,1,drop=F])
rhtn$mh_htn=1
htn = unique(rbind(ihtn,ohtn,rhtn))
#> nrow(ihtn)
#> nrow(ohtn)
#> nrow(rhtn)
#> nrow(htn)



a = grep("depression",ndcm[,9],ignore.case = T)
ndep = ndcm[a,c(1,9)]
#nrow(ndep)

idep= claim_feature(inp0,1,"^2962|^2963|^311",1,"^F32|^F33",0,NA,0,NA,0,NA,"mh_depression")
odep = claim_feature(outp0,1,"^2962|^2963|^311",1,"^F32|^F33",0,NA,0,NA,0,NA,"mh_depression")
rdep = unique(rx0[rx0$NDC %in% ndep$NDC,1,drop=F])
rdep$mh_depression=1
dep = unique(rbind(idep,odep,rdep))
#> nrow(idep)
#> nrow(odep)
#> nrow(rdep)
#> nrow(dep)



#anxiety OR ADHD OR Bipolar OR Schizophrenia

a = grep("anxiety|adhd|bipolar|schizophrenia",ndcm[,9],ignore.case = T)
nmen = ndcm[a,c(1,9)]
#nrow(nmen)


pt1= "^29[^6]|^296[^2^3]|^30|^31[^1]"
pt2 = "^F0[1-9]|^F[^0^3][0-9]|^F3[^2^3]"

imen= claim_feature(inp0,1,pt1,1,pt2,0,NA,0,NA,0,NA,"mh_mental_other")
omen = claim_feature(outp0,1,pt1,1,pt2,0,NA,0,NA,0,NA,"mh_mental_other")
pmen = claim_feature(pfp0,1,pt1,1,pt2,0,NA,0,NA,0,NA,"mh_mental_other")
rmen = unique(rx0[rx0$NDC %in% nmen$NDC,1,drop=F])
rmen$mh_mental_other=1
men = unique(rbind(imen,omen,pmen,rmen))

#> nrow(imen)
#> nrow(omen)
#> nrow(pmen)
#> nrow(rmen)
#> nrow(men)



ickd = claim_feature(inp0,1,"^585|V4511",1,"^N18|^Z49",1,"^3995",0,NA,1,"^9093[5-9]|^909[4-9][0-9]","mh_ckd")
ockd = claim_feature(outp0,1,"^585|V4511",1,"^N18|^Z49",1,"^3995",0,NA,1,"^9093[5-9]|^909[4-9][0-9]","mh_ckd")
pckd = claim_feature(pfp0,1,"^585|V4511",1,"^N18|^Z49",0,NA,0,NA,1,"^9093[5-9]|^909[4-9][0-9]","mh_ckd")
ckd = unique(rbind(ickd,ockd,pckd))
#> nrow(ickd)
#> nrow(ockd)
#> nrow(pckd)
#> nrow(ckd)



fcom = names(diab)[1]
tmp2 = Reduce(function(x, y) merge(x, y, by=fcom, all=TRUE), list(diab,gdia,htn,dep,men,ckd))
tmp2[is.na(tmp2)]=0

#> nrow(tmp2)



ichd = claim_feature(inp0,1,"^V1749",1,"Z8249",0,NA,0,NA,0,NA,"fh_chd")
ochd = claim_feature(outp0,1,"^V1749",1,"Z8249",0,NA,0,NA,0,NA,"fh_chd")
pchd = claim_feature(pfp0,1,"^V1749",1,"Z8249",0,NA,0,NA,0,NA,"fh_chd")
chd = unique(rbind(ichd,ochd,pchd))
#> nrow(ichd)
#> nrow(ochd)
#> nrow(pchd)
#> nrow(chd)



idia = claim_feature(inp0,1,"^V180",1,"Z833",0,NA,0,NA,0,NA,"fh_diabetes")
odia = claim_feature(outp0,1,"^V180",1,"Z833",0,NA,0,NA,0,NA,"fh_diabetes")
pdia = claim_feature(pfp0,1,"^V180",1,"Z833",0,NA,0,NA,0,NA,"fh_diabetes")
dia = unique(rbind(idia,odia,pdia))
#> nrow(idia)
#> nrow(odia)
#> nrow(pdia)
#> nrow(dia)



ibcs = claim_feature(inp0,1,"^V163",1,"Z803",0,NA,0,NA,0,NA,"fh_breastc")
obcs = claim_feature(outp0,1,"^V163",1,"Z803",0,NA,0,NA,0,NA,"fh_breastc")
pbcs = claim_feature(pfp0,1,"^V163",1,"Z803",0,NA,0,NA,0,NA,"fh_breastc")
bcs = unique(rbind(ibcs,obcs,pbcs))

#> nrow(ibcs)
#> nrow(obcs)
#> nrow(pbcs)
#> nrow(bcs)




icol = claim_feature(inp0,1,"^V160",1,"Z800",0,NA,0,NA,0,NA,"fh_colonc")
ocol = claim_feature(outp0,1,"^V160",1,"Z800",0,NA,0,NA,0,NA,"fh_colonc")
pcol = claim_feature(pfp0,1,"^V160",1,"Z800",0,NA,0,NA,0,NA,"fh_colonc")
col = unique(rbind(icol,ocol,pcol))
#> nrow(icol)
#> nrow(ocol)
#> nrow(pcol)
#> nrow(col)


ipro = claim_feature(inp0,1,"^V1642",1,"Z8042",0,NA,0,NA,0,NA,"fh_prostatec")
opro = claim_feature(outp0,1,"^V1642",1,"Z8042",0,NA,0,NA,0,NA,"fh_prostatec")
ppro = claim_feature(pfp0,1,"^V1642",1,"Z8042",0,NA,0,NA,0,NA,"fh_prostatec")
pro = unique(rbind(ipro,opro,ppro))
#> nrow(ipro)
#> nrow(opro)
#> nrow(ppro)
#> nrow(pro)


fcom = names(pro)[1]
tmp3 = Reduce(function(x, y) merge(x, y, by=fcom, all=TRUE), list(chd,dia,bcs,col,pro))
tmp3[is.na(tmp3)]=0
#> nrow(tmp3)


tmp = Reduce(function(x, y) merge(x, y, by=fcom, all=TRUE), list(tmp1,tmp2,tmp3))
tmp[is.na(tmp)]=0
#> nrow(tmp)



#add the gender info
f = "inpt_0518Y17/inpt_member_gender_race.csv"
ing = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)[,c(1,2)]
#> nrow(ing)


fcom = intersect(names(ing),names(inp))
ing = merge(x=inp[,1,drop=F], y=ing, by=fcom, all.x=T)
#> nrow(ing)




f = "outpt_0531Y17/outpt_member_gender_race.csv"
outg = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)[,c(1,2)]
#> nrow(outg)
outg = merge(x=outp[,1,drop=F], y=outg, by=fcom, all.x=T)
#> nrow(outg)



f = "prof_0520Y17/prof_claims_member_gender_race.csv"
pfg = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)[,c(1,2)]
#> nrow(pfg)
pfg = merge(x=pfp[,1,drop=F], y=pfg, by=fcom, all.x=T)
#> nrow(pfg)




f = "rx_0519Y17/rx_member_gender_race.csv"
rxg = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)[,c(1,2)]
#> nrow(rxg)
rxg = merge(x=rx0[,1,drop=F], y=rxg, by=fcom, all.x=T)
#> nrow(rxg)




claimg=unique(rbind(ing,outg,pfg,rxg))
#nrow(claimg)

t = count(claimg[,1])
t1 = t[t$freq>1,]
#nrow(t1)

claimg$gender=ifelse(claimg$Gender=="Male","M","F")
clmg = claimg[,c(1,3)]
fcom=names(clmg)[1]
clmg = merge(x=e, y=clmg, by=fcom, all.y=T)


#select the max(age)
inp$age = as.numeric(inp$Age)
inp[inp$age==999,]$age=-1
ina = aggregate(age ~ Encrypted.Member.ID, data=inp[,c(1,20)], max) 
#nrow(ina)


outp$age = as.numeric(outp$Age)
outp[outp$age==999,]$age=-1
outa = aggregate(age ~ Encrypted.Member.ID, data=outp[,c(1,20)], max) 
#nrow(outa)

pfp$age = as.numeric(pfp$Age)
pfp[pfp$age==999,]$age=-1
pfa = aggregate(age ~ Encrypted.Member.ID, data=pfp[,c(1,16)], max) 
#nrow(pfa)

rx0[rx0$Age=="90+",]$Age="91"
rx0$age = as.numeric(rx0$Age)
rx0[rx0$age==999,]$age=-1
rxa = aggregate(age ~ Encrypted.Member.ID, data=rx0[,c(1,5)], max) 
#nrow(rxa)


claima=unique(rbind(ina,outa,pfa,rxa))
#nrow(claima)

clma = aggregate(age ~ Encrypted.Member.ID, data=claima, max) 
#nrow(clma)

clma[clma$age==-1,]$age=NA

#merge the feature
ctmp = Reduce(function(x, y) merge(x, y, by=fcom, all=TRUE), list(clmg,clma,tmp))
ctmp1=ctmp[,c(1:5)]
#sum(is.na(ctmp1$age))

ctmp[is.na(ctmp)]=0
fclm = cbind(ctmp1, ctmp[,c(6:27)])
#sum(is.na(ctmp$age))
#nrow(fclm)


write.csv(fclm, paste(dst, "fclm", fyr, ".csv", sep=""), row.names=FALSE)



#lab
f = "lab_0517Y17/"
lab = read.csv(paste(dst, f, "lab_y", fyr, ".csv", sep=""), header=T, stringsAsFactors = FALSE)[,c(1,5,8,9)]
#nrow(lab)


flab=lab[lab$Encrypted.Member.ID %in% e$Encrypted.Member.ID,]
#nrow(flab)


#feature list
#bmi, height, weight, overweight, obese
library(xlsx)
f="lab_0517Y17/lab_ref.xlsx"
labref <- read.xlsx(paste(dst,f, sep=""), sheetName=paste("Sheet3", sep=""))
labref[labref$Quest.LOINC.Code %in% c("39156-5","3142-7","3137-7","8280-0"),]



lab_feature <- function(LOINC, df, fname){
  
  ff = df[df$Quest.LOINC.Code==LOINC,c(1,4)]
  ff$name = as.numeric(ff$Quest.Result)
  names(ff)[3]=fname
  return (ff[,c(1,3)])
}

fbmi = lab_feature("39156-5",flab,"bmi")
#> nrow(fbmi)

fbmi = aggregate(bmi ~ Encrypted.Member.ID, data=fbmi, mean) 
#> nrow(fbmi)

fbmi$overweight = ifelse(fbmi$bmi>=25 & fbmi$bmi<30, 1,0)
fbmi$obese = ifelse(fbmi$bmi>=30, 1,0)

fhei = lab_feature("3137-7",flab,"height")
fwei = lab_feature("3142-7",flab,"weight")

#> nrow(fhei)
#> nrow(fwei)

fhei = aggregate(height ~ Encrypted.Member.ID, data=fhei, mean) 
fwei = aggregate(weight ~ Encrypted.Member.ID, data=fwei, mean) 
#> nrow(fhei)
#> nrow(fwei)

#merge them
fcom = intersect(names(fhei),names(fwei))
hw = merge(x=fhei, y=fwei, by=fcom, all=T)
#nrow(hw)

hwb = merge(x=hw, y=fbmi, by=fcom, all=T)
#nrow(hwb)

#sum(is.na(hwb$bmi))
#length(which(hwb$height>0 & hwb$weight>0 & is.na(hwb$bmi)))

fage = unique(flab[,c(1,3)])
#nrow(fage)
#length(unique(fage[,1]))


fage$age = as.numeric(fage$Age)
fage[is.na(fage$age),]$age=-1
fage1 = aggregate(age ~ Encrypted.Member.ID, data=fage, max) 
#nrow(fage1)

fage1[fage1$age==-1,]$age=NA

fa = merge(x=fage1, y=hwb, by=fcom, all=T)
#nrow(fa)

fe = merge(x=e, y=fa, by=fcom, all.y=T)
#nrow(fe)


f = "lab_0517Y17/lab_member_gender.csv"
fg = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)

fcom = intersect(names(fg),names(fe))
fge = merge(x=fg, y=fe, by=fcom, all.y=T)
fge$gender=ifelse(fge$Quest.Gender=="Male","M","F")


#wc & large_wc features
fwc = lab_feature("8280-0",flab,"wc")
fwc = aggregate(wc ~ Encrypted.Member.ID, data=fwc, mean) 

fcom = names(fwc)[1]
fge1 = merge(x=fge[,-c(2)], y=fwc,by=fcom,all.x=T)
fge1$large_wc = ifelse((fge1$wc>=35 & fge1$gender=="F") | (fge1$wc>=40 & fge1$gender=="M"),1,0 )


write.csv(fge1, paste(dst, "flab", fyr, ".csv", sep=""), row.names=FALSE)
#length(unique(fge[,1]))


#HRA
hra = read.csv(paste(dst, "HRA_Y",fyr,"/HRA_Select_processed_Y", fyr, ".csv", sep=""), header=T, stringsAsFactors = FALSE)
#> nrow(hra)
hra=hra[hra$Encrypted.Member.ID %in% e$Encrypted.Member.ID,]
#> nrow(hra)

hra2 = read.csv(paste(dst, "HRA_Y", fyr, "/HRA_Fillin_processed_Y", fyr, ".csv", sep=""), header=T, stringsAsFactors = FALSE)

hra2=hra2[hra2$Encrypted.Member.ID %in% e$Encrypted.Member.ID,]



#question list
qlist = read.csv(paste(dst, "HRA_Y",fyr,"/HRA_question_Y",fyr,".csv", sep=""), header=T, stringsAsFactors = FALSE)

qid = c(2,840,80, 83,84,85,827,86,831,87,832,3000,1900,2100,2200,2300,2000,2600,2700,49,129,837,838)
hs = hra[hra$Question.ID %in% qid,]
#nrow(hs)


#as.list(strsplit(ff$Answer.ID,",")[[1]])

hs2 = hs[hs$Question.ID==2 & !is.na(hs$Answer.ID),]
r1 = hs2[grep("3",hs2$Answer.ID),c(1),drop=F]
r1$race_african=1
r2 = hs2[grep("4",hs2$Answer.ID),c(1),drop=F]
r2$race_asian=1
r3 = hs2[grep("5",hs2$Answer.ID),c(1),drop=F]
r3$race_white=1
r4 = hs2[grep("6",hs2$Answer.ID),c(1),drop=F]
r4$race_hisp=1
r5 = hs2[grep("7",hs2$Answer.ID),c(1),drop=F]
r5$race_native=1
fcom = names(r1)[1]
r = Reduce(function(x, y) merge(x, y, by=fcom, all=TRUE), list(r1,r2,r3,r4,r5))
r[is.na(r)]=0
#"MHQV-ADSU-PDYU-GA"


#> nrow(r)
#> nrow(r1)+nrow(r2)+nrow(r3)+nrow(r4)+nrow(r5)





df = hs[hs$Question.ID==840,]
df$smoking=ifelse(df$Answer.ID==3558,1,0)
df1 = hs[hs$Question.ID==80,]
df1$quit_attempt=ifelse(df1$Answer.ID==299,0,1)
df1$quit_current=ifelse(df1$Answer.ID==295,1,0)

#> nrow(df)
#> nrow(df1)

#length(setdiff(df1[,1],df[,1]))


smk = merge(x=df[,c(1,4)], y=df1[,c(1,4,5)], by=fcom, all.x=T)

#> nrow(smk)



#Q1: logic conflict
#length(setdiff(df1[,1],df[df$smoking==1,1]))


qo = c(83,84,85,827,86,831,87,832,49,129,837,838)
qlo = qlist[qlist$Question.ID %in% qo,c(2,3,4)]


ho = hs[hs$Question.ID %in% qo,]
fcom = intersect(names(ho),names(qlo))
hq = merge(x=ho, y=qlo, by=fcom, all.x=T)
#nrow(hq)



hra_feature2 <- function(Qid, df, fname){
  
  ff = df[df$Question.ID==Qid,]
  names(ff)[4]=fname
  return (ff[,c(3,4)])
}

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

#unique ID
#length(unique(hq[,3]))


fcom = names(o1)[1]
o = Reduce(function(x, y) merge(x, y, by=fcom, all=TRUE), list(o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12))
#nrow(o)


#qlist[qlist$Question.ID %in% c(30,19,21,22,23,20,26,27),]

hra_feature3 <- function(Qid, Aid, df, fname){
  
  ff = df[df$Question.ID==Qid,]
  ff$name=ifelse(ff$Answer.ID==Aid,1,0)
  names(ff)[4]=fname
  return (ff[,c(1,4)])
}

t1 = hra_feature3(3000,4087,hs,"mh_osteo")
t2 = hra_feature3(1900,4076,hs,"mh_ihd")
t3 = hra_feature3(2100,4078,hs,"mh_diabetes")
t4 = hra_feature3(2200,4079,hs,"mh_diabetes")
t5 = hra_feature3(2300,4080,hs,"mh_gdiabetes")
t6 = hra_feature3(2600,4083,hs,"mh_htn")
t7 = hra_feature3(2000,4077,hs,"mh_depression")
t8 = hra_feature3(2700,4084,hs,"mh_ckd")
t34 = unique(rbind(t3,t4))
t34 = aggregate(mh_diabetes ~ Encrypted.Member.ID, data=t34, max) 



fcom = names(t1)[1]
t = Reduce(function(x, y) merge(x, y, by=fcom, all=TRUE), list(t1,t2,t34,t5,t6,t7,t8))
#nrow(t)


sq = Reduce(function(x, y) merge(x, y, by=fcom, all=TRUE), list(r,smk,o,t))
#nrow(sq)



#get age

#fill-in question ID=147,149,150
qid = c(147,149,150)
hf = hra2[hra2$Question.ID %in% qid,]
#nrow(hf)



b1 = hf[hf$Question.ID==147,c(1,3)]
names(b1)[2]="height"
b2 = hf[hf$Question.ID==149,c(1,3)]
names(b2)[2]="weight"
b3 = hf[hf$Question.ID==150,c(1,3)]
names(b3)[2]="bmi"
#Q change the [25,29.9] to [25,30)
b3$overweight=ifelse(b3$bmi>=25 & b3$bmi<30, 1,0)
b3$obese=ifelse(b3$bmi>=30, 1,0)


#nrow(b1) #b2,b3


b = Reduce(function(x, y) merge(x, y, by=fcom, all=TRUE), list(b1,b2,b3))
#nrow(b)


sf = merge(x=sq, y=b, by=fcom, all=T)
#nrow(sf)


#add age to hra
hage = read.csv(paste(dst, "HRA_Y", fyr,"/HRA_age_status_Y",fyr,".csv", sep=""), header=T, stringsAsFactors = FALSE)[,c(1:3)]

#nrow(hage)
#nrow(e)

hage=hage[hage$Encrypted.Member.ID %in% e$Encrypted.Member.ID,]
fcom=names(hage)[1]
fage = merge(x=hage[,-c(2)], y=e, by=fcom, all.x=T)
#nrow(hage)

fage$age=as.integer(fage$Age)


fhra = merge(x=fage[,-c(2)], y=sf, by=fcom, all.x=T)
write.csv(fhra, paste(dst, "fhra", fyr,".csv", sep=""), row.names=FALSE)
#> nrow(fhra)


#Total
#combine claim,HRA and lab together
fclm = read.csv(paste(dst, "feature_Y",fyr,"/fclm", fyr,".csv", sep=""), header=T, stringsAsFactors = FALSE)
flab = read.csv(paste(dst, "feature_Y",fyr,"/flab", fyr,".csv", sep=""), header=T, stringsAsFactors = FALSE)
fhra = read.csv(paste(dst, "feature_Y",fyr,"/fhra", fyr,".csv", sep=""), header=T, stringsAsFactors = FALSE)


t1 = flab[,c(1,2,3,4)]
t2 = fhra[,c(1,2,3,4)]
t3 = fclm[,c(1,2,3,5)]

t = unique(rbind(t1, t2, t3))
#nrow(t)

#sum(is.na(t$age))

t[is.na(t$age),]$age=-1
tt = aggregate(age ~ Encrypted.Member.ID+Relationship+year, data=t, max) 
#nrow(tt)

#sum(tt$age==-1)
#tt[tt$age==-1,]$age=NA
#sum(is.na(tt$age))

r1=flab[,c(1,10)]
r2=fclm[,c(1,4)]
r = unique(rbind(r1,r2))
#> nrow(r)

#length(unique(r[,1]))


tt1 = merge(x=tt, y=r, by=fcom, all.x=T)
#nrow(tt1)

#sum(is.na(tt$age))
#sum(is.na(tt1$gender))
#nrow(tt1)-nrow(r)

#race and diet
#> names(fhra)
#[1] "Encrypted.Member.ID" "Relationship"        "year"                "age"                 "race_african"        "race_asian"          "race_white"          "race_hisp"          
#[9] "race_native"         "smoking"             "quit_attempt"        "quit_current"        "diet_fruitveg"       "diet_fiber"          "diet_highfat"        "diet_redmeat"       
#[17] "aero_perwk"          "aero_duration"       "strength_perwk"      "strength_duration"   "mh_osteo"            "mh_ihd"              "mh_diabetes"         "mh_gdiabetes"       
#[25] "mh_htn"              "mh_depression"       "mh_ckd"              "height"              "weight"               "bmi"                 "overweight"          "obese"              

tt2 = fhra[,c(1,5:9,13:24)]
#nrow(tt2)


#smoking
s1 = fhra[which(fhra$smoking %in% c(0,1)),c(1,10)]
s2 = fclm[which(fclm$smoking %in% c(0,1)),c(1,6)]

ss = unique(rbind(s1,s2))
#nrow(ss)
ss[is.na(ss)]=0
ss = aggregate(.~Encrypted.Member.ID, ss, max) 
#nrow(ss)


tt3 = ss

#quit
q1 = fhra[which(fhra$quit_current %in% c(0,1) | fhra$quit_attempt %in% c(0,1)),c(1,11,12)]
q2 = fclm[which(fclm$quit_current %in% c(0,1)),c(1,8,7)]

qq = unique(rbind(q1,q2))
#nrow(qq)
qq[is.na(qq)]=0
qq = aggregate(.~Encrypted.Member.ID, qq, max) 
#nrow(qq)

tt32= qq

#weight height bmi overweight and obese
t1 = flab[!is.na(flab$bmi),c(1,5,6,7,8,9)]
t2 = fhra[!is.na(fhra$bmi),c(1,32:36)]
t3 = fclm[fclm$overweight==1 | fclm$obese==1,c(1,9:10)]
#> nrow(t1)
#> nrow(t2)
#> nrow(t3)




#add weight height and bmi to t3
t3 = merge(x=t3, y=t1[,c(1:4)], by=fcom, all.x=T)
t31=t3[!is.na(t3$bmi),]
t32=t3[is.na(t3$bmi),]
#> nrow(t31)
#> nrow(t32)



t32 = merge(x=t32[,c(1:3)], y=t2[,c(1:4)], by=fcom, all.x=T)
#sum(is.na(t32$bmi))

t3 = rbind(t31,t32)[,c(1,4,5,6,2,3)]



#lab over hra
t12 = t2[!(t2[,1] %in% t1[,1]),]
#nrow(t12)
t112 = unique(rbind(t1,t12))
#nrow(t112)
#nrow(t1)+nrow(t12)

t123 = t3[!(t3[,1] %in% t112[,1]),]
#nrow(t123)
#sum(is.na(t123$bmi))


tt4 = rbind(t112,t123)
#nrow(tt4)
#length(unique(tt4[,1]))

tt42=flab[,c(1,c(11:12))]

#alcohol, mlsk
tt5 = fclm[,c(1,11:12,14,16,21,23:27)]
#nrow(tt5)



#> names(fclm)
#[1] "Encrypted.Member.ID" "Relationship"        "year"                "gender"              "age"                 "smoking"             "quit_current"        "quit_attempt"       
#[9] "overweight"          "obese"               "alcohol_depend"      "alcohol_treat"       "mh_osteo"            "mh_mlsk"             "mh_ihd"              "mh_avd_others"      
#[17] "mh_diabetes"         "mh_gdiabetes"        "mh_htn"              "mh_depression"       "mh_mental_other"     "mh_ckd"              "fh_chd"              "fh_diabetes"        
#[25] "fh_breastc"          "fh_colonc"           "fh_prostatec"       

  
#other common features
fncomm = c("Encrypted.Member.ID","mh_osteo", "mh_ihd","mh_diabetes","mh_gdiabetes","mh_htn","mh_depression","mh_ckd")

t1 = fhra[,fncomm]
#nrow(t1)

t1 = t1[!is.na(t1$mh_osteo),]
#nrow(t1)

t2 = fclm[,fncomm]
#select the 1 (yes) from hra and clm
t = unique(rbind(t1,t2))
t[is.na(t)]=0

#> nrow(t1)
#> nrow(t2)
#> nrow(t)
#> nrow(t1)+nrow(t2)-nrow(t)

tt6 = aggregate(.~Encrypted.Member.ID, t, max) 
#nrow(tt6)


#merge tt1-tt6
tt = Reduce(function(x, y) merge(x, y, by=fcom, all=TRUE), list(tt1,tt2,tt3,tt32,tt4,tt42,tt5,tt6))
#nrow(tt1)
#> nrow(tt2)
#> nrow(tt3)
#> nrow(tt4)
#> nrow(tt5)
#> nrow(tt6)
#> nrow(tt)


write.csv(tt, paste(dst, "f", fyr, ".csv", sep=""), row.names=FALSE)

dst2="/watson/quest_eh/data/extracted_tables/"
kk = tt[tt$Relationship=="Employee",]
write.csv(kk, paste(dst2, "demo_lifestyle_Y", fyr, ".csv", sep=""), row.names=FALSE)

#Y16
#> nrow(kk)
#[1] 33146
#Y15
#> nrow(kk)
#[1] 33689
#Y14
#> nrow(kk)
#[1] 31514




#summary stats

#multi.fun <- function(x) {cbind(freq = table(x), percentage = prop.table(table(x))*100)}
#multi.fun(demo$race_hisp)

fyr="14"
demo = read.csv(paste("/watson/quest_eh/data/extracted_tables/demo_lifestyle_Y",fyr,".csv", sep=""), header=T, stringsAsFactors = FALSE)

multi.fun0 <- function(x) {cbind(max = max(x), min=min(x), median=median(x), mean=mean(x))}
a1 = data.frame(multi.fun0(demo[demo$age!=-1,]$age))
a2 = data.frame(sum(demo$age==-1)/nrow(demo)*100)
names(a2)="NA"
a = cbind(a1,a2)


multi.fun2 <- function(x) {percentage = prop.table(table(demo[,x]))*100}

g1 = data.frame(sapply(5,function(x) multi.fun2(x)))
names(g1) = names(demo)[5]
g2 = sapply(5, function(x) {freq=sum(is.na(demo[,x]))/nrow(demo)*100})
g = rbind(g1,g2)


tl = c(6:10,23,33)
t = sapply(tl,function(x) multi.fun2(x))
t = data.frame(t)
names(t)=names(demo)[tl]
m = sapply(tl, function(x) {freq=sum(is.na(demo[,x]))/nrow(demo)*100})
tm = rbind(t,m)

b = cbind(g,tm)
rownames(b)[3]="NA"

a14$year="2014"
a15$year="2015"
a16$year="2016"
a = rbind(a14,a15,a16)

b14$year="2014"
b15$year="2015"
b16$year="2016"
b = rbind(b14,b15,b16)

library(xlsx)
write.xlsx(a, paste(dst, "demo_sum.xlsx", sep=""), sheetName="age", row.names=FALSE)
write.xlsx(b, paste(dst, "demo_sum.xlsx", sep=""), sheetName="other", append=TRUE, row.names=FALSE)


#

fyr="14"
demo14 = read.csv(paste("/watson/quest_eh/data/extracted_tables/demo_lifestyle_Y",fyr,".csv", sep=""), header=T, stringsAsFactors = FALSE)


