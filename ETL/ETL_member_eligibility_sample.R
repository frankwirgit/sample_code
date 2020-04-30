# An ETL example to process member eligibility and survey type of data

options(width=200)
library(plyr)
src="/watson/quest_eh/data/original_files/2017MAY04/"
dst="/watson/quest_eh/workspace/lcao/data/"

#1 lab data process
f161 = "Custom.table1.2016.part1.csv"
q161 = read.csv(paste(src, f161, sep=""), header=T, stringsAsFactors = FALSE)

f162 = "Custom.table1.2016.part2.csv"
q162 = read.csv(paste(src, f162, sep=""), header=T, stringsAsFactors = FALSE)

f163 = "Custom.table1.2016.part3.csv"
q163 = read.csv(paste(src, f163, sep=""), header=T, stringsAsFactors = FALSE)
qy16 = rbind(q161,q162,q163)

m <- unique(q162[q162$Quest.LOINC.Code %in% c("30376-8", "26523-1"),c(5,7,8,10)])
t3 <- unique(q163[,8])
setdiff(t3,t1)


f151 = "Custom.table1.2015.part1.csv"
q151 = read.csv(paste(src, f151, sep=""), header=T, stringsAsFactors = FALSE)


f152 = "Custom.table1.2015.part2.csv"
q152 = read.csv(paste(src, f152, sep=""), header=T, stringsAsFactors = FALSE)


f153 = "Custom.table1.2015.part3.csv"
q153 = read.csv(paste(src, f153, sep=""), header=T, stringsAsFactors = FALSE)
qy15 = rbind(q151,q152,q153)

diff <- setdiff(t51,t1)
m <- unique(q151[q151$Quest.LOINC.Code %in% diff,c(5,7,8,10)])
length(which(q151$Quest.LOINC.Code=="18684-1"))

f141 = "Custom.table1.2014.part1.csv"
q141 = read.csv(paste(src, f141, sep=""), header=T, stringsAsFactors = FALSE)


f142 = "Custom.table1.2014.part2.csv"
q142 = read.csv(paste(src, f142, sep=""), header=T, stringsAsFactors = FALSE)

f143 = "Custom.table1.2014.part3.csv"
q143 = read.csv(paste(src, f143, sep=""), header=T, stringsAsFactors = FALSE)
qy14 = rbind(q141,q142,q143)

q141 = q141[!is.na(q141$Quest.LOINC.Code),]
q141 = q141[q141$Quest.LOINC.Code!="" & q141$Quest.LOINC.Code!="na",]
t41 = unique(q141[,8])
diff <- setdiff(t41,t1)
diff <- diff[c(3:8)]
m <- unique(q141[q141$Quest.LOINC.Code %in% diff,c(5,7,8,10)])
write.csv(m, paste(dst, "lab_ref.csv", sep=""), row.names=FALSE)


q142 = q142[!is.na(q142$Quest.LOINC.Code),]
q142 = q142[q142$Quest.LOINC.Code!="" & q142$Quest.LOINC.Code!="na" & q142$Quest.LOINC.Code!="n/a",]
t42 = unique(q142[,8])

f131 = "Custom.table1.2013.part1.csv"
q131 = read.csv(paste(src, f131, sep=""), header=T, stringsAsFactors = FALSE)

f132 = "Custom.table1.2013.part2.csv"
q132 = read.csv(paste(src, f132, sep=""), header=T, stringsAsFactors = FALSE)

f133 = "Custom.table1.2013.part3.csv"
q133 = read.csv(paste(src, f133, sep=""), header=T, stringsAsFactors = FALSE)
qy13 = rbind(q131,q132,q133)

q131 = q131[!is.na(q131$Quest.LOINC.Code),]
q131 = q131[q131$Quest.LOINC.Code!="" & q131$Quest.LOINC.Code!="na" & q131$Quest.LOINC.Code!="n/a",]
t31 = unique(q131[,8])

t <- unique(c(t1,t41))
m <- unique(q131[q131$Quest.LOINC.Code %in% diff,c(5,7,8,10)])
write.csv(m, paste(dst, "lab_ref.csv", sep=""), row.names=FALSE)

n = q131[q131$Quest.LOINC.Code=="NLA" | q131$Quest.LOINC.Code=="WTF" | q131$Quest.LOINC.Code=="Test Code",]


dst="/watson/quest_eh/workspace/lcao/data/"

names(q161)
t <- unique(q161[,c(5,7,8,10)])
write.csv(t, paste(dst, "lab_ref.csv", sep=""), row.names=FALSE)

t1 <- unique(q161[,c(8,10)])


length(unique(tq1$Encrypted.Member.ID))
q = tq1[which(tq1$Encrypted.Member.ID=="MHSU-BNQN-QDYU-YA"),]
length(unique(q$Quest.Clinical.Variable))


#lab data clean up

#import the mapping table
library(xlsx)
dst="/watson/quest_eh/workspace/lcao/data/"
labref <- read.xlsx(paste(dst,"lab_ref.xlsx", sep=""), sheetName=paste("ref", sep=""))


src="/watson/quest_eh/data/original_files/2017MAY04/"

f161 = "Custom.table1.2016.part1.csv"
q161 = read.csv(paste(src, f161, sep=""), header=T, stringsAsFactors = FALSE)

f162 = "Custom.table1.2016.part2.csv"
q162 = read.csv(paste(src, f162, sep=""), header=T, stringsAsFactors = FALSE)

f163 = "Custom.table1.2016.part3.csv"
q163 = read.csv(paste(src, f163, sep=""), header=T, stringsAsFactors = FALSE)
qy16 = rbind(q161,q162,q163)

> nrow(q161)
[1] 738762
> nrow(q162)
[1] 468351
> nrow(q163)
[1] 667333


> nrow(qy16)
[1] 1874446

> nrow(q151)
[1] 443236
> nrow(q152)
[1] 666436
> nrow(q153)
[1] 639472
> nrow(qy15)
[1] 1749144

> nrow(q141)
[1] 692583
> nrow(q142)
[1] 422105
> nrow(q143)
[1] 623696
> qy14 = rbind(q141,q142,q143)
> nrow(qy14)
[1] 1738384



qy13 = rbind(q131,q132,q133)
> nrow(q131)
[1] 538172
> nrow(q132)
[1] 648686
> nrow(q133)
[1] 716654
> nrow(qy13)
[1] 1903512


> names(qy16)
[1] "Encrypted.Member.ID"         "Quest.Clinical.History.Date" "Quest.Relationship"          "Quest.Gender"                "Quest.Clinical.Variable"     "Quest.Amended.Result"       
[7] "Quest.Reference.Range"       "Quest.LOINC.Code"            "Quest.Result.Indicator"      "Quest.Units.of.Measure"      "Quest.Employment.Status"     "Age"                        
[13] "Quest.Result"  



t = unique(qy13[,c(1,3)])
tt = count(t[,1])
p = tt[tt$freq>1,]
m = qy13[qy13$Encrypted.Member.ID %in% p$x,]
n = unique(m[,c(1,3,4,12)])
n[n$Age!="" & !is.na(n$Age),]

> nrow(p)
[1] 17

Encrypted.Member.ID Quest.Relationship Quest.Gender Age
210405   ZTOV-GTYW-YDYU-AA             Spouse       Female  59
865336   ZTOV-GTYW-YDYU-AA           Employee       Female  59

391626   QTZW-BXBF-QDYU-PA           Employee       Female  61
973876   QTZW-BXBF-QDYU-PA             Spouse       Female  61

Encrypted.Member.ID Quest.Relationship Quest.Gender Age
1694660   YNYN-YNYN-YDYU-QO             Spouse         Male  60
1694758   YNYN-YNYN-YDYU-QO           Employee         Male  60


Encrypted.Member.ID Quest.Relationship Quest.Gender Age
1220063   SUPN-MNGH-PDYU-AO             Spouse         Male  57
1220114   SUPN-MNGH-PDYU-AO           Employee         Male  57

Encrypted.Member.ID Quest.Relationship Quest.Gender Age
453867    PNQD-QTSU-ODYF-ZA           Employee       Female  31
1100558   PNQD-QTSU-ODYF-ZA             Spouse       Female  31



#Quest.Amended.Result!="No" 
> count(qy16[,6])
x    freq
1  No 1872650
2 Yes    1796

> count(qy16[,11])
x    freq
1                      464269
2              Active 1378144
3    Leave of Absence   27486
4      Leave With Pay    4291
5          Terminated     205
6 Terminated With Pay      51


> count(qy15[,6])
x    freq
1 No 1749144
> count(qy15[,11])
x    freq
1                      455195
2              Active 1265989
3    Leave of Absence   23689
4      Leave With Pay    3743
5          Terminated     286
6 Terminated With Pay     242


> count(qy14[,6])
x    freq
1  No 1738364
2 Yes      20
> count(qy14[,11])
x    freq
1                      531213
2              Active 1165763
3    Leave of Absence   33852
4      Leave With Pay    6803
5          Terminated     602
6 Terminated With Pay     151


> count(qy13[,6])
x    freq
1 No 1903512
> count(qy13[,11])
x    freq
1                      513015
2              Active 1340976
3    Leave of Absence   40664
4      Leave With Pay    7782
5          Terminated     734
6 Terminated With Pay     341


#demo16 = unique(qy16[,c(1,3,4,11,12)])
> nrow(demo16)
[1] 34416


> demo15 = unique(qy15[,c(1,3,4,11,12)])
> nrow(demo15)
[1] 34267

> nrow(demo14)
[1] 32589

demo13 = unique(qy13[,c(1,3,4,11,12)])
> nrow(demo13)
[1] 31876


#discard - combine patients across years
demo16 = unique(qy16[,c(1,2,4,12)])
demo15 = unique(qy15[,c(1,2,4,12)])
demo14 = unique(qy14[,c(1,2,4,12)])
demo13 = unique(qy13[,c(1,2,4,12)])
demo = rbind(demo13, demo14, demo15, demo16)
demo = unique(demo)
demo = demo[order(demo$Encrypted.Member.ID,demo$Quest.Gender,demo$Age),]
> nrow(demo)
[1] 132939

> t = ddply(demo, c("Encrypted.Member.ID"), summarise, Age = max(Age))
> nrow(t)
[1] 52175

noage = t[is.na(t$Age) | t$Age=="999" | t$Age=="",]
> nrow(noage)
[1] 14188
noage = noage[,c(-2), drop=F]
fcom = intersect(names(noage),names(demo))
t1 = merge(x=noage, y=unique(demo[,c(1,3)]), by=fcom)
write.csv(t1, paste(dst, "lab_member_noage.csv", sep=""), row.names=FALSE)

hasage = t[!is.na(t$Age) & t$Age!="999" & t$Age!="",]
> nrow(hasage)
[1] 37987



#create the age mapping
fcom = intersect(names(hasage),names(demo))
t2 = merge(x=hasage, y=demo, by=fcom)
> nrow(t2)
[1] 38873
write.csv(t2, paste(dst, "lab_member_age.csv", sep=""), row.names=FALSE)


ck = count(t2[,1])
ck1 = ck[ck$freq>1,]

extra = demo[demo$Encrypted.Member.ID %in% ck1$Encrypted.Member.ID,]

#discard - combine patients across years
demo16 = unique(qy16[,c(1,4)])
demo15 = unique(qy15[,c(1,4)])
demo14 = unique(qy14[,c(1,4)])
demo13 = unique(qy13[,c(1,4)])
demo = rbind(demo13, demo14, demo15, demo16)
demo = unique(demo)
demo = demo[order(demo$Encrypted.Member.ID,demo$Quest.Gender,demo$Age),]
> nrow(demo)
[1] 132939


#no NA, Age>0,
#Age "" 1962, NA 5311, 999 1
> noage16 = demo16[is.na(demo16$Age) | demo16$Age=="999" | demo16$Age=="",]
> nrow(noage16)
[1] 7274
> nrow(unique(noage16[,c(1:4)]))
[1] 7188
noage16 = unique(noage16[,c(1:4)])
write.csv(noage16, paste(dst, "lab_noage16.csv", sep=""), row.names=FALSE)

> nrow(noage15)
[1] 7452
> nrow(unique(noage15[,c(1:4)]))
[1] 7374
> length(intersect(noage15$Encrypted.Member.ID, noage16$Encrypted.Member.ID))
[1] 5274
> length(setdiff(noage15$Encrypted.Member.ID, noage16$Encrypted.Member.ID))
[1] 2043


noage14 = demo14[is.na(demo14$Age) | demo14$Age=="999" | demo14$Age=="",]
> nrow(noage14)
[1] 8665
> nrow(unique(noage14[,c(1:4)]))
[1] 8609
> length(intersect(noage14$Encrypted.Member.ID, noage15$Encrypted.Member.ID))
[1] 4769
> length(intersect(noage14$Encrypted.Member.ID, noage16$Encrypted.Member.ID))
[1] 4053
> length(setdiff(noage14$Encrypted.Member.ID, noage15$Encrypted.Member.ID))
[1] 3824
> length(setdiff(noage14$Encrypted.Member.ID, noage16$Encrypted.Member.ID))
[1] 4540

noage14 = unique(noage14[,c(1:4)])
write.csv(noage14, paste(dst, "lab_noage14.csv", sep=""), row.names=FALSE)

noage13 = demo13[is.na(demo13$Age) | demo13$Age=="999" | demo13$Age=="",]
> nrow(unique(noage13[,c(1:4)]))
[1] 7257
> length(intersect(noage13$Encrypted.Member.ID, noage14$Encrypted.Member.ID))
[1] 4794
> length(intersect(noage13$Encrypted.Member.ID, noage15$Encrypted.Member.ID))
[1] 4175
> length(intersect(noage13$Encrypted.Member.ID, noage16$Encrypted.Member.ID))
[1] 3598
> length(setdiff(noage13$Encrypted.Member.ID, noage14$Encrypted.Member.ID))
[1] 2437
> length(setdiff(noage13$Encrypted.Member.ID, noage15$Encrypted.Member.ID))
[1] 3056
> length(setdiff(noage13$Encrypted.Member.ID, noage16$Encrypted.Member.ID))
[1] 3633

noage13 = unique(noage13[,c(1:4)])
write.csv(noage13, paste(dst, "lab_noage13.csv", sep=""), row.names=FALSE)




demo16 = demo16[!is.na(demo16$Age) & demo16$Age!="999" & demo16$Age!="",]
k = count(demo16, c('Encrypted.Member.ID','Quest.Relationship','Age'))
id <- unique(k[k$freq>1,]$Encrypted.Member.ID)
k3 <- demo16[demo16$Encrypted.Member.ID %in% id,]
Encrypted.Member.ID Quest.Relationship Quest.Gender Quest.Employment.Status Age
480958    QNYV-QWBL-YDYU-OA           Employee       Female                  Active  62
481008    QNYV-QWBL-YDYU-OA           Employee       Female        Leave of Absence  62
678125    ATQV-MNYU-YDYN-ZA           Employee       Female                  Active  23
1177818   ATQV-MNYU-YDYN-ZA           Employee       Female        Leave of Absence  23


demo15 = demo15[!is.na(demo15$Age) & demo15$Age!="999" & demo15$Age!="",]

k = count(demo15, c('Encrypted.Member.ID','Quest.Relationship','Age'))
id <- unique(k[k$freq>1,]$Encrypted.Member.ID)
k4 <- demo15[demo15$Encrypted.Member.ID %in% id,]
> nrow(k4)
[1] 0

demo14 = demo14[!is.na(demo14$Age) & demo14$Age!="999" & demo14$Age!="",]
k = count(demo14, c('Encrypted.Member.ID','Quest.Relationship','Age'))
id <- unique(k[k$freq>1,]$Encrypted.Member.ID)
k4 <- demo14[demo14$Encrypted.Member.ID %in% id,]
> nrow(k4)
[1] 4
Encrypted.Member.ID Quest.Relationship Quest.Gender Quest.Employment.Status Age
230506   ZLSD-PFAT-SDYV-SA           Employee       Female        Leave of Absence  53
541764   PWSW-GFQN-YDYF-OA           Employee       Female                  Active  30
541821   PWSW-GFQN-YDYF-OA           Employee       Female        Leave of Absence  30
825002   ZLSD-PFAT-SDYV-SA           Employee       Female                  Active  53


demo13 = demo13[!is.na(demo13$Age) & demo13$Age!="999" & demo13$Age!="",]
k = count(demo13, c('Encrypted.Member.ID','Quest.Relationship','Age'))
id <- unique(k[k$freq>1,]$Encrypted.Member.ID)
k5 <- demo13[demo13$Encrypted.Member.ID %in% id,]
> nrow(k5)
[1] 0

#Y16
t <- count(demo16, c('Encrypted.Member.ID','Quest.Relationship'))
> nrow(t)
[1] 26871

t2 <- t[t$freq>1,]
> nrow(t2)
[1] 271
t3 <- demo16[demo16$Encrypted.Member.ID %in% unique(t2$Encrypted.Member.ID),]
write.csv(t3, paste(dst, "lab_2status_2age16.csv", sep=""), row.names=FALSE)

#Y15
t <- count(demo15, c('Encrypted.Member.ID','Quest.Relationship'))
> nrow(t)
[1] 26740
> t2 <- t[t$freq>1,]
> nrow(t2)
[1] 75
t3 <- demo15[demo15$Encrypted.Member.ID %in% unique(t2$Encrypted.Member.ID),]
write.csv(t3, paste(dst, "lab_2age15.csv", sep=""), row.names=FALSE)

#Y14
t <- count(demo14, c('Encrypted.Member.ID','Quest.Relationship'))
> nrow(t)
[1] 23769
t2 <- t[t$freq>1,]
> nrow(t2)
[1] 155
t3 <- demo14[demo14$Encrypted.Member.ID %in% unique(t2$Encrypted.Member.ID),]
write.csv(t3, paste(dst, "lab_2status_2age14.csv", sep=""), row.names=FALSE)


#Y13
t <- count(demo13, c('Encrypted.Member.ID','Quest.Relationship'))
> nrow(t)
[1] 24618
> t2 <- t[t$freq>1,]
> nrow(t2)
[1] 1
> t3 <- demo13[demo13$Encrypted.Member.ID %in% unique(t2$Encrypted.Member.ID),]
> nrow(t3)
[1] 2

Encrypted.Member.ID Quest.Relationship Quest.Gender Quest.Employment.Status Age
31324    MUGV-MXQX-SDYW-AA           Employee       Female        Leave of Absence  36
572457   MUGV-MXQX-SDYW-AA           Employee       Female        Leave of Absence  37

write.csv(t3, paste(dst, "lab_2age13.csv", sep=""), row.names=FALSE)


#Y16
t1 <- t[t$freq==1,]
> nrow(t1)
[1] 26600

t1 <- demo16[demo16$Encrypted.Member.ID %in% unique(t1$Encrypted.Member.ID),]
write.csv(t1, paste(dst, "lab_demo16.csv", sep=""), row.names=FALSE)

#Y15
> nrow(t1)
[1] 26665
t1 <- demo15[demo15$Encrypted.Member.ID %in% unique(t1$Encrypted.Member.ID),]
write.csv(t1, paste(dst, "lab_demo15.csv", sep=""), row.names=FALSE)

#Y14
t1 <- demo14[demo14$Encrypted.Member.ID %in% unique(t1$Encrypted.Member.ID),]
write.csv(t1, paste(dst, "lab_demo14.csv", sep=""), row.names=FALSE)
> nrow(t1)
[1] 23614

#Y13
> nrow(t1)
[1] 24617
t1 <- demo13[demo13$Encrypted.Member.ID %in% unique(t1$Encrypted.Member.ID),]
write.csv(t1, paste(dst, "lab_demo13.csv", sep=""), row.names=FALSE)

#extract columns needed for analysis
qy16 = qy16[,c(1,3,7,8,9,13)]
> nrow(qy16)
[1] 1874446

#dedup
> qy16 = unique(qy16)
> nrow(qy16)
[1] 1847992


qy15 = unique(qy15[,c(1,3,7,8,9,13)])
> nrow(qy15)
[1] 1722754


qy14 = unique(qy14[,c(1,3,7,8,9,13)])
> nrow(qy14)
[1] 1712471

qy13 = unique(qy13[,c(1,3,7,8,9,13)])
> nrow(qy13)
[1] 1877147


#multiple lab records
#Y16
> s <- data.frame(table(qy16$Encrypted.Member.ID))
> max(s$Freq)
[1] 1737
> which(s$Freq==1737)
[1] 28240
> s[28240,]
Var1 Freq
28240 YNYN-YNYN-YDYW-YO 1737


#drop records with no values populated
> sum(is.na(qy16$Quest.Result))
[1] 0
> sum(qy16$Quest.Result=="")
[1] 36335
> sum(qy16$Quest.Result=="na")
[1] 0
> sum(qy16$Quest.Result=="n/a")
[1] 0
> sum(qy16$Quest.Result=="NA")
[1] 0
> qy16 = qy16[qy16$Quest.Result!="",]
> nrow(qy16)
[1] 1811657

write.csv(qy16, paste(dst, "lab_y16.csv", sep=""), row.names=FALSE)

#Y15
> sum(is.na(qy15$Quest.Result))
[1] 0
> sum(qy15$Quest.Result=="")
[1] 36910
> sum(qy15$Quest.Result=="n/a")
[1] 0
> sum(qy15$Quest.Result=="na")
[1] 0
> sum(qy15$Quest.Result=="NA")
[1] 0
> qy15 = qy15[qy15$Quest.Result!="",]
> nrow(qy15)
[1] 1685844
write.csv(qy15, paste(dst, "lab_y15.csv", sep=""), row.names=FALSE)

#Y14
> sum(is.na(qy14$Quest.Result))
[1] 0
> sum(qy14$Quest.Result=="")
[1] 33309
> sum(qy14$Quest.Result=="na")
[1] 0
> sum(qy14$Quest.Result=="n/a")
[1] 0
> sum(qy14$Quest.Result=="NA")
[1] 0
> qy14 = qy14[qy14$Quest.Result!="",]
> nrow(qy14)
[1] 1679162
write.csv(qy14, paste(dst, "lab_y14.csv", sep=""), row.names=FALSE)

#Y13
> nrow(qy13)
[1] 1877147
> sum(is.na(qy13$Quest.Result))
[1] 0
> sum(qy13$Quest.Result=="")
[1] 6425
> sum(qy13$Quest.Result=="na")
[1] 0
> sum(qy13$Quest.Result=="n/a")
[1] 0
> sum(qy13$Quest.Result=="NA")
[1] 0
> qy13 = qy13[qy13$Quest.Result!="",]
> nrow(qy13)
[1] 1870722
write.csv(qy13, paste(dst, "lab_y13.csv", sep=""), row.names=FALSE)

#merge across years



#2. HRA question process
fq = "Custom.table2.2016.part1.csv"
q = read.csv(paste(src, fq, sep=""), header=T, stringsAsFactors = FALSE)

b = data.frame(table(q$Encrypted.Member.ID))
max(b$Freq)
length(unique(q$Encrypted.Member.ID))
s =  q[which(q$Encrypted.Member.ID=="YNYN-YNYN-YDYF-OO"),]
length(unique(s$Question))




#===========================================================
#3. inpatient claim process
options(width=200)
library(plyr)
src="/watson/quest_eh/data/original_files/2017MAY04/"
f = "inpatient.claims.2013to2016.csv"
inpd = read.csv(paste(src, f, sep=""), header=T, stringsAsFactors = FALSE)
> nrow(inpd)
[1] 16109

src2="/watson/quest_eh/data/original_files/2017JUN05/UpdatedTable2-AND-Q42016data/"
f = "InpatientClaims.2016Q4.incurred.csv"
inpq4 = read.csv(paste(src2, f, sep=""), header=T, stringsAsFactors = FALSE)
> nrow(inpq4)
[1] 1000


#create gender race mapping - Encrypted Member ID, Gender, Ethnicity, 

> nrow(unique(inpd[,c(1,5)]))
[1] 9794
k = unique(inpd[,c(1,5,25)])
m = count(k[,c(1,2)])
t1 = m[m$freq==1,]
k1 = k[k$Encrypted.Member.ID %in% t1$Encrypted.Member.ID,]

t2 = m[m$freq>1,]
k2 = k[k$Encrypted.Member.ID %in% t2$Encrypted.Member.ID,]
nrow(k2)
[1] 64
k2 = k2[order(k2$Encrypted.Member.ID),]
k2 = k2[k2$Ethnicity!="N/A",]
nrow(k2)
[1] 32
> nrow(k1)
[1] 9762
> nrow(k1)+nrow(k2)
[1] 9794
> k = rbind(k1,k2)
> k = k[order(k$Encrypted.Member.ID),]
> nrow(k)
[1] 9794

dst="/watson/quest_eh/workspace/lcao/data/"
write.csv(k, paste(dst, "inpt_member_gender_race.csv", sep=""), row.names=FALSE)

f = "inpt_0518Y17/inpt_member_gender_race.csv"
inpd_mem = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)

k0 = k[!(k$Encrypted.Member.ID %in% unique(inpd_mem$Encrypted.Member.ID)), ]

inpd_mem0 = rbind(inpd_mem, k0)

write.csv(inpd_mem0, paste(dst, "inpt_member_gender_race_total.csv", sep=""), row.names=FALSE)


#create the data
#remove plan relationship, plan product, benefit package, mental health carrier, external medical indicator, external pharmacy indicator, extra claim vendor
#member months, subscriber months, 
inpd = unique(inpd[,c(1:3,6:8,10,12:15,17,20:24,26:57,59:81,84:100)])
> nrow(inpd)
[1] 16011

inpq4 = unique(inpq4[,c(1:3,6:8,10,12:15,17,20:24,26:57,59:81,84:100)])
> nrow(inpq4)
[1] 984

inpd = unique(rbind(inpd,inpq4))
> nrow(inpd)
[1] 16974


#DRG mapping
d = inpd[,"DRG",drop=F]
inpd$DRG = gsub("(^[0-9]{3}).*", "\\1", d$DRG)

d$DRG.Detail = gsub("^[0-9]{3}\\s", "", d$DRG)
d$DRG = gsub("(^[0-9]{3}).*", "\\1", d$DRG)
> d = unique(d)
> nrow(d)
[1] 643
> names(d)
[1] "DRG"        "DRG.Detail"
> length(unique(d[,1]))
[1] 643
write.csv(d, paste(dst, "inpt_DRG.csv", sep=""), row.names=FALSE)

f = "inpt_0518Y17/inpt_DRG.csv"
drg = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
d0 = d[!(d$DRG %in% unique(drg$DRG)), ]
drg0 = rbind(drg, d0)
write.csv(drg0, paste(dst, "inpt_DRG_total.csv", sep=""), row.names=FALSE)

#ICD mapping
d = inpd[,c(29:39),drop=F]

process_dignosis = function(x){
  
  gsub("(^\\S+).*", "\\1", d[, x])
} 


t = data.frame(sapply(2:11, function(x) gsub("(^\\S+).*", "\\1", d[, x])))
names(t)=names(d)[-1]

inpd[,c(30:39)]=t[,c(1:10)]

t2 = data.frame(sapply(2:11, function(x) gsub("^\\S+\\s*", "", d[, x]))) 
names(t2) = paste0(names(d)[-1],".Detail")

dig = cbind(d[,1,drop=F], t, t2)

dig9 = dig[dig$Submitted.ICD.Code.Version=="ICD-9",c(2:6,12:16)]

dig90 = dig9[,c(1,6)]
names(dig90)=c("ICD9","ICD9.Detail")
dig90 = unique(dig90)

dig91 = dig9[,c(2,7)]
names(dig91)=c("ICD9","ICD9.Detail")
dig91 = unique(dig91)

dig92 = dig9[,c(3,8)]
names(dig92)=c("ICD9","ICD9.Detail")
dig92 = unique(dig92)

dig93 = dig9[,c(4,9)]
names(dig93)=c("ICD9","ICD9.Detail")
dig93 = unique(dig93)

dig94 = dig9[,c(5,10)]
names(dig94)=c("ICD9","ICD9.Detail")
dig94 = unique(dig94)


dig901 = unique(rbind(dig90, dig91, dig92, dig93, dig94))

> nrow(dig90)
[1] 1883
> nrow(dig91)
[1] 1859
> nrow(dig92)
[1] 1815
> nrow(dig93)
[1] 1679
> nrow(dig94)
[1] 1395
> nrow(dig901)
[1] 3958

write.csv(dig901, paste(dst, "inpt_ICD9.csv", sep=""), row.names=FALSE)

f = "inpt_0518Y17/inpt_ICD9.csv"
dx9 = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)


dig1 = dig[dig$Submitted.ICD.Code.Version=="ICD-10",c(7:11,17:21)]

dig10 = dig1[,c(1,6)]
names(dig10)=c("ICD10","ICD10.Detail")
dig10 = unique(dig10)

dig20 = dig1[,c(2,7)]
names(dig20)=c("ICD10","ICD10.Detail")
dig20 = unique(dig20)

dig30 = dig1[,c(3,8)]
names(dig30)=c("ICD10","ICD10.Detail")
dig30 = unique(dig30)

dig40 = dig1[,c(4,9)]
names(dig40)=c("ICD10","ICD10.Detail")
dig40 = unique(dig40)

dig50 = dig1[,c(5,10)]
names(dig50)=c("ICD10","ICD10.Detail")
dig50 = unique(dig50)

dig101 = unique(rbind(dig10, dig20, dig30, dig40, dig50))

> nrow(dig10)
[1] 1277
> nrow(dig20)
[1] 1222
> nrow(dig30)
[1] 1184
> nrow(dig40)
[1] 1097
> nrow(dig50)
[1] 939
> nrow(dig101)
[1] 3129
> n = count(dig101[,1])
> n1 = n[n$freq>1,]
> nrow(n1)
[1] 0

write.csv(dig101, paste(dst, "inpt_ICD10.csv", sep=""), row.names=FALSE)

f = "inpt_0518Y17/inpt_ICD10.csv"
icd = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
d0 = dig101[!(dig101$ICD10 %in% unique(icd$ICD10DX)), ]

names(d0)=names(icd)
icd0 = rbind(icd, d0)

write.csv(icd0, paste(dst, "inpt_ICD10_total.csv", sep=""), row.names=FALSE)


#PCS mapping
p = inpd[,c(40:45),drop=F]
r = data.frame(sapply(1:6, function(x) gsub("(^\\S+).*", "\\1", p[, x])), stringsAsFactors=FALSE)
names(r)=names(p)

inpd[,c(40:45)]=r[,c(1:6)]
write.csv(inpd, paste(dst, "inpt_claims_y13-16.csv", sep=""), row.names=FALSE)

r2 = data.frame(sapply(1:6, function(x) gsub("^\\S+\\s*", "", p[, x]))) 
names(r2) = paste0(names(p),".Detail")

p1 = cbind(r[,1,drop=F],r2[,1,drop=F])
names(p1)=c("CPT9","CPT9.Detail")
p2 = cbind(r[,2,drop=F],r2[,2,drop=F])
names(p2)=c("CPT9","CPT9.Detail")
p3 = cbind(r[,3,drop=F],r2[,3,drop=F])
names(p3)=c("CPT9","CPT9.Detail")

p7 = unique(rbind(p1,p2,p3))
> nrow(p7)
[1] 1474
> nrow(unique(p1))
[1] 987
> nrow(unique(p2))
[1] 932
> nrow(unique(p3))
[1] 658
> n = count(p7[,1])
> n1 = n[n$freq>1,]
> nrow(n1)
[1] 0

write.csv(p7, paste(dst, "inpt_ICD9_PCS.csv", sep=""), row.names=FALSE)


f = "inpt_0518Y17/inpt_old/inpt_ICD9_PCS_old.csv"
pp = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
names(p7)=names(pp)
d0 = p7[!(p7$ICD9PCS %in% unique(pp$ICD9PCS)), ]
pp0 = rbind(pp, d0)
write.csv(pp0, paste(dst, "inpt_ICD9_PCS_total.csv", sep=""), row.names=FALSE)


p4 = cbind(r[,4,drop=F],r2[,4,drop=F])
names(p4)=c("CPT10","CPT10.Detail")
p5 = cbind(r[,5,drop=F],r2[,5,drop=F])
names(p5)=c("CPT10","CPT10.Detail")
p6 = cbind(r[,6,drop=F],r2[,6,drop=F])
names(p6)=c("CPT10","CPT10.Detail")

p8 = unique(rbind(p4,p5,p6))
> nrow(p8)
[1] 1383
> nrow(unique(p4))
[1] 773
> nrow(unique(p5))
[1] 658
> nrow(unique(p6))
[1] 447
> n = count(p8[,1])
> n1 = n[n$freq>1,]
> nrow(n1)
[1] 0

write.csv(p8, paste(dst, "inpt_ICD10_PCS.csv", sep=""), row.names=FALSE)

f = "inpt_0518Y17/inpt_ICD10_PCS.csv"
pp = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
names(p8)=names(pp)
d0 = p8[!(p8$ICD10PCS %in% unique(pp$ICD10PCS)), ]
pp0 = rbind(pp, d0)
write.csv(pp0, paste(dst, "inpt_ICD10_PCS_total.csv", sep=""), row.names=FALSE)



f = "inpt_0518Y17/inpt_claims_y13-16.csv"
inpd = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE, colClasses)
ik = fread(paste(dst, f, sep=""), header=T)

f = "inpt_0518Y17/inpt_old/inpt_claims_y13-16_old.csv"
inpd_old = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE, colClasses=("ICD.9.Procedure.3"="character"))


f1 = "inpt_0518Y17/inpt_old/inpt_claims_y16Q4.csv"
inpd42 = read.csv(paste(dst, f1, sep=""), header=T, stringsAsFactors = FALSE, 
colClasses=(c("ICD.9.Procedure.1"="character","ICD.9.Procedure.2"="character","ICD.9.Procedure.3"="character")))


write.csv(inpd_old, paste(dst, "inpt_0518Y17/inpt_old/inpt_claims_y13-16_old.csv", sep=""), row.names=FALSE)

write.csv(inpd4, paste(dst, "inpt_0518Y17/inpt_old/inpt_claims_y16Q4.csv", sep=""), row.names=FALSE)

> nrow(inpd_old)
[1] 16011
> nrow(inpd4)
[1] 984
> inpd = unique(rbind(inpd_old, inpd4))
> nrow(inpd)
[1] 16974

write.csv(inpd, paste(dst, "inpt_0518Y17/inpt_claims_y13-16.csv", sep=""), row.names=FALSE)


write.csv(inpd, paste(dst, "inpt_claims_y13-16.csv", sep=""), row.names=FALSE)


#update the missing age 
b = unique(inpd[inpd$Age!="999" & inpd$Age!="" & inpd$Age!="90+",c(1,4,21)])
b$age = as.integer(b$Age)
> nrow(b)
[1] 12345




t = aggregate(b[,c(3:4)], b[,1,drop=F], FUN=max)
nrow(t)
[1] 9973



t$age16 = as.integer(t$age) + 2016 - as.integer(t$Paid.Date)
t = t[,-c(2:3)]


a = inpd[inpd$Encrypted.Member.ID %in% t$Encrypted.Member.ID & (inpd$Age=="999" | inpd$Age==""),]
> nrow(a)
[1] 25


fcom = intersect(names(t),names(a))
d = merge(x=a, y=t, by=fcom, all.x=T)
d$Age = as.character(d$age16 - (2016-as.integer(d$Paid.Date)))
#drop age16
d = d[,c(-90)]
nrow(d)
[1] 25

c = inpd[!(inpd$Encrypted.Member.ID %in% t$Encrypted.Member.ID) | (inpd$Encrypted.Member.ID %in% t$Encrypted.Member.ID & inpd$Age!="999" & inpd$Age!=""),]
nrow(inpd)-nrow(d)
[1] 16949

> nrow(c)
[1] 16949



inpd0 = rbind(c, d)
> nrow(inpd0)
[1] 16974



inpd0 = unique(inpd0)
> nrow(inpd0)
[1] 16974




write.csv(inpd0, paste(dst, "inpt_claims_y13-16.csv", sep=""), row.names=FALSE)









################check the age
f1 = "lab_0517Y17/lab_member_noage.csv"
m1 = read.csv(paste(dst, f1, sep=""), header=T, stringsAsFactors = FALSE)

f2 = "lab_0517Y17/lab_member_gender.csv"
mt = read.csv(paste(dst, f2, sep=""), header=T, stringsAsFactors = FALSE)

fd = "inpt_0518Y17/inpt_claims_y13-16.csv"
inpd = read.csv(paste(dst, fd, sep=""), header=T, stringsAsFactors = FALSE)

> t = unique(inpd[,c(1,4)])
> t = t[t$Age!="999",]
> nrow(t)
[1] 11469


> m2 = m1[m1$Encrypted.Member.ID %in% t$Encrypted.Member.ID,]
> nrow(m2)
[1] 168

> t1 = unique(inpd[,1])
> length(t1)
[1] 9794
> f2 = "lab_0517Y17/lab_member_gender.csv"
> mt = read.csv(paste(dst, f2, sep=""), header=T, stringsAsFactors = FALSE)
> t2 = unique(mt[,1])
> length(t2)
[1] 52175
> length(intersect(t1,t2))
[1] 4055





# inpatient Yes/No data fields
#"Regular.Temp" "Discretionary" "C.Section" "Network.Indicator"  



#4. process outpatient claims
options(width=200)
library(plyr)
src="/watson/quest_eh/data/original_files/2017MAY04/"
dst="/watson/quest_eh/workspace/lcao/data/"

fo = "outpatient.claims.2013to2016.csv"
outp = read.csv(paste(src, fo, sep=""), header=T, stringsAsFactors = FALSE)

> nrow(outp)
[1] 294735


options(width=200)
library(plyr)
src="/watson/quest_eh/data/original_files/2017MAY04/"
dst="/watson/quest_eh/workspace/lcao/data/"
src2="/watson/quest_eh/data/original_files/2017JUN05/UpdatedTable2-AND-Q42016data/"
f = "OutpatientClaims.2016Q4.incurred.csv"
outpq4 = read.csv(paste(src2, f, sep=""), header=T, stringsAsFactors = FALSE)
> nrow(outpq4)
[1] 19542



#create gender race mapping - Encrypted Member ID, Gender, Ethnicity, 
> nrow(unique(outp[,c(1,5)]))
[1] 52168

k = unique(outp[,c(1,5,25)])
m = count(k[,c(1,2)])
t1 = m[m$freq==1,]
k1 = k[k$Encrypted.Member.ID %in% t1$Encrypted.Member.ID,]

t2 = m[m$freq>1,]
k2 = k[k$Encrypted.Member.ID %in% t2$Encrypted.Member.ID,]
> nrow(k2)
[1] 910
k2 = k2[order(k2$Encrypted.Member.ID),]
k2 = k2[k2$Ethnicity!="N/A",]
> nrow(k2)
[1] 455

> nrow(k1)
[1] 51713
> nrow(k1)+nrow(k2)
[1] 52168

k = rbind(k1,k2)
k = k[order(k$Encrypted.Member.ID),]
> nrow(k)
[1] 52168


dst="/watson/quest_eh/workspace/lcao/data/"
write.csv(k, paste(dst, "outpt_member_gender_race.csv", sep=""), row.names=FALSE)


f = "outpt_0531Y17/outpt_member_gender_race.csv"
mem = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
k0 = k[!(k$Encrypted.Member.ID %in% unique(mem$Encrypted.Member.ID)), ]
mem0 = rbind(mem, k0)
write.csv(mem0, paste(dst, "outpt_member_gender_race.csv", sep=""), row.names=FALSE)



#create the data
#remove plan relationship, plan product, benefit package, mental health carrier, external medical indicator, external pharmacy indicator, extra claim vendor
#member months, subscriber months, 
outp = unique(outp[,c(1:3,6:8,10,12:15,17,20:24,26:55,57:78,81:93)])
> nrow(outp)
[1] 240636

outpq4 = unique(outpq4[,c(1:3,6:8,10,12:15,17,20:24,26:55,57:78,81:93)])
> nrow(outpq4)
[1] 16818


#DX
dx = unique(outp[,c(30:40)])
> nrow(dx)
[1] 104110


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
[1] 7207

> length(unique(dxd9[,1]))
[1] 7207


write.csv(dxd9, paste(dst, "outpt_claims_ICD9.csv", sep=""), row.names=FALSE)

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
[1] 9335
> length(unique(dxd1[,1]))
[1] 9335


write.csv(dxd1, paste(dst, "outpt_claims_ICD10.csv", sep=""), row.names=FALSE)

f = "outpt_0531Y17/outpd_claims_ICD10.csv"
icd = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)

d0 = dxd1[!(dxd1$ICD10DX %in% unique(icd$ICD10DX)), ]
icd0 = rbind(icd, d0)

write.csv(icd0, paste(dst, "outpt_claims_ICD10.csv", sep=""), row.names=FALSE)




t = data.frame(sapply(31:40, function(x) gsub("(^\\S+).*", "\\1", outp[, x])))
names(t)=names(outp)[31:40]
outp[,c(31:40)]=t

t = data.frame(sapply(31:40, function(x) gsub("(^\\S+).*", "\\1", outpq4[, x])))
names(t)=names(outpq4)[31:40]
outpq4[,c(31:40)]=t

#PCS mapping
p = outp[,c(41:46),drop=F]
r = data.frame(sapply(1:6, function(x) gsub("(^\\S+).*", "\\1", p[, x])))
names(r)=names(p)

r2 = data.frame(sapply(1:6, function(x) gsub("^\\S+\\s*", "", p[, x]))) 
names(r2) = paste0(names(p),".Detail")

p1 = cbind(r[,1,drop=F],r2[,1,drop=F])
names(p1)=c("ICD9PCS","ICD9PCS.Detail")
p2 = cbind(r[,2,drop=F],r2[,2,drop=F])
names(p2)=c("ICD9PCS","ICD9PCS.Detail")
p3 = cbind(r[,3,drop=F],r2[,3,drop=F])
names(p3)=c("ICD9PCS","ICD9PCS.Detail")

p7 = unique(rbind(p1,p2,p3))
> nrow(p7)
[1] 1301

> n = count(p7[,1])
> n1 = n[n$freq>1,]
> nrow(n1)
[1] 0

write.csv(p7, paste(dst, "outpt_ICD9_PCS.csv", sep=""), row.names=FALSE)

f = "outpt_0531Y17/outpt_ICD9_PCS.csv"
pcs9 = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
d0 = p7[!(p7$ICD9PCS %in% unique(pcs9$ICD9PCS)), ]
pcs9 = rbind(pcs9, d0)
write.csv(pcs9, paste(dst, "outpt_ICD9_PCS.csv", sep=""), row.names=FALSE)



p4 = cbind(r[,4,drop=F],r2[,4,drop=F])
names(p4)=c("ICD10PCS","ICD10PCS.Detail")
p5 = cbind(r[,5,drop=F],r2[,5,drop=F])
names(p5)=c("ICD10PCS","ICD10PCS.Detail")
p6 = cbind(r[,6,drop=F],r2[,6,drop=F])
names(p6)=c("ICD10PCS","ICD10PCS.Detail")

p8 = unique(rbind(p4,p5,p6))
> nrow(p8)
[1] 123


> n = count(p8[,1])
> n1 = n[n$freq>1,]
> nrow(n1)
[1] 0

write.csv(p8, paste(dst, "outpt_ICD10_PCS.csv", sep=""), row.names=FALSE)


f = "outpt_0531Y17/outpt_ICD10_PCS_old.csv"
pp = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
names(p8)=names(pp)
d0 = p8[!(p8$ICD10PCS %in% unique(pp$ICD10PCS)), ]
pp0 = rbind(pp, d0)
write.csv(pp0, paste(dst, "outpt_ICD10_PCS.csv", sep=""), row.names=FALSE)



r = data.frame(sapply(41:46, function(x) gsub("(^\\S+).*", "\\1", outp[, x])))
names(r)=names(outp)[41:46]
outp[,c(41:46)]=r[,c(1:6)]

r = data.frame(sapply(41:46, function(x) gsub("(^\\S+).*", "\\1", outpq4[, x])))
names(r)=names(outpq4)[41:46]
outpq4[,c(41:46)]=r[,c(1:6)]


write.csv(outp, paste(dst, "outpt_claims_y13-16.csv", sep=""), row.names=FALSE)


fd = "outpt_0531Y17/outpt_claims_y13-16_old.csv"
outp = read.csv(paste(dst, fd, sep=""), header=T, stringsAsFactors = FALSE)

> nrow(outp)
[1] 240636
> nrow(outp2)
[1] 16818
outp0 = unique(rbind(outp, outpq4))
> nrow(outp0)
[1] 254344

> 240636+16818
[1] 257454


write.csv(outp0, paste(dst, "outpt_claims_y13-16.csv", sep=""), row.names=FALSE)


#update the missing age 
b = unique(out[out$Age!="999" & out$Age!="" & out$Age!="90+",c(1,4,21)])
b$age = as.integer(b$Age)
> nrow(b)
[1] 110559



t = aggregate(b[,c(3:4)], b[,1,drop=F], FUN=max)
nrow(t)
[1] 53416



t$age16 = as.integer(t$age) + 2016 - as.integer(t$Paid.Date)
t = t[,-c(2:3)]


a = out[out$Encrypted.Member.ID %in% t$Encrypted.Member.ID & (out$Age=="999" | out$Age==""),]
> nrow(a)
[1] 515


fcom = intersect(names(t),names(a))
d = merge(x=a, y=t, by=fcom, all.x=T)
d$Age = as.character(d$age16 - (2016-as.integer(d$Paid.Date)))
#drop age16
d = d[,c(-83)]
nrow(d)
[1] 515

c = out[!(out$Encrypted.Member.ID %in% t$Encrypted.Member.ID) | (out$Encrypted.Member.ID %in% t$Encrypted.Member.ID & out$Age!="999" & out$Age!=""),]
nrow(out)-nrow(d)
[1] 253829

> nrow(c)
[1] 253829



out0 = rbind(c, d)
> nrow(out0)
[1] 254344


out0 = unique(out0)
> nrow(out0)
[1] 254344



write.csv(out0, paste(dst, "outpt_claims_y13-16.csv", sep=""), row.names=FALSE)



# outpatient Yes/No data fields
# "Regular.Temp" "Network.Indicator"  





#5. process pharmacy claims
options(width=200)
library(plyr)
src="/watson/quest_eh/data/original_files/2017MAY04/"
dst="/watson/quest_eh/workspace/lcao/data/"

f1 = "pharmacy.claims.2014and2016.csv"
rx1 = read.csv(paste(src, f1, sep=""), header=T, stringsAsFactors = FALSE)

f2 = "pharmacy.claims.2013and2015.csv"
rx2 = read.csv(paste(src, f2, sep=""), header=T, stringsAsFactors = FALSE)

#select columns
rx = unique(rbind(rx1, rx2))
> nrow(rx1)
[1] 1016379
> nrow(rx2)
[1] 1022285

#unique
> nrow(rx)
[1] 1589998



src2="/watson/quest_eh/data/original_files/2017JUN05/UpdatedTable2-AND-Q42016data/"
f = "PharmacyClaims.2016Q4.incurred.csv"
#rx = read.csv(paste(src2, f, sep=""), header=T, stringsAsFactors = FALSE, allowEscapes = T)
library(data.table)
rxq4 = fread(paste(src2, f, sep=""), header=T)
rxq4 = data.frame(rxq4)
rxq4 = unique(rxq4)

> nrow(rxq4)
[1] 113918


#rx1 <- read.table(paste(src2, f, sep=""), header = TRUE, sep = ",", stringsAsFactors = FALSE, quote = "\\\\\"")
#rx = read.csv(text=gsub("(^\"|\"$)","", readLines(paste(src2, f, sep=""))))

#demographics
k = unique(rx[,c(1,5,25)])
> nrow(k)
[1] 83611


> nrow(unique(k[,c(1:2)]))
[1] 82447

k = unique(rx[,c(1,5,25)])
m = count(k[,c(1,2)])
t1 = m[m$freq==1,]
k1 = k[k$Encrypted.Member.ID %in% t1$Encrypted.Member.ID,]

t2 = m[m$freq>1,]
k2 = k[k$Encrypted.Member.ID %in% t2$Encrypted.Member.ID,]
> nrow(k2)
[1] 2328
k2 = k2[order(k2$Encrypted.Member.ID),]
k2 = k2[k2$Ethnicity!="N/A",]
> nrow(k2)
[1] 1164

> nrow(k1)
[1] 81283
> nrow(k1)+nrow(k2)
k = rbind(k1,k2)
k = k[order(k$Encrypted.Member.ID),]
> nrow(k)
[1] 82447


write.csv(k, paste(dst, "rx_member_gender_race.csv", sep=""), row.names=FALSE)

f = "rx_0519Y17/rx_member_gender_race.csv"
mem = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
k0 = k[!(k$Encrypted.Member.ID %in% unique(mem$Encrypted.Member.ID)), ]
mem0 = rbind(mem, k0)
write.csv(mem0, paste(dst, "rx_member_gender_race.csv", sep=""), row.names=FALSE)




#daw
daw = unique(rx[,c(36),drop=F])
daw$DAW.Detail = gsub("^\\S+\\s*", "", daw$DAW)
daw$DAW = gsub("(^[0-9]+):.*", "\\1", daw$DAW)
daw = daw[daw$DAW!="",]
write.csv(daw, paste(dst, "rx_DAW.csv", sep=""), row.names=FALSE)

#ndc
ndc = unique(rx[,c(31:35,38,42)])
> length(unique(rx[,31]))
[1] 19053
> nrow(ndc)
[1] 19053

ndc$NDC = gsub("(^[0-9]{11}).*", "\\1", ndc$NDC)


write.csv(ndc, paste(dst, "rx_NDC.csv", sep=""), row.names=FALSE)

f = "rx_0519Y17/rx_NDC.csv"
old = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
ndc0 = ndc[!(ndc$NDC %in% unique(old$NDC)), ]
ndc1 = rbind(old, ndc0)
write.csv(ndc1, paste(dst, "rx_NDC.csv", sep=""), row.names=FALSE)


#remove plan relationship, plan product, benefit package, mental health carrier, external medical indicator, external pharmacy indicator, 
#Vendor.Specialty.Drug.Indicator, extra claim vendor, provider type, plan provider type
#member months, subscriber months, 
#services, Claimants, Subscribers, 
rx= unique(rx[,c(1:3,6:8,10,12:15,17,20:24,26:31,36:37,39:40,44,47:67,69,72,75:87)])
> nrow(rx)
[1] 1589998

rxq4= unique(rxq4[,c(1:3,6:8,10,12:15,17,20:24,26:31,36:37,39:40,44,47:67,69,72,75:87)])
> nrow(rxq4)
[1] 113918



rx$NDC = gsub("(^[0-9]{11}).*", "\\1", rx$NDC)
rx$DAW = gsub("(^[0-9]+):.*", "\\1", rx$DAW)

write.csv(rx, paste(dst, "rx_y13-16.csv", sep=""), row.names=FALSE)


rxq4$NDC = gsub("(^[0-9]{11}).*", "\\1", rxq4$NDC)
rxq4$DAW = gsub("(^[0-9]+):.*", "\\1", rxq4$DAW)


rx0 = unique(rbind(rx, rxq4))
> nrow(rx0)
[1] 1658444


write.csv(rx0, paste(dst, "rx_y13-16.csv", sep=""), row.names=FALSE)

f = "rx_0519Y17/rx_y13-16.csv"
rx_old = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)

rx = rx0


#update the missing age 
b = unique(rx[rx$Age!="999" & rx$Age!="" & rx$Age!="90+",c(1,4,21)])
b$age = as.integer(b$Age)
> nrow(b)
[1] 288018





#library(plyr)
#t = ddply(b[,c(3:4)], .(Encrypted.Member.ID), numcolwise(max))

#library(dplyr)
#t = k2 %>%
#  group_by(Encrypted.Member.ID) %>%
#  summarise_each(funs(max))
#t = data.frame(t)

t = aggregate(b[,c(3:4)], b[,1,drop=F], FUN=max)
> nrow(t)
[1] 83486



t$age16 = as.integer(t$age) + 2016 - as.integer(t$Paid.Date)
t = t[,-c(2:3)]

a = rx[rx$Encrypted.Member.ID %in% t$Encrypted.Member.ID & (rx$Age=="999" | rx$Age==""),]
> nrow(a)
[1] 4630


fcom = intersect(names(t),names(a))
d = merge(x=a, y=t, by=fcom, all.x=T)
d$Age = as.character(d$age16 - (2016-as.integer(d$Paid.Date)))
#drop age16
d = d[,c(-65)]
> nrow(d)
[1] 4630



c = rx[!(rx$Encrypted.Member.ID %in% t$Encrypted.Member.ID) | (rx$Encrypted.Member.ID %in% t$Encrypted.Member.ID & rx$Age!="999" & rx$Age!=""),]
nrow(rx)-nrow(d)
[1] 1653814

> nrow(c)
[1] 1653814

rx0 = unique(rbind(c, d))
> nrow(rx0)
[1] 1658200


write.csv(rx0, paste(dst, "rx_y13-16.csv", sep=""), row.names=FALSE)


fd = "rx_0519Y17/rx_y13-16.csv"
rx = read.csv(paste(dst, fd, sep=""), header=T, stringsAsFactors = FALSE)

fd1 = "rx_0519Y17/rx_NDC.csv"
ndc = read.csv(paste(dst, fd1, sep=""), header=T, stringsAsFactors = FALSE)


# rx Yes/No data fields
# in data: "Regular.Temp" "Formulary"  "Refill" "Network.Indicator"  
# in dnc mapping: "Maintenance.Drug" "Specialty.Drug"



#7. check the question answer frequency
options(width=200)
library(plyr)
src="/watson/quest_eh/data/original_files/2017MAY04/"
dst="/watson/quest_eh/workspace/lcao/data/"

fq = "Custom.table2.2014.part1.csv"
q1 = read.csv(paste(src, fq, sep=""), header=T, stringsAsFactors = FALSE)
q1 = unique(q1)

fq = "Custom.table2.2014.part2.csv"
q2 = read.csv(paste(src2, fq, sep=""), header=T, stringsAsFactors = FALSE)
q2 = unique(q2)

fq = "Custom.table2.2014.part3.csv"
q3 = read.csv(paste(src, fq, sep=""), header=T, stringsAsFactors = FALSE)
q3 = unique(q3)

fq = "Custom.table2.2014.part4.csv"
q4 = read.csv(paste(src, fq, sep=""), header=T, stringsAsFactors = FALSE)
q4 = unique(q4)

fq = "Custom.table2.2014.part5.csv"
q5 = read.csv(paste(src, fq, sep=""), header=T, stringsAsFactors = FALSE)
q5 = unique(q5)

q = rbind(q1,q2,q3,q4,q5)

> nrow(q1)
[1] 904379
> nrow(q2)
[1] 932833
> nrow(q3)
[1] 914817
> nrow(q4)
[1] 894857
> nrow(q5)
[1] 330794
> nrow(q)
[1] 3977680



> names(q)
[1] "Encrypted.Member.ID"     "Age"                     "Question"                "Selected.Answer"         "Fill.In.Answer"          "Score.Date"              "Employee.Status"        
[8] "Quest.Employment.Status"








#create question list
qslist = unique(q[!is.na(q$Selected.Answer) & q$Selected.Answer!="",c(3),drop=F])
> nrow(qslist)
[1] 263
qflist = unique(q[!is.na(q$Fill.In.Answer) & q$Fill.In.Answer!="",c(3),drop=F])
> nrow(qflist)
[1] 13

qslist=qslist[order(qslist$Question),,drop=F]
qflist=qflist[order(qflist$Question),,drop=F]

write.csv(qslist, paste(dst, "HRA_SelectedQ_y14.csv", sep=""), row.names=FALSE)
write.csv(qflist, paste(dst, "HRA_FillInQ_y14.csv", sep=""), row.names=FALSE)

#after sorting up the question, upload
library(xlsx)
qlf = "HRA_0523Y17/HRA_Qlist_y14.xlsx"
ql <- read.xlsx(paste(dst, qlf, sep=""), sheetName="HRA_Q_y14")

#merge question
fcom = intersect(names(q),names(ql))
q1 = merge(x=q, y=ql, by=fcom, all.x=T)
ck = count(q1$QN)
q1 = q1[,-c(1)]
> nrow(q1)
[1] 3977680

q1 = unique(q1)
> nrow(q1)
[1] 3955348

q11 = q1[q1$Qtype==1 | q1$Qtype==3,-c(4,9)]
q12 = q1[q1$Qtype==2 | Q1$Qtype==3,-c(3,9)]
> nrow(q11)
[1] 3928056
> nrow(q12)
[1] 139283
> nrow(q)
[1] 3977680
> nrow(q11)+nrow(q12)
[1] 4067339


agg1 = aggregate(data=q11, Selected.Answer ~ QN, function(x) length(unique(x)))
names(agg1)[2]="counts"
agg2 = aggregate(data=q11, Selected.Answer ~ QN, function(x) sort(unique(x)))
names(agg2)[2]="answers"
agg2 = agg2[,c(-1),drop=F]
agg11 = cbind(agg1, agg2)

> which(agg11$counts>10)
[1]   1   7  22  24  26 139 179
> agg11[which(agg11$counts>10),]$counts
[1]  12 159  11  21  12  12  36
> agg11[which(agg11$counts>10),]$QN
[1]   1   7  22  24  26 140 180

b = as.character(unlist(agg11$answers[1]))
b = sort(b)
a = b[6:12]
> a
[1] "More than"         "More than 3"       "More than 3 t"     "More than 3 ti"    "More than 3 tim"   "More than 3 time"  "More than 3 times"
q11[q11$QN==1 & q11$Selected.Answer %in% a,]$Selected.Answer=a[7]
> count(q11[q11$QN==1,]$Selected.Answer)
x  freq
1                     211
2                 0 30399
3                 1   461
4                 2   143
5                 3    34
6 More than 3 times    62
7              <NA>   698

> b = as.character(unlist(agg11$answers[7]))
> b = sort(b)
> b
[1] ""                               "B"                              "Be"                             "Beg"                            "Bega"                          
[6] "Began"                          "Began m"                        "Began ma"                       "Began man"                      "Began mana"                    
[11] "Began manag"                    "Began managi"                   "Began managin"                  "Began managing"                 "Began managing a"              
[16] "Began managing a n"             "Began managing a ne"            "Began managing a new"           "Began managing a newl"          "Began managing a newly"        
[21] "Began managing a newly d"       "Began managing a newly diagn"   "I"                              "I h"                            "I ha"                          
[26] "I hav"                          "I have"                         "I have n"                       "I have no"                      "I have not"                    
[31] "I have not p"                   "I have not pa"                  "I have not par"                 "I have not part"                "I have not parti"              
[36] "I have not partic"              "I have not partici"             "I have not particip"            "I have not participa"           "I have not participat"         
[41] "I have not participate"         "I have not participated"        "I have not participated i"      "I have not participated in"     "I have not participated in a"  
[46] "I have p"                       "I have pa"                      "I have par"                     "I have part"                    "I have parti"                  
[51] "I have partic"                  "I have partici"                 "I have particip"                "I have participa"               "I have participat"             
[56] "I have participate"             "I have participated"            "I have participated i"          "I have participated in"         "I have participated in a"      
[61] "I have participated in a w"     "I have participated in a wel"   "I have participated in a welln" "Im"                             "Imp"                           
[66] "Impr"                           "Impro"                          "Improv"                         "Improve"                        "Improved"                      
[71] "Improved d"                     "Improved di"                    "Improved die"                   "Improved diet"                  "In"                            
[76] "Inc"                            "Incr"                           "Incre"                          "Increa"                         "Increas"                       
[81] "Increase"                       "Increased"                      "Increased e"                    "Increased ex"                   "Increased exe"                 
[86] "Increased exer"                 "Increased exerc"                "Increased exerci"               "Increased exercis"              "Increased exercise"            
[91] "L"                              "Lo"                             "Los"                            "Lost"                           "Lost w"                        
[96] "Lost we"                        "Lost wei"                       "Lost weig"                      "Lost weigh"                     "Lost weight"                   
[101] "Q"                              "Qu"                             "Qui"                            "Quit"                           "Quit t"                        
[106] "Quit to"                        "Quit tob"                       "Quit toba"                      "Quit tobac"                     "Quit tobacc"                   
[111] "Quit tobacco"                   "Quit tobacco/"                  "Quit tobacco/n"                 "Quit tobacco/ni"                "Quit tobacco/nic"              
[116] "Quit tobacco/nico"              "Quit tobacco/nicot"             "Quit tobacco/nicoti"            "Quit tobacco/nicotin"           "Quit tobacco/nicotine"         
[121] "R"                              "Re"                             "Rec"                            "Rece"                           "Recei"                         
[126] "Receiv"                         "Receive"                        "Received"                       "Received p"                     "Received pr"                   
[131] "Received pre"                   "Received prev"                  "Received preve"                 "Received preven"                "Received prevent"              
[136] "Received preventi"              "Received preventiv"             "Received preventive"            "Received preventive s"          "Received preventive sc"        
[141] "Received preventive scr"        "Received preventive scre"       "Received preventive scree"      "Received preventive screeni"    "Received preventive screenin"  
[146] "S"                              "Sa"                             "Saw"                            "Saw m"                          "Saw my"                        
[151] "Saw my p"                       "Saw my ph"                      "Saw my phy"                     "Saw my phys"                    "Saw my physi"                  
[156] "Saw my physic"                  "Saw my physici"                 "Saw my physicia"                "Saw my physician"              
> a1 = b[2:22]
> a2 = b[23:63]
> a3 = b[64:74]
> a4 = b[75:90]
> a5 = b[91:100]
> a6 = b[101:120]
> a7 = b[121:145]
> a8 = b[146:159]


> count(q11[q11$QN==7,]$Selected.Answer)
x  freq
1                                 12789
2    Began managing a newly diagn  1133
3  I have participated in a welln 10813
4                   Improved diet  7375
5              Increased exercise  6469
6                     Lost weight  5251
7           Quit tobacco/nicotine   638
8    Received preventive screenin  5298
9                Saw my physician  8169
10                           <NA>   698



> b
[1] "I do not have access to exercise fac"                "I do not have access to exercise facil"              "I do not have access to exercise facilities"        
[4] "I do not have time"                                  "I think exercise is boring"                          "I'm self conscious about how I look when I exercise"
[7] "I'm self-conscious about how I look when I ex"       "I'm self-conscious about how I look when I exercise" "I'm too tired"                                      
[10] "Lack of access to exercise facilities"               "No time"                                            

> a2 = b[6:8]
> a3 = b[c(4,11)]
> a1 = b[c(1:3, 10)]
> a1
[1] "I do not have access to exercise fac"        "I do not have access to exercise facil"      "I do not have access to exercise facilities" "Lack of access to exercise facilities"      
> a2
[1] "I'm self-conscious about how I look when I ex"       "I'm self-conscious about how I look when I exercise"
> a3
[1] "I do not have time" "No time"        

> count(q11[q11$QN==22,]$Selected.Answer)
x  freq
1         I do not have access to exercise facilities  1907
2                                  I do not have time 11241
3                          I think exercise is boring  1173
4 I'm self-conscious about how I look when I exercise   836
5                                       I'm too tired  5918
6                                                <NA>   698


> b = as.character(unlist(agg11$answers[179]))
> b = sort(b)
> b
[1] ""              "A"             "Al"            "Alw"           "Alwa"          "Alway"         "Always"        "N"             "Ne"            "Nea"           "Near"          "Nearl"        
[13] "Nearly"        "Nearly A"      "Nearly Al"     "Nearly Alw"    "Nearly Alwa"   "Nearly Alway"  "Nearly Always" "Nev"           "Neve"          "Never"         "S"             "Se"           
[25] "Sel"           "Seld"          "Seldo"         "Seldom"        "So"            "Som"           "Some"          "Somet"         "Someti"        "Sometim"       "Sometime"      "Sometimes"    
> a1 = b[2:7]
> a1[6]
[1] "Always"
> a2 = b[8:19]
> a2[12]
[1] "Nearly Always"
> a3 = b[20:22]
> a3[3]
[1] "Never"
> a4 = b[23:28]
> a4[6]
[1] "Seldom"
> a5 = b[29:36]
> a5[8]
[1] "Sometimes"
> q11[q11$QN==180 & q11$Selected.Answer %in% a1,]$Selected.Answer=a1[6]
> q11[q11$QN==180 & q11$Selected.Answer %in% a2,]$Selected.Answer=a2[12]
> q11[q11$QN==180 & q11$Selected.Answer %in% a3,]$Selected.Answer=a3[3]
> q11[q11$QN==180 & q11$Selected.Answer %in% a4,]$Selected.Answer=a4[6]
> q11[q11$QN==180 & q11$Selected.Answer %in% a5,]$Selected.Answer=a5[8]
> count(q11[q11$QN==180,]$Selected.Answer)
x  freq
1                1367
2        Always   427
3 Nearly Always  1357
4         Never 15407
5        Seldom  7412
6     Sometimes  5318
7          <NA>   698


> nrow(q11)
[1] 3918675
> q11 = unique(q11)
> nrow(q11)
[1] 3918203


write.csv(q12, paste(dst, "HRA_data_FillIn_y14.csv", sep=""), row.names=FALSE)

write.csv(q11, paste(dst, "HRA_data_Selected_y14.csv", sep=""), row.names=FALSE)



#read in questions
fi = "HRA_0523Y17/HRA_data_Selected_y14.csv"
q11 = read.csv(paste(dst, fi, sep=""), header=T, stringsAsFactors = FALSE)

#further changed questions:
#3, 9, 30, 44, 104, 114, 124, 160, 164, 

t  = vapply(agg11$answers, paste, collapse=",", character(1L))
t  = sapply(agg11$answers, paste, collapse=",")


agg11$answers  = vapply(agg11$answers, paste, collapse=",", character(1L))
write.csv(agg11, paste(dst, "HRA_Qrefer_y14.csv", sep=""), row.names=FALSE)


#check questions answered with multiple times by the same member 
t = count(q11[,c(1,2,5,6,7)])
t1 = t[t$freq>1,]
t2 = count(t1[,5,drop=F])
> t2[which(t2$freq>200),1]
[1]   7  47 126
> t2[which(t2$freq>200),2]
[1] 10124  3147  2317


#percentage of NA for each of the question

e = aggregate(data=q11, Selected.Answer ~ QN, function(x) (sum(is.na(x))+sum(x=="N/A")+sum(x==""))/nrow(q11))



src2="/watson/quest_eh/data/original_files/2017JUN05/UpdatedTable2-AND-Q42016data/"

fq = "Custom.table2_2016.part1_6.1.2017.csv"
q1 = read.csv(paste(src2, fq, sep=""), header=T, stringsAsFactors = FALSE)
#q1 = unique(q1)

fq = "Custom.table2_2016.part2_6.1.2017.csv"
q2 = read.csv(paste(src2, fq, sep=""), header=T, stringsAsFactors = FALSE)
#q2 = unique(q2)

fq = "Custom.table2_2016.part3_6.1.2017.csv"
q3 = read.csv(paste(src2, fq, sep=""), header=T, stringsAsFactors = FALSE)
#q3 = unique(q3)

fq = "Custom.table2_2016.part4_6.1.2017.csv"
q4 = read.csv(paste(src2, fq, sep=""), header=T, stringsAsFactors = FALSE)
#q4 = unique(q4)

fq = "Custom.table2_2016.part5_6.1.2017.csv"
q5 = read.csv(paste(src2, fq, sep=""), header=T, stringsAsFactors = FALSE)
#q5 = unique(q5)

fq = "Custom.table2_2016.part6_6.1.2017.csv"
q6 = read.csv(paste(src2, fq, sep=""), header=T, stringsAsFactors = FALSE)
#q6 = unique(q6)

#q=unique(rbind(q1,q2,q3,q4,q5,q6))
q = rbind(q1,q2,q3,q4,q5,q6)



> nrow(q)
[1] 4754622

> nrow(q1)
[1] 912268
> nrow(q2)
[1] 792922
> nrow(q3)
[1] 877536
> nrow(q4)
[1] 924491
> nrow(q5)
[1] 979340
> nrow(q6)
[1] 268065


> ck = q[q$Question.ID=="" | q$Answer.ID=="",]
> nrow(ck)





#m = q[!is.na(q$Selected.Answer) & q$Selected.Answer!="",]
m = q[is.na(q$Fill.In.Answer) & !(q$Question.ID %in% c(147,148)),]
m1 = unique(m[,c(3,9)])
m1 = m1[m1$Question!="",]
m1 = m1[order(m1$Question.ID),]


n = q[!is.na(q$Fill.In.Answer) | q$Question.ID %in% c(147,148),]
n1 = unique(n[,c(3,9)])

> nrow(m)
[1] 4551167
> nrow(n)
[1] 203455
> nrow(q)
[1] 4754622
> nrow(q)-nrow(m)
[1] 203455


a = unique(m[m$Question!="" & m$Selected.Answer!="",c(3,9,4,10)])
a = a[order(a$Question,a$Question.ID,a$Selected.Answer),]
b = unique(n[n$Question!="",c(3,9,4,10)])
b = b[order(b$Question,b$Question.ID,b$Selected.Answer,b$Answer.ID),]
ab = rbind(a,b)
write.csv(ab, paste(dst, "HRA_question_Y16.csv", sep=""), row.names=FALSE)

t2 = count(m1[,2])
t2 = t2[t2$freq>1,]
m1 = rbind(m1,n1)

m2  = m1[m1$Question.ID %in% t2$x,]
rownames(m2)=1:nrow(m2)
#For your gestational diabetes are you\x85
m2[16,]$Question="For your gestational diabetes are you....."
m3 = m2[grep("^For", m2$Question),]
m4 = m2[!(rownames(m2) %in% rownames(m3)),]
m4$Question.ID=m4$Question.ID*100
m2 = rbind(m3,m4)

p1 = m[m$Question %in% m4$Question,]

p2 = m[!(m$Question %in% m4$Question),]
nrow(p2)+nrow(p1) == nrow(m)
p1$Question.ID=p1$Question.ID*100
m = rbind(p1,p2)



#count the freq of a specific answer to a specific question
mp = m[,c(1,9,10)]
qf = count(mp)

> nrow(mp)
[1] 4551167
> nrow(qf)
[1] 4012564




qf = qf[order(qf$Encrypted.Member.ID, qf$Question.ID, qf$Answer.ID),]
#to generate t, it takes a while, need to optimize this function 
#t = ddply(qf, c("Encrypted.Member.ID","Question.ID"), summarise, freq=max(freq))
#an alternative way to generate t
t = aggregate(freq ~ Encrypted.Member.ID + Question.ID, data=qf, max) 

> nrow(t)
[1] 3912369



#merge
fcom = intersect(names(t),names(qf))
mf = merge(x=t, y=qf, by=fcom, all.x=T)

> nrow(mf)
[1] 3983360



#mf is the result after the max frequency logic is applied
#identify those ones with a single m4 record 
g1 = mf[mf$Question.ID %in% m2$Question.ID,]

g2 = g1[g1$Question.ID %in% m4$Question.ID,]

g3 = count(g2[,c(1,2)])
g4 = g3[g3$freq==1,][,-c(3)]

> nrow(g4)
[1] 736547


#identify those ones who have a single No answer to m4
fcom = intersect(names(g1), names(g4))
g5 = merge(x=g4, y=g1, by=fcom, all.x=T)
g5 = g5[g5$Answer.ID<4000,-c(3,4)]
g5$Question.ID=g5$Question.ID/100
g5$remove = 1

fcom = intersect(names(g1), names(g5))
g6 = merge(x=g5, y=g1, by=fcom, all.y=T)


sum(!is.na(g6$remove))
[1] 1368
g11 = g6[is.na(g6$remove),-c(3)]
> nrow(g11)
[1] 792377

g12 = mf[!(mf$Question.ID %in% m2$Question.ID),]
nrow(mf)-nrow(g11)-nrow(g12)
[1] 1368


mf1 = rbind(g11, g12)
> nrow(mf1)
[1] 3981992



write.csv(mf1, paste(dst, "HRA_Select_Y16.csv", sep=""), row.names=FALSE)


write.csv(n, paste(dst, "HRA_Fillin_Y16.csv", sep=""), row.names=FALSE)

f = "HRA_Select_Y16.csv"
hra = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)

library(xlsx)
f = "HRA_question_Y16.xlsx"
ql <- read.xlsx(paste(dst,f, sep=""), sheetName=paste("HRA_question_Y16", sep=""))


f = "HRA_0608Y17/HRA_sameQ_multipleA_y16.csv"
agg = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)

#find multiple answers per question per users
q = hra[,-c(3)]
q1 = count(q[,c(1:2)])
q2 = q1[q1$freq>1,]

> nrow(q2)
[1] 42942




fcom = intersect(names(q2),names(q))
qm = merge(x=q2[,-c(3)], y=q, by=fcom)
> nrow(qm)
[1] 113678




agg1 = aggregate(data=qm, Answer.ID ~ Encrypted.Member.ID + Question.ID, function(x) length(unique(x)))
names(agg1)[3]="counts"

agg2 = aggregate(data=qm, Answer.ID ~ Encrypted.Member.ID + Question.ID, function(x) sort(unique(x)))
agg2 = agg2[,-c(1:2),drop=F]
agg = cbind(agg1, agg2)

> nrow(agg)
[1] 42942



agg$Answer.ID  = vapply(agg$Answer.ID, paste, collapse=",", character(1L))

write.csv(agg, paste(dst, "HRA_sameQ_multipleA_y16.csv", sep=""), row.names=FALSE)


#the following statments don't work
#agg2 = with(q, aggregate(Selected.Answer ~ Question, FUN = function(x) c(AS=unique(x), ASN=length(unique(x))) ))
#agg2 = with(q, aggregate(Selected.Answer, list(Question), function(x) {c(AS=unique(x), ASN=length(unique(x)))}))


#read in the priority reference

f = "HRA_Y16/HRA_Selected_y16.csv"
hra = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)

#find multiple answers per question per users
q = hra[,-c(3)]
q1 = count(q[,c(1:2)])
q2 = q1[q1$freq>1,]

> nrow(q2)
[1] 42942




fcom = intersect(names(q2),names(q))
qm = merge(x=q2[,-c(3)], y=q, by=fcom)
> nrow(qm)
[1] 113678



a2 = q1[q1$freq==1,]
fcom = intersect(names(a2),names(q))
qn = merge(x=a2[,-c(3)], y=q, by=fcom)
> nrow(qn)
[1] 3868314
> nrow(qm)
[1] 113678
> nrow(q)-nrow(qn)-nrow(qm)
[1] 0

> nrow(unique(qm[,c(2:3)]))
[1] 517



f = "HRA_Y16/hra_question_review_2016_v3.csv"
pr = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)

pr1 = unique(pr[,c(5,9,13)])
> nrow(pr1)
[1] 592

names(pr1)
[1] "Question.ID"   "HRA_ANSWER_ID"     "Priority"
names(pr1)=c("Question.ID","Answer.ID","PRI")


hra[hra$Question.ID==11000,]$Question.ID=110
q = hra[,-c(3)]
q1 = count(q[,c(1:2)])
q2 = q1[q1$freq>1,]
> nrow(q2)
[1] 42942

fcom = intersect(names(q2),names(q))
qm = merge(x=q2[,-c(3)], y=q, by=fcom)
> nrow(qm)
[1] 113678



#remove question ID = 2, 3

qm1 = qm[!(qm$Question.ID %in% c(2,3)), ]

qm12 = qm[(qm$Question.ID %in% c(2,3)), ]
> nrow(qm1)
[1] 113291
> nrow(qm12)
[1] 387
> nrow(qm1)+nrow(qm12)-nrow(qm)
[1] 0


fcom = intersect(names(qm1),names(pr1))
qm2 = merge(x=qm1, y=pr1, by=fcom, all.x=T)

> unique(qm2$PRI)
[1] 4 3 2 1 5 6
> sum(is.na(qm2$PRI))
[1] 0
> names(qm2)
[1] "Question.ID"         "Answer.ID"           "Encrypted.Member.ID" "PRI"                
nrow(qm2[,c(1,2,3)])
[1] 113291




h = aggregate(PRI ~ Encrypted.Member.ID + Question.ID, data=qm2, min) 
> nrow(h)
[1] 42750

nrow(unique(qm2[,c(3,1)]))
[1] 42750


fcom = intersect(names(qm2),names(h))
qm3 = merge(x=qm2, y=h, by=fcom)

> nrow(qm3)
[1] 71212

t = unique(qm3[,c(2,1,4)])
t1 = count(t[,c(1,2)])
q4 = t1[t1$freq>1,]
unique(q4$Question.ID)
[1] 4030  833



qm31=qm3[!(qm3$Question.ID %in% c(4030,833)),c(2,1,4)]
qm32=qm3[qm3$Question.ID %in% c(4030,833),c(2,1,4)]
nrow(qm31)+nrow(qm32)-nrow(qm3)
[1] 0
> nrow(qm31)
[1] 22789
> nrow(qm32)
[1] 52389


qm4 = rbind(qm12, qm32)
> nrow(qm4)
[1] 52776


qm51 = aggregate(data=qm4, Answer.ID ~ Encrypted.Member.ID + Question.ID, function(x) sort(unique(x)))
qm51$Answer.ID  = vapply(qm51$Answer.ID, paste, collapse=",", character(1L))

nrow(unique(qm51[,c(1,2)]))
[1] 20153

nrow(unique(qm4[,c(1,2)]))
[1] 20153


#combine
qm6 = rbind(qm31, qm51)

> nrow(qm6)
[1] 42942
> nrow(unique(qm[,c(1,2)]))
[1] 42942



qm7 = rbind(qn, qm6)
write.csv(qm7, paste(dst, "HRA_Select_processed_Y16.csv", sep=""), row.names=FALSE)
> nrow(qm7)
[1] 3911256

> names(qm7)
[1] "Encrypted.Member.ID" "Question.ID"         "Answer.ID"          
> nrow(unique(qm7[,c(1:2)]))
[1] 3911256

> length(unique(qm7[,1]))
[1] 34549



#reshape from long form to wide

f = "HRA_Y16/HRA_Select_processed_Y16.csv"
qs = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
names(qs)[3]="q"
> nrow(qs)
[1] 3911256


f = "HRA_Y16/HRA_Fillin_processed_Y16.csv"
qf = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
names(qf)[3]="q"
> nrow(qf)
[1] 143551



qq = rbind(qs,qf)
> nrow(qq)
[1] 4054807




qw = reshape(qq, idvar = "Encrypted.Member.ID", timevar = "Question.ID", direction = "wide")
> nrow(qw)
[1] 34549
> length(unique(qq[,1]))
[1] 34549


write.csv(qw, paste(dst, "HRA_wide_Y16.csv", sep=""), row.names=FALSE)

f = "HRA_Y16/HRA_age_status_Y16.csv"
m = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
f = "HRA_Y16/HRA_Select_Y16.csv"
s = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)


#generate the multiple relationship list
fi = "inpt_0518Y17/inpt_claims_y13-16.csv"
inpd = read.csv(paste(dst, fi, sep=""), header=T, stringsAsFactors = FALSE)
k1 = unique(inpd[,c(1,3,16)])
k1 = k1[k1$Detailed.Relationship != "N/A",]
t = count(k1[,1])
t1 = t[t$freq>1,]
k2 = k1[k1$Encrypted.Member.ID %in% t1$x,]
k2 = k2[order(k2[,1]),]


k1 = unique(inpd[,c(1,3,16)])
k1 = k1[k1$Detailed.Relationship != "N/A",]
t = count(k1[,1])
t1 = t[t$freq>1,]
k2 = k1[k1$Encrypted.Member.ID %in% t1$x,]
k2 = k2[order(k2[,1]),]


write.csv(k2, paste(dst, "inpt_memb_relationship.csv", sep=""), row.names=FALSE)

fi = "rx_0519Y17/rx_y13-16.csv"
rx = read.csv(paste(dst, fi, sep=""), header=T, stringsAsFactors = FALSE)
k1 = unique(rx[,c(1,3,16)])
write.csv(k2, paste(dst, "rx_memb_relationship.csv", sep=""), row.names=FALSE)

fi = "prof_0520Y17/prof_claims_y13-16.csv"
pf = read.csv(paste(dst, fi, sep=""), header=T, stringsAsFactors = FALSE)
k1 = unique(pf[,c(1,3,16)])
write.csv(k2, paste(dst, "pf_memb_relationship.csv", sep=""), row.names=FALSE)


fi = "outpatient.claims.2013to2016.csv"
outpd = read.csv(paste(src, fi, sep=""), header=T, stringsAsFactors = FALSE)
k1 = unique(outpd[,c(1,3,23)])
write.csv(k2, paste(dst, "outpt_memb_relationship.csv", sep=""), row.names=FALSE)











summary(as.numeric(gsub("\\$|,","", inpd$Billed[1:10])))

as.numeric(sub("%", "", inpd$In.Network.Billed..[1:6]))
as.numeric(gsub("\\%", "", inpd$In.Network.Billed..[1:6]))







src="/watson/quest_eh/data/original_files/2017MAY04/"
f2 = "outpatient.claims.2013to2016.csv"
outpd = read.csv(paste(src, f2, sep=""), header=T, stringsAsFactors = FALSE)


f3 = "pharmacy.claims.2014and2016.csv"
pht = read.csv(paste(src, f3, sep=""), header=T, stringsAsFactors = FALSE)


#tips to do the grouping and aggreate functions
# aggregate
aggregate(df$Value, by = list(df$Gene), max)
aggregate(Value ~ Gene, data = df, max)

# tapply
tapply(df$Value, df$Gene, max)

# split + lapply
lapply(split(df, df$Gene), function(y) max(y$Value))

# plyr
require(plyr)
ddply(df, .(Gene), summarise, Value = max(Value))

# dplyr
require(dplyr)
df %>% group_by(Gene) %>% summarise(Value = max(Value))

# data.table
require(data.table)
dt <- data.table(df)
dt[ , max(Value), by = Gene]

# doBy
require(doBy)
summaryBy(Value~Gene, data = df, FUN = max)

# sqldf
require(sqldf)
sqldf("select Gene, max(Value) as Value from df group by Gene", drv = 'SQLite')

# ave
df[as.logical(ave(df$Value, df$Gene, FUN = function(x) x == max(x))),]


