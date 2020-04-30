# An ETL example to process HSA type of data

options(width=200)
library(plyr)
src="/watson/quest_eh/data/original_files/2017MAY04/"
dst="/watson/quest_eh/workspace/lcao/data/"


src3="/watson/quest_eh/data/original_files/2017JUN26/Update2014HRAdata/"

fq = "Custom.table2_2014.part1_6.23.2017.csv"
q1 = read.csv(paste(src3, fq, sep=""), header=T, stringsAsFactors = FALSE)


fq = "Custom.table2_2014.part2_6.23.2017.csv"
q2 = read.csv(paste(src3, fq, sep=""), header=T, stringsAsFactors = FALSE)


fq = "Custom.table2_2014.part3_6.23.2017.csv"
q3 = read.csv(paste(src3, fq, sep=""), header=T, stringsAsFactors = FALSE)


fq = "Custom.table2_2014.part4_6.23.2017.csv"
q4 = read.csv(paste(src3, fq, sep=""), header=T, stringsAsFactors = FALSE)


fq = "Custom.table2_2014.part5_6.23.2017.csv"
q5 = read.csv(paste(src3, fq, sep=""), header=T, stringsAsFactors = FALSE)


q = rbind(q1,q2,q3,q4,q5)

> nrow(q)
[1] 4317021
> nrow(q1)
[1] 973918
> nrow(q2)
[1] 885507
> nrow(q3)
[1] 873321
> nrow(q4)
[1] 936460
> nrow(q5)
[1] 647815


> nrow(q[q$Question=="Divorce/Seperation",])
[1] 39

q[which(q$Question=="Divorce/Seperation"),]$Question="Divorce/Separation"





t = which(q$Question.ID==153 & q$Selected.Answer=="Do not know or unsure" & q$Answer.ID==563)
> length(t)
NULL


t = which(q$Question.ID==156 & q$Selected.Answer=="Do not know" & q$Answer.ID==558)
> length(t)
NULL

#issue 7
k = q[is.na(q$Question.ID),]
> nrow(k)
[1] 8792


k= k[k$Question!="" & k$Selected.Answer!="",]
> nrow(k)
[1] 8246


k=k[k$Question!="Depression",]
> nrow(k)
[1] 8108


k = k[,-c(9,10)]

m = q[q$Fill.In.Answer=="" & !(q$Question.ID %in% c(147,148)),]
m1 = unique(m[,c(3,9)])
m1 = m1[m1$Question!="" & !is.na(m1$Question.ID),]
m1 = m1[order(m1$Question.ID),]


n = q[q$Fill.In.Answer!="" | q$Question.ID %in% c(147,148),]
n1 = unique(n[,c(3,9)])
n1 = n1[n1$Question!="" & !is.na(n1$Question.ID),]

> nrow(m)
[1] 4142352
> nrow(n)
[1] 174669
> nrow(q)
[1] 4317021
> nrow(q)-nrow(m)-nrow(n)
[1] 0



a = unique(m[m$Question!="" & m$Selected.Answer!="" & !is.na(m$Question.ID) & !is.na(m$Answer.ID),c(3,9,4,10)])
a = a[order(a$Question,a$Question.ID,a$Selected.Answer),]
b = unique(n[n$Question!="" & !is.na(n$Question.ID) & !is.na(n$Answer.ID),c(3,9,4,10)])
b[!(b$Question.ID %in% c(147,48)),]$Selected.Answer=""
b = unique(b)
b = b[order(b$Question,b$Question.ID,b$Selected.Answer,b$Answer.ID),]
ab = rbind(a,b)
nrow(ab)
1031
write.csv(ab, paste(dst, "HRA_question_Y14.csv", sep=""), row.names=FALSE)



f = "HRA_Y15/HRA_question_Y15.csv"
h15= read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)

h15$have=1
fcom=names(ab)
hh = merge(x=h15, y=ab, by=fcom, all.y=T)






#merge with question dictionary of Y15
q = qo
f = "HRA_Y15/HRA_question_Y15.csv"
qa15 = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)

qa15[c(184:187),]$Question="For your gestational diabetes are you"
fcom = names(qa15)
qm = merge(x=q, y=qa15, by=fcom)
> nrow(qm)
[1] 4091396

k = q[is.na(q$Question.ID),]
k= k[k$Question!="" & k$Selected.Answer!="",]
k=k[k$Question!="Depression",]
k = k[,-c(9,10)]
> nrow(k)
[1] 8108

fcom = intersect(names(k),names(qa15))
k2 = merge(x=k, y=qa15, by=fcom)
[1] 8106
> head(k2)

qo = q
q = rbind(qm,k2)
> nrow(q)
[1] 4099504

> nrow(qo)-nrow(q)
[1] 217517



q = q[,c(5,6,1,3,7,8,9,10,2,4)]
m = q[q$Fill.In.Answer=="" & !(q$Question.ID %in% c(147,148)),]
m1 = unique(m[,c(3,9)])
m1 = m1[m1$Question!="" & !is.na(m1$Question.ID),]
m1 = m1[order(m1$Question.ID),]


n = q[q$Fill.In.Answer!="" | q$Question.ID %in% c(147,148),]
n1 = unique(n[,c(3,9)])
n1 = n1[n1$Question!="" & !is.na(n1$Question.ID),]



t2 = count(m1[,2])
t2 = t2[t2$freq>1,]
m1 = rbind(m1,n1)

> nrow(m)
[1] 3925823
> nrow(n)
[1] 173681
> nrow(q)-nrow(m)-nrow(n)
[1] 0


write.csv(n, paste(dst, "HRA_Fillin_Y14.csv", sep=""), row.names=FALSE)

m2  = m1[m1$Question.ID %in% t2$x,]
rownames(m2)=1:nrow(m2)
#For your gestational diabetes are you\x85
m3 = m2[grep("^For", m2$Question),]
m4 = m2[!(rownames(m2) %in% rownames(m3)),]
m4$Question.ID=m4$Question.ID*100
m2 = rbind(m3,m4)

p1 = m[m$Question %in% m4$Question,]
p2 = m[!(m$Question %in% m4$Question),]
nrow(p2)+nrow(p1) == nrow(m)
p1$Question.ID=p1$Question.ID*100
m = rbind(p1,p2)

> nrow(m)
[1] 3925823



#count the freq of a specific answer to a specific question
mp = m[,c(1,9,10)]
qf = count(mp)

> nrow(mp)
[1] 3925823
> nrow(qf)
[1] 3629045


qf = qf[order(qf$Encrypted.Member.ID, qf$Question.ID, qf$Answer.ID),]
#to generate t, it takes a while, need to optimize this function 
#t = ddply(qf, c("Encrypted.Member.ID","Question.ID"), summarise, freq=max(freq))
#an alternative way to generate t
t = aggregate(freq ~ Encrypted.Member.ID + Question.ID, data=qf, max) 

> nrow(t)
[1] 3606327



#merge
fcom = intersect(names(t),names(qf))
mf = merge(x=t, y=qf, by=fcom, all.x=T)


> fcom
[1] "Encrypted.Member.ID" "Question.ID"         "freq"               

> nrow(mf)
[1] 3608430


#mf is the result after the max frequency logic is applied
#identify those ones with a single m4 record 
g1 = mf[mf$Question.ID %in% m2$Question.ID,]

g2 = g1[g1$Question.ID %in% m4$Question.ID,]

g3 = count(g2[,c(1,2)])
g4 = g3[g3$freq==1,][,-c(3)]
> nrow(g4)
[1] 654661



#identify those ones who have a single No answer to m4
fcom = intersect(names(g1), names(g4))
g5 = merge(x=g4, y=g1, by=fcom, all.x=T)
g5 = g5[g5$Answer.ID<4000,-c(3,4)]
g5$Question.ID=g5$Question.ID/100
g5$remove = 1

fcom = intersect(names(g1), names(g5))
g6 = merge(x=g5, y=g1, by=fcom, all.y=T)


sum(!is.na(g6$remove))
[1] 1205

g11 = g6[is.na(g6$remove),-c(3)]
g12 = mf[!(mf$Question.ID %in% m2$Question.ID),]

> nrow(g11)
[1] 705131
> nrow(g12)
[1] 2902094
> nrow(mf)-nrow(g11)-nrow(g12)
[1] 1205





mf1 = rbind(g11, g12)
write.csv(mf1, paste(dst, "HRA_Select_Y14.csv", sep=""), row.names=FALSE)
> nrow(mf1)
[1] 3607225


write.csv(n, paste(dst, "HRA_Fillin_Y14.csv", sep=""), row.names=FALSE)


#find multiple answers per question per users
q = mf1[,-c(3)]
q1 = count(q[,c(1:2)])
q2 = q1[q1$freq>1,]

> nrow(q2)
[1] 1743


fcom = intersect(names(q2),names(q))
qm = merge(x=q2[,-c(3)], y=q, by=fcom)

> nrow(qm)
[1] 3619


agg1 = aggregate(data=qm, Answer.ID ~ Encrypted.Member.ID + Question.ID, function(x) length(unique(x)))
names(agg1)[3]="counts"

agg2 = aggregate(data=qm, Answer.ID ~ Encrypted.Member.ID + Question.ID, function(x) sort(unique(x)))
agg2 = agg2[,-c(1:2),drop=F]
agg = cbind(agg1, agg2)

> nrow(agg)
[1] 1743



agg$Answer.ID  = vapply(agg$Answer.ID, paste, collapse=",", character(1L))
write.csv(agg, paste(dst, "HRA_sameQ_multipleA_y14.csv", sep=""), row.names=FALSE)





###########################
###### process the fill-in questions
########Y15

f = "HRA_Fillin_Y14.csv"
s14 = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)

> nrow(s14)
[1] 173681


h14 = s14[s14$Question.ID %in% c(147,148),c(1,4,9)]
> nrow(h14)
[1] 67772


hf = count(h14[h14$Question.ID==147,c(1)])
names(hf)[1]=names(h14)[1]

length(unique(h14[,1]))
length(unique(hf[,1]))
[1] 31140


t = which(h14$Question.ID==147)
h14[t,]$Selected.Answer=h14[t,]$Selected.Answer*12
t1 = aggregate(Selected.Answer ~ Encrypted.Member.ID, h14, FUN=sum)

fcom = intersect(names(hf),names(t1))
ht = merge(x=hf, y=t1, by=fcom)
ht$Height = ht$Selected.Answer*1./ht$freq

summary(ht$Height)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
36.00   63.00   66.00   65.92   68.00  107.00 


t0 = ht[,c(1,4)]
names(t0)[2]="Fill.In.Answer"
t0$Question.ID=147
t0 = t0[,c(1,3,2)]

hw = s14[s14$Question.ID %in% c(149,150),c(1,5,9)]
t1 = aggregate(Fill.In.Answer ~ Encrypted.Member.ID+Question.ID, hw, FUN=mean)

> summary(t1[t1$Question.ID==149,])
Encrypted.Member.ID  Question.ID  Fill.In.Answer 
Length:31117        Min.   :149   Min.   : 80.0  
Class :character    1st Qu.:149   1st Qu.:143.0  
Mode  :character    Median :149   Median :170.0  
Mean   :149   Mean   :176.5  
3rd Qu.:149   3rd Qu.:200.0  
Max.   :149   Max.   :550.0  
> summary(t1[t1$Question.ID==150,])
Encrypted.Member.ID  Question.ID  Fill.In.Answer  
Length:31110        Min.   :150   Min.   :  8.22  
Class :character    1st Qu.:150   1st Qu.: 23.82  
Mode  :character    Median :150   Median : 27.14  
Mean   :150   Mean   : 28.47  
3rd Qu.:150   3rd Qu.: 31.66  
Max.   :150   Max.   :108.49  



hl = s14[s14$Question.ID %in% c(152),c(1,5,9)]
t2 = aggregate(Fill.In.Answer ~ Encrypted.Member.ID+Question.ID, hl, FUN=min)

summary(t2)
> summary(t2)
Encrypted.Member.ID  Question.ID  Fill.In.Answer  
Length:12864        Min.   :152   Min.   : 20.00  
Class :character    1st Qu.:152   1st Qu.: 70.00  
Mode  :character    Median :152   Median : 76.00  
Mean   :152   Mean   : 75.53  
3rd Qu.:152   3rd Qu.: 80.00  
Max.   :152   Max.   :180.00  




hh = s14[s14$Question.ID %in% c(151,153,154),c(1,5,9)]
t3 = aggregate(Fill.In.Answer ~ Encrypted.Member.ID+Question.ID, hh, FUN=max)


length(unique(t0[,1]))
[1] 31140
#
length(unique(t1[,1]))
[1] 31117


length(unique(t2[,1]))
[1] 12864
#systolic and diastolic are missing a couple of pairs
length(unique(t3[t3$Question.ID==151,1]))
[1] 12862


> length(unique(t3[t3$Question.ID==153,1]))
[1] 4521
> length(unique(t3[t3$Question.ID==154,1]))
[1] 5344


t = rbind(t0,t1,t2,t3)
> nrow(t)
[1] 128958


write.csv(t, paste(dst, "HRA_Fillin_processed_Y14.csv", sep=""), row.names=FALSE)




#########################
###### process the age and status across years

q = q[,c(5,6,1,3,7,8,9,10,2,4)]
s = unique(q[,c(1,2,6,7,8)])
s[s$Age=="90+",]$Age="91"
s$age = as.integer(s$Age)
> nrow(s)
[1] 31327



t1 = aggregate(age ~ Encrypted.Member.ID+Score.Date, s, FUN=max)
nrow(t1)
length(unique(t1[,1]))
[1] 31166
> nrow(t1)
[1] 31166


t1$Age = as.character(t1$age)
t1[t1$age==91,]$Age="90+"
t1=t1[,-c(3)]




s1 = count(q[,c(1,6,7)])

d1 = aggregate(freq ~ Encrypted.Member.ID+Score.Date, s1, FUN=max)
fcom = intersect(names(d1),names(s1))
m1 = merge(x=s1, y=d1, by=fcom)
m1 = m1[,-c(3)]
m2 = count(m1[,c(1),drop=F])
m2 = m2[m2$freq>1,]
> nrow(m2)
[1] 0

> nrow(m1)
[1] 31166

s2 = count(q[,c(1,6,8)])

d2 = aggregate(freq ~ Encrypted.Member.ID+Score.Date, s2, FUN=max)
fcom = intersect(names(d2),names(s2))
m3 = merge(x=s2, y=d2, by=fcom)
m3 = m3[,-c(3)]
m4 = count(m3[,c(1),drop=F])
m4 = m4[m4$freq>1,]
> nrow(m4)
[1] 0

> nrow(m3)
[1] 31166


fcom=names(t1)[1:2]
a = merge(x=t1, y=m1, by=fcom)
b = merge(x=a, y=m3, by=fcom)


write.csv(b, paste(dst, "HRA_age_status_Y14.csv", sep=""), row.names=FALSE)



#############
f= "HRA_Select_Y14.csv"
hra = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
o = which(hra$Question.ID==11000)
hra[hra$Question.ID==11000,]$Question.ID=110


#find multiple answers per question per users
q = hra[,-c(3)]
q1 = count(q[,c(1:2)])
q2 = q1[q1$freq>1,]
nrow(q2)

> nrow(q2)
[1] 1743



fcom = intersect(names(q2),names(q))
qm = merge(x=q2[,-c(3)], y=q, by=fcom)


a2 = q1[q1$freq==1,]
fcom = intersect(names(a2),names(q))
qn = merge(x=a2[,-c(3)], y=q, by=fcom)

> nrow(qm)
[1] 3619
> nrow(qn)
[1] 3603606
> nrow(q)-nrow(qm)-nrow(qn)
[1] 0
> nrow(unique(qm[,c(2:3)]))
[1] 383



f = "HRA_Y16/hra_question_review_2016_v3.csv"
pr = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)

pr1 = unique(pr[,c(5,9,13)])
> nrow(pr1)
[1] 592

names(pr1)
[1] "Question.ID"   "HRA_ANSWER_ID"     "Priority"
names(pr1)=c("Question.ID","Answer.ID","PRI")


hra[hra$Question.ID==11000,]$Question.ID=110
#q = hra[,-c(3)]
#q1 = count(q[,c(1:2)])
#q2 = q1[q1$freq>1,]

#fcom = intersect(names(q2),names(q))
#qm = merge(x=q2[,-c(3)], y=q, by=fcom)



#remove question ID = 2, 3

qm1 = qm[!(qm$Question.ID %in% c(2,3)), ]

qm12 = qm[(qm$Question.ID %in% c(2,3)), ]

> nrow(qm)-nrow(qm1)-nrow(qm12)
[1] 0
> nrow(qm1)
[1] 3546
> nrow(qm12)
[1] 73




fcom = intersect(names(qm1),names(pr1))
qm2 = merge(x=qm1, y=pr1, by=fcom, all.x=T)

unique(qm2$PRI)
sum(is.na(qm2$PRI))
[1] 4 3 2 1 5 6
[1] 0
> names(qm2)
[1] "Question.ID"         "Answer.ID"           "Encrypted.Member.ID" "PRI"                
nrow(qm2[,c(1,2,3)])
[1] 3546





h = aggregate(PRI ~ Encrypted.Member.ID + Question.ID, data=qm2, min) 
> nrow(h)
[1] 1710
> nrow(unique(qm2[,c(3,1)]))
[1] 1710




fcom = intersect(names(qm2),names(h))
qm3 = merge(x=qm2, y=h, by=fcom)

> nrow(qm3)
[1] 1728
 
t = unique(qm3[,c(2,1,4)])
t1 = count(t[,c(1,2)])
q4 = t1[t1$freq>1,]
unique(q4$Question.ID)
[1] 833




qm31=qm3[!(qm3$Question.ID %in% c(4030,833)),c(2,1,4)]
qm32=qm3[qm3$Question.ID %in% c(4030,833),c(2,1,4)]
nrow(qm31)+nrow(qm32)-nrow(qm3)
[1] 0

> nrow(qm31)
[1] 1694
> nrow(qm32)
[1] 34

qm4 = rbind(qm12, qm32)
> nrow(qm4)
[1] 107


qm51 = aggregate(data=qm4, Answer.ID ~ Encrypted.Member.ID + Question.ID, function(x) sort(unique(x)))
qm51$Answer.ID  = vapply(qm51$Answer.ID, paste, collapse=",", character(1L))

nrow(unique(qm51[,c(1,2)]))
[1] 49

nrow(unique(qm4[,c(1,2)]))
[1] 49




#combine
qm6 = rbind(qm31, qm51)
nrow(qm6)
[1] 1743

nrow(unique(qm[,c(1,2)]))
[1] 1743





qm7 = rbind(qn, qm6)
write.csv(qm7, paste(dst, "HRA_Select_processed_Y14.csv", sep=""), row.names=FALSE)

> nrow(qm7)
[1] 3605349
> names(qm7)
[1] "Encrypted.Member.ID" "Question.ID"         "Answer.ID"          
> nrow(unique(qm7[,c(1:2)]))
[1] 3605349
> length(unique(qm7[,1]))
[1] 31166




#reshape from long form to wide

f = "HRA_Select_processed_Y14.csv"
qs = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
names(qs)[3]="q"
> nrow(qs)
[1] 3605349




f = "HRA_Fillin_processed_Y14.csv"
qf = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
names(qf)[3]="q"
> nrow(qf)
[1] 128958



qq = rbind(qs,qf)
> nrow(qq)
[1] 3734307



qw = reshape(qq, idvar = "Encrypted.Member.ID", timevar = "Question.ID", direction = "wide")
nrow(qw)
length(unique(qq[,1]))
[1] 31166



write.csv(qw, paste(dst, "HRA_wide_Y14.csv", sep=""), row.names=FALSE)

