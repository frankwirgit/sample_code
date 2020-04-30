# An ETL example to process Lab data
# including data cleaning and de-dup

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


f151 = "Custom.table1.2015.part1.csv"
q151 = read.csv(paste(src, f151, sep=""), header=T, stringsAsFactors = FALSE)


f152 = "Custom.table1.2015.part2.csv"
q152 = read.csv(paste(src, f152, sep=""), header=T, stringsAsFactors = FALSE)


f153 = "Custom.table1.2015.part3.csv"
q153 = read.csv(paste(src, f153, sep=""), header=T, stringsAsFactors = FALSE)
qy15 = rbind(q151,q152,q153)


f141 = "Custom.table1.2014.part1.csv"
q141 = read.csv(paste(src, f141, sep=""), header=T, stringsAsFactors = FALSE)


f142 = "Custom.table1.2014.part2.csv"
q142 = read.csv(paste(src, f142, sep=""), header=T, stringsAsFactors = FALSE)

f143 = "Custom.table1.2014.part3.csv"
q143 = read.csv(paste(src, f143, sep=""), header=T, stringsAsFactors = FALSE)
qy14 = rbind(q141,q142,q143)


f131 = "Custom.table1.2013.part1.csv"
q131 = read.csv(paste(src, f131, sep=""), header=T, stringsAsFactors = FALSE)

f132 = "Custom.table1.2013.part2.csv"
q132 = read.csv(paste(src, f132, sep=""), header=T, stringsAsFactors = FALSE)

f133 = "Custom.table1.2013.part3.csv"
q133 = read.csv(paste(src, f133, sep=""), header=T, stringsAsFactors = FALSE)
qy13 = rbind(q131,q132,q133)

#create age mapping
#combine patients across years
demo16 = unique(qy16[,c(1,2,4,12)])
demo15 = unique(qy15[,c(1,2,4,12)])
demo14 = unique(qy14[,c(1,2,4,12)])
demo13 = unique(qy13[,c(1,2,4,12)])
demo = rbind(demo13, demo14, demo15, demo16)
demo = unique(demo)
demo = demo[order(demo$Encrypted.Member.ID,demo$Quest.Gender,demo$Age),]
> nrow(demo)
[1] 132939


#replace age="999" with NA
demo[is.na(demo$Age) | demo$Age=="999" | demo$Age=="",]$age = NA
t = ddply(demo[!is.na(demo$Age),], c("Encrypted.Member.ID"), summarise, Age = max(Age))
> nrow(t)
[1] 39938

#no age
noage = unique(demo[!(demo$Encrypted.Member.ID %in% t$Encrypted.Member.ID), c(1,3)])
> nrow(noage)
[1] 12237

> nrow(noage)+nrow(t)
[1] 52175



write.csv(noage, paste(dst, "lab_member_noage.csv", sep=""), row.names=FALSE)

#has age
fcom = intersect(names(t),names(demo))
t2 = merge(x=t, y=demo, by=fcom)
write.csv(t2, paste(dst, "lab_member_age.csv", sep=""), row.names=FALSE)


#exclude the no age from age file
f1 = "lab_0517Y17/lab_member_noage.csv"
noage = read.csv(paste(dst, f1, sep=""), header=T, stringsAsFactors = FALSE)
f2 = "lab_0517Y17/lab_member_age.csv"
hasage = read.csv(paste(dst, f2, sep=""), header=T, stringsAsFactors = FALSE)

age1 = noage[!(noage$Encrypted.Member.ID  %in% unique(hasage$Encrypted.Member.ID)),]

noage[noage$Encrypted.Member.ID=="ZTQN-QVQN-ADYV-QA",]
        
#patients has the same age in different years
ck = count(t2[,1])
ck1 = ck[ck$freq>1,]

extra = demo[demo$Encrypted.Member.ID %in% ck1$Encrypted.Member.ID,]

#create gender mapping
demo16 = unique(qy16[,c(1,4)])
demo15 = unique(qy15[,c(1,4)])
demo14 = unique(qy14[,c(1,4)])
demo13 = unique(qy13[,c(1,4)])
demo = rbind(demo13, demo14, demo15, demo16)
demo = unique(demo)
> nrow(demo)
[1] 52175
> length(unique(demo[,1]))
[1] 52175

write.csv(demo, paste(dst, "lab_member_gender.csv", sep=""), row.names=FALSE)


#lab data extraction by years
> names(qy16)
[1] "Encrypted.Member.ID"         "Quest.Clinical.History.Date" "Quest.Relationship"          "Quest.Gender"                "Quest.Clinical.Variable"     "Quest.Amended.Result"       
[7] "Quest.Reference.Range"       "Quest.LOINC.Code"            "Quest.Result.Indicator"      "Quest.Units.of.Measure"      "Quest.Employment.Status"     "Age"                        
[13] "Quest.Result"  

#extract columns needed for analysis
nrow(qy16)
[1] 1874446
qy16 = unique(qy16[,c(1,2,3,7,8,9,11,12,13)])
nrow(qy16)
[1] 1851954


sum(is.na(qy16$Quest.Result))
[1] 0
> sum(qy16$Quest.Result=="")
[1] 36714
> sum(qy16$Quest.Result=="na")
[1] 0
> sum(qy16$Quest.Result=="n/a")
[1] 0
> sum(qy16$Quest.Result=="NA")
[1] 0
> qy16 = qy16[qy16$Quest.Result!="",]
> nrow(qy16)
[1] 1815240


qy15 = unique(qy15[,c(1,2,3,7,8,9,11,12,13)])
nrow(qy15)
[1] 1726104
qy15 = qy15[qy15$Quest.Result!="",]
nrow(qy15)
[1] 1689115


qy14 = unique(qy14[,c(1,2,3,7,8,9,11,12,13)])
qy14 = qy14[qy14$Quest.Result!="",]

qy13 = unique(qy13[,c(1,2,3,7,8,9,11,12,13)])
qy13 = qy13[qy13$Quest.Result!="",]

> nrow(qy14)
[1] 1682229
> nrow(qy13)
[1] 1870908


#compensate the age

> unique(qy16[qy16$Encrypted.Member.ID=="ADMV-MDQU-SDYN-ZA",c(1,2,3,8)])
Encrypted.Member.ID Quest.Clinical.History.Date Quest.Relationship Age
646366    ADMV-MDQU-SDYN-ZA                        2016           Employee    
1161019   ADMV-MDQU-SDYN-ZA                        2016           Employee  24

> q = qy16[is.na(qy16$Age) | qy16$Age=="999" | qy16$Age=="",]
> nrow(q)
[1] 436155
#t2 are those ones who have age info

f = "lab_member_age.csv"
t2 = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)
t3 = t2[t2$Encrypted.Member.ID %in% unique(q$Encrypted.Member.ID),]
t4 = t3
t4$age = as.integer(t4$Age) + 2016 - as.integer(t4$Quest.Clinical.History.Date)
t4 = t4[,c(1,5)]
names(t4)[2]="Age"
t4 = ddply(t4, c("Encrypted.Member.ID"), summarise, Age = max(Age))

q = q[,c(-8)]

fcom = intersect(names(q),names(t4))
q1 = merge(x=q, y=t4, by=fcom, all.x=T)
q2 = q1[is.na(q1$Age),]
t5 = t2[t2$Encrypted.Member.ID %in% unique(q2$Encrypted.Member.ID),]
> nrow(t5)
[1] 0
q1 = q1[,c(1:7,9,8)]

q2 = qy16[!is.na(qy16$Age) & qy16$Age!="999" & qy16$Age!="",]



> nrow(q1)+nrow(q2)
[1] 1815240
> nrow(qy16)
[1] 1815240

qm16 = rbind(q2,q1)

write.csv(qm16, paste(dst, "lab_y16.csv", sep=""), row.names=FALSE)

f = "lab_0517Y17/lab_y16.csv"
lab = read.csv(paste(dst, f, sep=""), header=T, stringsAsFactors = FALSE)



#Y15
q = qy15[is.na(qy15$Age) | qy15$Age=="999" | qy15$Age=="",]
t3 = t2[t2$Encrypted.Member.ID %in% unique(q$Encrypted.Member.ID),]
t4 = t3
t4$age = as.integer(t4$Age) + 2016 - as.integer(t4$Quest.Clinical.History.Date)
t4 = t4[,c(1,5)]
names(t4)[2]="Age"
t4 = ddply(t4, c("Encrypted.Member.ID"), summarise, Age = max(Age))

q = q[,c(-8)]

fcom = intersect(names(q),names(t4))
q1 = merge(x=q, y=t4, by=fcom, all.x=T)
q2 = q1[is.na(q1$Age),]
t5 = t2[t2$Encrypted.Member.ID %in% unique(q2$Encrypted.Member.ID),]
> nrow(t5)
[1] 0
q1 = q1[,c(1:7,9,8)]

q2 = qy15[!is.na(qy15$Age) & qy15$Age!="999" & qy15$Age!="",]

qm15 = rbind(q2,q1)

write.csv(qm15, paste(dst, "lab_y15.csv", sep=""), row.names=FALSE)

#Y14
q = qy14[is.na(qy14$Age) | qy14$Age=="999" | qy14$Age=="",]
t3 = t2[t2$Encrypted.Member.ID %in% unique(q$Encrypted.Member.ID),]
t4 = t3
t4$age = as.integer(t4$Age) + 2016 - as.integer(t4$Quest.Clinical.History.Date)
t4 = t4[,c(1,5)]
names(t4)[2]="Age"
t4 = ddply(t4, c("Encrypted.Member.ID"), summarise, Age = max(Age))

q = q[,c(-8)]

fcom = intersect(names(q),names(t4))
q1 = merge(x=q, y=t4, by=fcom, all.x=T)
q2 = q1[is.na(q1$Age),]
t5 = t2[t2$Encrypted.Member.ID %in% unique(q2$Encrypted.Member.ID),]
> nrow(t5)
[1] 0
q1 = q1[,c(1:7,9,8)]

q2 = qy14[!is.na(qy14$Age) & qy14$Age!="999" & qy14$Age!="",]

qm14 = rbind(q2,q1)

> nrow(q1)
[1] 499053
> nrow(q2)
[1] 1183176
> nrow(qm14)
[1] 1682229
> nrow(qy14)
[1] 1682229
> nrow(q1)+nrow(q2)
[1] 1682229


write.csv(qm14, paste(dst, "lab_y14.csv", sep=""), row.names=FALSE)

#Y13
q = qy13[is.na(qy13$Age) | qy13$Age=="999" | qy13$Age=="",]
t3 = t2[t2$Encrypted.Member.ID %in% unique(q$Encrypted.Member.ID),]
t4 = t3
t4$age = as.integer(t4$Age) + 2016 - as.integer(t4$Quest.Clinical.History.Date)
t4 = t4[,c(1,5)]
names(t4)[2]="Age"
t4 = ddply(t4, c("Encrypted.Member.ID"), summarise, Age = max(Age))

q = q[,c(-8)]

fcom = intersect(names(q),names(t4))
q1 = merge(x=q, y=t4, by=fcom, all.x=T)
q2 = q1[is.na(q1$Age),]
t5 = t2[t2$Encrypted.Member.ID %in% unique(q2$Encrypted.Member.ID),]
> nrow(t5)
[1] 0

q1 = q1[,c(1:7,9,8)]
q2 = qy13[!is.na(qy13$Age) & qy13$Age!="999" & qy13$Age!="",]

qm13 = rbind(q2,q1)

> nrow(qm13)
[1] 1870908
> nrow(qy13)
[1] 1870908
> nrow(q1)
[1] 483832
> nrow(q2)
[1] 1387076
> nrow(q1)+nrow(q2)
[1] 1870908



write.csv(qm13, paste(dst, "lab_y13.csv", sep=""), row.names=FALSE)