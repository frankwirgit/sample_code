#
# This file includes multiple examples related to debugging
# Check the data contents, run summary stats to trace errors
# Plotting the results to do error analysis
# Check and de-dup records
#
# Note: the patient IDs used displayed in this file are not real IDs
# and the pateints have been de-identified to meet data security

#clean the workspace
rm(list = ls())


# Example 1: Analyze the impact due to the clean SG data loss to the generation of glycemic insights 

#read the glycemic insights test version
giorg = read.csv(paste0(src4, "CSP.GLYCEMIC_INSIGHTS_2020MAR30.csv"), header=T, sep=",", stringsAsFactors = FALSE)
nrow(giorg)
[1] 4787
names(giorg)
[1] "STREAMS_ID"           "PERSON_ID"            "INSIGHT_TS"           "INSIGHT_ID"          
[5] "EVENT_NAME"           "OUTCOME"              "INSIGHT_DELIVERED_AT" "CREATED_AT"          
[9] "UPDATED_AT"           "FOOD_ID" 

length(unique(giorg$PERSON_ID))
#merge with 37 users
gw = count(giorg, vars=c("PERSON_ID"))

#read the glycemic insights pulled from production
gjson = read.csv(paste0(src4, "glycemic_json.csv"), header=F, sep="|", stringsAsFactors = FALSE)
nrow(gjson)
[1] 4787
names(gjson)=c(names(giorg), "FOOD_NAME","INSIGHT_JSON_DATA")

length(unique(gjson$PERSON_ID))
[1] 37


#gt = count(unique(gjson[,c("PERSON_ID","INSIGHT_ID")]), vars="PERSON_ID")
#head(gt[order(gt$freq,decreasing = T),])


#read the glycemic insights pulled from mdtdevdb21
gtest1 = gtest
gtest = read.csv(paste0(src4, "ginsight_test.csv"), header=F, sep="|", stringsAsFactors = FALSE)

nrow(gtest)
[1] 13053
names(gtest)=c(names(giorg), "FOOD_NAME","INSIGHT_JSON_DATA")

length(unique(gtest$PERSON_ID))
[1] 37


gw = count(gjson, vars=c("PERSON_ID"))
gv = count(gtest, vars=c("PERSON_ID"))
names(gw)[2]="numb_prod"
names(gv)[2]="numb_test"
vw = merge(x=gw,y=gv,by="PERSON_ID")
vw$diff = vw$numb_test - vw$numb_prod
head(vw[order(vw$diff),])


#check a specific user
gw = count(gjson[gjson$PERSON_ID==50918827,], vars=c("PERSON_ID","INSIGHT_ID"))
gv = count(gtest[gtest$PERSON_ID==50918827,], vars=c("PERSON_ID","INSIGHT_ID"))
names(gw)[3]="numb_prod"
names(gv)[3]="numb_test"
idvw = merge(x=gw,y=gv,by=c("PERSON_ID","INSIGHT_ID"), all=T)

idvw[is.na(idvw)]=0
idvw$diff = idvw$numb_test - idvw$numb_prod
head(idvw[order(idvw$diff, decreasing = T),])

#check a specific glycemic 389
write.csv(gjson[(gjson$PERSON_ID==50918827 & gjson$INSIGHT_ID=='glycemic.389'),c("STREAMS_ID","PERSON_ID","INSIGHT_TS","INSIGHT_DELIVERED_AT","INSIGHT_JSON_DATA")], paste0(src4,"gprod.csv"), row.names=FALSE)
write.csv(gtest[(gtest$PERSON_ID==50918827 & gtest$INSIGHT_ID=='glycemic.389'),c("STREAMS_ID","PERSON_ID","INSIGHT_TS","INSIGHT_DELIVERED_AT","INSIGHT_JSON_DATA")], paste0(src4,"gtest.csv"), row.names=FALSE)


gjson$epoch = as.numeric(strptime(substring(gjson$INSIGHT_TS,1,19),"%Y-%m-%d-%H.%M.%S", tz="UTC"))
gjson$dt = as.Date(as.POSIXct(gjson$epoch, origin="1970-01-01"))
gjson$delivered_prod = ifelse(is.na(gjson$STREAMS_ID),0,1)

gtest$epoch = as.numeric(strptime(substring(gtest$INSIGHT_TS,1,19),"%Y-%m-%d-%H.%M.%S", tz="UTC"))
gtest$dt = as.Date(as.POSIXct(gtest$epoch, origin="1970-01-01"))
gtest$delivered_test = ifelse(is.na(gtest$STREAMS_ID),0,1)

length(unique(gjson$dt))
[1] 197
length(unique(gtest$dt))
[1] 186

#check the time window range
sort(unique(gjson$dt))
"2019-02-01", "2019-09-11" - "2020-03-29"
sort(unique(gtest$dt))
"2019-02-01" "2019-09-11" - "2020-03-15"

#define the range <= "2020-03-14"
nrow(gjson)
1] 4787
gjson=gjson[gjson$dt<="2020-03-14",]
nrow(gjson)
[1] 4330

j = count(gjson,vars="INSIGHT_ID")
head(j[order(j$freq, decreasing = T),], 10)

nrow(gtest)
[1] 13053
gtest=gtest[gtest$dt<="2020-03-14",]
nrow(gtest)
[1] 13017

length(unique(gjson$dt))
[1] 182
length(unique(gtest$dt))
[1] 185

t = count(gtest,vars="INSIGHT_ID")
head(t[order(t$freq, decreasing = T),], 10)

sum(gjson$delivered_prod)/nrow(gjson)*100.
[1] 16.14319
sum(gtest$delivered_test)/nrow(gtest)*100.
[1] 37.05923


length(unique(gjson$INSIGHT_ID))
[1] 73
length(unique(gtest$INSIGHT_ID))
[1] 98
length(setdiff(unique(gtest$INSIGHT_ID),unique(gjson$INSIGHT_ID)))
[1] 29
setdiff(unique(gtest$INSIGHT_ID),unique(gjson$INSIGHT_ID))
tt = setdiff(unique(gjson$INSIGHT_ID),unique(gtest$INSIGHT_ID))

t1  = gjson[(gjson$INSIGHT_ID %in% tt),]

count(t1, vars="INSIGHT_ID")

setdiff(unique(gjson[gjson$delivered_prod==1,]$INSIGHT_ID), unique(gtest[gtest$delivered_test==1,]$INSIGHT_ID))

#generalize the case of glycemic 389
#gj = gjson[gjson$INSIGHT_ID=='glycemic.389',c("PERSON_ID","INSIGHT_ID","INSIGHT_TS","epoch","dt","delivered_prod","INSIGHT_JSON_DATA")]
#gt = gtest[gtest$INSIGHT_ID=='glycemic.389',c("PERSON_ID","epoch","delivered_test","INSIGHT_JSON_DATA")]

gj = gjson[,c("PERSON_ID","INSIGHT_ID","epoch","INSIGHT_TS","dt","delivered_prod","INSIGHT_JSON_DATA")]
gt = gtest[,c("PERSON_ID","INSIGHT_ID","epoch","delivered_test","INSIGHT_JSON_DATA")]


library(plyr)
library(data.table)
library(stringr)

a = data.frame(str_locate(gj$INSIGHT_JSON_DATA,",meanAllOtherBin:"))
gj$meanT_prod = substring(gj$INSIGHT_JSON_DATA,16,a$start-1)
b = data.frame(str_locate(gj$INSIGHT_JSON_DATA,",events:\\["))
gj$meanA_prod = substring(gj$INSIGHT_JSON_DATA,a$end+1,b$start-1)
c = data.frame(str_locate(gj$INSIGHT_JSON_DATA,"\\]"))
gj$event_prod = substring(gj$INSIGHT_JSON_DATA,b$end,c$start)

a = data.frame(str_locate(gt$INSIGHT_JSON_DATA,",meanAllOtherBin:"))
gt$meanT_test = substring(gt$INSIGHT_JSON_DATA,16,a$start-1)
b = data.frame(str_locate(gt$INSIGHT_JSON_DATA,",events:\\["))
gt$meanA_test = substring(gt$INSIGHT_JSON_DATA,a$end+1,b$start-1)
c = data.frame(str_locate(gt$INSIGHT_JSON_DATA,"\\]"))
gt$event_test = substring(gt$INSIGHT_JSON_DATA,b$end,c$start)

gj = gj[,-c(7)]
gt = gt[,-c(5)]

#if we focus on delivered ones
gmt = merge(x=gj[gj$delivered_prod==1,], y=gt, by=c("PERSON_ID","INSIGHT_ID","epoch"), all.x=T)
nrow(gmt)
[1] 699
gnt=gmt[!is.na(gmt$delivered_test),]
nrow(gnt)
[1] 448

#in general 
gjt = merge(x=gj, y=gt, by=c("PERSON_ID","INSIGHT_ID","epoch"), all.x=T)
nrow(gjt)
1] 4330

#found in prod but not in test
length(which(is.na(gjt$delivered_test)))
[1] 662
gponly=gjt[is.na(gjt$delivered_test),]
nrow(gponly)
[1] 662
length(unique(gponly$PERSON_ID))
[1] 34
length(unique(gponly$dt))
[1] 96

length(unique(gponly$INSIGHT_ID))
[1] 51

sum(gponly$delivered_prod)/nrow(gponly)*100.
[1] 37.91541

#break down on insight level
go = count(gponly, vars="INSIGHT_ID")
names(go)[2]="nprod"
go = merge(x=go, y=j, by="INSIGHT_ID")
go$pct = go$nprod/go$freq*100.
summary(go$pct)
head(go[order(go$nprod,decreasing = T),],15)

#match with the insight timestamp
gboth=gjt[!is.na(gjt$delivered_test),]
nrow(gboth)
[1] 3668

length(unique(gboth$PERSON_ID))
[1] 37
length(unique(gboth$dt))
[1] 171
length(unique(gboth$INSIGHT_ID))
[1] 65

sum(gboth$delivered_prod)/nrow(gboth)*100.
[1] 12.21374
sum(gboth$delivered_test)/nrow(gboth)*100.
[1] 17.58451

#break down on insight level
go = count(gboth, vars="INSIGHT_ID")
names(go)[2]="nboth"
go = merge(x=go, y=j, by="INSIGHT_ID")
go$pct = go$nboth/go$freq*100.
summary(go$pct)
go = count(gboth, vars="INSIGHT_ID")
names(go)[2]="nboth"
go = merge(x=go, y=t, by="INSIGHT_ID")
go$pct = go$nboth/go$freq*100.
summary(go$pct)
head(go[order(go$nboth,decreasing = T),],15)


j = count(gjson,vars="INSIGHT_ID")
j = j[order(j$freq, decreasing = T),]
names(j)[2]="counts_prod"
jl = count(gjson[gjson$delivered_prod==1,],vars="INSIGHT_ID")
jl = jl[order(jl$freq, decreasing = T),]
names(jl)[2]="counts_del_prod"
j$INSIGHT_ID=as.character(gsub("glycemic.", "g", j$INSIGHT_ID))
jl$INSIGHT_ID=as.character(gsub("glycemic.", "g", jl$INSIGHT_ID))
j = merge(x=j, y=jl, by="INSIGHT_ID", all.x=T)
j[is.na(j$counts_del_prod),]$counts_del_prod=0

#head(j[order(j$freq, decreasing = T),], 10)

t = count(gtest,vars="INSIGHT_ID")
t = t[order(t$freq, decreasing = T),]
names(t)[2]="counts_test"
tl = count(gtest[gtest$delivered_test==1,],vars="INSIGHT_ID")
tl = tl[order(tl$freq, decreasing = T),]
names(tl)[2]="counts_del_test"
t$INSIGHT_ID=as.character(gsub("glycemic.", "g", t$INSIGHT_ID))
tl$INSIGHT_ID=as.character(gsub("glycemic.", "g", tl$INSIGHT_ID))
t = merge(x=t, y=tl, by="INSIGHT_ID", all.x=T)
t[is.na(t$counts_del_test),]$counts_del_test=0


#head(t[order(t$freq, decreasing = T),], 10)


jt = merge(x=j,y=t,by="INSIGHT_ID")


b = count(gboth,vars="INSIGHT_ID")
b = b[order(b$freq, decreasing = T),]
names(b)[2]="counts_both"
bl = count(gboth[gboth$delivered_prod==1,],vars="INSIGHT_ID")
bl = bl[order(bl$freq, decreasing = T),]
names(bl)[2]="counts_del_both"
b$INSIGHT_ID=as.character(gsub("glycemic.", "g", b$INSIGHT_ID))
bl$INSIGHT_ID=as.character(gsub("glycemic.", "g", bl$INSIGHT_ID))
b = merge(x=b, y=bl, by="INSIGHT_ID", all.x=T)
b[is.na(b$counts_del_both),]$counts_del_both=0

jtb = merge(x=jt,y=b,by="INSIGHT_ID")
jtb = jtb[order(jtb$counts_prod,decreasing=T),]

#barplot
barplot(j[j$gi_counts>10,]$gi_counts, names.arg = j[j$gi_counts>10,]$INSIGHT_ID,
        xlab = "GI type", ylab = "# GI", ylim=c(0,850),
        main = "GI counts per type in Production")

library(ggplot2)

#example
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity")

#created, by insight ID
jtb20= jtb[1:23,]
GI_type <- c(jtb20$INSIGHT_ID, jtb20$INSIGHT_ID,jtb20$INSIGHT_ID)
system <- c(rep("test", nrow(jtb20)),rep("prod", nrow(jtb20)),rep("both", nrow(jtb20)))
GI_count <- c(jtb20$counts_test,jtb20$counts_prod,jtb20$counts_both)
dd <- data.frame(GI_type,system,GI_count)

#ggplot(dd, aes(fill=system, y=GI_count, x=GI_type)) + 
  #geom_bar(position="stack", stat="identity")

dd=dd[order(dd$GI_count, decreasing = T),]
ggplot(mapping = aes(fill=system, y=GI_count, x = reorder(GI_type, -GI_count))) +
  geom_col(data = dd[dd$system == "test", ], position = "dodge") +
  geom_col(data = dd[dd$system == "prod", ], aes(group = system),
           width = 0.8, position = position_dodge(width = 0.9)) + 
  geom_col(data = dd[dd$system == "both", ], aes(group = system),
           width = 0.6, position = position_dodge(width = 0.7)) +
  ggtitle("GI created counts per GI type")

#created, by event name
e1 = e
e1$INSIGHT_ID=as.character(gsub("glycemic.", "g", e1$INSIGHT_ID))
jtb20=jtb
jtb20=merge(x=jtb20, y=e1,by="INSIGHT_ID")
jtb20 = aggregate(jtb20[,2:7], by=list(EVENT_NAME=jtb20$EVENT_NAME), FUN = sum )


EVENT_NAME <- c(jtb20$EVENT_NAME, jtb20$EVENT_NAME,jtb20$EVENT_NAME)
system <- c(rep("test", nrow(jtb20)),rep("prod", nrow(jtb20)),rep("both", nrow(jtb20)))
GI_count <- c(jtb20$counts_test,jtb20$counts_prod,jtb20$counts_both)
dd <- data.frame(EVENT_NAME,system,GI_count)

ggplot(mapping = aes(fill=system, y=GI_count, x = reorder(EVENT_NAME, -GI_count))) +
  geom_col(data = dd[dd$system == "test", ], position = "dodge", width=0.5) +
  geom_col(data = dd[dd$system == "prod", ], aes(group = system),
           width = 0.4, position = position_dodge(width = 0.4)) + 
  geom_col(data = dd[dd$system == "both", ], aes(group = system),
           width = 0.3, position = position_dodge(width = 0.3)) +
  ggtitle("GI created counts per GI type")

#delivered, by insight Id

# The palette with grey:
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
scale_colour_manual(values=cbPalette)

cbPalette <- c("#E69F00", "#0072B2", "#999999","#009E73", "#F0E442", "#56B4E9", "#D55E00", "#CC79A7")

GI_count <- c(jtb20$counts_del_test,jtb20$counts_del_prod,jtb20$counts_del_both)
dc <- data.frame(GI_type,system,GI_count)
ggplot(mapping = aes(fill=system, y=GI_count, x = reorder(GI_type, -GI_count))) +
  geom_col(data = dc[dc$system == "test", ], position = "dodge") +
  geom_col(data = dc[dc$system == "prod", ], aes(group = system),
           width = 0.8, position = position_dodge(width = 0.9)) + 
  geom_col(data = dc[dc$system == "both", ], aes(group = system),
           width = 0.6, position = position_dodge(width = 0.7)) +
  ggtitle("GI delivered counts per GI type") +
  scale_fill_manual(values=cbPalette)


#delivered, by event name
GI_count <- c(jtb20$counts_del_test,jtb20$counts_del_prod,jtb20$counts_del_both)
dc <- data.frame(EVENT_NAME,system,GI_count)
ggplot(mapping = aes(fill=system, y=GI_count, x = reorder(EVENT_NAME, -GI_count))) +
  geom_col(data = dc[dc$system == "test", ], position = "dodge",width=0.5) +
  geom_col(data = dc[dc$system == "prod", ], aes(group = system),
           width = 0.4, position = position_dodge(width = 0.4)) + 
  geom_col(data = dc[dc$system == "both", ], aes(group = system),
           width = 0.3, position = position_dodge(width = 0.3)) +
  ggtitle("GI delivered counts per GI type") +
  scale_fill_manual(values=cbPalette)


#can we put both the system (both,prod,test) and event name (timeblock, meal etc) together
dd$domain="created"
dc$domain="delivered"
cd = rbind(dd,dc)


#ggplot(dd, aes(fill=system, y=GI_count, x=GI_type)) + 
#geom_bar(position="stack", stat="identity")

ggplot(cd, aes(fill=domain, y=GI_count, x=EVENT_NAME)) + 
geom_bar(position="dodge")


ggplot(mapping=aes(fill=system, y=GI_count, x=reorder(EVENT_NAME,-GI_count))) + 
  geom_col(data=cd[cd$domain=="created",], position="dodge") +
  geom_col(data=cd[cd$domain=="delivered",], aes(group=system),
           fill="black", width=0.5,position = position_dodge(width = 0.9))




ggplot(mapping=aes(fill=system, y=GI_count, x=reorder(EVENT_NAME,-GI_count))) + 
  geom_col(data=cd[cd$domain=="created",], position="dodge") +
  geom_col(data=cd[cd$domain=="delivered",], aes(group=system, color="delivered"),
           fill="#333333", width=0.5,position = position_dodge(width = 0.9)) +
  scale_fill_manual("created", values=c("#FFCC00", "#00CC66", "#0099FF")) +
  scale_color_manual("domain",values = c("#999999"),
                     guide = guide_legend(override.aes = list(fill = "#333333")))
  

jtb20= jtb[1:17,]
GI_type <- c(jtb20$INSIGHT_ID, jtb20$INSIGHT_ID,jtb20$INSIGHT_ID)
system <- c(rep("test", nrow(jtb20)),rep("prod", nrow(jtb20)),rep("both", nrow(jtb20)))
GI_count <- c(jtb20$counts_test,jtb20$counts_prod,jtb20$counts_both)
dd <- data.frame(GI_type,system,GI_count)
dd$domain="created"
GI_count <- c(jtb20$counts_del_test,jtb20$counts_del_prod,jtb20$counts_del_both)
dd2 <- data.frame(GI_type,system,GI_count)
dd2$domain="delivered"
dd=rbind(dd,dd2)

ggplot(mapping=aes(fill=system, y=GI_count, x=reorder(GI_type,-GI_count))) + 
  geom_col(data=dd[dd$domain=="created",], position="dodge") +
  geom_col(data=dd[dd$domain=="delivered",], aes(group=system, color="delivered"),
           fill="#333333", width=0.5,position = position_dodge(width = 0.9)) +
  scale_fill_manual("created", values=c("#FFCC00", "#00CC66", "#0099FF")) +
  scale_color_manual("domain",values = c("#999999"),
                     guide = guide_legend(override.aes = list(fill = "#333333")))


#production only, by insight Id
gp1 = count(gponly,vars="INSIGHT_ID")
names(gp1)[2]="counts_prod"
gp2 = count(gponly[gponly$delivered_prod==1,], vars="INSIGHT_ID")
names(gp2)[2]="counts_del_prod"
gp1 = merge(x=gp1,y=gp2,by="INSIGHT_ID", all.x=T)
gp1[is.na(gp1$counts_del_prod),]$counts_del_prod=0
gp1=gp1[order(gp1$counts_prod,decreasing = T),]

jtb20= gp1[1:20,]
GI_type <- c(jtb20$INSIGHT_ID, jtb20$INSIGHT_ID)
domain <- c(rep("created", nrow(jtb20)),rep("delivered", nrow(jtb20)))
GI_count <- c(jtb20$counts_prod,jtb20$counts_del_prod)
dn <- data.frame(GI_type,domain,GI_count)
dn$GI_type=as.character(gsub("glycemic.", "g", dn$GI_type))


ggplot(mapping = aes(fill=domain, y=GI_count, x = reorder(GI_type, -GI_count))) +
  geom_col(data = dn[dn$domain == "created", ], position = "dodge") +
  geom_col(data = dn[dn$domain == "delivered", ], aes(group = domain),
           width = 0.8, position = position_dodge(width = 0.9)) +
  ggtitle("GI created/delivered in Production only")


#product only, by event name
gp1 = merge(x=gp1, y=e, by="INSIGHT_ID", all.x=T)
gp2 = aggregate(gp1$counts_prod, by=list(EVENT_NAME=gp1$EVENT_NAME), FUN=sum)
names(gp2)[2]="counts_prod"
#data.frame(tapply(gp1$counts_del_prod, gp1$EVENT_NAME, FUN=sum))
gp3 = aggregate(gp1$counts_del_prod, by=list(EVENT_NAME=gp1$EVENT_NAME), FUN=sum)
names(gp3)[2]="counts_del_prod"
gp2 = merge(x=gp2,gp3,by="EVENT_NAME")


jtb20= gp2
EVENT_NAME <- c(jtb20$EVENT_NAME, jtb20$EVENT_NAME)
domain <- c(rep("created", nrow(jtb20)),rep("delivered", nrow(jtb20)))
GI_count <- c(jtb20$counts_prod,jtb20$counts_del_prod)
dn <- data.frame(EVENT_NAME,domain,GI_count)


ggplot(mapping = aes(fill=domain, y=GI_count, x = reorder(EVENT_NAME, -GI_count))) +
  geom_col(data = dn[dn$domain == "created", ], position = "dodge", width=0.4) +
  geom_col(data = dn[dn$domain == "delivered", ], aes(group = domain),
           width = 0.3, position = position_dodge(width = 0.2)) +
  ggtitle("GI created/delivered in Production only")


#histogram plot

, position = position_dodge(width = 0.6)

hist(j$gi_counts)
x = j$INSIGHT_ID
h <- hist(x, breaks=20, xlab="GI type", ylim=c(0,850), ylab="# of GIs", main="Histogram of GI counts per type in Production", col="green")
xfit<-seq(min(x),max(x),length=55)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)



#match with deliver status
length(which(gboth$delivered_prod==gboth$delivered_test))
[1] 3265

length(which(gboth$meanT_prod==gboth$meanT_test & gboth$meanA_prod==gboth$meanA_test))
[1] 2962


#most restricted
length(which(gboth$meanT_prod==gboth$meanT_test & gboth$meanA_prod==gboth$meanA_test & gboth$event_prod==gboth$event_test))
[1] 2857
gk = gboth[(gboth$meanT_prod==gboth$meanT_test & gboth$meanA_prod==gboth$meanA_test & gboth$event_prod==gboth$event_test),]
nrow(gk)
length(unique(gk$PERSON_ID))

length(unique(gk$dt))

length(unique(gk$INSIGHT_ID))

sum(gk$delivered_prod)/nrow(gk)*100.
sum(gk$delivered_test)/nrow(gk)*100.

gd = gboth[(gboth$meanT_prod!=gboth$meanT_test | gboth$meanA_prod!=gboth$meanA_test | gboth$event_prod!=gboth$event_test),]
nrow(gd)
[1] 811

length(unique(gd$PERSON_ID))

length(unique(gd$dt))

length(unique(gd$INSIGHT_ID))

sum(gd$delivered_prod)/nrow(gd)*100.
sum(gd$delivered_test)/nrow(gd)*100.


[1] "glycemic.147" "glycemic.185" "glycemic.187" "glycemic.188" "glycemic.189" "glycemic.207" "glycemic.209" "glycemic.227"
[9] "glycemic.381" "glycemic.385" "glycemic.387" "glycemic.389" "glycemic.390" "glycemic.393" "glycemic.395" "glycemic.459"
[17] "glycemic.469" "glycemic.477" "glycemic.479" "glycemic.489" "glycemic.519" "glycemic.536" "glycemic.540" "glycemic.541"
[25] "glycemic.542" "glycemic.543" "glycemic.544" "glycemic.545" "glycemic.546" "glycemic.547" "glycemic.548" "glycemic.81" 
[33] "glycemic.91"  "glycemic.92"  "glycemic.93"  "glycemic.99" 

#find in test, but no in prod
hj = gjson[,c("PERSON_ID","INSIGHT_ID","epoch","delivered_prod","INSIGHT_JSON_DATA")]
ht = gtest[,c("PERSON_ID","INSIGHT_ID","epoch","INSIGHT_TS","dt","delivered_test","INSIGHT_JSON_DATA")]

a = data.frame(str_locate(hj$INSIGHT_JSON_DATA,",meanAllOtherBin:"))
hj$meanT_prod = substring(hj$INSIGHT_JSON_DATA,16,a$start-1)
b = data.frame(str_locate(hj$INSIGHT_JSON_DATA,",events:\\["))
hj$meanA_prod = substring(hj$INSIGHT_JSON_DATA,a$end+1,b$start-1)
c = data.frame(str_locate(hj$INSIGHT_JSON_DATA,"\\]"))
hj$event_prod = substring(hj$INSIGHT_JSON_DATA,b$end,c$start)


a = data.frame(str_locate(ht$INSIGHT_JSON_DATA,",meanAllOtherBin:"))
ht$meanT_test = substring(ht$INSIGHT_JSON_DATA,16,a$start-1)
b = data.frame(str_locate(ht$INSIGHT_JSON_DATA,",events:\\["))
ht$meanA_test = substring(ht$INSIGHT_JSON_DATA,a$end+1,b$start-1)
c = data.frame(str_locate(ht$INSIGHT_JSON_DATA,"\\]"))
ht$event_test = substring(ht$INSIGHT_JSON_DATA,b$end,c$start)


hj = hj[,-c(5)]
ht = ht[,-c(7)]

hjt = merge(x=hj, y=ht, by=c("PERSON_ID","INSIGHT_ID","epoch"), all.y=T)
nrow(hjt)
[1] 13017

length(which(is.na(hjt$delivered_prod)))
[1] 9349
htonly=hjt[is.na(hjt$delivered_prod),]
nrow(htonly)
[1] 9349

length(unique(htonly$PERSON_ID))
[1] 37
length(unique(htonly$dt))
[1] 162
length(unique(htonly$INSIGHT_ID))
[1] 90
sum(htonly$delivered_test)/nrow(htonly)*100.


#get the event name and insight Id mapping
ej = unique(gjson[,c("INSIGHT_ID","EVENT_NAME")])
et = unique(gtest[,c("INSIGHT_ID","EVENT_NAME")])
e = merge(x=ej, y=et, by=c("INSIGHT_ID","EVENT_NAME"), all=T)

#get the example
> m[m$freq==max(m$freq),]
PERSON_ID freq
13  50906241  786

> m = htonly[htonly$PERSON_ID==50906241 & htonly$delivered_test==0,]
> n = htonly[htonly$PERSON_ID==50906241 & htonly$delivered_test==1,]

m=merge(x=m[,c(1,2,3)], y=e, by="INSIGHT_ID")
n=merge(x=n[,c(1,2,3)], y=e, by="INSIGHT_ID")

m = merge(x=m[-c(4)],y=gtest,by=c("PERSON_ID","epoch","INSIGHT_ID"),all.x=T)
n = merge(x=n[-c(4)],y=gtest,by=c("PERSON_ID","epoch","INSIGHT_ID"),all.x=T)
write.csv(m,paste0(src4,"em.csv"), row.names=FALSE)
write.csv(n,paste0(src4,"en.csv"), row.names=FALSE)

n = gboth[(gboth$delivered_prod==1 & (gboth$meanT_prod!=gboth$meanT_test | gboth$meanA_prod!=gboth$meanA_test | gboth$event_prod!=gboth$event_test)),]
n = merge(x=n[,c(1,2,3)],y=gjson,by=c("PERSON_ID","epoch","INSIGHT_ID"),all.x=T)
n=n[,-c(14,15)]
n$domain='prod'
n2 = merge(x=n[,c(1,2,3)],y=gtest,by=c("PERSON_ID","epoch","INSIGHT_ID"),all.x=T)
n2=n2[,-c(14,15)]
n2$domain='test'
n = rbind(n,n2)
n = n[order(n$PERSON_ID,n$INSIGHT_ID,n$epoch,n$domain),]

write.csv(n,paste0(src4,"ediff.csv"), row.names=FALSE)

n=gponly[gponly$delivered_prod==1,]
nrow(n)
n = merge(x=n[,c(1,2,3)],y=gjson,by=c("PERSON_ID","epoch","INSIGHT_ID"),all.x=T)
n=n[,-c(14,15)]
write.csv(n,paste0(src4,"eponly.csv"), row.names=FALSE)

> unique(m$EVENT_NAME)
[1] "CALENDARDAY" "TIMEBLOCK"  
> unique(n$EVENT_NAME)
[1] "CALENDARDAY"    "allBolusEvents" "TIMEBLOCK"      "MEAL"  

m = merge(x=m[-c(4)],y=gtest,by=c("PERSON_ID","epoch","INSIGHT_ID"),all.x=T)
n = merge(x=n[-c(4)],y=gtest,by=c("PERSON_ID","epoch","INSIGHT_ID"),all.x=T)


#break down on insight level
go = count(htonly, vars="INSIGHT_ID")
names(go)[2]="ntest"
go = merge(x=go, y=t, by="INSIGHT_ID")
go$pct = go$ntest/go$freq*100.
summary(go$pct)
head(go[order(go$ntest,decreasing = T),],15)

gt = count(htonly[htonly$INSIGHT_ID=='glycemic.389',], vars="dt")
summary(gt$freq)
head(gt[order(gt$freq, decreasing = T),],10)

gt = count(htonly, vars="dt")
summary(gt$freq)
head(gt[order(gt$freq, decreasing = T),],10)



###################################


# Example 2: Debug the clean SG data loss
# Compare the DB2 extraction from raw SG and clean SG tables
# Compare the Kafka extraction based on Kafka topics of MDTLIVE and MDTPRIME
# Analyze the impacts brought by gaps 


#read the mdtlive parsed through Python script (parse.py)
# process MDTLIVE

#nohup grep "highLimit" MDTLIVE.out > mdtlive.out
#nohup python parse.py -f /tmp/mdtlive.out > sorg

library(plyr)
options(width=200)
src4="/Users/lcao@us.ibm.com/Downloads/cleanSG_counts/csp/mdtlive/"

s0 = read.table(paste0(src4, "sorg"), header=F, sep="", stringsAsFactors = FALSE)
nrow(s0)
[1] 12212421

#ss = rbind(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10)
names(s0)=c("topic","partition","offset","PERSON_ID","sgvalue","sgtime")
s0$epoch = as.numeric(strptime(substring(s0$sgtime,1,19),"%Y-%m-%dT%H:%M:%S", tz="UTC"))
s0$dt = as.Date(as.POSIXct(s0$epoch, origin="1970-01-01"))
length(unique(s0$dt))
[1] 113

#save partition information
sp0 = unique(s0[,c("PERSON_ID","partition")])
nrow(sp0)
[1] 1620


s0 = unique(s0[,c("PERSON_ID","partition","sgvalue","sgtime","epoch","dt")])
nrow(s0)
[1] 11939991

#dups
> (12212421-11939991)/11939991
[1] 0.0228166

length(unique(s0$PERSON_ID))
[1] 1620


s1 = read.table(paste0(src4, "scat"), header=F, sep="", stringsAsFactors = FALSE)
nrow(s1)
[1] 12478202
[1] 12960580
[1] 13039814

names(s1)=c("topic","partition","offset","PERSON_ID","sgvalue","sgtime")
#names(s1)=c("PERSON_ID","sgvalue","sgtime")
s1$epoch = as.numeric(strptime(substring(s1$sgtime,1,19),"%Y-%m-%dT%H:%M:%S", tz="UTC"))
s1$dt = as.Date(as.POSIXct(s1$epoch, origin="1970-01-01"))
length(unique(s1$dt))
[1] 96
[1] 94
[1] 96

#save partition information
sp1 = unique(s1[,c("PERSON_ID","partition")])
nrow(sp1)
[1] 1635
[1] 1651
[1] 1658

s1 = unique(s1[,c("PERSON_ID","partition","sgvalue","sgtime","epoch","dt")])
nrow(s1)
[1] 12127079
[1] 12568312
[1] 12647433

#dups
> (12478202-12127079)/12127079
[1] 0.02895363

> (12960580-12568312)/12568312
[1] 0.03121087

> (13039814-12647433)/12647433
[1] 0.03102456

length(unique(s1$PERSON_ID))
[1] 1635
[1] 1651
[1] 1658

sd0 = count(s0, vars=("dt"))
sd1 = count(s1, vars=("dt"))

write.csv(sd0,paste0(src4,"sd0.csv"), row.names=FALSE)
write.csv(sd1,paste0(src4,"sd1.csv"), row.names=FALSE)
write.csv(sd1,paste0(src4,"sd2.csv"), row.names=FALSE)
write.csv(sd1,paste0(src4,"sd_cat.csv"), row.names=FALSE)


sd2 = read.csv(paste0(src4, "sd2.csv"), header=T, stringsAsFactors = FALSE)
nrow(sd2)
sd2$dt = as.Date(sd2$dt)

names(sd0)[2]="toolbert_orignal_miss"
names(sd2)[2]="toolbert0322_miss"
names(sd1)[2]="cat0323_miss"

sd2 = merge(x=sd0, y=sd2, by="dt", all=T)
sd2[is.na(sd2)] <- 0

sd = merge(x=sd2, y=sd1, by="dt", all=T)
sd[is.na(sd)] <- 0

write.csv(sd,paste0(src4,"sd_compare.csv"), row.names=FALSE)

sk0 = s0[(s0$dt>="2019-12-05" & s0$dt<="2020-01-19"),]
nrow(sk0)
[1] 7681606
sk1 = s1[(s1$dt>="2020-01-20" & s1$dt<="2020-03-02"),]
nrow(sk1)
[1] 6948490
[1] 7456058

sk = rbind(sk0,sk1)
nrow(sk)
[1] 14630096
[1] 15137664
rm(sk0,sk1)

length(unique(sk$dt))
[1] 89
length(unique(sk$PERSON_ID))
[1] 1753
[1] 1776

length(intersect(unique(sk$PERSON_ID), unique(allu$PERSON_ID)))

dd = dr[(dr$dt>="2019-12-05" & dr$dt<="2020-03-02"),]
nrow(dr)
[1] 4641717
nrow(dd)
[1] 4486127

liveuser=unique(dd[,c("PERSON_ID"), drop=F])
> nrow(liveuser)
[1] 900

length(unique(dd$PERSON_ID))
[1] 900


livepc = count(sk,vars=c("PERSON_ID"))
names(livepc)[2]="livesg_count"
rawpc = count(dd,vars="PERSON_ID")
names(rawpc)[2]="rawsg_count"

diffpc = merge(x=livepc, y=rawpc, by=("PERSON_ID"))
diffpc$diff_count=diffpc$rawsg_count-diffpc$livesg_count
nrow(diffpc)
[1] 876
[1] 886



mm = diffpc[diffpc$diff_count<0,]
nrow(mm)
[1] 0





#==== pull out the top 50 users who have both mdtlive and rawSG match from "2020-01-20" & s1$dt<="2020-03-02"
lvm = count(sk1,vars=c("PERSON_ID"))
names(lvm)[2]="livesg_count"
dm = dr[(dr$dt>="2020-01-20" & dr$dt<="2020-03-02"),]
nrow(dm)
[1] 1759446
ram = count(dm,vars="PERSON_ID")
names(ram)[2]="rawsg_count"

diffm = merge(x=lvm, y=ram, by=("PERSON_ID"))
diffm$diff_count=diffm$rawsg_count-diffm$livesg_count
nrow(diffm)
[1] 545

onlive = diffm[diffm$diff_count==0,]
nrow(onlive)
[1] 495

#no mdtlive data before 01/20
dn = dr[(dr$dt<"2020-01-20"),]
nrow(dn)
dn1 = unique(dn[,"PERSON_ID", drop=F])
onlive1 = onlive[!(onlive$PERSON_ID %in% dn1$PERSON_ID), ]

#no prime data 
onlive2 = onlive1[!(onlive1$PERSON_ID %in% pk$PERSON_ID), ]


testu = head(onlive2[order(onlive2$livesg_count, decreasing=T),c("PERSON_ID"),drop=F], 50)
write.csv(testu,paste0(src4,"50users.csv"), row.names=FALSE)

length(intersect(unique(pk$PERSON_ID), testu$PERSON_ID))

dpc = diffpc[diffpc$diff_count>0,]
nrow(dpc)
[1] 164
[1] 118

#df_dedup = unique(df[c("PERSON_ID","dt")])


#count the total SG and total dates for users in mdtlive
w1 = count(sk, vars=c("PERSON_ID","dt"))
names(w1)[3]="daily_SG"
w2 = count(w1, vars=c("PERSON_ID"))
names(w2)[2]="total_dt"

#ensure they have mdtprime
w1 = w1[(w1$PERSON_ID %in% unique(pk$PERSON_ID)),]
w2 = w2[(w2$PERSON_ID %in% unique(pk$PERSON_ID)),]

w3 = aggregate(w1$daily_SG, by=list(PERSON_ID=w1$PERSON_ID), FUN=mean)
names(w3)[2]="mean_SG"

w2 = merge(x=w2, y=w3, by="PERSON_ID")
nrow(w2[(w2$mean_SG>=150 & w2$total_dt>=50),])
nrow(w2[(w2$total_dt>=50),])
summary(w2[(w2$total_dt>=50),]$mean_SG)

w4 = w2[(w2$mean_SG>=216 & w2$total_dt>=50),]
write.csv(w4,paste0(src4,"37users.csv"), row.names=FALSE)
wu = unique(w4[,"PERSON_ID",drop=F])
write.csv(wu,paste0(src4,"37usersid.csv"), row.names=FALSE)

#check the specific user detail 

f = 50934325
f = 50777861
f = 50847711 

pp = p0[p0$PERSON_ID==f,]
lp = sk[sk$PERSON_ID==f,]
nrow(lp)

dp = dd[dd$PERSON_ID==f,]
nrow(dp)

ldp = merge(x=lp[,c("PERSON_ID","partition","sgvalue","sgtime","epoch","dt")], y=dp[,c("PERSON_ID","SG","SG_TIMESTAMP","epoch")],
           by=c("PERSON_ID","epoch"))
nrow(ldp)
[1] 5740

d1 = merge(x=lp[,c("PERSON_ID","partition","sgvalue","sgtime","epoch")], y=dp[,c("PERSON_ID","SG","SG_TIMESTAMP","epoch","dt")],
            by=c("PERSON_ID","epoch"), all.x=T)
nrow(d1)
nrow(d1[is.na(d1$SG),])


d2 = merge(x=lp[,c("PERSON_ID","partition","sgvalue","sgtime","epoch")], y=dp[,c("PERSON_ID","SG","SG_TIMESTAMP","epoch","dt")],
           by=c("PERSON_ID","epoch"), all.y=T)
nrow(d2)
d4 = d2[!is.na(d2$sgvalue),]
count(d4,vars="dt")

d3 = d2[is.na(d2$sgvalue),]
nrow(d3)
count(d3,vars="dt")


# process MDTPRIME

#nohup grep "highLimit" MDTPRIME.out > prime_org.input &
#nohup python parse.py -f /tmp/prime_org > primeorg &
  
p0 = read.table(paste0(src4, "primeorg"), header=F, sep="", stringsAsFactors = FALSE)
nrow(p0)
[1] 1192388

names(p0)=c("topic","partition","offset","PERSON_ID","sgvalue","sgtime")
p0$epoch = as.numeric(strptime(substring(p0$sgtime,1,19),"%Y-%m-%dT%H:%M:%S", tz="UTC"))
p0$dt = as.Date(as.POSIXct(p0$epoch, origin="1970-01-01"))
length(unique(p0$dt))
[1] 182

p0 = unique(p0[,c("PERSON_ID","partition","sgvalue","sgtime","epoch","dt")])
nrow(p0)
[1] 1185375

primeuser=unique(p0[,c("PERSON_ID"), drop=F])
nrow(primeuser)
[1] 358

pluser = merge(x=liveuser, y=primeuser, by="PERSON_ID")
nrow(pluser)


#dups
> (1192388-1185375)/1185375
[1] 0.005916271

p1 = read.table(paste0(src4, "prime0321"), header=F, sep="", stringsAsFactors = FALSE)
nrow(p1)
[1] 976171

names(p1)=c("topic","partition","offset","PERSON_ID","sgvalue","sgtime")
#names(p1)=c("PERSON_ID","sgvalue","sgtime")
p1$epoch = as.numeric(strptime(substring(p1$sgtime,1,19),"%Y-%m-%dT%H:%M:%S", tz="UTC"))
p1$dt = as.Date(as.POSIXct(p1$epoch, origin="1970-01-01"))
length(unique(p1$dt))
[1] 168
sort(unique(p1$dt))

p1 = unique(p1[,c("PERSON_ID","partition","sgvalue","sgtime","epoch","dt")])
nrow(p1)
[1] 969942

#dups
> (976171-969942)/969942
[1] 0.006422033


pd0 = count(p0, vars=("dt"))
pd1 = count(p1, vars=("dt"))

write.csv(pd0,paste0(src4,"pd0.csv"), row.names=FALSE)
write.csv(pd1,paste0(src4,"pd1.csv"), row.names=FALSE)


pk = p0[(p0$dt>="2019-12-05" & p0$dt<="2020-03-02"),]
nrow(pk)
[1] 742732

length(unique(pk$PERSON_ID))
[1] 340

length(unique(dpc$PERSON_ID))
[1] 164
[1] 118

#found prime data
jp0 = merge(x=pk, y=unique(dpc[,c("PERSON_ID"),drop=F]), by=c("PERSON_ID"))
nrow(jp0)
[1] 199205
[1] 199158

length(unique(jp0$PERSON_ID))
[1] 92
[1] 91

x=unique(pk[,c("PERSON_ID"),drop=F])
y=unique(dpc[,c("PERSON_ID"),drop=F])
length(intersect(y$PERSON_ID,x$PERSON_ID))
[1] 92
[1] 91

#get all live data for those ones who have live loss in MDTLIVE
jp1 = merge(x=sk, y=unique(dpc[,c("PERSON_ID"),drop=F]), by=c("PERSON_ID"))
nrow(jp1)
[1] 1036739
[1] 573113

jp = rbind(jp0, jp1)
nrow(jp)
[1] 1235944
[1] 772271

length(unique(jp$PERSON_ID))
[1] 164
[1] 118

####!!!!
jp = unique(jp)
nrow(jp)
[1] 1220365
[1] 756739

#add partition information
#sp = unique(rbind(sp0, sp1))
#jp = merge(x=jp, y=sp, by=c("PERSON_ID"), all.x=T)
#nrow(jp[is.na(jp$partition),])
#[1] 0

#get raw SG for those ones who have live loss in MDTLIVE
rloss = merge(x=dd, y=unique(dpc[,c("PERSON_ID"),drop=F]), by=c("PERSON_ID"))
nrow(rloss)
[1] 1301041
[1] 757819

#merge (MDTPRIME+MDTLIVE) with rawSG
ju = merge(x=jp[,c("PERSON_ID","partition","sgvalue","sgtime","epoch")], y=rloss[,c("PERSON_ID","SG","SG_TIMESTAMP","SG_TIMESTAMP_TZ","epoch","dt")],
           by=c("PERSON_ID","epoch"), all.y=T)

nrow(ju)
[1] 1301041
[1] 757819

u0 = ju[is.na(ju$sgvalue),]
nrow(u0)
[1] 80676
[1] 1080


#add the partition
u0 = merge(x=u0[,c(-3)], y=sp, by="PERSON_ID", all.x=T)
u0 = u0[,-c(3,4)]
length(unique(u0$PERSON_ID))
[1] 84
[1] 32

#add the UTC sgtime
u0$sg_utc=format(as.POSIXct(u0$epoch, origin='1970-1-1',tz='UTC'),"%Y-%m-%dT%H:%M:%S")


ut = count(u0,vars="dt")
names(ut)[2]="missing_rawSG"

write.csv(ut,paste0(src4,"no_prime_live_dt.csv"), row.names=FALSE)
write.csv(u0,paste0(src4,"no_prime_live.csv"), row.names=FALSE)
write.csv(u0[u0$dt!='2020-03-02',],paste0(src4,"no_prime_live_4dates.csv"), row.names=FALSE)
write.csv(u0[u0$dt=='2020-03-02',],paste0(src4,"no_prime_live_0302.csv"), row.names=FALSE)

nrow(u0[u0$dt!='2020-03-02',])
nrow(u0[u0$dt=='2020-03-02',])
 
u1 = count(u0,vars=c("PERSON_ID","dt"))

u2 = count(u0,vars=c("PERSON_ID"))
head(u2[order(u2$freq,decreasing = T),])

j = unique(jp[,c("PERSON_ID","partition")])

t = t[order(t$freq,decreasing = T),]
t = merge(x=t,y=j, by="PERSON_ID")


ut = u0[,c(1,2,4)]

rut = merge(x=ut,y=rawsg, by=c("PERSON_ID","SG_TIMESTAMP"), all.x=T)
nrow(rut)
write.csv(rut,paste0(src4,"check_catchup.csv"), row.names=FALSE)

###### pull in MDTLIVE toolbert data

sed -i -e 1,69d live2.csv
wc -l live2.csv
sed -i -e 838421,838422d live2.csv

sed -i -e "s/MDTLIVE,/MDTLIVE|/g" live2.csv
sed -i -e "s/, partition =/| partition =/g" live2.csv
sed -i -e "s/, offset =/| offset =/g" live2.csv
sed -i -e "s/, timestamp =/| timestamp =/g" live2.csv
sed -i -e "s/, key =/| key =/g" live2.csv
sed -i -e "s/, value =/| value =/g" live2.csv

src2="/Users/lcao@us.ibm.com/Downloads/cleanSG_counts/csp/mdtlive/"
dh = read.csv(paste0(src2, "live2.csv"), quote = "", header=F, sep="|", stringsAsFactors = FALSE)

> nrow(dh)
[1] 838420

dh$V1=as.character(gsub(" topic = ", "", dh$V1))
dh$V2=as.character(gsub(" partition = ", "", dh$V2))
dh$V3=as.character(gsub(" offset = ", "", dh$V3))
dh$V4=as.character(gsub(" timestamp = ", "", dh$V4))
dh$V5=as.character(gsub(" key = ", "", dh$V5))
dh$V6=as.character(gsub(" value = ", "", dh$V6))
dh$V6=gsub("\"", "", dh$V6)

library(plyr)
library(data.table)
library(stringr)



k <- do.call(rbind, lapply(dh$V6, function(v) parse_sg(v)))

parse_sg <- function(v){
  a = data.frame(str_locate_all(v,"dateTime:"))
  if(nrow(a)>0){
    a$sgtime = substring(v,a$end+1,a$end+25)
    b = data.frame(str_locate_all(v,",sg:"))
    c = data.frame(str_locate_all(v,",timeChange:"))
    a$sgvalue = substring(v,b$end+1,c$start-1)
    return(a[,c("sgtime","sgvalue")])
  }
}

dim(dh)
dim(k)

ck= k
ck$PERSON_ID=50905171
ck$epoch = as.numeric(strptime(substring(ck$sgtime,1,19),"%Y-%m-%dT%H:%M:%S", tz="UTC"))

c = dr[dr$PERSON_ID==50905171,]
dim(c)

ckm = merge(x = c[,c("PERSON_ID","SG","SG_TIMESTAMP","timezone","epoch","dt")], y = ck[,c("PERSON_ID","sgtime","sgvalue","epoch")], by=c("PERSON_ID","epoch"), all.x=T)
miss = ckm[is.na(ckm$sgtime),]
dim(miss)
count(miss,vars="dt")

ckn = merge(x = c[,c("PERSON_ID","SG","SG_TIMESTAMP","timezone","epoch","dt")], y = ck[,c("PERSON_ID","sgtime","sgvalue","epoch")], by=c("PERSON_ID","epoch"), all.y=T)
nrow(ckn[is.na(ckn$SG_TIMESTAMP),])



#==========

dh$V6=gsub("\"", "", dh$V6)
a = data.frame(str_locate(dh$V6,"sgs:\\[\\{dateTime:"))
dh$sgtime = substring(dh$V6,a$end+1,a$end+25)
b = data.frame(str_locate(dh$V6,",sg:"))
c = data.frame(str_locate(dh$V6,",timeChange:"))
dh$sgvalue = substring(dh$V6,b$end+1,c$start-1)

dh2 = dh[!is.na(dh$sgvalue),-c(6)]
dim(dh)
dim(dh2)

> dim(dh)
[1] 3309    8
> dim(dh2)
[1] 3007    7


dh2$epoch = as.numeric(strptime(substring(dh2$sgtime,1,19),"%Y-%m-%dT%H:%M:%S", tz="UTC"))
#dh2$epoch = dh2$epoch+as.numeric(substring(dh2$sgtime,20,22))*3600
dh2$dt = as.Date(as.POSIXct(dh2$epoch, origin="1970-01-01"))
#equivilent: dh2$dt = as.Date(dh2$sgtime)

ck = dh2[dh2$V5==50905171,]
c = dr[dr$PERSON_ID==50905171,]
#n = dc1[dc1$PERSON_ID==50943643,]
names(ck)[5]="PERSON_ID"
ckm = merge(x = c[,c("PERSON_ID","SG","SG_TIMESTAMP","timezone","epoch","dt")], y = ck[,c("PERSON_ID","sgtime","sgvalue","epoch")], by=c("PERSON_ID","epoch"), all.x=T)
ckm = ckm[is.na(ckm$sgtime),]
ckm = ckm[ckm$dt=='2020-03-07',]

ckn = merge(x = c[,c("PERSON_ID","SG","SG_TIMESTAMP","timezone","epoch","dt")], y = ck[,c("PERSON_ID","sgtime","sgvalue","epoch")], by=c("PERSON_ID","epoch"), all.y=T)
nrow(ckn[is.na(ckn$SG_TIMESTAMP),])


ckn = merge(x = n, y = ck[,c("PERSON_ID","sgtime","sgvalue","epoch")], by=c("PERSON_ID","epoch"))
##########################
##### compare counts of cleanSG vs rawSG

library(plyr)
src="/Users/lcao@us.ibm.com/Downloads/cleanSG_counts/"
src1="/Users/lcao@us.ibm.com/Downloads/cleanSG_counts/csp/"
options(width=200)

cleansg = read.table(paste0(src, "cleansg.txt"), header=T, sep="", stringsAsFactors = FALSE)
nrow(cleansg)
[1] 2182206
[1] 113265

dc=cleansg[,c("PERSON_ID","PUMP_TIME_TS","SGMGDL")]
length(unique(dc$PERSON_ID))
[1] 857
[1] 152

dc$epoch = as.numeric(strptime(dc$PUMP_TIME_TS,"%Y-%m-%d-%H.%M.%S", tz="UTC"))
dc = dc[order(dc$epoch),]

dc$dt = as.Date(as.POSIXct(dc$epoch, origin="1970-01-01"))
length(unique(dc$dt))
[1] 135
[1] 100

dc0 = dc
dc = dc0[(dc0$dt>="2019-12-01" & dc0$dt<="2020-03-02"),]
#dc = dc0[(dc0$dt>="2020-03-03" & dc0$dt<="2020-03-09"),]
length(unique(dc$dt))
[1] 93
[1] 7
length(unique(dc$PERSON_ID))
[1] 842
[1] 150

library(plyr)
count_epoch=count(dc, vars=c("PERSON_ID","epoch"))
dup_epoch=count_epoch[count_epoch$freq>1,]
if(nrow(dup_epoch)>0){
  #count dups by id
  dup_epoch=dup_epoch[,-c(3)]
  count_dup=count(dup_epoch,vars=c("PERSON_ID"))
  count_dup = count_dup[order(count_dup$freq,decreasing = TRUE),]
}

#dedup
dc1 = unique(dc)
nrow(dc1)
[1] 2026186
[1] 83137

dd=count(dc1,vars=c("PERSON_ID"))
names(dd)[2]="cleansg_total"
cd = merge(x = count_dup, y = dd, by="PERSON_ID")
cd$percent=cd$freq/cd$cleansg_total*100.
write.csv(cd,paste0(src,"dup_sum.csv"), row.names=FALSE)

nrow(cd)
[1] 41
[1] 36
summary(cd$percent)
#==========

rawsg = read.table(paste0(src,"rawsg.txt"), header=T, sep="", stringsAsFactors = FALSE)
nrow(rawsg)
[1] 4795559
[1] 157387

length(unique(rawsg$PERSON_ID))

dr = rawsg[,c("PERSON_ID","SG", "SG_TIMESTAMP","SG_TIMESTAMP_TZ")]
length(unique(dr$PERSON_ID))
[1] 927
[1] 200

dr$epoch = as.numeric(strptime(dr$SG_TIMESTAMP,"%Y-%m-%d-%H.%M.%S", tz="UTC"))
dr$timezone=as.numeric(dr$SG_TIMESTAMP_TZ)/3600
dr$epoch = dr$epoch+as.numeric(dr$SG_TIMESTAMP_TZ)
dr = dr[order(dr$epoch),]
dr$dt = as.Date(as.POSIXct(dr$epoch, origin="1970-01-01"))
dr0 = dr
dr = dr0[(dr0$dt>="2019-12-01" & dr0$dt<="2020-03-02"),]
#dr = dr0[(dr0$dt>="2020-03-03" & dr0$dt<="2020-03-09"),]

length(unique(dr$dt))
[1] 93
[1] 7
length(unique(dr$PERSON_ID))
[1] 911 (3month)
[1] 195 (1week)

allu = read.csv(paste0(src4, "3month_users.csv"), header=T, stringsAsFactors = FALSE)[,,drop=F]
nrow(allu)
[1] 1800

#important - roll back 24-hour + 20 min = (87600) to each user from the very last rawSG of his
fd = unique(dr$PERSON_ID)
rollback_rawsg = function(f){
  df = dr[dr$PERSON_ID==f,]
  cutoff = df$epoch[nrow(df)] - 87600
  if (cutoff>0){
    df = df[df$epoch<cutoff,]
    return (data.frame(df))
  }
}

dr2 <- do.call(rbind, lapply(fd, function(x) rollback_rawsg(x)))
nrow(dr2)
[1] 4447603
[1] 86274
nrow(dr)
[1] 4641717
[1] 123667


#check duplicates
raw_epoch=count(dr2, vars=c("PERSON_ID","epoch"))
rawdup_epoch=raw_epoch[raw_epoch$freq>1,]
nrow(rawdup_epoch)
[1] 0

#count the days for those users who are in the rawSG - it's the number of active days
dr3 = unique(dr2[c("PERSON_ID","dt")])
actived = count(dr3, vars=c("PERSON_ID"))
names(actived)[2]="adays"
length(unique(actived$PERSON_ID))
[1] 877
[1] 137
write.csv(actived,paste0(src,"active_days.csv"), row.names=FALSE)


#apply the threshold for the number of active days
#(1) less than 15 days of data in the past 3 months
ad1 = actived[actived$adays>=15,][,c(1),drop=F]
nrow(ad1)
[1] 452

#(2) users for whom have data for 3 weeks from cutoff date, 
#02/11/2020 - 03/03/2020
dr4  = dr3[(dr3$dt>='2020-02-11' & dr3$dt<='2020-03-03'),][,c(1),drop=F]
ad4 = unique(dr4)
#ad4 = unique(dr4$PERSON_ID)
nrow(ad4)
[1] 349

adtotal = unique(dr2[,c("PERSON_ID"), drop=F])
adtotal = merge(x= adtotal, y=ad1, by=c("PERSON_ID"))
adtotal = merge(x= adtotal, y=ad4, by=c("PERSON_ID"))

#count the users who have rawSG on a daily basis - it's the number of active users
au = count(dr3, vars=c("dt"))
names(au)[2]="ausers"
length(unique(au$dt))
[1] 92
[1] 6
write.csv(au,paste0(src,"active_users.csv"), row.names=FALSE)


dr5 = merge(x=dr2, y=adtotal, by=c("PERSON_ID"))
dr6 = unique(dr5[c("PERSON_ID","dt")])
dr7 = count(dr6, vars=c("dt"))
summary(dr7$freq)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
3.0   114.0   126.0   130.5   151.0   180.0 
names(dr7)[2]="ausers"

#merge cleansg (dc1) and rawsg (dr) 
rc = merge(x = dr2[,c("PERSON_ID", "epoch","SG")], y = dc1[,c("PERSON_ID", "epoch","SGMGDL")], by = c("PERSON_ID", "epoch"), all.x = TRUE)
rm <- rc[is.na(rc$SGMGDL),c("PERSON_ID", "epoch","SG")]
length(unique(rm$PERSON_ID))
[1] 866
[1] 117
write.csv(rm,paste0(src,"sg_missing.csv"), row.names=FALSE)

#==== day level analysis
rm$dt = as.Date(as.POSIXct(rm$epoch, origin="1970-01-01"))

dayrm=count(rm,vars=c("dt"))
dayrm = dayrm[order(dayrm$freq,decreasing = TRUE),]
dayrm = dayrm[order(dayrm$dt),]
names(dayrm)[2]="missing_numb"

with(dayrm,plot(dt,missing_numb, type="l", col="red"))

plot(dayrm$dt, dayrm$missing_numb,xlab="Date", ylab="Number of Missing",
abline(lm(dayrm$missing_numb~dayrm$dt)))


#put on a filter 

pt=count(dr2, vars=c("PERSON_ID","dt"))

summary(pt$freq)
nrow(pt[pt$freq<72,])
[1] 1675 (with 24-roll back)
[1] 54

x = pt$freq
h <- hist(x, breaks=20, col="green")

#filter out those dates with total rawSG less than 6 hours - 72 SGs
pt1 = pt[pt$freq>=72,]
pt1 = pt1[,c(-3)]
prm = merge(x = rm, y = pt1, by=c("PERSON_ID","dt"))
nrow(rm)
[1] 2438063
[1] 10847
nrow(prm)
[1] 2399331
[1] 10466

dayprm=count(prm,vars=c("dt"))
dayprm =dayprm[order(dayprm$freq,decreasing = TRUE),]
dayprm = dayprm[order(dayprm$dt),]
names(dayprm)[2]="missing_numb_filtered"
nn = merge(x = dayrm, y = dayprm, by=c("dt"))
pp=count(dr2, vars=c("dt"))
names(pp)[2]="total_rawSG"

nn = merge(x = nn, y = pp, by=c("dt"))
nn$pct_missing=nn$missing_numb/nn$total_rawSG*100
nn$pct_missing_filtered=nn$missing_numb_filtered/nn$total_rawSG*100

nn = merge(x = nn, y = au, by="dt")
nn$pct_missing_filtered_per_au=nn$missing_numb_filtered/(nn$total_rawSG*nn$ausers)*100.

summary(nn$pct_missing_filtered)
summary(nn$pct_missing_filtered_per_au)

write.csv(nn,paste0(src,"day_missing.csv"), row.names=FALSE)



#==== day level analysis with active day threshold
rma = merge(x=rm, y=adtotal, by=c("PERSON_ID"))
dayrma=count(rma,vars=c("dt"))
dayrma = dayrma[order(dayrma$freq,decreasing = TRUE),]
dayrma = dayrma[order(dayrma$dt),]
names(dayrma)[2]="missing_numb"

#put on a filter 
pta=count(dr5, vars=c("PERSON_ID","dt"))
#filter out those dates with total rawSG less than 6 hours - 72 SGs
pt1a = pta[pta$freq>=72,]
pt1a = pt1a[,c(-3)]
prma = merge(x = rma, y = pt1a, by=c("PERSON_ID","dt"))
nrow(rma)
[1] 1382920
nrow(prma)
[1] 1362470

dayprma=count(prma,vars=c("dt"))
dayprma =dayprma[order(dayprma$freq,decreasing = TRUE),]
dayprma = dayprma[order(dayprma$dt),]
names(dayprma)[2]="missing_numb_filtered"
nna = merge(x = dayrma, y = dayprma, by=c("dt"))

ppa=count(dr5, vars=c("dt"))
names(ppa)[2]="total_rawSG"

nna= merge(x = nna, y = ppa, by=c("dt"))
nna$pct_missing=nna$missing_numb/nna$total_rawSG*100
nna$pct_missing_filtered=nna$missing_numb_filtered/nna$total_rawSG*100

nna = merge(x = nna, y = dr7, by="dt")
nna$pct_missing_filtered_per_au=nna$missing_numb_filtered/(nna$total_rawSG*nna$ausers)*100.

summary(nna$pct_missing_filtered)
summary(nna$pct_missing_filtered_per_au)





plot(dayprm$dt, dayprm$missing_numb_filtered,xlab="Date", ylab="Number of Missing - filtered",
     abline(lm(dayprm$missing_numb_filtered~dayprm$dt), col="red"))

plot(nn$dt, nn$pct_missing_filtered,xlab="Date", ylab="Pct of Missing - filtered",
     abline(lm(nn$pct_missing_filtered~nn$dt), col="blue"))

aa = unique(pt[c("PERSON_ID","dt")])
a1=count(aa, vars=c("dt"))
names(a1)[2]="numb_users"

plot(a1$dt, a1$numb_users,xlab="Date", ylab="Number of Users",
     abline(lm(a1$numb_users~a1$dt), col="brown"))
write.csv(a1,paste0(src,"numb_users.csv"), row.names=FALSE)

#=====

#take rows 1 to n-1 and subtract rows 2 to n:
rn$gap <- c(NA, with(rn, epoch[-1] - epoch[-nrow(rn)]))
next_start = rn[which(rn$gap>350),]

#compute the average continuous session duration - which is the cleanSG gap in this case
t = c(rownames(rn[1,]), rownames(next_start), rownames(rn[nrow(rn),]))
t=unique(t)
rn_need = rn[t,]
rn_need$cont <- c(NA, with(rn_need, epoch[-1] - gap[-1] - epoch[-nrow(rn_need)]))
#remove the starting point
rn_need = rn_need[!is.na(rn_need$gap),]
#remove zero gaps - which is no missing SGs between the two consequential gaps
rn_need=rn_need[(rn_need$cont>0),]

avg_cont_in_mins = mean(rn_need$cont, na.rm=T)/60

#==========


count_ms=count(rm,vars=c("PERSON_ID"))
count_ms = count_ms[order(count_ms$freq,decreasing = TRUE),]

#"PERSON_ID" should be in the cleansg to be an active user
duc=unique(dc1[,c("PERSON_ID"),drop=F])
dm = merge(x = count_ms, y = duc, by="PERSON_ID")
dm = dm[order(dm$freq,decreasing = TRUE),]

cr=count(dr,vars=c("PERSON_ID"))
names(cr)[2]="total"
cm = merge(x = dm, y = cr, by="PERSON_ID")
cm$percent=cm$freq/cm$total*100.

#===plus the active day info
cm = merge(x = cm, y = actived, by="PERSON_ID")
cm$percent_per_ad=cm$freq/(cm$total*cm$adays)*100.



summary(cm$percent)
summary(cm$percent_per_ad)

#apply active days threshold
cmc = merge(x=cm, y=adtotal, by="PERSON_ID")
summary(cm$percent_per_ad)


write.csv(cm,paste0(src,"missing_sum.csv"), row.names=FALSE)

ca = merge(x = dm, y = cr, by="PERSON_ID", all.y = TRUE)
ca = ca[is.na(ca$freq),]
ca = ca[order(ca$total,decreasing = TRUE),]

#scatter plot
y = cm$percent
plot(y)
#histogram plot
x = cm$percent
h <- hist(x, breaks=20, xlab="Missing Ratio", ylab="# of users", main="Histogram of Missing CleanSG Ratio", col="green")
xfit<-seq(min(x),max(x),length=55)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

#density plot
d <- density(cm$percent) # returns the density data
plot(d, main="Density of Missing cleanSG Ratio") # plots the results
polygon(d, col="green", border="blue")



#=== function to compute the gap freq/distribution and duration for individual user
fdr = unique(rm$PERSON_ID)
fdc = unique(dc1$PERSON_ID)
ru = data.frame(setdiff(fdr, fdc))
names(ru)[1]="PERSON_ID"
rur = merge(x = dr, y = ru, by="PERSON_ID")
rurf = count(rur,vars=c("PERSON_ID"))

nrow(rurf)
[1] 38
[1] 0

write.csv(rurf,paste0(src,"rawuser_only.csv"), row.names=FALSE)

summary(rurf$freq)

fid = intersect(fdr,fdc)
length(unique(fid))
1] 828
[1] 117

s <- do.call(rbind, lapply(fid, function(x) gap_freq_cont(x)))

sf=count(s,vars=c("PERSON_ID"))
names(sf)[2]="numb_gap"
sc = aggregate(s$cont, by=list(PERSON_ID=s$PERSON_ID), FUN=sum)
names(sc)[2]="total_duration"
sfc = merge(x = sf, y = sc, by="PERSON_ID")
sfc$mean_duration=sfc$total_duration/sfc$numb_gap
summary(sfc$numb_gap)
summary(sfc$mean_duration)

sfc$mean_missing=as.integer(sfc$mean_duration/300)+1
summary(sfc$mean_missing)

sfc = merge(x = sfc, y = actived, by="PERSON_ID")
sfc$gap_per_ad=sfc$numb_gap/sfc$adays*1.
sfc$mean_missing_per_ad=sfc$mean_missing/sfc$adays*1.
summary(sfc$gap_per_ad)
summary(sfc$mean_missing_per_ad)

write.csv(sfc,paste0(src,"gap_sum.csv"), row.names=FALSE)

sfc3 = read.csv(paste(src, "gap_sum.csv", sep=""), header=T,stringsAsFactors = FALSE)
sfc3 = merge(x=sfc3, y=adtotal, by=c("PERSON_ID"))
head(sfc3[order(sfc3$mean_missing, decreasing = TRUE),c("PERSON_ID","numb_gap","mean_missing","adays","mean_missing_per_ad")],10)


summary(sfc3$gap_per_ad)
summary(sfc3$mean_missing_per_ad)

#== gap count by day
sd=count(s,vars=c("dt"))
names(sd)[2]="numb_gap"
sd = merge(x = sd, y = au, by="dt")
sd$gap_per_au = sd$numb_gap/sd$ausers*1.
summary(sd$gap_per_au)
write.csv(sd,paste(src1,"gap_sum_byday.csv"), row.names=FALSE)

sd=count(s,vars=c("dt"))
names(sd)[2]="numb_gap"
sd7 = merge(x = sd, y = dr7, by="dt")
sd7$gap_per_au = sd7$numb_gap/sd7$ausers*1.
summary(sd7$gap_per_au)



#===========
rma = merge(x=rm, y=adtotal, by=c("PERSON_ID"))
rmb = count(rma, vars=c("PERSON_ID","dt"))
names(rmb)[3]="missing_SG"
#dr5 = merge(x=dr2, y=adtotal, by=c("PERSON_ID"))
dr8 = count(dr5, vars=c("PERSON_ID", "dt"))
names(dr8)[3]="total_SG"

rdr = merge(x=rmb, y=dr8, by=c("PERSON_ID","dt"), all.y=TRUE)
rdr[is.na(rdr$missing_SG),]$missing_SG=0

rdy=rdr[rdr$total_SG>=72,]
rdr$missing_pct=rdr$missing_SG/rdr$total_SG*100.
summary(rdy$missing_pct)

#histogram plot
x = rdy[rdm$dt<'2020-01-01',]$missing_pct
y = rdy[(rdm$dt>='2020-01-01' & rdy$dt<'2020-02-01'),]$missing_pct
z = rdy[rdm$dt>='2020-02-01' & rdy$dt<'2020-03 n-01',]$missing_pct
hist(z, breaks=30, col="red", add=T)
hist(y, breaks=30, col="yellow", add=T)
hist(x, breaks=30, xlab="Daily Missing Ratio", ylab="# of per day per user", main="(Dec Y19) missing SG per day per user", col="green")
rdm = rdy[rdy$missing_pct>=90,]
#xn = rdd[seq(1, length(rdd$dt), by=10),]$dt
#barplot(rdd$freq, names.arg=xn, col="blue")

barplot(rdd$freq, names.arg=rdd$dt, col="blue")

head(rdd[order(rdd$freq, decreasing = T),])


xfit<-seq(min(x),max(x),length=55)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

rdz = unique(rdy[,c("PERSON_ID","dt")])
sz = merge(x=s,y=rdz,by=c("PERSON_ID","dt"))

sfz=count(sz,vars=c("PERSON_ID","dt"))
names(sfz)[3]="numb_gap_per_ad"
summary(sfz$numb_gap_per_ad)

sfx = merge(x=sfz, y=rdy, by=c("PERSON_ID","dt"))
sfx$missing_SG_per_gap=sfx$missing_SG/sfx$numb_gap_per_ad*1.
summary(sfx$missing_SG_per_gap)

rdm = rdy[rdy$missing_pct>=90,]
rdd = count(rdm, vars="dt")

#==================

user a, b
day d1, d2
d1 8, 6
d2 2

per user per active day: ((8+2)/2+6/1)/2 days= 11/2=5.5
per day per active user: ((8+6)/2+2/1)/2 users= 8/2 = 4


#filter out those users whose missing is more than 300 SGs
t = sfc[sfc$mean_missing>=288,]

x = sfc$numb_gap
h <- hist(x, breaks=40, xlab="Average Number of Missing", ylab="# of users", main="Histogram of Missing CleanSG", col="green")


x = sfc$mean_missing
h <- hist(x, breaks=40, xlab="Average Number of Missing", ylab="# of users", main="Histogram of Missing CleanSG", col="green")

d <- density(x) # returns the density data
plot(d, main="Density of Average Missing cleanSG Number") # plots the results
polygon(d, col="green", border="blue")

gap_freq_cont = function(f){
  rn = rm[rm$PERSON_ID==f,]
  #take rows 1 to n-1 and subtract rows 2 to n:
  rn$gap <- c(NA, with(rn, epoch[-1] - epoch[-nrow(rn)]))
  next_start = rn[which(rn$gap>350),]

  #compute the continuous gap duration - which is the cleanSG gap in this case
  t = c(rownames(rn[1,]), rownames(next_start), rownames(rn[nrow(rn),]))
  t=unique(t)
  rn_need = rn[t,]
  #rn_need$gap[nrow(rn_need)]=0
  rn_need$cont <- c(NA, with(rn_need, epoch[-1] - gap[-1] - epoch[-nrow(rn_need)]))
  #remove the starting point
  rn_need = rn_need[!is.na(rn_need$gap),]
  #remove zero gaps - which is no missing SGs between the two consequential gaps
  rn_need=rn_need[(rn_need$cont>0),]

  return (data.frame(rn_need))
}


dr = rawsg[,c("PERSON_ID","SG", "SG_TIMESTAMP","SG_TIMESTAMP_TZ","CREATED_AT")]
length(unique(dr$PERSON_ID))
[1] 927


dr$epoch = as.numeric(strptime(dr$SG_TIMESTAMP,"%Y-%m-%d-%H.%M.%S", tz="UTC"))
dr$timezone=as.numeric(dr$SG_TIMESTAMP_TZ)/3600
dr$epoch = dr$epoch+as.numeric(dr$SG_TIMESTAMP_TZ)
dr$create_epoch = as.numeric(strptime(dr$CREATED_AT,"%Y-%m-%d-%H.%M.%S", tz="UTC"))
dr$create_epoch = dr$create_epoch+as.numeric(dr$SG_TIMESTAMP_TZ)
dr$dt = as.Date(as.POSIXct(dr$epoch, origin="1970-01-01"))
dr = dr[(dr$dt>="2019-12-01" & dr$dt<="2020-03-02"),]


#dr = dr[order(dr$epoch),] #don not sort in order
fd = unique(dr$PERSON_ID)
dr2 <- do.call(rbind, lapply(fd, function(x) rollback_rawsg(x)))


ff = unique(adtotal$PERSON_ID)
v <- do.call(rbind, lapply(ff, function(x) compute_outoforder(x)))



#rr = dr2[dr2$PERSON_ID==f,]
#rr = rr[order(rr$CREATED_AT),]
compute_outoforder = function(f){
  rr = dr2[dr2$PERSON_ID==f,]
  rr = rr[order(rr$CREATED_AT),]
  #take rows 1 to n-1 and subtract rows 2 to n:
  rr$gain <- c(NA, with(rr, epoch[-1] - epoch[-nrow(rr)]))
  outoforder = rr[which(rr$gain<0),]
  if (nrow(outoforder)>0){
    ood = count(outoforder, vars=c("PERSON_ID"))
    ood$numb_days=length(unique(rr$dt))
    #ood$numb_delay_sg=ceiling((rr$create_epoch - rr$epoch)/300)
    return (data.frame(ood))
  }
}




v1 <- merge(x=v, y=adtotal, by=c("PERSON_ID"))
names(v1)[2]="numb_outoforder"
v1$numb_outoforder_per_ad = v1$numb_outoforder/v1$numb_days
summary(v1$numb_outoforder)
summary(v1$numb_days)
summary(v1$numb_outoforder_per_ad)

write.csv(v,paste(src,"outoforder_case.csv"), row.names=FALSE)
write.csv(rr,paste(src,"ck.csv"), row.names=FALSE)

rr1 = dr2[dr2$PERSON_ID==f,]
write.csv(rr1,paste(src,"50929100_pulling_order.csv"), row.names=FALSE)
rr = rr1[order(rr1$CREATED_AT),]
write.csv(rr,paste(src,"50929100_orderby_createdat.csv"), row.names=FALSE)

ry = rawsg[rawsg$PERSON_ID==f,]
write.csv(ry,paste(src,f,"_rawSG.csv"), row.names=FALSE)
#========

#merge with active users
dr5 <- merge(x=dr2, y=adtotal, by=c("PERSON_ID"))
> length(unique(dr5$PERSON_ID))
[1] 235
> nrow(dr5)
[1] 2483391
#merge with days more than 25% full SG
> nrow(pt[(pt$freq>=72),])
[1] 19824
> nrow(pt1)
[1] 19824
pdr = merge(x = dr5, y = pt1, by=c("PERSON_ID","dt"))
> nrow(pdr)
[1] 2450442
> nrow(dr5)
[1] 2483391


#========
#test
z1 <- c("a","b","b","a","a","c","a","a","a","a","b","c")
rle(z1)
> rle(z1)$lengths
[1] 1 2 2 1 4 1 1
> sequence(rle(z1)$lengths)
[1] 1 1 2 1 2 1 1 2 3 4 1 1
rn = rm[rm$PERSON_ID==f,]


rn = rm[rm$PERSON_ID==f,]
#take rows 1 to n-1 and subtract rows 2 to n:
rn$gap <- c(NA, with(rn, epoch[-1] - epoch[-nrow(rn)]))
rn[which(rn$gap<350),]$gap=300
rn[which(rn$gap>=350),]$gap=NA
rnt = count(rn,vars=c("gap"))
#numb of gap
numb_gap = rnt[is.na(rnt$gap),]$freq
#average SG in a gap
as.integer(nrow(rn)/numb_gap)

g = with(rle(rn$gap), lengths[values == 300])
g1 = g[which(g>0)]+1
g2 = c(rep(1,times=numb_gap-length(g1)), g1)
sum(g2)/numb_gap
with(rle(rn$gap), max(lengths[values == 300], na.rm=T))

most_consecutive_val = function(x, val = 300) {
  with(rle(x), max(lengths[values == val], na.rm=T))
}

#========


compare_cleansg_rawsg <- function(f){
  person_id = f
  f = 50895138

  #read in the cleanSG
  dv = read.csv(paste(src, "USERSG_", f, ".csv", sep=""), header=T,stringsAsFactors = FALSE)[,c("PERSON_ID","PUMP_TIME_TS","SGMGDL")]
  dv$epoch = as.numeric(strptime(dv$PUMP_TIME_TS,"%Y-%m-%d-%H.%M.%S", tz="UTC"))
  #if there is timezone, dv$epoch = dv$epoch-3600*as.numeric(substr(dv$sg_timestamp,20,22))
  dv = dv[order(dv$epoch),]

  #epoch -> date  as.Date(as.POSIXct(val, origin="1970-01-01"))
  dv$dt = as.Date(dv$PUMP_TIME_TS)

  #check duplicates
  #count_epoch <- aggregate(dv, by=list(dv$PERSON_ID, dv$epoch), FUN=length);
  #library(plyr)
  count_epoch=count(dv, vars=c("PERSON_ID","epoch"))
  dup_epoch=count_epoch[count_epoch$freq>1,]
  if(nrow(dup_epoch)>0){
    dup_epoch$dt = as.Date(as.POSIXct(dup_epoch$epoch, origin="1970-01-01"))
    dup_epoch=dup_epoch[,-c(3)]
    #count dups by date
    count_dup=count(dup_epoch,vars=c("PERSON_ID","dt"))
  }
  #dedup cleanSG
  dv1 = unique(dv)
  #count by date
  cleansg_dt=count(dv1, vars=c("PERSON_ID","dt"))

  #read in the rawSG
  dr = read.csv(paste(src, "SENSOR_GLUCOSE_DATA_", f, ".csv", sep=""), header=T,stringsAsFactors = FALSE)[,c("PERSON_ID","SG", "SG_TIMESTAMP","SG_TIMESTAMP_TZ")]
  dr$epoch = as.numeric(strptime(dr$SG_TIMESTAMP,"%Y-%m-%d-%H.%M.%S", tz="UTC"))
  dr$timezone=as.numeric(dr$SG_TIMESTAMP_TZ)/3600
  dr$epoch = dr$epoch+as.numeric(dr$SG_TIMESTAMP_TZ)
  dr = dr[order(dr$epoch),]
  #assign UTC date
  dr$dt = as.Date(as.POSIXct(dr$epoch, origin="1970-01-01"))

  #check duplicates
  raw_epoch=count(dr, vars=c("PERSON_ID","epoch"))
  rawdup_epoch=raw_epoch[raw_epoch$freq>1,]
  if(nrow(rawdup_epoch)>0){
    rawdup_epoch$dt = as.Date(as.POSIXct(rawdup_epoch$epoch, origin="1970-01-01"))
    rawdup_epoch=rawdup_epoch[,-c(3)]
    #count dups by date
    rawcount_dup=count(rawdup_epoch,vars=c("PERSON_ID","dt"))
  }

  #dup rawSG
  dr1 = unique(dr)
  #count by date
  rawsg_dt=count(dr1, vars=c("PERSON_ID","dt"))

  #merge cleansg and rawsg counts
  names(cleansg_dt)[3]="cleansg"
  names(rawsg_dt)[3]="rawsg"
  sgcount = merge(x = cleansg_dt, y = rawsg_dt, by = c("PERSON_ID", "dt"), all.y = TRUE)



}



###################################


# Example 3: Analyze the impact to the PE re-distributions and job load balances 
# due to the streams job restarting caused by various network and system issues



#========= count the PE restart and across all streams application nodes from app1 to app5
#command to parse out the PE restart freq on each node
#cat strapp2.txt | grep "Completed reading the DPS configuration" | grep -o "pe.\w*.out" | sort | uniq -c | sort > n2.txt
#read in the PE


#or=====

#pe = data.frame(matrix(ncol =1, nrow = 5))
#for (i in 1:5){
  #pe[i,1]=paste("pe", i, sep = "")
#}
#pe[1:5,1] = paste("pe", c(1:5), sep = "")
#pe = paste("pe", c(1:5), sep = "")

src="/Users/lcao@us.ibm.com/Downloads/defect_sgclean/"
for (i in 1:5){
  assign(paste("pe", i, sep = ""), read.table(paste(src, "n", i, ".txt", sep=""), header=F, sep="", stringsAsFactors = FALSE) )
}


for (i in 1:5){
  pe = read.table(paste(src, "n", i, ".txt", sep=""), header=F, sep="", stringsAsFactors = FALSE)
  names(pe)=c("restart_freq","pe_id")
  pe$app_numb=i
  assign(paste0("pe",i), pe)
}


#avg, min, max, std restart on each app node
summary(pe1$restart_freq)
sd(pe1$restart_freq, na.rm = FALSE)
summary(pe2$restart_freq)
sd(pe2$restart_freq, na.rm = FALSE)
summary(pe3$restart_freq)
sd(pe3$restart_freq, na.rm = FALSE)
summary(pe4$restart_freq)
sd(pe4$restart_freq, na.rm = FALSE)
summary(pe5$restart_freq)
sd(pe5$restart_freq, na.rm = FALSE)

> summary(pe1$restart_freq)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
1.000   1.000   2.000   3.487   4.000  27.000
> sd(pe1$restart_freq, na.rm = FALSE)
[1] 3.835806
> summary(pe2$restart_freq)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
1.00    1.00    2.00    3.96    5.00   23.00
> sd(pe2$restart_freq, na.rm = FALSE)
[1] 3.773114
> summary(pe3$restart_freq)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
1.000   2.000   4.000   6.059   8.000  34.000
> sd(pe3$restart_freq, na.rm = FALSE)
[1] 5.368381
> summary(pe4$restart_freq)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
1.00    2.00    4.00    4.49    6.00   16.00
> sd(pe4$restart_freq, na.rm = FALSE)
[1] 3.528713
> summary(pe5$restart_freq)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
1.000   1.000   2.000   3.214   4.000  13.000
> sd(pe5$restart_freq, na.rm = FALSE)
[1] 2.386298


#total pes
#pe12 = merge(x = pe1[,2,drop=F], y = pe2[,2,drop=F], by = c("pe_id"), all = TRUE)
#pe123 = merge(x = pe12[,,drop=F], y = pe3[,2,drop=F], by = c("pe_id"), all = TRUE)
#pe1234 = merge(x = pe123[,,drop=F], y = pe4[,2,drop=F], by = c("pe_id"), all = TRUE)
#peall = merge(x = pe1234[,,drop=F], y = pe5[,2,drop=F], by = c("pe_id"), all = TRUE)

pesum = rbind(pe1,pe2,pe3,pe4,pe5)

pesum = pesum[order(pesum$pe_id),]
peapp=count(pesum, vars=c("pe_id"))
peapp = peapp[order(peapp$freq),]

#total pe
> nrow(peapp)
[1] 581

#across nodes freq
summary(peapp$freq)
> summary(peapp$freq)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
1.000   1.000   2.000   2.892   5.000   5.000
> length(which(peapp$freq==1))/581*100
[1] 35.28399
> length(which(peapp$freq==2))/581*100
[1] 14.80207
> length(which(peapp$freq==3))/581*100
[1] 6.36833
> length(which(peapp$freq==4))/581*100
[1] 12.56454
> length(which(peapp$freq==5))/581*100
[1] 30.98107

#across which app nodes
#pe numb
#app1 351, app2 354, app3 323, app4 339, app5 313

#remove pes which doesn't move
pemove = peapp[peapp$freq>1,]
> nrow(pemove)
[1] 376
#merge with pesum
pem = merge(x = pemove[,1,drop=F], y = pesum, by = c("pe_id"), all.x = TRUE)
> nrow(pem)
[1] 1475
length(which(pem$app_numb==1))/nrow(pem)*100.
> length(which(pem$app_numb==1))/nrow(pem)*100.
[1] 19.05085
> length(which(pem$app_numb==2))/nrow(pem)*100.
[1] 21.01695
> length(which(pem$app_numb==3))/nrow(pem)*100.
[1] 20.81356
> length(which(pem$app_numb==4))/nrow(pem)*100.
[1] 20.67797
> length(which(pem$app_numb==5))/nrow(pem)*100.
[1] 18.44068
> sd(pem$app_numb, na.rm = FALSE)
[1] 1.384799

#sum up the restart freq for moving PEs
#by app node
summary(pem[pem$app_numb==1,]$restart_freq)
> summary(pem[pem$app_numb==1,]$restart_freq)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
1.000   1.000   2.000   4.011   5.000  27.000
> summary(pem[pem$app_numb==2,]$restart_freq)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
1.000   2.000   3.000   4.345   6.000  23.000
> summary(pem[pem$app_numb==3,]$restart_freq)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
1.000   2.000   5.000   6.277   8.000  34.000
> summary(pem[pem$app_numb==4,]$restart_freq)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
1.000   2.000   4.000   4.833   7.000  16.000
> summary(pem[pem$app_numb==5,]$restart_freq)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
1.000   2.000   3.000   3.478   4.000  13.000

pefreq = aggregate(pem$restart_freq, by=list(pem$pe_id), FUN=sum);
names(pefreq)=c("pe_id","total_restart")
summary(pefreq$total_restart)
> summary(pefreq$total_restart)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
2.00    4.00   16.00   18.14   27.00   65.00
sd(pefreq$total_restart, na.rm = FALSE)
[1] 14.35154
pefreq = pefreq[order(pefreq$total_restart),]
restartfreq=count(pefreq, vars=c("total_restart"))

#39 PEs have 2 restarts
summary(restartfreq$freq)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
1.000   2.000   5.000   6.963   8.000  39.000
sd(restartfreq$freq)
> sd(restartfreq$freq)
[1] 8.211733

#scatter plot===
y = pefreq$total_restart
plot(y)
x <- pefreq$total_restart
h <- hist(x, breaks=20, xlab="Restart Freq", ylab="# of PEs", main="Hist of total restart", col="green")
xfit<-seq(min(x),max(x),length=55)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

d <- density(pefreq$total_restart) # returns the density data
plot(d, main="Density of total restart") # plots the results
polygon(d, col="green", border="blue")

d1 <- density(pem[pem$app_numb==1,]$restart_freq)
plot(d1)
polygon(d1, border="blue")
d2 <- density(pem[pem$app_numb==2,]$restart_freq)
lines(d2)
polygon(d2, border="red")
d3 <- density(pem[pem$app_numb==3,]$restart_freq)
lines(d3)
polygon(d3, border="green")
d4 <- density(pem[pem$app_numb==4,]$restart_freq)
lines(d4)
polygon(d4, border="brown")
d5 <- density(pem[pem$app_numb==5,]$restart_freq, adjust=2)
lines(d5)
polygon(d5, border="yellow")


d5 <- density(pem[pem$app_numb==5,]$restart_freq, adjust = 2)
plot(d5)
polygon(d5, border="black")
summary(pem[pem$app_numb==1,]$restart_freq)


#==draw density functions

#library(ggplot2)
#ggplot(restartfreq, aes(x=total_r, y = value)) + geom_point(shape = 3)


#========


###################################


# Example 4: Analyze the impact brought by the missing data (gap) and catchup packets
# to the hypo alerts - such as false hypo alert, renewed hypo alerts and the impact to
# the FDR (false discovery ratio)


dst = "/storage2/medtronics/src_lcao/r3_prod_sample/"
dst2 <<- "/storage2/medtronics/src_lcao/r3_prod_sample/alert_special/"
fid1 <- sub("_alert_db2.csv", "", list.files(src, pattern = "[0-9]_alert_db2.csv$"))
fid4 <- sub("_hypofeature_kafka.csv", "", list.files(src, pattern = "[0-9]_hypofeature_kafka.csv$"))
fid14 = intersect(fid1,fid4)
#> length(fid14)
#[1] 402


for (f in fid14[startn:endn]){

  #f=50788056
  fa1 = check_alert_special(f)

}

#tp <- do.call(rbind, lapply(f, function(x) check_alert_special(x)))

tp <- do.call(rbind, lapply(fid14, function(x) check_alert_special(x)))
dst="/storage2/medtronics/src_lcao/r3_prod_sample/"
write.csv(tp, paste(dst, "r3_alert_all.csv", sep=""), row.names=FALSE)

#minutes after gap <= 15 minutes
#gap size min = 60 minutes
#include sg value
#include whether false or not

tp1 = tp[which(tp$min_after_gap<=15 & tp$gap_size_min>=60),]
write.csv(tp1, paste(dst, "r3_alert_filtered.csv", sep=""), row.names=FALSE)

tp2 = tp1[which(tp1$false_alert==1),]

ff = unique(as.character(tp2$person_id))

tp2 <<- tp2[order(tp2$person_id, tp2$epoch),]
tv <- do.call(rbind, lapply(ff, function(x) check_feature_vector(x)))
write.csv(tv, paste(dst, "r3_alert_feature_vectors.csv", sep=""), row.names=FALSE)

n = names(tv)[28:45]


sum_null_freq <- function(n){
  u = data.frame(length(which(tv[,n]==-999)))
  names(u)[1]=names(tv)[n]
  return (u);
}
tt <-  do.call(cbind, lapply(c(28:45), function(x) sum_null_freq(x)))

write.csv(tt, paste(dst, "r3_alert_sum.csv", sep=""), row.names=FALSE)


check_feature_vector <- function(f){
  person_id = f
  dv = read.csv(paste(src, f, "_hypofeature_kafka.csv", sep=""), header=T,stringsAsFactors = FALSE)
  dv$epoch = as.numeric(strptime(dv$sg_timestamp,"%Y-%m-%dT%H:%M:%S", tz="UTC"))
  dv$epoch = dv$epoch-3600*as.numeric(substr(dv$sg_timestamp,20,22))
  names(dv)[1]="person_id"
  dv = dv[order(dv$epoch),]
  #merge tp1 with dv
  tp3 = tp2[which(tp2$person_id==f),]
  tpv = merge(x = tp3, y = dv, by = c("person_id", "epoch"))
  return (tpv)
}





check_alert_special <- function(f){

    #get the alert
    #f=50788056
    person_id <<- f
    da = read.csv(paste(src, f, "_alert_db2.csv", sep=""), header=T)
    da = da[,c("userid","alert_start")]
    da$epoch = as.numeric(strptime(da$alert_start,"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
    da = da[,-2]
    da$alert=1
    names(da)[1]="person_id"
    da <<- da[order(da$epoch),]


    #general
    numb_SG=0
    duration_in_days=0

    #dups
    numb_dups=0
    pct_dups=0

    #hypo
    pct_hypoSG=0

    #gap
    pct_missing=0
    numb_gap = 0
    avg_gap_in_mins=0
    stdv_gap=0
    avg_cont_in_mins=0


    #merge with feature vector to check the missing and hypoSG
    #hypo-SG in 4-hr watch-up window after the alert being sent
    dv = read.csv(paste(src, f, "_hypofeature_kafka.csv", sep=""), header=T,stringsAsFactors = FALSE)[,c(1:6,19:21)]
    dv$epoch = as.numeric(strptime(dv$sg_timestamp,"%Y-%m-%dT%H:%M:%S", tz="UTC"))
    dv$epoch = dv$epoch-3600*as.numeric(substr(dv$sg_timestamp,20,22))
    dv = dv[,-c(2,4)]
    names(dv)[1]="person_id"
    dv$hypoSG=ifelse(dv$sglatest>=80,0,1) #change to 70mgdL for more restricted rule
    dv <<- dv[order(dv$epoch),]


    numb_SG=nrow(dv)
    if(nrow(dv)>0){

      #dups
      numb_dups = numb_SG - length(unique(dv$epoch))
      if(numb_dups>0){
        pct_dups = numb_dups/numb_SG*100.
      }
      #de-dup
      dv <- dv[!duplicated(dv$epoch),]

      #duration in days
      duration_in_days=(dv$epoch[nrow(dv)]-dv$epoch[1])/(3600*24)

      #hypoSG
      dv$hypoSG=ifelse(dv$sg>=70,0,1)
      pct_hypoSG = sum(dv$hypoSG)/numb_SG*100.

      #gap
      if(duration_in_days>0){
        pct_missing=100-numb_SG*(1-pct_dups/100.)/(duration_in_days*288)*100.
        if (pct_missing<0){
          pct_missing=0
        }
      }

      #take rows 1 to n-1 and subtract rows 2 to n:
      dv$gap <- c(NA, with(dv, epoch[-1] - epoch[-nrow(dv)]))

      df3 <<- dv[which(dv$gap>350),]
      if(length(which(dv$gap>350))>0){
        numb_gap = nrow(df3)
        #average gap duration
        avg_gap_in_mins = mean(df3$gap, na.rm=T)/60
        stdv_gap=sd(df3$gap, na.rm=T)
      }

      #compute the average continuous session duration
      t3 = c(1, as.numeric(rownames(df3)), nrow(dv))
      df4 = dv[t3,]
      df4$gap[nrow(df4)]=0
      df4$cont <- c(NA, with(df4, epoch[-1] - gap[-1] - epoch[-nrow(df4)]))

      #average continuous session duration
      avg_cont_in_mins = mean(df4$cont, na.rm=T)/60
    }

      #for each alert in da, compute the SG, the closest gap, and lagging from the gap
      #missing SG in 4-hr watch-up window
      da <<- da
      dv <<- dv

      if(nrow(da)>0 & numb_gap>0){
        w <- do.call(rbind, lapply(da$epoch, function(e) check_special_case(e)))
        #merge w
        #da1 = merge(x = da, y = w, by = c("epoch"))
        return (w)
        #write.csv(tp, paste(dst2, f, "_as.csv", sep=""), row.names=FALSE)
      }

      #column bind with da

}


#compute pct of special case
#alert ocurrs within 15min after missing
#alert's SG is greater than 90
#missing gap size

#based on each alert
#alert's SG value
#the lagging of previous gap
#the gap size

#mark each gap with size and ending epoch
#the closest ending epoch with each alert

check_special_case <- function(e){

  #e = da$epoch[1]
  epoch=e
  #alert's sg value
  alert_sg=NA
  if(length(which(dv$epoch==e))>0){
    alert_sg = dv[which(dv$epoch==e),]$sglatest
  }
  #time past since the last gap ending epoch to the alert
  min_after_gap = NA
  gap_size_min=NA
  numb_gap_before_alert=length(which(df3$epoch<=e))
  if(numb_gap_before_alert>0){
    min_after_gap = (e - df3[numb_gap_before_alert,]$epoch)/60.
    gap_size_min = df3[numb_gap_before_alert,]$gap/60.
  }

  #check TP or FP
  dt = dv[which(dv$epoch>=e & dv$epoch<=e+4*3600),]

  first_hypoSG=NA
  leadtime_firstHypo=NA

  if(nrow(dt)>0){
      false_alert=ifelse(sum(dt$hypoSG, na.rm=T)>0,0,1)
      if(false_alert==0){
        first_hypoSG=dt$epoch[which(dt$hypoSG==1)[1]]
        leadtime_firstHypo=(first_hypoSG-e)/60.
      }
  }

  return (data.frame(person_id, epoch, alert_sg, min_after_gap, gap_size_min,false_alert, first_hypoSG, leadtime_firstHypo))

}


############# include all alert

check_watchup_include_missing <- function(e){

  hypoSG_watchup=NA
  win_watchup=NA
  first_hypoSG=NA
  epoch = e
  false_alert=1

  alert_sg=NA
  if(length(which(dv$epoch==e))>0){
    alert_sg = dv[which(dv$epoch==e),]$sglatest
  }

  #look for the first hypoSG after sending out this alert
  #in any length window after the alert is sent out
  dt = dv[which(dv$epoch>=e),]


  if(nrow(dt)>0){
    hypoSG_watchup=ifelse(sum(dt$hypoSG, na.rm=T)>0,1,0)
    if(hypoSG_watchup==1){
      first_hypoSG=dt$epoch[which(dt$hypoSG==1)[1]]
      win_watchup=(first_hypoSG-e)/60.
      if (win_watchup <= 240){ #first hypoSG is beyond 4 hours window
        false_alert = 0;
      }
    }

  }

  return (data.frame(epoch, alert_sg, hypoSG_watchup,first_hypoSG,win_watchup, false_alert))
}


check_alert_include_missing <- function(f){


  person_id = f
  da = read.csv(paste(src, f, "_alert_db2.csv", sep=""), header=T)
  da = da[,c("userid","alert_start")]
  da$epoch = as.numeric(strptime(da$alert_start,"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
  da = da[,-2]
  names(da)[1]="person_id"
  da <<- da[order(da$epoch),]

  numb_total_alert=0
  da$alert_lag <- c(240, with(da, epoch[-1] - epoch[-nrow(da)])/60.)
  numb_total_alert=nrow(da)

  da <<- da

  if(nrow(da)>0){
    #merge with feature vector to check the missing and hypoSG
    #hypo-SG in 4-hr watch-up window after the alert being sent
    dv = read.csv(paste(src, f, "_hypofeature_kafka.csv", sep=""), header=T,stringsAsFactors = FALSE)[,c(1:6,19:21)]
    dv$epoch = as.numeric(strptime(dv$sg_timestamp,"%Y-%m-%dT%H:%M:%S", tz="UTC"))
    dv$epoch = dv$epoch-3600*as.numeric(substr(dv$sg_timestamp,20,22))
    dv = dv[,-c(2,4)]
    names(dv)[1]="person_id"
    dv$hypoSG=ifelse(dv$sglatest>=70,0,1) #change to 70mgdL for more restricted rule
    dv <<- dv[order(dv$epoch),]


    #include all alerts
    k <- do.call(rbind, lapply(da$epoch, function(e) check_watchup_include_missing(e)))

    #merge back to original alert set
    da = merge(x = da, y = k, by = c("epoch"))
    return (da)

  }

}

#tm <- do.call(rbind, lapply(f, function(x) check_alert_include_missing(x)))
tm <- do.call(rbind, lapply(fid14, function(x) check_alert_include_missing(x)))
dst="/storage2/medtronics/src_lcao/r3_prod_sample/"
write.csv(tm, paste(dst, "r3_alert_renew.csv", sep=""), row.names=FALSE)

#include all alerts
sum(tm$false_alert,na.rm=T)/nrow(tm)*100.
[1] 68.71781

#exclude the alert with win_watchup==0 meaning the alert are hypoSG
tm1 = tm[which(tm$win_watchup!=0),]
sum(tm1$false_alert,na.rm=T)/nrow(tm1)*100.
[1] 63.51399

#compute renewed alert rates
#alerts having hypoSG found
tm2 = tm[which(tm$hypoSG_watchup!=0),]

#no limit to the watchup window - as long as the weekly-pull data lasts to the end of the week
#unique person_id and first_hypoSG
tm3 = tm2[!duplicated(tm2[c(2,6)]),]
100 - nrow(tm3)/nrow(tm2)*100
[1] 69.77674

#watch window size is 4hr - remove win_watchup more than 4hrs
dim(tm2[which(tm2$win_watchup>240),])
[1] 3633    8
> dim(tm2[which(tm2$false_alert==1),])
[1] 3633    8

tm4 = tm2[which(tm2$win_watchup<=240),]

#allow new alert generated after 30min - the current setting
tm5 = tm4[!duplicated(tm4[c(2,6)]),]
#renew alert ratio
100 - nrow(tm5)/nrow(tm4)*100
[1] 28.3664

#allow new alert generated after 1hr
tm6 = tm4[which(tm4$alert_lag>=60),]
tm7 = tm6[!duplicated(tm6[c(2,6)]),]
100 - nrow(tm7)/nrow(tm6)*100
[1] 17.07799

#allow new alert generated after 1.5hr
tm8 = tm4[which(tm4$alert_lag>=90),]
tm9 = tm8[!duplicated(tm8[c(2,6)]),]
100 - nrow(tm9)/nrow(tm8)*100
[1] 9.257206

#allow new alert generated after 2hr
tm10 = tm4[which(tm4$alert_lag>=120),]
tm11 = tm10[!duplicated(tm10[c(2,6)]),]
100 - nrow(tm11)/nrow(tm10)*100
[1] 4.694256

#allow new alert generated after 2.5hr
tm10 = tm4[which(tm4$alert_lag>=150),]
tm11 = tm10[!duplicated(tm10[c(2,6)]),]
100 - nrow(tm11)/nrow(tm10)*100
[1] 2.33489

#allow new alert generated after 3hr
tm10 = tm4[which(tm4$alert_lag>=180),]
tm11 = tm10[!duplicated(tm10[c(2,6)]),]
100 - nrow(tm11)/nrow(tm10)*100
[1] 0.8626887

#allow new alert generated after 3.5hr
tm10 = tm4[which(tm4$alert_lag>=210),]
tm11 = tm10[!duplicated(tm10[c(2,6)]),]
100 - nrow(tm11)/nrow(tm10)*100
[1] 0.154321

#allow new alert generated after 4hr
tm10 = tm4[which(tm4$alert_lag>=240),]
tm11 = tm10[!duplicated(tm10[c(2,6)]),]
100 - nrow(tm11)/nrow(tm10)*100
[1] 0


################## drop alerts having missing SG in the next 4hr window


check_watchup_drop_missing <- function(e){
  pct_missing_watchup=NA
  has_missing=NA
  hypoSG_watchup=NA
  leadtime_watchup=NA
  first_hypoSG=NA
  epoch = e

  dt = dv[which(dv$epoch>=e & dv$epoch<=e+4*3600),]

  if(nrow(dt)>0){
    has_missing = 0
    pct_missing_watchup=(1-nrow(dt)/48)*100.
    if(pct_missing_watchup<0){
      pct_missing_watchup=0.
    }
    if(pct_missing_watchup>0){
      has_missing = 1
    }

    if(has_missing==0){ #only compute lead time for alert without SG missing in the watchup window
      hypoSG_watchup=ifelse(sum(dt$hypoSG, na.rm=T)>0,1,0)
      if(hypoSG_watchup==1){
        first_hypoSG=dt$epoch[which(dt$hypoSG==1)[1]]
        leadtime_watchup=(first_hypoSG-e)/60.
      }
    }
  }

  return (data.frame(epoch, has_missing, pct_missing_watchup,hypoSG_watchup,first_hypoSG,leadtime_watchup))
}




check_alert_modify <- function(f){

  #f=50417539
  #f=50788056
  person_id = f
  da = read.csv(paste(src, f, "_alert_db2.csv", sep=""), header=T)
  da = da[,c("userid","alert_start")]
  da$epoch = as.numeric(strptime(da$alert_start,"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
  da = da[,-2]
  da$alert=1
  names(da)[1]="person_id"
  da <<- da[order(da$epoch),]

  numb_total_alert=0
  avg_alert_lag=0
  stdv_alert_lag=0
  da$lag <- c(240, with(da, epoch[-1] - epoch[-nrow(da)])/60.)
  numb_total_alert=nrow(da)
  avg_alert_lag=mean(da$lag, na.rm=T)
  stdv_alert_lag=sd(da$lag, na.rm=T)

  FDR_org=NA
  FDR = NA

  if(nrow(da)>0){
    #merge with feature vector to check the missing and hypoSG
    #hypo-SG in 4-hr watch-up window after the alert being sent
    dv = read.csv(paste(src, f, "_hypofeature_kafka.csv", sep=""), header=T,stringsAsFactors = FALSE)[,c(1:6,19:21)]
    dv$epoch = as.numeric(strptime(dv$sg_timestamp,"%Y-%m-%dT%H:%M:%S", tz="UTC"))
    dv$epoch = dv$epoch-3600*as.numeric(substr(dv$sg_timestamp,20,22))
    dv = dv[,-c(2,4)]
    names(dv)[1]="person_id"
    dv$hypoSG=ifelse(dv$sglatest>=80,0,1) #change to 70mgdL for more restricted rule
    dv <<- dv[order(dv$epoch),]


    #missing SG in 4-hr watch-up window
    k <- do.call(rbind, lapply(da$epoch, function(e) check_watchup_drop_missing(e)))
    pct_missing_watchup=mean(k$pct_missing_watchup,na.rm=T)
    numb_alert_missingSG=length(which(k$pct_missing_watchup>0))

    #merge back to original alert set
    da = merge(x = da, y = k, by = c("epoch"))

    #count the true negative alerts among alert having non-missing in watchup window
    numb_alert_non_missingSG=length(which(da$has_missing==0))
    numb_fp_alert=length(which(da$has_missing==0 & da$hypoSG_watchup==0))
    numb_fp_alert_org=length(which(da$hypoSG_watchup==0))

    if(numb_alert_non_missingSG>0){
      FDR=numb_fp_alert/numb_alert_non_missingSG*100
    }

    FDR_org=numb_fp_alert_org/numb_total_alert*100

    #remove any alerts which is the hypoSG itself - its leadtime is 0
    da0 = da[which(da$leadtime_watchup!=0),]

    #keep those ones having hypoSG found
    da1 = da0[which(da0$hypoSG_watchup==1),]

    #sort by first_hypoSG, and keep the earliest alert with the same first_hypoSG
    pct_renew_tp_alert=NA
    sum_leadtime=NA
    numb_tp_alert=nrow(da1)
    nonrenew_lag=NA
    avg_leadtime=NA

    if(numb_tp_alert>0){
      da1$rank= ave(da1$epoch, da1$first_hypoSG, FUN = seq_along)
      da1$lag2 <- c(240, with(da1, epoch[-1] - epoch[-nrow(da1)])/60.)
      nonrenew_lag = mean(da1$lag2, na.rm=T)

      da2 = da1[which(da1$rank==1),]
      #renew alert among true positive alerts
      pct_renew_tp_alert = (1-nrow(da2)/nrow(da1))*100


      if(nrow(da2)>0){
        sum_leadtime=sum(da2$leadtime_watchup, na.rm=T)
        avg_leadtime=sum_leadtime/numb_tp_alert
      }

    }

  }

  return(data.frame(person_id,numb_total_alert,avg_alert_lag,stdv_alert_lag,
                    numb_alert_missingSG,pct_missing_watchup,
                    numb_alert_non_missingSG,numb_fp_alert,numb_fp_alert_org,FDR,FDR_org,
                    nonrenew_lag,pct_renew_tp_alert,sum_leadtime,numb_tp_alert,avg_leadtime))
}



fid1 <- sub("_alert_db2.csv", "", list.files(src, pattern = "[0-9]_alert_db2.csv$"))
#> length(fid1)
#[1] 500
fid4 <- sub("_hypofeature_kafka.csv", "", list.files(src, pattern = "[0-9]_hypofeature_kafka.csv$"))
#> length(fid4)
#[1] 470
fid14 = intersect(fid1,fid4)
#> length(fid14)
#[1] 402

tm <- do.call(rbind, lapply(fid14, function(x) check_alert_modify(x)))
dst="/storage2/medtronics/src_lcao/r3_prod_sample/"
write.csv(tm, paste(dst, "r3_alert_drop_missing_checkup.csv", sep=""), row.names=FALSE)


#> colMeans(tm[,2:16], na.rm=T)
#numb_total_alert            avg_alert_lag           stdv_alert_lag     numb_alert_missingSG      pct_missing_watchup numb_alert_non_missingSG            numb_fp_alert        numb_fp_alert_org
#18.838308               457.841832               598.088398                 3.562189                 8.714733                15.276119                 7.452736                 7.452736
#FDR                  FDR_org             nonrenew_lag       pct_renew_tp_alert             sum_leadtime            numb_tp_alert             avg_leadtime
#52.494804                40.848596               727.432941                22.569480               476.518902                 6.174129                60.307536


#> lead_time=sum(tm$sum_leadtime,na.rm=T)/sum(tm$numb_tp_alert,na.rm=T)
#> lead_time
#[1] 62.97268

#> FDR=sum(tm$numb_fp_alert,na.rm=T)/sum(tm$numb_alert_non_missingSG,na.rm=T)*100
#> FDR
#[1] 48.78684

#> FDR_org=sum(tm$numb_fp_alert_org,na.rm=T)/sum(tm$numb_total_alert,na.rm=T)*100
#> FDR_org
#[1] 39.5616
