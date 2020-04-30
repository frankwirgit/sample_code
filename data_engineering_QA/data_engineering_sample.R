#
# An example of the data re-engineering process by reading the raw data from multiple data sources 
# with different study IDs, convert them with the given formats, get them mapped and unified, then
# merge them to the pre-designed data repository
# Those patient data used in this example have been de-identified
#


library(xlsx)
#src = "/data/new_feb_2017/10004/"

std1="04092"
std2="10003"
std3="10004"
std4="10008"

part = "SH"
#part="AE"
#"OP","KO","KP","KB","EQ","OK","KS","MH"

src = "C:/Users/IBM_ADMIN/Documents/fwork/Projects/JJ_prod/depuy/"
f1 <- read.xlsx(paste(src, std1, "/STUDY_", std1, "_Update_CRF_08_", part, ".xlsx", sep=""), sheetName="CONTENTS")
f3 <- read.xlsx(paste(src, std2, "/STUDY_", std2, "_CRF_10_", part, ".xlsx", sep=""), sheetName="CONTENTS")
f <- read.xlsx(paste(src,"data_map_0405Y17.xlsx", sep=""), sheetName=paste("var_map_", part, sep=""))


f1 <- read.xlsx(paste(src, std1, "/STUDY_04092_Update_CRF_06_OP.xlsx", sep=""), sheetName="CONTENTS")
f3 <- read.xlsx(paste(src, std2, "/STUDY_10003_CRF_09_OP.xlsx", sep=""), sheetName="CONTENTS")

f1 <- read.xlsx(paste(src, std1, "/STUDY_04092_Update_CRF_05_KO.xlsx", sep=""), sheetName="CONTENTS")
f3 <- read.xlsx(paste(src, std2, "/STUDY_10003_CRF_03_KO.xlsx", sep=""), sheetName="CONTENTS")

f3 <- read.xlsx(paste(src, std2, "/STUDY_10003_CRF_07_KP.xlsx", sep=""), sheetName="CONTENTS")

f3 <- read.xlsx(paste(src, std2, "/STUDY_10003_CRF_06_KB.xlsx", sep=""), sheetName="CONTENTS")

f3 <- read.xlsx(paste(src, std2, "/STUDY_10003_CRF_04_EQ.xlsx", sep=""), sheetName="CONTENTS")

f3 <- read.xlsx(paste(src, std2, "/STUDY_10003_CRF_05_OK.xlsx", sep=""), sheetName="CONTENTS")

f1 <- read.xlsx(paste(src, std1, "/STUDY_04092_Update_CRF_02_KS.xlsx", sep=""), sheetName="CONTENTS")
f3 <- read.xlsx(paste(src, std2, "/STUDY_10003_CRF_08_KS.xlsx", sep=""), sheetName="CONTENTS")

f1 <- read.xlsx(paste(src, std1, "/STUDY_04092_Update_CRF_01_MH.xlsx", sep=""), sheetName="CONTENTS")
f3 <- read.xlsx(paste(src, std2, "/STUDY_10003_CRF_01_DE.xlsx", sep=""), sheetName="CONTENTS")


f1_full = as.character(f1$NAME)
f1_mapped = as.character(f$X04092[!is.na(f$X04092)])
f1_unmapped = data.frame(name=setdiff(f1_full, f1_mapped))
write.csv(f1_unmapped, paste(src, "unmapped1.csv", sep=""), row.names=FALSE)

f3_full = as.character(f3$NAME)
f3_mapped = as.character(f$X10003[!is.na(f$X10003)])
f3_unmapped = data.frame(name=setdiff(f3_full, f3_mapped))
write.csv(f3_unmapped, paste(src, "unmapped3.csv", sep=""), row.names=FALSE)



#read in feature list and related data
part="SH"
f <- read.xlsx(paste(src,"data_map_0405Y17.xlsx", sep=""), sheetName=paste("value_tweak_", part, sep=""))
fdata = f[!is.na(f$feature_name),c("feature_name","X10004","X10003","X04092")]

#read in data

#studyID 10004
f4n = as.character(fdata$X10004)
#f4 <- read.xlsx(paste(src, std3, "/STUDY_10004_CRF_01_SH.xlsx", sep=""), sheetName="SH")[,f4n]
fv4 = read.csv(paste(src, std3, "/STUDY_10004_CRF_01_SH.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]

#nrow(fv4[duplicated(fv4), ])


#tweak the values
fv4$ETHNIC = toupper(fv4$ETHNIC)
#fv4$ETHNIC = ifelse(fv4$ETHNIC=="Hispanic or Latino",1,ifelse(fv4$ETHNIC=="Not Hispanic or Latino",0,NA))

#change Yes/No to 1/0
fw_list = f4n[c(9:14)]
b = data.frame(sapply(fv4[,fw_list], function(x) ifelse(is.na(x), NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv4[,fw_list]= b[,fw_list]

fw_list = f4n[c(16:66)]
b = data.frame(sapply(fv4[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv4[,fw_list]= b[,fw_list]

#fv4$ANAT_ANGLE=ifelse(fv4$ANAT_ANGLE=="",NA,as.character(fv4$ANAT_ANGLE))

names(fv4)= as.character(fdata$feature_name)
fv4$STUDY_ID="10004"




#studyID 10008
fv8 = read.csv(paste(src, std4, "/STUDY_10008_CRF_01_SH.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]

fv8$ETHNIC = toupper(fv8$ETHNIC)
#fv8$ETHNIC = ifelse(fv8$ETHNIC=="Hispanic or Latino",1,ifelse(fv8$ETHNIC=="Not Hispanic or Latino",0,NA))

fw_list = f4n[c(9:14)]
b = data.frame(sapply(fv8[,fw_list], function(x) ifelse(is.na(x), NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv8[,fw_list]= b[,fw_list]

fw_list = f4n[c(16:66)]
b = data.frame(sapply(fv8[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv8[,fw_list]= b[,fw_list]


names(fv8)= as.character(fdata$feature_name)


fcom = intersect(names(fv4),names(fv8))
fv48 = merge(x=fv4, y=fv8, by=fcom, all=T)

fv48[is.na(fv48$STUDY_ID),]$STUDY_ID = "10008" 

#library(plyr)
#count(pv48$STUDY_ID)
#studyID 10003
f3n = as.character(fdata$X10003[!is.na(fdata$X10003)])
fv3 = read.csv(paste(src, std2, "/STUDY_10003_CRF_01_DE.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f3n]

#fv3$ETHNICITY = ifelse(fv3$ETHNICITY=="HISPANIC OR LATINO",1,ifelse(fv3$ETHNICITY=="NOT HISPANIC OR LATINO",0,NA))
#convert the KG to LBS
fv3$WEIGHT=2.20462*fv3$WEIGHT

fw_list = f3n[c(9:14)]
b = data.frame(sapply(fv3[,fw_list], function(x) ifelse(is.na(x), NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv3[,fw_list]= b[,fw_list]

fw_list = f3n[c(15:62)]
b = data.frame(sapply(fv3[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv3[,fw_list]= b[,fw_list]
names(fv3)=as.character(fdata$feature_name[!is.na(fdata$X10003)])


fcom = intersect(names(fv48),names(fv3))
fv348 = merge(x=fv48, y=fv3, by=fcom, all=T)
fv348[is.na(fv348$STUDY_ID),]$STUDY_ID = "10003" 

#studyID 04092
f1n = as.character(fdata$X04092[!is.na(fdata$X04092)])
fv1 = read.csv(paste(src, std1, "/STUDY_04092_Update_CRF_01_MH.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f1n]

fv1$ETHNIC = ifelse(fv1$ETHNIC=="HISPANIC","HISPANIC OR LATINO",ifelse(fv1$ETHNIC=="NOT HISPANIC","NOT HISPANIC OR LATINO",""))


fw_list = f1n[c(9:13)]
b = data.frame(sapply(fv1[,fw_list], function(x) ifelse(is.na(x), NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv1[,fw_list]= b[,fw_list]

fw_list = f1n[c(15:41)]
b = data.frame(sapply(fv1[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv1[,fw_list]= b[,fw_list]
names(fv1)=as.character(fdata$feature_name[!is.na(fdata$X04092)])

fcom = intersect(names(fv348),names(fv1))
fv1348 = merge(x=fv348, y=fv1, by=fcom, all=T)
fv1348[is.na(fv1348$STUDY_ID),]$STUDY_ID = "04092" 

write.csv(fv1348, paste(src, "SH.csv", sep=""), row.names=FALSE)

######################################################

#read in feature list and related data
part="OK"
f <- read.xlsx(paste(src,"data_map_0405Y17.xlsx", sep=""), sheetName=paste("value_tweak_", part, sep=""))
fdata = f[!is.na(f$feature_name),c("feature_name","X10004","X10003","X04092")]

#read in data

#studyID 10004
f4n = as.character(fdata$X10004)
fv4 = read.csv(paste(src, std3, "/STUDY_10004_CRF_04_OKS.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv4= fv4[!duplicated(fv4), ]

#nrow(fv4[duplicated(fv4), c("SUBJ","OPSIDE")])
#fv4[duplicated(fv4), c("SUBJ","OPSIDE")]
                       
#tweak the values
#change empty and dot to NA, convert character to numeric
fw_list = f4n[c(4:15)]
b = data.frame(sapply(fv4[,fw_list], function(x) ifelse(x=="" | x==".", NA, suppressWarnings(as.numeric(x)))))
fv4[,fw_list]= b[,fw_list]
names(fv4)= as.character(fdata$feature_name)
fv4$STUDY_ID="10004"

#studyID 10008
fv8 = read.csv(paste(src, std4, "/STUDY_10008_CRF_04_OKS.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv8= fv8[!duplicated(fv8), ]

#fv8[duplicated(fv8), c("SUBJ","OPSIDE")]


b = data.frame(sapply(fv8[,fw_list], function(x) ifelse(x=="" | x==".", NA, suppressWarnings(as.numeric(x)))))
fv8[,fw_list]= b[,fw_list]
names(fv8)= as.character(fdata$feature_name)

fcom = intersect(names(fv4),names(fv8))
fv48 = merge(x=fv4, y=fv8, by=fcom, all=T)

fv48[is.na(fv48$STUDY_ID),]$STUDY_ID = "10008" 


#studyID 10003
f3n = as.character(fdata$X10003[!is.na(fdata$X10003)])
fv3 = read.csv(paste(src, std2, "/STUDY_10003_CRF_05_OK.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f3n]
fv3= fv3[!duplicated(fv3), ]

b = data.frame(sapply(fv3[,fw_list], function(x) ifelse(x=="" | x==".", NA, suppressWarnings(as.numeric(x)))))
fv3[,fw_list]= b[,fw_list]
names(fv3)=as.character(fdata$feature_name[!is.na(fdata$X10003)])


fcom = intersect(names(fv48),names(fv3))
fv348 = merge(x=fv48, y=fv3, by=fcom, all=T)
fv348[is.na(fv348$STUDY_ID),]$STUDY_ID = "10003" 

write.csv(fv348, paste(src, "OK.csv", sep=""), row.names=FALSE)

######################################################
#read in feature list and related data
part="EQ"
f <- read.xlsx(paste(src,"data_map_0405Y17.xlsx", sep=""), sheetName=paste("value_tweak_", part, sep=""))
fdata = f[!is.na(f$feature_name),c("feature_name","X10004","X10003","X04092")]


#read in data

#studyID 10004
f4n = as.character(fdata$X10004)
fv4 = read.csv(paste(src, std3, "/STUDY_10004_CRF_03_EQ.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv4= fv4[!duplicated(fv4), ]
fw_list = f4n[c(4:8)]
b = data.frame(sapply(fv4[,fw_list], function(x) as.numeric(gsub("[^0-9]", "", x))))
fv4[,fw_list]= b[,fw_list]
names(fv4)= as.character(fdata$feature_name)
fv4$STUDY_ID="10004"

#studyID 10008
fv8 = read.csv(paste(src, std4, "/STUDY_10008_CRF_03_EQ.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv8= fv8[!duplicated(fv8), ]

b = data.frame(sapply(fv8[,fw_list], function(x) as.numeric(gsub("[^0-9]", "", x))))
fv8[,fw_list]= b[,fw_list]
names(fv8)= as.character(fdata$feature_name)

fcom = intersect(names(fv4),names(fv8))
fv48 = merge(x=fv4, y=fv8, by=fcom, all=T)

fv48[is.na(fv48$STUDY_ID),]$STUDY_ID = "10008" 

#studyID 10003
f3n = as.character(fdata$X10003[!is.na(fdata$X10003)])
fv3 = read.csv(paste(src, std2, "/STUDY_10003_CRF_04_EQ.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f3n]
fv3= fv3[!duplicated(fv3), ]

#b = data.frame(sapply(fv3[,fw_list], function(x) as.numeric(gsub("[^0-9]", "", x))))
#fv3[,fw_list]= b[,fw_list]
names(fv3)=as.character(fdata$feature_name[!is.na(fdata$X10003)])


fcom = intersect(names(fv48),names(fv3))
fv348 = merge(x=fv48, y=fv3, by=fcom, all=T)
fv348[is.na(fv348$STUDY_ID),]$STUDY_ID = "10003" 

write.csv(fv348, paste(src, "EQ.csv", sep=""), row.names=FALSE)

######################################################
#read in feature list and related data
part="KB"
f <- read.xlsx(paste(src,"data_map_0405Y17.xlsx", sep=""), sheetName=paste("value_tweak_", part, sep=""))
fdata = f[!is.na(f$feature_name),c("feature_name","X10004","X10003","X04092")]

#read in data

#studyID 10004
f4n = as.character(fdata$X10004)
fv4 = read.csv(paste(src, std3, "/STUDY_10004_CRF_05_KB.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv4= fv4[!duplicated(fv4), ]

r1 <- c("NEVER","RARELY","SOMETIMES","OFTEN","ALWAYS")
r2 <- c("NOT AT ALL","SLIGHTLY","MODERATELY","VERY","COMPLETELY")
r3 <- c("VERY DISSATISFIED","DISSATISFIED","A LITTLE DISSATISFIED","A LITTLE SATISFIED","SATISFIED","VERY SATISFIED")

fw_list = c("PKIP_AWARE","PKIP_CONF_ACT","PKIP_STAB_ACT")
b = data.frame(sapply(fv4[,fw_list], function(x) match(toupper(x),r1)-1))
fv4[,fw_list]= b[,fw_list]

fw_list = c("PKIP_DEG_ACT")
b = data.frame(sapply(fv4[,fw_list,drop=F], function(x) match(toupper(x),r2)-1))
fv4[,fw_list]= b[,fw_list]

fw_list = c("PKIP_EVD_ACT","PKIP_SAT_FUNC")
b = data.frame(sapply(fv4[,fw_list], function(x) match(toupper(x),r3)-1))
fv4[,fw_list]= b[,fw_list]

names(fv4)= as.character(fdata$feature_name)
fv4$STUDY_ID="10004"

fv8 = read.csv(paste(src, std4, "/STUDY_10008_CRF_05_KB.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv8= fv8[!duplicated(fv8), ]

fw_list = c("PKIP_AWARE","PKIP_CONF_ACT","PKIP_STAB_ACT")
b = data.frame(sapply(fv8[,fw_list], function(x) match(toupper(x),r1)-1))
fv8[,fw_list]= b[,fw_list]

fw_list = c("PKIP_DEG_ACT")
b = data.frame(sapply(fv8[,fw_list,drop=F], function(x) match(toupper(x),r2)-1))
fv8[,fw_list]= b[,fw_list]

fw_list = c("PKIP_EVD_ACT","PKIP_SAT_FUNC")
b = data.frame(sapply(fv8[,fw_list], function(x) match(toupper(x),r3)-1))
fv8[,fw_list]= b[,fw_list]

names(fv8)= as.character(fdata$feature_name)

fcom = intersect(names(fv4),names(fv8))
fv48 = merge(x=fv4, y=fv8, by=fcom, all=T)

fv48[is.na(fv48$STUDY_ID),]$STUDY_ID = "10008" 

#studyID 10003
f3n = as.character(fdata$X10003[!is.na(fdata$X10003)])
fv3 = read.csv(paste(src, std2, "/STUDY_10003_CRF_06_KB.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f3n]
fv3= fv3[!duplicated(fv3), ]

fw_list = c("KBP_AWARE","KBP_CONF_ACT","KBP_STAB_ACT")
b = data.frame(sapply(fv3[,fw_list], function(x) match(toupper(x),r1)-1))
fv3[,fw_list]= b[,fw_list]

fw_list = c("KBP_DEG_ACT")
b = data.frame(sapply(fv3[,fw_list,drop=F], function(x) match(toupper(x),r2)-1))
fv3[,fw_list]= b[,fw_list]

fw_list = c("KBP_EVD_ACT","KBP_SAT_FUNC")
b = data.frame(sapply(fv3[,fw_list], function(x) match(toupper(x),r3)-1))
fv3[,fw_list]= b[,fw_list]

names(fv3)=as.character(fdata$feature_name[!is.na(fdata$X10003)])

fcom = intersect(names(fv48),names(fv3))
fv348 = merge(x=fv48, y=fv3, by=fcom, all=T)
fv348[is.na(fv348$STUDY_ID),]$STUDY_ID = "10003" 

write.csv(fv348, paste(src, "KB.csv", sep=""), row.names=FALSE)

######################################################
#read in feature list and related data
part="KP"
f <- read.xlsx(paste(src,"data_map_0405Y17.xlsx", sep=""), sheetName=paste("value_tweak_", part, sep=""))
fdata = f[!is.na(f$feature_name),c("feature_name","X10004","X10003","X04092")]

#read in data

#studyID 10004
f4n = as.character(fdata$X10004)
fv4 = read.csv(paste(src, std3, "/STUDY_10004_CRF_06_KP.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv4= fv4[!duplicated(fv4), ]

r1 <- c("NEVER","RARELY","SOMETIMES","OFTEN","ALWAYS")

fw_list = f4n[4:6]
b = data.frame(sapply(fv4[,fw_list], function(x) match(toupper(x),r1)-1))
fv4[,fw_list]= b[,fw_list]

fw_list = f4n[7:11]
b = data.frame(sapply(fv4[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv4[,fw_list]= b[,fw_list]

names(fv4)= as.character(fdata$feature_name)
fv4$STUDY_ID="10004"

#studyID 10008
fv8 = read.csv(paste(src, std4, "/STUDY_10008_CRF_06_KP.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv8= fv8[!duplicated(fv8), ]

fw_list = f4n[4:6]
b = data.frame(lapply(fv8[,fw_list], function(x) match(toupper(x),r1)-1))
fv8[,fw_list]= b[,fw_list]

fw_list = f4n[7:11]
b = data.frame(lapply(fv8[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv8[,fw_list]= b[,fw_list]

names(fv8)= as.character(fdata$feature_name)


fcom = intersect(names(fv4),names(fv8))
fv48 = merge(x=fv4, y=fv8, by=fcom, all=T)

fv48[is.na(fv48$STUDY_ID),]$STUDY_ID = "10008" 

#studyID 10003
f3n = as.character(fdata$X10003[!is.na(fdata$X10003)])
fv3 = read.csv(paste(src, std2, "/STUDY_10003_CRF_07_KP.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f3n]
fv3= fv3[!duplicated(fv3), ]

fw_list = f3n[4:6]
b = data.frame(sapply(fv3[,fw_list], function(x) match(toupper(x),r1)-1))
fv3[,fw_list]= b[,fw_list]


#fw_list = f3n[7:11]
#b = data.frame(lapply(fv3[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="YES" | x=="Y",1,0))))
#fv3[,fw_list]= b[,fw_list]

names(fv3)=as.character(fdata$feature_name[!is.na(fdata$X10003)])

fcom = intersect(names(fv48),names(fv3))
fv348 = merge(x=fv48, y=fv3, by=fcom, all=T)
fv348[is.na(fv348$STUDY_ID),]$STUDY_ID = "10003" 

write.csv(fv348, paste(src, "KP.csv", sep=""), row.names=FALSE)

######################################################
#read in feature list and related data
part="KO"
f <- read.xlsx(paste(src,"data_map_0405Y17.xlsx", sep=""), sheetName=paste("value_tweak_", part, sep=""))
fdata = f[!is.na(f$feature_name),c("feature_name","X10004","X10003","X04092")]

#read in data

#studyID 10004
f4n = as.character(fdata$X10004)
fv4 = read.csv(paste(src, std3, "/STUDY_10004_CRF_02_KO.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv4= fv4[!duplicated(fv4), ]

#r1 <- c("NEVER","RARELY","SOMETIMES","OFTEN","ALWAYS")
#r4 <- c("NONE","MILD","MODERATE","SEVERE","EXTREME")

fw_list = f4n[c(4:8, 10:20,22:38,40:44,46:49)]
b = data.frame(sapply(fv4[,fw_list], function(x) as.numeric(gsub("[^0-9]", "", x))))
fv4[,fw_list]= b[,fw_list]
names(fv4)= as.character(fdata$feature_name)
fv4$STUDY_ID="10004"

#studyID 10008
fv8 = read.csv(paste(src, std4, "/STUDY_10008_CRF_02_KO.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv8= fv8[!duplicated(fv8), ]

b = data.frame(sapply(fv8[,fw_list], function(x) as.numeric(gsub("[^0-9]", "", x))))
fv8[,fw_list]= b[,fw_list]
names(fv8)= as.character(fdata$feature_name)

fcom = intersect(names(fv4),names(fv8))
fv48 = merge(x=fv4, y=fv8, by=fcom, all=T)

fv48[is.na(fv48$STUDY_ID),]$STUDY_ID = "10008" 

#studyID 10003
f3n = as.character(fdata$X10003[!is.na(fdata$X10003)])
fv3 = read.csv(paste(src, std2, "/STUDY_10003_CRF_03_KO.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f3n]
fv3= fv3[!duplicated(fv3), ]

fw_list = f3n[c(4:8, 10:20,22:38,40:44,46:49)]
b = data.frame(sapply(fv3[,fw_list], function(x) as.numeric(x)))
fv3[,fw_list]= b[,fw_list]
names(fv3)=as.character(fdata$feature_name[!is.na(fdata$X10003)])


fcom = intersect(names(fv48),names(fv3))
fv348 = merge(x=fv48, y=fv3, by=fcom, all=T)
fv348[is.na(fv348$STUDY_ID),]$STUDY_ID = "10003" 

#studyID 04092
f1n = as.character(fdata$X04092[!is.na(fdata$X04092)])
fv1 = read.csv(paste(src, std1, "/STUDY_04092_Update_CRF_05_KO.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f1n]
fv1= fv1[!duplicated(fv1), ]

fw_list = f1n[c(4:8, 10:20,22:38,40:44,46:49)]
b = data.frame(sapply(fv1[,fw_list], function(x) as.numeric(gsub("[^0-9]", "", x))))
fv1[,fw_list]= b[,fw_list]
names(fv1)=as.character(fdata$feature_name[!is.na(fdata$X04092)])

fcom = intersect(names(fv348),names(fv1))
fv1348 = merge(x=fv348, y=fv1, by=fcom, all=T)
fv1348[is.na(fv1348$STUDY_ID),]$STUDY_ID = "04092" 

write.csv(fv1348, paste(src, "KO.csv", sep=""), row.names=FALSE)

######################################################
#read in feature list and related data
part="KS"
f <- read.xlsx(paste(src,"data_map_0405Y17.xlsx", sep=""), sheetName=paste("value_tweak_", part, sep=""))
fdata = f[!is.na(f$feature_name),c("feature_name","X10004","X10003","X04092")]

#read in data

#studyID 10004
f4n = as.character(fdata$X10004)
fv4 = read.csv(paste(src, std3, "/STUDY_10004_CRF_07_KS.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv4= fv4[!duplicated(fv4), ]
fv8 = read.csv(paste(src, std4, "/STUDY_10008_CRF_07_KS.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv8= fv8[!duplicated(fv8), ]


comments <- function(x) {

t1 <- c("NEVER","SOMETIMES","ALWAYS")
fw_list = f4n[c(6)]
b = data.frame(sapply(fv4[,fw_list,drop=F], function(x) match(toupper(x),t1)-1))
fv4[,fw_list]= b[,fw_list]

b = data.frame(sapply(fv8[,fw_list,drop=F], function(x) match(toupper(x),t1)-1))
fv8[,fw_list]= b[,fw_list]

                
t2 <- c("VERY DISSATISFIED","DISSATISFIED","NEUTRAL","SATISFIED","VERY SATISFIED")
fw_list = f4n[c(7:11)]
b = data.frame(sapply(fv4[,fw_list,drop=F], function(x) match(toupper(x),t2)-1))
fv4[,fw_list]= b[,fw_list]

b = data.frame(sapply(fv8[,fw_list,drop=F], function(x) match(toupper(x),t2)-1))
fv8[,fw_list]= b[,fw_list]

t3 <- c("TOO HIGH - \"I'M A LOT WORSE THAN I THOUGHT\"","TOO HIGH - \"I'M SOMEWHAT WORSE THAN I THOUGHT\"",
        "JUST RIGHT - \"MY EXPECTATIONS WERE MET\"",
        "TOO LOW - \"I'M A SOMEWHAT BETTER THAN I THOUGHT\"","TOO LOW - \"I'M A LOT BETTER THAN I THOUGHT\"")

fw_list = f4n[c(12:14)]
b = data.frame(sapply(fv4[,fw_list,drop=F], function(x) match(toupper(x),t3)-1))
fv4[,fw_list]= b[,fw_list]

b = data.frame(sapply(fv8[,fw_list,drop=F], function(x) match(toupper(x),t3)-1))
fv8[,fw_list]= b[,fw_list]


t4 <- c("NO, NOT AT ALL","YES, A LITTLE BIT","YES, SOMEWHAT","YES, A MODERATE AMOUNT","YES, A LOT")
fw_list = f4n[c(15:17)]
b = data.frame(sapply(fv4[,fw_list,drop=F], function(x) match(toupper(x),t4)-1))
fv4[,fw_list]= b[,fw_list]

b = data.frame(sapply(fv8[,fw_list,drop=F], function(x) match(toupper(x),t4)-1))
fv8[,fw_list]= b[,fw_list]

#Yes, No -> 1/0
fw_list = f4n[c(18,20)]
b = data.frame(sapply(fv4[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv4[,fw_list]= b[,fw_list]

b = data.frame(sapply(fv8[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv8[,fw_list]= b[,fw_list]


t5 <- c("CANNOT STAND","0-5 MINUTES","6-15 MINUTES","16-30 MINUTES","31-60 MINUTES","MORE THAN AN HOUR")
fw_list = f4n[c(21)]
b = data.frame(sapply(fv4[,fw_list,drop=F], function(x) match(toupper(x),t5)-1))
fv4[,fw_list]= b[,fw_list]

b = data.frame(sapply(fv8[,fw_list,drop=F], function(x) match(toupper(x),t5)-1))
fv8[,fw_list]= b[,fw_list]


t6 <- c("CANNOT WALK","0-5 MINUTES","6-15 MINUTES","16-30 MINUTES","31-60 MINUTES","MORE THAN AN HOUR")
fw_list = f4n[c(22)]
b = data.frame(sapply(fv4[,fw_list,drop=F], function(x) match(toupper(x),t6)-1))
fv4[,fw_list]= b[,fw_list]

b = data.frame(sapply(fv8[,fw_list,drop=F], function(x) match(toupper(x),t6)-1))
fv8[,fw_list]= b[,fw_list]

t7 <- c("I NEVER DO THIS","CANNOT DO (BECAUSE OF KNEE)","VERY SEVERE","SEVERE","MODERATE","SLIGHT","NO BOTHER")
fw_list = f4n[c(23:36)]
b = data.frame(sapply(fv4[,fw_list], function(x) match(toupper(x),t7)-1))
fv4[,fw_list]= b[,fw_list]

b = data.frame(sapply(fv8[,fw_list], function(x) match(toupper(x),t7)-1))
fv8[,fw_list]= b[,fw_list]

}

names(fv4)= as.character(fdata$feature_name)
fv4$STUDY_ID="10004"
names(fv8)= as.character(fdata$feature_name)

fcom = intersect(names(fv4),names(fv8))
fv48 = merge(x=fv4, y=fv8, by=fcom, all=T)

fv48[is.na(fv48$STUDY_ID),]$STUDY_ID = "10008" 


#studyID 10003
f3n = as.character(fdata$X10003[!is.na(fdata$X10003)])
fv3 = read.csv(paste(src, std2, "/STUDY_10003_CRF_08_KS.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f3n]
fv3= fv3[!duplicated(fv3), ]

names(fv3)=as.character(fdata$feature_name[!is.na(fdata$X10003)])


fcom = intersect(names(fv48),names(fv3))
fv348 = merge(x=fv48, y=fv3, by=fcom, all=T)
fv348[is.na(fv348$STUDY_ID),]$STUDY_ID = "10003" 

#studyID 04092
f1n = as.character(fdata$X04092[!is.na(fdata$X04092)])
fv1 = read.csv(paste(src, std1, "/STUDY_04092_Update_CRF_02_KS.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f1n]
fv1= fv1[!duplicated(fv1), ]

names(fv1)=as.character(fdata$feature_name[!is.na(fdata$X04092)])

fcom = intersect(names(fv348),names(fv1))
fv1348 = merge(x=fv348, y=fv1, by=fcom, all=T)
fv1348[is.na(fv1348$STUDY_ID),]$STUDY_ID = "04092" 

write.csv(fv1348, paste(src, "KS.csv", sep=""), row.names=FALSE)

######################################################
#read in feature list and related data
part="OP"
f <- read.xlsx(paste(src,"data_map_0405Y17.xlsx", sep=""), sheetName=paste("value_tweak_", part, sep=""))
fdata = f[!is.na(f$feature_name),c("feature_name","X10004","X10003","X04092")]

#read in data

#studyID 10004
f4n = as.character(fdata$X10004)
fv4 = read.csv(paste(src, std3, "/STUDY_10004_CRF_08_OP.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv4= fv4[!duplicated(fv4), ]
fv8 = read.csv(paste(src, std4, "/STUDY_10008_CRF_08_OP.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv8= fv8[!duplicated(fv8), ]

compute_duration = function(i){
  
  t = ifelse(df$duration[i]>0, df$duration[i], ifelse(df$starttime[i]!="" & df$endtime[i]!="", 
      (as.numeric(substr(df$endtime[i], 1, regexpr(":", df$endtime[i])-1))-
       as.numeric(substr(df$starttime[i], 1, regexpr(":", df$starttime[i])-1)))*60+
      as.numeric(substr(df$endtime[i], regexpr(":", df$endtime[i])+1, nchar(df$endtime[i])))-
      as.numeric(substr(df$starttime[i], regexpr(":", df$starttime[i])+1, nchar(df$starttime[i]))),NA))
  return (t);
}

df <- fv4[,c("SGSTTIM","SGENTIM","PROC_DUR_MIN")]
names(df)=c("starttime","endtime","duration")
#df[1:5,"duration"]=0
b = sapply(1:nrow(df), function(x) compute_duration(x))
fv4$PROC_DUR_MIN = b

df <- fv4[,c("TOURNI_ON_TIME","TOURNI_OFF_TIME","TOURNI_DUR_MIN")]
names(df)=c("starttime","endtime","duration")
b = sapply(1:nrow(df), function(x) compute_duration(x))
fv4$TOURNI_DUR_MIN = b


df <- fv8[,c("SGSTTIM","SGENTIM","PROC_DUR_MIN")]
names(df)=c("starttime","endtime","duration")
b = sapply(1:nrow(df), function(x) compute_duration(x))
fv8$PROC_DUR_MIN = b

df <- fv8[,c("TOURNI_ON_TIME","TOURNI_OFF_TIME","TOURNI_DUR_MIN")]
names(df)=c("starttime","endtime","duration")
b = sapply(1:nrow(df), function(x) compute_duration(x))
fv8$TOURNI_DUR_MIN = b

fw_list = f4n[c(3, 11:16, 20, 22, 24:26,28, 31)]
b = data.frame(sapply(fv4[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv4[,fw_list]= b[,fw_list]
b = data.frame(sapply(fv8[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv8[,fw_list]= b[,fw_list]

#capitalize 
fw_list = c("LIG_BAL_TECH")
b = toupper(fv4[,fw_list])
fv4[,fw_list]= b
b = toupper(fv8[,fw_list])
fv8[,fw_list]= b

names(fv4)= as.character(fdata$feature_name)
fv4 = fv4[,-c(5,6,8,9)]
fv4$STUDY_ID="10004"

names(fv8)= as.character(fdata$feature_name)
fv8 = fv8[,-c(5,6,8,9)]

fcom = intersect(names(fv4),names(fv8))
fv48 = merge(x=fv4, y=fv8, by=fcom, all=T)

fv48[is.na(fv48$STUDY_ID),]$STUDY_ID = "10008" 

#studyID 10003
f3n = as.character(fdata$X10003[!is.na(fdata$X10003)])
fv3 = read.csv(paste(src, std2, "/STUDY_10003_CRF_09_OP.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f3n]
fv3= fv3[!duplicated(fv3), ]

df <- fv3[,c("SRG_START_TIME","SRG_END_TIME","PROC_DUR_MIN")]
names(df)=c("starttime","endtime","duration")
b = sapply(1:nrow(df), function(x) compute_duration(x))
fv3$PROC_DUR_MIN = b


df <- fv3[,c("TOURNI_ON_TIME","TOURNI_OFF_TIME","TOURNI_MIN")]
names(df)=c("starttime","endtime","duration")
b = sapply(1:nrow(df), function(x) compute_duration(x))
fv3$TOURNI_MIN = b

#tweak the values
fv3$FEM_CEMENT = ifelse(fv3$FEM_CEMENT=="CEMENTED",1,ifelse(fv3$FEM_CEMENT=="NON CEMENTED",0,NA))

fw_list = f3n[c(3, 12:26)]
b = data.frame(sapply(fv3[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv3[,fw_list]= b[,fw_list]

names(fv3)=as.character(fdata$feature_name[!is.na(fdata$X10003)])
fv3 = fv3[,-c(4,5,7,8)]

fcom = intersect(names(fv48),names(fv3))
fv348 = merge(x=fv48, y=fv3, by=fcom, all=T)
fv348[is.na(fv348$STUDY_ID),]$STUDY_ID = "10003" 

#studyID 04092
f1n = as.character(fdata$X04092[!is.na(fdata$X04092)])
fv1 = read.csv(paste(src, std1, "/STUDY_04092_Update_CRF_06_OP.csv", sep=""), 
               colClasses=c(SRGSTM="character", SRGETM="character", ONTIME="character", OFFTIM="character"), header=TRUE, stringsAsFactors = FALSE)[,f1n]
fv1= fv1[!duplicated(fv1), ]

compute_duration2 = function(i){
  
  t = ifelse(!is.na(df$duration[i]) & df$duration[i]>0, df$duration[i], ifelse(df$starttime[i]!="" & df$endtime[i]!="", 
      (as.numeric(substr(df$endtime[i], 1, 2))-as.numeric(substr(df$starttime[i], 1, 2)))*60+
        as.numeric(substr(df$endtime[i], 3, 4))-as.numeric(substr(df$starttime[i], 3, 4)),NA))
  return (t);
}

fv1$PROC_DUR_MIN=0
df <- fv1[,c("SRGSTM","SRGETM","PROC_DUR_MIN")]
names(df)=c("starttime","endtime","duration")
b = sapply(1:nrow(df), function(x) compute_duration2(x))
fv1$PROC_DUR_MIN = b


df <- fv1[,c("ONTIME","OFFTIM","TOURMN")]
names(df)=c("starttime","endtime","duration")
b = sapply(1:nrow(df), function(x) compute_duration2(x))
fv1$TOURMN = b

fw_list = f1n[c(3)]
b = data.frame(sapply(fv1[,fw_list,drop=F], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv1[,fw_list]= b[,fw_list]

names(fv1)=c(as.character(fdata$feature_name[!is.na(fdata$X04092)]), "PROC_DUR_MIN")
fv1 = fv1[,-c(4,5,6,7)]

fcom = intersect(names(fv348),names(fv1))
fv1348 = merge(x=fv348, y=fv1, by=fcom, all=T)
fv1348[is.na(fv1348$STUDY_ID),]$STUDY_ID = "04092" 

write.csv(fv1348, paste(src, "OP.csv", sep=""), row.names=FALSE)

######################################################
#read in feature list and related data
part="AE"
f <- read.xlsx(paste(src,"data_map_0405Y17.xlsx", sep=""), sheetName=paste("value_tweak_", part, sep=""))
fdata = f[!is.na(f$feature_name),c("feature_name","X10004","X10003","X04092")]

#read in data

#studyID 10004
f4n = as.character(fdata$X10004)
fv4 = read.csv(paste(src, std3, "/STUDY_10004_CRF_09_AE.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv4= fv4[!duplicated(fv4), ]
fv8 = read.csv(paste(src, std4, "/STUDY_10008_CRF_09_AE.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f4n]
fv8= fv8[!duplicated(fv8), ]

fw_list = f4n[c(4:12, 17:24,26,28:37,40)]
b = data.frame(sapply(fv4[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv4[,fw_list]= b[,fw_list]

b = data.frame(sapply(fv8[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="Y",1,0))))
fv8[,fw_list]= b[,fw_list]

t1 <- c("MILD","MODERATE","SEVERE")
fw_list = f4n[c(14)]
b = data.frame(sapply(fv4[,fw_list,drop=F], function(x) match(toupper(x),t1)-1))
fv4[,fw_list]= b[,fw_list]
b = data.frame(sapply(fv8[,fw_list,drop=F], function(x) match(toupper(x),t1)-1))
fv8[,fw_list]= b[,fw_list]

t2 <- list(y="NOT RELATED",o=c("POSSIBLY RELATED","REMOTE POSSIBILITY"),u="DEFINITELY RELATED")

b = unlist(lapply(toupper(fv4$AEDVRLL), grep, t2))
fv4$AEDVRLL=b
b = unlist(lapply(toupper(fv4$AEPDRLL), grep, t2))
fv4$AEPDRLL=b

b = unlist(lapply(toupper(fv8$AEDVRLL), grep, t2))
fv8$AEDVRLL=b
b = unlist(lapply(toupper(fv8$AEPDRLL), grep, t2))
fv8$AEPDRLL=b

t4 <- c("YES","NO","CONTINUING","AWAITING RESOLUTION","UNABLE TO RESOLVE","RESOLVED","FATAL","NOT RECOVERED/NOT RESOLVED","RECOVERED/RESOLVED","RECOVERING/RESOLVING","RESOLVED WITH RESIDUAL EFFECTS","UNKNOWN","")
t5 <- c("RESOLVED","NOT RESOLVED","AWAITING RESOLUTION","AWAITING RESOLUTION","NOT RESOLVED","RESOLVED","FATAL","NOT RESOLVED","RESOLVED","RESOLVING","RESOLVED WITH RESIDUAL",NA,NA)

hash_list = function(x){
  
  return (t5[match(toupper(x),t4)])
  
}


fw_list = f4n[c(38)]
b = data.frame(sapply(fv4[,fw_list,drop=F], function(x) hash_list(x)))
fv4[,fw_list]= b[,fw_list]
b = data.frame(sapply(fv8[,fw_list,drop=F], function(x) hash_list(x)))
fv8[,fw_list]= b[,fw_list]


names(fv4)= as.character(fdata$feature_name)
fv4$STUDY_ID="10004"

names(fv8)= as.character(fdata$feature_name)

fcom = intersect(names(fv4),names(fv8))
fv48 = merge(x=fv4, y=fv8, by=fcom, all=T)

fv48[is.na(fv48$STUDY_ID),]$STUDY_ID = "10008" 


#studyID 10003
f3n = as.character(fdata$X10003[!is.na(fdata$X10003)])
fv3 = read.csv(paste(src, std2, "/STUDY_10003_CRF_10_AE.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f3n]
fv3= fv3[!duplicated(fv3), ]

fw_list = f3n[c(4,9:11,13:19)]
b = data.frame(sapply(fv3[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="YES" | x=="Y",1,0))))
fv3[,fw_list]= b[,fw_list]

t1 <- c("MILD","MODERATE","SEVERE")
fw_list = f3n[c(6)]
b = data.frame(sapply(fv3[,fw_list,drop=F], function(x) match(toupper(x),t1)-1))
fv3[,fw_list]= b[,fw_list]

fv3[which(toupper(fv3$AE_DEV_RL)=="DEFINITELY"),]$AE_DEV_RL=1
fv3[which(toupper(fv3$AE_DEV_RL)=="DEFINITELY NOT"),]$AE_DEV_RL=3
fv3[which(fv3$AE_DEV_RL=="" | is.na(fv3$AE_DEV_RL)),]$AE_DEV_RL = NA
fv3[!(fv3$AE_DEV_RL %in% c(1,3,NA)),]$AE_DEV_RL=2
fv3$AE_DEV_RL=as.numeric(fv3$AE_DEV_RL)

fv3[fv3$AE_PROC_RL=="DEFINITELY",]$AE_PROC_RL=1
fv3[fv3$AE_PROC_RL=="DEFINITELY NOT",]$AE_PROC_RL=3
fv3[fv3$AE_PROC_RL=="" | is.na(fv3$AE_PROC_RL),]$AE_PROC_RL = NA
fv3[!(fv3$AE_PROC_RL %in% c(1,3,NA)),]$AE_PROC_RL=2
fv3$AE_PROC_RL=as.numeric(fv3$AE_PROC_RL)

fw_list = f3n[c(20)]
b = data.frame(sapply(fv3[,fw_list,drop=F], function(x) hash_list(x)))
fv3[,fw_list]= b[,fw_list]

names(fv3)=as.character(fdata$feature_name[!is.na(fdata$X10003)])

fcom = intersect(names(fv48),names(fv3))
fv348 = merge(x=fv48, y=fv3, by=fcom, all=T)
fv348[is.na(fv348$STUDY_ID),]$STUDY_ID = "10003" 

#studyID 04092
f1n = as.character(fdata$X04092[!is.na(fdata$X04092)])
fv1 = read.csv(paste(src, std1, "/STUDY_04092_Update_CRF_08_AE.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)[,f1n]
fv1= fv1[!duplicated(fv1), ]

fw_list = f1n[c(8:10,12:19,22)]
b = data.frame(sapply(fv1[,fw_list], function(x) ifelse(is.na(x) | x=="", NA, ifelse(x=="Yes" | x=="YES" | x=="Y",1,0))))
fv1[,fw_list]= b[,fw_list]

t1 <- c("MILD","MODERATE","SERIOUS")
fw_list = f1n[c(5)]
b = data.frame(sapply(fv1[,fw_list,drop=F], function(x) match(toupper(x),t1)-1))
fv1[,fw_list]= b[,fw_list]

t2 <- list(y="DEFINITELY NOT RELATED",o=c("POSSIBLY RELATED","REMOTE POSSIBILITY"),u="DEFINITELY RELATED")

fv1[fv1$AEDVRLL=="" | is.na(fv1$AEDVRLL),]$AEDVRLL = NA
b = unlist(lapply(fv1$AEDVRLL[!is.na(fv1$AEDVRLL)], grep, t2))
fv1$AEDVRLL[!is.na(fv1$AEDVRLL)]=b
fv1$AEDVRLL=as.numeric(fv1$AEDVRLL)

fv1[fv1$AEPDRLL=="" | is.na(fv1$AEPDRLL),]$AEPDRLL = NA
b = unlist(lapply(fv1$AEPDRLL[!is.na(fv1$AEPDRLL)], grep, t2))
fv1$AEPDRLL[!is.na(fv1$AEPDRLL)]=b
fv1$AEPDRLL=as.numeric(fv1$AEPDRLL)

fw_list = f1n[c(20)]
b = data.frame(sapply(fv1[,fw_list,drop=F], function(x) hash_list(x)))
fv1[,fw_list]= b[,fw_list]

names(fv1)=as.character(fdata$feature_name[!is.na(fdata$X04092)])


fcom = intersect(names(fv348),names(fv1))
fv1348 = merge(x=fv348, y=fv1, by=fcom, all=T)
fv1348[is.na(fv1348$STUDY_ID),]$STUDY_ID = "04092" 

write.csv(fv1348, paste(src, "AE.csv", sep=""), row.names=FALSE)

######################################################

#merge the data

ok = read.csv(paste(src, "OK.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
names(ok)[which(names(ok)=="OK_DAYS")]="DAYS"

eq = read.csv(paste(src, "EQ.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
names(eq)[which(names(eq)=="EQ_DAYS")]="DAYS"

ok = ok[,-c(17)]
eq = eq[,-c(18)]
fcom = intersect(names(ok),names(eq))
tmp2 = merge(x=ok, y=eq, by=fcom, all=T)

#nrow(tmp2[duplicated(tmp2), ])
tmp2 = tmp2[!duplicated(tmp2), ]



kb = read.csv(paste(src, "KB.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
names(kb)[which(names(kb)=="KB_DAYS")]="DAYS"
kb = kb[,-c(33)]

fcom = intersect(names(tmp2),names(kb))
tmp3 = merge(x=tmp2, y=kb, by=fcom, all=T)
#nrow(tmp3[duplicated(tmp3), ])

kp = read.csv(paste(src, "KP.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
names(kp)[which(names(kp)=="KP_DAYS")]="DAYS"
kp = kp[,-c(12)]
fcom = intersect(names(tmp3),names(kp))
tmp4 = merge(x=tmp3, y=kp, by=fcom, all=T)
#nrow(tmp4[duplicated(tmp4), ])

ko = read.csv(paste(src, "KO.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
names(ko)[which(names(ko)=="KO_DAYS")]="DAYS"
ko = ko[,-c(78)]
fcom = intersect(names(tmp4),names(ko))
tmp5 = merge(x=tmp4, y=ko, by=fcom, all=T)
#nrow(tmp5[duplicated(tmp5), ])

ks = read.csv(paste(src, "KS.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
names(ks)[which(names(ks)=="KS_DAYS")]="DAYS"
ks = ks[,-c(70)]
fcom = intersect(names(tmp5),names(ks))
tmp6 = merge(x=tmp5, y=ks, by=fcom, all=T)
#nrow(tmp6[duplicated(tmp6), ])



op = read.csv(paste(src, "OP.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
#drop study ID
op = op[,-c(45)]
fcom = intersect(names(tmp6),names(op))

tmp1 = merge(x=tmp6, y=op, by=fcom, all=T)
tmp1 = tmp1[!duplicated(tmp1), ]

sh = read.csv(paste(src, "SH.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
#drop study ID
#sh = sh[,-c(68)]
fcom = intersect(names(tmp1),names(sh))
tmp7 = merge(x=sh, y=tmp1, by=fcom, all=T)
tmp7 = tmp7[!duplicated(tmp7), ]
#length(which(is.na(tmp7$STUDY_ID)==T))

t<- which(is.na(tmp7$STUDY_ID)==T)
t1 <- tmp7[t,]
ks = read.csv(paste(src, "KS.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
names(ks)[which(names(ks)=="KS_DAYS")]="DAYS"
tt <- ks[ks$SUBJ %in% unique(t1$SUBJ),]
head(tt)
tmp7[which(is.na(tmp7$STUDY_ID)==T),]$STUDY_ID="10004"


write.csv(tmp7, paste(src, "SH_OK_EQ_KB_KP_KO_KS_OP.csv", sep=""), row.names=FALSE)


#check the duplications of the (subject, days)

library(plyr)
ck = count(tmp3[,c("SUBJ","DAYS")])
b = ck[ck$freq>1,]
nrow(b)
#merge
c1 = merge(x=b, y=tmp3, by=c("SUBJ","DAYS"))
c1 = c1[order(c1$SUBJ, c1$DAYS),]
c1 = c1[,c(names(kb))]
#PKIP_CONF_KNEEL, PKIP_TOTALSCORE

ck = count(tmp5[,c("SUBJ","DAYS")])
b = ck[ck$freq>1,]
b = subset(b, !(b$SUBJ %in% unique(c1$SUBJ)))
c2= merge(x=b, y=tmp5, by=c("SUBJ","DAYS"))
c2 = c2[,c(names(ko))]

ck = count(tmp6[,c("SUBJ","DAYS")])
b = ck[ck$freq>1,]
b = subset(b, !(b$SUBJ %in% c(unique(c1$SUBJ), unique(c2$SUBJ))))
c3= merge(x=b, y=tmp6, by=c("SUBJ","DAYS"))
c3 = c3[,c(names(ks))]

ck = count(tmp7[,c("SUBJ","DAYS")])
b = ck[ck$freq>1,]
b = subset(b, !(b$SUBJ %in% c(unique(c1$SUBJ), unique(c2$SUBJ), unique(c3$SUBJ))))


library(xlsx)
write.xlsx(c1, paste0(src, file="subday_dup.xlsx"), sheetName="KB", row.names=FALSE)
write.xlsx(c2, paste0(src, file="subday_dup.xlsx"), sheetName="KO", append=TRUE, row.names=FALSE)
write.xlsx(c3, paste0(src, file="subday_dup.xlsx"), sheetName="KS", append=TRUE, row.names=FALSE)

#SPACER BLOCKS Spacer blocks
#OTHER - SPECIFY Other
#KNEE BALANCER Knee balancer
#LAMINAR SPREADER Laminar spreader
#ELECTRONIC SENSOR TENSOR (EST)  Electronic sensor tensor (eST)
#Attune balanced sizer
#Attune measured sizer

#BY HAND


for(i in 1:(nrow(df))){
  
  t = ifelse(df$PROC_DUR_MIN[i]>0, df$PROC_DUR_MIN[i], ifelse(df$SGSTTIM[i]!="" & df$SGENTIM[i]!="", 
                                                              (as.numeric(substr(df$SGENTIM[i], 1, regexpr(":", df$SGENTIM[i])-1))-as.numeric(substr(df$SGSTTIM[i], 1, regexpr(":", df$SGSTTIM[i])-1)))*60+
                                                                as.numeric(substr(df$SGENTIM[i], regexpr(":", df$SGENTIM[i])+1, nchar(df$SGENTIM[i])))-
                                                                as.numeric(substr(df$SGSTTIM[i], regexpr(":", df$SGSTTIM[i])+1, nchar(df$SGSTTIM[i]))),NA))
  df$PROC_DUR_MIN[i]=t;
}

