# An example to compute the sensor low glucose alert in a moving window
# hypo alert is target predictive outcome
# here by examining the actual counts in order to compare with the hypo model prediction
# to evaluate the model performance 

library(batch)
startn <- 1
endn <- 1
parseCommandArgs()
print(startn)
print(endn)



recentlowg_alert = function(x){
  
  #initialize the recent hypos in each windows
  lowg30d <- NA
  lowg14d <- NA
  lowg7d <- NA
  lowg3d <- NA
  lowg1d <- NA
  
  #define 30d, 14d, 7d and 1d windows
  
  #get the 30d window
  w30d = sa[which(sa$dt<x & sa$dt>=x-2592000),]
  
  #compute the hypo counts for all 6 flags
  if(nrow(w30d)>0){
    lowg30d= length(which(w30d$SensorAlert=="Low Glucose"))
    
    #get the 14d window
    w14d = w30d[which(w30d$dt<x & w30d$dt>=x-1209600),]
    
    if(nrow(w14d)>0){
      lowg14d= length(which(w14d$SensorAlert=="Low Glucose"))
      
      #get the 7d window
      w7d = w14d[which(w14d$dt<x & w14d$dt>=x-604800),]
      
      if(nrow(w7d)>0){
        lowg7d= length(which(w7d$SensorAlert=="Low Glucose"))
        
        #get the 3d window
        w3d = w7d[which(w7d$dt<x & w7d$dt>=x-259200),]
        
        if(nrow(w3d)>0){
          lowg3d= length(which(w3d$SensorAlert=="Low Glucose"))
          
          #get the 1d window
          w1d = w3d[which(w3d$dt<x & w3d$dt>=x-86400),]
          
          if(nrow(w1d)>0){
            lowg1d = length(which(w1d$SensorAlert=="Low Glucose"))
          }  #1d
        }  #3d
      }  #7d
    }  #14d
  } #30d
  
  
  return (data.frame(bolus_dt=x, lowg30d=lowg30d, lowg14d=lowg14d, lowg7d=lowg7d, lowg3d=lowg3d, lowg1d=lowg1d))
  
}


#start the main program

#read in sa & bolus
src = "/storage2/medtronics/tenthousand_csv/"
dest = "/storage2/medtronics/tenthousand_features_csv/lowg/"


fid <<- sub("_bolus.csv", "", list.files(src, pattern = "[0-9]_bolus.csv$"))


count_lowg_alert <- function(startn, endn){
  
  startn = as.numeric(startn)
  endn = as.numeric(endn)
  
  
  for (pid in fid[startn:endn]){
    

#start = proc.time()

#pid ="10010"

    
    sa = read.csv(paste(src, pid, "_SensorAlert.csv", sep=""), header=TRUE)[,c("SensorAlert_dt","SensorAlert")]
    #sa = sa[sa$SensorAlert=="Low Glucose",]
    bolus = read.csv(paste(src, pid, "_bolus.csv", sep=""), header=TRUE)[,c("bolus_dt","bolus_delivered_U")]
    bolus <<- bolus[bolus$bolus_delivered_U>0,][,c("bolus_dt"), drop=F]
    
    #convert from factor to date
    sa$dt = as.numeric(strptime(sa$SensorAlert_dt,"%m/%d/%Y %H:%M:%S", tz="UTC"))
    bolus$dt = as.numeric(strptime(bolus$bolus_dt,  "%m/%d/%Y %H:%M:%S", tz="UTC"))
    
    #locate the sg timestamp range
    min_sa = bolus$dt[1]-2592000
    max_sa = bolus$dt[length(bolus$dt)]
    
    
    sa <<- sa[which(sa$dt>=min_sa & sa$dt <= max_sa),c(2:3)]
    
    s <- do.call(rbind, lapply(bolus$dt, function(x) recentlowg_alert(x)))
    #s <- do.call(rbind, lapply(pid, function(x) recentlowg_alert(x)))
    
    s$lowg1d3d <- NA
    s$lowg3d7d <- NA
    s$lowg7d14d <- NA
    s$lowg14d30d <- NA
    
    s$lowg1d3d = s$lowg3d-s$lowg1d
    s$lowg3d7d = s$lowg7d-s$lowg3d
    s$lowg7d14d = s$lowg14d-s$lowg7d
    s$lowg14d30d = s$lowg30d-s$lowg14d
    
    
    write.csv(s, paste(dest, pid, "_lowgalert.csv", sep=""), row.names=FALSE)
    
    #proc.time() - start
  }
}

count_lowg_alert(startn, endn)

#end of the program
