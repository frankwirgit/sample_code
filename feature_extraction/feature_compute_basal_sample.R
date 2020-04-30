# An example to compute a specific feature - basal insulin rate on a daily basis
# Basal ratio is one of the complicated insulin feature included in the hypo prediction model


#compute the basal value in different windows (8hr, 4hr, 2hr, 30min and 5min) before each bolus event

#look for the basal rate
parse_rate <- function(z){
  target_dt = z$dt[1]
  #create the start time 
  a = as.POSIXct(target_dt, origin="1970-1-1", tz="EST")
  start_dt = as.numeric(strptime(as.Date(a, tz="EST"),"%Y-%m-%d", tz="EST"))
  pdt = (target_dt - start_dt)/(24*3600)
  
  brate = 0
  rate_ind = which(z$BasalPatternSettingChange_ChangeStartTime>=pdt)
  if(length(rate_ind)>0){
    brate = z$BasalPatternSettingChange_ChangeRate_Uhr[rate_ind[1]+1]
  }else{
    
    brate = z$BasalPatternSettingChange_ChangeRate_Uhr[1]
  }
  return (brate)
  
}

get_basal_rate <- function(z){
  
  brate = 0
  #check the record type
  #the record is a both selected pattern and changed pattern 
  if (!is.na(z$BasalPatternSelected_dt[1]) & !is.na(z$BasalPatternSettingChange_dt[1])) {
    
    brate = parse_rate(z)
    
    #get the selected pattern number
    pattern_numb = as.integer(z$BasalPatternSelected[1])
    
    #save the selected pattern
    if(is.na(latest_z[[pattern_numb]]$dt[1]) | (latest_z[[pattern_numb]]$dt[1]<z$dt[1])){
      
      latest_z[[pattern_numb]] <<- z
    }
    
  }else if(!is.na(z$BasalPatternSettingChange_dt[1])){ #changed pattern
    
    brate = parse_rate(z)
    
  }else if(!is.na(z$BasalPatternSelected_dt[1])){
    
    #extract the saved latest pattern and calculate the basal amount
    pattern_numb = as.integer(z$BasalPatternSelected[1])
    tmp_dt = z$dt[1]
    z = latest_z[[pattern_numb]]
    
    if(!is.na(z$dt[1])){ #if there is a saved pattern
      
      z$dt = tmp_dt
      brate = parse_rate(z)
      
    }else{ #if there is no saved pattern, then the inital state of this pattern is lost, use the last saved rate
      #trace the last changed pattern
      brate = last_basal_rate
    }
    
  }else if(!is.na(z$TempBasal_value_Uhr_Percent[1])){ #if there is a temp basal
    # adjust the basal amount using percentage of the time frame
    # since the temp release time is always longer than 5 min, the temp percentage can be applied to rate only
    if( (!is.na(z$TempBasal_value_Uhr_Percent[1]))) {
      brate = last_basal_rate*z$TempBasal_value_Uhr_Percent[1]/100.
    }
    
  }
  
  return (brate)
}

  
#for loop or sapply for every bolus event
#compute basal amount in a specific window before each bolus event
basal_each_bolus <- function(x, win_sec){
  
  #initialize the three patterns latestly changed and the pattern used in current 
  z0 = data.frame(bsc[1,])
  z0$dt <- NA #mark as the flag
  latest_z <<- list(z0, z0, z0)
  
  
  #trace the last rate used
  tmp = which(bsc$dt<(x-win_sec) & bsc$BasalPatternSettingChange_ChangeRate_Uhr>0)
  if(length(tmp)==0){
    last_basal_rate = 0 #no last rate found
  } else{
    zt = bsc[bsc$dt==bsc$dt[tmp[length(tmp)]],]
    last_basal_rate <<- parse_rate(zt)
  }
  
  #define 5min window
  #x = bolus$dt[2082]
  w5m = bsc[which(bsc$dt<x & bsc$dt>=x-win_sec),]

  #if there is no basal change information - 
  if(nrow(w5m)==0){
     #compute the basal by using the last rate traced back in history
     return (win_sec*last_basal_rate/3600)
    
  }
  else if(nrow(w5m)>0) {
    #141800
    #if there are multiple basal records in the matrix, get each of the timeslots divided by basal records
    #compute the basal rate for each of the records
    basal_dt = unique(w5m$dt)
    udt = c(x-win_sec, basal_dt, x)
    udt = unique(udt)
    nb = length(udt)
    ck <- udt[2:nb]-udt[1:(nb-1)]
    
    #compute basal_amt
    basal_amt1 = ck[1]*last_basal_rate/3600
    
    
    basal_amt2 = 0
    for(i in 1:(length(basal_dt))){
      
      zt = w5m[w5m$dt==basal_dt[i],]
      basal_rate = get_basal_rate(zt)
      last_basal_rate <<- basal_rate
      basal_amt2 = basal_amt2 + ck[i]*basal_rate/3600
     
    }
    
    return (basal_amt1+basal_amt2)
    
  }
}
      






#begin the main program

library(DataCombine)

start= proc.time()

#pid ="665385"
#pid="10015"
#pid = "10010"
#pid = "103634"
pid = "50472243"

#read in sg & bolus
src = "C:/Users/IBM_ADMIN/Documents/fwork/Projects/MDT_prod/data/"
selected = read.csv(paste(src, pid, "_BasalPatternSelected.csv", sep=""), header=TRUE)[,1:2]
changed = read.csv(paste(src, pid, "_BasalPatternSettingChange.csv", sep=""), header=TRUE)[,c(1,4:5)]
tempb = read.csv(paste(src, pid, "_TempBasal.csv", sep=""), header=TRUE)[,c(1,3:4)]
#clean up the temp basal by removing the 0 time records
tempb = tempb[tempb$TempBasal_time_min>0,]

bolus = read.csv(paste(src, pid, "_bolus.csv", sep=""), header=TRUE)[,c(1), drop=F]

#convert from factor to date
selected$dt = as.numeric(strptime(selected$BasalPatternSelected_dt,"%m/%d/%Y %H:%M:%S", tz="EST"))
changed$dt = as.numeric(strptime(changed$BasalPatternSettingChange_dt,"%m/%d/%Y %H:%M:%S", tz="EST"))
tempb$dt = as.numeric(strptime(tempb$TempBasal_dt,"%m/%d/%Y %H:%M:%S", tz="EST"))
bolus$dt = as.numeric(strptime(bolus$bolus_dt,  "%m/%d/%Y %H:%M:%S", tz="EST"))


#merge selected with bolus
bc = merge(x=selected, y=changed, by="dt", all=T)
bc = bc[!is.na(bc$dt),]
bsc <<- merge(x=bc, y=tempb, by="dt", all=T)
bsc = bsc[!is.na(bsc$dt),]



#inital last basal rate
last_basal_rate <<- 0

#a function to define those 5 windows (8hr 4hr 2hr 30min and 5min) and compute the basal for each window
compute_basal <- function(x){
  
  
  #b8hr = basal_each_bolus(x, 28800)
  b4hr = basal_each_bolus(x, 14400)
  #b2hr = basal_each_bolus(x,  7200)
  #b30m = basal_each_bolus(x,  1800)
  #b5m  = basal_each_bolus(x,   300)
  #return (data.frame(bolus_ts=x, basal8hr=b8hr, basal4hr=b4hr, basal2hr=b2hr, basal30m=b30m, basal5m=b5m))
  return (data.frame(bolus_ts=x, bolus4hBasal_U=b4hr))
}



start = proc.time()
basal_5win = do.call(rbind, lapply(bolus$dt[1:260], function(x) compute_basal(x)))
basal_5win = do.call(rbind, lapply(bolus$dt, function(x) compute_basal(x)))
proc.time()-start

#basal_5win = do.call(rbind, lapply(bolus$dt[1:470], function(x) compute_basal(x)))

write.csv(basal_5win, paste(src, pid, "_bolus_basalAmt.csv", sep=""), row.names=FALSE)

#method 2
#insert bolus into the history divided by 5min intervals

#divide the whole history
#bolus_hist <<- seq(bolus$dt[1]-28800, bolus$dt[nrow(bolus)]+300, by=300)
#start = proc.time()
#basal_bolus_history <<- sapply(bolus_hist[2:length(bolus_hist)], function(x) basal_each_bolus(x))
#proc.time()-start

#return the starting position of each bolus, then the end index would be start+96
#build a dataframe and rbind to be 

#insert_bolus <- function(y){
#  startp = which(bolus_hist>(y-28800))[1]-1
#  return ( cbind(bolus_ts=y, data.frame(bolus5minBasal_U=basal_bolus_history[startp:(startp+95)])) )
#}

#basal2 = do.call(rbind, lapply(bolus$dt, function(y) insert_bolus(y))) 



#end of the program



basal_row <- function(x){
  w5m = bsc[which(bsc$dt<x & bsc$dt>=x-300),]
  udt = unique(w5m$dt)
  return (length(udt))
}

#mt = sapply(bolus$dt, function(x) basal_row(x))

basal_row2 <- function(x){
  w5m = bsc[which(bsc$dt<x & bsc$dt>=x-300),]
  return (nrow(w5m))
}

#mt2 = sapply(bolus$dt, function(x) basal_row2(x))

