# An example to compute RHD feature (patent defined)

library(batch)
startn <- 1
endn <- 2
parseCommandArgs()
print(startn)
print(endn)


#get the file list
src_bolus = "/storage2/medtronics/tenthousand_csv/"
src_hypo = "/storage2/medtronics/hypocounts/"
dest = "/storage2/medtronics/rhd/"

#get the pat ID
fid <- sub("_SG_hypo.csv", "", list.files(dest, pattern = "[0-9]_SG_hypo.csv$"))


compute_rhd = function(x){
  
  #initial the RHD to be zero - there is no SG reading, no hypo. 0
  rhd = NA

  #get the 4 hour window after each bolus timestamp x
  #include the last sg readings before the bolus event
  if (length(which(sg$ts>=x & sg$ts<=x+14400))>0){
     
     indx = which(sg$ts>=x & sg$ts<=x+14400 & sg$hypo==1)
     if(length(indx)>0){
       rhd = 240 - (sg$ts[indx[1]]-x)/60.
     }else{
       rhd = 0
     }
  }
  
  return (rhd)
  
}


compute_rhd1d = function(x){
  #each bolus event is used as a baseline to trace back 1d window
    
    #initialize the recent hypos in each windows
    rhd1d <- NA
    
    #define 1d windows prior to this bolus 
    w1d = bolus[which(bolus$dt<x & bolus$dt>=x-86400),]
            
    if(nrow(w1d)>0){ #if there are bolus record(s)
        #compute the average of rhd
        rhd1d = mean(w1d$rhd, na.rm=T)
        if(is.nan(rhd1d)){
          rhd1d = NA
        }
    }
    
    return (rhd1d)
    
  }
  


run_rhd <- function(startn, endn){
  
  startn = as.numeric(startn)
  endn = as.numeric(endn)t
  for (f in fid[startn:endn]){
    
    ###############################################
    #1. count the hypo events from SG readings
    ###############################################
    
    #read in sg & bolus
    sg = read.csv(paste(src_hypo, f, "_SG_hypo.csv", sep=""), header=TRUE)
    bolus = read.csv(paste(src_bolus, f, "_bolus.csv", sep=""), header=TRUE)
    
    #convert from factor to date
    sg$ts <-  as.numeric(strptime(sg$SG_dt,"%m/%d/%Y %H:%M:%S", tz="UTC"))
    sg <<- sg  #assign as global variable
    
    bolus$dt = as.numeric(strptime(bolus$bolus_dt,  "%m/%d/%Y %H:%M:%S", tz="UTC"))
    
    ###############################################
    #2. count hypo events based on bolus events
    ###############################################
    
    
    #locate the starting of the bolus which may have SG readings
    #to eliminate the bolus that starts earlier than 4 hours before read the first available SG
    min_bt = sg$ts[1]-14400
    max_bt = sg$ts[nrow(sg)]
    
    set_bt = (bolus$dt>=min_bt & bolus$dt <= max_bt)
    
    #initial counts
    bolus$rhd = NA
    
    t <- sapply(bolus$dt[set_bt], function(x) compute_rhd(x))
    
    bolus[set_bt,]$rhd = t
    
    bolus <<- bolus
    
    t2 <- sapply(bolus$dt, function(x) compute_rhd1d(x))
    
    bolus$rhd1d = t2
    
    #compute the rhd1d
    
  write.csv(bolus, paste(dest, f, "_bolus_rhd.csv", sep=""), row.names=FALSE)
    
  }
  
}


    