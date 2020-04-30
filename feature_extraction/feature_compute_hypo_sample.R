# An example to count hypo based on the bolus events through a moving window

library(batch)
startn <- 1
endn <- 2
parseCommandArgs()
print(startn)
print(endn)

##/usr/bin/Rscript

#get the file list
#src="/homes/hny5/lcao/dataset/"
#src = "C:/Users/IBM_ADMIN/Documents/fwork/Projects/MDT_prod/data/"
src = "/storage2/medtronics/tenthousand_csv/"
dest = "/storage2/medtronics/hypocounts/"

#get the pat ID
fid <- sub("_SG.csv", "", list.files(src, pattern = "[0-9]_SG.csv$"))
#fdone <- sub("_bolus_hypo.csv", "", list.files(dest, pattern = "[0-9]_bolus_hypo.csv$"))

#fid2 = setdiff(fid, fdone)


#each bolus event is used as a baseline to extract the sg window (2hr, 3hr, 4hr etc.)
count_hypo4 = function(x){
  #define the 2-hr window after the current bolus event
  #include the last sg readings before the bolus event
  hypo2h <- NA
  hypo3h <- NA
  hypo4h <- NA
  shypo2h <- NA
  shypo3h <- NA
  shypo4h <- NA
  
  #define 3 time windows
  
  #get the 4 hour window
  indx = which(nsg_t>=x & nsg_t<=x+14400)
  w4h = sg[indx,]
  
  
  #compute the hypo flags
  if(nrow(w4h)>0){
    shypo4h = sum(w4h$hypo_severe)
    hypo4h = sum(w4h$hypo)
    
    #get the 3 hour window
    nsg_t2 = nsg_t[indx]
    w3h = w4h[which(nsg_t2>=x & nsg_t2<=x+10800),]
    
    if(nrow(w3h)>0){
      shypo3h = sum(w3h$hypo_severe)
      hypo3h = sum(w3h$hypo)
      
      #get the 2 hour window
      w2h = w4h[which(nsg_t2>=x & nsg_t2<=x+7200),]
      if(nrow(w2h)>0){
        shypo2h = sum(w2h$hypo_severe)
        hypo2h = sum(w2h$hypo)
      }
      
    }
    
  }
  
  return (list(hypo2h=hypo2h, hypo3h=hypo3h, hypo4h=hypo4h, shypo2h=shypo2h, shypo3h=shypo3h, shypo4h=shypo4h))
  
}



run_count <- function(startn, endn){
  
  startn = as.numeric(startn)
  endn = as.numeric(endn)
  #for (f in fid2[startn:endn]){
  for (f in fid[startn:endn]){
    
    ###############################################
    #1. count the hypo events from SG readings
    ###############################################
    
    #read in sg & bolus
    sg = read.csv(paste(src, f, "_SG.csv", sep=""), header=TRUE)
    sg <<- sg;
    bolus = read.csv(paste(src, f, "_bolus.csv", sep=""), header=TRUE)
    
    #convert from factor to date
    nsg_t <<- as.numeric(strptime(sg$SG_dt,"%m/%d/%Y %H:%M:%S", tz="UTC"))
    nbolus_t = as.numeric(strptime(bolus$bolus_dt,  "%m/%d/%Y %H:%M:%S", tz="UTC"))
  
    #to count hypo events for the whole sg once
    sg$hypo_severe=0
    
    #severe hypo event definition: 2 or more consecutive sg reading lower than 50 mgdL
    
    hsev= which(sg$SG_mgdL<50)
    nb = length(hsev)
    if(nb>0){
      ck <- nsg_t[hsev][2:nb]-nsg_t[hsev][1:(nb-1)]
      names(ck)=hsev[2:nb]
      #total count of severe hypo
      hypo_severe=length(which(rle(ck)$values==300))
      if(hypo_severe>0) {
        
        w2 = as.numeric(rle(ck)$length[which(rle(ck)$value==300)])
        w1 = as.numeric(names(which(rle(ck)$values==300)))
        
        #mark the 2nd low sg as the hypo event = 1
        #if more than 2 low sg readings, still it's the 2nd low sg is marked
        sg[w1-w2+1,]$hypo_severe=1
      }
    }
    
    #regular hypo event definition: 2 or more consecutive sg reading lower than 70 mgdL
    sg$hypo = 0
    
    hreg= which(sg$SG_mgdL<70)
    nb = length(hreg)
    if(nb>0){
      ck <- nsg_t[hreg][2:nb]-nsg_t[hreg][1:(nb-1)]
      names(ck)=hreg[2:nb]
      #total count - 
      hypo=length(which(rle(ck)$values==300))
      if(hypo>0) {
        w2 = as.numeric(rle(ck)$length[which(rle(ck)$value==300)])
        w1 = as.numeric(names(which(rle(ck)$values==300)))
        
        #mark the 2nd low sg as the hypo event = 1
        #if more than 2 low sg readings, still it's the 2nd low sg is marked
        sg[w1-w2+1,]$hypo=1
      }
    }
    
    
    write.csv(sg, paste(dest, f, "_SG_hypo.csv", sep=""), row.names=FALSE)
    
    
    ###############################################
    #2. count hypo events based on bolus events
    ###############################################
    
    
    #locate the starting of the bolus which may have SG readings
    #to eliminate the bolus that starts earlier than 4 hours before read the first available SG
    min_bt = nsg_t[1]-14400
    max_bt = nsg_t[length(nsg_t)]
    
    set_bt = (nbolus_t>=min_bt & nbolus_t <= max_bt)
    
    #initial counts
    bolus$hypo2h <- NA
    bolus$hypo3h <- NA
    bolus$hypo4h <- NA
    bolus$shypo2h <- NA
    bolus$shypo3h <- NA
    bolus$shypo4h <- NA
    
    
    t <- sapply(nbolus_t[set_bt], function(x) count_hypo4(x))
    t <- ifelse(t>0, 1, 0)
    tt <- data.frame(t(t))
    bolus[set_bt,]$hypo2h = unlist(tt$hypo2h)
    bolus[set_bt,]$hypo3h = unlist(tt$hypo3h)
    bolus[set_bt,]$hypo4h = unlist(tt$hypo4h)
    bolus[set_bt,]$shypo2h = unlist(tt$shypo2h)
    bolus[set_bt,]$shypo3h = unlist(tt$shypo3h)
    bolus[set_bt,]$shypo4h = unlist(tt$shypo4h)
    
  
    write.csv(bolus, paste(dest, f, "_bolus_hypo.csv", sep=""), row.names=FALSE)
    
  }

}

run_count(startn, endn)


