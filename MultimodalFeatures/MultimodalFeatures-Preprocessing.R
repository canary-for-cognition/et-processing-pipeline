library(dplyr)
library(ggimage) 
library(ggplot2)
library(RColorBrewer)
library(jpeg)
library(grid)
library(readxl)

InfoUnits <<-       data.frame("IU" = "BOY", "KW" =c('boy', 'son', 'brother', 'male child'),stringsAsFactors=FALSE)
InfoUnits <<- rbind(InfoUnits,
                   data.frame( "IU" = "GIRL", 
                                         "KW" = c('girl', 'daughter', 'sister', 'female child')),
                   data.frame( "IU" = "WOMAN", 
                               "KW" = c('woman', 'mom', 'mother', 'lady', 'parent', 'female', 'adult', 'grownup')),
                   # data.frame( "IU" = "KITCHEN", --> We don't have this AOI in ET data
                   #             "KW" = c('kitchen', 'room')),
                   data.frame( "IU" = "WINDOW", #"IU" = "EXTERIOR" --> The AOI for exterior in ET data was named as "window" .
                               "KW" = c('exterior', 'outside', 'garden', 'yard', 'outdoors', 'backyard', 'driveway', 'path', 'tree', 'bush')),
                   data.frame( "IU" = "COOKIE", 
                               "KW" = c('cookie', 'biscuit', 'cake', 'treat')),
                   data.frame( "IU" = "JAR", 
                               "KW" =c('jar', 'container', 'crock', 'pot')),
                   data.frame( "IU" = "STOOL", 
                               "KW" = c('stool', 'seat', 'chair', 'ladder')),
                   data.frame( "IU" = "SINK", 
                               "KW" = c('sink', 'basin', 'washbasin', 'washbowl', 'washstand', 'tap')),
                   data.frame( "IU" = "PLATE", 
                               "KW" = c('plate')),
                   data.frame( "IU" = "DISHCLOTH", 
                               "KW" =c('dishcloth', 'dishrag', 'rag', 'cloth', 'napkin', 'towel')),
                   data.frame( "IU" = "WATER", 
                               "KW" = c('water', 'dishwater', 'liquid')),
                   # data.frame( "IU" = "WINDOW", --> We don't have this AOI in ET data (the one named "window" refer to Exterior InfoUnit)
                   #             "KW" = c('window', 'frame', 'glass')),
                   # data.frame( "IU" = "CUPBOARD", --> We don't have this AOI in ET data
                   #             "KW" =c('cupboard', 'closet', 'shelf')),
                   data.frame( "IU" = "DISHES", 
                               "KW" = c('dish', 'dishes', 'cup', 'cups', 'counter')),
                   data.frame( "IU" = "CURTAINS", 
                               "KW" = c('curtain', 'curtains', 'drape', 'drapes', 'drapery', 'drapery', 'blind', 'blinds', 'screen', 'screens')))



convertTimestamp <- function(x){
  aux <- x %>% toString() %>% strsplit(":") %>% `[[`(1)
  if (length(aux) == 2) return (as.numeric(aux[2])*1000)
  else if (length(aux) == 3) return (as.numeric(strsplit(aux[2],"nanos")[[1]])*1000+as.numeric(aux[3])/1000000)
  else return(NA)
}




synchronizeEyeTranscript <- function (pid) {
  print(paste0("Processing pid: ",pid))
  #read ET file for given pid
  e_file <- paste0("/Users/obarral/Documents/CANARY/Data/PRE-LOCKDOWN-ALL-DATA/Preprocessing/Eye_Raw/CanaryExperiment_",pid,".tsv")
  if (!file.exists(e_file)) {
    print(paste0("No eye tracking file for PID: ",pid))
    return()
  }
  e <- read.csv(e_file,sep="\t",stringsAsFactors = F)
  #There is an offset since beginning of log, and beginning of AUDIO recording
  #compute the offset by identifying beginning of AUDIO recording on tobii log file
  t_offset <- e$RecordingTimestamp[which(e$MediaName == "Screen Recordings (1)")[1]]
  e$InfoUnit <- NA
  #read transcript for given pid
  t <- read_excel(paste0("/Users/obarral/Documents/CANARY/Data/PRE-LOCKDOWN-ALL-DATA/CANARY Human Edited Transcripts/Transcript_",pid,".xlsx"))

  
  #format timestamps
  t$time_stamp_start <- sapply(t$time_stamp_start,convertTimestamp)
  t$time_stamp_end <- sapply(t$time_stamp_end,convertTimestamp) 
  
  #add offset
  t$time_stamp_start <- t$time_stamp_start + t_offset
  t$time_stamp_end <- t$time_stamp_end + t_offset
  
  #Convert Task number to integer
  t$task_number <- as.integer(t$task_number)
  
  
  #remove anny occurrece of 's otherwise words such as boy's are not identified as infounits
  t$spoken_word <- gsub('\'s','',t$spoken_word)
  
  #remove all puntuation from words
  t$spoken_word <- gsub('[[:punct:] ]+','',t$spoken_word)
  #remove capital letters
  t$spoken_word <- tolower(t$spoken_word)
  
  #select cookietheft task
  tc <- t[t$task_number==1 & !is.na(t$task_number),]
  
  #remove rows where experimenter is speaking
  tc <- tc[!(tc$experimenter_speaking %in% c("Experimenter","experimenter")),]
  
  ##for each word in transcript
  for (i in 1:nrow(tc)) {
    w <- tc$spoken_word[i]
    #check if word matches any keyword for any infoUnit
    infU_hit <- w == InfoUnits$KW
    if (is.na(sum(infU_hit))) next
    if (sum(infU_hit) == 0) next
    else if (sum(infU_hit) == 1){
      infU  <- InfoUnits$IU[infU_hit]
      if (infU == "COOKIE") { #check if it actually belongs to cookie jar INFOUNIT and not to cookie INFOUNIT
        if (tc$spoken_word[i+1] %in% InfoUnits[InfoUnits$IU=="JAR",]$KW) next
      }
      # add infounit marker in corresponding rows of eye tracking data for the duration of the mention of this word
      e$InfoUnit[e$RecordingTimestamp>= tc$time_stamp_start[i] & e$RecordingTimestamp<= tc$time_stamp_end[i]] <- infU
    }
    else {
      #error, print out message
      print(paste0("Following word has been found in multiple InfoUnit definitions: ",w))
    }
  }
  e$InfoUnit <- factor(e$InfoUnit,levels = InfoUnits$IU,labels=InfoUnits$IU)
  
  ###Write down modified eye tracking file
  write.csv(e,paste0("/Users/obarral/Documents/CANARY/Data/PRE-LOCKDOWN-ALL-DATA/Preprocessing/Eye_with_infoUnit/",pid,".csv"),row.names=F)
  # print(paste0("Pid: ",pid," COMPLETED"))
}



##Find all PIDs for which we have transcripts
transcripts <- list.files(path = "/Users/obarral/Documents/CANARY/Data/PRE-LOCKDOWN-ALL-DATA/CANARY Human Edited Transcripts/", pattern=".xlsx")
pids <- sapply(transcripts,function(x) strsplit(x,".xlsx") %>% `[[`(1) %>% `[`(1) %>% strsplit("_")%>% `[[`(1) %>% `[`(2))

#generate synchronized eye-language files for each pid
for (pid in pids) synchronizeEyeTranscript(pid)

# 
# ###SanityCheck##Check if any file has no infounit found
# e_t <- list.files(path = "/Users/obarral/Documents/CANARY/Data/PRE-LOCKDOWN-ALL-DATA/Preprocessing/Eye_with_infoUnit/", pattern=".csv",full.names = T)
# print( "FOLLOWING PIDS HAVE NO INFORMATION UNITS TIED TO EYE DATA:")
# for (x in e_t) {
#   aux <- read.csv(x)
#   if (sum(!is.na(aux$InfoUnit))==0) print(x)
# }

