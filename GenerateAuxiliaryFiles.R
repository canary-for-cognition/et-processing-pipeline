library(dplyr)
library(tidyr)
library(rlist)
library(readxl)

##Modify Tobii Files with wrong file names
fixWrongNameFile <- function(studyId, tobiId) {
  if (studyId %in% c("HA-192","HO-193","HL-176")) { #these pids were logged as a followup studies, by mistake. Modify and Rename file
    f <- paste0("CANARY 6 Month Follow-up_sally_",tobiId,".tsv")
    dat<- read.csv(paste0(TOBII_EXPORT_DIR,f),sep = "\t",stringsAsFactors = F,dec=".")
    dat$StudioProjectName <- "CanaryExperiment"
  }
  else if (studyId != tobiId) { #if file was labelled with a wrong studyId, read file using tobiId
    f <- paste0("CanaryExperiment_",tobiId,".tsv")
    if (!file.exists(paste0(TOBII_EXPORT_DIR,f))) return(NULL)
    dat<- read.csv(paste0(TOBII_EXPORT_DIR,f),sep = "\t",stringsAsFactors = F,dec=".")
  } else return (NULL)
  
  #update file with correct studyId
  dat$RecordingName <- studyId
  dat$ParticipantName <- studyId
  
  #Fetch colnames
  con <- file(paste0(TOBII_EXPORT_DIR,f),"r")
  col_names <- readLines(con,n=1)
  close(con)
  #write modified Tobii file directly in preprocess dir
  correctFileName <- paste0("CanaryExperiment_",studyId,".tsv")
  write.table(x = dat,file = paste0(PREPROCESS_DIR_DATA,correctFileName),sep = "\t",dec = ".",quote=FALSE,na = "",col.names = c(strsplit(col_names,"\t")[[1]],""),row.names = FALSE)
  print(paste0(f," renamed to: ",correctFileName))
  return(correctFileName)
}

##Modify Tobii Files to fix event mismatch
fixMismatchEvents <- function(pid,filepath,f) {
  d<- read.csv(filepath,sep = "\t",stringsAsFactors = F,dec=".")
  aux <- which(d$KeyPressEvent %in% c("Right", "Space","Down","Next"))
  nEvents <- length(aux)
  if (nEvents %in% c(9,35)){
    #the PID has already been fixed, do nothing
    return (nEvents)
  } else if (pid == "EA-069"){
    # Problem: Experimenter went backwards during the second instructional slide, leading to an extra right key press to go back to the second instructional slide
    # Solution: Delete the first right key press
    d$KeyPressEvent[aux[1]] <- ""
  } else if (pid == "EE-105"){
    # Problem: Missing the first event (it was a mouse click instead of keypress). Currently Pupil Calibration starts at Event 1 instead of 2. 
    # Solution: Artificially add a first dummy event on the logs so that pupil calibration becomes event 2
    d$KeyPressEvent[aux[1]-1] <- "Space"
  } else if (pid %in% c("HA-128","HA-142")){
    # Problem: The last event (end of memory task) has been pressed with Escape instead of arrow/space 
    # Solution: Modify log by changing Esc with SPACE
    d$KeyPressEvent[d$KeyPressEvent == "Escape"] <- "Space"
  } else if (pid == "HO-187") {
    # Problem: Experimenter went backwards (left mouse click) during the paragraph reading task so we have an extra space key press. To get the text reading task, we will have to get the left mouse click (which is the start of the task).
    # Solution: 
    #   Deleted the following two Space events:
    #     KeyPress, Space -- timestamp: 148858 (00:02:28.857)
    #     KeyPress, Space -- timestamp: 161738 (00:02:41.737)
    #   Changed the following Left mouse click event to be a space event:
    #     LeftMouseClick -- timestamp: 167697 (00:02:47.697)
    d$KeyPressEvent[d$RecordingTimestamp == 148858] <- ""
    d$KeyPressEvent[d$RecordingTimestamp == 161738] <- ""
    d$MouseEvent[d$RecordingTimestamp == 167697] <- ""
    d$KeyPressEvent[d$RecordingTimestamp == 167697] <- "Space"
  } else return(nEvents)
  #recompute nEvents after correction
  nEvents <- sum(d$KeyPressEvent %in% c("Right", "Space","Down","Next"))
  
  #Fetch colnames
  con <- file(filepath,"r")
  col_names <- readLines(con,n=1)
  close(con)
  #write modified Tobii file directly in preprocess dir
  write.table(x = d,file = paste0(PREPROCESS_DIR_DATA,f),sep = "\t",dec = ".",quote=FALSE,na = "",col.names = c(strsplit(col_names,"\t")[[1]],""),row.names = FALSE)
  return(nEvents)
}

generateAOIFiles <- function(task) {
  ##Fetch all AOI definitions from Tobii export for the task at hand
  lns <- file(paste0("/Users/obarral/Documents/CANARY/AOIs/",task,"/All_AOIs.txt"),open="r") %>% readLines
  i <-1
  AOIs <- list()
  while (i < length(lns)) {
    AOI <- list()
    AOI[["name"]] <- lns[i]
    i <- i + 1
    while (lns[i] != "") {
      AOI <- list.append(AOI,strsplit(lns[i],"\t")[[1]] %>% paste0(collapse=","))
      i <- i + 1
    }
    AOIs <- list.append(AOIs,AOI)
    i <- i + 1 
  }
  #format for output
  AOIs <- lapply(AOIs,paste0,collapse="\t")
  
  #gather all segment files for all participants
  files <- list.files(paste0(PREPROCESS_DIR,"Segments"), pattern=".seg", all.files=FALSE,  full.names=FALSE)
  #for each participant, print out an AOI file with the corresponding task timestamps
  for (f in files) {
    d_seg<- read.table(paste0(PREPROCESS_DIR,"Segments/",f),sep = "\t",stringsAsFactors = F,header = F)
    #fetch timestamps for task at hand only
    d_ini <- d_seg[d_seg[,1]==task,3]
    d_end <- d_seg[d_seg[,1]==task,4]
    
    #print formatted AOI file for EMDAT
    fname_aoi <- paste0(PREPROCESS_DIR,"/AOIs/",strsplit(f,".seg")[[1]],".aoi")
    #clear file if it already exists
    close( file( fname_aoi, open="w" ) )
    #write formatted AOI-segment info to file
    sink(fname_aoi,append = T)
    lapply(AOIs,function(aoi) {
      cat(aoi)
      cat(paste0("\n","#","\t",d_ini,",",d_end,"\n"))
    })
    closeAllConnections()
  }
  
}


generatePupilFile <-function() {
  #gather all segment files for all participants
  files <- list.files(paste0(PREPROCESS_DIR,"Segments"), pattern=".seg", all.files=FALSE,  full.names=FALSE)
  #for each participant, print out an AOI file with the corresponding task timestamps
  d <- c()
  for (f in files) d<- rbind(d,cbind(strsplit(f,".seg")[[1]] %>% strsplit("CanaryExperiment_") %>% `[[`(1) %>% tail(1),
                                read.table(paste0(PREPROCESS_DIR,"Segments/",f),sep = "\t",stringsAsFactors = F,header = F)))
  d <- d[,c(1,2)]
  names(d) <- c("pid","ScID")
  #read pupilSizes
  pup_sizes <- read.csv(PUPIL_BASELINES_FILE,header = FALSE)
  names(pup_sizes) <-c("pid","pupSize")
  d <- merge(d,pup_sizes,by = "pid")
  d <- spread(d,key=ScID,value=pupSize)
  write.table(d,paste0(PREPROCESS_DIR,"PupilBaselines/all_rest_pupil_sizes.tsv"),row.names = FALSE,sep="\t",quote = FALSE)

}


generateTimeWindows <- function(d,sceneName,windowSizes) {
  for (windowSize in windowSizes) {
    aux <- seq(d$Ini[d$ScID==sceneName],d$End[d$ScID==sceneName],by=windowSize)
    #if there is at least one full segment of size windowSize
    if (length(aux)>1) {
      segIni <- aux[-length(aux)]
      segEnd <- aux[-1]
      segID <- paste0(sceneName,"_",windowSize,"_",0:(length(aux)-2))
      d <- rbind(d,data.frame(data.frame(ScID = segID, SegID = sceneName, Ini = segIni, End = segEnd)))
    }
  }
  return (d)
}

generateSegments <- function(d,SegCookie = NULL, SegRead = NULL, SegMem = NULL) {
  aux <- which(d$KeyPressEvent %in% c("Right", "Space","Down","Next"))
  tasks <- c("PupilCalib","CookieTheft","Reading","Memory")
  if (length(aux) == 9) { #old codification
    iniTimes <- c(d$RecordingTimestamp[aux[2]],
                  d$RecordingTimestamp[aux[4]],
                  d$RecordingTimestamp[aux[6]],
                  d$RecordingTimestamp[aux[8]])
    endTimes <- c(d$RecordingTimestamp[aux[3]],
                  d$RecordingTimestamp[aux[5]], 
                  d$RecordingTimestamp[aux[7]],
                  d$RecordingTimestamp[aux[9]])
  }else if (length(aux) ==35) {#new codification
    iniTimes <- c(d$RecordingTimestamp[aux[14]],
                  d$RecordingTimestamp[aux[20]],
                  d$RecordingTimestamp[aux[25]],
                  d$RecordingTimestamp[aux[34]])
    endTimes <- c(d$RecordingTimestamp[aux[15]],
                  d$RecordingTimestamp[aux[21]], 
                  d$RecordingTimestamp[aux[26]],
                  d$RecordingTimestamp[aux[35]])  
    
  } else return(NULL)
  d <- data.frame(ScID = tasks, SegID = tasks, Ini = iniTimes, End = endTimes)
  #generate windows if specified by the corresponfing SegSize for each task
  if (!is.null(SegCookie))  d <- generateTimeWindows(d,"CookieTheft",SegCookie) 
  if (!is.null(SegRead))  d <- generateTimeWindows(d,"Reading",SegRead) 
  if (!is.null(SegMem))  d <- generateTimeWindows(d,"Memory",SegMem) 
  return(d)
}

computePupilBaseline <- function(d,seg) {
  pBase <- seg[seg$SegID =="PupilCalib",]
  #calculate the pupil baseline from 1 sec after first second of cross onset until 1 sec prior to cross offset
  ##Here potentially compute a smarter approach that gets baseline from by finding 5 secs of valid data fixated on the cross
  d_base <- d[d$RecordingTimestamp >= pBase$Ini+1000 & d$RecordingTimestamp <= pBase$End-1000,]
  #Convert -1s to NAs
  d_base$PupilLeft[d_base$PupilLeft == -1] <- NA
  d_base$PupilRight[d_base$PupilRight == -1] <- NA
  # initialize pupil sizes to compute baseline from
  pup_sizes <- rowMeans(cbind(d_base$PupilLeft,d_base$PupilRight))
  #try to replace missing pupil sizes by potentially not missing left/right values
  pup_sizes[is.na(pup_sizes)] <- d_base$PupilLeft[is.na(pup_sizes)]
  pup_sizes[is.na(pup_sizes)] <- d_base$PupilRight[is.na(pup_sizes)]
  #return the mean of the valid (not-NA) averaged Left-Right pupil sizes
  return(mean(pup_sizes,na.rm=T))
}

convertTimestampToString <- function(t) {
  t_str <- list()
  t_str[[1]] <- floor(t/60000)
  t_str[[2]] <- floor(t/1000) - t_str[[1]]*60
  t_str[[3]] <- t -t_str[[1]]*60000 - t_str[[2]]*1000
  
  return(
    paste0(t_str,collapse=":")
  )
}

#Write timestamps of a certain task to deparate file so can be used for transcripts
writeBips <- function(outfile,pid, dat,d_seg, task, offset) {
  #compute timestamps as per the Eye tracking log
  timestampIni <- d_seg$Ini[d_seg$SegID == task]
  timestampEnd <- d_seg$End[d_seg$SegID == task]
  #compute timestamps of the bips in the synchronized Audio files
  timestampIni_bip <- timestampIni - offset
  timestampEnd_bip <- timestampEnd - offset
  
  cat(paste0(
          paste(pid,task,timestampIni,timestampEnd,
                convertTimestampToString(timestampIni),convertTimestampToString(timestampEnd),
                timestampIni_bip,timestampEnd_bip,
                convertTimestampToString(timestampIni_bip),convertTimestampToString(timestampEnd_bip),sep=","),
          "\n"),
      file =outfile,append = TRUE)
}


processSegments <- function () {
  
  ##Read ParticipantLog
  p_log <- read_excel("CANARY_Participant_Log.xlsx", sheet = "Participant_Log")
  # #write p_log sheet1 as csv in preprocess folder
  # write.table(x = p_log,file = paste0(PREPROCESS_DIR,"CANARY_Participant_Log.csv"),sep = ",",dec = ".",quote=FALSE,na = "",row.names = FALSE)
  
  ##Reset output files
  cat ("",file=PREPROCESS_FILE,append=FALSE)
  cat("",file=PUPIL_BASELINES_FILE,append=FALSE)
  cat ("StudyID,Task,timestampIni,timestampEnd,timeIni,timeEnd,timestampIni_bip,timestampEnd_bip,timeIni_bip,timeEnd_bip\n",
       file=BIPS_FILE,append=FALSE)
  
  #count number of files exported from Tobii
  files <- list.files(TOBII_EXPORT_DIR, pattern=".tsv",all.files = FALSE,  full.names=FALSE)
  #log number of Tobii files
  cat (paste0("Number of Files exported from Tobii (including follow-up): ",length(files),"\n"),
       file = PREPROCESS_FILE,append=TRUE)
  
  ##get rid of empty lines
  p_log <- p_log[!(p_log$`Study ID` %in% c("",NA)),]
  #log
  cat (paste0("Number of StudyIDs in Log: ",length(p_log$`Study ID`),"\n",
              "Number of unique StudyIDs in Log: ",length(unique(p_log$`Study ID`)),"\n"),
       file = PREPROCESS_FILE,append=TRUE)
  
  ##get rid of entries for which we don't have TobiiID data
  p_log <- p_log[!(p_log$`Recording Name Tobii` %in% c("",NA,"N/A")),]
  #log
  cat (paste0("Number of Tobii RecordingName in Log: ",length(p_log$`Recording Name Tobii`),"\n",
              "Number of unique Tobii RecordingName in Log: ",length(unique(p_log$`Recording Name Tobii`)),"\n"),
              # "Number of Tobii RecordingName not matching their StudyID: ",sum(p_log$`Recording Name Tobii` != p_log$`Study ID` ),"\n"),
       file = PREPROCESS_FILE,append=TRUE)
  
  ##get rid of entries for which p_log indicates eye tacking data is invalid
  invData <- p_log$`Eye-Tracking Calibration?` == 0
  p_log <- p_log[!invData,]
  #log
  cat (paste0("Number of Tobii RecordingName in Log marked as invalid data : ",sum(invData),"\n",
              "Number of Tobii RecordingName with valid data: ",length(p_log$`Recording Name Tobii`),"\n",
              "Number of unique Tobii RecordingName with valid data: ",length(unique(p_log$`Recording Name Tobii`)),"\n"),
       file = PREPROCESS_FILE,append=TRUE)
  
  
  pids <- c()
  pupBaselines <- c()
  pidsNotFound <- c()
  pidsWrongEvents <-c()
  processedPIDS <- 0
  for (i in 1:nrow(p_log)) {
    tobiId <- p_log$`Recording Name Tobii`[i]
    studyId <- p_log$`Study ID`[i]
    f <- paste0("CanaryExperiment_",studyId,".tsv")
    FILEPATH <- paste0(TOBII_EXPORT_DIR,f)
    
    ## if the file is not in tobii export directory
    if (!(f %in% files)) { 
        f <- fixWrongNameFile(studyId,tobiId) # try to fix name
        if (is.null(f)){ #name could not be fixed: file not found
          pidsNotFound <- c(pidsNotFound,studyId)
          print (paste0(f," not found"))
          next
        } else { #name could be fixed: update filepath to point to the preprocess_dir, where renamed file is located
          FILEPATH <- paste0(PREPROCESS_DIR_DATA,f)
        }
    }
    #read data
    dat<- read.csv(FILEPATH,sep = "\t",stringsAsFactors = F,dec=".")
  
    #extract segments timestamps
    d_seg <- generateSegments(dat)
    if (is.null(d_seg)) { #if the number of events is wrong
      fm <- fixMismatchEvents(studyId,FILEPATH,f) #fix the error
      if (fm %in% c(9,35)) {
        #if fixed successfully, read corrected data and re-compute segments
        FILEPATH <- paste0(PREPROCESS_DIR_DATA,f)
        dat<- read.csv(FILEPATH,sep = "\t",stringsAsFactors = F,dec=".")
        d_seg <- generateSegments(dat)
        print (paste0(f," events don't match: corrected successfully (events = ",fm,")"))
      } else {
        #if not fixed correctly add to log, and go to next iteration of loop
        pidsWrongEvents <- c(pidsWrongEvents,studyId)
        print (paste0(f," events don't match: could not correct (events = ",fm,")"))
        cat (paste0(f," events don't match: could not correct (events = ",fm,")\n"),
             file = PREPROCESS_FILE,append=TRUE)
        next
      }
    }
    #export segment file 
    write.table(d_seg,paste0(PREPROCESS_DIR,"Segments/",strsplit(f,".tsv")[[1]],".seg"),col.names = FALSE,row.names = FALSE,sep="\t",quote = FALSE)
  
    #append pid and Pupil Baseline to the pupilBaseline File
    cat(paste0(studyId,",",computePupilBaseline(dat,d_seg),"\n"),
        file=PUPIL_BASELINES_FILE,append=TRUE)
  
    ##find timestamp offset for human-readable bips
    if (!("Screen Recordings (1)" %in% unique(dat$MediaName))){
       cat (paste0(f,"Don't have \"Screen Recordings (1)\", cannot compute offset for bips \n"),
                                                                     file = PREPROCESS_FILE,append=TRUE)
      offset <- 0
    } else offset <- dat$RecordingTimestamp[which(dat$MediaName == "Screen Recordings (1)")[1]]
    
    # Output timestamps for CookieTheft, Reading, and Memory
    writeBips (BIPS_FILE, studyId,dat,d_seg, "PupilCalib",offset)
    writeBips (BIPS_FILE, studyId,dat,d_seg, "CookieTheft",offset)
    writeBips (BIPS_FILE, studyId,dat,d_seg, "Reading",offset)
    writeBips (BIPS_FILE, studyId,dat,d_seg, "Memory",offset)

    #copy file to PREPROCESS_DIR_DATA
    file.copy(FILEPATH, PREPROCESS_DIR_DATA)
    
    #update amount of processedPIDs
    processedPIDS <- processedPIDS+1
    
    print (paste0(f," processed correctly"))
  
  }
  
  #log processedPIDS, pidsNotFound, pidsWrongEvents
  cat (paste0("Number of StudyIds processed correctly: ",processedPIDS,"\n",
               "Number of StudyIds with wrong number of events: ",length(pidsWrongEvents),"\n",
               "Number of StudyIds from Participant log without corresponding Tobii export file: ",length(pidsNotFound),"\n"),
        file = PREPROCESS_FILE,append=TRUE)
  if (length(pidsNotFound) > 0) {
    cat (paste0("StudyIds from Participant log without corresponding Tobii export file: ",paste0(pidsNotFound,collapse = ","),"\n"),
         file = PREPROCESS_FILE,append=TRUE)
  }
}


convertPartLogToCSV <- function () {
  ##Read ParticipantLog
  p_log <- read_excel(paste0(MAIN_DIR,"CANARY_Participant_Log.xlsx"), sheet = "Participant_Log")
  #write p_log sheet1 as csv in preprocess folder
  # write.csv(x = p_log,file = paste0(PREPROCESS_DIR,"CANARY_Participant_Log.csv"),quote=FALSE,row.names = FALSE)
  write.table(x = p_log,file = paste0(PREPROCESS_DIR,"CANARY_Participant_Log.csv"),sep = "\t",dec = ".",quote=FALSE,na = "",row.names = FALSE)
}


############
###########
############
########### MAIN


#partent directory where to organize data
MAIN_DIR <<- "/Users/obarral/Documents/CANARY/Data/PRE-LOCKDOWN-ALL-DATA/"
##Folder with The tobii exports
TOBII_EXPORT_DIR <<- paste0(MAIN_DIR,"Tobii Export 29Apr2020/")
#Output folder for auxiliary filesto be saved
PREPROCESS_DIR <<- paste0(MAIN_DIR,"Preprocessing/")
#Output folder for renamed files to be saved
PREPROCESS_DIR_DATA <<- paste0(PREPROCESS_DIR,"Eye_Raw/")
#output file for logs, pupil baseline, and bip (timestamps) files
PREPROCESS_FILE <<-  paste0(PREPROCESS_DIR,"Preprocess_log.txt")
PUPIL_BASELINES_FILE <<- paste0(PREPROCESS_DIR,"PupilBaselines/pupil_baselines.csv")
BIPS_FILE <<-paste0(PREPROCESS_DIR,"BipsTimestamps/TasksTimestamps.csv")




##process all segment files, write bip times, and generate pupil baselines
processSegments()

#aggregate all pupil baselines in one file
generatePupilFile()

##generate AOIs for Cookie Theft
generateAOIFiles("CookieTheft")


#convertParticipantLog to CSV
convertPartLogToCSV()



