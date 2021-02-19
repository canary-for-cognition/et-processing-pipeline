library(dplyr)
library(ggplot2)
library(Rmisc)
library(reshape2)



computeFeatures <- function(d,grouping) {
  return ( d %>% # the names of the new data frame and the data frame to be summarised
             group_by_at(grouping) %>%   # the grouping variable
             dplyr::summarise(mean_Sacc_Ampl = mean(Length[Direction == "S"],na.rm=T), 
                              sd_Sacc_Ampl = sd(Length[Direction == "S"],na.rm=T), 
                              mean_Sacc_Dist = mean(Length_Words[Direction == "S"],na.rm=T), 
                              sd_Sacc_Dist = sd(Length_Words[Direction == "S"],na.rm=T), 
                              mean_Regr_Ampl = mean(Length[Direction == "R"],na.rm=T), 
                              sd_Regr_Ampl = sd(Length[Direction == "R"],na.rm=T), 
                              mean_Regr_Dist = mean(abs(Length_Words[Direction == "R"]),na.rm=T), 
                              sd_Regr_Dist = sd(abs(Length_Words[Direction == "R"]),na.rm=T), 
                              max_Regr_Ampl = max(Length[Direction == "R"],na.rm=T), 
                              max_Regr_Dist = max(abs(Length_Words[Direction == "R"]),na.rm=T),
                              count_Fix = sum(!is.na(StartFixation)),
                              count_FirstPass_FirstFix = sum(FirstOccurence & FirstPass),
                              count_LaterPass_FirstFix = sum(FirstOccurence & !FirstPass),
                              count_MultiFix = sum(!FirstOccurence & FirstPass),
                              count_ReFix = sum(!FirstOccurence & !FirstPass),
                              # count_ReadingFixEnd = sum(Word_ID_End == 155,na.rm=T),
                              # count_ReadingFixStart = sum(Word_ID_Start == 155,na.rm=T),
                              # max_ReadingFixEnd = max(Word_ID_End,na.rm=T),
                              # max_ReadingFixStart = max(Word_ID_Start,na.rm=T),
                              mean_FirstPass_FirstFix_Duration = mean(StartFixationDuration[FirstOccurence & FirstPass],na.rm=T),
                              sd_FirstPass_FirstFix_Duration = sd(StartFixationDuration[FirstOccurence & FirstPass],na.rm=T),
                              mean_Wrapup_Duration = mean(StartFixationDuration[is_last_word],na.rm=T),
                              sd_Wrapup_Duration = sd(StartFixationDuration[is_last_word],na.rm=T),
                              ratio_Wrapup = mean(StartFixationDuration[is_last_word],na.rm=T)/mean(StartFixationDuration[!is_last_word],na.rm=T),
                              mean_Wrapup_Line_Duration = mean(StartFixationDuration[is_last_line_word],na.rm=T),
                              sd_Wrapup_Line_Duration = sd(StartFixationDuration[is_last_line_word],na.rm=T),
                              ratio_Wrapup_Line = mean(StartFixationDuration[is_last_line_word],na.rm=T)/mean(StartFixationDuration[!is_last_line_word],na.rm=T))#,
                              # count_ReReadingFix = ,
                              # Label = unique(Label))
  )
}



d <- read.csv("/Users/obarral/Documents/CANARY/Diego/Reading/FINAL/ReadingFeaturesExtras.csv",stringsAsFactors = F)
d <- d[-1]

#convert FristOccurence and FirstPass to Logic
d$FirstOccurence <- d$FirstOccurence =="True"
d$FirstPass <- d$FirstPass=="True"

# #add label
# d$Label <- sapply(d$Patient,function(x){
#   substring(x,1,1)
# })
# d$Label[d$Label=="H"]<-"HC"
# d$Label[d$Label=="E"] <- "P"

aois <-read.csv("/Users/obarral/Documents/CANARY/AOIs/Reading/Reading_AOIs.csv",stringsAsFactors = F)
#add info whether word is last word in sentence
d$is_last_word <- FALSE
d$is_last_word[d$Word_ID_Start %in% aois$word_id[which(diff(aois$sentence_id)==1)]] <- TRUE

#add info whether word is last word in line
d$is_last_line_word <- FALSE
d$is_last_line_word[d$Word_ID_Start %in% aois$word_id[which(diff(aois$line_id)==1)]] <- TRUE

# #add info whether word is processed before the whole text has been read
# d$re_reading <- TRUE


#compute features
feats <- computeFeatures(d,"PatientName")
feats <- feats %>% replace(is.na(.), -1)

write.csv(feats,"/Users/obarral/Documents/CANARY/Data/PRE-LOCKDOWN-ALL-DATA/Reading/readingFeatures.csv",row.names = F)
