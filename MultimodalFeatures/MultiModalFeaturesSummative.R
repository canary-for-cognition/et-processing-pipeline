library(dplyr)
library(ggplot2)
library(Rmisc)
library(reshape2)

#vizualize features per group
VizFeature <- function(d,featname) {
  return (ggplot(d, aes_string(x="Label", y = featname, colour="Label", group="Label")) + 
            geom_boxplot()+
            # facet_wrap(.~InfoUnit)+
            # theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5)) +
            guides(colour = FALSE)
          # labs(y = "First mention then look", x="Group")
  )
}


computeFeatures <- function(d,grouping) {
  return ( d %>% # the names of the new data frame and the data frame to be summarised
             group_by_at(grouping) %>%   # the grouping variable
             dplyr::summarise(mean_Latency = mean(Latency,na.rm=T),  # calculates the mean of each group
                              med_Latency = median(Latency,na.rm=T), #calculates median of each group
                              sd_Latency = sd(Latency), # calculates the standard deviation of each group
                              mean_Latency_f = mean(FrontLatency,na.rm = T),  # calculates the mean of each group
                              med_Latency_f = median(FrontLatency,na.rm=T), #calculates median of each group
                              sd_Latency_f = sd(FrontLatency), # calculates the standard deviation of each group
                              mean_Latency_b = mean(BackLatency,na.rm=T),  # calculates the mean of each group
                              med_Latency_b = median(BackLatency,na.rm=T), #calculates median of each group
                              sd_Latency_b = sd(BackLatency), # calculates the standard deviation of each group
                              prop_dir_f = sum(Direction=="F",na.rm=T)/sum(!is.na(Direction)), #percentage of forward latencies
                              # prop_dir_b = sum(Direction=="B",na.rm=T)/sum(!is.na(Direction)), #percentage of backward latencies
                              # prop_dir = sum(DirectionForward,na.rm=T)/sum(DirectionForward==F,na.rm=T),
                              #compute how many times an infounit was gazed upon while it was being mentioned
                              fallinAOI = sum(apply(data.frame(.data[["InfoUnits"]],.data[["HITS_0"]],stringsAsFactors = F),1,function(x) {
                                aux <- strsplit(x[[2]],"\'") %>% `[[`(1)
                                aux <- aux[!(aux %in% c("[","]",","))]
                                x[[1]] %in% aux 
                              })),
                              Label = unique(Label))
  )
}


#read features computed for each infounit mentioned for each user
d <- read.csv("/Users/obarral/Documents/CANARY/Diego/multimodal/MultimodalFeatures.csv",stringsAsFactors = F)
d <- d[-1]

#add participant labels to dataframe (just in case we want to visualize the computed features, this is not used to inform the features of course)
d$Label <- sapply(d$Patient,function(x){
  substring(x,1,1)
})
d$Label[d$Label=="H"]<-"HC"
d$Label[d$Label=="E"] <- "P"

###For each latency identify whether it'sa front or backwards latency (the smallest of the two)

##convert missing latencies to NA
d$FrontLatency[d$FrontLatency==-1] <- NA
d$BackLatency[d$BackLatency==-1] <- NA

#select the minimum of the two latencies as the one of interest
d$Latency <- pmin(d$BackLatency,d$FrontLatency,na.rm = T)

#calculate range within 95% CI for these latencies fall for the current dataset cutoff ~ 3.7seconds (makes sense)
cutoff <- boxplot.stats(d$Latency)$stats[5]

#only keep latencies within 95% conf. interval of the median. 
#This step is done so that we get rid of latencies that were computed between mentions and gazes that were on same AOI/Infonuit but are not related
d$Latency[d$Latency > cutoff] <- NA
d$FrontLatency[d$FrontLatency > cutoff] <- NA
d$BackLatency[d$BackLatency > cutoff] <- NA

#create new feature that indicates whether the computed latency is a fron or backwards latency
d$Direction[d$Latency == d$BackLatency] <- "B"
d$Direction[d$Latency == d$FrontLatency] <- "F"
##

#compute all features across all AOIs
feats <- computeFeatures(d,"Patient")


#compute all features for each AOI
aux <- computeFeatures(d,c("Patient","InfoUnits"))

#convert to wide format
aux <- dcast(melt(aux, id.vars=c("Patient", "InfoUnits")), Patient~variable+InfoUnits)
#merge with dataframe
feats <- merge(feats,aux,by="Patient")
#Convert Nas back to -1
feats[is.na(feats)] <- -1
feats[feats=="NaN"] <- -1
write.csv(feats,"/Users/obarral/Documents/CANARY/Data/PRE-LOCKDOWN-ALL-DATA/MULTIMODAL/multimodalFeaturesSummative.csv",row.names = F)


#####Code below in case we want to visualize some of the computed features. 
#NO NEED TO RUN IF ONLY WISHING TO GENERATE FEATURES
VizFeature(feats,"mean_Latency")
VizFeature(feats,"med_Latency")
VizFeature(feats,"sd_Latency")
VizFeature(feats,"prop_dir_f")

VizFeature(feats,"mean_Latency_f")
VizFeature(feats,"med_Latency_f")
VizFeature(feats,"mean_Latency_b")
VizFeature(feats,"med_Latency_b")

VizFeature(feats,"fallinAOI")

