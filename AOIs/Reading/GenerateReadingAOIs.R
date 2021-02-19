#######
######
#THIS  SCRIPT WAS USED TO GENERATE THE AOIS FOR THE READING TASK.
#ONE MAY WANT TO USE THIS CODE IF AT SOME POINT CANARY ADOPTS A DIFFERENT READING TASK 
#AND ONE WANTS TO GENERATE BOUNDINGBOXES AROUND THE WORDS BASED ON THE PIXEL COLOUR
#######
########





library(dplyr)
library(ggimage)
library(ggplot2)
library(imager)





#find end and start positions of words or lines
findSpaces <- function (imm, SIZESPACE=10,thresholdblack = .9,FINDLINES=FALSE) {
  if (FINDLINES) {
    sumcols <- apply(imm,2,sum) 
  }else sumcols <- apply(imm,1,sum)   
  runs = rle(sumcols <= thresholdblack)
  myruns = which(runs$values == TRUE & runs$lengths > SIZESPACE)
  
  runs.lengths.cumsum = cumsum(runs$lengths)
  ends = runs.lengths.cumsum[myruns]
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
  # starts <- starts [-1]
  # ends <- ends [-1]
  # starts <- starts [-length(starts)]
  # ends <- ends [-length(ends)]
  return(data.frame("start" = ends[-length(ends)], "end" = starts[-1]))
}



# ######Figure out AOIS
# 
image_file = "/Users/obarral/Documents/CANARY/AOIs/Reading/Reading.JPG"
##load image in greyscale
im <- load.image(image_file) %>% grayscale
##load basic AOI info
d <- read.csv("/Users/obarral/Documents/CANARY/AOIs/Reading/Reading_AOIs_basicInfo.txt",stringsAsFactors = F)

#initialize new fields
d[,c("tl_x","tl_y","tr_x","tr_y","bl_x","bl_y","br_x","br_y")] <- NA


####Find lines
linesStartEnd <- findSpaces(im, SIZESPACE=30,thresholdblack = 10,FINDLINES=TRUE)

# Set safety margin
FIXEDMARGIN <- 22
linesmargins <- (linesStartEnd$start[-1]-linesStartEnd$end[-nrow(linesStartEnd)])/2


##set first and last lines
d[d$line_id==1,c("tl_y","tr_y")] <- (linesStartEnd$start[1]-FIXEDMARGIN)
d[d$line_id == 1,c("bl_y","br_y")] <-(linesStartEnd$end[1]+linesmargins[1])

d[d$line_id==nrow(linesStartEnd),c("bl_y","br_y")] <- (linesStartEnd$end[nrow(linesStartEnd)]+FIXEDMARGIN)
d[d$line_id == nrow(linesStartEnd),c("tl_y","tr_y")] <- (linesStartEnd$start[nrow(linesStartEnd)]-linesmargins[nrow(linesStartEnd)-1])


##set the rest
for (i in 2:(nrow(linesStartEnd)-1)) {
  d[d$line_id == i,c("bl_y","br_y")] <- linesStartEnd$end[i]+linesmargins[i]
  d[d$line_id == i,c("tl_y","tr_y")] <- linesStartEnd$start[i]-linesmargins[i]
}
# d[1:(nrow(linesStartEnd)-1),c("bl_y","br_y")] <- linesStartEnd$end[1:(nrow(linesStartEnd)-1)]+linesmargins
# d[2:nrow(linesStartEnd),c("tl_y","tr_y")] <- linesStartEnd$start[2:nrow(linesStartEnd)]-linesmargins


#Set coordinates according to lines found
for (lineID in unique(d$line_id)) {
  
  
###set X positions
  #select the specific line
  imm <- im[,linesStartEnd$start[lineID]:linesStartEnd$end[lineID]]
  # as.cimg(imm) %>% plot
  
  ##find start and end of words
  wordStartEnd <- findSpaces(imm,10,.9)

  ##set special cases for first and last words of lines
  d[d$line_id == lineID,][1,c("tl_x","bl_x")] <- wordStartEnd$start[1]-FIXEDMARGIN
  d[d$line_id == lineID,][nrow(wordStartEnd),c("tr_x","br_x")] <- wordStartEnd$end[nrow(wordStartEnd)]+FIXEDMARGIN
  
  ##set the rest
  spaces <- (wordStartEnd$start[-1]-wordStartEnd$end[-nrow(wordStartEnd)])/2
  d[d$line_id == lineID,][1:(nrow(wordStartEnd)-1),c("tr_x","br_x")] <- wordStartEnd$end[1:(nrow(wordStartEnd)-1)] + spaces  
  d[d$line_id == lineID,][2:nrow(wordStartEnd),c("tl_x","bl_x")] <- wordStartEnd$start[2:nrow(wordStartEnd)] - spaces  
  
}

img <- readJPEG(image_file)
g <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = FALSE)



###plot aois
ggplot() +
  annotation_custom(g, -Inf, Inf, -Inf, Inf) +
  scale_y_reverse(expand = c(0,0), limits=c(1050,0)) +
  scale_x_continuous(expand = c(0,0), limits=c(0,1680)) +
  theme(plot.margin = unit(c(0,0,0,0), "mm"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks.length = unit(0,'mm'),
        axis.text.y = element_blank()) +
  # geom_rect(data=d,aes(xmin=bl_x,xmax=tr_x,ymin=bl_y,ymax=tr_y),color="red",alpha =0)
  geom_point(aes(x=1120,y=402,size=2))
  # geom_point(data=d,aes(x=V1,y=V2))

# geom_vline(xintercept = aux$start,color="red") +
# geom_vline(xintercept = aux$end,color="grey")
  # geom_hline(yintercept = aux$start,color="red") +
  # geom_hline(yintercept = aux$end,color="grey")

ggsave("/Users/obarral/Documents/CANARY/AOIs/Reading/Reading_AOIs.png", height=1050/100, width = 1680/100, units = 'in', dpi = 100)

write.csv(d,"/Users/obarral/Documents/CANARY/AOIs/Reading/Reading_AOIs.txt",row.names = F)
