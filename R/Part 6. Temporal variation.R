library(stringr)
library(ggpubr)
library(cowplot)

# Read in data
ArgusDF <- read.csv('/Users/denaclink/Downloads/Data_test_Argus/Argus_data2024_Updated (1).csv')

# Calculate number of observations
sum(na.omit(ArgusDF$n.detection))

# Create hour column
ArgusDF$hour <- str_split_fixed(ArgusDF$date, pattern = '_', n=2)[,2]

# Check structure
head(ArgusDF)

range(ArgusDF$date)

# Convert to numeric
ArgusDF$hour <- as.numeric(ArgusDF$hour )

TotalHours <- as.data.frame(table(ArgusDF$hour ))
colnames(TotalHours) <- c('hour','sum')

ArgusDFDetectOnly <- subset(ArgusDF, n.detection > 0)

DetectionByHour <- table(ArgusDFDetectOnly$n.detection,ArgusDFDetectOnly$hour)
DetectionByHourSum <- as.data.frame(colSums(DetectionByHour))
DetectionByHourSum$hour <- rownames(DetectionByHourSum)
colnames(DetectionByHourSum) <- c('sum','hour')

AdjustedHourDetections <- data.frame()

for(a in 1:nrow(DetectionByHourSum)){
 tempRow <-  DetectionByHourSum[a,]
 TempTotal <- subset(TotalHours,hour==tempRow$hour)
 tempRow$hour_adj <- tempRow$sum /TempTotal$sum
 AdjustedHourDetections <- rbind.data.frame(AdjustedHourDetections,tempRow)
 }

AdjustedHourDetections$hour <- str_pad(AdjustedHourDetections$hour, 2, pad = "0")
AdjustedHourDetections$hour <- paste(AdjustedHourDetections$hour,':00',sep='')

AllCallsBar <- ggbarplot(data=AdjustedHourDetections, x='hour',y="hour_adj",fill='lightgrey')+xlab('Local time')+ylab('Call rate \n (# calls per hour)')



# Color by call type ------------------------------------------------------
ArgusDFDetectOnly <- subset(ArgusDF, n.detection > 0 )

DetectionByHour <- table(ArgusDFDetectOnly$call.type,ArgusDFDetectOnly$hour)
DetectionByHourSum <- as.data.frame(t(DetectionByHour))
colnames(DetectionByHourSum) <- c('hour','call.type', 'sum')
DetectionByHourSum$hour <- as.character(DetectionByHourSum$hour)
TotalHours$hour <- as.character(TotalHours$hour)

AdjustedHourDetectionsCallType <- data.frame()

for(a in 1:nrow(DetectionByHourSum)){
  tempRow <-  DetectionByHourSum[a,]
  TempTotal <- subset(TotalHours,hour==tempRow$hour)
  tempRow$hour_adj <- tempRow$sum /TempTotal$sum
  AdjustedHourDetectionsCallType <- rbind.data.frame(AdjustedHourDetectionsCallType,tempRow)
}

AdjustedHourDetectionsCallType$hour <- str_pad(AdjustedHourDetectionsCallType$hour, 2, pad = "0")
AdjustedHourDetectionsCallType$hour <- paste(AdjustedHourDetectionsCallType$hour,':00',sep='')
levels(AdjustedHourDetectionsCallType$call.type) <- c('Long call', "Short call")

ByCallTypeBar <- ggbarplot(data=AdjustedHourDetectionsCallType, x='hour',y="hour_adj",
          fill = 'call.type',position = position_dodge(0.9))+
            xlab('Local time')+ylab('Call rate \n (# calls per hour)')+
              scale_fill_manual(values = c('lightyellow','lightblue')) +
                theme(legend.title=element_blank())

cowplot::plot_grid(AllCallsBar,ByCallTypeBar, nrow=2, labels=c('A','B'), label_x = 0.9)
