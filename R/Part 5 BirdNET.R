# Load necessary libraries
library(stringr)  # For string manipulation
library(caret)    # For machine learning and model evaluation
library(ggpubr)   # For data visualization
library(dplyr)    # For data manipulation
library(data.table) # For sorting the detections
library(ggplot2)
library(ROCR)


# NOTE you need to change the file paths below to where your files are located on your computer

#  Performance Binary --------------------------------------------------------

# Get a list of TopModel result files
TopModelresults <- list.files('data/birdnet/allsamples',
                              full.names = TRUE)

# Get a list of annotation selection table files
TestDataSet <- list.files('/Users/denaclink/Downloads/Data_test_Argus/ValidationSelections',
                          full.names = TRUE)

start.time.buffer <- 3
end.time.buffer <- 3

# Preallocate space for TopModelDetectionDF
TopModelDetectionDF <- data.frame()

Seq <- seq(1:length(TopModelresults))

# Loop through each TopModel result file
for (a in Seq) {

  # Read the TopModel result table into a data frame
  TempTopModelTable <- read.delim2(TopModelresults[a])

  TempTopModelTable <- TempTopModelTable[,-c(4,5)]

  TempTopModelTable <- subset(TempTopModelTable,Common.Name=='crestedargus')

  # Extract the short name of the TopModel result file
  ShortName <- basename(TopModelresults[a])
  ShortName <- str_split_fixed(ShortName, pattern = '.BirdNET', n = 2)[, 1]

  # Find the corresponding annotation selection table
  testDataIndex <- which(str_detect(TestDataSet, ShortName))

  if(length(testDataIndex) > 0){
  TestDataTable <- read.delim2(TestDataSet[testDataIndex])

  TestDataTable$Class <- 'crestedargus'
  # Round Begin.Time..s. and End.Time..s. columns to numeric
  TestDataTable$Begin.Time..s. <- round(as.numeric(TestDataTable$Begin.Time..s.))
  TestDataTable$End.Time..s. <- round(as.numeric(TestDataTable$End.Time..s.))

  DetectionList <- list()
  # Loop through each row in TempTopModelTable
  for (c in 1:nrow(TempTopModelTable)) {
    TempRow <- TempTopModelTable[c,]


    # Check if Begin.Time..s. is not NA
    if (!is.na(TempRow$Begin.Time..s.)) {
      # Convert Begin.Time..s. and End.Time..s. to numeric
      TempRow$Begin.Time..s. <- as.numeric(TempRow$Begin.Time..s.)
      TempRow$End.Time..s. <- as.numeric(TempRow$End.Time..s.)

      # Determine if the time of the detection is within the time range of an annotation
      TimeBetween <- data.table::between(TempRow$Begin.Time..s.,
                                         TestDataTable$Begin.Time..s. - start.time.buffer,
                                         TestDataTable$End.Time..s. + end.time.buffer)


      # Extract the detections matching the time range
      matched_detections <- TestDataTable[TimeBetween, ]

      if (nrow(matched_detections) > 0) {
        # Set Class based on the Call.Type in matched_detections
        TempRow$Class <- 'crestedargus'
        DetectionList[[length( unlist(DetectionList))+1]] <-  which(TimeBetween == TRUE)
      } else {
        # Set Class to 'noise' if no corresponding annotation is found
        TempRow$Class <- 'noise'
      }

      # Append TempRow to TopModelDetectionDF
      TopModelDetectionDF <- rbind.data.frame(TopModelDetectionDF, TempRow)
    }
  }

  # Identify missed detections


  if (length( unlist(DetectionList)) > 0 &  length( unlist(DetectionList)) < nrow(TestDataTable) ) {

    missed_detections <- TestDataTable[-unlist(DetectionList), ]
    # Prepare missed detections data
    missed_detections <- missed_detections[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")]
    #missed_detections$Detections <- ShortName
    missed_detections$Confidence <- 0
    missed_detections$Species.Code <- 'crestedargus'
    missed_detections$Common.Name <- 'crestedargus'

    missed_detections$Class <- 'crestedargus'

    # Append missed detections to TopModelDetectionDF
    TopModelDetectionDF <- rbind.data.frame(TopModelDetectionDF, missed_detections)
  }

  if (length( unlist(DetectionList)) == 0) {

    missed_detections <- TestDataTable
    # Prepare missed detections data
    missed_detections <- missed_detections[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")]
    missed_detections$Confidence <- 0
    #missed_detections$Detections <- ShortName
    missed_detections$Species.Code <- 'crestedargus'
    missed_detections$Common.Name <- 'crestedargus'

    missed_detections$Class <- 'crestedargus'

    # Append missed detections to TopModelDetectionDF
    TopModelDetectionDF <- rbind.data.frame(TopModelDetectionDF, missed_detections)

  }

}
}

head(TopModelDetectionDF)
nrow(TopModelDetectionDF)
TopModelDetectionDF$Class <- as.factor(TopModelDetectionDF$Class)

table(TopModelDetectionDF$Class)

subset(TopModelDetectionDF,Class=='crestedargus')

# Convert Class column to a factor variable
TopModelDetectionDF$Class <- as.factor(TopModelDetectionDF$Class)

# Display unique values in the Class column
unique(TopModelDetectionDF$Class)

# Define a vector of confidence Thresholds
Thresholds <-seq(0.1,0.9,0.1)
Thresholds <- c(Thresholds,seq(.9,1,.01))

# Create an empty data frame to store results
BestF1data.framecrestedargusBinary <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-TopModelDetectionDF

  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single$Confidence  <=Thresholds[a], 'noise','crestedargus')

    # Calculate confusion matrix using caret package
    caretConf <- caret::confusionMatrix(
      as.factor(TopModelDetectionDF_single$PredictedClass),
      as.factor(TopModelDetectionDF_single$Class),positive = 'crestedargus',
      mode = 'everything')


    # Extract F1 score, Precision, and Recall from the confusion matrix
    F1 <- caretConf$byClass[7]
    Precision <- caretConf$byClass[5]
    Recall <- caretConf$byClass[6]
    FP <- caretConf$table[1,2]
    TN <- sum(caretConf$table[2,])#+JahooAdj
    FPR <-  FP / (FP + TN)
    # Create a row for the result and add it to the BestF1data.frameGreyGibbon
    #TrainingData <- training_data_type
    TempF1Row <- cbind.data.frame(F1, Precision, Recall,FPR)
    TempF1Row$Thresholds <- Thresholds[a]
    BestF1data.framecrestedargusBinary <- rbind.data.frame(BestF1data.framecrestedargusBinary, TempF1Row)
  }

BestF1data.framecrestedargusBinary

crestedargusMax <- round(max(na.omit(BestF1data.framecrestedargusBinary$F1)),2)

# Metric plot
crestedargusBinaryPlot <- ggplot(data = BestF1data.framecrestedargusBinary, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested argus (binary) \n max F1:",crestedargusMax),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")

crestedargusBinaryPlot
