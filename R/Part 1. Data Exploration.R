library(ggpubr)
library(stringr)
library(tuneR)
library(apcluster)


# Data preparation --------------------------------------------------------
SelectionTablesTrainingPath <- '/Users/denaclink/Downloads/Data_test_Argus/TrainingSelections/'
WavsTrainingPath <- '/Users/denaclink/Downloads/Data_test_Argus/TrainingWavs'

output.dir.train.signal <- 'data/train/crestedargus/'
dir.create(output.dir.train.signal,recursive = T)

output.dir.train.noise <- 'data/train/noise/'
dir.create(output.dir.train.noise,recursive = T)

class <- 'crestedargus'

# Data exploration using selection tables ---------------------------------
AllFiles <- '/Users/denaclink/Downloads/Data_test_Argus'

# Read in files
AllFileslist <- list.files('/Users/denaclink/Downloads/Data_test_Argus',recursive = TRUE,
                           pattern = '.txt', full.names = TRUE)

# Combine into list
AllFilesdf <- lapply( AllFileslist, read.delim, header = T, sep ="\t")

# Combine into dataframe
AllFilesdfcombined <- do.call(rbind, AllFilesdf )

# How many detections are there?
nrow(AllFilesdfcombined)

# Explore Raven Selection tables
SelectionTablesTraining <- list.files(SelectionTablesTrainingPath,
           full.names = T)

# Read the files in
SelectionTablesTrainingdf <- lapply(SelectionTablesTraining, read.delim, header = T, sep ="\t")

# Combine them
SelectionTablesTrainingcombined <- do.call(rbind, SelectionTablesTrainingdf)

# Check structure
colnames(SelectionTablesTrainingcombined)

# Add class column
SelectionTablesTrainingcombined$class <- class

# Add duration column
SelectionTablesTrainingcombined$duration <- SelectionTablesTrainingcombined$End.Time..s. - SelectionTablesTrainingcombined$Begin.Time..s.

# Check range of low frequency
range(SelectionTablesTrainingcombined$Low.Freq..Hz.)

# Check range of high frequency
range(SelectionTablesTrainingcombined$High.Freq..Hz.)

# Check duration
range(SelectionTablesTrainingcombined$duration)



# Create training dataset -------------------------------------------------
WavsTraining <- list.files(WavsTrainingPath,
                                      full.names = T)

WavsTrainingBase <- basename(WavsTraining)

WavNameShort <- str_split_fixed(WavsTrainingBase,pattern = '.wav', n=2)[,1]


for(a in 1:length(WavNameShort)) {

 SelectionIndex <- which(str_detect(SelectionTablesTraining,WavNameShort[a]))

 if(length(SelectionIndex) >0){
 TempSelectionTable <- read.delim(SelectionTablesTraining[SelectionIndex])

 TempSelectionTable$Begin.Time..s. <- round(TempSelectionTable$Begin.Time..s.,0)

 TempSelectionTable$End.Time..s. <- round(TempSelectionTable$End.Time..s.,0)

 TempWav <- readWave(WavsTraining[a])

 # Create clips for signal
 Signals <- lapply(1:nrow(TempSelectionTable),
                             function(i)
                               extractWave(
                                 TempWav,
                                 from = TempSelectionTable$Begin.Time..s.[i]-1.5,
                                 to = TempSelectionTable$End.Time..s.[i]+1.5,
                                 xunit = c("time"),
                                 plot = F,
                                 output = "Wave"
                               ))


lapply(1:length(Signals),
                   function(i)
                     writeWave(
                       Signals[[i]],
                       filename = paste(
                         output.dir.train.signal,
                         class, '_',
                         WavNameShort[a],i,'.wav',sep=''
                     ),
                     extensible = F
                     ))


 # Create clips for noise (assuming exhaustive annotations)

if(nrow(TempSelectionTable) > 1){
NoiseClips <- lapply(1: (nrow(TempSelectionTable)-1),
                  function(i)
                    extractWave(
                      TempWav,
                      from = TempSelectionTable$End.Time..s.[i],
                      to = TempSelectionTable$Begin.Time..s.[i +1],
                      xunit = c("time"),
                      plot = F,
                      output = "Wave"
                    ))
}

if(nrow(TempSelectionTable) == 1){
  NoiseClips <- extractWave(
                           TempWav,
                           from = max(TempSelectionTable$End.Time..s.),
                           to = duration(TempWav),
                           xunit = c("time"),
                           plot = F,
                           output = "Wave"
                         )

  NoiseClips <- list(NoiseClips)
}




for(b in 1:length(NoiseClips)){

TempWavPath <- paste(tempdir(),'/',WavNameShort[a],'_TempWav.wav',sep='')

if(duration(NoiseClips[[b]]) > 150){
writeWave(NoiseClips[[b]],TempWavPath)

DetectBLED(input=TempWavPath,
           min.freq = 500,
           max.freq = 1800,
           noise.quantile.val=0.15,
           spectrogram.window =512,
           pattern.split = ".wav",
           min.signal.dur = 3,
           max.sound.event.dur = 12,
           wav.output = "TRUE",
           output.dir = output.dir.train.noise,
           swift.time=FALSE,
           #time.start=06,
           #time.stop=11,
           write.table.output=FALSE,
           verbose=TRUE,
           random.sample=FALSE)
}

}
}

}


# Visualize results -------------------------------------------------------

gibbonID(input.dir='data/train/',output.dir="data/thumbnails/",
                              q.fixed=0.1,class='affinity.adaptive',
                                add.spectrograms=TRUE,min.freq=500,max.freq=1800)


# IMPORTANT: Manually verify the clips to make sure they are in the right folder --------
# Note, there are some very LQ samples but we will move forward anyways
# If model performance is low, then perhaps will want to remove them


# Use BLED detector over all ----------------------------------------------
# New note: Performance was very poor so trying different approach
ListTrainingWavs <- list.files(WavsTrainingPath, full.names = TRUE)

for(a in 1:length(ListTrainingWavs)) {

DetectBLED(input=ListTrainingWavs[[a]],
           min.freq = 500,
           max.freq = 1800,
           noise.quantile.val=0.15,
           spectrogram.window =512,
           pattern.split = ".wav",
           min.signal.dur = 3,
           max.sound.event.dur = 12,
           wav.output = "TRUE",
           output.dir = 'data/bled_detector/noise/',
           swift.time=FALSE,
           #time.start=06,
           #time.stop=11,
           write.table.output=FALSE,
           verbose=TRUE,
           random.sample=FALSE)

}


# Create spectrograms from BLED detector ----------------------------------
devtools::load_all('/Users/denaclink/Desktop/RStudioProjects/gibbonNetR')

# Create images
spectrogram_images(
  trainingBasePath='data/bled_detector/',
  outputBasePath='data/bled_detector_images/',
  splits=c(1,0,0),
  random = "FALSE",
  minfreq.khz = 0.5,
  maxfreq.khz = 1.8,
  new.sampleratehz = 'NA'
)

# Match manually sorted images to soundfiles
# NOTE that need to add a new 'crestedargus' folder AND change name from 'noise' to 'crestedargus' for positive samples
ImageList <- list.files('data/bled_detector_images/train/',
                        full.names = T,recursive = T)

ImageListShort <- basename(ImageList)

WavFileList1 <- list.files('data/bled_detector/',
                           full.names = T,recursive = T)

OutPutDir <- 'data/train_hq'

for(i in 1:length(ImageListShort)){

  TempNamefull <- str_split_fixed (basename(ImageListShort[i]), pattern='.jpg', n=2)[,1]
  Class <- str_split_fixed(TempName,pattern = '_',n=2)[,1]
  TempName <- str_split_fixed(TempNamefull,pattern = '_',n=2)[,2]

  WavFileListTemp <- WavFileList1[which(str_detect(WavFileList1,TempName))]

  TempDir <- paste(OutPutDir,str_split_fixed(ImageListShort[i],pattern = '_',n=2)[,1],sep='/')

  dir.create(TempDir, recursive = T)

  OutPutFile <- paste(TempDir, '/', TempNamefull,'.wav',sep='')

  file.copy(from=WavFileListTemp,
            to=OutPutFile)

}

