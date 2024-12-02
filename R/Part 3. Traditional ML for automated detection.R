library(gibbonR)

# Set location of validation files
ValidationFiles <- '/Users/denaclink/Downloads/Data_test_Argus/ValidationWavs'

# List training data folders

TrainingFolders <- 'data/train_hq/'

min.freq <- 500
max.freq <- 1800
target.signal <- 'crestedargus'

# Automated detection and classification all training data ----------------------------------
trainingdata <- MFCCFunction(input.dir= 'data/train_hq/', min.freq = min.freq, max.freq = max.freq)

trainingdata$class <- as.factor(trainingdata$class)


output.dir <- 'data/results_tradML/allsamples_hq'
dir.create(output.dir,recursive = TRUE)

feature.df <- trainingdata

gibbonR(input=ValidationFiles,
        input.type = 'directory',
        feature.df=feature.df,
        model.type.list=c('SVM'),
        tune = FALSE, # Use default
        short.wav.duration=300,
        target.signal = target.signal,
        min.freq = min.freq, max.freq = max.freq,
        noise.quantile.val=0.15,
        minimum.separation =3,
        n.windows = 9, num.cep = 12,
        spectrogram.window =160,
        pattern.split = ".wav",
        min.signal.dur = 1,
        max.sound.event.dur = 12,
        maximum.separation =1,
        probability.thresh.svm =0,
        probability.thresh.rf = 0,
        wav.output = "FALSE",
        output.dir =output.dir,
        swift.time=FALSE,time.start=5,time.stop=10,
        write.table.output=TRUE,verbose=TRUE,
        random.sample='NA')
