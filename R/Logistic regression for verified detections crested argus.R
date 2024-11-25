library(stringr)
library(ggpubr)
library(bbmle)

# DENA NOTE - need to make a multiclass model version

SortedDetectionFolders <- list.files('/Users/denaclink/Desktop/RStudioProjects/Crested-argus/segmentsmixup_test/',full.names = TRUE)

#par(mfrow=c(3,2))

for(a in 1:length(SortedDetectionFolders)){
FullFolders <- list.files(SortedDetectionFolders[a],full.names = TRUE)
TruePositives <- list.files(FullFolders[str_detect(FullFolders,'Positive')])
FalsePositives <- list.files(FullFolders[str_detect(FullFolders,'Negative')])


PositiveDF <- cbind.data.frame(rep('Positive', length(TruePositives)),TruePositives)
colnames(PositiveDF) <- c('Category','File')
NegativeDF <- cbind.data.frame(rep('Negative', length(FalsePositives)),FalsePositives)
colnames(NegativeDF) <- c('Category','File')


CombinedDF <- rbind.data.frame(PositiveDF,NegativeDF)
CombinedDF$Probability <- as.numeric(str_split_fixed(CombinedDF$File, pattern = '_', n=2)[,1])
CombinedDF$Class <- (str_split_fixed(CombinedDF$File, pattern = '_', n=3)[,2])

CombinedDF$Eval <-  ifelse(CombinedDF$Category=='Positive',1,0)
CombinedDF$Probability <- CombinedDF$Probability -0.0001

CombinedDF$logit=log(CombinedDF$Probability /(1-CombinedDF$Probability ))

null.model=glm(Eval~1, CombinedDF, family = 'binomial')
conf.model=glm(Eval~Probability, CombinedDF, family = 'binomial')
logit.model=glm(Eval~logit, CombinedDF, family = 'binomial')
AICctab(null.model, conf.model,weights=T)
summary(conf.model)


prediction.range.conf=seq(0,1,.001)
prediction.range.logit=seq(-3,7,.1) # this is the approximate range of the logit scores

predictions.conf=predict(conf.model, list(Probability=prediction.range.conf), type='r')
predictions.logit=predict(logit.model, list(logit=prediction.range.logit), type='r')

# thresholds for pr(Positive)= .65, 0.95, and 0.99
cutoff65.c=(log(.65/(1-.65))-conf.model$coefficients[1])/conf.model$coefficients[2]
cutoff95.c=(log(.95/(1-.95))-conf.model$coefficients[1])/conf.model$coefficients[2]
cutoff99.c=(log(.99/(1-.99))-conf.model$coefficients[1])/conf.model$coefficients[2]

cutoff65.l=(log(.65/(1-.65))-logit.model$coefficients[1])/logit.model$coefficients[2]
cutoff95.l=(log(.95/(1-.95))-logit.model$coefficients[1])/logit.model$coefficients[2]
cutoff99.l=(log(.99/(1-.99))-logit.model$coefficients[1])/logit.model$coefficients[2]



plot(Eval~Probability, CombinedDF, #main = paste('Confidence scores \n',CombinedDF$Class[1]),
     ylab = 'pr(BirdNET prediction is correct)', xlab = 'Confidence score',
     xlim=range(prediction.range.conf), pch=16, cex=1.5, col=rgb(0,0,0,.2))

lines(predictions.conf~prediction.range.conf, lwd=4, col=rgb(0,.65,1,.5))
abline(v=cutoff65.c, col='orange', lwd=4)
text(cutoff65.c-.02 ,0, "65")
abline(v=cutoff95.c, col='red', lwd=4)
text(cutoff95.c-.02 ,0, "95")
abline(v=cutoff99.c, col='magenta', lwd=4)
text(cutoff99.c-.02 ,0, "99")

# plot(Eval~Probability, CombinedDF, main = paste('Logit scores \n',CombinedDF$Class[1]),
#      ylab = 'pr(BirdNET prediction is correct)', xlab = 'logit score',
#      xlim=range(prediction.range.logit), pch=16, cex=1.5, col=rgb(0,0,0,.2))
#
# lines(predictions.logit~prediction.range.logit, lwd=4, col=rgb(0,.65,1,.5))
# abline(v=cutoff65.l, col='orange', lwd=4)
# text(cutoff65.l-.02 ,0, "65")
# abline(v=cutoff95.l, col='red', lwd=4)
# text(cutoff95.l-.02 ,0, "95")
# abline(v=cutoff99.l, col='magenta', lwd=4)
# text(cutoff99.l-.02 ,0, "99")
}


