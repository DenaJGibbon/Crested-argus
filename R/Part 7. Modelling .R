# Get lunar phase
library(lunar)
library(glmmTMB)
library(bbmle)
library(nasapower)
library(suntools)
library(lubridate)
library(performance)
library(sjPlot)

# Read in data
ArgusDF <-read.csv('/Users/denaclink/Downloads/Data_test_Argus/Argus_data2024_Updated (1).csv')
ArgusDF <- (na.omit(ArgusDF))

# Convert to required data
Year <- substr(ArgusDF$date,1,4)
Month <- substr(ArgusDF$date,5,6)
Day <- substr(ArgusDF$date,7,8)
ArgusDF$hour <- as.numeric(str_split_fixed(ArgusDF$date,pattern = '_', n=2)[,2])

ArgusDF$LunarDate <-paste(Year,Month,Day,sep = '-')

ArgusDF$LunarCategory <- as.factor((lunar.phase(as.Date(ArgusDF$LunarDate), name=TRUE)))

ArgusDF$LunarIllumination <- round(lunar.illumination(as.Date(ArgusDF$LunarDate)),2)

ArgusDFTimeSub <- subset(ArgusDF, hour <= 6 | hour >= 18 )

ggboxplot(data=ArgusDFTimeSub,x='LunarIllumination', y="n.detection" )



# Model selection for night calls with moon phase -------------------------

ArgusDFTimeSub$Site <- as.factor(ArgusDFTimeSub$Site )

# Convert call counts to binary
ArgusDFTimeSub$n.detection <-
  ifelse(ArgusDFTimeSub$n.detection  == 0 , 0, 1)

m.argusslunarbinary.intercept <-
  glmmTMB(n.detection ~   (1 | LunarDate),
          data = ArgusDFTimeSub,
          family = "binomial")

m.argusslunarbinary.1 <-
  glmmTMB(n.detection ~ LunarCategory + (1 | LunarDate),
          data = ArgusDFTimeSub,
          family = "binomial")

m.argusslunarbinary.2  <-
  glmmTMB(n.detection ~ LunarIllumination + (1 | LunarDate),
          data = ArgusDFTimeSub,
          family = "binomial")



bbmle::AICctab(
  m.argusslunarbinary.intercept,
  m.argusslunarbinary.1,
  m.argusslunarbinary.2,
  weights = T
)


# Calculate pseudo R-squared
MuMIn::r.squaredGLMM(m.argusslunarbinary.intercept)

# Model diagnostics
performance::check_residuals(m.argusslunarbinary.intercept)

# Check collinearity
performance::check_collinearity(m.argusslunarbinary.intercept)


# Get weather data --------------------------------------------------------
Lat1 <- str_split_fixed(ArgusDF$lat,pattern = ',',n=2)[,1]
Lat2 <- str_split_fixed(ArgusDF$lat,pattern = ',',n=2)[,2]
ArgusDF$lat_updated <- as.numeric(str_remove(paste(Lat1,Lat2,sep='.'),pattern = ','))

long1 <- str_split_fixed(ArgusDF$long,pattern = ',',n=2)[,1]
long2 <- str_split_fixed(ArgusDF$long,pattern = ',',n=2)[,2]
ArgusDF$long_updated <- as.numeric(str_remove(paste(long1,long2,sep='.'),pattern = ','))


data <- get_power(
  community = "RE",
  lonlat = c(mean(ArgusDF$long_updated), mean(ArgusDF$lat_updated)),
  dates = c(min(ArgusDF$LunarDate),max(ArgusDF$LunarDate)),
  temporal_api = "HOURLY",
  pars = "PRECTOTCORR"
)

data$YYYYMMDD <- paste(data$YEAR,str_pad(data$MO, 2, pad = "0") ,str_pad(data$DY, 2, pad = "0"),sep='-')

PrecipList <- list()

for(a in 1:nrow(ArgusDF)){
  TempRow <- subset(data, YYYYMMDD==ArgusDF[a,]$LunarDate & HR == ArgusDF[a,]$hour)
  PrecipList[[a]] <- TempRow$PRECTOTCORR
}

ArgusDF$PRECTOTCORR <- unlist(PrecipList)


library(suntools)

#Calculate sunset in Ithaca, NY, USA on June 1, 2023

Sunrise <- sunriset(
  matrix(c(c(mean(ArgusDF$long_updated), mean(ArgusDF$lat_updated))), nrow = 1),
  as.POSIXct(ArgusDF$LunarDate, tz = "GMT"),
  direction='sunrise',
  POSIXct.out=TRUE
)


ArgusDF$SunriseTime <-  Sunrise$time + hours(7) # add 3 hours


Sunset <- sunriset(
  matrix(c(c(mean(ArgusDF$long_updated), mean(ArgusDF$lat_updated))), nrow = 1),
  as.POSIXct(ArgusDF$LunarDate, tz = "GMT"),
  direction='sunset',
  POSIXct.out=TRUE
)

ArgusDF$SunsetTime <-  Sunrise$time + hours(7) # add 3 hours

# Time Categories: sunrise = 05, sunset = 18

# Create an offset based on recording effort
# This accounts for differences in recording duration

argus.aggregate.off.set <- data.frame()

date.index <- unique(ArgusDF$LunarDate)

for (b in 1:length(date.index)) {

   temp.subset <-
    subset(ArgusDF, LunarDate == date.index[b])

  log.recorder.offset <- log(length(unique(temp.subset$recorder)))

  temp.subset <- cbind.data.frame(temp.subset, log.recorder.offset)

  argus.aggregate.off.set <-
    rbind.data.frame(argus.aggregate.off.set, temp.subset)
}

# Check number of argus calls in dataset before running models
argus.aggregate.off.set.check <-
  droplevels(subset(argus.aggregate.off.set, call.type != 'none'))

sum(argus.aggregate.off.set.check$n.detection) # is 370



# Model selection using binary argus.aggregate.off.set
argus.aggregate.off.set$hour <- as.numeric(argus.aggregate.off.set$hour)
argus.aggregate.off.set$time.category <- ifelse(argus.aggregate.off.set$hour == 5, "sunrise",
                            ifelse(argus.aggregate.off.set$hour == 18, "sunset",
                                   ifelse(argus.aggregate.off.set$hour < 5 , "latenight",
                                          ifelse(argus.aggregate.off.set$hour > 5 & argus.aggregate.off.set$hour < 12 , "morning",
                                                 ifelse(argus.aggregate.off.set$hour >= 12 & argus.aggregate.off.set$hour < 18 , "afternoon",
                                                        ifelse( argus.aggregate.off.set$hour > 18 & argus.aggregate.off.set$hour < 24 , "earlynight",
                                                        "unknown"))))))

unique(argus.aggregate.off.set$time.category)

# Convert call counts to binary
argus.aggregate.off.set$n.detection <-
  ifelse(argus.aggregate.off.set$n.detection  == 0 , 0, 1)

table(argus.aggregate.off.set$n.detection )

argus.aggregate.off.set$hour <- as.factor(argus.aggregate.off.set$hour)

argus.aggregate.off.set$time.category <- as.factor(argus.aggregate.off.set$time.category)
levels(argus.aggregate.off.set$time.category) <- c('Afternoon','Night','Late Night',
                                                   'Morning','Sunrise','Sunset')

argus.aggregate.off.set$Rainfall <-  argus.aggregate.off.set$PRECTOTCORR

m.argusscomb.intercept <-
  glmmTMB(
    n.detection ~  (1 | Site/LunarDate),
    data = argus.aggregate.off.set,
    family = "binomial"
  )

m.argusscomb.1 <-
  glmmTMB(
    n.detection ~ Rainfall + (1 |Site/LunarDate),
    data = argus.aggregate.off.set,
    family = "binomial"
  )

m.argusscomb.2 <-
  glmmTMB(
    n.detection ~ Rainfall + time.category  +  (1 | Site/LunarDate),
    data = argus.aggregate.off.set,
    family = "binomial"
  )

m.argusscomb.3 <-
  glmmTMB(
    n.detection ~   Rainfall  +  (1 |Site/LunarDate),
    data = argus.aggregate.off.set,
    family = "binomial"
  )


# Compare models using AIC
bbmle::AICctab(
  m.argusscomb.intercept,
  m.argusscomb.1,
  m.argusscomb.2,
  m.argusscomb.3,
  weights = T
)

# Check summary of top model
anova(m.argusscomb.2, m.argusscomb.intercept)

summary(m.argusscomb.2)

# Evaluate performance using pseudo-R squared
MuMIn::r.squaredGLMM(m.argusscomb.2)

# Check for normality of residuals
check_residuals(simulate_residuals(m.argusscomb.2))

sjPlot::plot_model(m.argusscomb.2, sort.est = TRUE, vline.color = "red",show.values = TRUE,
                   title = "Crested argus calls (binary)")+theme_bw()


