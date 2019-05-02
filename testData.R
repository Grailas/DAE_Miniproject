# Load libraries
library("xlsx")
library(ggplot2)
library(dplyr)
library(Hmisc)
# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
setwd("D:/Dropbox/AAU/4. Semester/Design and Analysis of Experiments/DAE experiment/Test results")

# Read excel files
baguette <- read.xlsx("baguetteResults.xlsx", 1)
questionnaire <- read.xlsx("questionnaire.xlsx", 1)
schedule <- read.xlsx("schedule.xlsx", 1)

# Schedule
## Remove times, names and status
scheduleClean <- schedule[,c(3:4,6)]

## Rename columns
names(scheduleClean) <- c("ParticipantNumber", "Sequence", "Notes")


# SSQ
## Remove SSQ points and questionnaire respondent status rows
questionnaire <- questionnaire[,-c(6:26)]

## Seperate pre and post data
preSSQ <- subset(questionnaire, PrePost=="Before")
postSSQ <- subset(questionnaire, PrePost=="After")

## Remove PrePost tag from pre
preSSQ <- preSSQ[,-2]

## Remove pre data from post
postSSQ <- postSSQ[,-c(2:5)]

## Rename columns
names(preSSQ) <- c("ParticipantNumber", "Gender", "VrUse", "DominantHand", "NauseaPre", "OculomotorPre", "DisorientationPre", "SSQPre")
names(postSSQ) <- c("ParticipantNumber", "NauseaPost", "OculomotorPost", "DisorientationPost", "SSQPost")

## Merge SSQ data
combinedSSQ <- merge(preSSQ, postSSQ, by="ParticipantNumber")

## Add column for SSQ comparison
combinedSSQ$SSQChange <- combinedSSQ$SSQPost-combinedSSQ$SSQPre


# Baguette
## Calculate offset from center
baguette$CutOffset <- baguette[,c("CutPosition")]-50

## Get absolute offset from center
baguette$CutOffsetAbs <- abs(baguette$CutOffset)

## Remove ids and cut times
baguette <- baguette[,-c(1,5)]

## Create column converting size labels to numeric values
baguette$BaguetteSizeUnits <- recode(baguette$BaguetteSize, 'Small'=0.5, 'Medium'=1, 'Large'=2)

## Add 1 to trial numbers, so they start at 1
baguette$TrialNumber <- baguette$TrialNumber + 1
  
## Turn data into wide format, having each individual participant per row
baguetteFormatted <- reshape(baguette, idvar = "ParticipantNumber", timevar = "TrialNumber", direction = "wide")


# Results
## Combine schedule data with survey data
results <- merge(scheduleClean, combinedSSQ, by="ParticipantNumber")

## combine with baguette test data
results <- merge(results, baguetteFormatted, by="ParticipantNumber")


# Plots
## Cut positions by attempt
cutPosAttGraph <- ggplot(results) + labs(colour = "Attempt")
cutPosAttGraph + geom_density(aes(x=results$CutPosition.1, colour = "First")) + geom_density(aes(x=results$CutPosition.2, colour = "Second")) + geom_density(aes(x=results$CutPosition.3, colour = "Third")) + xlab("Cut Position") + ylab("Frequency")

## Cut positions by size
cutPosSizGraph <- ggplot(baguette)
cutPosSizGraph + geom_density(aes(x=CutPosition, colour = BaguetteSize)) + ylab("Frequency")

cutPosSizScatter <- ggplot(baguette, aes(x=BaguetteSizeUnits, y=CutPosition, colour = BaguetteSizeUnits))
cutPosSizScatter + geom_point() + geom_smooth(method = "lm")

cutPosSizBox <- ggplot(baguette, aes(x=BaguetteSize, y=CutPosition, colour = BaguetteSize))
cutPosSizBox + geom_boxplot()


# Correlation
## Pearson 
cor(baguette$CutPosition, baguette$BaguetteSizeUnits, use="complete.obs", method="pearson")

rcorr(baguette$CutPosition, baguette$BaguetteSizeUnits, type="pearson")

## Shared variability
cor(baguette$CutPosition, baguette$BaguetteSizeUnits, use="complete.obs", method="pearson")^2*100

## Spearman
cor(baguette$CutPosition, baguette$BaguetteSizeUnits, method="spearman")



# Q-Q   REMEMBER TO SORT DATA BY BAGUETTE SIZE
##Cut1
###Histograms
hist.cut1 <- ggplot(baguetteFormatted, aes(CutPosition.1)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white")
hist.cut1

###Normal curve
hist.cut1 + stat_function(fun=dnorm, args = list(mean = mean(baguetteFormatted$CutPosition.1, na.rm = TRUE), sd = sd(baguetteFormatted$CutPosition.1, na.rm = TRUE)), colour = "black", size = 1)

###qqplot
qqplot.cut1 <- qplot(sample=baguetteFormatted$CutPosition.1)
qqplot.cut1


##Cut2
###Histograms
hist.cut2 <- ggplot(baguetteFormatted, aes(CutPosition.2)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white")
hist.cut2

###Normal curve
hist.cut2 + stat_function(fun=dnorm, args = list(mean = mean(baguetteFormatted$CutPosition.2, na.rm = TRUE), sd = sd(baguetteFormatted$CutPosition.2, na.rm = TRUE)), colour = "black", size = 1)

###qqplot
qqplot.cut2 <- qplot(sample=baguetteFormatted$CutPosition.2)
qqplot.cut2


##Cut3
###Histograms
hist.cut3 <- ggplot(baguetteFormatted, aes(CutPosition.3)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white")
hist.cut3

###Normal curve
hist.cut3 + stat_function(fun=dnorm, args = list(mean = mean(baguetteFormatted$CutPosition.3, na.rm = TRUE), sd = sd(baguetteFormatted$CutPosition.3, na.rm = TRUE)), colour = "black", size = 1)

###qqplot
qqplot.cut3 <- qplot(sample=baguetteFormatted$CutPosition.3)
qqplot.cut3


library(pastecs)
#skew and curtosis cut1
stat.desc(baguetteFormatted$CutPosition.1, basic = FALSE, norm = TRUE)

#skew and curtosis for all cuts
stat.desc(cbind(baguetteFormatted$CutPosition.1,baguetteFormatted$CutPosition.2,baguetteFormatted$CutPosition.3), basic = FALSE, norm = TRUE)

#format output
round(stat.desc(cbind(baguetteFormatted$CutPosition.1,baguetteFormatted$CutPosition.2,baguetteFormatted$CutPosition.3), basic = FALSE, norm = TRUE), digits = 3)

#Shapiro test cut 1
shapiro.test(baguetteFormatted$CutPosition.1)
