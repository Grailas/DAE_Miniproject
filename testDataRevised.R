# Load libraries
library("xlsx")
library(ggplot2)
library(dplyr)
library(Hmisc)
library(tidyr)
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
## Remove ids, trial number and cut times
baguette <- baguette[,-c(1,3,5)]

## Convert to wide format
baguetteFormatted <- spread(baguette, BaguetteSize, CutPosition)

## Calculate offsets from center
baguetteFormatted$Offset.Large <- baguetteFormatted[,c("Large")]-50
baguetteFormatted$Offset.Medium <- baguetteFormatted[,c("Medium")]-50
baguetteFormatted$Offset.Small <- baguetteFormatted[,c("Small")]-50

## Get absolute offset from center
baguetteFormatted$AbsOffset.Large <- abs(baguetteFormatted$Offset.Large)
baguetteFormatted$AbsOffset.Medium <- abs(baguetteFormatted$Offset.Medium)
baguetteFormatted$AbsOffset.Small <- abs(baguetteFormatted$Offset.Small)


# Results
## Combine schedule data with survey data
results <- merge(scheduleClean, combinedSSQ, by="ParticipantNumber")

## combine with baguette test data
results <- merge(results, baguetteFormatted, by="ParticipantNumber")

## export
library(multiplex)
write.dat(results, "result")


# Plots
## Cut positions by attempt
##cutPosAttGraph <- ggplot(results) + labs(colour = "Attempt")
##cutPosAttGraph + geom_density(aes(x=results$CutPosition.1, colour = "First")) + geom_density(aes(x=results$CutPosition.2, colour = "Second")) + geom_density(aes(x=results$CutPosition.3, colour = "Third")) + xlab("Cut Position") + ylab("Frequency")

## Cut positions by size
cutPosSizGraph <- ggplot(baguette)
cutPosSizGraph + geom_density(aes(x=CutPosition, colour = BaguetteSize)) + ylab("Frequency")

cutPosSizScatter <- ggplot(baguette, aes(x=BaguetteSize, y=CutPosition, colour = BaguetteSize))
cutPosSizScatter + geom_point() + geom_smooth(method = "lm")

cutPosSizBox <- ggplot(baguette, aes(x=BaguetteSize, y=CutPosition, colour = BaguetteSize))
cutPosSizBox + geom_boxplot()


# Correlation
## Add sizes in units
baguette$BaguetteSizeUnits <- NA
baguette$BaguetteSizeUnits[baguette$BaguetteSize=="Small"] <- 0.5
baguette$BaguetteSizeUnits[baguette$BaguetteSize=="Medium"] <- 1
baguette$BaguetteSizeUnits[baguette$BaguetteSize=="Large"] <- 2

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
hist.cut1 <- ggplot(results, aes(Small)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white")
hist.cut1

###Normal curve
hist.cut1 + stat_function(fun=dnorm, args = list(mean = mean(results$Small, na.rm = TRUE), sd = sd(results$Small, na.rm = TRUE)), colour = "black", size = 1)

###qqplot
qqplot.cut1 <- qplot(sample=results$Small)
qqplot.cut1


##Cut2
###Histograms
hist.cut2 <- ggplot(results, aes(Medium)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white")
hist.cut2

###Normal curve
hist.cut2 + stat_function(fun=dnorm, args = list(mean = mean(results$Medium, na.rm = TRUE), sd = sd(results$Medium, na.rm = TRUE)), colour = "black", size = 1)

###qqplot
qqplot.cut2 <- qplot(sample=results$Medium)
qqplot.cut2


##Cut3
###Histograms
hist.cut3 <- ggplot(results, aes(Large)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white")
hist.cut3

###Normal curve
hist.cut3 + stat_function(fun=dnorm, args = list(mean = mean(results$Large, na.rm = TRUE), sd = sd(results$Large, na.rm = TRUE)), colour = "black", size = 1)

###qqplot
qqplot.cut3 <- qplot(sample=results$Large)
qqplot.cut3


library(pastecs)
#skew and curtosis cut1
stat.desc(results$AbsOffset.Small, basic = FALSE, norm = TRUE)

#skew and curtosis for all cuts
stat.desc(cbind(results$AbsOffset.Small,results$AbsOffset.Medium,results$AbsOffset.Large), basic = FALSE, norm = TRUE)

#format output
round(stat.desc(cbind(results$AbsOffset.Small,results$AbsOffset.Medium,results$AbsOffset.Large), basic = FALSE, norm = TRUE), digits = 3)

#Shapiro test cut 1
shapiro.test(results$AbsOffset.Small)

#T tests
library(Hmisc)

##small-medium
dep.t.testSM <- t.test(results$AbsOffset.Small, results$AbsOffset.Medium, paired = TRUE)
dep.t.testSM

##small-large
dep.t.testSL <- t.test(results$AbsOffset.Small, results$AbsOffset.Large, paired = TRUE)
dep.t.testSL

#medium-large
dep.t.testML <- t.test(results$AbsOffset.Medium, results$AbsOffset.Large, paired = TRUE)
dep.t.testML

pvalues <- c(0.0001333, 0.0002749, 0.6095)
p.adjust(pvalues, method = "bonferroni")


#effect size
##SM
t_SM <- dep.t.testSM$statistic[[1]]
df_SM <- dep.t.testSM$parameter[[1]]
r_SM <- sqrt(t_SM^2/(t_SM^2+df_SM))
round(r_SM,3)

##SL
t_SL <- dep.t.testSL$statistic[[1]]
df_SL <- dep.t.testSL$parameter[[1]]
r_SL <- sqrt(t_SL^2/(t_SL^2+df_SL))
round(r_SL,3)

##ML
t_ML <- dep.t.testML$statistic[[1]]
df_ML <- dep.t.testML$parameter[[1]]
r_ML <- sqrt(t_ML^2/(t_ML^2+df_ML))
round(r_ML,3)


#Anova
##Create a long-format copy of the test data, with absolute offset
baguetteA <- baguette
baguetteA$AbsOffset <- abs(baguetteA$CutPosition-50)

library(ez)
##Run Anova
baguetteModel <- ezANOVA(data = baguetteA, dv = .(AbsOffset), wid = .(ParticipantNumber), within = .(BaguetteSize), detailed=TRUE, type = 3)
baguetteModel

# running posthoc tests with bonferroni correction
pairwise.t.test(baguetteA$AbsOffset, baguetteA$BaguetteSize, paired = TRUE, p.adjust.method ="bonferroni")

#Running Histogram for each cut size, Small, Medium and Large
histog <- ggplot(results, aes(x=results$Offset.Small))
histog + geom_histogram()

histog <- ggplot(results, aes(x=results$Offset.Medium))
histog + geom_histogram()

histog <- ggplot(results, aes(x=results$Offset.Large))
histog + geom_histogram()

#Hist showing the levels of experience with VR
histog <- ggplot(results, aes(x=results$VrUse))
histog + stat_count()

#Hist for the SSQ post
histog <- ggplot(results, aes(x=results$SSQPost))
histog + geom_histogram()

#Running interquartile range for the each error
error = results$Offset.Small 
IQR(error)

error =  results$Offset.Medium
IQR(error)

error =  results$Offset.Large
IQR(error)

#Running scatterplot
scatter <- ggplot(results, aes(x=results$ParticipantNumber, y= results$Offset.Small))
scatter + geom_point()

scatter <- ggplot(results, aes(x=results$Offset.Medium, y= results$ParticipantNumber))
scatter + geom_point()

scatter <- ggplot(results, aes(x=results$Offset.Large, y= results$Offset.Medium))
scatter + geom_point()
