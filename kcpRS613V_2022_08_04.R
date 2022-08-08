#initialization
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(jtools)
library(kcpRS)
library(SimDesign)
library(mclust)
library(MSwM)
library(tvReg)
library(qpcR)
library(mgcv)
library(berryFunctions)

setwd("C:/Users/Arto/OneDrive/Documents/Psychology/Master Year 1/Thesis")

#read data
data613 <- read.csv("./Data/esm_613.csv", header=TRUE, sep= ';', na.strings=c("","NOT_DISPLAYED"))


#Data frame
data613s <- data613[c("ScheduledTime","relaxed","rumination","impulsivity","anger","craving")]
data613relaxed <- spacing(data613s,data613s$relaxed)

relaxed <- data613relaxed["relaxed"]  %>% unlist %>% as.numeric %>% standardize
rumination <- data613relaxed["rumination"]  %>% unlist %>% as.numeric %>% standardize
impulsivity <- data613relaxed["impulsivity"]  %>% unlist %>% as.numeric %>% standardize
anger <- data613relaxed["anger"]  %>% unlist %>% as.numeric %>% standardize
craving <- data613relaxed["craving"]  %>% unlist %>% as.numeric %>% standardize

data613kcp <- cbind(impulsivity,anger,craving,relaxed,rumination)%>% as.data.frame %>% drop_na
data613kcpidd <- data613kcp[c("impulsivity","anger","craving")]
data613kcpshared <- data613kcp[c("relaxed","rumination")]
rm(relaxed,rumination,impulsivity,anger,craving)



#kcpRS with bonferoni correction and workflow for all variables

kcp613idd15 <- kcpRS(data = data613kcpidd, RS_fun = runMean, RS_name = "mean",
                    wsize = 15, nperm = 1000, Kmax = 25, alpha = 0.05, varTest = FALSE,
                    ncpu = 1)

kcp613idd25 <- kcpRS(data = data613kcpidd, RS_fun = runMean, RS_name = "mean",
                   wsize = 25, nperm = 1000, Kmax = 25, alpha = 0.05, varTest = FALSE,
                   ncpu = 1)

kcp613idd35 <- kcpRS(data = data613kcpidd, RS_fun = runMean, RS_name = "mean",
                   wsize = 35, nperm = 1000, Kmax = 25, alpha = 0.05, varTest = FALSE,
                   ncpu = 1)

kcp613shared15 <- kcpRS(data = data613kcpshared, RS_fun = runMean, RS_name = "mean",
                      wsize = 15, nperm = 1000, Kmax = 25, alpha = 0.05, varTest = FALSE,
                      ncpu = 1)

kcp613shared25 <- kcpRS(data = data613kcpshared, RS_fun = runMean, RS_name = "mean",
                    wsize = 25, nperm = 1000, Kmax = 25, alpha = 0.05, varTest = FALSE,
                    ncpu = 1)

kcp613shared35 <- kcpRS(data = data613kcpshared, RS_fun = runMean, RS_name = "mean",
                        wsize = 35, nperm = 1000, Kmax = 25, alpha = 0.05, varTest = FALSE,
                        ncpu = 1)


#Adjusted rand index

ARIkcp <- function(data,kcp){
ARIkcpv <- c() #create empty vector
cps <- unlist(kcp[9])
cp1 <- cps[1]
ARIkcpv <- append(ARIkcpv,rep(x = 1, cp1))#appends first phase to vector
ARIkcpv <- append(ARIkcpv,rep(x = 2, nrow(data613kcp)-cp1))

return(ARIkcpv)
}

ARIidd <- as.data.frame(ARIkcp(data613kcpidd,kcp613idd15))
ARIshared <- as.data.frame(ARIkcp(data613kcpshared,kcp613shared15))

#reindex
NAlist613 <- is.na(data613relaxed[,c("relaxed","rumination","impulsivity","anger","craving")])
IndexlistKCP <- c()
for (i in 1:nrow(data613relaxed)){
  if (NAlist613[i,1]==FALSE & NAlist613[i,2]==FALSE & NAlist613[i,3]==FALSE & NAlist613[i,4]==FALSE & NAlist613[i,5]==FALSE){
    IndexlistKCP <- append(IndexlistKCP,i)
  }
}

rownames(ARIidd) <-IndexlistKCP
rownames(ARIshared) <-IndexlistKCP

Convergencekcp <- adjustedRandIndex(ARIidd[,1],ARIshared[,1])

#phase means

mphase1relaxed <- mean(data613relaxed$relaxed[1:426],na.rm = TRUE)
mphase2relaxed <- mean(data613relaxed$relaxed[427:1585],na.rm = TRUE)
mphase3relaxed <- mean(data613relaxed$relaxed[1586:2577],na.rm = TRUE)
mphase4relaxed <- mean(data613relaxed$relaxed[2578:nrow(data613relaxed)],na.rm = TRUE)

mphase1rumination <- mean(data613relaxed$rumination[1:426],na.rm = TRUE)
mphase2rumination <- mean(data613relaxed$rumination[427:1585],na.rm = TRUE)
mphase3rumination <- mean(data613relaxed$rumination[1586:2577],na.rm = TRUE)
mphase4rumination <- mean(data613relaxed$rumination[2578:nrow(data613relaxed)],na.rm = TRUE)

mphase1anger <- mean(data613relaxed$anger[1:2858],na.rm = TRUE)
mphase2anger <- mean(data613relaxed$anger[2859:nrow(data613relaxed)],na.rm = TRUE)

mphase1impulsivity <- mean(data613relaxed$impulsivity[1:2858],na.rm = TRUE)
mphase2impulsivity <- mean(data613relaxed$impulsivity[2859:nrow(data613relaxed)],na.rm = TRUE)

mphase1craving <- mean(data613relaxed$craving[1:2858],na.rm = TRUE)
mphase2craving <- mean(data613relaxed$craving[2859:nrow(data613relaxed)],na.rm = TRUE)

PhaseMeans613 <- list(mphase1craving,mphase1anger,mphase1relaxed,mphase1rumination,mphase1impulsivity,mphase2anger,mphase2craving,mphase2relaxed,mphase2rumination,mphase2impulsivity)
rm(mphase1craving,mphase1anger,mphase1relaxed,mphase1rumination,mphase1impulsivity,mphase2anger,mphase2craving,mphase2relaxed,mphase2rumination,mphase2impulsivity)

#phase SDs

sdphase1relaxed <- sd(data613relaxed$relaxed[1:426],na.rm = TRUE)
sdphase2relaxed <- sd(data613relaxed$relaxed[427:1585],na.rm = TRUE)
sdphase3relaxed <- sd(data613relaxed$relaxed[1586:2577],na.rm = TRUE)
sdphase4relaxed <- sd(data613relaxed$relaxed[2578:nrow(data613relaxed)],na.rm = TRUE)

sdphase1rumination <- sd(data613relaxed$rumination[1:426],na.rm = TRUE)
sdphase2rumination <- sd(data613relaxed$rumination[427:1585],na.rm = TRUE)
sdphase3rumination <- sd(data613relaxed$rumination[1586:2577],na.rm = TRUE)
sdphase4rumination <- sd(data613relaxed$rumination[2578:nrow(data613relaxed)],na.rm = TRUE)

sdphase1anger <- sd(data613relaxed$anger[1:2858],na.rm = TRUE)
sdphase2anger <- sd(data613relaxed$anger[2859:nrow(data613relaxed)],na.rm = TRUE)

sdphase1impulsivity <- sd(data613relaxed$impulsivity[1:2858],na.rm = TRUE)
sdphase2impulsivity <- sd(data613relaxed$impulsivity[2859:nrow(data613relaxed)],na.rm = TRUE)

sdphase1craving <- sd(data613relaxed$craving[1:2858],na.rm = TRUE)
sdphase2craving <- sd(data613relaxed$craving[2859:nrow(data613relaxed)],na.rm = TRUE)

Phasesds613 <- list(sdphase1craving,sdphase1anger,sdphase1relaxed,sdphase1rumination,sdphase1impulsivity,sdphase2anger,sdphase2craving,sdphase2relaxed,sdphase2rumination,sdphase2impulsivity)
rm(sdphase1craving,sdphase1anger,sdphase1relaxed,sdphase1rumination,sdphase1impulsivity,sdphase2anger,sdphase2craving,sdphase2relaxed,sdphase2rumination,sdphase2impulsivity)

rm(data613kcp,data613kcpidd,data613kcpshared,Convergencekcp)