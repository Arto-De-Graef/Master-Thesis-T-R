#Set up KCP and RSM
binReKCP <- Matchlength(IndexlistRelaxed,IndexlistKCP,binRelaxeds,ARIshared)
binRuKCP <- Matchlength(IndexlistRumination,IndexlistKCP,binRuminations,ARIshared)
binAKCP <- Matchlength(Indexlistanger,IndexlistKCP,binangers,ARIidd)
binIKCP <- Matchlength(Indexlistimpulsivity,IndexlistKCP,binimpulsivitys,ARIidd)
binCKCP <- Matchlength(Indexlistcraving,IndexlistKCP,bincravings,ARIidd)

#ARI
ConvergenceKCPRe <- adjustedRandIndex(unlist(binReKCP[,1]),unlist(binReKCP[,2]))
ConvergenceKCPRu <- adjustedRandIndex(unlist(binRuKCP[,1]),unlist(binRuKCP[,2]))
ConvergenceKCPA <- adjustedRandIndex(unlist(binAKCP[,1]),unlist(binAKCP[,2]))
ConvergenceKCPI <- adjustedRandIndex(unlist(binIKCP[,1]),unlist(binIKCP[,2]))
ConvergenceKCPC <- adjustedRandIndex(unlist(binCKCP[,1]),unlist(binCKCP[,2]))

#Looking at means at SDs
Indexdata <- (1:nrow(data613spaced))

binRelaxedscom <- Matchlength(IndexlistRelaxed,Indexdata,binRelaxeds,as.data.frame(data613spaced[,"relaxed"]))
MSDrelaxedRSM <- binRelaxedscom %>% group_by(binRelaxedscom[,1]) %>% summarise_at(vars(2),list(mean = mean, sd = sd))

binRuminationscom <- Matchlength(IndexlistRumination,Indexdata,binRuminations,as.data.frame(data613spaced[,"rumination"]))
MSDRuminationRSM <- binRuminationscom %>% group_by(binRuminationscom[,1]) %>% summarise_at(vars(2),list(mean = mean, sd = sd))

binangerscom <- Matchlength(Indexlistanger,Indexdata,binangers,as.data.frame(data613spaced[,"anger"]))
MSDangerRSM <- binangerscom %>% group_by(binangerscom[,1]) %>% summarise_at(vars(2),list(mean = mean, sd = sd))

binimpulsivityscom <- Matchlength(Indexlistimpulsivity,Indexdata,binimpulsivitys,as.data.frame(data613spaced[,"impulsivity"]))
MSDimpulsivityRSM <- binimpulsivityscom %>% group_by(binimpulsivityscom[,1]) %>% summarise_at(vars(2),list(mean = mean, sd = sd))

bincravingscom <- Matchlength(Indexlistcraving,Indexdata,bincravings,as.data.frame(data613spaced[,"craving"]))
MSDcravingRSM <- bincravingscom %>% group_by(bincravingscom[,1]) %>% summarise_at(vars(2),list(mean = mean, sd = sd))