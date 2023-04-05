dir <- "."
list.files(dir)

masterSummaryStats <- data.frame()

dFiles <- list.files(paste0(dir,"/"),"data.*.csv")
pFiles <- list.files(paste0(dir,"/"),"params.*.csv")

fileNumber <- 1

for (cur in dFiles){
  #Reading data and parameters
  dataReps <- read.csv(paste0(dir,"/",cur))
  params <- read.csv(paste0(dir,"/",pFiles[which(gsub(".*_params","",pFiles) == gsub(".*_data","",cur))])) #param and data files which match suffixes
  
  for (r in 1:max(dataReps$rep)){
    data <- subset(dataReps,rep == r)
    
    #Produce summary statistics from the data file  
    summaryStats <- data.frame()
    # for (r in 1:params$replicates){
    for (g in unique(data$gen)){
      dataSubRG_send <- subset(data,rep == r & gen==g & indType == "Sender")
      dataSubRG_rece <- subset(data,rep == r & gen==g & indType == "Receiver")
      sendStrat1 <- as.numeric(nrow(subset(dataSubRG_send,strategy == 1)))
      sendStrat2 <- as.numeric(nrow(subset(dataSubRG_send,strategy == 2)))
      sendStrat3 <- as.numeric(nrow(subset(dataSubRG_send,strategy == 3)))
      receStrat1 <- as.numeric(nrow(subset(dataSubRG_rece,strategy == 1)))
      receStrat2 <- as.numeric(nrow(subset(dataSubRG_rece,strategy == 2)))
      receStrat3 <- as.numeric(nrow(subset(dataSubRG_rece,strategy == 3)))
      sendAlphaBeta <- as.numeric(mean(dataSubRG_send$alphaBeta))
      receAlphaBeta <- as.numeric(mean(dataSubRG_rece$alphaBeta))
      sendFit <- as.numeric(mean(dataSubRG_send$fitness))
      receFit <- as.numeric(mean(dataSubRG_rece$fitness))
      summaryStats <- rbind(summaryStats,c(r,g,"Sender",
                                           sendStrat1,"strat1",sendAlphaBeta,sendFit))
      summaryStats <- rbind(summaryStats,c(r,g,"Sender",
                                           sendStrat2,"strat2",sendAlphaBeta,sendFit))
      summaryStats <- rbind(summaryStats,c(r,g,"Sender",
                                           sendStrat3,"strat3",sendAlphaBeta,sendFit))
      summaryStats <- rbind(summaryStats,c(r,g,"Receiver",
                                           receStrat1,"strat1",receAlphaBeta,receFit))
      summaryStats <- rbind(summaryStats,c(r,g,"Receiver",
                                           receStrat2,"strat2",receAlphaBeta,receFit))
      summaryStats <- rbind(summaryStats,c(r,g,"Receiver",
                                           receStrat3,"strat3",receAlphaBeta,receFit))
    }
    # }
    colnames(summaryStats) <- c("rep","gen","indType","stratNum","stratType","meanAlphaBeta","meanFit")
    
    #Calculate expected alpha and beta
    expBeta <- max(min(as.numeric(params$c2),1),0)
    expAlpha <- max(min(as.numeric(params$m/(1-params$m)),1),0)
    summaryStats$expAlphaBeta <- 0
    for (i in 1:nrow(summaryStats)){
      summaryStats[i,]$expAlphaBeta<-ifelse(summaryStats[i,]$indType=="Sender",expAlpha,expBeta)
    }
    
    summaryStats <- cbind(summaryStats,fileNumber)
    
    #Add parameters to summary stats
    for (i in 1:ncol(params)){
      summaryStats <- cbind(summaryStats,params[i])
    }
    
    #Add to master summaryStat df
    masterSummaryStats <- rbind(masterSummaryStats,summaryStats)  
    
    fileNumber <- fileNumber + 1
  }
}

#Save as a .csv file
fTime <- gsub("_params.*.","",pFiles[1])
fName<-unique(gsub("_.*..csv","",gsub(".*_params_","",pFiles)))
write.csv(masterSummaryStats,paste0(dir,"/",fTime,"_",fName,"_summaryStats.csv"))








#Replicate Aggeregate Data
masterEndStats <- data.frame()

#Set a % cutoff to look at last x% of data from a simulation
cutoff <- .25

#Set a tolerance - simulations within this range of equilibrium are considered at equilibrium
tolerance <- .10
#### Create masterEndStats data frame - results of last cutoff% generations ####
for (fn in unique(masterSummaryStats$fileNumber)){
  repData <- subset(masterSummaryStats,fileNumber == fn)
  repData$gen <- as.numeric(repData$gen)
  repData$meanAlphaBeta<-as.numeric(repData$meanAlphaBeta)
  repData$stratNum<-as.numeric(repData$stratNum)
  
  endData <- subset(repData, gen >= max(repData$gen) - cutoff*max(repData$gen))  
  pm <- endData[1,] #for parameters
  
  meanAlpha <- mean(subset(endData,indType == "Sender")$meanAlphaBeta)
  meanBeta <- mean(subset(endData,indType == "Receiver")$meanAlphaBeta)
  
  sdAlpha <- sd(subset(endData,indType == "Sender")$meanAlphaBeta)
  sdBeta <- sd(subset(endData,indType == "Receiver")$meanAlphaBeta)
  
  meanSS1 <- mean(subset(endData,indType == "Sender" & stratType == "strat1")$stratNum)/N
  meanSS2 <- mean(subset(endData,indType == "Sender" & stratType == "strat2")$stratNum)/N
  meanSS3 <- mean(subset(endData,indType == "Sender" & stratType == "strat3")$stratNum)/N
  meanRS1 <- mean(subset(endData,indType == "Receiver" & stratType == "strat1")$stratNum)/N
  meanRS2 <- mean(subset(endData,indType == "Receiver" & stratType == "strat2")$stratNum)/N
  meanRS3 <- mean(subset(endData,indType == "Receiver" & stratType == "strat3")$stratNum)/N
  
  sdSS1 <- sd((subset(endData,indType == "Sender" & stratType == "strat1")$stratNum)/N)
  sdSS2 <- sd((subset(endData,indType == "Sender" & stratType == "strat2")$stratNum)/N)
  sdSS3 <- sd((subset(endData,indType == "Sender" & stratType == "strat3")$stratNum)/N)
  sdRS1 <- sd((subset(endData,indType == "Receiver" & stratType == "strat1")$stratNum)/N)
  sdRS2 <- sd((subset(endData,indType == "Receiver" & stratType == "strat2")$stratNum)/N)
  sdRS3 <- sd((subset(endData,indType == "Receiver" & stratType == "strat3")$stratNum)/N)
  
  expAlpha <- subset(endData,indType=="Sender")[1,]$expAlphaBeta
  expBeta <- subset(endData,indType=="Receiver")[1,]$expAlphaBeta
  
  #expected sender and receiver strategies - ONLY FOR ZOLLMAN PARAMETERS (v, w = 0 or 1)
  expEquilibZoll <- "_"
  if (pm$c1 < 1 & pm$c2 >= 1){
    expEquilibZoll <- "separating honest"
  } else if (pm$c1 < pm$c2 & pm$c2 < 1){
    expEquilibZoll <- "hybrid"
  } else if (pm$c2 <= pm$c1 & pm$c1 < 1){
    expEquilibZoll <= "pooling all signal"
  } else if (pm$c2 > 1 & pm$c1 > 1){
    expEquilibZoll <= "pooling no signal"
  } else if (pm$c2 < 1 & pm$c1 >= 1){
    expEquilibZoll <- "pooling no signal - reverse"
  }
  if (pm$c1 < 0 || pm$c2 < 0){
    expEquilibZoll <- paste0(expEquilibZoll,", negative costs")
  }
  
  #Detect big 'switches' - if the max abundance of two strategies in the interval each reach >50%, detect a switch
  switchS <- 0
  switchR <- 0
  
  maxSS1 <- max((subset(endData,indType == "Sender" & stratType == "strat1")$stratNum)/N)
  maxSS2 <- max((subset(endData,indType == "Sender" & stratType == "strat2")$stratNum)/N)
  maxSS3 <- max((subset(endData,indType == "Sender" & stratType == "strat3")$stratNum)/N)
  maxRS1 <- max((subset(endData,indType == "Receiver" & stratType == "strat1")$stratNum)/N)
  maxRS2 <- max((subset(endData,indType == "Receiver" & stratType == "strat2")$stratNum)/N)
  maxRS3 <- max((subset(endData,indType == "Receiver" & stratType == "strat3")$stratNum)/N)
  
  if (maxSS1 > 0.5 & maxSS2 > 0.5){
    switchS <- 1
  } else if (maxSS2 > 0.5 & maxSS3 > 0.5){
    switchS <- 1
  } else if (maxSS1 > 0.5 & maxSS3 > 0.5){
    switchS <- 1
  }
  if (maxRS1 > 0.5 & maxRS2 > 0.5){
    switchR <- 1
  } else if (maxRS2 > 0.5 & maxRS3 > 0.5){
    switchR <- 1
  } else if (maxRS1 > 0.5 & maxRS3 > 0.5){
    switchR <- 1
  }
  
  #What % deviation are we away from equilibrium? 
  devAlpha <- meanAlpha - expAlpha
  devBeta <- meanBeta - expBeta 
  
  #****Need to add initialization option to this. New cpp code exports this
  row <- data.frame(meanAlpha,meanBeta,sdAlpha,sdBeta,
                    meanSS1,meanSS2,meanSS3,meanRS1,meanRS2,meanRS3,
                    sdSS1,sdSS2,sdSS3,sdRS1,sdRS2,sdRS3,
                    expAlpha,expBeta,devAlpha,devBeta,
                    maxSS1,maxSS2,maxSS2,maxRS1,maxRS2,maxRS3,
                    switchS,switchR,expEquilibZoll,
                    pm$rep,pm$fileNumber,pm$N,pm$G,pm$c1,pm$c2,pm$v1,pm$v2,pm$w1,pm$w2
                    ,pm$w3,pm$w4,pm$m,pm$interactionPartners,pm$mutRateAlpha,pm$mutRateBeta
                    ,pm$mutRateStrategySender,pm$mutRateStrategyReceiver,pm$mutStepAlpha
                    ,pm$mutStepBeta,pm$initStrategySender,pm$initStrategyReceiver,
                    pm$initAlpha,pm$initBeta,pm$initializationType,pm$alphaBetaMutation,
                    pm$cauchyDist)
  masterEndStats <- rbind(masterEndStats,row)
  
}
names <- c("meanAlpha","meanBeta","sdAlpha","sdBeta",
           "meanSS1","meanSS2","meanSS3","meanRS1","meanRS2","meanRS3",
           "sdSS1","sdSS2","sdSS3","sdRS1","sdRS2","sdRS3",
           "expAlpha","expBeta","devAlpha","devBeta",
           "maxSS1","maxSS2","maxSS3","maxRS1","maxRS2","maxRS3",
           "switchS","switchR","expEquilibZoll",
           "rep","fileNumber","N","G","c1","c2","v1","v2",
           "w1","w2","w3","w4","m","interactionPartners",
           "mutRateAlpha","mutRateBeta","mutRateStrategySender",
           "mutRateStrategyReceiver","mutStepAlpha","mutStepBeta",
           "initStrategySender","initStrategyReceiver","initAlpha",
           "initBeta","initializationType","alphaBetaMutation","cauchyDist")
colnames(masterEndStats) <- names
#### ####

write.csv(masterEndStats,paste0(dir,"/",fTime,"_",fName,"_endStats.csv"))



#End
