dir <- "."
dir <- "C:/Users/owner/Documents/DONE"
list.files(dir)

masterSummaryStats <- data.frame()

dFiles <- list.files(paste0(dir,"/"),"data.*.csv")
pFiles <- list.files(paste0(dir,"/"),"params.*.csv")

fileNumber <- 1

for (cur in dFiles){
  #Reading data and parameters
  dataReps <- read.csv(paste0(dir,"/",cur))
  params <- read.csv(paste0(dir,"/",pFiles[which(gsub(".*_params","",pFiles) == gsub(".*_data","",cur))])) #param and data files which match suffixes
  params
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

#End
