library(ggplot2)

dir <- "C:/Users/owner/Documents/S4/Simulation"

list.files(dir,"data.*.csv")
fileSelect <- 7


data <- read.csv(paste0(dir,"/",list.files(dir,"data.*.csv")[fileSelect]))
params <- read.csv(paste0(dir,"/",list.files(dir,"params.*.csv")[fileSelect]))


#This snippet produces summary stats from raw data #####
summaryStats <- data.frame()
for (r in 1:params$replicates){
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
}
colnames(summaryStats) <- c("rep","gen","indType","stratNum","stratType","meanAlphaBeta","meanFit")
#### end ####


head(summaryStats)

ggplot(data=subset(summaryStats,rep==1),aes(x=as.numeric(gen))) + 
  geom_point(aes(y=as.numeric(stratNum),color=paste0(stratType))) +
  geom_path(aes(y=as.numeric(stratNum),color=paste0(stratType)),alpha=0.1) +
  geom_point(aes(y=as.numeric(as.numeric(meanAlphaBeta)*as.numeric(params$N)),color=paste0(indType)),shape=4) +
  geom_path(aes(y=as.numeric(as.numeric(meanAlphaBeta)*as.numeric(params$N)),color=paste0(indType)),alpha=0.1) +
  theme_bw() + 
  facet_grid(indType~.)


summaryStats$exp <- 

ggplot(data=subset(summaryStats,rep==1),aes(x=as.numeric(gen))) + 
  geom_line(y=as.numeric(params$c2*params$N)) +
  geom_line(y=as.numeric(params$N*params$m/(1-params$m))) +
  geom_point(aes(y=as.numeric(stratNum),color=paste0(stratType))) +
  geom_path(aes(y=as.numeric(stratNum),color=paste0(stratType)),alpha=0.1) +
  geom_point(aes(y=as.numeric(as.numeric(meanAlphaBeta)*as.numeric(params$N)),color=paste0(indType)),shape=4) +
  geom_path(aes(y=as.numeric(as.numeric(meanAlphaBeta)*as.numeric(params$N)),color=paste0(indType)),alpha=0.1) +
  theme_bw() + 
  facet_grid(indType~.)


