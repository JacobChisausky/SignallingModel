library(ggplot2)
library(grid)
library(gtable)

# Sender Strategy 1: If T1, signal with probability α. If T2, signal.
# Sender Strategy 2: Never signal.
# Sender Strategy 3: If T1, don’t signal. If T2, signal.
# Receiver Strategy 1: If receipt of signal, A1 with probability β. Otherwise A2.
# Receiver Strategy 2: Always A1.
# Receiver Strategy 3: If receipt of signal, A2. Otherwise A1.

#Trajectory (summaryStats.csv) analyses
dir <- "C:/Users/owner/Documents/DONE"
list.files(dir)
selection <- 9

#Read data
masterSummaryStats <- read.csv(paste0(dir,"/",list.files(dir)[selection]))
##

#Organize parameters for all simulations
masterParams <- list()
variableParams <- c()
paramNames <- c()
for (c in which(colnames(masterSummaryStats) == "N"):ncol(masterSummaryStats)){
  masterParams[[c+1-which(colnames(masterSummaryStats) == "N")]] <- unique(masterSummaryStats[,c])
  paramNames <- c(paramNames,colnames(masterSummaryStats)[c])
  if(length(unique(masterSummaryStats[,c])) > 1 ){
    variableParams<- c(variableParams,colnames(masterSummaryStats)[c])
  }
}
names(masterParams) <- paramNames
N <- as.numeric(masterParams$N)
###

print(variableParams)



#Trajectories - Choose data to graph
D <- subset(masterSummaryStats)


#What to print on plot label: ####
lab <- paste0("Params:\n\nN: ",toString(unique(D$N)),
              "\nm: ",toString(unique(D$m)),
              "\nc1: ",toString(unique(D$c1)),
              "\nc2: ",toString(unique(D$c2)),
              "\nv1: ",toString(unique(D$v1)),
              "\nv2: ",toString(unique(D$v2)),
              "\nw1: ",toString(unique(D$w1)),
              "\nw2: ",toString(unique(D$w2)),
              "\nw3: ",toString(unique(D$w3)),
              "\nw4: ",toString(unique(D$w4)),
              "\nInter. Pars.: ",toString(unique(D$interactionPartners)),
              "\nmuRateA: ",toString(unique(D$mutRateAlpha)),
              "\nmuRateB: ",toString(unique(D$mutRateBeta)),
              "\nmuRateSS: ",toString(unique(D$mutRateStrategySender)),
              "\nmuRateSR: ",toString(unique(D$mutRateStrategyReceiver)),
              "\nmuStepA: ",toString(unique(D$mutStepAlpha)),
              "\nmuStepB: ",toString(unique(D$mutStepBeta)),
              "\n\n\nData:"
)
#### ####

variableParams


#Plot of replicate trajectories ####
ggplot(data=D,aes(x=as.numeric(gen)/100)) +
  
  #facet_grid(indType~as.numeric(fileNumber)) +
  facet_grid(indType~c2+interactionPartners+mutStepAlpha+as.numeric(fileNumber)) +
  
  geom_line(aes(y=expAlphaBeta*N)) +
  geom_point(aes(y=as.numeric(stratNum),color=paste0(stratType)),size=1) +
  geom_line(aes(y=as.numeric(stratNum),color=paste0(stratType)),alpha=0.1) +
  geom_point(aes(y=as.numeric(meanAlphaBeta*N),color=paste0(indType)),size=1,shape=4) +
  geom_line(aes(y=as.numeric(meanAlphaBeta*N),color=paste0(indType)),alpha=0.1) +
  theme_bw() + 
  labs(color=lab,x="Generation / 100") +
  theme(panel.margin.x=unit(0.1, "lines") , panel.margin.y=unit(.5,"lines")) +
  scale_y_continuous(
    name = "Count",
    sec.axis = sec_axis(~.*1/N, name="Alpha or Beta")) +
  scale_color_hue(labels = c("Beta", "Alpha","Strategy 1","Strategy 2","Strategy 3")) +
  labs(title="10 replicates - c1 and c2 similar. Large N")
#### ####  

#Plot of replicate trajectories with fitness ####
D$normFit <- 0
minFit <- min(D$meanFit)
maxFit <- max(D$meanFit)
D$normFit <- (D$meanFit-minFit)/(maxFit-minFit)

ggplot(data=D,aes(x=as.numeric(gen)/100)) +
  facet_grid(indType~as.numeric(fileNumber)) +
  geom_line(aes(y=expAlphaBeta*N)) +
  geom_point(aes(y=as.numeric(stratNum),color=paste0(stratType)),size=1) +
  geom_line(aes(y=as.numeric(stratNum),color=paste0(stratType)),alpha=0.1) +
  geom_point(aes(y=as.numeric(meanAlphaBeta*N),color=paste0(indType)),size=1,shape=4) +
  geom_line(aes(y=as.numeric(meanAlphaBeta*N),color=paste0(indType)),alpha=0.1) +
  geom_line(aes(y=as.numeric(normFit*N)),alpha=0.9,color="blue") +
  theme_bw() + 
  labs(color=lab,x="Generation / 100") +
  theme(panel.margin.x=unit(0.1, "lines") , panel.margin.y=unit(.5,"lines")) +
  scale_y_continuous(
    name = "Count",
    sec.axis = sec_axis(~.*1/N, name="Alpha or Beta or Fitness")) +
  scale_color_hue(labels = c("Beta", "Alpha","Strategy 1","Strategy 2","Strategy 3")) +
  labs(title="5 replicates - c1 and c2 similar. Large N")
#### ####



#Endpoint (endStats.csv) analyses