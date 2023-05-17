library(tidyverse)
library(ggplot2)

# Exper 2 ####
dir <- "C:/Users/owner/Documents/DONE/Experiment_2"
summaryFiles <- list.files(dir,"*summaryStats*")


#First - create endData stats from summaryStats files produced by cpp
cutoff <- 0.25   #Look at last this % of generations
iterator <- 1
endDataAll<-data.frame()
for (i in summaryFiles){
  
  dataAllReps <- read.csv(paste0(dir,"/",i))
  
  for (r in unique(dataAllReps$rep)){
    data <- subset(dataAllReps,rep==r)
    
    data$fileName <- i
    dataEnd <- subset(data,gen>=((max(gen)-(cutoff*max(gen)))))
    dataEnd$concatStrat <- paste0(dataEnd$indType,"_",dataEnd$stratType)
    
    dataEndSenders <- subset(dataEnd,indType == "Sender")
    meanAlpha <- mean(dataEndSenders$meanAlphaBeta)
    dataEndReceivers <- subset(dataEnd,indType == "Receiver")
    meanBeta <- mean(dataEndReceivers$meanAlphaBeta)
    
    row <- data.frame()
    for (j in unique(dataEnd$concatStrat)){
      temp <- subset(dataEnd,concatStrat == j)
      row <- temp[1,]
      if (row$indType == "Receiver"){
        row$meanAlphaBeta <- meanBeta
      } else {
        row$meanAlphaBeta <- meanAlpha
      }
      
      
      row$fileSuffix <- gsub("_[0-9]","",gsub("_*..csv","",gsub(".*summaryStats_","",row$fileName)))
      row$meanFit <- mean(temp$meanFit)
      row$meanStratNum <- mean(temp$stratNum)
      row$meanStratFreq <- mean(temp$stratNum/row$N)
      row$stratNum <- "NA"
      row$gen <- "NA"
      row$fileNum <- iterator
      
      endDataAll <- rbind(endDataAll,row)
      
    }
    iterator <- iterator + 1
  }
}

head(endDataAll)


#Organize parameters for all simulations
masterParams <- list()
variableParams <- c()
paramNames <- c()
for (c in which(colnames(endDataAll) == "N"):which(colnames(endDataAll) == "fileSuffix")){
  masterParams[[c+1-which(colnames(endDataAll) == "N")]] <- unique(endDataAll[,c])
  paramNames <- c(paramNames,colnames(endDataAll)[c])
  if(length(unique(endDataAll[,c])) > 1 ){
    variableParams<- c(variableParams,colnames(endDataAll)[c])
  }
}
names(masterParams) <- paramNames
N <- as.numeric(masterParams$N)
###
print(variableParams)
masterParams


#### Start Plots ####
dataAll <- subset(endDataAll)
unique(dataAll$c2)
unique(dataAll$c1)
head(dataAll)
dataAll$c1c2 <- paste0(dataAll$c1,"_",dataAll$c2)

#Add column to order along x axis
collect2<-data.frame()
for (i in unique(dataAll$c1c2)){
  temp <- subset(dataAll,c1c2==i)
  iterator <- 1
  temp
  collect1 <- data.frame()
  for (j in unique(temp$fileNum)){
    temp2 <- subset(temp,fileNum==j)
    temp2$orderedFileNum <- iterator
    temp2
    collect1<-rbind(collect1,temp2)
    iterator <- iterator + 1
  }
  collect2<-rbind(collect2,collect1)
}
collect2$orderedFileNum

dataAllOrderedReduced <- collect2

head(dataAllOrderedReduced)
for (i in 1:nrow(dataAllOrderedReduced)){
  if (dataAllOrderedReduced[i,]$c2<=dataAllOrderedReduced[i,]$c1){
    if (dataAllOrderedReduced[i,]$indType=="Sender"){
      dataAllOrderedReduced[i,]$expAlphaBeta <- 1
    } else {
      dataAllOrderedReduced[i,]$expAlphaBeta <- 0
    }
  }
  else if (dataAllOrderedReduced[i,]$c2>=1 & dataAllOrderedReduced[i,]$c1 < 1){
    if (dataAllOrderedReduced[i,]$indType=="Sender"){
      dataAllOrderedReduced[i,]$expAlphaBeta <- 0
    } else {
      dataAllOrderedReduced[i,]$expAlphaBeta <- 1
    }
  }
}

dataAllOrderedReduced <- subset(dataAllOrderedReduced,orderedFileNum<=50)
head(dataAllOrderedReduced)
dataAllOrderedReduced$shortStrat <- ""
for (i in 1:nrow(dataAllOrderedReduced)){
  if (dataAllOrderedReduced[i,]$concatStrat=="Receiver_strat1"){
    dataAllOrderedReduced[i,]$shortStrat <- "RS1"
  }
  if (dataAllOrderedReduced[i,]$concatStrat=="Receiver_strat2"){
    dataAllOrderedReduced[i,]$shortStrat <- "RS2"
  }
  if (dataAllOrderedReduced[i,]$concatStrat=="Receiver_strat3"){
    dataAllOrderedReduced[i,]$shortStrat <- "RS3"
  }
  if (dataAllOrderedReduced[i,]$concatStrat=="Sender_strat1"){
    dataAllOrderedReduced[i,]$shortStrat <- "SS1"
  }
  if (dataAllOrderedReduced[i,]$concatStrat=="Sender_strat2"){
    dataAllOrderedReduced[i,]$shortStrat <- "SS2"
  }
  if (dataAllOrderedReduced[i,]$concatStrat=="Sender_strat3"){
    dataAllOrderedReduced[i,]$shortStrat <- "SS3"
  }
}



#First - equilibrium analysis for all combinations
ggplot(dataAllOrderedReduced,aes(x=orderedFileNum)) +
  geom_point(aes(y=meanStratFreq,color=stratType),alpha=.7) + 
  geom_point(aes(y=meanAlphaBeta),shape="X") +
  geom_line(aes(y=expAlphaBeta),color="orange",alpha=0.9,size=.8) +
  theme_bw() +
  facet_grid(c1+indType~c2) + 
  labs(x="Replicate",y="Strategy Freq or Alpha or Beta") +
  labs(subtitle = "c2") + 
  labs(color =  "c1\n\n\nStrategy") +
  labs(title = "Hybrid Equilibria Expectation")
ggsave("AllEquilibs.png",
       device="png",path=dir,height=8,width=10,unit="in")

#Next - look at mean fitnesses

ggplot(dataAllOrderedReduced,aes(x=as.factor(shortStrat))) +
  geom_jitter(aes(y=meanFit,color=shortStrat),alpha=.5,height=0) + 
  geom_jitter(aes(y=meanStratFreq),color="grey",alpha=.5,height=0) + 
  geom_jitter(aes(y=meanStratFreq),color="grey",alpha=.5,height=0) + 
  geom_jitter(aes(y=meanAlphaBeta),color="orange",alpha=.5,height=0,shape="X") +
  theme_bw() +
  facet_grid(c2~c1) +
  ylim(0,1) + 
  labs(title="Hybrid Expectation - Fitnesses") +
  labs(x="Strategy",y="Mean Fitness") +
  labs(subtitle="c1") +
  labs(color="c2\n\n\nStrategy") +
  labs(caption="Grey: Frequency of strategy\nOrange X: Alpha or Beta")

ggsave("HybridFitsAndEquilib.png",
       device="png",path=dir,height=8,width=10,unit="in")

#### Exper 2B More hybrid equilibs
ggplot(subset(dataAllOrderedReduced,(c2==0.3|c2==0.5|c2==0.7|c2==0.9|c1==0.1|
                                       c1==0.3|c1==0.5|c1==0.7) & c1!=0.25 & c1!= 0.75          ),aes(x=orderedFileNum)) +
  geom_point(aes(y=meanStratFreq,color=stratType),alpha=.7) + 
  geom_point(aes(y=meanAlphaBeta),shape="X") +
  geom_line(aes(y=expAlphaBeta),color="orange",alpha=0.9,size=.8) +
  theme_bw() +
  facet_grid(c2+indType~c1) + 
  labs(x="Replicate",y="Strategy Freq or Alpha or Beta") +
  labs(subtitle = "c1") + 
  labs(color =  "c2\n\n\nStrategy") +
  labs(title = "Hybrid Equilibria Expectation")

ggsave("HybridEquilibs_2B.png",
       device="png",path=dir,height=8,width=10,unit="in")



ggplot(subset(dataAllOrderedReduced,(c2==0.3|c2==0.5|c2==0.7|c2==0.9|c1==0.1|
                                       c1==0.3|c1==0.5|c1==0.7) & c1!=0.25 & c1!= 0.75),aes(x=as.factor(shortStrat))) +
  geom_jitter(aes(y=meanFit,color=shortStrat),alpha=.5,height=0) + 
  geom_jitter(aes(y=meanStratFreq),color="grey",alpha=.5,height=0) + 
  geom_jitter(aes(y=meanStratFreq),color="grey",alpha=.5,height=0) + 
  geom_jitter(aes(y=meanAlphaBeta),color="orange",alpha=.5,height=0,shape="X") +
  theme_bw() +
  facet_grid(c2~c1) +
  ylim(0,1) + 
  labs(title="Hybrid Expectation - Fitnesses") +
  labs(x="Strategy",y="Mean Fitness") +
  labs(subtitle="c1") +
  labs(color="c2\n\n\nStrategy") +
  labs(caption="Grey: Frequency of strategy\nOrange X: Alpha or Beta")

ggsave("HybridFitsAndEquilib_2B.png",
       device="png",path=dir,height=8,width=10,unit="in")








#### Investigating the expected honest replicates that didn't work. ####
# They are c1 .75 and c2 1 or 1.5

iterator <- 1
honestInvAll<-data.frame()
for (i in summaryFiles){
  
  dataAllReps <- read.csv(paste0(dir,"/",i))
  if (unique(dataAllReps$c1) == 0.75 & (unique(dataAllReps$c2) == 1.0 | unique(dataAllReps$c2) == 1.5)){
    
    
    for (r in unique(dataAllReps$rep)){
      data <- subset(dataAllReps,rep==r)
      
      data$fileNum <- iterator
      iterator <- iterator + 1 
      
      honestInvAll <- rbind(honestInvAll,data)
    }
    
  }
}
head(honestInvAll)
honestInvAll$concatStrat <- paste0(honestInvAll$indType,"_",honestInvAll$stratType)






honestInvAll$shortStrat <- ""
for (i in 1:nrow(honestInvAll)){
  if (honestInvAll[i,]$concatStrat=="Receiver_strat1"){
    honestInvAll[i,]$shortStrat <- "RS1"
  } else if (honestInvAll[i,]$concatStrat=="Receiver_strat2"){
    honestInvAll[i,]$shortStrat <- "RS2"
  } else if (honestInvAll[i,]$concatStrat=="Receiver_strat3"){
    honestInvAll[i,]$shortStrat <- "RS3"
  } else if (honestInvAll[i,]$concatStrat=="Sender_strat1"){
    honestInvAll[i,]$shortStrat <- "SS1"
  } else if (honestInvAll[i,]$concatStrat=="Sender_strat2"){
    honestInvAll[i,]$shortStrat <- "SS2"
  } else if (honestInvAll[i,]$concatStrat=="Sender_strat3"){
    honestInvAll[i,]$shortStrat <- "SS3"
  }
}

honestInvAll$c2
oneReps <- unique(subset(honestInvAll,c2==1.0)$fileNum)
onefiveReps <- unique(subset(honestInvAll,c2==1.5)$fileNum)

for (i in 1:25){
  d <- subset(honestInvAll,fileNum==onefiveReps[i])
  
  p<-ggplot(d,aes(x=gen)) +
    geom_point(aes(y=meanAlphaBeta),shape="X",size=2) +
    geom_point(aes(y=(stratNum/N),color=stratType)) +
    geom_point(aes(y=meanFit,color=stratType),shape="O") +
    labs(title=paste0("Honest Expected: c1 = ",d[1,]$c1,",  c2 = ",d[1,]$c2)) +
    ylim(0,1) + theme_bw() +
    labs(color="Strategy") +
    labs(x="Generation",y="Mean Alpha or Beta or Fitness or Strategy Frequency") +
    facet_grid(indType~.) +
    labs(caption="X: Mean alpha or beta\nO: Mean fitness\nDots: Strategy Frequencies") +
    labs(subtitle=paste0("Replicate: ",d$fileNum))
  ggsave(plot=p,paste0(unique(d$c2),"_rep",unique(d$fileNum),"_invHonest.png"),
         device="png",path=paste0(dir,"/invHonest"),height=8,width=10,unit="in")
  
  #Look at smaller time intervales - are there 'pulses' of 
  #mutants drifting in. Cycles of mutants invading
}

#### 2A - Small time scale ####
dir2A <- "C:/Users/owner/Documents/DONE/Exper_2A_FineScaleHonest"
summaryFiles2A <- list.files(dir2A,"*summaryStats*")

iterator <- 1
honestInvAll2A<-data.frame()
for (i in summaryFiles2A){
  
  dataAllReps2A <- read.csv(paste0(dir2A,"/",i))
  
  for (r in unique(dataAllReps2A$rep)){
    data <- subset(dataAllReps2A,rep==r)
    
    data$fileNum <- iterator
    iterator <- iterator + 1 
    
    honestInvAll2A <- rbind(honestInvAll2A,data)
  }
  
  
}


honestInvAll2A$concatStrat <- paste0(honestInvAll2A$indType,"_",honestInvAll2A$stratType)

#Skip this - it takes too long. Just run it when needed
honestInvAll2A$shortStrat <- ""
for (i in 1:nrow(honestInvAll2A)){
  if (honestInvAll2A[i,]$concatStrat=="Receiver_strat1"){
    honestInvAll2A[i,]$shortStrat <- "RS1"
  } else if (honestInvAll2A[i,]$concatStrat=="Receiver_strat2"){
    honestInvAll2A[i,]$shortStrat <- "RS2"
  } else if (honestInvAll2A[i,]$concatStrat=="Receiver_strat3"){
    honestInvAll2A[i,]$shortStrat <- "RS3"
  } else if (honestInvAll2A[i,]$concatStrat=="Sender_strat1"){
    honestInvAll2A[i,]$shortStrat <- "SS1"
  } else if (honestInvAll2A[i,]$concatStrat=="Sender_strat2"){
    honestInvAll2A[i,]$shortStrat <- "SS2"
  } else if (honestInvAll2A[i,]$concatStrat=="Sender_strat3"){
    honestInvAll2A[i,]$shortStrat <- "SS3"
  }
}


honestInvAll2A$c1_c2 <- paste0(honestInvAll2A$c1,"_",honestInvAll2A$c2)

unique(honestInvAll2A$c1_c2)
unique(subset(honestInvAll2A,c1_c2=="0.75_1.5")$fileNum)

sum(d$gen==150)
(max(d$gen)+1)*6
nrow(d)

for (i in unique(honestInvAll2A$fileNum)){
  d <- subset(honestInvAll2A,fileNum==i)
  
  p<-ggplot(d,aes(x=gen)) +
    geom_point(aes(y=meanAlphaBeta),shape="X",size=1) +
    geom_point(aes(y=(stratNum/N),color=stratType),size=1.3) +
    geom_point(aes(y=meanFit,color=stratType),shape="O",size=.5,alpha=.5) +
    labs(title=paste0("Honest Expected: c1 = ",d[1,]$c1,",  c2 = ",d[1,]$c2)) +
    ylim(0,1) + theme_bw() +
    labs(color="Strategy") +
    labs(x="Generation",y="Mean Alpha or Beta or Fitness or Strategy Frequency") +
    facet_grid(indType~.) +
    labs(caption="X: Mean alpha or beta\nO: Mean fitness\nDots: Strategy Frequencies") +
    labs(subtitle=paste0("Replicate: ",d$fileNum))
  ggsave(plot=p,paste0(unique(d$c1),"_",unique(d$c2),"_rep",unique(d$fileNum),"_smallScale.png"),
         device="png",path=paste0(dir2A,"/smallScale"),height=8,width=20,unit="in")
  
  
}




#Exper 2E - K tournament ####
dirExp2E <- "C:/Users/owner/Documents/DONE/Exper_2E"
summaryFilesExp2E <- list.files(dirExp2E,"*summaryStats*")

#First - create endData stats from summaryStats files produced by cpp
cutoff <- 0.25   #Look at last this % of generations
iterator <- 1
endDataAllExp2E<-data.frame()
for (i in summaryFilesExp2E){
  
  dataAllReps <- read.csv(paste0(dirExp2E,"/",i))
  
  for (r in unique(dataAllReps$rep)){
    data <- subset(dataAllReps,rep==r)
    
    data$fileName <- i
    dataEnd <- subset(data,gen>=((max(gen)-(cutoff*max(gen)))))
    dataEnd$concatStrat <- paste0(dataEnd$indType,"_",dataEnd$stratType)
    
    dataEndSenders <- subset(dataEnd,indType == "Sender")
    meanAlpha <- mean(dataEndSenders$meanAlphaBeta)
    dataEndReceivers <- subset(dataEnd,indType == "Receiver")
    meanBeta <- mean(dataEndReceivers$meanAlphaBeta)
    
    row <- data.frame()
    for (j in unique(dataEnd$concatStrat)){
      temp <- subset(dataEnd,concatStrat == j)
      row <- temp[1,]
      if (row$indType == "Receiver"){
        row$meanAlphaBeta <- meanBeta
      } else {
        row$meanAlphaBeta <- meanAlpha
      }
      
      
      row$fileSuffix <- gsub("_[0-9]","",gsub("_*..csv","",gsub(".*summaryStats_","",row$fileName)))
      row$meanFit <- mean(temp$meanFit)
      row$meanStratNum <- mean(temp$stratNum)
      row$meanStratFreq <- mean(temp$stratNum/row$N)
      row$stratNum <- "NA"
      row$gen <- "NA"
      row$fileNum <- iterator
      
      endDataAllExp2E <- rbind(endDataAllExp2E,row)
      
    }
    iterator <- iterator + 1
  }
}

head(endDataAllExp2E)

#Organize parameters for all simulations
masterParams <- list()
variableParams <- c()
paramNames <- c()
for (c in which(colnames(endDataAllExp2E) == "N"):which(colnames(endDataAllExp2E) == "fileSuffix")){
  masterParams[[c+1-which(colnames(endDataAllExp2E) == "N")]] <- unique(endDataAllExp2E[,c])
  paramNames <- c(paramNames,colnames(endDataAllExp2E)[c])
  if(length(unique(endDataAllExp2E[,c])) > 1 ){
    variableParams<- c(variableParams,colnames(endDataAllExp2E)[c])
  }
}
names(masterParams) <- paramNames
N <- as.numeric(masterParams$N)
###
print(variableParams)
masterParams



endDataAllExp2E$c1c2 <- paste0(endDataAllExp2E$c1,"_",endDataAllExp2E$c2)

#Add column to order along x axis
collect2<-data.frame()
for (i in unique(endDataAllExp2E$c1c2)){
  temp <- subset(endDataAllExp2E,c1c2==i)
  iterator <- 1
  temp
  collect1 <- data.frame()
  for (j in unique(temp$fileNum)){
    temp2 <- subset(temp,fileNum==j)
    temp2$orderedFileNum <- iterator
    temp2
    collect1<-rbind(collect1,temp2)
    iterator <- iterator + 1
  }
  collect2<-rbind(collect2,collect1)
}
collect2$orderedFileNum

endDataAllExp2EOrderedReduced <- collect2

head(endDataAllExp2EOrderedReduced)
for (i in 1:nrow(endDataAllExp2EOrderedReduced)){
  if (endDataAllExp2EOrderedReduced[i,]$c2<=endDataAllExp2EOrderedReduced[i,]$c1){
    if (endDataAllExp2EOrderedReduced[i,]$indType=="Sender"){
      endDataAllExp2EOrderedReduced[i,]$expAlphaBeta <- 1
    } else {
      endDataAllExp2EOrderedReduced[i,]$expAlphaBeta <- 0
    }
  }
  else if (endDataAllExp2EOrderedReduced[i,]$c2>=1 & endDataAllExp2EOrderedReduced[i,]$c1 < 1){
    if (endDataAllExp2EOrderedReduced[i,]$indType=="Sender"){
      endDataAllExp2EOrderedReduced[i,]$expAlphaBeta <- 0
    } else {
      endDataAllExp2EOrderedReduced[i,]$expAlphaBeta <- 1
    }
  }
}

#endDataAllExp2EOrderedReduced <- subset(endDataAllExp2EOrderedReduced,orderedFileNum<=50)

endDataAllExp2EOrderedReduced

library(ggplot2)
#install.packages("rlang")

#plot
ggplot(subset(endDataAllExp2EOrderedReduced,mutStepAlpha==0.25),aes(x=orderedFileNum)) +
  geom_point(aes(y=meanStratFreq,color=stratType),alpha=.7) + 
  geom_point(aes(y=meanAlphaBeta),shape="X") +
  geom_line(aes(y=expAlphaBeta),color="orange",alpha=0.9,size=.8) +
  theme_bw() +
  facet_grid(c1+indType~c2) + 
  labs(x="Replicate",y="Strategy Freq or Alpha or Beta") +
  labs(subtitle = "c2") + 
  labs(color =  "c1\n\n\nStrategy") +
  labs(title = "NewNorm")
ggsave("kTorunHighC.png",
       device="png",path=dir,height=8,width=10,unit="in")

#Next - look at mean fitnesses
head(endDataAllExp2EOrderedReduced)
endDataAllExp2EOrderedReduced$shortStrat <- ""
for (i in 1:nrow(endDataAllExp2EOrderedReduced)){
  if (endDataAllExp2EOrderedReduced[i,]$concatStrat=="Receiver_strat1"){
    endDataAllExp2EOrderedReduced[i,]$shortStrat <- "RS1"
  }
  if (endDataAllExp2EOrderedReduced[i,]$concatStrat=="Receiver_strat2"){
    endDataAllExp2EOrderedReduced[i,]$shortStrat <- "RS2"
  }
  if (endDataAllExp2EOrderedReduced[i,]$concatStrat=="Receiver_strat3"){
    endDataAllExp2EOrderedReduced[i,]$shortStrat <- "RS3"
  }
  if (endDataAllExp2EOrderedReduced[i,]$concatStrat=="Sender_strat1"){
    endDataAllExp2EOrderedReduced[i,]$shortStrat <- "SS1"
  }
  if (endDataAllExp2EOrderedReduced[i,]$concatStrat=="Sender_strat2"){
    endDataAllExp2EOrderedReduced[i,]$shortStrat <- "SS2"
  }
  if (endDataAllExp2EOrderedReduced[i,]$concatStrat=="Sender_strat3"){
    endDataAllExp2EOrderedReduced[i,]$shortStrat <- "SS3"
  }
}

ggplot(endDataAllExp2EOrderedReduced,aes(x=as.factor(shortStrat))) +
  geom_jitter(aes(y=meanFit,color=shortStrat),alpha=.5,height=0) + 
  #geom_jitter(aes(y=meanStratFreq),color="grey",alpha=.5,height=0) + 
  #geom_jitter(aes(y=meanStratFreq),color="grey",alpha=.5,height=0) + 
  #geom_jitter(aes(y=meanAlphaBeta),color="orange",alpha=.5,height=0,shape="X") +
  theme_bw() +
  facet_grid(c2~c1) +
  # ylim(0,1) + 
  labs(title="Hybrid Expectation - Fitnesses") +
  labs(x="Strategy",y="Mean Fitness") +
  labs(subtitle="c1") +
  labs(color="c2\n\n\nStrategy") +
  labs(caption="Grey: Frequency of strategy\nOrange X: Alpha or Beta")

ggsave("NegCostFitsAndEquilib.png",
       device="png",path=dir,height=8,width=10,unit="in")


####Exper 2E sub_General - K tournament ####
dirExp2E_subGeneral <- "C:/Users/owner/Documents/DONE/Exper_2E/sub_General"
summaryFilesExp2E_subGeneral <- list.files(dirExp2E_subGeneral,"*summaryStats*")

#First - create endData stats from summaryStats files produced by cpp
cutoff <- 0.25   #Look at last this % of generations
iterator <- 1
endDataAllExp2E_subGeneral<-data.frame()
for (i in summaryFilesExp2E_subGeneral){
  
  dataAllReps <- read.csv(paste0(dirExp2E_subGeneral,"/",i))
  
  for (r in unique(dataAllReps$rep)){
    data <- subset(dataAllReps,rep==r)
    
    data$fileName <- i
    dataEnd <- subset(data,gen>=((max(gen)-(cutoff*max(gen)))))
    dataEnd$concatStrat <- paste0(dataEnd$indType,"_",dataEnd$stratType)
    
    dataEndSenders <- subset(dataEnd,indType == "Sender")
    meanAlpha <- mean(dataEndSenders$meanAlphaBeta)
    dataEndReceivers <- subset(dataEnd,indType == "Receiver")
    meanBeta <- mean(dataEndReceivers$meanAlphaBeta)
    
    row <- data.frame()
    for (j in unique(dataEnd$concatStrat)){
      temp <- subset(dataEnd,concatStrat == j)
      row <- temp[1,]
      if (row$indType == "Receiver"){
        row$meanAlphaBeta <- meanBeta
      } else {
        row$meanAlphaBeta <- meanAlpha
      }
      
      
      row$fileSuffix <- gsub("_[0-9]","",gsub("_*..csv","",gsub(".*summaryStats_","",row$fileName)))
      row$meanFit <- mean(temp$meanFit)
      row$meanStratNum <- mean(temp$stratNum)
      row$meanStratFreq <- mean(temp$stratNum/row$N)
      row$stratNum <- "NA"
      row$gen <- "NA"
      row$fileNum <- iterator
      
      endDataAllExp2E_subGeneral <- rbind(endDataAllExp2E_subGeneral,row)
      
    }
    iterator <- iterator + 1
  }
}

head(endDataAllExp2E_subGeneral)

#Organize parameters for all simulations
masterParams <- list()
variableParams <- c()
paramNames <- c()
for (c in which(colnames(endDataAllExp2E_subGeneral) == "N"):which(colnames(endDataAllExp2E_subGeneral) == "fileSuffix")){
  masterParams[[c+1-which(colnames(endDataAllExp2E_subGeneral) == "N")]] <- unique(endDataAllExp2E_subGeneral[,c])
  paramNames <- c(paramNames,colnames(endDataAllExp2E_subGeneral)[c])
  if(length(unique(endDataAllExp2E_subGeneral[,c])) > 1 ){
    variableParams<- c(variableParams,colnames(endDataAllExp2E_subGeneral)[c])
  }
}
names(masterParams) <- paramNames
N <- as.numeric(masterParams$N)
###
print(variableParams)
masterParams



endDataAllExp2E_subGeneral$c1c2 <- paste0(endDataAllExp2E_subGeneral$c1,"_",endDataAllExp2E_subGeneral$c2)


#Add column to order along x axis
collect2<-data.frame()
for (i in unique(paste0(endDataAllExp2E_subGeneral$m,"_",endDataAllExp2E_subGeneral$c1c2))){
  temp <- subset(endDataAllExp2E_subGeneral,paste0(endDataAllExp2E_subGeneral$m,"_",endDataAllExp2E_subGeneral$c1c2)==i)
  iterator <- 1
  temp
  collect1 <- data.frame()
  for (j in unique(temp$fileNum)){
    temp2 <- subset(temp,fileNum==j)
    temp2$orderedFileNum <- iterator
    temp2
    collect1<-rbind(collect1,temp2)
    iterator <- iterator + 1
  }
  collect2<-rbind(collect2,collect1)
}
collect2$orderedFileNum

endDataAllExp2E_subGeneralOrderedReduced <- collect2

head(endDataAllExp2E_subGeneralOrderedReduced)
for (i in 1:nrow(endDataAllExp2E_subGeneralOrderedReduced)){
  if (endDataAllExp2E_subGeneralOrderedReduced[i,]$c2<=endDataAllExp2E_subGeneralOrderedReduced[i,]$c1){
    if (endDataAllExp2E_subGeneralOrderedReduced[i,]$indType=="Sender"){
      endDataAllExp2E_subGeneralOrderedReduced[i,]$expAlphaBeta <- 1
    } else {
      endDataAllExp2E_subGeneralOrderedReduced[i,]$expAlphaBeta <- 0
    }
  }
  else if (endDataAllExp2E_subGeneralOrderedReduced[i,]$c2>=1 & endDataAllExp2E_subGeneralOrderedReduced[i,]$c1 < 1){
    if (endDataAllExp2E_subGeneralOrderedReduced[i,]$indType=="Sender"){
      endDataAllExp2E_subGeneralOrderedReduced[i,]$expAlphaBeta <- 0
    } else {
      endDataAllExp2E_subGeneralOrderedReduced[i,]$expAlphaBeta <- 1
    }
  }
}

#endDataAllExp2E_subGeneralOrderedReduced <- subset(endDataAllExp2E_subGeneralOrderedReduced,orderedFileNum<=50)

endDataAllExp2E_subGeneralOrderedReduced

library(ggplot2)
#install.packages("rlang")

unique(endDataAllExp2E_subGeneralOrderedReduced$c1)
unique(endDataAllExp2E_subGeneralOrderedReduced$c2)

variableParams

#plot
ggplot(subset(endDataAllExp2E_subGeneralOrderedReduced,c1<0.9),aes(x=orderedFileNum)) +
  geom_point(aes(y=meanStratFreq,color=stratType),alpha=.7) + 
  geom_point(aes(y=meanAlphaBeta),shape="X") +
  geom_line(aes(y=expAlphaBeta),color="orange",alpha=0.9,size=.8) +
  theme_bw() +
  facet_grid(c1+indType~m+c2) + 
  labs(x="Replicate",y="Strategy Freq or Alpha or Beta") +
  labs(subtitle = "m\nc2") + 
  labs(color =  "c1\n\n\nStrategy") +
  labs(title = "NewNorm")
ggsave("c1_c2_m.png",
       device="png",path=dirExp2E_subGeneral,height=8,width=12,unit="in")

#Next - look at mean fitnesses
head(endDataAllExp2E_subGeneralOrderedReduced)
endDataAllExp2E_subGeneralOrderedReduced$shortStrat <- ""
for (i in 1:nrow(endDataAllExp2E_subGeneralOrderedReduced)){
  if (endDataAllExp2E_subGeneralOrderedReduced[i,]$concatStrat=="Receiver_strat1"){
    endDataAllExp2E_subGeneralOrderedReduced[i,]$shortStrat <- "RS1"
  }
  if (endDataAllExp2E_subGeneralOrderedReduced[i,]$concatStrat=="Receiver_strat2"){
    endDataAllExp2E_subGeneralOrderedReduced[i,]$shortStrat <- "RS2"
  }
  if (endDataAllExp2E_subGeneralOrderedReduced[i,]$concatStrat=="Receiver_strat3"){
    endDataAllExp2E_subGeneralOrderedReduced[i,]$shortStrat <- "RS3"
  }
  if (endDataAllExp2E_subGeneralOrderedReduced[i,]$concatStrat=="Sender_strat1"){
    endDataAllExp2E_subGeneralOrderedReduced[i,]$shortStrat <- "SS1"
  }
  if (endDataAllExp2E_subGeneralOrderedReduced[i,]$concatStrat=="Sender_strat2"){
    endDataAllExp2E_subGeneralOrderedReduced[i,]$shortStrat <- "SS2"
  }
  if (endDataAllExp2E_subGeneralOrderedReduced[i,]$concatStrat=="Sender_strat3"){
    endDataAllExp2E_subGeneralOrderedReduced[i,]$shortStrat <- "SS3"
  }
}

ggplot(endDataAllExp2E_subGeneralOrderedReduced,aes(x=as.factor(shortStrat))) +
  geom_jitter(aes(y=meanFit,color=shortStrat),alpha=.5,height=0) + 
  #geom_jitter(aes(y=meanStratFreq),color="grey",alpha=.5,height=0) + 
  #geom_jitter(aes(y=meanStratFreq),color="grey",alpha=.5,height=0) + 
  #geom_jitter(aes(y=meanAlphaBeta),color="orange",alpha=.5,height=0,shape="X") +
  theme_bw() +
  facet_grid(c2~c1) +
  # ylim(0,1) + 
  labs(title="Hybrid Expectation - Fitnesses") +
  labs(x="Strategy",y="Mean Fitness") +
  labs(subtitle="c1") +
  labs(color="c2\n\n\nStrategy") +
  labs(caption="Grey: Frequency of strategy\nOrange X: Alpha or Beta")

ggsave("NegCostFitsAndEquilib.png",
       device="png",path=dir,height=8,width=10,unit="in")



####Exper 2E sub_Quick - K tournament ####
dirExp2E_subQuick <- "C:/Users/owner/Documents/DONE/Exper_2E/sub_quick"
summaryFilesExp2E_subQuick <- list.files(dirExp2E_subQuick,"*summaryStats*")

#First - create endData stats from summaryStats files produced by cpp
cutoff <- 0.25   #Look at last this % of generations
iterator <- 1
endDataAllExp2E_subQuick<-data.frame()
for (i in summaryFilesExp2E_subQuick){
  
  dataAllReps <- read.csv(paste0(dirExp2E_subQuick,"/",i))
  
  for (r in unique(dataAllReps$rep)){
    data <- subset(dataAllReps,rep==r)
    
    data$fileName <- i
    dataEnd <- subset(data,gen>=((max(gen)-(cutoff*max(gen)))))
    dataEnd$concatStrat <- paste0(dataEnd$indType,"_",dataEnd$stratType)
    
    dataEndSenders <- subset(dataEnd,indType == "Sender")
    meanAlpha <- mean(dataEndSenders$meanAlphaBeta)
    dataEndReceivers <- subset(dataEnd,indType == "Receiver")
    meanBeta <- mean(dataEndReceivers$meanAlphaBeta)
    
    row <- data.frame()
    for (j in unique(dataEnd$concatStrat)){
      temp <- subset(dataEnd,concatStrat == j)
      row <- temp[1,]
      if (row$indType == "Receiver"){
        row$meanAlphaBeta <- meanBeta
      } else {
        row$meanAlphaBeta <- meanAlpha
      }
      
      #remove NA from mean fit - it messes things up later... Just make 0s
      dataEnd$meanFit <- replace(dataEnd$meanFit, dataEnd$meanFit == "NaN", 0)
      
      row$fileSuffix <- gsub("_[0-9]","",gsub("_*..csv","",gsub(".*summaryStats_","",row$fileName)))
      row$meanFit <- mean(temp$meanFit)
      row$meanStratNum <- mean(temp$stratNum)
      row$meanStratFreq <- mean(temp$stratNum/row$N)
      row$stratNum <- "NA"
      row$gen <- "NA"
      row$fileNum <- iterator
      
      endDataAllExp2E_subQuick <- rbind(endDataAllExp2E_subQuick,row)
      
    }
    iterator <- iterator + 1
  }
}

head(endDataAllExp2E_subQuick)

#Organize parameters for all simulations
masterParams <- list()
variableParams <- c()
paramNames <- c()
for (c in which(colnames(endDataAllExp2E_subQuick) == "N"):which(colnames(endDataAllExp2E_subQuick) == "fileSuffix")){
  masterParams[[c+1-which(colnames(endDataAllExp2E_subQuick) == "N")]] <- unique(endDataAllExp2E_subQuick[,c])
  paramNames <- c(paramNames,colnames(endDataAllExp2E_subQuick)[c])
  if(length(unique(endDataAllExp2E_subQuick[,c])) > 1 ){
    variableParams<- c(variableParams,colnames(endDataAllExp2E_subQuick)[c])
  }
}
names(masterParams) <- paramNames
N <- as.numeric(masterParams$N)
###
print(variableParams)
masterParams



endDataAllExp2E_subQuick$c1c2 <- paste0(endDataAllExp2E_subQuick$c1,"_",endDataAllExp2E_subQuick$c2)
endDataAllExp2E_subQuick$lab <- paste0(endDataAllExp2E_subQuick$c1,
                                       "_",endDataAllExp2E_subQuick$m,
                                       "_",endDataAllExp2E_subQuick$initializationType,
                                       "_",endDataAllExp2E_subQuick$cauchyDist,
                                       "_",endDataAllExp2E_subQuick$mutRateAlpha,
                                       "_",endDataAllExp2E_subQuick$mutRateStrategySender,
                                       "_",endDataAllExp2E_subQuick$c2
)

#Add column to order along x axis
collect2<-data.frame()
for (i in unique(endDataAllExp2E_subQuick$lab)){
  temp <- subset(endDataAllExp2E_subQuick,endDataAllExp2E_subQuick$lab==i)
  iterator <- 1
  temp
  collect1 <- data.frame()
  for (j in unique(temp$fileNum)){
    temp2 <- subset(temp,fileNum==j)
    temp2$orderedFileNum <- iterator
    temp2
    collect1<-rbind(collect1,temp2)
    iterator <- iterator + 1
  }
  collect2<-rbind(collect2,collect1)
}
collect2$orderedFileNum

endDataAllExp2E_subQuickOrderedReduced <- collect2

head(endDataAllExp2E_subQuickOrderedReduced)
for (i in 1:nrow(endDataAllExp2E_subQuickOrderedReduced)){
  if (endDataAllExp2E_subQuickOrderedReduced[i,]$c2<=endDataAllExp2E_subQuickOrderedReduced[i,]$c1){
    if (endDataAllExp2E_subQuickOrderedReduced[i,]$indType=="Sender"){
      endDataAllExp2E_subQuickOrderedReduced[i,]$expAlphaBeta <- 1
    } else {
      endDataAllExp2E_subQuickOrderedReduced[i,]$expAlphaBeta <- 0
    }
  }
  else if (endDataAllExp2E_subQuickOrderedReduced[i,]$c2>=1 & endDataAllExp2E_subQuickOrderedReduced[i,]$c1 < 1){
    if (endDataAllExp2E_subQuickOrderedReduced[i,]$indType=="Sender"){
      endDataAllExp2E_subQuickOrderedReduced[i,]$expAlphaBeta <- 0
    } else {
      endDataAllExp2E_subQuickOrderedReduced[i,]$expAlphaBeta <- 1
    }
  }
}

#endDataAllExp2E_subQuickOrderedReduced <- subset(endDataAllExp2E_subQuickOrderedReduced,orderedFileNum<=50)

endDataAllExp2E_subQuickOrderedReduced

library(ggplot2)
#install.packages("rlang")

unique(endDataAllExp2E_subQuickOrderedReduced$c1)
unique(endDataAllExp2E_subQuickOrderedReduced$c2)

variableParams

#plot
for (xm in unique(endDataAllExp2E_subQuickOrderedReduced$m)){
  for (xmutRateAlpha in unique(endDataAllExp2E_subQuickOrderedReduced$mutRateAlpha)){
    for (xmutRateStrategySender in unique(endDataAllExp2E_subQuickOrderedReduced$mutRateStrategySender)){
      for (xcauchyDist in unique(endDataAllExp2E_subQuickOrderedReduced$cauchyDist)){
        
        d <- subset(endDataAllExp2E_subQuickOrderedReduced,m==xm & cauchyDist==xcauchyDist & mutRateAlpha==xmutRateAlpha & mutRateStrategySender==xmutRateStrategySender)
        
        p<-ggplot(d,aes(x=orderedFileNum)) +
          geom_point(aes(y=meanStratFreq,color=stratType),alpha=.7) + 
          geom_point(aes(y=meanAlphaBeta),shape="X") +
          geom_line(aes(y=expAlphaBeta),color="orange",alpha=0.9,size=.5) +
          theme_bw() +
          facet_grid(c1+indType~c2+initializationType+cauchyDist) + 
          labs(x="Replicate",y="Strategy Freq or Alpha or Beta") +
          labs(subtitle = "m\ninitializationType\nmutRateAlpha\nmutRateStrategySender\nc2") + 
          labs(color =  "c1\n\n\nStrategy") +
          labs(title = paste0("m: ",xm,"   mutRateAlpha: ",xmutRateAlpha,"   mutRateStrategy: ",xmutRateStrategySender,"   cauchyDist: ",xcauchyDist,".png"))
        
        ggsave(plot=p,paste0(xcauchyDist,"_",xm,"_",xmutRateAlpha,"_",xmutRateStrategySender,".png"),
               device="png",path=dirExp2E_subQuick,height=8,width=12,unit="in")
        
      }
    }
  }
}

variableParams


#### #### 2E_quick equilibria ####
#for (tol in c(0.05,0.1,0.25,0.4)){
tol <- 0.25
tolerance <- tol

head(endDataAllExp2E_subQuickOrderedReduced)
dAll <- endDataAllExp2E_subQuickOrderedReduced
head(dAll)
dAll$devAlphaBeta <- dAll$meanAlphaBeta-dAll$expAlphaBeta
equilibs <- data.frame()

i <- 1
while (i <= nrow(dAll)){
  #determine dominant sender and receiver strat
  row <- dAll[i,]
  
  #determine expected dominant strategies
  
  #senders
  s<-dAll[i:(i+2),]
  #dominant sender strat
  domSendStrat <- s[which(s$meanStratNum==max(s$meanStratNum)),]$stratType
  devAlpha <- unique(s$meanAlphaBeta - s$expAlphaBeta)
  alpha <- unique(s$meanAlphaBeta)
  meanSendFit <- mean(s$meanFit * s$meanStratFreq)
  #Also find out out the strategy deviation?
  
  #receivers
  r<-dAll[(i+3):(i+5),]
  
  domRecStrat <- r[which(r$meanStratNum==max(r$meanStratNum)),]$stratType
  devBeta <- unique(r$meanAlphaBeta - r$expAlphaBeta)
  beta <- unique(r$meanAlphaBeta)
  meanRecFit <- mean(r$meanFit * r$meanStratFreq)
  r
  meanRecFit
  
  if (domRecStrat == "strat1" & domSendStrat == "strat1"){
    if (beta >= (1 - tolerance) & alpha <= tolerance){
      equilib <- "Honest"
      #What if it falls within honest but is closer to hybrid?
      if (beta < (1 - (tolerance/2)) & alpha > (tolerance/2)){
        equilib <- "Hybrid"
      }
    } else if (abs(devBeta) < tolerance &  abs(devAlpha) < tolerance & unique(s$expAlphaBeta) > 0 & unique(r$expAlphaBeta) < 1){
      equilib <- "Hybrid"
    } else {
      equilib<-"Pooling"
    }
  } else {
    equilib <- "Pooling"
  }
  
  row$domSendStrat <- domSendStrat
  row$domRecStrat <- domRecStrat
  row$meanSendFit <- meanSendFit
  row$meanRecFit <- meanRecFit
  row$devAlpha <- devAlpha
  row$devBeta <- devBeta
  row$alpha <- alpha
  row$beta <- beta
  row$equilib <- equilib
  equilibs <- rbind(equilibs,row)
  
  i <- i + 6  
}



for (xm in unique(endDataAllExp2E_subQuickOrderedReduced$m)){
  for (xmutRateAlpha in unique(endDataAllExp2E_subQuickOrderedReduced$mutRateAlpha)){
    for (xmutRateStrategySender in unique(endDataAllExp2E_subQuickOrderedReduced$mutRateStrategySender)){
      for (xcauchyDist in unique(endDataAllExp2E_subQuickOrderedReduced$cauchyDist)){
        
        d <- subset(equilibs,m==xm & cauchyDist==xcauchyDist & mutRateAlpha==xmutRateAlpha & mutRateStrategySender==xmutRateStrategySender)
        #here...        
        
        #get mean of mean fitnesses...
        d_post<-data.frame()
        for (u in unique(paste(d$c1,d$c2,d$initializationType,d$equilib,sep="_"))){
          dTemp <- subset(d,paste(d$c1,d$c2,d$initializationType,d$equilib,sep="_")==u)
          dTemp$meanMeanSendFit <- mean(dTemp$meanSendFit)
          dTemp$meanMeanRecFit <- mean(dTemp$meanRecFit)
          d_post <- rbind(d_post,dTemp)
        }
        d_post
        d
        
        p<-  ggplot(d_post,aes(x=as.factor(equilib))) +
          geom_bar(aes(group=as.factor(equilib),fill=as.factor(equilib))) +
          theme_bw() +
          facet_grid(c1~initializationType+c2) +
          labs(x="Equilibrium Reached",y="Number of Replicates") +
          labs(subtitle = "tolerance = 0.25\ninitialization condition\nc2") + 
          labs(fill =  "c1\n\n\n\nEquilibrium") +
          labs(title = paste0("m: ",xm,"   mutRateAlpha: ",xmutRateAlpha,"   mutRateStrategy: ",xmutRateStrategySender,"   cauchyDist: ",xcauchyDist))
        
        p2 <- p + geom_text(aes(label=round(meanMeanSendFit,2),y=2)) +
          geom_text(aes(label=round(meanMeanRecFit,2),y=6)) +
          labs(caption="Top: Mean Receiver Fitness\nBottom: Mean Sender Fitness")
        
        #Good!
        
        ggsave(plot=p2,paste0(xcauchyDist,"_",xm,"_",xmutRateAlpha,"_",xmutRateStrategySender,".png"),
               device="png",path=paste0(dirExp2E_subQuick,"/equilibsFits"),height=8,width=10,unit="in")
        #Issue - it looks like receiver fitness at pooling equilib = m
        ##It should be 1 - m!!! 
        
        
      }
    }
  }
}


for (xc1 in unique(endDataAllExp2E_subQuickOrderedReduced$c1)){
  for (xc2 in unique(endDataAllExp2E_subQuickOrderedReduced$c2)){
    for (xcauchyDist in unique(endDataAllExp2E_subQuickOrderedReduced$cauchyDist)){
      
      d <- subset(equilibs,c1==xc1 & c2 == xc2 & cauchyDist==xcauchyDist)
      
      
      p<- ggplot(d,aes(x=as.factor(equilib))) +
        geom_bar(aes(group=as.factor(equilib),fill=as.factor(equilib))) +
        theme_bw() +
        facet_grid(mutRateAlpha~initializationType+mutRateStrategySender) +
        labs(x="Equilibrium Reached",y="Number of Replicates") +
        labs(subtitle = "tolerance = 0.25\ninitialization condition\nmutRateStrategy") + 
        labs(fill =  "mutRateAlpha\n\n\n\nEquilibrium") +
        labs(title = paste0("c1: ",xc1,"   c2: ",xc2,"   cauchyDist: ",xcauchyDist))
      
      
      ggsave(plot=p,paste0(xc1,"_",xc2,"_",xcauchyDist,".png"),
             device="png",path=paste0(dirExp2E_subQuick,"/equilibs2"),height=8,width=10,unit="in")
      
      
      
    }
  }
}


#Next - look at mean fitnesses
head(endDataAllExp2E_subQuickOrderedReduced)
endDataAllExp2E_subQuickOrderedReduced$shortStrat <- ""
for (i in 1:nrow(endDataAllExp2E_subQuickOrderedReduced)){
  if (endDataAllExp2E_subQuickOrderedReduced[i,]$concatStrat=="Receiver_strat1"){
    endDataAllExp2E_subQuickOrderedReduced[i,]$shortStrat <- "RS1"
  }
  if (endDataAllExp2E_subQuickOrderedReduced[i,]$concatStrat=="Receiver_strat2"){
    endDataAllExp2E_subQuickOrderedReduced[i,]$shortStrat <- "RS2"
  }
  if (endDataAllExp2E_subQuickOrderedReduced[i,]$concatStrat=="Receiver_strat3"){
    endDataAllExp2E_subQuickOrderedReduced[i,]$shortStrat <- "RS3"
  }
  if (endDataAllExp2E_subQuickOrderedReduced[i,]$concatStrat=="Sender_strat1"){
    endDataAllExp2E_subQuickOrderedReduced[i,]$shortStrat <- "SS1"
  }
  if (endDataAllExp2E_subQuickOrderedReduced[i,]$concatStrat=="Sender_strat2"){
    endDataAllExp2E_subQuickOrderedReduced[i,]$shortStrat <- "SS2"
  }
  if (endDataAllExp2E_subQuickOrderedReduced[i,]$concatStrat=="Sender_strat3"){
    endDataAllExp2E_subQuickOrderedReduced[i,]$shortStrat <- "SS3"
  }
}

ggplot(endDataAllExp2E_subQuickOrderedReduced,aes(x=as.factor(shortStrat))) +
  geom_jitter(aes(y=meanFit,color=shortStrat),alpha=.5,height=0) + 
  #geom_jitter(aes(y=meanStratFreq),color="grey",alpha=.5,height=0) + 
  #geom_jitter(aes(y=meanStratFreq),color="grey",alpha=.5,height=0) + 
  #geom_jitter(aes(y=meanAlphaBeta),color="orange",alpha=.5,height=0,shape="X") +
  theme_bw() +
  facet_grid(c2~c1) +
  # ylim(0,1) + 
  labs(title="Hybrid Expectation - Fitnesses") +
  labs(x="Strategy",y="Mean Fitness") +
  labs(subtitle="c1") +
  labs(color="c2\n\n\nStrategy") +
  labs(caption="Grey: Frequency of strategy\nOrange X: Alpha or Beta")

ggsave("NegCostFitsAndEquilib.png",
       device="png",path=dir,height=8,width=10,unit="in")




####Exper 2E sub_invest - K tournament ####
dirExp2E_subInvest <- "C:/Users/owner/Documents/DONE/Exper_2E/sub_Investigate"
summaryFilesExp2E_subInvest <- list.files(dirExp2E_subInvest,"*summaryStats*")
#First - create endData stats from summaryStats files produced by cpp
cutoff <- 0.25   #Look at last this % of generations
iterator <- 1
endDataAllExp2E_subInvest<-data.frame()
for (i in summaryFilesExp2E_subInvest){
  
  dataAllReps <- read.csv(paste0(dirExp2E_subInvest,"/",i))
  
  for (r in unique(dataAllReps$rep)){
    data <- subset(dataAllReps,rep==r)
    
    data$fileName <- i
    dataEnd <- subset(data,gen>=((max(gen)-(cutoff*max(gen)))))
    dataEnd$concatStrat <- paste0(dataEnd$indType,"_",dataEnd$stratType)
    
    dataEndSenders <- subset(dataEnd,indType == "Sender")
    meanAlpha <- mean(dataEndSenders$meanAlphaBeta)
    dataEndReceivers <- subset(dataEnd,indType == "Receiver")
    meanBeta <- mean(dataEndReceivers$meanAlphaBeta)
    
    row <- data.frame()
    for (j in unique(dataEnd$concatStrat)){
      temp <- subset(dataEnd,concatStrat == j)
      row <- temp[1,]
      if (row$indType == "Receiver"){
        row$meanAlphaBeta <- meanBeta
      } else {
        row$meanAlphaBeta <- meanAlpha
      }
      
      
      row$fileSuffix <- gsub("_[0-9]","",gsub("_*..csv","",gsub(".*summaryStats_","",row$fileName)))
      row$meanFit <- mean(temp$meanFit)
      row$meanStratNum <- mean(temp$stratNum)
      row$meanStratFreq <- mean(temp$stratNum/row$N)
      row$stratNum <- "NA"
      row$gen <- "NA"
      row$fileNum <- iterator
      
      endDataAllExp2E_subInvest <- rbind(endDataAllExp2E_subInvest,row)
      
    }
    iterator <- iterator + 1
  }
}

head(endDataAllExp2E_subInvest)

#Organize parameters for all simulations
masterParams <- list()
variableParams <- c()
paramNames <- c()
for (c in which(colnames(endDataAllExp2E_subInvest) == "N"):which(colnames(endDataAllExp2E_subInvest) == "fileSuffix")){
  masterParams[[c+1-which(colnames(endDataAllExp2E_subInvest) == "N")]] <- unique(endDataAllExp2E_subInvest[,c])
  paramNames <- c(paramNames,colnames(endDataAllExp2E_subInvest)[c])
  if(length(unique(endDataAllExp2E_subInvest[,c])) > 1 ){
    variableParams<- c(variableParams,colnames(endDataAllExp2E_subInvest)[c])
  }
}
names(masterParams) <- paramNames
N <- as.numeric(masterParams$N)
###
print(variableParams)
masterParams



endDataAllExp2E_subInvest$lab <- paste0(endDataAllExp2E_subInvest$c1,"_",
                                        endDataAllExp2E_subInvest$c2,"_",
                                        endDataAllExp2E_subInvest$N,"_",
                                        endDataAllExp2E_subInvest$m,"_",
                                        endDataAllExp2E_subInvest$mutRateAlpha,"_",
                                        endDataAllExp2E_subInvest$mutRateStrategySender,"_",
                                        endDataAllExp2E_subInvest$initializationType )




#Add column to order along x axis
collect2<-data.frame()
for (i in unique(endDataAllExp2E_subInvest$lab)){
  temp <- subset(endDataAllExp2E_subInvest,endDataAllExp2E_subInvest$lab==i)
  iterator <- 1
  temp
  collect1 <- data.frame()
  for (j in unique(temp$fileNum)){
    temp2 <- subset(temp,fileNum==j)
    temp2$orderedFileNum <- iterator
    temp2
    collect1<-rbind(collect1,temp2)
    iterator <- iterator + 1
  }
  collect2<-rbind(collect2,collect1)
}
collect2$orderedFileNum

endDataAllExp2E_subInvestOrderedReduced <- collect2

head(endDataAllExp2E_subInvestOrderedReduced)
for (i in 1:nrow(endDataAllExp2E_subInvestOrderedReduced)){
  if (endDataAllExp2E_subInvestOrderedReduced[i,]$c2<=endDataAllExp2E_subInvestOrderedReduced[i,]$c1){
    if (endDataAllExp2E_subInvestOrderedReduced[i,]$indType=="Sender"){
      endDataAllExp2E_subInvestOrderedReduced[i,]$expAlphaBeta <- 1
    } else {
      endDataAllExp2E_subInvestOrderedReduced[i,]$expAlphaBeta <- 0
    }
  }
  else if (endDataAllExp2E_subInvestOrderedReduced[i,]$c2>=1 & endDataAllExp2E_subInvestOrderedReduced[i,]$c1 < 1){
    if (endDataAllExp2E_subInvestOrderedReduced[i,]$indType=="Sender"){
      endDataAllExp2E_subInvestOrderedReduced[i,]$expAlphaBeta <- 0
    } else {
      endDataAllExp2E_subInvestOrderedReduced[i,]$expAlphaBeta <- 1
    }
  }
}

#endDataAllExp2E_subInvestOrderedReduced <- subset(endDataAllExp2E_subInvestOrderedReduced,orderedFileNum<=50)

endDataAllExp2E_subInvestOrderedReduced

library(ggplot2)
#install.packages("rlang")

unique(endDataAllExp2E_subInvestOrderedReduced$c1)
unique(endDataAllExp2E_subInvestOrderedReduced$c2)

variableParams

#plot
ggplot(subset(endDataAllExp2E_subInvestOrderedReduced,c1<0.9),aes(x=orderedFileNum)) +
  geom_point(aes(y=meanStratFreq,color=stratType),alpha=.7) + 
  geom_point(aes(y=meanAlphaBeta),shape="X") +
  geom_line(aes(y=expAlphaBeta),color="orange",alpha=0.9,size=.8) +
  theme_bw() +
  facet_grid(c1+indType~m+c2) + 
  labs(x="Replicate",y="Strategy Freq or Alpha or Beta") +
  labs(subtitle = "m\nc2") + 
  labs(color =  "c1\n\n\nStrategy") +
  labs(title = "NewNorm")
ggsave("c1_c2_m.png",
       device="png",path=dirExp2E_subInvest,height=8,width=12,unit="in")

#Next - look at mean fitnesses
head(endDataAllExp2E_subInvestOrderedReduced)
endDataAllExp2E_subInvestOrderedReduced$shortStrat <- ""
for (i in 1:nrow(endDataAllExp2E_subInvestOrderedReduced)){
  if (endDataAllExp2E_subInvestOrderedReduced[i,]$concatStrat=="Receiver_strat1"){
    endDataAllExp2E_subInvestOrderedReduced[i,]$shortStrat <- "RS1"
  }
  if (endDataAllExp2E_subInvestOrderedReduced[i,]$concatStrat=="Receiver_strat2"){
    endDataAllExp2E_subInvestOrderedReduced[i,]$shortStrat <- "RS2"
  }
  if (endDataAllExp2E_subInvestOrderedReduced[i,]$concatStrat=="Receiver_strat3"){
    endDataAllExp2E_subInvestOrderedReduced[i,]$shortStrat <- "RS3"
  }
  if (endDataAllExp2E_subInvestOrderedReduced[i,]$concatStrat=="Sender_strat1"){
    endDataAllExp2E_subInvestOrderedReduced[i,]$shortStrat <- "SS1"
  }
  if (endDataAllExp2E_subInvestOrderedReduced[i,]$concatStrat=="Sender_strat2"){
    endDataAllExp2E_subInvestOrderedReduced[i,]$shortStrat <- "SS2"
  }
  if (endDataAllExp2E_subInvestOrderedReduced[i,]$concatStrat=="Sender_strat3"){
    endDataAllExp2E_subInvestOrderedReduced[i,]$shortStrat <- "SS3"
  }
}

ggplot(endDataAllExp2E_subInvestOrderedReduced,aes(x=as.factor(shortStrat))) +
  geom_jitter(aes(y=meanFit,color=shortStrat),alpha=.5,height=0) + 
  #geom_jitter(aes(y=meanStratFreq),color="grey",alpha=.5,height=0) + 
  #geom_jitter(aes(y=meanStratFreq),color="grey",alpha=.5,height=0) + 
  #geom_jitter(aes(y=meanAlphaBeta),color="orange",alpha=.5,height=0,shape="X") +
  theme_bw() +
  facet_grid(c2~c1) +
  # ylim(0,1) + 
  labs(title="Hybrid Expectation - Fitnesses") +
  labs(x="Strategy",y="Mean Fitness") +
  labs(subtitle="c1") +
  labs(color="c2\n\n\nStrategy") +
  labs(caption="Grey: Frequency of strategy\nOrange X: Alpha or Beta")

ggsave("NegCostFitsAndEquilib.png",
       device="png",path=dir,height=8,width=10,unit="in")



####2E - trajectories ####
dir2E <- "C:/Users/owner/Documents/DONE/Exper_2E"
summaryFiles2E <- list.files(dir2E,"*summaryStats*")

iterator <- 1
honestInvAll2E<-data.frame()
for (i in summaryFiles2E){
  
  dataAllReps2E <- read.csv(paste0(dir2E,"/",i))
  
  for (r in unique(dataAllReps2E$rep)){
    data <- subset(dataAllReps2E,rep==r)
    
    data$fileNum <- iterator
    iterator <- iterator + 1 
    
    honestInvAll2E <- rbind(honestInvAll2E,data)
  }
  
  
}


honestInvAll2E$concatStrat <- paste0(honestInvAll2E$indType,"_",honestInvAll2E$stratType)
honestInvAll2E$c1_c2 <- paste0(honestInvAll2E$c1,"_",honestInvAll2E$c2)


for (i in unique(honestInvAll2E$fileNum)){
  d <- subset(honestInvAll2E,fileNum==i)
  if (unique(d$c1 == 0.75)){
    p<- ggplot(d,aes(x=gen)) +
      geom_point(aes(y=((stratNum/N*10)-5),color=stratType),size=2) +
      geom_point(aes(y=meanFit,color=stratType),shape="O",size=2,alpha=1) +
      geom_point(aes(y=(meanAlphaBeta*10)-5),shape="X",size=2) +
      labs(title=paste0("Honest Expected: c1 = ",d[1,]$c1,",  c2 = ",d[1,]$c2)) +
      #ylim(0,1) +
      theme_bw() +
      labs(color="Strategy") +
      labs(x="Generation",y="Mean Alpha or Beta or Fitness or Strategy Frequency") +
      facet_grid(indType~.) +
      labs(caption="X: Mean alpha or beta\nO: Mean fitness\nDots: Strategy Frequencies") +
      labs(subtitle=paste0("Replicate: ",d$fileNum))
    
    
    ggsave(plot=p,paste0(unique(d$c1),"_",unique(d$c2),"_rep",unique(d$fileNum),"_fit.png"),
           device="png",path=paste0(dir2E,"/traj"),height=8,width=20,unit="in")
  }
  
}







####2E - sub_Invest trajectories ####
dir2E_subInvest <- "C:/Users/owner/Documents/DONE/Exper_2E/sub_Investigate"
summaryFiles2E_subInvest <- list.files(dir2E_subInvest,"*summaryStats*")

iterator <- 1
for (i in summaryFiles2E_subInvest){
  
  d <- read.csv(paste0(dir2E_subInvest,"/",i))
  unique(d$c1)
  unique(d$c2)
  unique(d$N)
  unique(d$m)
  unique(d$mutRateAlpha)
  unique(d$mutRateStrategySender)
  unique(d$initializationType)
  # to continue...
  
  
  
  
  
  p<- ggplot(d,aes(x=gen)) +
    geom_point(aes(y=((stratNum/N*10)-5),color=stratType),size=2) +
    geom_point(aes(y=meanFit,color=stratType),shape="O",size=2,alpha=1) +
    geom_point(aes(y=(meanAlphaBeta*10)-5),shape="X",size=2) +
    labs(title=paste0("Honest Expected: c1 = ",d[1,]$c1,",  c2 = ",d[1,]$c2)) +
    #ylim(0,1) +
    theme_bw() +
    labs(color="Strategy") +
    labs(x="Generation",y="Mean Alpha or Beta or Fitness or Strategy Frequency") +
    facet_grid(indType~.) +
    labs(caption="X: Mean alpha or beta\nO: Mean fitness\nDots: Strategy Frequencies") +
    labs(subtitle=paste0("Replicate: ",d$fileNum))
  
  
  ggsave(plot=p,paste0("_fit.png"),
         device="png",path=paste0(dir2E_subInvest,"/traj"),height=8,width=20,unit="in")
  
}

for (i in unique(honestInvAll2E_subInvest$fileNum)){
  
  d <- subset(honestInvAll2E_subInvest,fileNum==i)
  if (unique(d$c1 == 0.75)){
    p<- ggplot(d,aes(x=gen)) +
      geom_point(aes(y=((stratNum/N*10)-5),color=stratType),size=2) +
      geom_point(aes(y=meanFit,color=stratType),shape="O",size=2,alpha=1) +
      geom_point(aes(y=(meanAlphaBeta*10)-5),shape="X",size=2) +
      labs(title=paste0("Honest Expected: c1 = ",d[1,]$c1,",  c2 = ",d[1,]$c2)) +
      #ylim(0,1) +
      theme_bw() +
      labs(color="Strategy") +
      labs(x="Generation",y="Mean Alpha or Beta or Fitness or Strategy Frequency") +
      facet_grid(indType~.) +
      labs(caption="X: Mean alpha or beta\nO: Mean fitness\nDots: Strategy Frequencies") +
      labs(subtitle=paste0("Replicate: ",d$fileNum))
    
    
    ggsave(plot=p,paste0(unique(d$c1),"_",unique(d$c2),"_rep",unique(d$fileNum),"_fit.png"),
           device="png",path=paste0(dir2E_subInvest,"/traj"),height=8,width=20,unit="in")
  }
  
}








#2D - trajectories ####
dir2D <- "C:/Users/owner/Documents/DONE/Exper_2D"
summaryFiles2D <- list.files(dir2D,"*summaryStats*")

iterator <- 1
honestInvAll2D<-data.frame()
for (i in summaryFiles2D){
  
  dataAllReps2D <- read.csv(paste0(dir2D,"/",i))
  
  for (r in unique(dataAllReps2D$rep)){
    data <- subset(dataAllReps2D,rep==r)
    
    data$fileNum <- iterator
    iterator <- iterator + 1 
    
    honestInvAll2D <- rbind(honestInvAll2D,data)
  }
  
  
}


honestInvAll2D$concatStrat <- paste0(honestInvAll2D$indType,"_",honestInvAll2D$stratType)

#Skip this - it takes too long. Just run it when needed
honestInvAll2D$shortStrat <- ""
for (i in 1:nrow(honestInvAll2D)){
  if (honestInvAll2D[i,]$concatStrat=="Receiver_strat1"){
    honestInvAll2D[i,]$shortStrat <- "RS1"
  } else if (honestInvAll2D[i,]$concatStrat=="Receiver_strat2"){
    honestInvAll2D[i,]$shortStrat <- "RS2"
  } else if (honestInvAll2D[i,]$concatStrat=="Receiver_strat3"){
    honestInvAll2D[i,]$shortStrat <- "RS3"
  } else if (honestInvAll2D[i,]$concatStrat=="Sender_strat1"){
    honestInvAll2D[i,]$shortStrat <- "SS1"
  } else if (honestInvAll2D[i,]$concatStrat=="Sender_strat2"){
    honestInvAll2D[i,]$shortStrat <- "SS2"
  } else if (honestInvAll2D[i,]$concatStrat=="Sender_strat3"){
    honestInvAll2D[i,]$shortStrat <- "SS3"
  }
}


honestInvAll2D$c1_c2 <- paste0(honestInvAll2D$c1,"_",honestInvAll2D$c2)

unique(honestInvAll2D$c1_c2)
unique(subset(honestInvAll2D,c1_c2=="0.75_1.5")$fileNum)

for (i in unique(honestInvAll2D$fileNum)){
  i<-1
  d <- subset(honestInvAll2D,fileNum==i)
  
  ggplot(d,aes(x=gen)) +
    geom_point(aes(y=(stratNum/N),color=stratType),size=2) +
    #  geom_point(aes(y=meanFit,color=stratType),shape="O",size=2,alpha=1) +
    geom_point(aes(y=meanAlphaBeta),shape="X",size=2) +
    labs(title=paste0("Honest Expected: c1 = ",d[1,]$c1,",  c2 = ",d[1,]$c2)) +
    #ylim(0,1) +
    theme_bw() +
    labs(color="Strategy") +
    labs(x="Generation",y="Mean Alpha or Beta or Fitness or Strategy Frequency") +
    facet_grid(indType~.) +
    labs(caption="X: Mean alpha or beta\nO: Mean fitness\nDots: Strategy Frequencies") +
    labs(subtitle=paste0("Replicate: ",d$fileNum))
  
  
  ggsave(plot=p,paste0(unique(d$c1),"_",unique(d$c2),"_rep",unique(d$fileNum),"_smallScale.png"),
         device="png",path=paste0(dir2D,"/smallScale"),height=8,width=20,unit="in")
  
  
}
































#Exper 3 - Negative Cost ######

dirExp3 <- "C:/Users/owner/Documents/DONE/Experiment_3"
summaryFilesExp3 <- list.files(dir,"*summaryStats*")

#First - create endData stats from summaryStats files produced by cpp
cutoff <- 0.25   #Look at last this % of generations
iterator <- 1
endDataAllExp3<-data.frame()
for (i in summaryFilesExp3){
  
  dataAllReps <- read.csv(paste0(dirExp3,"/",i))
  
  for (r in unique(dataAllReps$rep)){
    data <- subset(dataAllReps,rep==r)
    
    data$fileName <- i
    dataEnd <- subset(data,gen>=((max(gen)-(cutoff*max(gen)))))
    dataEnd$concatStrat <- paste0(dataEnd$indType,"_",dataEnd$stratType)
    
    dataEndSenders <- subset(dataEnd,indType == "Sender")
    meanAlpha <- mean(dataEndSenders$meanAlphaBeta)
    dataEndReceivers <- subset(dataEnd,indType == "Receiver")
    meanBeta <- mean(dataEndReceivers$meanAlphaBeta)
    
    row <- data.frame()
    for (j in unique(dataEnd$concatStrat)){
      temp <- subset(dataEnd,concatStrat == j)
      row <- temp[1,]
      if (row$indType == "Receiver"){
        row$meanAlphaBeta <- meanBeta
      } else {
        row$meanAlphaBeta <- meanAlpha
      }
      
      
      row$fileSuffix <- gsub("_[0-9]","",gsub("_*..csv","",gsub(".*summaryStats_","",row$fileName)))
      row$meanFit <- mean(temp$meanFit)
      row$meanStratNum <- mean(temp$stratNum)
      row$meanStratFreq <- mean(temp$stratNum/row$N)
      row$stratNum <- "NA"
      row$gen <- "NA"
      row$fileNum <- iterator
      
      endDataAllExp3 <- rbind(endDataAllExp3,row)
      
    }
    iterator <- iterator + 1
  }
}

head(endDataAllExp3)


#Organize parameters for all simulations
masterParams <- list()
variableParams <- c()
paramNames <- c()
for (c in which(colnames(endDataAllExp3) == "N"):which(colnames(endDataAllExp3) == "fileSuffix")){
  masterParams[[c+1-which(colnames(endDataAllExp3) == "N")]] <- unique(endDataAllExp3[,c])
  paramNames <- c(paramNames,colnames(endDataAllExp3)[c])
  if(length(unique(endDataAllExp3[,c])) > 1 ){
    variableParams<- c(variableParams,colnames(endDataAllExp3)[c])
  }
}
names(masterParams) <- paramNames
N <- as.numeric(masterParams$N)
###
print(variableParams)
masterParams



endDataAllExp3$c1c2 <- paste0(endDataAllExp3$c1,"_",endDataAllExp3$c2)


#Add column to order along x axis
collect2<-data.frame()
for (i in unique(endDataAllExp3$c1c2)){
  temp <- subset(endDataAllExp3,c1c2==i)
  iterator <- 1
  temp
  collect1 <- data.frame()
  for (j in unique(temp$fileNum)){
    temp2 <- subset(temp,fileNum==j)
    temp2$orderedFileNum <- iterator
    temp2
    collect1<-rbind(collect1,temp2)
    iterator <- iterator + 1
  }
  collect2<-rbind(collect2,collect1)
}
collect2$orderedFileNum

endDataAllExp3OrderedReduced <- collect2

head(endDataAllExp3OrderedReduced)
for (i in 1:nrow(endDataAllExp3OrderedReduced)){
  if (endDataAllExp3OrderedReduced[i,]$c2<=endDataAllExp3OrderedReduced[i,]$c1){
    if (endDataAllExp3OrderedReduced[i,]$indType=="Sender"){
      endDataAllExp3OrderedReduced[i,]$expAlphaBeta <- 1
    } else {
      endDataAllExp3OrderedReduced[i,]$expAlphaBeta <- 0
    }
  }
  else if (endDataAllExp3OrderedReduced[i,]$c2>=1 & endDataAllExp3OrderedReduced[i,]$c1 < 1){
    if (endDataAllExp3OrderedReduced[i,]$indType=="Sender"){
      endDataAllExp3OrderedReduced[i,]$expAlphaBeta <- 0
    } else {
      endDataAllExp3OrderedReduced[i,]$expAlphaBeta <- 1
    }
  }
}

endDataAllExp3OrderedReduced <- subset(endDataAllExp3OrderedReduced,orderedFileNum<=50)

library(ggplot2)
#install.packages("rlang")

#Negative costs
ggplot(endDataAllExp3OrderedReduced,aes(x=orderedFileNum)) +
  geom_point(aes(y=meanStratFreq,color=stratType),alpha=.7) + 
  geom_point(aes(y=meanAlphaBeta),shape="X") +
  geom_line(aes(y=expAlphaBeta),color="orange",alpha=0.9,size=.8) +
  theme_bw() +
  facet_grid(c1+indType~c2) + 
  labs(x="Replicate",y="Strategy Freq or Alpha or Beta") +
  labs(subtitle = "c2") + 
  labs(color =  "c1\n\n\nStrategy") +
  labs(title = "Negative Costs")
ggsave("NegCosts.png",
       device="png",path=dir,height=8,width=10,unit="in")

#Next - look at mean fitnesses
head(endDataAllExp3OrderedReduced)
endDataAllExp3OrderedReduced$shortStrat <- ""
for (i in 1:nrow(endDataAllExp3OrderedReduced)){
  if (endDataAllExp3OrderedReduced[i,]$concatStrat=="Receiver_strat1"){
    endDataAllExp3OrderedReduced[i,]$shortStrat <- "RS1"
  }
  if (endDataAllExp3OrderedReduced[i,]$concatStrat=="Receiver_strat2"){
    endDataAllExp3OrderedReduced[i,]$shortStrat <- "RS2"
  }
  if (endDataAllExp3OrderedReduced[i,]$concatStrat=="Receiver_strat3"){
    endDataAllExp3OrderedReduced[i,]$shortStrat <- "RS3"
  }
  if (endDataAllExp3OrderedReduced[i,]$concatStrat=="Sender_strat1"){
    endDataAllExp3OrderedReduced[i,]$shortStrat <- "SS1"
  }
  if (endDataAllExp3OrderedReduced[i,]$concatStrat=="Sender_strat2"){
    endDataAllExp3OrderedReduced[i,]$shortStrat <- "SS2"
  }
  if (endDataAllExp3OrderedReduced[i,]$concatStrat=="Sender_strat3"){
    endDataAllExp3OrderedReduced[i,]$shortStrat <- "SS3"
  }
}

ggplot(endDataAllExp3OrderedReduced,aes(x=as.factor(shortStrat))) +
  geom_jitter(aes(y=meanFit,color=shortStrat),alpha=.5,height=0) + 
  geom_jitter(aes(y=meanStratFreq),color="grey",alpha=.5,height=0) + 
  geom_jitter(aes(y=meanStratFreq),color="grey",alpha=.5,height=0) + 
  geom_jitter(aes(y=meanAlphaBeta),color="orange",alpha=.5,height=0,shape="X") +
  theme_bw() +
  facet_grid(c2~c1) +
  ylim(0,1) + 
  labs(title="Hybrid Expectation - Fitnesses") +
  labs(x="Strategy",y="Mean Fitness") +
  labs(subtitle="c1") +
  labs(color="c2\n\n\nStrategy") +
  labs(caption="Grey: Frequency of strategy\nOrange X: Alpha or Beta")

ggsave("NegCostFitsAndEquilib.png",
       device="png",path=dir,height=8,width=10,unit="in")




# Exper 4 ####
dir4 <- "C:/Users/owner/Documents/DONE/Exper_4"
summaryFiles4 <- list.files(dir4,"*summaryStats*")


#Calculation of receiver fitnesses may be messed up by NA's 
#IT should be 10 for honest signalling but is 3.3333... 1/3 of 10

#First - create endData stats from summaryStats files produced by cpp
cutoff <- 0.25   #Look at last this % of generations
iterator <- 1
endDataAll4<-data.frame()

for (i in summaryFiles4){
  
  dataAllReps <- read.csv(paste0(dir4,"/",i))
  
  for (r in unique(dataAllReps$rep)){
    data <- subset(dataAllReps,rep==r)
    
    data$fileName <- i
    dataEnd <- subset(data,gen>=((max(gen)-(cutoff*max(gen)))))
    dataEnd$concatStrat <- paste0(dataEnd$indType,"_",dataEnd$stratType)
    
    dataEndSenders <- subset(dataEnd,indType == "Sender")
    meanAlpha <- mean(dataEndSenders$meanAlphaBeta)
    dataEndReceivers <- subset(dataEnd,indType == "Receiver")
    meanBeta <- mean(dataEndReceivers$meanAlphaBeta)
    
    #remove NA from mean fit - it messes things up later... Just make 0s
    dataEnd$meanFit <- replace(dataEnd$meanFit, dataEnd$meanFit == "NaN", 0)
    
    
    row <- data.frame()
    for (j in unique(dataEnd$concatStrat)){
      temp <- subset(dataEnd,concatStrat == j)
      row <- temp[1,]
      if (row$indType == "Receiver"){
        row$meanAlphaBeta <- meanBeta
      } else {
        row$meanAlphaBeta <- meanAlpha
      }
      
      
      row$fileSuffix <- gsub("_[0-9]","",gsub("_*..csv","",gsub(".*summaryStats_","",row$fileName)))
      row$meanFit <- mean(temp$meanFit)
      row$meanStratNum <- mean(temp$stratNum)
      row$meanStratFreq <- mean(temp$stratNum/row$N)
      row$stratNum <- "NA"
      row$gen <- "NA"
      row$fileNum <- iterator
      
      endDataAll4 <- rbind(endDataAll4,row)
      
    }
    iterator <- iterator + 1
  }
}

head(endDataAll4)

#Organize parameters for all simulations
masterParams <- list()
variableParams <- c()
paramNames <- c()
for (c in which(colnames(endDataAll4) == "N"):which(colnames(endDataAll4) == "fileSuffix")){
  masterParams[[c+1-which(colnames(endDataAll4) == "N")]] <- unique(endDataAll4[,c])
  paramNames <- c(paramNames,colnames(endDataAll4)[c])
  if(length(unique(endDataAll4[,c])) > 1 ){
    variableParams<- c(variableParams,colnames(endDataAll4)[c])
  }
}
names(masterParams) <- paramNames
N <- as.numeric(masterParams$N)
###
print(variableParams)
masterParams



endDataAll4$c1c2 <- paste0(endDataAll4$c1,"_",endDataAll4$c2)
endDataAll4$lab <- paste0(endDataAll4$c1,
                          "_",endDataAll4$m,
                          "_",endDataAll4$initializationType,
                          "_",endDataAll4$cauchyDist,
                          "_",endDataAll4$mutRateAlpha,
                          "_",endDataAll4$mutRateStrategySender,
                          "_",endDataAll4$c2
)

#Add column to order along x axis
collect2<-data.frame()
for (i in unique(endDataAll4$lab)){
  temp <- subset(endDataAll4,endDataAll4$lab==i)
  iterator <- 1
  temp
  collect1 <- data.frame()
  for (j in unique(temp$fileNum)){
    temp2 <- subset(temp,fileNum==j)
    temp2$orderedFileNum <- iterator
    temp2
    collect1<-rbind(collect1,temp2)
    iterator <- iterator + 1
  }
  collect2<-rbind(collect2,collect1)
}
collect2$orderedFileNum

endDataAll4OrderedReduced <- collect2

head(endDataAll4OrderedReduced)
for (i in 1:nrow(endDataAll4OrderedReduced)){
  if (endDataAll4OrderedReduced[i,]$c2<=endDataAll4OrderedReduced[i,]$c1){
    if (endDataAll4OrderedReduced[i,]$indType=="Sender"){
      endDataAll4OrderedReduced[i,]$expAlphaBeta <- 1
    } else {
      endDataAll4OrderedReduced[i,]$expAlphaBeta <- 0
    }
  }
  else if (endDataAll4OrderedReduced[i,]$c2>=1 & endDataAll4OrderedReduced[i,]$c1 < 1){
    if (endDataAll4OrderedReduced[i,]$indType=="Sender"){
      endDataAll4OrderedReduced[i,]$expAlphaBeta <- 0
    } else {
      endDataAll4OrderedReduced[i,]$expAlphaBeta <- 1
    }
  }
}

#endDataAll4OrderedReduced <- subset(endDataAll4OrderedReduced,orderedFileNum<=50)

endDataAll4OrderedReduced

library(ggplot2)
#install.packages("rlang")

unique(endDataAll4OrderedReduced$c1)
unique(endDataAll4OrderedReduced$c2)

variableParams
i<-4
unique(endDataAll4OrderedReduced$type)

endDataAll4OrderedReduced$type <- ""
for (i in 1:nrow(endDataAll4OrderedReduced)){
  if (grepl("NoSig",endDataAll4OrderedReduced[i,]$fileName) == TRUE){
    endDataAll4OrderedReduced[i,]$type = "NoSignal"
  }
  if (grepl("honest",endDataAll4OrderedReduced[i,]$fileName) == TRUE){
    endDataAll4OrderedReduced[i,]$type = "Honest"
  }
  if (grepl("Hybrid",endDataAll4OrderedReduced[i,]$fileName) == TRUE){
    endDataAll4OrderedReduced[i,]$type = "Hybrid"
  }
}

#plot - ignore. Old
for (xm in unique(endDataAll4OrderedReduced$m)){
  for (xmutRateAlpha in unique(endDataAll4OrderedReduced$mutRateAlpha)){
    for (xmutRateStrategySender in unique(endDataAll4OrderedReduced$mutRateStrategySender)){
      for (xcauchyDist in unique(endDataAll4OrderedReduced$cauchyDist)){
        
        d <- subset(endDataAll4OrderedReduced,m==xm & cauchyDist==xcauchyDist & mutRateAlpha==xmutRateAlpha & mutRateStrategySender==xmutRateStrategySender)
        
        p<-ggplot(d,aes(x=orderedFileNum)) +
          geom_point(aes(y=meanStratFreq,color=stratType),alpha=.7) + 
          geom_point(aes(y=meanAlphaBeta),shape="X") +
          geom_line(aes(y=expAlphaBeta),color="orange",alpha=0.9,size=.5) +
          theme_bw() +
          facet_grid(c1+indType~c2+initializationType+cauchyDist) + 
          labs(x="Replicate",y="Strategy Freq or Alpha or Beta") +
          labs(subtitle = "m\ninitializationType\nmutRateAlpha\nmutRateStrategySender\nc2") + 
          labs(color =  "c1\n\n\nStrategy") +
          labs(title = paste0("m: ",xm,"   mutRateAlpha: ",xmutRateAlpha,"   mutRateStrategy: ",xmutRateStrategySender,"   cauchyDist: ",xcauchyDist,".png"))
        p
        ggsave(plot=p,paste0(xcauchyDist,"_",xm,"_",xmutRateAlpha,"_",xmutRateStrategySender,".png"),
               device="png",path=dir4,height=8,width=12,unit="in")
        
      }
    }
  }
}

variableParams

#Determine Equilibs
#for (tol in c(0.05,0.1,0.25,0.4)){
tol <- 0.25
tolerance <- tol

head(endDataAll4OrderedReduced)
dAll <- endDataAll4OrderedReduced
head(dAll)
dAll$devAlphaBeta <- dAll$meanAlphaBeta-dAll$expAlphaBeta
equilibs <- data.frame()

i <- 1
while (i <= nrow(dAll)){
  
  #determine dominant sender and receiver strat
  row <- dAll[i,]
  #determine expected dominant strategies
  
  #senders
  s<-dAll[i:(i+2),]
  #dominant sender strat
  domSendStrat <- s[which(s$meanStratNum==max(s$meanStratNum)),]$stratType
  devAlpha <- unique(s$meanAlphaBeta - s$expAlphaBeta)
  alpha <- unique(s$meanAlphaBeta)
  meanSendFit <- sum(s$meanFit * s$meanStratFreq)
  #Also find out out the strategy deviation?
  
  #receivers
  r<-dAll[(i+3):(i+5),]
  
  domRecStrat <- r[which(r$meanStratNum==max(r$meanStratNum)),]$stratType
  devBeta <- unique(r$meanAlphaBeta - r$expAlphaBeta)
  beta <- unique(r$meanAlphaBeta)
  meanRecFit <- sum(r$meanFit * r$meanStratFreq)
  
  if (domRecStrat == "strat1" & domSendStrat == "strat1"){
    if (beta >= (1 - tolerance) & alpha <= tolerance){
      equilib <- "Honest"
      #What if it falls within honest but is closer to hybrid?
      if (beta < (1 - (tolerance/2)) & alpha > (tolerance/2)){
        equilib <- "Hybrid"
      }
    } else if (abs(devBeta) < tolerance &  abs(devAlpha) < tolerance & unique(s$expAlphaBeta) > 0 & unique(r$expAlphaBeta) < 1){
      equilib <- "Hybrid"
    } else {
      equilib<-"Pooling"
    }
  } else {
    equilib <- "Pooling"
  }
  
  row$domSendStrat <- domSendStrat
  row$domRecStrat <- domRecStrat
  row$meanSendFit <- meanSendFit
  row$meanRecFit <- meanRecFit
  row$devAlpha <- devAlpha
  row$devBeta <- devBeta
  row$alpha <- alpha
  row$beta <- beta
  row$equilib <- equilib
  equilibs <- rbind(equilibs,row)
  
  i <- i + 6  
}

equilibs

endDataAll4OrderedReduced
variableParams
endDataAll4OrderedReduced

d <- subset(equilibs)

d$concat <- paste(d$c1,d$c2,d$initializationType,d$type,sep="_")
#Determine expected fitnesses
#if honest: fitness = m(1-c1)
#if hybrid: fitness = m(c2-c1)

#NOTE - we have some 'hybrid' runs which were not actually hybrid - remove these above...

d$senderExpFitness<-ifelse(d$type=="NoSignal",0,
                           ifelse(d$type=="Honest",d$m*(1.0 - d$c1)  ,
                                  d$m*(d$c2 - d$c1)
                           ))
#d$m*((ifelse(d$type=="Honest",1,d$c1))-d$c1))
#Receiver: if honest, expect fitness = 1
#if hybrid, expect payoff m*b + (1-m)*(1-b)
#If nosignal, expect 1-m
#This only works for m<0.5
d$receiverExpFitness<-ifelse(d$type=="NoSignal",1.0-d$m,ifelse(d$type=="Honest",1.0,
                                                               (d$m*d$c2 + (1-d$m)*(1-d$c2)) )) #use value of c2 for beta since this is what it should be at hybrid equilib


#Remove 'hybrid' reps which aren't actually hybrid

d2<-rbind(subset(d,d$type != "Hybrid"),subset(d,d$type == "Hybrid" & (d$c1 < 1.0 & d$c2 < 1.0)))

#get mean of mean fitnesses...
d_post<-data.frame()
for (u in unique(d2$concat)){
  dTemp <- subset(d2,concat==u)
  dTemp$meanMeanSendFit <- mean(dTemp$meanSendFit)
  dTemp$meanMeanRecFit <- mean(dTemp$meanRecFit)
  d_post <- rbind(d_post,dTemp)
}
d_post

#HERE HHH
unique(d2$concat)
u<-unique(d2$concat)[13]
du<-subset(d_post,concat==u)

#What is exp fitness for no signalling? If m < 0.5, receivers should never respond, so 0

p<-  ggplot(du,aes(x=as.factor(type))) +
  geom_bar(aes(group=as.factor(type),fill=as.factor(type))) +
  theme_bw() +
  facet_grid(c1~initializationType+c2) +
  labs(x="Equilibrium Reached",y="Number of Replicates") +
  labs(subtitle = paste0("tolerance = 0.25\ninitialization condition\nc2",
                         "\nexpReceiverFit = ", du$receiverExpFitness,
                         "\nexpSenderFit = ", du$senderExpFitness
  )) + #Only works for honest and hybrid - not no signal
  labs(fill =  "c1\n\n\n\nEquilibrium") +
  labs(title = paste0(du$type))

p2 <- p + geom_text(aes(label=round(meanMeanSendFit,2)/du$interactionPartners,y=2)) +
  geom_text(aes(label=round(meanMeanRecFit/du$interactionPartners,2),y=6)) +
  labs(caption="Top: Mean Receiver Fitness\nBottom: Mean Sender Fitness")
du
#Good!

ggsave(plot=p2,paste0(xT,".png"),
       device="png",path=paste0(dir4,"/equilibsFits"),height=8,width=10,unit="in")
#Issue - it looks like receiver fitness at pooling equilib = m
##It should be 1 - m!!! 




for (xc1 in unique(endDataAll4OrderedReduced$c1)){
  for (xc2 in unique(endDataAll4OrderedReduced$c2)){
    for (xcauchyDist in unique(endDataAll4OrderedReduced$cauchyDist)){
      
      d <- subset(equilibs,c1==xc1 & c2 == xc2 & cauchyDist==xcauchyDist)
      
      
      p<- ggplot(d,aes(x=as.factor(equilib))) +
        geom_bar(aes(group=as.factor(equilib),fill=as.factor(equilib))) +
        theme_bw() +
        facet_grid(mutRateAlpha~initializationType+mutRateStrategySender) +
        labs(x="Equilibrium Reached",y="Number of Replicates") +
        labs(subtitle = "tolerance = 0.25\ninitialization condition\nmutRateStrategy") + 
        labs(fill =  "mutRateAlpha\n\n\n\nEquilibrium") +
        labs(title = paste0("c1: ",xc1,"   c2: ",xc2,"   cauchyDist: ",xcauchyDist))
      
      
      ggsave(plot=p,paste0(xc1,"_",xc2,"_",xcauchyDist,".png"),
             device="png",path=paste0(dir4,"/equilibs2"),height=8,width=10,unit="in")
      
      
      
    }
  }
}



#Exper 5 - honest start ####
dir5 <- "D:/StAndrews/SignallingDiscrete/Exper5/honest_all"
summaryFiles5 <- list.files(dir5,"*summaryStats*")
summaryFiles5
#First - create endData stats from summaryStats files produced by cpp
cutoff <- 0.25   #Look at last this % of generations
iterator <- 1
endDataAll5<-data.frame()

for (i in summaryFiles5){
  
  dataAllReps <- read.csv(paste0(dir5,"/",i))
  for (r in unique(dataAllReps$rep)){
    data <- subset(dataAllReps,rep==r)
    
    data$fileName <- i
    dataEnd <- subset(data,gen>=((max(gen)-(cutoff*max(gen)))))
    dataEnd$concatStrat <- paste0(dataEnd$indType,"_",dataEnd$stratType)
    
    dataEndSenders <- subset(dataEnd,indType == "Sender")
    meanAlpha <- mean(dataEndSenders$meanAlphaBeta)
    dataEndReceivers <- subset(dataEnd,indType == "Receiver")
    meanBeta <- mean(dataEndReceivers$meanAlphaBeta)
    
    #remove NA from mean fit - it messes things up later... Just make 0s
    dataEnd$meanFit <- replace(dataEnd$meanFit, dataEnd$meanFit == "NaN", 0)
    
    
    row <- data.frame()
    for (j in unique(dataEnd$concatStrat)){
      temp <- subset(dataEnd,concatStrat == j)
      row <- temp[1,]
      if (row$indType == "Receiver"){
        row$meanAlphaBeta <- meanBeta
      } else {
        row$meanAlphaBeta <- meanAlpha
      }
      
      
      row$fileSuffix <- gsub("_[0-9]","",gsub("_*..csv","",gsub(".*summaryStats_","",row$fileName)))
      row$meanFit <- mean(temp$meanFit)
      row$meanStratNum <- mean(temp$stratNum)
      row$meanStratFreq <- mean(temp$stratNum/row$N)
      row$stratNum <- "NA"
      row$gen <- "NA"
      row$fileNum <- iterator
      
      endDataAll5 <- rbind(endDataAll5,row)
      
    }
    iterator <- iterator + 1
  }
}
#..do we have all c1 and c2 values here? Check val of iterator

if (1==2){
  #Organize parameters for all simulations
  masterParams <- list()
  variableParams <- c()
  paramNames <- c()
  for (c in which(colnames(endDataAll5) == "N"):which(colnames(endDataAll5) == "fileSuffix")){
    masterParams[[c+1-which(colnames(endDataAll5) == "N")]] <- unique(endDataAll5[,c])
    paramNames <- c(paramNames,colnames(endDataAll5)[c])
    if(length(unique(endDataAll5[,c])) > 1 ){
      variableParams<- c(variableParams,colnames(endDataAll5)[c])
    }
  }
  names(masterParams) <- paramNames
  N <- as.numeric(masterParams$N)
  ###
  print(variableParams)
  masterParams
}

endDataAll5$c1c2 <- paste0(endDataAll5$c1,"_",endDataAll5$c2)
endDataAll5$lab <- paste0(endDataAll5$c1,
                          "_",endDataAll5$m,
                          "_",endDataAll5$initializationType,
                          "_",endDataAll5$cauchyDist,
                          "_",endDataAll5$mutRateAlpha,
                          "_",endDataAll5$mutRateStrategySender,
                          "_",endDataAll5$c2
)
write.csv(endDataAll5,file=paste0(dir5,"/_endDataAll5.csv"), row.names=FALSE)

#Add column to order along x axis
if (1==2){
  collect2<-data.frame()
  for (i in unique(endDataAll5$lab)){
    temp <- subset(endDataAll5,endDataAll5$lab==i)
    iterator <- 1
    temp
    collect1 <- data.frame()
    for (j in unique(temp$fileNum)){
      temp2 <- subset(temp,fileNum==j)
      temp2$orderedFileNum <- iterator
      temp2
      collect1<-rbind(collect1,temp2)
      iterator <- iterator + 1
    }
    collect2<-rbind(collect2,collect1)
  }
  collect2$orderedFileNum
  endDataAll5OrderedReduced <- collect2
}
endDataAll5OrderedReduced <- endDataAll5

head(endDataAll5OrderedReduced)
for (i in 1:nrow(endDataAll5OrderedReduced)){
  if (endDataAll5OrderedReduced[i,]$c2<=endDataAll5OrderedReduced[i,]$c1){
    if (endDataAll5OrderedReduced[i,]$indType=="Sender"){
      endDataAll5OrderedReduced[i,]$expAlphaBeta <- 1
    } else {
      endDataAll5OrderedReduced[i,]$expAlphaBeta <- 0
    }
  }
  else if (endDataAll5OrderedReduced[i,]$c2>=1 & endDataAll5OrderedReduced[i,]$c1 < 1){
    if (endDataAll5OrderedReduced[i,]$indType=="Sender"){
      endDataAll5OrderedReduced[i,]$expAlphaBeta <- 0
    } else {
      endDataAll5OrderedReduced[i,]$expAlphaBeta <- 1
    }
  }
}

unique(endDataAll5OrderedReduced$c1)
write.csv(endDataAll5OrderedReduced,file=paste0(dir5,"/_endDataAll5_orderedReduced.csv"), row.names=FALSE)

#Determine Equilibs
#for (tol in c(0.05,0.1,0.25,0.4)){
tol <- 0.20
#add tol to equilib file name
tolerance <- tol
unique(endDataAll5OrderedReduced$c1)
dAll <- endDataAll5OrderedReduced
head(dAll)
dAll$devAlphaBeta <- dAll$meanAlphaBeta-dAll$expAlphaBeta
equilibs <- data.frame()

i <- 1
while (i <= nrow(dAll)){
  
  #determine dominant sender and receiver strat
  row <- dAll[i,]
  #determine expected dominant strategies
  
  #senders
  s<-dAll[i:(i+2),]
  #dominant sender strat
  domSendStrat <- s[which(s$meanStratNum==max(s$meanStratNum)),]$stratType
  devAlpha <- unique(s$meanAlphaBeta - s$expAlphaBeta)
  alpha <- unique(s$meanAlphaBeta)
  meanSendFit <- sum(s$meanFit * s$meanStratFreq)
  #Also find out out the strategy deviation?
  
  #receivers
  r<-dAll[(i+3):(i+5),]
  
  domRecStrat <- r[which(r$meanStratNum==max(r$meanStratNum)),]$stratType
  devBeta <- unique(r$meanAlphaBeta - r$expAlphaBeta)
  beta <- unique(r$meanAlphaBeta)
  meanRecFit <- sum(r$meanFit * r$meanStratFreq)
  
  if (domRecStrat == "strat1" & domSendStrat == "strat1"){
    if (beta >= (1 - tolerance) & alpha <= tolerance){
      equilib <- "Honest"
      #What if it falls within honest but is closer to hybrid?
      if (beta < (1 - (tolerance/2)) & alpha > (tolerance/2)){
        equilib <- "Hybrid"
      }
    } else if (abs(devBeta) < tolerance &  abs(devAlpha) < tolerance & unique(s$expAlphaBeta) > 0 & unique(r$expAlphaBeta) < 1){
      equilib <- "Hybrid"
    } else {
      equilib<-"Pooling"
    }
  } else {
    equilib <- "Pooling"
  }
  
  row$domSendStrat <- domSendStrat
  row$domRecStrat <- domRecStrat
  row$meanSendFit <- meanSendFit
  row$meanRecFit <- meanRecFit
  row$devAlpha <- devAlpha
  row$devBeta <- devBeta
  row$alpha <- alpha
  row$beta <- beta
  row$equilib <- equilib
  equilibs <- rbind(equilibs,row)
  
  i <- i + 6  
}

equilibs$tol <- tol
equilibs$u <- paste0(equilibs$mutRateAlpha,"_",equilibs$mutRateStrategySender)
#..

#### saved data ####
write.csv(equilibs,file=paste0(dir5,"/",tol,"_equilibsAllHonest.csv"), row.names=FALSE)

#I need to run some more negative c2.
#And more positive c1
#I've only done odds with odds and evens with evens. Which I like for readability
#I can throw out all the 'extra value ones'
#I will need to write more of a code for this in r probably
#TO do:
#c1 = 1.4, 1.6 with c2 0, -.2, -.4, -.6, .2, ... 1.6
#c2 = -.2, -.4, -.6 with c1 = -.6, ... 1.6
#Odd high
#c1 = 1.5 with c2 0, -.1, -.3, -.5 ... 0.1 ... 1.5
#
#odd low
#c2 = -0.1, -.3, -.5 with c1 = -.5, ... 1.5
#
#These jobs are running now 135_00-01_...

#### Plots ####
unique(equilibs$u)
#For now - to show Graeme 5/16 tomorrow meeting
for (i in unique(equilibs$u)){
  data<-subset(equilibs,(equilibs$c2 >= 0) & u==i)
  
  library(scatterpie)
  
  #making data for pie chart
  data$row <- paste0(data$c1,"_",data$c2)
  
  pies <- data.frame()
  for (r in unique(data$row)){
    dTemp <- subset(data,row==r)
    sumHybrid <- sum(dTemp$equilib=="Hybrid")
    sumHonest <- sum(dTemp$equilib=="Honest")
    sumPooling <- sum(dTemp$equilib=="Pooling")
    pRow<-data.frame(r)
    pRow$c1Pie <- unique(dTemp$c1)
    pRow$c2Pie <- unique(dTemp$c2)
    pRow$Honest <- sumHonest
    pRow$Hybrid <- sumHybrid
    pRow$Pooling <- sumPooling
    pies <- rbind(pies,pRow)
  }
  pies2<-pies[,-1]
  #For publication!
  #This is for HONEST START. I am running more sims for odd c1 and c2 values to add to this.
  
  p<-ggplot(data,aes(c1,c2)) +
    geom_vline(xintercept=1,linetype="dashed") + 
    geom_vline(xintercept=0) + 
    geom_hline(yintercept=1,linetype="dashed") + 
    geom_hline(yintercept=0) + 
    geom_abline(intercept = 0, slope = 1, 
                linetype="dashed") +
    geom_scatterpie(aes(x=c1Pie,y=c2Pie,group=r),data=pies2,
                    cols = colnames(pies2)[3:5]) +
    theme_bw() +
    scale_fill_brewer(palette="Set1") +
    labs(title = paste0(
      "Mut Rate Alpha Beta: ",unique(data$mutRateAlpha),
      "\nMut Rate Strategy: ",unique(data$mutRateStrategySender)
    ))
  
  ggsave(paste0(i,".png"),plot = p,
         device="png",path=dir5,height=8,width=10,unit="in")
  
  
}

#for (i in unique(equilibs$u))
data<-subset(equilibs,c2 >= 0)
library(scatterpie)

#making data for pie chart
data$row <- paste0(data$c1,"_",data$c2,"_",data$u)
pies <- data.frame()
for (r in unique(data$row)){
  dTemp <- subset(data,row==r)
  sumHybrid <- sum(dTemp$equilib=="Hybrid")
  sumHonest <- sum(dTemp$equilib=="Honest")
  sumPooling <- sum(dTemp$equilib=="Pooling")
  pRow<-data.frame(r)
  pRow$c1Pie <- unique(dTemp$c1)
  pRow$c2Pie <- unique(dTemp$c2)
  pRow$Honest <- sumHonest
  pRow$Hybrid <- sumHybrid
  pRow$Pooling <- sumPooling
  pRow$mutRateAlphaBeta <- unique(dTemp$mutRateAlpha)
  pRow$mutRateStrat <- unique(dTemp$mutRateStrategySender)
  pies <- rbind(pies,pRow)
}
pies2<-pies[,-1]

p<-ggplot(data,aes(c1,c2)) +
  geom_vline(xintercept=1,linetype="dashed") + 
  geom_vline(xintercept=0) + 
  geom_hline(yintercept=1,linetype="dashed") + 
  geom_hline(yintercept=0) + 
  geom_abline(intercept = 0, slope = 1, 
              linetype="dashed") +
  geom_scatterpie(aes(x=c1Pie,y=c2Pie,group=r),data=pies2,
                  cols = colnames(pies2)[3:5]) +
  theme_bw() +
  scale_fill_brewer(palette="Set1") +
  labs(title="Honest Start") +
  labs(subtitle="Mut Rate Strategy") +
  labs(fill="Mut Rate\nAlpha Beta\n\nEquilibrium") +
  facet_grid(mutRateAlphaBeta~mutRateStrat)
p
ggsave(paste0("all.png"),plot=p,
       device="png",path=dir5,height=8,width=10,unit="in")


#Exper 5 - No Signal start ####
dir5 <- "D:/StAndrews/SignallingDiscrete/Exper5/130_19_24_32_Exper_5a_noSig"
summaryFiles5 <- list.files(dir5,"*summaryStats*")

#First - create endData stats from summaryStats files produced by cpp
cutoff <- 0.25   #Look at last this % of generations
iterator <- 1
endDataAll5<-data.frame()

for (i in summaryFiles5){
  
  dataAllReps <- read.csv(paste0(dir5,"/",i))
  for (r in unique(dataAllReps$rep)){
    data <- subset(dataAllReps,rep==r)
    
    data$fileName <- i
    dataEnd <- subset(data,gen>=((max(gen)-(cutoff*max(gen)))))
    dataEnd$concatStrat <- paste0(dataEnd$indType,"_",dataEnd$stratType)
    
    dataEndSenders <- subset(dataEnd,indType == "Sender")
    meanAlpha <- mean(dataEndSenders$meanAlphaBeta)
    dataEndReceivers <- subset(dataEnd,indType == "Receiver")
    meanBeta <- mean(dataEndReceivers$meanAlphaBeta)
    
    #remove NA from mean fit - it messes things up later... Just make 0s
    dataEnd$meanFit <- replace(dataEnd$meanFit, dataEnd$meanFit == "NaN", 0)
    
    
    row <- data.frame()
    for (j in unique(dataEnd$concatStrat)){
      temp <- subset(dataEnd,concatStrat == j)
      row <- temp[1,]
      if (row$indType == "Receiver"){
        row$meanAlphaBeta <- meanBeta
      } else {
        row$meanAlphaBeta <- meanAlpha
      }
      
      
      row$fileSuffix <- gsub("_[0-9]","",gsub("_*..csv","",gsub(".*summaryStats_","",row$fileName)))
      row$meanFit <- mean(temp$meanFit)
      row$meanStratNum <- mean(temp$stratNum)
      row$meanStratFreq <- mean(temp$stratNum/row$N)
      row$stratNum <- "NA"
      row$gen <- "NA"
      row$fileNum <- iterator
      
      endDataAll5 <- rbind(endDataAll5,row)
      
    }
    iterator <- iterator + 1
  }
}
#..do we have all c1 and c2 values here? Check val of iterator

if (1==2){
  #Organize parameters for all simulations
  masterParams <- list()
  variableParams <- c()
  paramNames <- c()
  for (c in which(colnames(endDataAll5) == "N"):which(colnames(endDataAll5) == "fileSuffix")){
    masterParams[[c+1-which(colnames(endDataAll5) == "N")]] <- unique(endDataAll5[,c])
    paramNames <- c(paramNames,colnames(endDataAll5)[c])
    if(length(unique(endDataAll5[,c])) > 1 ){
      variableParams<- c(variableParams,colnames(endDataAll5)[c])
    }
  }
  names(masterParams) <- paramNames
  N <- as.numeric(masterParams$N)
  ###
  print(variableParams)
  masterParams
}

endDataAll5$c1c2 <- paste0(endDataAll5$c1,"_",endDataAll5$c2)
endDataAll5$lab <- paste0(endDataAll5$c1,
                          "_",endDataAll5$m,
                          "_",endDataAll5$initializationType,
                          "_",endDataAll5$cauchyDist,
                          "_",endDataAll5$mutRateAlpha,
                          "_",endDataAll5$mutRateStrategySender,
                          "_",endDataAll5$c2
)
write.csv(endDataAll5,file=paste0(dir5,"/_endDataAll5.csv"), row.names=FALSE)

#Add column to order along x axis
if (1==2){
  collect2<-data.frame()
  for (i in unique(endDataAll5$lab)){
    temp <- subset(endDataAll5,endDataAll5$lab==i)
    iterator <- 1
    temp
    collect1 <- data.frame()
    for (j in unique(temp$fileNum)){
      temp2 <- subset(temp,fileNum==j)
      temp2$orderedFileNum <- iterator
      temp2
      collect1<-rbind(collect1,temp2)
      iterator <- iterator + 1
    }
    collect2<-rbind(collect2,collect1)
  }
  collect2$orderedFileNum
  endDataAll5OrderedReduced <- collect2
}
endDataAll5OrderedReduced <- endDataAll5

head(endDataAll5OrderedReduced)
for (i in 1:nrow(endDataAll5OrderedReduced)){
  if (endDataAll5OrderedReduced[i,]$c2<=endDataAll5OrderedReduced[i,]$c1){
    if (endDataAll5OrderedReduced[i,]$indType=="Sender"){
      endDataAll5OrderedReduced[i,]$expAlphaBeta <- 1
    } else {
      endDataAll5OrderedReduced[i,]$expAlphaBeta <- 0
    }
  }
  else if (endDataAll5OrderedReduced[i,]$c2>=1 & endDataAll5OrderedReduced[i,]$c1 < 1){
    if (endDataAll5OrderedReduced[i,]$indType=="Sender"){
      endDataAll5OrderedReduced[i,]$expAlphaBeta <- 0
    } else {
      endDataAll5OrderedReduced[i,]$expAlphaBeta <- 1
    }
  }
}

unique(endDataAll5OrderedReduced$c1)
write.csv(endDataAll5OrderedReduced,file=paste0(dir5,"/_endDataAll5_orderedReduced.csv"), row.names=FALSE)

#Determine Equilibs
#for (tol in c(0.05,0.1,0.25,0.4)){
tol <- 0.20
#add tol to equilib file name
tolerance <- tol
unique(endDataAll5OrderedReduced$c1)
dAll <- endDataAll5OrderedReduced
head(dAll)
dAll$devAlphaBeta <- dAll$meanAlphaBeta-dAll$expAlphaBeta
equilibs <- data.frame()

i <- 1
while (i <= nrow(dAll)){
  
  #determine dominant sender and receiver strat
  row <- dAll[i,]
  #determine expected dominant strategies
  
  #senders
  s<-dAll[i:(i+2),]
  #dominant sender strat
  domSendStrat <- s[which(s$meanStratNum==max(s$meanStratNum)),]$stratType
  devAlpha <- unique(s$meanAlphaBeta - s$expAlphaBeta)
  alpha <- unique(s$meanAlphaBeta)
  meanSendFit <- sum(s$meanFit * s$meanStratFreq)
  #Also find out out the strategy deviation?
  
  #receivers
  r<-dAll[(i+3):(i+5),]
  
  domRecStrat <- r[which(r$meanStratNum==max(r$meanStratNum)),]$stratType
  devBeta <- unique(r$meanAlphaBeta - r$expAlphaBeta)
  beta <- unique(r$meanAlphaBeta)
  meanRecFit <- sum(r$meanFit * r$meanStratFreq)
  
  if (domRecStrat == "strat1" & domSendStrat == "strat1"){
    if (beta >= (1 - tolerance) & alpha <= tolerance){
      equilib <- "Honest"
      #What if it falls within honest but is closer to hybrid?
      if (beta < (1 - (tolerance/2)) & alpha > (tolerance/2)){
        equilib <- "Hybrid"
      }
    } else if (abs(devBeta) < tolerance &  abs(devAlpha) < tolerance & unique(s$expAlphaBeta) > 0 & unique(r$expAlphaBeta) < 1){
      equilib <- "Hybrid"
    } else {
      equilib<-"Pooling"
    }
  } else {
    equilib <- "Pooling"
  }
  
  row$domSendStrat <- domSendStrat
  row$domRecStrat <- domRecStrat
  row$meanSendFit <- meanSendFit
  row$meanRecFit <- meanRecFit
  row$devAlpha <- devAlpha
  row$devBeta <- devBeta
  row$alpha <- alpha
  row$beta <- beta
  row$equilib <- equilib
  equilibs <- rbind(equilibs,row)
  
  i <- i + 6  
}

equilibs$tol <- tol
equilibs$u <- paste0(equilibs$mutRateAlpha,"_",equilibs$mutRateStrategySender)
#..

#### saved data ####
write.csv(equilibs,file=paste0(dir5,"/",tol,"_equilibsAllHonest.csv"), row.names=FALSE)

#I need to run some more negative c2.
#And more positive c1
#I've only done odds with odds and evens with evens. Which I like for readability
#I can throw out all the 'extra value ones'
#I will need to write more of a code for this in r probably
#TO do:
#c1 = 1.4, 1.6 with c2 0, -.2, -.4, -.6, .2, ... 1.6
#c2 = -.2, -.4, -.6 with c1 = -.6, ... 1.6
#Odd high
#c1 = 1.5 with c2 0, -.1, -.3, -.5 ... 0.1 ... 1.5
#
#odd low
#c2 = -0.1, -.3, -.5 with c1 = -.5, ... 1.5
#
#These jobs are running now 135_00-01_...

#### Here HHH plot ####
unique(equilibs$u)
#For now - to show Graeme 5/16 tomorrow meeting
for (i in unique(equilibs$u)){
  data<-subset(equilibs,(equilibs$c2 >= 0) & u==i)
  
  library(scatterpie)
  
  #making data for pie chart
  data$row <- paste0(data$c1,"_",data$c2)
  
  pies <- data.frame()
  for (r in unique(data$row)){
    dTemp <- subset(data,row==r)
    sumHybrid <- sum(dTemp$equilib=="Hybrid")
    sumHonest <- sum(dTemp$equilib=="Honest")
    sumPooling <- sum(dTemp$equilib=="Pooling")
    pRow<-data.frame(r)
    pRow$c1Pie <- unique(dTemp$c1)
    pRow$c2Pie <- unique(dTemp$c2)
    pRow$Honest <- sumHonest
    pRow$Hybrid <- sumHybrid
    pRow$Pooling <- sumPooling
    pies <- rbind(pies,pRow)
  }
  pies2<-pies[,-1]
  #For publication!
  #This is for HONEST START. I am running more sims for odd c1 and c2 values to add to this.
  
  p<-ggplot(data,aes(c1,c2)) +
    geom_vline(xintercept=1,linetype="dashed") + 
    geom_vline(xintercept=0) + 
    geom_hline(yintercept=1,linetype="dashed") + 
    geom_hline(yintercept=0) + 
    geom_abline(intercept = 0, slope = 1, 
                linetype="dashed") +
    geom_scatterpie(aes(x=c1Pie,y=c2Pie,group=r),data=pies2,
                    cols = colnames(pies2)[3:5]) +
    scale_fill_brewer(palette="Set1") +
    theme_bw() +
    labs(title = paste0(
      "Mut Rate Alpha Beta: ",unique(data$mutRateAlpha),
      "\nMut Rate Strategy: ",unique(data$mutRateStrategySender)
    ))
  
  ggsave(paste0(i,".png"),plot = p,
         device="png",path=dir5,height=8,width=10,unit="in")
  
  
}

#for (i in unique(equilibs$u))
data<-subset(equilibs,(equilibs$c2 >= 0))

library(scatterpie)

#making data for pie chart
data$row <- paste0(data$c1,"_",data$c2,"_",data$u)
pies <- data.frame()
for (r in unique(data$row)){
  dTemp <- subset(data,row==r)
  sumHybrid <- sum(dTemp$equilib=="Hybrid")
  sumHonest <- sum(dTemp$equilib=="Honest")
  sumPooling <- sum(dTemp$equilib=="Pooling")
  pRow<-data.frame(r)
  pRow$c1Pie <- unique(dTemp$c1)
  pRow$c2Pie <- unique(dTemp$c2)
  pRow$Honest <- sumHonest
  pRow$Hybrid <- sumHybrid
  pRow$Pooling <- sumPooling
  pRow$mutRateAlphaBeta <- unique(dTemp$mutRateAlpha)
  pRow$mutRateStrat <- unique(dTemp$mutRateStrategySender)
  pies <- rbind(pies,pRow)
}
pies2<-pies[,-1]

p<-ggplot(data,aes(c1,c2)) +
  geom_vline(xintercept=1,linetype="dashed") + 
  geom_vline(xintercept=0) + 
  geom_hline(yintercept=1,linetype="dashed") + 
  geom_hline(yintercept=0) + 
  geom_abline(intercept = 0, slope = 1, 
              linetype="dashed") +
  geom_scatterpie(aes(x=c1Pie,y=c2Pie,group=r),data=pies2,
                  cols = colnames(pies2)[3:5]) +
  theme_bw() +
  scale_fill_brewer(palette="Set1") +
  labs(title="No Signalling Start") +
  labs(subtitle="Mut Rate Strategy") +
  labs(fill="Mut Rate\nAlpha Beta\n\nEquilibrium") +
  facet_grid(mutRateAlphaBeta~mutRateStrat)
p
ggsave(paste0("all.png"),plot=p,
       device="png",path=dir5,height=8,width=10,unit="in")

