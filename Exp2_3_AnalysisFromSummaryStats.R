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




#Exper 2D - New Normalization Routine #### 
dirExp2D <- "C:/Users/owner/Documents/DONE/Exper_2D"
summaryFilesExp2D <- list.files(dirExp2D,"*summaryStats*")

#First - create endData stats from summaryStats files produced by cpp
cutoff <- 0.25   #Look at last this % of generations
iterator <- 1
endDataAllExp2D<-data.frame()
for (i in summaryFilesExp2D){
  
  dataAllReps <- read.csv(paste0(dirExp2D,"/",i))
  
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
      
      endDataAllExp2D <- rbind(endDataAllExp2D,row)
      
    }
    iterator <- iterator + 1
  }
}

head(endDataAllExp2D)

#### #### 


#Organize parameters for all simulations
masterParams <- list()
variableParams <- c()
paramNames <- c()
for (c in which(colnames(endDataAllExp2D) == "N"):which(colnames(endDataAllExp2D) == "fileSuffix")){
  masterParams[[c+1-which(colnames(endDataAllExp2D) == "N")]] <- unique(endDataAllExp2D[,c])
  paramNames <- c(paramNames,colnames(endDataAllExp2D)[c])
  if(length(unique(endDataAllExp2D[,c])) > 1 ){
    variableParams<- c(variableParams,colnames(endDataAllExp2D)[c])
  }
}
names(masterParams) <- paramNames
N <- as.numeric(masterParams$N)
###
print(variableParams)
masterParams



endDataAllExp2D$c1c2 <- paste0(endDataAllExp2D$c1,"_",endDataAllExp2D$c2)

#Add column to order along x axis
collect2<-data.frame()
for (i in unique(endDataAllExp2D$c1c2)){
  temp <- subset(endDataAllExp2D,c1c2==i)
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

endDataAllExp2DOrderedReduced <- collect2

head(endDataAllExp2DOrderedReduced)
for (i in 1:nrow(endDataAllExp2DOrderedReduced)){
  if (endDataAllExp2DOrderedReduced[i,]$c2<=endDataAllExp2DOrderedReduced[i,]$c1){
    if (endDataAllExp2DOrderedReduced[i,]$indType=="Sender"){
      endDataAllExp2DOrderedReduced[i,]$expAlphaBeta <- 1
    } else {
      endDataAllExp2DOrderedReduced[i,]$expAlphaBeta <- 0
    }
  }
  else if (endDataAllExp2DOrderedReduced[i,]$c2>=1 & endDataAllExp2DOrderedReduced[i,]$c1 < 1){
    if (endDataAllExp2DOrderedReduced[i,]$indType=="Sender"){
      endDataAllExp2DOrderedReduced[i,]$expAlphaBeta <- 0
    } else {
      endDataAllExp2DOrderedReduced[i,]$expAlphaBeta <- 1
    }
  }
}

#endDataAllExp2DOrderedReduced <- subset(endDataAllExp2DOrderedReduced,orderedFileNum<=50)

endDataAllExp2DOrderedReduced

library(ggplot2)
#install.packages("rlang")

#plot
ggplot(subset(endDataAllExp2DOrderedReduced,mutStepAlpha==0.25),aes(x=orderedFileNum)) +
  geom_point(aes(y=meanStratFreq,color=stratType),alpha=.7) + 
  geom_point(aes(y=meanAlphaBeta),shape="X") +
  geom_line(aes(y=expAlphaBeta),color="orange",alpha=0.9,size=.8) +
  theme_bw() +
  facet_grid(c1+indType~c2) + 
  labs(x="Replicate",y="Strategy Freq or Alpha or Beta") +
  labs(subtitle = "c2") + 
  labs(color =  "c1\n\n\nStrategy") +
  labs(title = "NewNorm")
ggsave("NewNorm.png",
       device="png",path=dir,height=8,width=10,unit="in")

#Next - look at mean fitnesses
head(endDataAllExp2DOrderedReduced)
endDataAllExp2DOrderedReduced$shortStrat <- ""
for (i in 1:nrow(endDataAllExp2DOrderedReduced)){
  if (endDataAllExp2DOrderedReduced[i,]$concatStrat=="Receiver_strat1"){
    endDataAllExp2DOrderedReduced[i,]$shortStrat <- "RS1"
  }
  if (endDataAllExp2DOrderedReduced[i,]$concatStrat=="Receiver_strat2"){
    endDataAllExp2DOrderedReduced[i,]$shortStrat <- "RS2"
  }
  if (endDataAllExp2DOrderedReduced[i,]$concatStrat=="Receiver_strat3"){
    endDataAllExp2DOrderedReduced[i,]$shortStrat <- "RS3"
  }
  if (endDataAllExp2DOrderedReduced[i,]$concatStrat=="Sender_strat1"){
    endDataAllExp2DOrderedReduced[i,]$shortStrat <- "SS1"
  }
  if (endDataAllExp2DOrderedReduced[i,]$concatStrat=="Sender_strat2"){
    endDataAllExp2DOrderedReduced[i,]$shortStrat <- "SS2"
  }
  if (endDataAllExp2DOrderedReduced[i,]$concatStrat=="Sender_strat3"){
    endDataAllExp2DOrderedReduced[i,]$shortStrat <- "SS3"
  }
}

ggplot(endDataAllExp2DOrderedReduced,aes(x=as.factor(shortStrat))) +
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





