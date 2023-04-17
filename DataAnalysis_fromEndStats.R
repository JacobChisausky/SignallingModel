library(tidyverse)
#Using endStats.csv files for data analysis

dir <- "C:/Users/owner/Documents/DONE"
list.files(dir,"*endStats.csv")
selection <- c(1,2,3,4)

iterator <- 1
endDataAll<-data.frame()
for (i in selection){
  data <- read.csv(paste0(dir,"/",list.files(dir,"*endStats.csv")[i]))
  data$fileNumberAll <- 0
  data$fileName <- list.files(dir,"*endStats.csv")[i]
  #Adding fileNumberAll before combining data
  for (r in 1:nrow(data)){
    data[r,]$fileNumberAll <- iterator
    iterator <- iterator + 1
  }
  endDataAll <- rbind(endDataAll,data)
  
}

endDataAll


# #Need to read summaryStats files to get parameters now.
# summaryParamsAll<-data.frame()
# sFiles <- list.files(dir,"95.*summaryStats.csv")
# for (i in sFiles){
#   summary <- read.csv(paste0(dir,"/",i))
#   summaryParams <- subset(summary,gen==max(summary$gen)&indType=="Sender"&stratType=="strat1")
#   filePrefix<-gsub("_summaryStats.csv","",gsub(".*[0-9]_","",i))
#   summaryParams$fileName <- i
#   summaryParams$filePrefix <- filePrefix
#   
#   summaryParamsAll <- rbind(summaryParamsAll,summaryParams)
# }
# nrow(summaryParamsAll)==nrow(endDataAll)
# 
# endDataAll$filePrefix <- gsub("_endStats.csv","",endDataAll$fileName)
# summaryParamsAll2 <- summaryParamsAll[,-(3:9)]
# summaryParamsAll2$filePrefix <- gsub("_summaryStats.csv","",summaryParamsAll2$fileName)
# summaryParamsAll2
# subset(summaryParamsAll2,filePrefix==endDataAll[1,]$filePrefix)


endDataAll
#Organize parameters for all simulations
masterParams <- list()
variableParams <- c()
paramNames <- c()
for (c in which(colnames(endDataAll) == "N"):ncol(endDataAll)){
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

#First experiment - start conditions and c2 and mut staps and mut rates
endDataAll$startCondition <- ""
for (i in 1:nrow(endDataAll)){
  if (endDataAll[i,]$filePrefix == "95_12_42_54_muSensitivityRandInit"){
    endDataAll[i,]$startCondition <- "Random"
  } 
  if (endDataAll[i,]$filePrefix == "95_12_43_18_muSensitivityHonestStart"){
    endDataAll[i,]$startCondition <- "Honest"
  } 
  if (endDataAll[i,]$filePrefix == "95_12_43_31_muSensitivityNoSigStart"){
    endDataAll[i,]$startCondition <- "Quiet"
  } 
  if (endDataAll[i,]$filePrefix == "95_12_44_06_muSensitivityRandStartCloseC2"){
    endDataAll[i,]$startCondition <- "Random"
  } 
}
write.csv(endDataAll,paste0(dir,"/","95_starts_endDataAll.csv"))

masterParams

d<-subset(endDataAll,c2==0.75 & mutStepAlpha == 0.10 & mutRateAlpha == 0.01 & mutRateStrategySender == 0.01)
d<-subset(endDataAll,
          c2==0.75 
          #  mutStepAlpha == 0.10 
          #  mutRateAlpha == 0.01 
          #  mutRateStrategySender == 0.01
)


ggplot(d) + 
  geom_jitter(aes(x=as.factor(startCondition),group=startCondition,y=meanAlpha,color=as.factor(c2)),height=0,alpha=0.8,shape="S",size=2) +
  geom_jitter(aes(x=as.factor(startCondition),group=startCondition,y=meanBeta,color=as.factor(c2)),height=0,alpha=0.8,shape="R",size=2) +
  ylim(0,1) + theme_bw() +
  geom_point(aes(y=expAlpha,x=as.factor(startCondition),color=as.factor(c2)),shape="A",size=4) +
  geom_point(aes(y=expBeta,x=as.factor(startCondition),color=as.factor(c2)),shape="B",size=4)
# facet_grid(mutRateAlpha+mutRateStrategySender~mutStepAlpha,labeller=labeller(label_both))

head(d)


#For thesis
d<-subset(endDataAll, c2==0.4) 
d<-subset(endDataAll, c2==0.75) 

d$mu_AlphaBeta <- d$mutRateAlpha
d$mu_Strategy <- d$mutRateStrategySender
d$mut_Size <- d$mutStepAlpha

p<-ggplot(d) + 
  geom_jitter(aes(x=as.factor(startCondition),group=startCondition,y=devAlpha),color="blue",height=0,alpha=0.8,shape="A",size=2) +
  geom_jitter(aes(x=as.factor(startCondition),group=startCondition,y=devBeta),color="red",height=0,alpha=0.8,shape="B",size=2) +
  theme_bw() +
  geom_jitter(aes(x=as.factor(startCondition),y=-1+meanSS1),color="lightblue",alpha=0.7,size=0.9) +
  geom_jitter(aes(x=as.factor(startCondition),y=-1+meanRS1),color="pink",alpha=0.7,size=0.9) +
  facet_grid(mu_AlphaBeta+mu_Strategy~mut_Size, labeller = labeller(.rows = label_both, .cols = label_both)) +
  labs(x="Starting Condition") +
  labs(y="Deviation from Hybrid Equilibrium") +
  labs(caption="A = Alpha.  B = Beta.  Blue dot = sender strategy 1 frequency.  Pink dot = receiver strategy 1 frequency (deviation from 1.0)") +
  labs(title = paste0("c2 = ",unique(d$c2)))
ggsave(paste0(unique(d$c2),"_muSensitivity.png"),plot=p,device="png",path=dir)


head(d)


ggplot(d) + 
  geom_jitter(aes(x=as.factor(paste0(fileNumberAll,"_startCondition")),group=startCondition,y=meanAlpha),color="blue",height=0,alpha=0.8,shape="A",size=2) +
  geom_jitter(aes(x=as.factor(paste0(fileNumberAll,"_startCondition")),group=startCondition,y=meanBeta),color="red",height=0,alpha=0.8,shape="B",size=2) +
  theme_bw() +
  geom_jitter(aes(x=as.factor(paste0(fileNumberAll,"_startCondition")),y=meanSS1),color="lightblue",alpha=0.7,size=0.9) +
  geom_jitter(aes(x=as.factor(paste0(fileNumberAll,"_startCondition")),y=meanRS1),color="pink",alpha=0.7,size=0.9) +
  
  geom_jitter(aes(x=as.factor(paste0(fileNumberAll,"_startCondition")),y=meanSS2),color="green",alpha=0.7,size=0.7) +
  geom_jitter(aes(x=as.factor(paste0(fileNumberAll,"_startCondition")),y=meanRS2),color="orange",alpha=0.7,size=0.7) +
  
  facet_grid(mu_AlphaBeta+mu_Strategy~mut_Size, labeller = labeller(.rows = label_both, .cols = label_both)) +
  labs(x="Starting Condition") +
  labs(y="Deviation from Hybrid Equilibrium") +
  labs(caption="A = Alpha.  B = Beta.  Blue dot = sender strategy 1 frequency.  Pink dot = receiver strategy 1 frequency (deviation from 1.0)") +
  labs(title = paste0("c2 = ",unique(d$c2)))

ggplot(d) + 
  geom_point(aes(x=as.factor(paste0(fileNumberAll,"_startCondition")),group=startCondition,y=meanAlpha),color="blue",height=0,alpha=0.8,shape="A",size=2) +
  geom_point(aes(x=as.factor(paste0(fileNumberAll,"_startCondition")),group=startCondition,y=meanBeta),color="red",height=0,alpha=0.8,shape="B",size=2) +
  theme_bw() +
  geom_point(aes(x=as.factor(paste0(fileNumberAll,"_startCondition")),y=meanSS1),color="lightblue",alpha=0.7,size=0.9) +
  geom_point(aes(x=as.factor(paste0(fileNumberAll,"_startCondition")),y=meanRS1),color="pink",alpha=0.7,size=0.9) +
  
  geom_point(aes(x=as.factor(paste0(fileNumberAll,"_startCondition")),y=meanSS2),color="green",alpha=0.7,size=0.7) +
  geom_point(aes(x=as.factor(paste0(fileNumberAll,"_startCondition")),y=meanRS2),color="orange",alpha=0.7,size=0.7) +
  
  facet_grid(mu_AlphaBeta+mu_Strategy~mut_Size, labeller = labeller(.rows = label_both, .cols = label_both)) +
  labs(x="Starting Condition") +
  labs(y="Deviation from Hybrid Equilibrium") +
  labs(caption="A = Alpha.  B = Beta.  Blue dot = sender strategy 1 frequency.  Pink dot = receiver strategy 1 frequency (deviation from 1.0)") +
  labs(title = paste0("c2 = ",unique(d$c2)))

#Not deviation but real vals

#d<-subset(endDataAll,c2==0.75 & mutStepAlpha == 0.10 & mutRateAlpha == 0.01 & mutRateStrategySender == 0.01)

for (c in c(0.4,0.75)){
  d<-subset(endDataAll,c2==c)
  
  d$mu_AlphaBeta <- d$mutRateAlpha
  d$mu_Strategy <- d$mutRateStrategySender
  d$mut_Size <- d$mutStepAlpha
  colnames(d)[1]<-"row"
  d$order1<-0
  
  Ua <- unique(d$mu_AlphaBeta)
  Ub <- unique(d$mu_Strategy)
  Uc <- unique(d$mut_Size)
  dAll <- data.frame()
  for (a in Ua){
    for (b in Ub){
      for (c in Uc){
        dTemp <- subset(d,mu_AlphaBeta==a & mu_Strategy == b & mut_Size == c)
        
        attach(dTemp)
        dTemp<-dTemp[order(as.factor(startCondition)),]
        dTemp$order1 <- 0
        i<-1
        for (r in 1:nrow(dTemp)){
          dTemp[r,]$order1 <- i
          i <- i + 1
          if (r < nrow(dTemp) & dTemp[r,]$startCondition != dTemp[r+1,]$startCondition){
            i <- i + 10
          }
        }  
        
        detach(dTemp)
        dAll <- rbind(dAll,dTemp)
      }
    }
  }
  
  #old - good for no facets. Does not work if you have facets
  # attach(d)
  # dSort<-d[order(as.factor(startCondition)),]
  # dSort$order <- 0
  # i<-1
  # for (r in i:nrow(dSort)){
  #   dSort[r,]$order <- i
  #   i <- i + 1
  #   if (r < nrow(dSort) & dSort[r,]$startCondition != dSort[r+1,]$startCondition){
  #     i <- i + 10
  #   }
  # }
  
  
  
  #Order is honest, quiet, random on x axis
  
  p<-ggplot(dAll,aes(x=order1)) + 
    geom_point(aes(y=meanAlpha),color="royalblue1",alpha=1,shape="A",size=2) +
    geom_point(aes(y=meanBeta),color="red",alpha=1, shape="B",size=2) +
    theme_bw() +
    geom_point(aes(y=meanSS1),color="steelblue1",alpha=1,shape="1",size=2) +
    geom_point(aes(y=meanRS1),color="violetred1",alpha=1,shape="1",size=2) +
    
    geom_point(aes(y=meanSS2),color="navy",alpha=1,shape="2",size=2) +
    geom_point(aes(y=meanRS2),color="red3",alpha=1,shape="2",size=2) +
    
    geom_line(aes(y=expAlpha),color="royalblue1",size=1) +
    geom_line(aes(y=expBeta),color="red",size=1) +
    ylim(0,1) + 
    labs(x="Honest                      Quiet                      Random") +
    
    facet_grid(mu_AlphaBeta+mu_Strategy~mut_Size, labeller = labeller(.rows = label_both, .cols = label_both)) +
    labs(y="Frequency") +
    labs(title = paste0("c2 = ",unique(d$c2)))
  ggsave(plot=p,paste0(unique(dAll$c2),"_muSensitivityRealVals.png"),
         device="png",path=dir,height=10,width=16,unit="in")
  
  
  p4<-ggplot(dAll,aes(x=order1)) + 
    geom_point(aes(y=meanAlpha),color="royalblue1",alpha=1,shape="A",size=2) +
    geom_point(aes(y=meanBeta),color="red",alpha=1, shape="B",size=2) +
    theme_bw() +
    #  geom_point(aes(y=meanSS1),color="steelblue1",alpha=1,shape="1",size=2) +
    #  geom_point(aes(y=meanRS1),color="violetred1",alpha=1,shape="1",size=2) +
    
    #  geom_point(aes(y=meanSS2),color="navy",alpha=1,shape="2",size=2) +
    #  geom_point(aes(y=meanRS2),color="red3",alpha=1,shape="2",size=2) +
    
    geom_line(aes(y=expAlpha),color="royalblue1",size=1) +
    geom_line(aes(y=expBeta),color="red",size=1) +
    ylim(0,1) + 
    labs(x="Honest                      Quiet                      Random") +
    
    facet_grid(mu_AlphaBeta+mu_Strategy~mut_Size, labeller = labeller(.rows = label_both, .cols = label_both)) +
    labs(y="Frequency") +
    labs(title = paste0("c2 = ",unique(d$c2)))
  ggsave(plot=p4,paste0(unique(dAll$c2),"_muSensitivityRealVals_noStrat.png"),
         device="png",path=dir,height=10,width=16,unit="in")
  
  ###And now, deviance instead of real vals
  
  p2<-ggplot(dAll,aes(x=order1)) + 
    geom_line(y=0,size=.7,color="black") +
    geom_point(aes(y=devAlpha),color="royalblue1",alpha=1,shape="A",size=2) +
    geom_point(aes(y=devBeta),color="red",alpha=1, shape="B",size=2) +
    theme_bw() +
    geom_point(aes(y=meanSS1-1),color="steelblue1",alpha=1,shape="1",size=2) +
    geom_point(aes(y=meanRS1-1),color="violetred1",alpha=1,shape="1",size=2) +
    
    
    geom_point(aes(y=meanSS2),color="navy",alpha=1,shape="2",size=2) +
    geom_point(aes(y=meanRS2),color="red3",alpha=1,shape="2",size=2) +
    
    labs(x="Honest                      Quiet                      Random") +
    
    facet_grid(mu_AlphaBeta+mu_Strategy~mut_Size, labeller = labeller(.rows = label_both, .cols = label_both)) +
    labs(y="Deviation from Hybrid Equilibrium") +
    labs(title = paste0("c2 = ",unique(d$c2)))
  ggsave(plot=p2,paste0(unique(dAll$c2),"_muSensitivityDeviation.png"),
         device="png",path=dir,height=10,width=16,unit="in")
  
  p3<-ggplot(dAll,aes(x=order1)) + 
    geom_line(y=0,size=.7,color="black") +
    geom_point(aes(y=devAlpha),color="royalblue1",alpha=1,shape="A",size=2) +
    geom_point(aes(y=devBeta),color="red",alpha=1, shape="B",size=2) +
    theme_bw() +
    #  geom_point(aes(y=meanSS1-1),color="steelblue1",alpha=1,shape="1",size=2) +
    #  geom_point(aes(y=meanRS1-1),color="violetred1",alpha=1,shape="1",size=2) +
    
    #  geom_point(aes(y=meanSS2),color="navy",alpha=1,shape="2",size=2) +
    #  geom_point(aes(y=meanRS2),color="red3",alpha=1,shape="2",size=2) +
    
    labs(x="Honest                      Quiet                      Random") +
    
    facet_grid(mu_AlphaBeta+mu_Strategy~mut_Size, labeller = labeller(.rows = label_both, .cols = label_both)) +
    labs(y="Deviation from Hybrid Equilibrium") +
    labs(title = paste0("c2 = ",unique(d$c2)))
  ggsave(plot=p3,paste0(unique(dAll$c2),"_muSensitivityDeviation_noStrats.png"),
         device="png",path=dir,height=10,width=16,unit="in")
  
  
}

#Let's investigate fitnesses for different strategies
d<-endDataAll


d$mu_AlphaBeta <- d$mutRateAlpha
d$mu_Strategy <- d$mutRateStrategySender
d$mut_Size <- d$mutStepAlpha

d$startCondition

#need to melt d to compare strategy fitnesses
dStratMeans <- data.frame()
for (i in c("meanSS1","meanSS2","meanSS3","meanRS1","meanRS2","meanRS3")){
  col <- which(colnames(d)  ==i)
  d1<-d
  colnames(d)[12]
  d1$meanStrategyFit <- d[,col]
  d1$strategyFitType <- gsub("mean","",colnames(d)[col])
  dStratMeans <- rbind(dStratMeans,d1)  
}
dStratMeans
#add startCondition as ?color?
#expss1 = T1 Sig A1 + T1 NoSig A1 + 




for (c in c(0.4,0.75)){
  p<-ggplot(subset(dStratMeans,c2==c),aes(x=as.factor(strategyFitType))) + 
    geom_jitter(aes(y=meanStrategyFit
                    #,color=as.factor(startCondition)
                    )
                ,height=0,alpha=0.3) +
    
    theme_bw() +
    
    facet_grid(mu_AlphaBeta+mu_Strategy~mut_Size, labeller = labeller(.rows = label_both, .cols = label_both)) +
    labs(title = paste0("Fitnesses of strategies. c2 = ",c)) +
    labs(x="Strategy",y="Mean Fitness",color="Initialization")
  ggsave(plot=p,paste0(c,"_StrategyFitnesses.png"),
         device="png",path=dir,height=10,width=16,unit="in")
  
}
#Perhaps I should make analytical predictions for fitness and determine why SS1 fitness is depressed
#Fitnesses seem too HIGH
#Fit SS1 =  


#Determine if hybrid equilib reached or no?
for (tol in c(0.05,0.1,0.25,0.4)){
  
  dAll <- endDataAll
  tolerance <- tol
  dAll$equilib <- -1
  for (i in 1:nrow(dAll)){
    if (abs(dAll[i,]$devAlpha) < tolerance & abs(dAll[i,]$devBeta) < tolerance & dAll[i,]$meanSS1 > (1 - tolerance) & dAll[i,]$meanRS1 > (1 - tolerance) ){
      dAll[i,]$equilib <- 1
    } else {
      dAll[i,]$equilib <- 0
    }
  }
  dAll$mu_AlphaBeta <- dAll$mutRateAlpha
  dAll$mu_Strategy <- dAll$mutRateStrategySender
  dAll$mut_Size <- dAll$mutStepAlpha
  
  Ua <- unique(dAll$mu_AlphaBeta)
  Ub <- unique(dAll$mu_Strategy)
  Uc <- unique(dAll$mut_Size)
  Ue <- unique(dAll$c2)
  Uf <- unique(dAll$startCondition)
  dAgg <- data.frame()
  for (a in Ua){
    for (b in Ub){
      for (c in Uc){
        for (e in Ue){
          for (f in Uf){
            dTemp <- subset(dAll,mu_AlphaBeta==a & mu_Strategy == b & mut_Size == c  & c2 == e & startCondition == f)
            yes <- sum(dTemp$equilib)
            no <- nrow(dTemp) - yes
            
            row <- dTemp[1,]
            row$yes <- yes
            row$no <- no
            
            dAgg <- rbind(dAgg, row)
          }
        }
      }
    }
  }
  
  p4<- ggplot(dAgg) + 
    geom_point(aes(y=(yes/(yes+no)),  x=as.factor(startCondition),  group=as.factor(startCondition),color=as.factor(c2)),size=3,alpha=0.66) +
    facet_grid(mu_AlphaBeta+mu_Strategy~mut_Size, labeller = labeller(.rows = label_both, .cols = label_both)) +
    theme_bw() + labs(x="Initial Condition") + 
    labs(y="Percent of Reps Reaching Hybrid Equilib") +
    labs(title=paste0("Tolerance: ",tol))
  
  ggsave(plot=p4,paste0(tol,"_tolerance_muSensitivityAgg.png"),
         device="png",path=dir,height=10,width=13,unit="in")
  
}

