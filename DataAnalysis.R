library(ggplot2)
library(grid)
library(gtable)

dir <- "C:/Users/owner/Documents/S4/Simulation"

list.files(dir,"data.*.csv")
fileSelect <- 7


data <- read.csv(paste0(dir,"/",list.files(dir,"data.*.csv")[fileSelect]))
params <- read.csv(paste0(dir,"/",list.files(dir,"params.*.csv")[fileSelect]))
N <- as.numeric(params$N)

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


expBeta <- as.numeric(params$c2)
expAlpha <- as.numeric(params$m/(1-params$m))
summaryStats$exp <- 0
for (i in 1:nrow(summaryStats)){
  summaryStats[i,]$exp<-ifelse(summaryStats[i,]$indType=="Sender",expAlpha,expBeta)
}


lab <- paste0("c1: ",params$c1,
              "\nc2: ",params$c2,
              "\nv1: ",params$v1,
              "\nv2: ",params$v2,
              "\nw1: ",params$w1,
              "\nw2: ",params$w2,
              "\nw3: ",params$w3,
              "\nw4: ",params$w4,
              "\nmuRateA: ",params$mutRateAlpha,
              "\nmuRateB: ",params$mutRateBeta,
              "\nmuRateSS: ",params$mutRateStrategySender,
              "\nmuRateSR: ",params$mutRateStrategyReceiver,
              "\nmuStepA: ",params$mutStepAlpha,
              "\nmuStepB: ",params$mutStepBeta
              )



p<-ggplot(data=subset(summaryStats,rep==1),aes(x=as.numeric(gen))) +
  geom_line(aes(y=exp*N)) +
  geom_point(aes(y=as.numeric(stratNum),color=paste0(stratType))) +
  geom_path(aes(y=as.numeric(stratNum),color=paste0(stratType)),alpha=0.1) +
  geom_point(aes(y=as.numeric(as.numeric(meanAlphaBeta)*as.numeric(params$N)),color=paste0(indType)),shape=4) +
  geom_path(aes(y=as.numeric(as.numeric(meanAlphaBeta)*as.numeric(params$N)),color=paste0(indType)),alpha=0.1) +
  theme_bw() + 
  facet_grid(indType~.) + 
  labs(color="Data",x="Generation") +
  scale_y_continuous(
    name = "Count",
    sec.axis = sec_axis(~.*1/N, name="Alpha or Beta")) +
  scale_color_hue(labels = c("Alpha", "Beta","Strategy 1","Strategy 2","Strategy 3"))

{
label = lab
g = ggplotGrob(p)
g$layout$clip[g$layout$name == "panel"] = "off"
grid.draw(g)
textgrob = textGrob(label, gp = gpar(cex = .75), )
width = unit(1, "grobwidth",textgrob) + unit(10, "points")
height = unit(1, "grobheight", textgrob)+ unit(10, "points")
rectgrob = rectGrob(gp=gpar(colour = "black", fill = NA), height = height, width = width)
labelGrob = gTree("labelGrob", children = gList(rectgrob, textgrob))
g = ggplotGrob(p)
leg = g$grobs[[which(g$layout$name == "guide-box")]]
xpos = 5
textgrob = textGrob(x = unit(xpos, "points"), label, gp = gpar(cex = .75), just = "left")
width = unit(1, "grobwidth",textgrob) + unit(2*xpos, "points")  # twice the x position
height = unit(1, "grobheight", textgrob)+ unit(2*xpos, "points")
rectgrob = rectGrob(x = unit(0, "points"), just = "left", 
                    gp = gpar(colour = "black", fill = NA), height = height, width = width)
labelGrob = gTree("labelGrob", children = gList(rectgrob, textgrob))
pos = subset(leg$layout, grepl("guides", name), t:r)
leg = gtable_add_rows(leg, height, pos = pos$t+1)
leg = gtable_add_grob(leg, labelGrob, t = pos$t+2, l = pos$l)
leg$widths[pos$l] = max(width, leg$widths[pos$l])
leg$heights[pos$t+1] = unit(5, "pt")
g$grobs[[which(g$layout$name == "guide-box")]] = leg
g$widths[g$layout[grepl("guide-box", g$layout$name), "l"]] = max(width, sum(leg$widths))
grid.newpage()
}
grid.draw(g)


