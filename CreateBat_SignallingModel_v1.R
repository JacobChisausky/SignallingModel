library("lubridate")

##How to use this script: 
#set "parameters_in_file_name" to the parameters you want shown in the name of your .json files
#set "windowsExeName" to the name of the .exe file you use on windows (if desired)
#set "sh_BinaryName" to the name of your compiled code on Linux
#if "peregrine_job_name_custom" is left blank (""), a default will be applied.
#set "writeBatOrSh" to write either a bat file, a sh script, or both
#"exeFolder" is where your executable file for windows is, and where the exeFolder created by this script will be stored
#
#Enter parameter values to create .json files and scripts for
#NOTE:
#You many enter up to two parameters as vectors (c(x,y,z,...)). .json files for all combinations of these parameters will be created
#For now, string parameters need to be entered as "\"text\""


#sh_BinaryName      <- "program"  #for when we move to HPC
#peregrine_custom_job_name <- ""
#peregrine_time     <- "0-08:00:00"

writeBatOrSh       <- "bat" #accepts "both", "bat", or "sh" - for when we move to HPC

exeFolder <- "C:/Users/owner/eclipse-workspace/Signalling Model/Debug"  #Where is the .exe located?
windowsExeName <- "signalling_model.exe" 

jobFolder <- "C:/Users/owner/Documents/S4/Simulation"  #The folder path when you want the JOBS to be written to

jobFolderNew <- "rScriptTest2"    #This will be appended to the end of jobFolder to write these jobs into a new folder. Leave blank to write jobs into base folder
fileName <- "rScriptTest"   #The name of files CPP will write

json_per_script <- 3 #Leave 0 to add all json files to one script. This allows jobs to be run in parallel - many json per script = sequential, few per script = parallel if you have multiple CPUs

seed <- 0  #If 0, use a rng to set seed. It will be recorded in param file in case you want to rerun a simulation. If you enter a nonzero value here, it will be used as seed
N   <- c(200,100,10)
G   <- c(10000)
c1  <- c(0.5)
c2  <- c(0.9)
v1  <- c(1)
v2  <- c(1)
w1  <- c(1)
w2  <- c(0)
w3  <- c(0)
w4  <- c(1)
m   <- c(0.4)
interactionPartners <- c(1)
mutRateAlpha        <- c(0.01)
mutRateBeta         <- c(0.01)
mutRateStrategySender   <- c(0.01)
mutRateStrategyReceiver <- c(0.01)
mutStepAlpha <- c(0.4)
mutStepBeta  <- c(0.4)
alphaBetaMutation  <- c("\"random\"")
initializationType <- c("\"random\"")
cauchyDist  <- c("true")
initStrategySender   <- c(1)
initStrategyReceiver <- c(1)
initAlpha      <- c(1)
initBeta       <- c(0)
replicates     <- c(2)
coutReport     <- c(0)
reportFreq     <- c(500)

##-------------------------------------

#strFileName   <- paste0("\"",fileName,"\"")
#strFolderName <- paste0("\"",paste0(jobFolder,"/",jobFolderNew),"\"")

#if (peregrine_custom_job_name == ""){
#  peregrine_job_name <- paste("job_",paste(params_in_file_name,collapse="_"),sep="")
#} else {
#  peregrine_job_name <- peregrine_custom_job_name
#}

#folderTime<-format(Sys.time(),"%Y_%d_%m_%H_%M_%S")
#folderPath <- (paste(exeFolder,"/",folderTime,"_",paste(params_in_file_name,collapse="_"),"/",sep=""))

#if (custom_folder_name != ""){
#  folderPath <- (paste(exeFolder,"/",custom_folder_name,"/",sep=""))
#}

#Add time to folder path in same way as cpp - so that alphabetically, the newest folder is always last
t_hr <- format(Sys.time(), "%H")
t_min <- format(Sys.time(), "%M")
t_sec <- format(Sys.time(), "%S")
t_yday <- yday(Sys.Date())-1
date <- paste(t_yday,t_hr,t_min,t_sec,sep="_")

folderPath<-paste0(jobFolder,"/",date,"_",jobFolderNew)
dir.create(folderPath)
fileNameList<-c()
iterator<-1;  #add this to each job title in case two jobs get submitted with same name at same second

#Check to see if any files in folder share name and if so, increase iterator so as to not overwrite them

params<-list()
params<-list(seed,N,G,c1,c2,v1,v2,w1,w2,w3,w4,m,interactionPartners,mutRateAlpha,
             mutRateBeta,mutRateStrategySender,mutRateStrategyReceiver,mutStepAlpha,
             mutStepBeta,alphaBetaMutation,initializationType,cauchyDist,
             initStrategySender,initStrategyReceiver,initAlpha,initBeta,replicates,
             coutReport,reportFreq,dataFileName,dataFileFolder)
paramsNames <- c("seed","N","G","c1","c2","v1","v2","w1","w2","w3","w4","m","interactionPartners","mutRateAlpha",
                 "mutRateBeta","mutRateStrategySender","mutRateStrategyReceiver","mutStepAlpha",
                 "mutStepBeta","alphaBetaMutation","initializationType","cauchyDist",
                 "initStrategySender","initStrategyReceiver","initAlpha","initBeta","replicates",
                 "coutReport","reportFreq","dataFileName","dataFileFolder")

#names<-c()  #This chunk is for setting the .json file names, based on params_in_file_name
#i<-1
#for (i in 1:length(params_in_file_name)){
#  names<-append(names,match(params_in_file_name[i],paramsNames))
#}
#names<-sort(names)

for (X1 in seed){
  for (X2 in N){
    for (X3 in G){
      for (X4 in c1){
        for (X5 in c2){
          for (X6 in v1){
            for (X7 in v2){
              for (X8 in w1){
                for (X9 in w2){
                  for (X10 in w3){
                    for (X11 in w4){
                      for (X12 in m){
                        for (X13 in interactionPartners){
                          for (X14 in mutRateAlpha){
                            for (X15 in mutRateBeta){
                              for (X16 in mutRateStrategySender){
                                for (X17 in mutRateStrategyReceiver){
                                  for (X18 in mutStepAlpha){
                                    for (X19 in mutStepBeta){
                                      for (X20 in alphaBetaMutation){
                                        for (X21 in initializationType){
                                          for (X22 in cauchyDist){
                                            for (X23 in initStrategySender){
                                              for (X24 in initStrategyReceiver){
                                                for (X25 in initAlpha){
                                                  for (X26 in initBeta){
                                                    for (X27 in replicates){
                                                      for (X28 in coutReport){
                                                        for (X29 in reportFreq){
                                                          
                                                          if (seed == 0){
                                                            X1 <- sample(1:99999999, 1)
                                                          }
                                                          
                                                          Xparams <- c(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,
                                                                       X25,X26,X27,X28,X29,paste0("\"",fileName,"_",iterator,"\""),paste0("\"",jobFolder,"/",date,"_",jobFolderNew,"\""))
                                                          
                                                          writeTo <- paste0(folderPath,"/",fileName,"_",iterator)
                                                          
                                                          paste0("\"",date,"_",jobFolder,"/",jobFolderNew,"\"")
                                                          
                                                          txt <- "{\n\t"
                                                          i <- 1
                                                          for (i in 1:(length(paramsNames)-1)){
                                                            txt <- paste(txt,"\"",paramsNames[i],"\": ",Xparams[i],",\n\t",sep="")
                                                          }
                                                          txt <- paste(txt,"\"",paramsNames[i+1],"\": ",Xparams[i+1],"\n",
                                                                       "}",sep="")
                                                          
                                                          fileConn<-file(paste0(writeTo,"_Parameters.json"))
                                                          writeLines(txt,fileConn)
                                                          close(fileConn)
                                                          
                                                          fileNameList<-append(fileNameList,paste0(fileName,"_",iterator))
                                                          
                                                          iterator <- iterator + 1
                                                        }}}}}}}}}}}}}}}}}}}}}}}}}}}}}

##------------Creating .bat and .sh files
#bat file for windows
if (writeBatOrSh == "both" || writeBatOrSh == "Both" || writeBatOrSh == "bat" ||  writeBatOrSh == "Bat"){
  file.copy(paste(exeFolder,"/",windowsExeName,sep=""),folderPath)
  txt<-""
  i<-1
  for (i in 1:(length(fileNameList))){
    txt <- paste(txt,"call ",windowsExeName," ",fileNameList[i],"_Parameters.json",sep="")
    if (i < length(fileNameList)){
      txt <- paste(txt,"\n",sep="")
    }
  }
  
  fileConn<-file(paste(folderPath,"/signalling_model",".bat",sep=""))
  writeLines(txt,fileConn)
  close(fileConn)
}

#sh file for linux - to do
# if (writeBatOrSh == "both" || writeBatOrSh == "both" || writeBatOrSh == "sh" || writeBatOrSh == "Sh"){
#   
#   if (json_per_script == 0){
#     txt<-paste("#!/bin/bash\n#SBATCH --job-name=",peregrine_job_name,
#                "\n#SBATCH --time=",peregrine_time,"\n\nmodule load foss/2022a\n",sep="")
#     i<-1
#     for (i in 1:(length(fileNameList))){
#       txt <- paste(txt,"./",sh_BinaryName," ",fileNameList[i],"_Parameters.json",sep="")
#       if (i < length(fileNameList)){
#         txt <- paste(txt,"\n",sep="")
#       }
#     }
#     
#     fileConn<-file(paste(folderPath,"SLmodel",".sh",sep=""))
#     writeLines(txt,fileConn)
#     close(fileConn)
#     
#   } else {
#     
#     m <- 0
#     while (m <= floor(length(fileNameList)/json_per_script)){ #number of scripts to write
#       
#       txt<-paste("#!/bin/bash\n#SBATCH --job-name=",peregrine_job_name,
#                  "\n#SBATCH --time=",peregrine_time,"\n\nmodule load foss/2022a\n",sep="")
#       
#       i<- m * json_per_script
#       if (i == 0){
#         i <- 1
#       }
#       while (i <= (length(fileNameList)) && (i - (m * json_per_script)) < 10 || i == length(fileNameList)){
#         txt <- paste(txt,"./",sh_BinaryName," ",fileNameList[i],"_Parameters.json",sep="")
#         if (i < length(fileNameList)){
#           txt <- paste(txt,"\n",sep="")
#         }
#         i <- i + 1
#       }
#       
#       gsub("[\r]", "", txt)
#       
#       fileConn<-file(paste(folderPath,"SLmodel_",m,".sh",sep=""))
#       writeLines(txt,fileConn)
#       close(fileConn)
#       
#       m <- m + 1
#     } 
#   }
# }
