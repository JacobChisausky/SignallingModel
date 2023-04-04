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

sh_BinaryName      <- "Program"  #compiled program name

hpc_time <- "0-24:00:00"
hpc_mem <- 8000

auto_time <- TRUE #If true, ignore hpc_time and automatically calculate expected time

jobType       <- "array" #accepts "bat", "sh", "both", or "array"

jobFolder <- "testParams"  #The folder path when you want the JOBS to be written to. To be created in this directiory

programPath <- "~/SignallingModel/src/Program"  #Where is the compiled program located? Include program name

#jobFolderNew <- "rScriptTest2"    #This will be appended to the end of jobFolder to write these jobs into a new folder. Leave blank to write jobs into base folder

fileName <- "testParams"   #The name of files CPP will write

json_per_script <- 3 #Leave 0 to add all json files to one script. This allows jobs to be run in parallel - many json per script = sequential, few per script = parallel if you have multiple CPUs
#json_per_script is ignored if jobType = "array" or "bat"

seed <- 0  #If 0, use a rng to set seed. It will be recorded in param file in case you want to rerun a simulation. If you enter a nonzero value here, it will be used as seed
N   <- c(10000)
G   <- c(10000)
c1  <- c(0.25)
c2  <- c(0.75,0.45)
v1  <- c(1)
v2  <- c(1)
w1  <- c(1)
w2  <- c(0)
w3  <- c(0)
w4  <- c(1)
m   <- c(0.25)
interactionPartners <- c(10,5)
mutRateAlpha        <- c(0.01)
mutRateBeta         <- c(0.01)
mutRateStrategySender   <- c(0.01)
mutRateStrategyReceiver <- c(0.01)
mutStepAlpha <- c(0.1,0.5)
mutStepBeta  <- c(0.4)
alphaBetaMutation  <- c("\"always\"")  # "always", "random", or "strict"
initializationType <- c("\"random\"")  # "random" or "parameter"
cauchyDist  <- c("true")
initStrategySender   <- c(1)
initStrategyReceiver <- c(1)
initAlpha      <- c(1)
initBeta       <- c(0)
replicates     <- c(1)
coutReport     <- c(0)
reportFreq     <- c(G/50)

sameMutRatesAB    <- 1  #If 1, then the mutRateAlpha will replace the value for mutRateBeta so that they are the same. If 0, they are independent
sameMutRatesStrat <- 1  #If 1, then mutRateStrategySender will replace mutRateStrategyReceiver
sameMutSteps      <- 1  #If 1, then mutStepAlpha will replace the value for mutStepBeta on each run

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


#Move compiled program into new folder
#file.copy(programPath,jobFolder)

#Time allocation estimate
if (auto_time == TRUE){
  minuteEst <- (0.000000005) * max(G) * max(N) * max(interactionPartners) * (1.5 / (max(interactionPartners)+5)) * max(replicates)
  
  #Give twice as much time as expected to be safe
  minuteAlloc <- minuteEst * 2
  dAlloc <- floor(minuteAlloc/1440)
  rem <- minuteAlloc - dAlloc*1440
  hAlloc <- floor(rem/60)
  rem <- rem - hAlloc*60
  if (hAlloc < 10){
    hAlloc <- paste0("0",hAlloc)
  }
  if (rem < 10){
    rem <- paste0("0",rem)
  }
  hpc_time <- paste0(dAlloc,"-",hAlloc,":",rem,":00")
}


#Add time to folder path in same way as cpp - so that alphabetically, the newest folder is always last
t_hr <- format(Sys.time(), "%H")
t_min <- format(Sys.time(), "%M")
t_sec <- format(Sys.time(), "%S")
t_yday <- yday(Sys.Date())-1
date <- paste(t_yday,t_hr,t_min,t_sec,sep="_")

folderPath<-paste0("./",date,"_",jobFolder)
dir.create(folderPath)
fileNameList<-c()
iterator<-1;  #add this to each job title in case two jobs get submitted with same name at same second

dataFileName <- fileName
dataFileFolder <- folderPath

#Move compiled program into new folder
file.copy(programPath,folderPath)

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
                            if (sameMutRatesAB == 1){
                              mutRateBeta <- X14
                            }
                            for (X15 in mutRateBeta){
                              for (X16 in mutRateStrategySender){
                                if (sameMutRatesStrat == 1){
                                  mutRateStrategyReceiver <- X16
                                }
                                for (X17 in mutRateStrategyReceiver){
                                  for (X18 in mutStepAlpha){
                                    if (sameMutSteps == 1){
                                      mutStepBeta <- X18
                                    }
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
                                                                       X25,X26,X27,X28,X29,paste0("\"",fileName,"_",iterator,"\""),paste0("\"",".","\""))
                                                          
                                                          writeTo <- paste0(folderPath,"/",fileName,"_",iterator)
                                                          
                                                          
                                                          
                                                          txt <- "{\n\t"
                                                          i <- 1
                                                          for (i in 1:(length(paramsNames)-1)){
                                                            txt <- paste(txt,"\"",paramsNames[i],"\": ",Xparams[i],",\n\t",sep="")
                                                          }
                                                          txt <- paste(txt,"\"",paramsNames[i+1],"\": ",Xparams[i+1],"\n",
                                                                       "}",sep="")
                                                          
                                                          fileConn<-file(paste0(writeTo,"_params.json"))
                                                          writeLines(txt,fileConn)
                                                          close(fileConn)
                                                          
                                                          fileNameList<-append(fileNameList,paste0(fileName,"_",iterator))
                                                          
                                                          iterator <- iterator + 1
                                                        }}}}}}}}}}}}}}}}}}}}}}}}}}}}}

##------------Creating .bat and .sh files
#bat file for windows
if (jobType == "both" || jobType == "bat"){
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
if (jobType == "both" || jobType == "sh"){
  
  if (json_per_script == 0){
    txt<-paste("#!/bin/bash\n#SBATCH --job-name=",fileName,
               "\n#SBATCH --mem=",hpc_mem,
               "\n#SBATCH --time=",hpc_time,"\n\nmodule load foss/2022a\n",sep="")
    i<-1
    for (i in 1:(length(fileNameList))){
      txt <- paste(txt,"./",sh_BinaryName," ",fileNameList[i],"_Parameters.json",sep="")
      if (i < length(fileNameList)){
        txt <- paste(txt,"\n",sep="")
      }
    }
    
    fileConn<-file(paste(folderPath,"/SignallingModel",".sh",sep=""))
    writeLines(txt,fileConn)
    close(fileConn)
    
  } else {
    
    
    m <- 0
    while (m < floor(length(fileNameList)/json_per_script)){ #number of scripts to write
      
      txt<-paste("#!/bin/bash\n#SBATCH --job-name=",fileName,
                 "\n#SBATCH --mem=",hpc_mem,
                 "\n#SBATCH --time=",hpc_time,"\n\nmodule load foss/2022a\n",sep="")
      
      i<- (m * json_per_script) + 1
      #   if (i == 0){
      #     i <- 1
      #   }
      while (i <= (length(fileNameList)) && (i <= (m+1)*json_per_script)  &&  (i - (m * json_per_script)) < 10 || i == length(fileNameList)){
        txt <- paste(txt,"./",sh_BinaryName," ",fileNameList[i],"_Parameters.json",sep="")
        if (i < length(fileNameList)){
          txt <- paste(txt,"\n",sep="")
        }
        i <- i + 1
      }
      
      gsub("[\r]", "", txt)
      
      fileConn<-file(paste(folderPath,"/SignallingModel_",m,".sh",sep=""))
      writeLines(txt,fileConn)
      close(fileConn)
      
      m <- m + 1
    } 
  }
}

if (jobType == "array"){
  
  txt<-paste0("#!/bin/bash\n#SBATCH --job-name=",fileName,
              "\n#SBATCH --mail-user=jacob.chisausky@evobio.eu\n#SBATCH --mail-type=ALL\n#SBATCH --time=",
              hpc_time,"\n#SBATCH --mem=",hpc_mem,"\n#SBATCH --array=1-",iterator-1,
              "\n\nmodule load foss/2022a\n\necho \"SLURM_JOBID: \" $SLURM_JOBID\necho \"SLURM_ARRAY_TASK_ID: \" $SLURM_ARRAY_TASK_ID\necho \"SLURM_ARRAY_JOB_ID: \" $SLURM_ARRAY_JOB_ID\n\n./",sh_BinaryName," `ls -d *.json | awk NR==$SLURM_ARRAY_TASK_ID`")
  
  fileConn<-file(paste(folderPath,"/SignallingModel",".sh",sep=""))
  writeLines(txt,fileConn)
  close(fileConn)
  
  #  file.copy(paste("./",sh_BinaryName,sep=""),folderPath)
  
}
