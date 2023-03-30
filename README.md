# SignallingModel
Files in this program:

CreateBat_SignallingModel_v1.R 
    This is an R script in takes user-entered parameter values and creates a folder with Parameter.json files as well as a bash script to run the jobs in sequence
    The user can input a vector of values and parameter json files are written with all parameter combinations: this allows easy testing of questions
    For instance, one could enter N <- (100,1000,1000) and mutRateAlpha <- (0.01,0.001) and 6 files will be created, with each combination of N and mutRateAlpha
    
DataAnalysis.R
    This is an R script which creates a graph of the trajectory of a single replicate, for now. It shows strategy frequencies and alpha and beta through time for a selected simulation
    
rScriptTest_Parameters.json
    This is an example .json file which shows the format that .json files need to be to be read by the cpp program
    
The program now needs to be run in a bash or shell script with the format "<executable name> <parameter file name_Parameters.json>"
