# gSMS-validation
Supporting materials for field validation analysis of the gamma-ray soil moisture sensor (gSMS).

# Description:  

This is a repository containing the data and code needed to reproduce the analysis in the paper titled “Field Testing of gamma-ray spectroscopy method for soil water content estimation in an agricultural field” by Becker et al. (in submission). The analysis was performed in R version 4.3.1.  

# Contents: 

### CsvFiles
A folder containing all the data required for the main analysis in "gSMS_Results.Rmd” code.  
### FieldSWCdata 
A folder containing excel files of raw gravimetric water content and bulk density values for each of the gravimetric sampling campaign days. 
### renv
Folder containing the R environment for use with this project. 
### gSMS_Results.Rmd 
Code used to perform the main statistical analysis of the paper. Creates the output folder, “Output” and the markdown html file, "gSMS_Results.html".  
### DepthWeightsFunction_SensAnalysis.R 
Code that can be used to calculate the weights that should be used in depth weighting gravimetric samples based on sampling intervals, bulk density, and water content of interest. Creates the output folder “WeightingOutput” and Figure 2 in the paper.
### CalcVerticalWeightedAverage.R 
Code that applies the weights found by “DepthWeightsFunction.R” to calculate weighted average gravimetric water content values.  
### CalcArithmeticAverage.R
Code that calculated arithmetic averaged as opposed to depth weighted average for comparison purposes.  
### EffectOfNumberSamplesInFootprint.R
Used to create Figure 9 in the paper. Creates the output folder, "NumberSampLocOutput". 
### NumCalNeeded.R 
Used to create Figure 10 in the paper.
