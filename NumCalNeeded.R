# Sophia Becker
# 09.05.23

# number of calibrations to attain certain % RMSE

# load libraries

#### Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(egg)
library(scales)
library(corrplot)
library(interactions) # test for interactions between variables
library(jtools) # for summ()
library(caret) # for LOOCV
library(rtop)

# create output directory/file system

stamp <- Sys.Date()
mainDir <- getwd()
outDir <- file.path(mainDir, paste0("NumberCalibrationsOutput_", stamp))
if (!dir.exists(outDir)){
  dir.create(outDir)
}else{
  print("dir exists")
}

#############################LOAD DATA#########################################
# Load summary time series data

timeseries <- read.csv(paste0(mainDir,"\\CsvFiles\\CombinedTimeSeries.csv"))

timeseries$Date<- as.Date(timeseries$Date)

colnames(timeseries)[11]<- "Precip.mm" #rename

# calibration data

veg <- read.csv(paste0(mainDir, "\\CsvFiles\\CalibrationDataVeg.csv"))

k40 <- read.csv(paste0(mainDir, "\\CsvFiles\\CalibrationDataK40.csv"))

grav_swc <- read.csv(paste0(mainDir, "\\CsvFiles\\V_Wt_AvgPore.csv"))

SOC_Latt <- read.csv(paste0(mainDir, "\\CsvFiles\\SOCandLatticeValues.csv"))

# Gravimetric Sample dates in DOY
SampleDOY21 <- c(217,
                 231,
                 246,
                 260,
                 288,
                 302,
                 316,
                 337)
SampleDOY22 <- c(84,
                 105,
                 138,
                 147,
                 215,
                 229,
                 243,
                 260, 
                 278, 
                 295)
SampleDOY23 <- c(135, 
                  159, 
                  172,
                  191, 
                  205,
                  221,
                  240,
                  264,
                  296)

SampleDates21 <- as.Date(SampleDOY21-1, origin = "2021-01-01")
SampleDates22 <- as.Date(SampleDOY22-1, origin = "2022-01-01")
SampleDates23 <- as.Date(SampleDOY23-1, origin = "2023-01-01")

Sampledf <- data.frame(grav_swc, veg, k40)

Sampledf$Date <- as.Date(grav_swc$Date)

# Calculate Total water and add to  Sampledf

# Let Wtot be the total water

Wsoc <- SOC_Latt[2,2] # soil organic water content (g/g) 

Wl <- SOC_Latt[1,2] # lattice water content (g/g)

SampWtot <- Sampledf$V_Wt_Avg..g.g. + Wl + Wsoc # add lattice and SOC water

# add Wtot to Sample dataframe

Sampledf$DepthWtWtot <- SampWtot

#### Read indiviual profile  data

# Find datasheet paths

folders <- as.list(list.files("C:\\Users\\sbecker14\\Documents\\GRScalibration\\", pattern = "_calibration", all.files = TRUE, full.names = TRUE))


files <- as.list(list.files(paste0(mainDir,"\\FieldSWCdata"),
                            pattern = "Datasheets.xls", 
                            all.files = TRUE, full.names = TRUE))

# Load data sheets

# Function to read correct data sheets

read_data <- function(path){
  data <- read_excel(path, sheet = 1)
  return(data)
}

df1 <- read_data(files[[1]])
df2 <- read_data(files[[2]])
df3 <- read_data(files[[3]])
df4 <- read_data(files[[4]])
df5 <- read_data(files[[5]])
df6 <- read_data(files[[6]])
df7 <- read_data(files[[7]])
df8 <- read_data(files[[8]])
df9 <- read_data(files[[9]])
df10 <- read_data(files[[10]])
df11 <- read_data(files[[11]])
df12 <- read_data(files[[12]])
df13 <- read_data(files[[13]])
df14 <- read_data(files[[14]])
df15 <- read_data(files[[15]])
df16 <- read_data(files[[16]])
df17 <- read_data(files[[17]])
df18 <- read_data(files[[18]])
df19 <- read_data(files[[19]])
df20 <- read_data(files[[20]])
df21 <- read_data(files[[21]])
df22 <- read_data(files[[22]])
df23 <- read_data(files[[23]])
df24 <- read_data(files[[24]])
df25 <- read_data(files[[25]])
df26 <- read_data(files[[26]])
df27 <- read_data(files[[27]])

# Simplify data frames
# The df1 has less samples because we ran out of cans and only collected at 4 angles (0, 90, 180, 270) for the 12 m radius
df1_w <- df1[17:136, c(2:4, 12,14)]
df1_w <- apply(df1_w, FUN = unlist, MARGIN = 2)
df1_w <- apply(df1_w, FUN = as.numeric, MARGIN = c(1,2))
df1_w <- drop_na(data.frame(df1_w))
excelDate <- as.numeric(df1[1,2][[1]])
date <- as.Date(excelDate, origin = "1904-01-01")
df1_w <- data.frame(df1_w, date)
colnames(df1_w)<- c(df1[16,c(2:4, 12,14)], "Date")

simplify <- function(CalibrationDataSheet){
  simplified <- CalibrationDataSheet[17:150,c(2:4, 12,14)]
  simplified <- apply(simplified, FUN = unlist, MARGIN = 2 )
  simplified <- apply(simplified, FUN = as.numeric, MARGIN = c(1,2))
  simplified <- data.frame(simplified)
  #simplified <- drop_na(simplified)
  excelDate <- as.numeric(CalibrationDataSheet[1,2][[1]])
  date <- as.Date(excelDate, origin = "1904-01-01")
  simplified <- data.frame(simplified, date)
  colnames(simplified)<- c(CalibrationDataSheet[16,c(2:4, 12,14)], "Date")
  return(simplified)
}

df2_w <- simplify(df2)
df3_w <- simplify(df3)
df4_w <- simplify(df4)
df5_w <- simplify(df5)
df6_w <- simplify(df6)
df7_w <- simplify(df7)
df8_w <- simplify(df8)
df9_w <- simplify(df9)
df10_w <- simplify(df10)
df11_w <- simplify(df11)
df12_w <- simplify(df12)
df13_w <- simplify(df13)
df14_w <- simplify(df14)
df15_w <- simplify(df15)
df16_w <- simplify(df16)
df17_w <- simplify(df17)
df18_w <- simplify(df18)
df19_w <- simplify(df19)
df20_w <- simplify(df20)
df21_w <- simplify(df21)
df22_w <- simplify(df22)
df23_w <- simplify(df23)
df24_w <- simplify(df24)
df25_w <- simplify(df25)
df26_w <- simplify(df26)
df27_w <- simplify(df27)

#### Calculate Depth-weighted Average using weights calculated from FindVerticalWeights.R script

# load weights

weights <- read.csv(paste0(mainDir, "\\weightingOutput\\Weights.csv"))

# Function depth_wt_avg(simplified, wts)
# Function to calculate depth-weighted average for all given sample location
# simplified: a simplified data frame 
# wts: the weights for each depth, a dataframe with rows for each depth and 5 columns (index, upper depth of interval in cm, lower depth of interval in cm, weight)

depth_wt_avg <- function(simplified, wts){
  weighted2.5 <- simplified %>% filter(`Mean Depth (cm)`== 2.5 & `Distance (m)` < 50) %>% dplyr::select(`Soil Water (wt. %)`) * wts[1, 4]/100
  weighted7.5 <- simplified %>% filter(`Mean Depth (cm)`== 7.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[2,4]/100
  weighted12.5 <- simplified %>% filter(`Mean Depth (cm)`== 12.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[3,4]/100
  weighted17.5 <- simplified %>% filter(`Mean Depth (cm)`== 17.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[4,4]/100
  weighted22.5 <- simplified %>% filter(`Mean Depth (cm)`== 22.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[5,4]/100
  weighted27.5 <- simplified %>% filter(`Mean Depth (cm)`== 27.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[6,4]/100
  weighted32.5 <- simplified %>% filter(`Mean Depth (cm)`== 32.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[7,4]/100
  #weighted50 <- simplified %>% filter(`Mean Depth (cm)`== 50) %>% dplyr::select(`Soil Water (wt. %)`) * wts[8,4]/100
  
  combineDepthWeighted <- data.frame(weighted2.5, weighted7.5, weighted12.5, weighted17.5,
                                     weighted22.5, weighted27.5, weighted32.5)
  colnames(combineDepthWeighted)<- c("Depth2.5", "Depth7.5", "Depth12.5", "Depth17.5", "Depth22.5", "Depth27.5", "Depth32.5")
  
  # Compute weighted average for each row (sample location):
  depth_weighted_avg <- apply(combineDepthWeighted, FUN = sum, MARGIN = 1)

  return(depth_weighted_avg)
}

simplified<- df2_w
wts <- weights
weighted2.5 <- simplified %>% filter(`Mean Depth (cm)`== 2.5 & `Distance (m)` <50) %>% dplyr::select(`Soil Water (wt. %)`) * wts[1, 4]/100
weighted7.5 <- simplified %>% filter(`Mean Depth (cm)`== 7.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[2,4]/100
weighted12.5 <- simplified %>% filter(`Mean Depth (cm)`== 12.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[3,4]/100
weighted17.5 <- simplified %>% filter(`Mean Depth (cm)`== 17.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[4,4]/100
weighted22.5 <- simplified %>% filter(`Mean Depth (cm)`== 22.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[5,4]/100
weighted27.5 <- simplified %>% filter(`Mean Depth (cm)`== 27.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[6,4]/100
weighted32.5 <- simplified %>% filter(`Mean Depth (cm)`== 32.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[7,4]/100
#weighted50 <- simplified %>% filter(`Mean Depth (cm)`== 50) %>% dplyr::select(`Soil Water (wt. %)`) * wts[8,4]/100

combineDepthWeighted <- data.frame(weighted2.5, weighted7.5, weighted12.5, weighted17.5,
                                   weighted22.5, weighted27.5, weighted32.5)
colnames(combineDepthWeighted)<- c("Depth2.5", "Depth7.5", "Depth12.5", "Depth17.5", "Depth22.5", "Depth27.5", "Depth32.5")

# make list of simplified data frames

df_list <- list(df1_w, df2_w, df3_w, df4_w, df5_w, df6_w, df7_w, df8_w, df9_w, df10_w, df11_w, df12_w, df13_w, df14_w, df15_w, df16_w, df17_w, df18_w, 
                df19_w, df20_w, df21_w, df22_w, df23_w, df24_w, df25_w, df26_w, df27_w)

# loop through list and combine in a data frame of all sample days with 19 individual profiles 
nsamp <- 19

dataSummary <- data.frame(matrix(nrow = length(df_list), ncol = nsamp))

for (i in 1:length(df_list)){
  result <- depth_wt_avg(df_list[[i]], wts = weights)
  if (length(result)< nsamp){
    diff <- nsamp - length(result)
    adddiff <- rep(NA, diff)
    result <- c(result, adddiff)
  }
  dataSummary[i,]<- result
}

# in the data frame, dataSummary, rows are sample dates and columns are depth-weighted values for sample profile locations

################################################################################

# randomly select the calibration days
# selectcal: function that randomly selects combinations of calibration days without duplicates or replacement
# first, all combinations of profile locations are generated
# then, the combinations are randomly sampled

# inputs:
# nsamp = number of calibrations selected
# nrep = number sampling repetitions
# npossible = number of calibrations possible
# cal_days = vector of calibration IDs

cal_days <- c(1:27)
nsamp <- 17
nrep <- 100
npossible <- 27

select_days <- function(nsamp, npossible, nrep, cal_days){
  all_comb <- combn(cal_days, nsamp) # all combinations of sampling days that can be chosen
  num_colum <- ncol(all_comb)
  
  elements <- c(1:num_colum)
  
  # Perform the random selection without replacement
  if(nrep > length(elements)){
    selected_col <- sample(elements, size = nrep, replace = TRUE) # allow replacement if there are less combinations than desired samples
  }else{
    selected_col <- sample(elements, size = nrep, replace = FALSE)
  }
  selected_comb <- all_comb[,selected_col]
}

selected_comb <- select_days(nsamp, npossible, nrep, cal_days)

# for each set of calibration days, select 10 sample locations

select_and_avg <- function(df){
  selectedprof <- sample(c(1:ncol(df)), size = 10, replace = FALSE)
  profiles <- df[,selectedprof]
  CalDayAvgs <- apply(profiles, MARGIN = 1, FUN = mean, na.rm = TRUE)
  CalDayAvgsTot <- CalDayAvgs+Wl+Wsoc
  return(CalDayAvgsTot)
}


# Parameters
mu.g <- 0.05257 # mass attentuation coefficient of soil (silica) cm^2/g
mu.w <- 0.05835 # mass attentuation coefficient of water cm^2/g

bweslope <- -0.012 # Baldoncini et al., 2019

minIo <- max(timeseries$K40)
maxIo <- 1.5*minIo

# solve for Io using the two bare soil gravimetric samples

SampledfBare <- Sampledf[17:18,]
Io <- (SampledfBare$DepthWtWtot*(mu.w/mu.g)+1)*SampledfBare$K

avgIo <- mean(Io)

##################################################################################################################
#MAKE A FUNCTION THAT CAN BE REPEATED FOR EVERY SELECTED COMBINATION OF A SPECIFIED NUMBER OF CALIBRATIONS CHOSEN

# inputs:
# possible_comb = a matrix where each column is a possible combination of sample days from the function select_days()
# comb_index = the index indicating which column to use from the matrix, possible_comb
# SampleDataFrame = Sampledf, a dataframe containing K-40 and "true" total water content (all 19 profiles)
# SWCbyDayAndPos = dataSummary, a matrix where rows are sample dates and columns are depth-weighted values for sample profile locations
 
# output: the RMSE from fitting the calibration equation for one random combination of sample days

RmseFor1Com <- function(possible_comb, comb_index, SampleDataFrame, SWCbyDayAndPos){
  CalDaySWC <- select_and_avg(dataSummary[possible_comb[,comb_index],])
  
  filtSampledf <- SampleDataFrame[possible_comb[,comb_index],]
  filtSampledf$SWC <- CalDaySWC
  
  # simplified data frame for fitting model parameters
  data <- data.frame(It = filtSampledf$K, BWE = filtSampledf$TotBWE, y = filtSampledf$SWC)
  
  # x1 = It
  # x2 = BWE
  # pars  = c(Io, d)
  
  avg.d <- 0.5
  min.d <- 0
  max.d <- 1
  
  CalEq_B <- function(x1, x2, pars)
    (((((pars[1] *(bweslope*x2+1))/x1)-1)*(mu.g/mu.w)*pars[2]))
  
  # write objective function, sum of absolute value of residuals
  OFUN <- function(pars, x1, x2, yobs){
    yvals = CalEq_B(x1, x2, pars)
    sum(abs(yvals-yobs))}
  
  num.obs <- nrow(SampleDataFrame)-1
  num.pred <- 2+3
  
  sceuares <- sceua(OFUN, pars = c(avgIo, avg.d), lower = c(minIo, min.d), 
                    upper = c(maxIo, max.d),
                    x1 = data$It, x2 = data$BWE, yobs = data$y)
  
  pred1 <- CalEq_B(x1 = SampleDataFrame$K, x2 = SampleDataFrame$TotBWE, pars = sceuares$par)
  resid <- pred1 - SampleDataFrame$DepthWtWtot
  sqerr <- resid^2
  rmse <- sqrt(mean(sqerr))
  
  results <- data.frame(RMSE = rmse, Io = sceuares$par[1], d = sceuares$par[2], BWEslope = bweslope)
  
  # calculate summary statistics
  SSE <- sum(sqerr)
  SStot <- sum((data$y - mean(data$y))^2)
  rsq <- 1-SSE/SStot
  adj.rsq <- 1- ((1-rsq)*(num.obs-1)/(num.obs-num.pred-1))
  sum.stats <- data.frame(RMSE = rmse, Rsq = rsq, AdjRsq = adj.rsq, 
                           Io = results$Io, a = results$d)
  
  return(sum.stats)
}

# inputs:
# NumDays = sample size under consideration
# MaxDays = Maximum sample size
# nreps = numer of random simulations to perform
# cal_days = a vector containing integers representing all the sample days (here 1 to 27)
# aSampleDataFrame = Sampledf, a dataframe containing K-40 and "true" total water content (all 19 profiles)
# aSWCbyDayAndPos = dataSummary, a matrix where rows are sample dates and columns are depth-weighted values for sample profile locations

# output: a dataframe with one row containing the RMSe, R-squared, and parameter values for the sample size

AvgRMSEforNumDays <- function(NumDays, MaxDays, nreps, cal_days, aSampleDataFrame, aSWCbyDayAndPos){
  
  selected_comb <- select_days(nsamp = NumDays, npossible = MaxDays, nreps, cal_days)
  
  # For loop that Calculates RmseFor1Com for every possible comb_index
  
  RMSEres <- data.frame(matrix(ncol = 5, nrow = ncol(selected_comb)))
  colnames(RMSEres)<- c("RMSE", "Rsq", "AdjRsq", "Io", "a")
  
  for (i in 1:ncol(selected_comb)){
    rmse_res <- RmseFor1Com (possible_comb = selected_comb, comb_index = i, SampleDataFrame = aSampleDataFrame, SWCbyDayAndPos = aSWCbyDayAndPos)
    RMSEres[i,]<- rmse_res
    print(c("REP", i))
  }
  
  AvgRMSEres <- apply(RMSEres, MARGIN = 2, FUN = mean)
  print(c("DAYS", NumDays))
  return(AvgRMSEres)
}

# try with sample size 7:
AvgRMSEforNumDays(NumDays = 7, MaxDays = 27, nreps = 3, cal_days = c(1:27), aSampleDataFrame = Sampledf, aSWCbyDayAndPos = dataSummary)

# write a for loop to calculate the RMSE for calibration day sample size 3 to 27

samplesize <- c(3:27)

FinalRes <- data.frame(matrix(ncol = 6, nrow = length(samplesize)))
colnames(FinalRes) <- c("n", "RMSE", "Rsq", "AdjRsq", "Io", "a")

set.seed(5)

for (q in 1:length(samplesize)){
  sampRMSE <- AvgRMSEforNumDays(NumDays = samplesize[q], MaxDays = 27, nreps = 1000, cal_days = c(1:27), aSampleDataFrame = Sampledf, aSWCbyDayAndPos = dataSummary)
  n <- samplesize[q]
  FinalRes[q,] <- c(n, sampRMSE)
  print(n)
}

write.csv(FinalRes, file = paste0(outDir, "\\BootstrapResults", stamp, ".csv"))

ResPlot <- ggplot()+
  geom_point(data = FinalRes, aes(x = n, y = RMSE))+
  labs(y = bquote("RMSE " (g ~g^-1)), x = "Number of Calibrations")+
  scale_x_continuous(breaks = seq(3, 28,2))+
  scale_y_continuous(breaks = seq(0.02, 0.042, 0.002), limits = c(0.0295, 0.0392))+
  theme(axis.text = element_text(size = 14, color = "black"), axis.title = element_text(size = 16))

ggsave(ResPlot, file = paste0(outDir, "\\RMSEvsSampleSize_", stamp, ".tiff"),width = 6, height = 6, units = "in", dpi = 300 )


