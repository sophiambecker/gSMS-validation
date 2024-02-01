# Sophia Becker
# 8/29/23

# A script that calculates the weighted average of each profile and then the average of 
# all the profiles for a given calibration to get the "ground truth" SWC estimate

# load libraries
library(readxl)
library(dplyr)
library(tidyr)

# Set up script

stamp <- Sys.Date()
mainDir <- getwd()
outDir <- file.path(mainDir, "CsvFiles")
if (!exists("outDir")){
  dir.create(outDir)
}

# load weights

weights <- read.csv(paste0(mainDir, "\\weightingOutput\\Weights.csv"))

# Find datasheet paths 

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

# Function to simplify the data sheets that have been read
# returns a data frame

simplify <- function(CalibrationDataSheet){
  simplified <- CalibrationDataSheet[17:150,c(2:4, 12,14)]
  simplified <- apply(simplified, FUN = unlist, MARGIN = 2 )
  simplified <- apply(simplified, FUN = as.numeric, MARGIN = c(1,2))
  simplified <- data.frame(simplified)
  excelDate <- as.numeric(CalibrationDataSheet[1,2][[1]])
  date <- as.Date(excelDate, origin = "1904-01-01")
  simplified <- data.frame(simplified, date)
  colnames(simplified)<- c(CalibrationDataSheet[16,c(2:4, 12,14)], "Date")
  simplified <- simplified %>% filter(`Distance (m)` %in% c(0, 2, 5, 12))
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

# put all the simplified data frames in a list
df_w_list <- list(df1_w, df2_w, df3_w, df4_w, df5_w, df6_w, df7_w, df8_w, df9_w, 
                  df10_w, df11_w, df12_w, df13_w, df14_w, df15_w, df16_w, df17_w, df18_w, 
                  df19_w, df20_w, df21_w, df22_w, df23_w, df24_w, df25_w, df26_w, df27_w)

# function that calculates the depth weighted average for each data frame
# inputs: simplified = simplified data frame, wts = data frame with the weight assigned to each sampling interval
# output: data frame with the day, depth weighted average, and standard deviation and standard error of the depth weighted average

depth_wt_avg <- function(simplified, wts){
  day <- as.character(df_w_list[[i]][1,6])
  weighted2.5 <- simplified %>% filter(`Mean Depth (cm)`== 2.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[1, 4]/100
  weighted7.5 <- simplified %>% filter(`Mean Depth (cm)`== 7.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[2,4]/100
  weighted12.5 <- simplified %>% filter(`Mean Depth (cm)`== 12.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[3,4]/100
  weighted17.5 <- simplified %>% filter(`Mean Depth (cm)`== 17.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[4,4]/100
  weighted22.5 <- simplified %>% filter(`Mean Depth (cm)`== 22.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[5,4]/100
  weighted27.5 <- simplified %>% filter(`Mean Depth (cm)`== 27.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[6,4]/100
  weighted32.5 <- simplified %>% filter(`Mean Depth (cm)`== 32.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[7,4]/100
  
  combineDepthWeighted <- data.frame(weighted2.5, weighted7.5, weighted12.5, weighted17.5,
                                     weighted22.5, weighted27.5, weighted32.5)
  colnames(combineDepthWeighted)<- c("Depth2.5", "Depth7.5", "Depth12.5", "Depth17.5", "Depth22.5", "Depth27.5", "Depth32.5")
  
  # Compute weighted average for each row (sample location):
  depth_weighted_avg <- apply(combineDepthWeighted, FUN = sum, MARGIN = 1)
  
  # average across sample locations
  avg <- mean(depth_weighted_avg, na.rm = TRUE)
  
  # calculate standard deviation and standard error
  sdev2 <- sd(depth_weighted_avg, na.rm = TRUE)
  serr2 <- sdev2/sqrt(19)# 19 profiles collected in the footprint
  return(data.frame(day, avg, sdev2, serr2))
}

# loop through list and combine in a data frame that includes the sampling date. 

Avg_SWC <- data.frame(matrix(nrow = length(df_w_list), ncol = 4))

colnames(Avg_SWC) <- c("Date", "V_Wt_Avg (g/g)", "SD (g/g)", "SE (g/g)") 

for (i in 1:length(df_w_list)){
  result <- depth_wt_avg(df_w_list[[i]], weights)
  Avg_SWC[i,]<- result
}

# export SWC values
write.csv(Avg_SWC, file = paste0(outDir, "\\V_Wt_AvgPore", stamp,".csv"))
