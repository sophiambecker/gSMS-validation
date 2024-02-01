# Sophia Becker
# 8/29/23

# A script that calculates the average of each profile and then the average of 
# all the profiles for a given calibration to get the "ground truth" SWC estimate

# load libraries
library(readxl)
library(dplyr)
library(tidyr)

# Set up script

stamp <- Sys.Date()
mainDir <- "C:\\Users\\sbecker14\\Documents\\GRScalibration\\"
outDir <- file.path(mainDir, "RecommendationAnalysisAll27\\Weighting\\WeightedSWCData")
if (!exists("outDir")){
  dir.create(outDir)
}


# Find datasheet paths

folders <- as.list(list.files("C:\\Users\\sbecker14\\Documents\\GRScalibration\\", pattern = "_calibration", all.files = TRUE, full.names = TRUE))

files <- list()

for (f in folders){
  path <- list.files(f, pattern = "Datasheets.xls", full.names = TRUE)
  files<- append(files, path)
}

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


df_w_list <- list(df1_w, df2_w, df3_w, df4_w, df5_w, df6_w, df7_w, df8_w, df9_w, 
                  df10_w, df11_w, df12_w, df13_w, df14_w, df15_w, df16_w, df17_w, df18_w, 
                  df19_w, df20_w, df21_w, df22_w, df23_w, df24_w, df25_w, df26_w, df27_w)

# example with just 1 dataframe

v_avg_df <- df2_w %>% group_by(`Bearing Degrees (0 N)`, `Distance (m)`)%>%
  summarise(v_avg = mean(`Soil Water (wt. %)`, na.rm = TRUE))

overall_avg <- mean(v_avg_df$v_avg, na.rm = TRUE)

# for all the dataframes:

Avg_SWC <- data.frame(matrix(nrow = length(df_w_list), ncol = 4))
colnames(Avg_SWC)<- c("Date", "SWC (g/g)", "SD (g/g)", "SE (g/g)")
for (i in 1:length(df_w_list)){
  day <- as.character(df_w_list[[i]][1,6])
  
  v_avg_df <- df_w_list[[i]] %>% group_by(`Bearing Degrees (0 N)`, `Distance (m)`)%>%
    summarise(v_avg = mean(`Soil Water (wt. %)`, na.rm = TRUE))
  overall_avg <- mean(v_avg_df$v_avg, na.rm = TRUE)
  sdev <- sd(v_avg_df$v_avg)
  serr <- sdev/sqrt(19)
  Avg_SWC[i,]<- c(day, overall_avg/100, sdev, serr) # put in g/g
}

# export SWC values
write.csv(Avg_SWC, file = paste0(outDir, "\\ArithmeticAvg_", stamp,".csv"))

