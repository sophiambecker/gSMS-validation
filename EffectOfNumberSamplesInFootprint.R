# Sophia Becker
# 7.20.23

#### Load libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# create output directory/file system

stamp <- Sys.Date()
mainDir <- getwd()

outDir <- file.path(mainDir, paste0("NumberSampLocOutput", stamp))
if (!dir.exists(outDir)){
  dir.create(outDir)
}else{
  print("dir exists")
}

#### Read sample data

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
  weighted2.5 <- simplified %>% filter(`Mean Depth (cm)`== 2.5& `Distance (m)` < 50) %>% dplyr::select(`Soil Water (wt. %)`) * wts[1, 4]/100
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
weighted2.5 <- simplified %>% filter(`Mean Depth (cm)`== 2.5) %>% dplyr::select(`Soil Water (wt. %)`) * wts[1, 4]/100
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

# loop through list and combine in a data frame  
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

meanDataSummary <- apply(as.matrix(dataSummary), MARGIN = 1, FUN = mean, na.rm = TRUE)
################################################################################

nsamp <- 4
sampday <- 4
nrep <- 100
nloc <- 19

# select: function that randomly selects combinations of profile locations without duplicates
# first, all combinations of profile locations are generated
# then, the combinations are randomly sampled

# inputs:
# nsamp = number of samples selected
# sampday =sample day
# df = depth-weighted data frame (19 profiles for each day)
# nrep = number sampling repetitions
# nloc = number of soil sample locations (profiles)

select_comb <- function(nsamp, sampday, df, nrep = 10000, nloc = 19){
  all_comb <- combn(nloc, nsamp) # all combinations of profile locations that can be chosen
  
  num_colum <- ncol(all_comb) # number of columns/combinations possible
  elements <- c(1:num_colum)
  
  # Choose row for one sample day
  sampleday <- as.numeric(dataSummary[sampday,]) 
  
  sampleday_mean <- mean(sampleday, na.rm = TRUE) # find "true" theta
  
  # Create an empty matrix to store the results
  results <- matrix(NA, nrow = nrep, ncol = 1)
  
  # Perform the random selection without replacement
  if(nrep > length(elements)){
    selected_col <- sample(elements, size = nrep, replace = TRUE) # allow replacement if there are less combinations than desired samples
  }else{
    selected_col <- sample(elements, size = nrep, replace = FALSE)
  }
  selected_comb <- all_comb[,selected_col]
  
 return(selected_comb)
}


profile_comb <- select_comb(nsamp = 10, sampday = 5, df = dataSummary, nrep = 100, nloc = 19)

# for every combination, find the values and the mean of the values

find_profile_mean <- function(prof_loc, prof_values){
  selected_vals <- prof_values[prof_loc]
  selected_mean <- mean(as.numeric(selected_vals), na.rm = TRUE)
  return(selected_mean)
}

# apply the mean find_profile_mean function across columns 
selected_means <- apply(X = profile_comb, MARGIN = 2, FUN = find_profile_mean, prof_values = dataSummary[1,])
selected_resid <- abs(mean(as.numeric(dataSummary[1,]), na.rm = TRUE)- selected_means)

calc_rmse <- sqrt(mean(selected_resid^2))

# write a for loop that uses the above functions/code to loop through the number of samples and the sample days

# Define variables

nsampdays <- 27 # number of sampling days
numloc <- 19
numreps <- 1000
# create dataframe for results
# Results columns will be: TrueTheta value, number of samples, RMSE

Results <- matrix(ncol = 3)

set.seed(20) # set seed for random sampler

for (i in 1:nsampdays){
  daily_res <- matrix(ncol = 3, nrow = nloc - 5 )
  for (j in 1:(nloc-5)){
    num_samples <- j+2 
    loc_comb <- select_comb(nsamp = num_samples, sampday = i, df = dataSummary, nrep = numreps, nloc = numloc)
    selected_means <- apply(X = loc_comb, MARGIN = 2, FUN = find_profile_mean, prof_values = dataSummary[i,])
    theta_true <- meanDataSummary[i]
    selected_resid <- abs(theta_true-selected_means)
    rmse <- sqrt(mean(selected_resid^2))
    daily_res[j,] <- c(theta_true, num_samples,rmse)
  }
  Results <- rbind(Results, daily_res)
  print(paste0("sampling day", i))
}

Results <- as.data.frame(Results)
colnames(Results) <- c("TrueTheta", "NumSamples", "RMSE")

Results <- na.omit(Results)
Results$TrueTheta <- round(Results$TrueTheta, 4)

###########################################################################
# plot variance vs average water content

# use dataSummary df
# rows are sampling day
# columns are sampling locations

vary <- apply(as.matrix(dataSummary), MARGIN = 1, FUN = var, na.rm = TRUE)
theta_means <- apply(as.matrix(dataSummary), MARGIN = 1, FUN = mean, na.rm = TRUE)

VarPlot <- ggplot()+
  geom_point(aes(x = theta_means, y = vary))+
  labs(x = bquote("Total Water Content " (g ~g^-1)), y = "Variance among 19 profiles")

ggsave(VarPlot, filename = paste0(outDir, "\\VarianceVsSWC_", stamp, ".tiff"), width = 6, height = 6, dpi = 400)

############################################################################
## Interpolate results to regularly spaced grid

library(MBA)
library(fields) # image.plot()
library(spBayes) # iDist()

### Drop in the bucket smoothing

# set up grid every 0.5 units

s.x <- Results[,1]*100
s.y <- Results[,2]
s <- cbind(s.x, s.y) # coordinates
R <- Results[,3] # RMSE data

max.dist <- 0.9 *max(iDist(s))
bins <- 100
nsx <- 40
nsy <- 40
sx2 <- seq(10,30,l = nsx)
sy2 <- seq(3,16,l= nsy)
sGrid2 <-expand.grid(sx2,sy2)

# for each grid point, do a find with radius 2 and assign to the corresponding 0.5 unit grid
# write distance formula function

dst <- function(x1, x2, y1, y2) {
  x <- sqrt((x2-x1)^2+(y2-y1)^2)
  return(x)}

#for loop that finds all distances between one sample point and all geophysical observations (or interpolated grid nodes) 
#function: alldst
#s: "simple" data frame that has the coordinates in UTM for all the data layer values you wish to find
##with mE in the first column and mN in the second column
#datalayer: data frame with all the datalayer values in it - the values of every column in this data frame will be extracted
#a: the row(coordinate pair) in s for which the distances are found
#RETURNS data frame with the distance between one coordinate point in s and all locations in the data layers, along with all the 
##values at the data layer locations

alldst <- function(s,datalayer,a){
  d <- matrix(nrow = nrow(datalayer), ncol = 1)  #empty vector to hold distance values
  names(d)<- c("m")
  for (i in 1:nrow(datalayer)){
    dt <- as.numeric(dst(s[a,1], datalayer[i,1], s[a,2], datalayer[i,2]))
    d[i,] <-dt
  }
  #d<- d[-1]
  d.all <- cbind(d,datalayer)
  return(d.all)
}

#function that returns a row with the distance to the closest data layer point from a sample coordinate pair
##and also all of the data layer values at that closest data layer point
#RETURNS the rows from alldst that are within the search radius
rows <- function(s, datalayer, a, radius){
  hey <- alldst(s, datalayer, a)
  within <- hey %>% filter(d < radius)
  return(within)
}

#RETURNS a data frame where each row corresponds to a coordinate pair in s and the columns are:
## 1) distance between coordinate point in s and closest point in data layer 2-3) X and Y coord of 
###that closest data layer point 4-end) The corresponding values from each data layer

find <- function(s, datalayer, radius){
  
  v <- matrix(nrow = nrow(s), ncol = 1+length(datalayer)) #create empty vector (ncol is length of minrow output)
  for (q in 1:nrow(s)){
    r<- rows(s, datalayer, q, radius) #find rows within radius
    v[q,]<- apply(r, MARGIN = 2, FUN = mean)}  #matrix v contains average of all points within the radius
  f<-as.data.frame(v)
  colnames(f)<- c("d",c(names(datalayer)))
  return(f)
}

# here, s should be the newly created grid
# datalayer should be the Results dataframe
ResultsMod <- data.frame(TrueTheta= Results$TrueTheta*100, NumSamples = Results$NumSamples, RMSE = Results$RMSE)

Gridded <- find(sGrid2, ResultsMod, 3)

# add correct grid coord
Gridded$TrueTheta <- sGrid2$Var1
Gridded$NumSamples <- sGrid2$Var2

#Plot the smoothed values:
surf2 <- list()
surf2$x <- sx2/100
surf2$y <- sy2
surf2$z <- matrix(Gridded$RMSE,nrow = length(sx2), ncol =length(sy2))

image.plot(surf2, xaxs = "r", yaxs = "r", xlab = bquote("Total Water Content " (g ~g^-1)),
           ylab = "Number of Samples",zlim = range(surf2$z), legend.shrink = 0.6, legend.args = list(text = "RMSE ", side = 3, 
                                                                                                    font = 2, line = 1, cex = 1))
points(x = s.x/100, y = s.y, pch = 20, cex = 0.75)
contour(surf2, add = T, labcex = 0.8, nlevels = 6)
abline(h = 14, lty = 2, col = "white", lwd = 2)

# then do IDW interpolation to finer grid #############################

library(spatstat) # owin(), ppp()
library(Metrics) # mse()

# create observation window
obs_window <- owin(xrange = c(min(Gridded$TrueTheta), max(Gridded$TrueTheta)), yrange = c(3, 16))

# create point pattern object
ppp_results <- ppp(Gridded$TrueTheta, Gridded$NumSamples, marks = Gridded$RMSE, window = obs_window)

# create idw object
idw_results <- idw(ppp_results, power = 2, at = "pixels", dimyx = 70)

# visualize idw results
plot(idw_results, main = "power = 2")

gwc <- idw_results$xcol/100 # water content in g/g

# Show as relative error (RMSE/observed value) instead of RMSE #########

# use sweep function from base package to calculate relative error

relErr <- sweep(t(idw_results$v), 2, gwc, `/`)# divide each element in respective columns
                                          #of matrix by corresponding value in the vector, gwc
#Plot the predicted values:
surf <- list()
surf$x <- idw_results$xcol/100 
surf$y <- idw_results$yrow
surf$z <- relErr*100 # relative error as a percentage

#image.plot(surf)

tiff(filename = paste0(outDir, "\\SmoothedSampSize_RelativeError", stamp, ".tiff"), width = 6, height = 6, units = "in", pointsize = 12, res = 400)
image.plot(surf, xlab = bquote("Total Water Content " (g ~g^-1)),
           ylab = "Number of Samples",zlim = range(surf$z), legend.shrink = 0.6, legend.args = list(text = " Relative \n Error (%)  ", side = 3, 
                                                                                                    font = 2, line = 1, cex = 1))
points(x = s.x/100, y = s.y, pch = 20, cex = 0.75)
contour(surf, add = T, labcex = 0.8, nlevels = 12)
dev.off()

# with RMSE
surf <- list()
surf$x <- idw_results$xcol/100 
surf$y <- idw_results$yrow
surf$z <- t(idw_results$v)# rmse

tiff(filename = paste0(outDir, "\\SmoothedSampSize_RMSE", stamp, ".tiff"), width = 6, height = 6, units = "in", pointsize = 12, res = 400)
image.plot(surf, xlab = bquote("Total Water Content " (g ~g^-1)),
           ylab = "Number of Samples",zlim = range(surf$z), legend.shrink = 0.6, legend.args = list(text = "RMSE ", side = 3, 
                                                                                                    font = 2, line = 1, cex = 1))
points(x = s.x/100, y = s.y, pch = 20, cex = 0.75)
contour(surf, add = T, labcex = 0.8, nlevels = 10)
dev.off()

# determine best power to use for idw
powers <- seq(0.01, 5, 0.01)
mse_result <- NULL
for(power in powers){
  CV_idw <- idw(ppp_results, power=power, at="points")
  mse_result <- c(mse_result,
                  Metrics::mse(ppp_results$marks,CV_idw))
  print(power)
}
optimal_power <- powers[which.min(mse_result)]
optimal_power

plot(powers, mse_result)


