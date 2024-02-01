# Sophia Becker
# 8/10/23
# Depth weighting function
# Based upon Baldoncini, M., Alb?ri, M., Chiarelli, E., Giulia Cristina Raptisa, K., Strati, V., & Mantovani, F. (2018). Investigating the potentialities of Monte Carlo simulation for assessing soil water content via proximal gamma-ray spectroscopy. Journal of Environmental Radioactivity, 192, 105-116.
# Also based upon Franz, T. E., Zreda, M., Ferre, T. P. A., Rosolem, R., Zweck, C., Stillman, S., Zeng, X., & Shuttleworth, W. J. (2012). Measurement depth of the cosmic ray soil moisture probe affected by hydrogen from various sources: MEASUREMENT DEPTH OF THE COSMIC-RAY SOIL MOISTURE PROBE. Water Resources Research, 48(8). https://doi.org/10.1029/2012WR011871

# https://www.r-bloggers.com/2019/07/integration-in-r/

stamp <- Sys.Date()
mainDir <- getwd()
mainDir
outDir <- file.path(mainDir, "WeightingOutput")
if (!dir.exists(outDir)){
  dir.create(outDir)
}else{
  print("dir exists")
}

library(ggplot2)
library(mosaicCalc)
library(egg)

#load SWC data
SOC <- 0.004775333 # average SOC water content (non-weighted)(g/g)
Lattice <- 0.050666667 # average lattice water content (non-weighted)(g/g)Pore <- read.csv(paste0(mainDir, "\\CsvFiles\\ArithmeticAvg.csv"))
AvgPore <- mean(Pore$SWC..g.g.)
AvgTotGrav <- AvgPore+SOC+ Lattice # average total water in g/g 
                                  # set this to 0 for weights for a completely dry soil

# Define variables in Equation 3 from Baldoncini et al., 2018

m.mu.s <- 0.005257 # mass attenuation coefficient in m^2 kg^-1 of soil
m.mu.a <- 0.005229 # mass attenuation coefficient in m^2 kg^-1 of air
m.mu.w <-0.005835 # mass attenuation coefficient in m^2 kg^-1 of water
  
rho <- 1368 # average bulk density of soil kg/m^3 (non-weighted)

# incorporate average water content into rho:
rho.w <- AvgTotGrav*rho # average total volumetric water content in kg/m^3
rhoTOT <-rho +rho.w #sum of soil bulk density and water content density to get total ground density
rho.a <- 1.24 # density of air kg/m^3

wt.s <- rho/rhoTOT # weight fraction of soil 
wt.w <- rho.w/rhoTOT # weight fraction of water

m.mu.g <- wt.s*m.mu.s + wt.w*m.mu.w # mass attentuation coefficient of the ground with AvgTotGrav water content

mu.s <- m.mu.g * rhoTOT # linear attenuation coefficient of ground in m^-1 with AvgTotGrav water content

mu.a <- m.mu.a *rho.a # linear attenuation coefficient of air in m^-1

h <- 1.86 # height (m) of detector above the ground
tmax <- 0.7 # m # preliminary estimate

###################################################
# Anti-derivative for gamma flux, Flux(h, t), from defined soil thickness 
# and infinite radius 
# integrating over x, the detector angle (theta)
# Variable t is not defined

F = antiD(sin(x)*exp(-(mu.a * h)/cos(x)) * (1- exp(-(mu.s *t)/cos(x))) ~ x, mu.a = mu.a, h = h, mu.s = mu.s, force.numeric = TRUE)
F

# Anti-derivative for gamma flux, Flux_tot(h), from semi-infinite homogeneous source
# integrating over x, the detector angle (theta)

Ftot = antiD(sin(x)*exp(-(mu.a * h)/cos(x)) ~ x, mu.a = mu.a, h = h, force.numeric = TRUE)
Ftot

# Calculate Relative gamma flux originating at and above d, depth in soil profile 

FluxRel <- function(d)((F(x = pi/2, t = d) - F(x = 0, t = d))/(Ftot(x = pi/2) - Ftot(x = 0)))
FluxRelV <- Vectorize(FluxRel)

# check
depth <- array(seq(0, tmax, 0.001))
calcFluxRel <- FluxRelV(depth)
plot(calcFluxRel, -depth)

RelFluxDf <- data.frame(d = depth, RelFlux = calcFluxRel)

# extract closest values to criteria
d50 <- RelFluxDf[which.min(abs(0.5 - RelFluxDf$RelFlux)),]
d65 <- RelFluxDf[which.min(abs(0.65 - RelFluxDf$RelFlux)),]
d75 <- RelFluxDf[which.min(abs(0.75 - RelFluxDf$RelFlux)),]
d95 <- RelFluxDf[which.min(abs(0.95 - RelFluxDf$RelFlux)),]
d99 <- RelFluxDf[which.min(abs(0.99 - RelFluxDf$RelFlux)),]

combD <- rbind(d50, d65, d75, d95, d99)
combD$I_rel<-c("50%", "65%", "75%", "95%", "99%")
combD$d <- -combD$d

print(combD)

SensingDepth <- ggplot()+
  geom_hline(data = combD, aes(yintercept = d[1], linetype  = I_rel[1], color = I_rel[1]), linewidth = 1)+
  geom_hline(data = combD, aes(yintercept = d[2], linetype  = I_rel[2], color = I_rel[2]), linewidth = 1)+
  geom_hline(data = combD, aes(yintercept = d[3], linetype  = I_rel[3], color = I_rel[3]), linewidth = 1)+
  geom_hline(data = combD, aes(yintercept = d[4], linetype  = I_rel[4], color = I_rel[4]), linewidth = 1)+
  geom_hline(data = combD, aes(yintercept = d[5], linetype  = I_rel[5], color = I_rel[5]), linewidth = 1)+
  geom_hline(aes(yintercept = 0))+
  labs(x = "Radius (m)", y = "Depth (m)", linetype  = "Relative \n Intensity", color = "Relative \n Intensity")+
  scale_x_continuous(breaks = seq(-100, 100, 10), minor_breaks = seq(-100, 100, 2), limits = c(-30, 30))+
  scale_y_continuous(breaks = seq(-0.8, 0, 0.1), minor_breaks = seq(-0.8, 0.1, 0.02), limits = c(-0.8, 0.05))+
  scale_color_viridis_d(begin = 0, end = 0.8)

ggsave(SensingDepth, filename = paste0(outDir, "\\SensingDepthInfiniteRadius", stamp, ".tiff"), width = 6, height = 6, dpi = 400)

# update tmax for 99% of detected signal
tmax <- -combD$d[5]

#########Explore effect of rho and average gravimetric water content on tmax ##########

# Maximum sensing depth as a function of rho: ################################

# r is rho in kg/m^3
# w is total water content
# p is % sensing depth

max_depth <- function(r, w, p){
  
  m.mu.s <- 0.005257 # mass attenuation coefficient in m^2 kg^-1 of soil
  m.mu.a <- 0.005229 # mass attenuation coefficient in m^2 kg^-1 of air
  m.mu.w <-0.005835 # mass attenuation coefficient in m^2 kg^-1 of water
  
  rho <- r # average bulk density of soil kg/m^3 (non-weighted)
  
  # incorporate average water content into rho:
  rho.w <- w*rho # average total volumetric water content in kg/m^3
  rhoTOT <-rho +rho.w #sum of soil bulk density and water content density to get total ground density
  rho.a <- 1.24 # density of air kg/m^3
  
  wt.s <- rho/rhoTOT # weight fraction of soil 
  wt.w <- rho.w/rhoTOT # weight fraction of water
  
  m.mu.g <- wt.s*m.mu.s + wt.w*m.mu.w # mass attentuation coefficient of the ground with AvgTotGrav water content
  
  mu.s <- m.mu.g * rhoTOT # linear attenuation coefficient of ground in m^-1 with AvgTotGrav water content
  
  mu.a <- m.mu.a *rho.a # linear attenuation coefficient of air in m^-1
  
  h <- 1.86 # height (m) of detector above the ground
  tmax <- 0.7 # m # preliminary estimate
  
  # Anti-derivative for gamma flux, Flux(h, t), from defined soil thickness 
  # and infinite radius 
  # integrating over x, the detector angle (theta)
  # Variable t is not defined
  
  F = antiD(sin(x)*exp(-(mu.a * h)/cos(x)) * (1- exp(-(mu.s *t)/cos(x))) ~ x, mu.a = mu.a, h = h, mu.s = mu.s, force.numeric = TRUE)
  F
  
  # Anti-derivative for gamma flux, Flux_tot(h), from semi-infinite homogeneous source
  # integrating over x, the detector angle (theta)
  
  Ftot = antiD(sin(x)*exp(-(mu.a * h)/cos(x)) ~ x, mu.a = mu.a, h = h, force.numeric = TRUE)
  Ftot
  
  # Calculate Relative gamma flux originating at and above d, depth in soil profile 
  
  FluxRel <- function(d)((F(x = pi/2, t = d) - F(x = 0, t = d))/(Ftot(x = pi/2) - Ftot(x = 0)))
  FluxRelV <- Vectorize(FluxRel)
  depth <- array(seq(0, tmax, 0.001))
  calcFluxRel <- FluxRelV(depth)
  plot(calcFluxRel, -depth)
  
  RelFluxDf <- data.frame(d = depth, RelFlux = calcFluxRel)
  
  # extract closest values to criteria
  d99 <- RelFluxDf[which.min(abs(p - RelFluxDf$RelFlux)),]
  
  maxD <- d99[1,1]
  
  return(maxD)
}

Rho.range <- seq(1100, 1700, 10)
rho.df <- data.frame(matrix(ncol = 4, nrow = length(Rho.range)))
colnames(rho.df)<- c("rho (kg/m^3)", "0.99 max depth", "0.95 max depth", "0.86 max depth")
for(r in 1:length(Rho.range)){
  depth0.99 <- max_depth(Rho.range[r], w = AvgTotGrav, p = 0.99)
  depth0.95 <- max_depth(Rho.range[r], w = AvgTotGrav, p = 0.95)
  depth0.86 <- max_depth(Rho.range[r], w = AvgTotGrav, p = 0.86)
  rho.df[r,]<- c(Rho.range[r], depth0.99, depth0.95, depth0.86)
}

# repeat for water content:
W.range <- seq(0, 0.7, 0.01)
w.df <- data.frame(matrix(ncol = 4, nrow = length(W.range)))
colnames(w.df)<- c("Total w (g/g)", "0.99 max depth", "0.95 max depth", "0.86 max depth")
for(r in 1:length(W.range)){
  depth0.99 <- max_depth(rho, w = W.range[r], p = 0.99)
  depth0.95 <- max_depth(rho, w = W.range[r], p= 0.95)
  depth0.86 <- max_depth(rho, w = W.range[r], p= 0.86)
  w.df[r,]<- c(W.range[r], depth0.99, depth0.95, depth0.86)
}

sensDepthrho <- ggplot()+
  geom_line(aes(x = rho.df$`rho (kg/m^3)`/1000, y = rho.df$`0.99 max depth`, linetype = "99%"))+
  geom_line(aes(x = rho.df$`rho (kg/m^3)`/1000, y = rho.df$`0.95 max depth`, linetype = "95%"))+
  geom_line(aes(x = rho.df$`rho (kg/m^3)`/1000, y = rho.df$`0.86 max depth`, linetype = "86%"))+
  scale_x_continuous(name = bquote("Bulk density " (g ~cm^-3)), breaks = seq(1.1, 1.7, 0.1))+
  scale_y_reverse( limits = c(0.45, 0.05))+ 
  labs(y = "Sensing Depth (m)", title = "a)", linetype = "% Signal")

sensDepthW <- ggplot()+
  geom_line(aes(x = w.df$`Total w (g/g)`, y = w.df$`0.99 max depth`, linetype = "99%"))+
  geom_line(aes(x = w.df$`Total w (g/g)`, y = w.df$`0.95 max depth`, linetype = "95%"))+
  geom_line(aes(x = w.df$`Total w (g/g)`, y = w.df$`0.86 max depth`, linetype = "86%"))+
  scale_x_continuous(name = bquote("Total water " (g ~g^-1)), breaks = seq(0, 0.7, 0.1))+
  scale_y_reverse( limits = c(0.45, 0.05))+
  labs( y = "Sensing Depth (m)",title = "b)", linetype = "% Signal")

combsens <- ggarrange(sensDepthrho, sensDepthW, ncol = 1, nrow = 2)
ggsave(combsens, filename = paste0(outDir, "\\SensitivityOfMaxSensingDepth.tiff"), dpi = 300, width = 6, height = 6)
 
###############################################################################
# Integrate Relative Flux function ############################################

A<- antiD(FluxRelV(d_i)~d_i, force.numeric = TRUE)
A

# check
A(d_i = tmax)-A(d_i = 0)

# Find constant, a, that conserves the weights #################################

whole.under <- A(d_i= tmax)- A(d_i= 0) # area under the curve from 0 to tmax
whole.over <- 1*tmax - whole.under # area between a relative Flux of 0.99 and the curve
a <- 1/whole.over

####### Calculate and plot integrals for visual check #########################
intgr.pred <- matrix(depth[-length(depth)], nrow = length(depth)-1, ncol = 2)

for (s in 1:(length(depth)-1)){
  out <- (A(d_i = depth[s+1])-A(d_i = depth[s]))
  intgr.pred[s, 2]<- out
}

#tiff(paste0(outDir, "\\Integrals", stamp,".tif"))
plot(intgr.pred[,1], intgr.pred[,2], xlab = "Depth, m", ylab = "Area under curve for 5 cm interval", 
     main = "Integrals for 5 cm intervals")
#dev.off()

#### Find weights for sampling depth intervals #################################

depth2 <- seq(0, 0.3, 0.05) # sampling depth intervals
depth2 <- append(depth2, tmax) # the last sample, 0.3 - 0.35, will represent 0.3 - tmax. 

# Set up dataframe
weights <- matrix(nrow = length(depth2)-1, ncol = 2)
weights[,1] <- depth2[-length(depth2)]

# Calculate integral for each interval(A_i), subtract from total area of interval (B_i), 
# and multiply by weight-conserving constant (a)

for (w in 1:(length(depth2)-1)){
  B_i <- (depth2[w+1]- depth2[w])*1 # width times height (height = 1)
  A_i <- (A(d_i = depth2[w+1])-A(d_i = depth2[w]))
  weighted <- a*(B_i-A_i) # multiply by weighting factor, a, calculated above
  weights[w,2] <- weighted
}

# Visualize weights
plot(x = weights[,1], y = weights[,2], xlab = "Depth", ylab = "Weight",
     ylim = c(0, 0.5), xlim = c(0, 1), main = "Weights calculated for sampling intervals")

# Check that weights sum to 1
print(paste0("the sum of the weights is ", sum(weights[,2])))

# Edit weights data frame

to <- depth2[-length(depth2)] + 0.05 #upper end of 5 cm sampling intervals
to[length(to)]<- tmax

weights.df <- data.frame(weights[,1], to, weights[,2])
colnames(weights.df)<- c("From (m)", "To (m)", "Weight")

# Export weights data frame
write.csv(weights.df, file = paste0(outDir, "\\Weights_", stamp, ".csv"))
