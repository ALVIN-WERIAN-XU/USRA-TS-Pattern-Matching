# Define four different time series
library(interpTools)
library(tsinterp)
library(stats)
library(plotly)

simData_1 <- simXt(N = 1000, numTrend = 2, numFreq = 10, bandwidth = 2, snr = 1.5)
simData_2 <- simXt(N = 1000, numTrend = 4, numFreq = 30, snr = 3)
simData_3 <- simXt(N = 1000, numTrend = 3, a = c(1/3, 2/3, 5/3), mu = 5, numFreq = 20, snr = 0.7)
simData_4 <- simXt(N = 1000, numTrend = 6, numFreq = 40, p = 1, q = 2)       

plotXt(simData_1, cptwise = T, axisLabels = F, plot.title = T, return = NULL)
plotXt(simData_2, cptwise = T, axisLabels = F, plot.title = T, return = NULL)
plotXt(simData_3, cptwise = T, axisLabels = F, plot.title = T, return = NULL)
plotXt(simData_4, cptwise = T, axisLabels = F, plot.title = T, return = NULL)

# Plot time series and residuals for Series 1
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(simData_1$Xt, type = "l", main = "Series 1", ylab = "Value", xlab = "Time")

# Plot time series and residuals for Series 2
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(simData_2$Xt, type = "l", main = "Series 2", ylab = "Value", xlab = "Time")

# Plot time series and residuals for Series 3
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(simData_3$Xt, type = "l", main = "Series 3", ylab = "Value", xlab = "Time")

# Plot time series and residuals for Series 4
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(simData_4$Xt, type = "l", main = "Series 4", ylab = "Value", xlab = "Time")

# Checking seasonality and periodicity for Series 1
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
acf(simData_1$Xt, main = "Series 1 ACF")
spectrum(simData_1$Xt, main = "Series 1 Period")

# ACF shows strong periodic seasonal component
# specific peaks

# Checking seasonality and periodicity for Series 2
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
acf(simData_2$Xt, main = "Series 2 ACF")
spectrum(simData_2$Xt, main = "Series 2 Period")

# similar as 1

# Checking seasonality and periodicity for Series 3
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
acf(simData_3$Xt, main = "Series 3 ACF")
spectrum(simData_3$Xt, main = "Series 3 Period")

# less regularity --- more random variation and less periodicity
# lack strong ACF--- stochastic
# lack distinct peak

# Checking seasonality and periodicity for Series 4
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
acf(simData_4$Xt, main = "Series 4 ACF")
spectrum(simData_4$Xt, main = "Series 4 Period")

#series might be less seasonal and potentially more influenced by non-repeating external factors or noise.
# High frequency with high spectrum

# Creating list object containing the time series themselves in preparation for simulateGaps()
OriginalData = list(D1 = simData_1$Xt,
                    D2 = simData_2$Xt,
                    D3 = simData_3$Xt,
                    D4 = simData_4$Xt)

# Usage in simulateGaps() 
p = c(0.05,0.10,0.15,0.20)
g = c(1,5,10)
K = 10 
GappyData <- simulateGaps(OriginalData = OriginalData, p = p, g = g, K = K)

# Replace with different interpolation methods
method1 <- "NN"
method2 <- "LI"
method3 <- "NCS"
method4 <- "FMM"
method5 <- "HCS"
method6 <- "SI"
method7 <- "KAF"
method8 <- "KSF"
method9 <- "LOCF"
method10 <- "NOCB"
method11 <- "SMA"
method12 <- "LWMA"
method13 <- "EWMA"
method14 <- "RMEA"
method15 <- "RMED"
method16 <- "RMOD"
method17 <- "RRND"

# Fail to use
method18 <- "HWI"

TtP <- 0
freqSave <- 0
MtP <- 0

# Interpolation
# For interp interpolation
replace_interp<- parInterpolate(GappyData = GappyData, methods = c(method14,method15,method16), numCores = 1)

# HWI interpolation
resinterp<- parInterpolate(GappyData = GappyData, methods = method18, numCores = 1)

# NCS 
NCS_interp <- parInterpolate(GappyData = GappyData, methods = method3, numCores = 1)

# FMM
FMM_interp <- parInterpolate(GappyData = GappyData, methods = method4, numCores = 1)

# NCS and FMM
NCSFMM_interp <- parInterpolate(GappyData = GappyData, methods = c(method3,method4), numCores = 1)

# performance
# For replace inrerpolation
replace_pf <- performance(OriginalData = OriginalData, IntData = replace_interp, GappyData = GappyData)

# For HWI interpolation
HWI_pf <- performance(OriginalData = OriginalData, IntData = resinterp, GappyData = GappyData)

# Fail: NCS 
NCS_pf <- performance(OriginalData = OriginalData, IntData = NCS_interp, GappyData = GappyData)

# Fail: FMM
FMM_pf <- performance(OriginalData = OriginalData, IntData = FMM_interp, GappyData = GappyData)

# Fail: NCS and FMM performance
NCSFMM_pf <- performance(OriginalData = OriginalData, IntData = NCSFMM_interp, GappyData = GappyData)


# Aggregation
# For replace
ag_pf <- aggregate_pf(pf, custom = NULL)

# For HWI
HWIag_pf <- aggregate_pf(HWI_pf, custom = NULL)

# plotsurface

# Success
multiSurface(
  d = 1:length(ag_pf),
  m = c(method14,method15,method16),
  metric="MSE",
  ag_pf,
  layer_type = "method",
  f = "median",
  highlight = method14,
  highlight_color =  "#FA812F",
  colors = c("#F9E0AA", "#F7C65B", "mistyrose2", "mistyrose2", "#FA4032", "#F92111")
)

# Success
plotSurface(
  d = 1:length(ag_pf),
  m = c(method14,method15,method16),
  metric="MSE",
  ag_pf,
  layer_type = "method",
  f = "median",
  highlight = NULL,
  highlight_color = "#FF0000")

# Success
plotSkew(
  HWIag_pf,
  plotEach = TRUE,
  show_symmetric = NULL,
  output = "plot",
  metric = rownames(HWIag_pf[[1]][[1]][[1]][[1]])
)

# Fail (problem of agEvaluate())
heatmapGrid(
  ag_pf,
  f = "median",
  crit="MSE",
  m=c(method14,method15,method16),
  d = 1:length(ag_pf),
  colors = c("#F9E0AA", "#F7C65B", "#FAAF08", "#FA812F", "#FA4032", "#F92111")
)

# Fail
plotSurface2(
  agObject=ag_pf,
  m = c(method14,method15,method16),
  metric = "MSE",
  toggle="method",
  f = "median",
  highlight = method14,
  highlight_color = "#FF0000",
  colors = c("#F9E0AA", "#F7C65B", "mistyrose2", "mistyrose2"))

# Fail 
plotMetrics(
  ag_pf,
  d = names(ag_pf),
  p=c(0.05,0.10),
  g=c(1,5),
  m = c(method14,method15,method16),
  metric="MSE",
  highlight_color = "mistyrose2"
)

# Load required libraries
library(interpTools)
library(tsinterp)
library(stats)
library(plotly)
library(ggplot2)

# 1. Generate four different time series
set.seed(42)  # Ensure reproducibility
simData_1 <- simXt(N = 1000, numTrend = 2, numFreq = 10, bandwidth = 2, snr = 1.5)
simData_2 <- simXt(N = 1000, numTrend = 4, numFreq = 30, snr = 3)
simData_3 <- simXt(N = 1000, numTrend = 3, a = c(1/3, 2/3, 5/3), mu = 5, numFreq = 20, snr = 0.7)
simData_4 <- simXt(N = 1000, numTrend = 6, numFreq = 40, p = 1, q = 2)       

# 2. Store time series in a list
OriginalData = list(D1 = simData_1$Xt,
                    D2 = simData_2$Xt,
                    D3 = simData_3$Xt,
                    D4 = simData_4$Xt)

# 3. Simulate missing data
p = c(0.05, 0.10, 0.15, 0.20)  # Percentage of missing values
g = c(1,5,10)  # Gap sizes
K = 10  # Number of different missing patterns
GappyData <- simulateGaps(OriginalData = OriginalData, p = p, g = g, K = K)

# 4. Define all 16 interpolation methods
methods <- c("NN", "LI", "NCS", "FMM", "HCS", "SI", "KAF", "KSF",
             "LOCF", "NOCB", "SMA", "LWMA", "EWMA", "RMEA", "RMED", "RMOD")

num_simulations <- 1000

# Special Initialization of HWI 
TtP <- 0
freqSave <- 0
MtP <- 0


# 5. Create an empty list to store RMSE results
rmse_results <- list()

# 6. Loop to perform interpolation 1000 times for each method
for (method in methods) {
  rmse_list <- numeric(num_simulations)  # Store RMSE values
  
  for (i in 1:num_simulations) {
    # Perform interpolation
    interpolated_data <- parInterpolate(GappyData = GappyData, methods = method, numCores = 1)
    
    # Compute performance (RMSE)
    performance_result <- performance(OriginalData = OriginalData, IntData = interpolated_data, GappyData = GappyData)
    
    # Extract RMSE value
    rmse_list[i] <- performance_result[[1]][[1]]$RMSE  # Modify indexing based on output structure
  }
  
  # Store RMSE values for the method
  rmse_results[[method]] <- rmse_list
}

# 7. Convert results into a data frame for plotting
rmse_data <- do.call(rbind, lapply(names(rmse_results), function(m) {
  data.frame(Method = m, RMSE = rmse_results[[m]])
}))

# 8. Plot histograms of RMSE distributions
ggplot(rmse_data, aes(x = RMSE, fill = Method)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.3) +
  theme_minimal() +
  labs(title = "RMSE Distribution for 16 Interpolation Methods",
       x = "RMSE",
       y = "Frequency") +
  scale_fill_manual(values = rainbow(16)) +
  theme(legend.position = "bottom")

