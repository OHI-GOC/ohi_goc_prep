# Intermediate biomass priors
rm(list = ls())
setwd('/Users/EricaFerrer/Desktop/Overfishing & Emissions/')

# Define functions
ma    <- function(x){
  x.1    <-   filter(x, rep(1/3,3), sides=1)
  x.1[1] <- x[1]
  x.1[2] <- (x[1]+x[2])/2
  return(x.1)
}

# Read data
#data = read.csv(file = 'data/metadata_conapesca_MSY_august.csv') # Initial biomass range
#data = read.csv("Data/metadata_File2.csv")
data = read.csv("Data/File 2_Stocks Metadata.csv")

#data.ts = read.csv('data/data_conapesca_MSY_august.csv') # Catch timeseries
#data.ts = read.csv("Data/FocalFisheries_File1.csv")
data.ts = read.csv("Data/File 1_CONAPESCA_SoI Timeseries.csv")
colnames(data.ts)<- c("Stock","yr","ct","n_sample")

# Go stock by stock and calculate intermediate and final biomass range
th = 0.3 # Threshold of increment of a 30%
n <- NROW(data)
intb.low = rep(NA,n)
intb.hi = rep(NA,n)
endb.low = rep(NA,n)
endb.high = rep(NA,n)

i=2

for (i in 1:length(data$ID)){
  if(data$Pick[i] == 1){
    stock = as.character(data$Stock[i])
    stock.id = which(data.ts$Stock == stock)
    stock.ts = data.ts[stock.id,]
    
    # Calculate 3 years running average
    stock.ts$ct = ma(stock.ts$ct)
    #stock.ts$ct[i] = forecast::ma(stock.ts$ct[i], order=3)
    
    # Get intermediate year and calculate coefficient (C)
    int.yr = data$int.yr[i]
    int.yr.id = which(stock.ts$yr == int.yr)
    st.catch = stock.ts$ct[1]
    int.catch = stock.ts$ct[int.yr.id]
    
    C = (int.catch - st.catch)/st.catch
    
    # Conditions
    if(abs(C) <= th){
      intb.low[i] = data$stb.low[i]
      intb.hi[i] = data$stb.hi[i]
    } else if(C < th){
      intb.low[i] = data$stb.low[i] - 0.1
      intb.hi[i] = data$stb.hi[i] - 0.1
    } else if(C > th){
      intb.low[i] = data$stb.low[i] + 0.1
      intb.hi[i] = data$stb.hi[i] + 0.1
    }
    
    # Get coefficient and ranges for final biomass
    max.yr.i <- which.max(stock.ts$ct[4:(length(stock.ts$ct)-3)])+3
    
    # Contrast final year against maximum year
    max.catch = stock.ts$ct[max.yr.i]
    end.catch = stock.ts$ct[nrow(stock.ts)]
    C2 = end.catch/max.catch
    
    # If Contrast is high C2 > 0.8, increase the upper limit of the original biomass
    if(C2 > 0.8){
      endb.low[i] = data$stb.low[i]
      endb.high[i] = min(c(data$stb.hi[i] + 0.1,1)) # Upper boundary of 1
      # If contrast is very low, decrease it by 1
    } else if(C2 < 0.5){
      endb.low[i] = 0.01
      endb.high[i] = 0.4
      if(endb.high[i] ==0.4){
        if(C2 < 0.05) {endb.high[i] <- 0.1
        } else if(C2 < 0.15) {
          endb.high[i] <- 0.2
        } else if(C2 < 0.35) {
          endb.high[i] <- 0.3
        } else {endb.high[i] <- 0.4}
      }
      
    } else {
      endb.low[i] = data$stb.low[i]
      endb.high[i] = data$stb.hi[i]
    }
    
    # if default endbio is low (0.01-0.4), check whether the upper bound should be lower than 0.4 for depleted stocks
  }
}

R = data.frame(data$Stock, intb.low, intb.hi, endb.low, endb.high)
write.csv(R, file = 'Data/InitialConditions.csv')
