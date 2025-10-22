# Script to plot raw time series per stock and select intermediate year
rm(list = ls())
setwd("Desktop/Overfishing & Emissions/Data/")

# Set functions

# Load data
#data = read.csv('data/data_conapesca_MSY_august.csv')
#cinfo = read.csv('data/metadata_conapesca_MSY_august.csv')

#data = read.csv("FocalFisheries_File1.csv")
data = read.csv("File 1_CONAPESCA_SoI Timeseries.csv")
#cinfo = read.csv("metadata_File2.csv")
cinfo = read.csv("File 2_Stocks Metadata.csv", header=TRUE)
colnames(data)<- c("Stock","yr","ct","n_sample")

# Select different stocks
stocks = cinfo$Stock
int.yr = numeric()
start.yr = numeric()
end.yr = numeric()
pick = numeric()
#pdf('output/intermediate_years.pdf', height = 5,width = 7)
# pdf("IntermediateYears.pdf", height = 5, width = 7)

for (i in 1:length(stocks)){
  Stock = as.character(stocks[i])
  id.Stock =  which(data$Stock == Stock)
  data.Stock = data[id.Stock,]
  
  # Export min/max year
  
  n.data <- NROW(data.Stock)
  
  # start.yr[i] = min(data.Stock$yr)
  # end.yr[i] = max(data.Stock$yr)
  # duration = end.yr[i] - start.yr[i]
  
  
  if (n.data > 9){
    pick[i] = 1
    # Plot
    plot(ct ~ yr, data.Stock, type = 'l', lwd = 2,
         xlab = "Year", ylab = "Total catch (kg)",
         main = Stock[1])
    points(ct ~ yr, data.Stock)
    abline(v = 2002, lty = 2)
    #abline(v = 2015, lty = 2)
    abline(v = 2017, lty = 2)
    
    # Plot maximum
    #m = max(data.Stock$ct[2:14])
    m = max(data.Stock$ct[2:16])
    id.max = which(data.Stock$ct == m)
    points(ct[id.max] ~ yr[id.max], data.Stock, col = 'red', pch = 19)
    
    # Interactive selection of the intermediate year
    int.yr[i] = round(as.numeric(locator(1))[1])
    id.int = which(data.Stock$yr == int.yr[i])
    points(ct[id.int] ~ yr[id.int], data.Stock, col = 'blue', pch = 19)
    
  } else {
    int.yr[i] = NA
    start.yr[i] = NA
    end.yr[i] = NA
    pick[i] = 0
  }
  
}

output <- data.frame(ID = cinfo$ID,
                     stock = cinfo$Stock,
                     int.yr = int.yr,
                     start.yr = start.yr,
                     end.yr = end.yr,
                     pick = pick)

#write.csv(cinfo,'data/metadata_conapesca_MSY_august_intyr.csv')
write.csv(output,"IntermediateYears.csv", row.names = F)
dev.off()
