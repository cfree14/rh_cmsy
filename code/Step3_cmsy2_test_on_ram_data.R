
# Setup
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(plyr)
library(dplyr)
library(reshape2)
library(datalimited2)

# Directories
datadir <- "data"
codedir <- "code/functions"
plotdir <- "figures/ram_test"
outputdir <- "output/ram_test_cmsy2"

# Read RAM Legacy Database
load(file.path(datadir, "ram_test_dataset.Rdata"))

# Loop through and test
################################################################################

# Add B/BMSY predictions column
stocks$bbmsy <- NA

# Loop through and test
i <- 1
for(i in 1:nrow(stocks)){
    
  # Subset data
  stock <- stocks$stockid[i]
  sdata <- data %>% 
    filter(stockid==stock) %>% 
    rename(stock=stockid)
  key <- stocks %>% 
    filter(stockid==stock) %>% 
    select(species, family, resilience) %>% 
    mutate(id_fixed=F)
  
  # Assess using RH-cMSY
  out <- try(cmsy2(year=sdata$year, catch=sdata$catch, resilience=key$resilience, verbose=F))
  
  # If RH-cMSY converged, visualize
  if(!inherits(out, "try-error")){
    
    # Save output
    save(out, file=file.path(outputdir, paste0(stock, ".Rdata")))
    
    # Record B/BMSY prediction
    stocks$bbmsy[i] <- last(out$preds$bbmsy)

  }
  
}

# Quickly calculate and visualize relative error
################################################################################

# Calculalate proportional error
stocks$pe <- (stocks$bbmsy - stocks$bbmsy_final) / abs(stocks$bbmsy_final)

# Visualize proportional error
par(mfrow=c(1,1))
boxplot(stocks$pe, frame=F, las=1, ylab="Proportional error", lty=1)

# Export
write.csv(stocks, file.path(outputdir, "ram_test_results_quick.csv"), row.names=F)


