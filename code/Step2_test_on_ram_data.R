
# Setup
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(plyr)
library(dplyr)
library(reshape2)

# Directories
datadir <- "data"
codedir <- "code/functions"
plotdir <- "figures/ram_test"
outputdir <- "output/ram_test"

# Read RAM Legacy Database
load(file.path(datadir, "ram_test_dataset.Rdata"))

# Source RH-cMSY funcitons
source(file.path(codedir, "rh_cmsy.R"))

# Loop through and test
################################################################################

# Add B/BMSY predictions column
stocks$bbmsy <- NA
stocks$nviable <- NA
# stocks <- filter(stocks, type=="invert")

# Loop through and test
i <- 1
stock <- "BSBASSSATL"
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
  print(paste(i, stock, sep=" - "))
  out <- try(rh_cmsy(data=sdata, key=key, npairs=5000))
  
  # If RH-cMSY converged, visualize
  if(!inherits(out, "try-error")){
    
    # Format "true" data for visualization
    true <- NULL
    true$rk <- data.frame(r=NA, k=NA)
    true$ts <- sdata
    
    # Visualize
    # plot_rh_cmsy(out, true)
    plot_rh_cmsy(out, true, savedir=plotdir)
    
    # Save output
    save(out, file=file.path(outputdir, paste0(stock, ".Rdata")))
    
    # Record B/BMSY prediction
    stocks$bbmsy[i] <- last(out$preds$bbmsy)
    stocks$nviable[i] <- nrow(out$id_rk_v)
    
  }
  
}

# Quickly calculate and visualize relative error
################################################################################

# Calculalate proportional error
stocks$pe <- (stocks$bbmsy - stocks$bbmsy_final) / abs(stocks$bbmsy_final)

# Visualize proportional error
par(mfrow=c(1,1))
boxplot(stocks$pe, frame=F, las=1, ylab="Proportional error", lty=1)
lines(x=c(0.6,1.4), y=c(0,0), lty=3, lwd=1.3)

# Export
write.csv(stocks, file.path(outputdir, "ram_test_results_quick.csv"), row.names=F)


