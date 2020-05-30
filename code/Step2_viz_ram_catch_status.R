
# Setup
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(plyr)
library(dplyr)
library(reshape2)
library(zoo)
library(segmented)

# Directories
datadir <- "data"
plotdir <- "figures"

# Read RAM Legacy Database
load(file.path(datadir, "ram_test_dataset.Rdata"))

# Plot data
################################################################################

# Extract trend
trend <- function(x, y){
  lmfit <- lm(y ~ x)
  freeR::slope(lmfit)
}

# Colors
col <- c("darkgreen", "orange", "red", "grey70")

# Format data
data_orig <- data
data <- data_orig %>% 
  mutate(status=bbmsy2catg(bbmsy),
         status=ifelse(is.na(status), "unknown", status),
         status_col=col[factor(status, levels=c("under", "fully", "over", "unknown"))]) %>% 
  group_by(stockid) %>% 
  mutate(catch_sd=catch/max(catch), 
         catch_smooth=rollapply(catch_sd, 10, mean, partial=T),
         catch_trend=rollapply(catch_smooth, 5, function(y) trend(1:5, y), fill=NA))



# Setup plotting
figname <- "AppendixA_RAM_catch_status_plots.pdf"
pdf(file.path(plotdir, figname), height=11.5, width=8.5)
par(mfrow=c(6,4), mar=c(3,2,2,0.5), oma=c(4,4,4,4))

# Loop through stocks and plot
i <- 1
# for(i in 1:20){
for(i in 1:nrow(stocks)){
  
  # Subset
  stock <- stocks$stockid[i]
  sdata <- filter(data, stockid==stock)
  
  # Catch
  ######################################
  
  # Plot catch (raw)
  xmin <- freeR::floor1(min(sdata$year), 50)
  xmax <- freeR::ceiling1(max(sdata$year), 10)
  plot(catch_sd ~ year, sdata, type="h", col=status_col, las=2, bty="n", 
       xlim=c(xmin, 2020), ylim=c(0,1), ylab="", xlab="", main=stock, cex.main=0.9)
  
  # Catch (smoothed)
  ######################################
  
  # Plot catch (smoothed)
  plot(catch_smooth ~ year, sdata, type="h", col=status_col, las=2, bty="n", 
       xlim=c(xmin, 2020), ylim=c(0,1), ylab="", xlab="", main="", cex.main=0.9)
  
  # Evaluate segmented models
  ######################################
  
  # Plot catch (smoothed)
  plot(catch_smooth ~ year, sdata, type="h", col=status_col, las=2, bty="n", 
       xlim=c(xmin, 2020), ylim=c(0,1), ylab="", xlab="", main="", cex.main=0.9)
  
  # Step 1. Decide maximum number of inflection points to evaluate
  nyr <- nrow(sdata)
  nyr_use <- nyr - 10
  n_eval <- ceiling(nyr_use / 10)
  
  # Step 2. Segmented regression requires a linear regression
  lmfit <- lm(catch_smooth ~ year, sdata)
  
  # Step 3. Loop through number of breakpoints
  segfits <- list()
  segfits[[1]] <- lmfit
  for(j in 1:n_eval){
    start_yrs <- seq(min(sdata$year)+5, max(sdata$year)-5, length.out=j+2)[2:(j+1)]
    segfit <- try(segmented(lmfit, seg.Z = ~year, psi=start_yrs))
    if(!inherits(segfit, "try-error")){
      segfits[[j+1]] <- segfit
    }else{
      segfits[[j+1]] <- NA
    }
  }
  names(segfits) <- c("lm", paste0("seg", 1:n_eval))
  
  # Step 4. Use AIC to compete models
  segfits_use <- segfits[which(sapply(segfits, length)!=1)]
  aic_vals <- sapply(segfits_use, AIC)
  best_model <- segfits_use[[which.min(aic_vals)]]
  best_model_name <- names(segfits_use)[[which.min(aic_vals)]]
    
  # Step 5. Plot best model
  if(best_model_name!="lm"){
    plot(best_model, add=T, lwd=1.2, rug=F)
  }
  
  # Evaluate segmented models
  ######################################
  
  # Plot empty
  freeR::emptyplot()
  
}

# Off
dev.off()


