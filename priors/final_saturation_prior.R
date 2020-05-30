
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Read RAM Legacy Database
load("/Users/cfree/Dropbox/Prelim Database Files/Versions/RAM v4.41 (8-20-18)/DB Files With Assessment Data/DBdata (assessment data only).RData")


# Build data
################################################################################

# Get TB0 and SSB0 values and units
refs <- bioparams_values_views %>% 
  # Get TB0 and SSB0
  select(stockid, TB0, SSB0) %>% 
  filter(!is.na(TB0) | !is.na(SSB0)) %>% 
  rename(tb0=TB0, ssb0=SSB0) %>% 
  # Add TB0 and SSB0 units
  left_join(select(bioparams_units_views, stockid, TB0, SSB0), by="stockid") %>% 
  rename(tb0_units=TB0, ssb0_units=SSB0)

# Get stock time series values and units
ts <- timeseries_values_views %>% 
  # Reduce to stocks with either TB0 or SSB0
  filter(stockid %in% refs$stockid) %>% 
  # Select columns of interest
  select(stockid, year, TB, SSB, TC, TL) %>% 
  setNames(., tolower(colnames(.))) %>% 
  # Add units
  left_join(select(timeseries_units_views, stockid, TB, SSB, TC, TL), by="stockid") %>% 
  rename(tb_units=TB, ssb_units=SSB, tc_units=TC, tl_units=TL) %>% 
  # Add TB0 and SSB0 values
  left_join(refs, by="stockid")  %>% 
  # Create B0 reference points
  mutate(tbdivtb0=ifelse(tb0_units==tb_units, tb/tb0, NA),
         ssbdivssb0=ifelse(ssb0_units==ssb_units, ssb/ssb0, NA))


# Identify which saturation measure to use and which year (TB/TB0 > SSB/SSB0)
stats <- ts %>% 
  group_by(stockid) %>% 
  # Determine year/value of most recent B/B0
  summarise(tbdivtb0_yr=max(year[!is.na(tbdivtb0)]),
            ssbdivssb0_yr=max(year[!is.na(ssbdivssb0)]),
            tbdivtb0_yr=ifelse(is.infinite(tbdivtb0_yr), NA, tbdivtb0_yr),
            ssbdivssb0_yr=ifelse(is.infinite(ssbdivssb0_yr), NA, ssbdivssb0_yr),
            tbdivtb0=ifelse(!is.na(tbdivtb0_yr), tbdivtb0[year==tbdivtb0_yr], NA),
            ssbdivssb0=ifelse(!is.na(ssbdivssb0_yr), ssbdivssb0[year==ssbdivssb0_yr], NA)) %>% 
  # Remove stocks without B/B0 information
  filter(!is.na(tbdivtb0_yr) | !is.na(ssbdivssb0_yr)) %>% 
  # Determine whether to use TB/TBMSY or SSB/SSBMSY
  mutate(bdivb0_type=NA,
         bdivb0_type=ifelse(!is.na(tbdivtb0) & is.na(ssbdivssb0), "TB",
                          ifelse(!is.na(ssbdivssb0) & is.na(tbdivtb0), "SSB", 
                                 ifelse(tbdivtb0_yr >= ssbdivssb0, "TB", bdivb0_type))),
         bdivb0=ifelse(bdivb0_type=="TB", tbdivtb0, ssbdivssb0),
         bdivb0_yr=ifelse(bdivb0_type=="TB", tbdivtb0_yr, ssbdivssb0_yr)) %>% 
  # Select columns
  select(stockid, bdivb0_type, bdivb0_yr, bdivb0)

# Calculate catch statistic for the relevant years
cstats <- ts %>% 
  # Reduce to stocks with B/BO information
  filter(stockid %in% stats$stockid) %>% 
  # Remove years occurign AFTER the year of the B/B0 estimate
  left_join(select(stats, stockid, bdivb0_yr), by="stockid") %>% 
  group_by(stockid) %>% 
  filter(year<=bdivb0_yr & (!is.na(tc) | !is.na(tl))) %>% 
  # Identify # of years of catch data, maximum catch, year of maximum catch, and proportion of cmax in terminal year
  group_by(stockid, bdivb0_yr) %>% 
  summarize(tc_n=sum(!is.na(tc)),
            tc_yr1=min(year[!is.na(tc)]),
            tc_max=max(tc, na.rm=T),
            tc_max_yr=min(year[tc==tc_max]),
            tc_final=tc[year==max(year)],
            tc_ratio=tc_final / tc_max,
            # Landings stats
            tl_n=sum(!is.na(tl)),
            tl_yr1=min(year[!is.na(tl)]),
            tl_max=max(tl, na.rm=T),
            tl_max_yr=min(year[tl==tl_max]),
            tl_final=tl[year==max(year)],
            tl_ratio=tl_final / tl_max,
            # Choose which to use
            catch_use=ifelse(!is.na(tc_final), "TC", "TL"),
            catch_n=ifelse(catch_use=="TC", tc_n, tl_n),
            catch_yr1=ifelse(catch_use=="TC", tc_yr1, tl_yr1),
            catch_max=ifelse(catch_use=="TC", tc_max, tl_max),
            catch_max_yr=ifelse(catch_use=="TC", tc_max_yr, tl_max_yr),
            catch_final=ifelse(catch_use=="TC", tc_final, tl_final),
            catch_ratio=ifelse(catch_use=="TC", tc_ratio, tl_ratio)) %>% 
  # Reduce columns
  select(stockid, bdivb0_yr, catch_use, catch_n, catch_yr1, 
         catch_max, catch_max_yr,  catch_final, catch_ratio)

# Add prop
for(i in 1:nrow(cstats)){
  catch_yr1 <- cstats$catch_yr1[i]
  bdivb0_yr <- cstats$bdivb0_yr[i]
  catch_max_yr <- cstats$catch_max_yr[i]
  catch_n <- cstats$catch_n[i]
  cstats$catch_max_yr_prop[i] <- which(catch_yr1:bdivb0_yr %in% catch_max_yr) / catch_n
}

# Merge stats
data <- stats %>% 
  left_join(select(cstats, stockid, catch_ratio, catch_max_yr_prop), by="stockid")

# Plot data
g <- ggplot(data, aes(x=catch_ratio, y=bdivb0, col=catch_max_yr_prop)) +
  geom_point() +
  labs(x="Catch ratio", y="Saturation (B/B0)") +
  geom_hline(yintercept=1, linetype="dashed") +
  scale_color_continuous(name="Max catch position") +
  theme_bw()
g


