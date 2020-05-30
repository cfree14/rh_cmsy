
# Setup
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(plyr)
library(dplyr)
library(reshape2)
<<<<<<< HEAD
library(datalimited2)
=======
>>>>>>> b000ee230b68cb93da265e6daf169dadc732c35a

# Directories
datadir <- "data"

# Read RAM Legacy Database
load("/Users/cfree/Dropbox/Prelim Database Files/Versions/RAM v4.41 (8-20-18)/DB Files With Assessment Data/DBdata (assessment data only).Rdata")


# Build stock key
################################################################################

# Build stock key
stock_key <- stock %>% 
  select(-c(tsn, inmyersdb, myersstockid)) %>% 
  # Add area name
  left_join(select(area, areaid, country, areaname), by="areaid") %>% 
  # Rename columns
  rename(species=scientificname, comm_name=commonname, area=areaname) %>% 
  # Format columns
  mutate(comm_name=freeR::sentcase(comm_name),
         species=gsub("spp.", "spp", species),
         species=revalue(species, c("Chrysophrys auratus"="Pagrus auratus",
                                    "Clupea pallasii"="Clupea pallasii pallasii",
                                    "Epinephelus flavolimbatus"="Hyporthodus flavolimbatus",
                                    "Epinephelus niveatus"="Hyporthodus niveatus",
                                    "Etrumeus teres"="Etrumeus sadina",
                                    "Loligo bleekeri"="Heterololigo bleekeri",
                                    "Loligo pealeii"="Doryteuthis pealeii",
                                    "Loligo reynaudii"="Loligo vulgaris reynaudii",
                                    "Merluccius gayi"="Merluccius gayi gayi",
                                    "Mullus barbatus"="Mullus barbatus barbatus",
                                    "Neoplatycephalus richardsoni"="Platycephalus richardsoni",
                                    "Psetta maxima"="Scophthalmus maximus",
                                    "Strangomera bentincki"="Clupea bentincki",
                                    "Tetrapturus albidus"="Kajikia albida",
                                    "Sardinops melanostictus"="Sardinops sagax"))) %>% 
  # Rearrange columns
  select(stockid, stocklong, country, region, area, species, comm_name)

# Check names
freeR::check_names(stock_key$species)


# Identify usable data
################################################################################

# Identify potentially usable stocks
colnames(timeseries_values_views)
stats <- timeseries_values_views %>% 
  group_by(stockid) %>% 
  # How many years of each data?
  summarize(tl_n=sum(!is.na(TL)), 
            tc_n=sum(!is.na(TC)), 
            bbmsy_tb_n=sum(!is.na(TBdivTBmsy)), 
            bbmsy_ssb_n=sum(!is.na(SSBdivSSBmsy))) %>% 
  # What are the catch units?
  left_join(select(timeseries_units_views, stockid, TL, TC), by="stockid") %>% 
  rename(tl_units=TL, tc_units=TC) %>% 
  # Which to use?
  mutate(catch_use=ifelse(tc_n>=tl_n, "TC", "TL"),
         catch_n=ifelse(tc_n>=tl_n, tc_n, tl_n),
         catch_units=ifelse(tc_n>=tl_n, tc_units, tl_units),
         bbmsy_use=ifelse(bbmsy_tb_n>=bbmsy_ssb_n, "TB", "SSB"),
         bbmsy_n=ifelse(bbmsy_tb_n>=bbmsy_ssb_n, bbmsy_tb_n, bbmsy_ssb_n)) %>% 
  # Remove ones with 20 yr of catch data
  filter(catch_n >= 20 & bbmsy_n>0)

# Build data - first pass
colnames(timeseries_values_views)
data_1st <- timeseries_values_views %>% 
  # Stocks of interest
  filter(stockid %in% stats$stockid) %>%
  # Columns of interest
<<<<<<< HEAD
  select(stockid, year, TC, TL, TBdivTBmsy, SSBdivSSBmsy, ERdivERmsy, FdivFmsy) %>% 
  rename(tc=TC, tl=TL, bbmsy_tb=TBdivTBmsy, bbmsy_ssb=SSBdivSSBmsy, 
         uumsy=ERdivERmsy, ffmsy=FdivFmsy) %>% 
=======
  select(stockid, year, TC, TL, TBdivTBmsy, SSBdivSSBmsy) %>% 
  rename(tc=TC, tl=TL, bbmsy_tb=TBdivTBmsy, bbmsy_ssb=SSBdivSSBmsy) %>% 
>>>>>>> b000ee230b68cb93da265e6daf169dadc732c35a
  # Add catch and B/BMSY to use
  left_join(select(stats, stockid, catch_use, bbmsy_use), by="stockid") %>% 
  # Create columns for catch and B/BMSY to use
  mutate(catch=ifelse(catch_use=="TC", tc, tl),
         bbmsy=ifelse(catch_use=="TB", bbmsy_tb, bbmsy_ssb)) %>% 
  # Reduce data
<<<<<<< HEAD
  select(stockid, year, catch, bbmsy, uumsy, ffmsy) %>% 
=======
  select(stockid, year, catch, bbmsy) %>% 
>>>>>>> b000ee230b68cb93da265e6daf169dadc732c35a
  filter(!is.na(catch)) %>% 
  # Trim years...
  group_by(stockid) %>% 
  # with 0 catch from beginning of time series
  filter(year>=min(year[catch>0])) %>% 
  # without BBMSY at end of time series
  filter(year<=max(year[!is.na(bbmsy)]))

# Make sure there are still 20 years of catch data and that catch data isn't gappy
check <- data_1st %>% 
  group_by(stockid) %>% 
  summarize(nyr=n(),
            yr1=min(year), 
            yr2=max(year),
            gaps=length(yr1:yr2)!=nyr) %>% 
  filter(nyr>=20)

# Any gaps?
sum(check$gaps) # must be 0


# Build final data
################################################################################

<<<<<<< HEAD
# Near final data
###########################

=======
>>>>>>> b000ee230b68cb93da265e6daf169dadc732c35a
# Data - second pass
data_2nd <- data_1st %>% 
  filter(stockid %in% check$stockid)

<<<<<<< HEAD
# Stock key
###########################

=======
>>>>>>> b000ee230b68cb93da265e6daf169dadc732c35a
# Final stock key
stocks <- data_2nd %>%
  summarize(nyr=n(),
            yr1=min(year),
            yr2=max(year),
            bbmsy_final=bbmsy[n()]) %>% 
  # Add type/units 
  left_join(select(stats, stockid, catch_use, catch_units, bbmsy_use), by="stockid") %>% 
  # Add stock info
  left_join(stock_key, by="stockid") %>% 
  # Rearrange columns
  select(stockid, stocklong:comm_name, nyr, yr1, yr2, catch_use, catch_units, bbmsy_use, bbmsy_final, everything()) %>% 
  # Remove stocks with high final B/BMSY - not fair to test RH-cMSY on these
  filter(bbmsy_final<=4)
  
<<<<<<< HEAD
# Get resilience
spp <- sort(unique(stocks1$species))
res <- datalimited2::resilience(spp)

# Get taxa info
taxa1 <- freeR::taxa(spp)
fam <- select(taxa1, sciname, type, family)

# Add taxa and resilience
stocks <- stocks1 %>% 
  left_join(fam, by=c("species"="sciname")) %>% 
  left_join(res, by="species") %>% 
  mutate(family=ifelse(species=="Lepidorhombus spp", "Scophthalmidae", family),
         type=ifelse(species=="Lepidorhombus spp", "fish", type))

# Any stocks missing family?
stocks$species[is.na(stocks$type)]
stocks$species[is.na(stocks$family)]

# Stocks missing resilience
sort(unique(stocks$species[is.na(stocks$resilience)]))
stocks$resilience[stocks$species=="Cervimunida johni"] <- "High" # blue squat lobster
stocks$resilience[stocks$species=="Farfantepenaeus duorarum"] <- "High" # pink shrimp
stocks$resilience[stocks$species=="Haliotis midae"] <- "Low" # perlemoen abalone
stocks$resilience[stocks$species=="Lepidorhombus spp"] <- "Low" # megrim
stocks$resilience[stocks$species=="Litopenaeus setiferus"] <- "High" # white shrimp 
stocks$resilience[stocks$species=="Pandalus borealis"] <- "High" # northern shrimp
stocks$resilience[stocks$species=="Paralithodes platypus"] <- "High" # blue king crab
stocks$resilience[stocks$species=="Pleuroncodes monodon"] <- "High" # red squat lobster
stocks$resilience[stocks$species=="Chionoecetes bairdi"] <- "Medium" # tanner crab: M=0.23,0.13-0.35 (r=0.26-0.70=)
stocks$resilience[stocks$species=="Chionoecetes opilio"] <- "Medium" # snow crab: M=0.23 (r=0.46)
stocks$resilience[stocks$species=="Haliotis iris"] <- "Low" # blackfoot paua (abalone): M<0.1 (r=0.2=Low)
stocks$resilience[stocks$species=="Homarus americanus"] <- "Medium" # American lobster: mod-high vulnerability (46/100) = mod resilience
stocks$resilience[stocks$species=="Loligo pealeii"] <- "High" # longfin inshore squid: low-mod vulnerability (26/100) = high resilience
stocks$resilience[stocks$species=="Metanephrops challengeri"] <- "Medium" # scampi: M=0.2-0.3 (r=0.4-0.6=Medium)
stocks$resilience[stocks$species=="Paralithodes camtschaticus"] <- "High" # red king crab: low vulnerability (12/100) = high resilience
stocks$resilience[stocks$species=="Placopecten magellanicus"] <- "Medium" # Atl. deep-sea scallop: mod vulnerability (45/100) = mod resilience
stocks$resilience[stocks$species=="Spisula solidissima"] <- "Medium" # Atlantic surf clam: mod vulnerability (37/100) = mod resilience
sort(unique(stocks$species[is.na(stocks$resilience)]))

=======
>>>>>>> b000ee230b68cb93da265e6daf169dadc732c35a
# Final year B/BMSY distribution
hist(stocks$bbmsy_final, breaks=seq(0,13,0.25), las=1, 
     xlim=c(0,4), ylim=c(0,40), xlab="B/BMSY", main="", col="grey60", border=F)

# Final data
<<<<<<< HEAD
###########################

# Final data
data <- data_2nd %>% 
  filter(stockid %in% stocks$stockid)

# Inspect completeness
freeR::complete(data)
freeR::complete(stocks)

data <- data_2nd %>% 
  filter(stockid %in% stocks$stockid)
=======
data <- data_2nd %>% 
  filter(stockid %in% stocks$stockid)

>>>>>>> b000ee230b68cb93da265e6daf169dadc732c35a

# Export data
################################################################################

# Export
save(data, stocks, file=file.path(datadir, "ram_test_dataset.Rdata"))

