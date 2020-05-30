
# Things to do
# 1) Update plotting of r prior to accomodate genus r and also plot resilience r
# 2) Not sure how ID_FIXED == FALSE performs right now
# 3) To ease diagnostics, make more functions have plot=T/F argument to see things (i.e., calc_r_prior(), sim_trajs(), etc(), run_rh_cmsy())


################################################################################
# RH-cMSY priors
################################################################################

# Read r priors by family
r_vals_fam <- read.csv("~/Dropbox/Chris/Rutgers/projects/mscom/data/priors/data/r_priors_by_family.csv", as.is=T)

# Source FishLife r prior code
codedir <- "code/functions"
# source(file.path(codedir, "fishlife_r_priors.R"))
source("/Users/cfree/Dropbox/Chris/Rutgers/projects/rh_cmsy/code/functions/fishlife_r_priors.R")

# R priors
calc_r_priors <- function(key){
  
  # Get FishLife r priors: species-specific and genus-specific
  r_prior_fl_spp <- r_prior(key$species) %>% 
    rename(ln_r_mu_spp=ln_r_mu, ln_r_sd_spp=ln_r_sd)
  r_prior_fl_gen <- r_prior(paste(stringr::word(key$species, 1), "spp")) %>% 
    rename(ln_r_mu_gen=ln_r_mu, ln_r_sd_gen=ln_r_sd) %>% 
    mutate(species=key$species)
  r_prior_fl <- r_prior_fl_spp %>% 
    left_join(r_prior_fl_gen, by="species")

  # Build r prior table
  r_priors <- key %>% 
    select(species, family, resilience) %>% 
    # Add FishLife r priors
    left_join(r_prior_fl, by="species") %>% 
    # Add Neubauer family-level r priors
    left_join(r_vals_fam, by="family") %>% 
    # Set resilience priors and record final prior source
    mutate(resilience=factor(resilience, levels=c("Very low", "Low", "Medium", "High")),
           r_lo=c(0.01, 0.01, 0.01, 0.50)[resilience],
           r_hi=c(0.15, 0.50, 1.00, 1.25)[resilience],
           # If resilience is NA (unknown), set wide prior
           r_lo=ifelse(is.na(resilience), 0.01, r_lo),
           r_hi=ifelse(is.na(resilience), 1.25, r_hi),
           use=ifelse(!is.na(ln_r_mu_spp), "FishLife", ifelse(!is.na(r_mu), "family", "resilience")))
  return(r_priors)
}

# K priors
calc_k_priors <- function(data){
  k_priors <- data %>% 
    group_by(stock) %>% 
    summarize(cmax=max(catch),
              # 1st try
              k_lo_try1=cmax*2,
              k_hi_try1=cmax*25,
              # 2nd try
              k_lo_try2=cmax*1,
              k_hi_try2=cmax*50,
              # 3rd try
              k_lo_try3=cmax*1,
              k_hi_try3=cmax*75,
              # 4th try
              k_lo_try4=cmax*1,
              k_hi_try4=cmax*100)
  return(k_priors)
}

# Initial saturation priors
# yrs <- 1900:2015
# s1 <- calc_sat1_priors(data=data.frame(year=yrs, stock=yrs))
# plot(x=yrs, y=s1$s1_lo, type="l", ylim=c(0,2)); lines(x=yrs, y=s1$s1_hi)
calc_sat1_priors <- function(data){
  s1_priors <- data %>% 
    group_by(stock) %>% 
    summarize(yr1=min(year),
              s1_lo=ifelse(yr1<=1945, 0.75, NA),
              s1_lo=ifelse(yr1>1945 & yr1<1980, 0.75+(0.2-0.75)/(1980-1945)*(yr1-1945), s1_lo),
              s1_lo=ifelse(yr1>=1980, 0.2, s1_lo),
              s1_hi=1)
  return(s1_priors)
}

# Final saturation priors
calc_sat2_priors_old <- function(data){
  s2_priors <- data %>% 
    group_by(stock) %>% 
    summarize(cfinal=catch[year==max(year)],
              cmax=max(catch),
              cratio=cfinal/cmax,
              s2_lo=0 + 0.2*cratio,
              s2_hi=0.5 + 0.4*cratio)
  return(s2_priors)
}

# Final saturation priors (narrower version)
calc_sat2_priors <- function(data){
  s2_priors <- data %>% 
    group_by(stock) %>% 
    summarize(cfinal=catch[year==max(year)],
              cmax=max(catch),
              cratio=cfinal/cmax,
              s2_lo=0.0 + 0.4*cratio,
              s2_hi=0.4 + 0.4*cratio)
  return(s2_priors)
}


################################################################################
# Diagnostic plots
################################################################################

# Plot viable r/k pairs
plot_rk <- function(id_rk_viable, r, k){
  par(mfrow=c(1,1))
  rmin <- pmax(0.01, floor(min(id_rk_viable$r)/0.1)*0.1)
  rmax <- ceiling(max(id_rk_viable$r)/0.1)*0.1
  plot(k/1000 ~ r, id_rk_viable, log="xy", bty="n", las=1, pch=15,
       xlim=c(rmin, rmax), ylim=unlist(k_prior[, c("k_lo", "k_hi")])/1000,
       xlab="Intrinsic growth rate, r", ylab="Carrying capacity, K", col="gray80")
  points(x=r, y=k/1000, col="red", pch=15)
}

# Plot viable biomass trajectories
plot_b <- function(b_mat_viable, yrs){
  par(mfrow=c(1,1))
  yr1 <- floor(min(yrs) / 10) * 10
  yr2 <- ceiling(max(yrs) / 10) * 10
  plot(b_mat_viable[,1]/1000 ~ yrs, type="n", bty="n", las=2, xlim=c(yr1, yr2),
       ylim=c(0, max(b_mat_viable/1000, na.rm=T)), xlab="Year", ylab="Biomass (1000s)")
  for(k in 1:ncol(b_mat_viable)){lines(x=yrs, y=b_mat_viable[,k]/1000, col="grey80")}
}

# Plot viable B/BMSY trajectories
plot_bbmsy <- function(bbmsy_mat_viable, bbmsy_est, yrs){
  par(mfrow=c(1,1))
  yr1 <- floor(min(yrs) / 10) * 10
  yr2 <- ceiling(max(yrs) / 10) * 10
  plot(bbmsy_mat_viable[,1] ~ yrs, type="n", bty="n", las=2, xlim=c(yr1, yr2),
       ylim=c(0, max(bbmsy_mat_viable, na.rm=T)), xlab="Year", ylab=expression("B/B"["MSY"]))
  for(k in 1:ncol(bbmsy_mat_viable)){lines(x=yrs, y=bbmsy_mat_viable[,k], col="grey80")}
  lines(x=yrs, y=bbmsy_est, col="red")
  lines(x=c(yr1, yr2), y=c(0.5, 0.5), lty=3)
}

# Plot viable exploitation trajectories
plot_er <- function(er_mat_viable, yrs){
  par(mfrow=c(1,1))
  yr1 <- floor(min(yrs) / 10) * 10
  yr2 <- ceiling(max(yrs) / 10) * 10
  plot(er_mat_viable[,1] ~ yrs, type="n", bty="n", las=2, xlim=c(yr1, yr2),
       ylim=c(0, max(er_mat_viable)), xlab="Year", ylab="Exploitation rate")
  for(k in 1:ncol(er_mat_viable)){lines(x=yrs, y=er_mat_viable[,k], col="grey80")}
}

# Plot viable U/UMSY trajectories
plot_uumsy <- function(uumsy_mat_viable, uumsy_est, yrs){
  par(mfrow=c(1,1))
  yr1 <- floor(min(yrs) / 10) * 10
  yr2 <- ceiling(max(yrs) / 10) * 10
  plot(uumsy_mat_viable[,1] ~ yrs, type="n", bty="n", las=2, xlim=c(yr1, yr2),
       ylim=c(0, max(uumsy_mat_viable, na.rm=T)), xlab="Year", ylab=expression("U/U"["MSY"]))
  for(k in 1:ncol(uumsy_mat_viable)){lines(x=yrs, y=uumsy_mat_viable[,k], col="grey80")}
  lines(x=yrs, y=uumsy_est, col="red")
  lines(x=c(yr1, yr2), y=c(1, 1), lty=3)
}

################################################################################
# Helper functions
################################################################################

# Look up families
get_family <- function(species){
  
  # FB/SLB taxa key
  taxa_key_fb <- rfishbase::load_taxa(server="https://fishbase.ropensci.org") %>% mutate(type="fish") %>% select(type, everything())
  taxa_key_slb <- rfishbase::sealifebase %>% mutate(type="invert") %>% select(type, everything())
  taxa_key <-  taxa_key_fb %>%
    bind_rows(taxa_key_slb) %>%
    setNames(tolower(names(.))) %>%
    mutate(sciname=paste(genus, species)) %>%
    select(type, class, order, family, genus, species, sciname) %>%
    unique()
  
  # Build family key
  spp <- unique(species)
  family_key <- taxa_key %>%
    filter(sciname %in% spp) %>% 
    select(sciname, family) %>% 
    rename(species=sciname)
  return(family_key)
  
}

################################################################################
# Simulate biomass trajectories
################################################################################

# Attempts using species r
# Try 1: default k (2-25 * cmax), default r, npairs
# Try 2: wider k (1-50 * cmax), default r, npairs * 2

# Attempts using genus based r
# Try 3: wider k (1-50 * cmax), wider r (genus), npairs * 3
# Try 4: wider k (1-75 * cmax), wider r (genus), npairs * 4
# Try 5: wider k (1-100 * cmax), wider r (genus), npairs * 5

# Attempts using resilience based r
# Try 6: wider k (1-50 * cmax), wider r (resilience), npairs * 3
# Try 7: wider k (1-75 * cmax), wider r (resilience), npairs * 4
# Try 8: wider k (1-100 * cmax), wider r (resilience), npairs * 5

# Simulate biomass
sim_trajs <- function(key, c_vec, nyrs, yrs, r_prior, k_prior, s1_prior, s2_prior, npairs, try){
  
  # Get initial depletions (IDs) to evaluate
  id_fixed <- key$id_fixed
  if(id_fixed==T){
    ids <- 1
  }else{
    ids <- seq(s1_prior$s1_lo, s1_prior$s1_hi, length.out=5)
  }
  
  # Number of pairs (depends on attempt number)
  npairs_do <- npairs * try
  
  # R values to evaluate 
  ##############################
  
  # Method
  r_prior_method <- r_prior$use
  
  # FishLife-based r priors (speciess-specific)
  # Used when method == FishLife and try <= 2
  if(r_prior_method=="FishLife" & try<=2){
    rs <- rlnorm(npairs_do, meanlog=r_prior$ln_r_mu_spp, sdlog=r_prior$ln_r_sd_spp)
  }
  
  # FishLife-based r priors (genus-specific)
  # Used when method == FishLife and try > 2 but <= 5
  if(r_prior_method=="FishLife" & try>2 & try <= 5){
    rs <- rlnorm(npairs_do, meanlog=r_prior$ln_r_mu_gen, sdlog=r_prior$ln_r_sd_gen)
  }
  
  # Family-based r priors
  # Used when method == family and try <= 5
  if(r_prior_method=="family" & try <= 5){
    rs <- rlnorm(npairs_do, meanlog=log(r_prior$r_mu), sdlog=r_prior$r_sd)
  }
  
  # Resilience-based r priors
  # Used when method == resilience or when try > 5
  if(r_prior_method=="resilience" | try > 5){
    rs <- runif(npairs_do, r_prior$r_lo, r_prior$r_hi)
  }
  
  # K values to evaluate 
  ##############################
  
  # K values to evaluate 
  # (depends on attempt number)
  if(try==1){ks <- runif(npairs_do, k_prior$k_lo_try1, k_prior$k_hi_try1)} # Default K [2-25] and default r (species)
  if(try==2){ks <- runif(npairs_do, k_prior$k_lo_try2, k_prior$k_hi_try2)} # Widen K [1-50] and default r (species)
  if(try==3){ks <- runif(npairs_do, k_prior$k_lo_try2, k_prior$k_hi_try2)} # Widen K [1-50] and widen r (genus)
  if(try==4){ks <- runif(npairs_do, k_prior$k_lo_try3, k_prior$k_hi_try3)} # Widen K [1-75] and widen r (genus)
  if(try==5){ks <- runif(npairs_do, k_prior$k_lo_try4, k_prior$k_hi_try4)} # Widen K [1-100] and widen r (genus)
  if(try==6){ks <- runif(npairs_do, k_prior$k_lo_try2, k_prior$k_hi_try2)} # Widen K [1-50] and widen r (resilience)
  if(try==7){ks <- runif(npairs_do, k_prior$k_lo_try3, k_prior$k_hi_try3)} # Widen K [1-75] and widen r (resilience)
  if(try==8){ks <- runif(npairs_do, k_prior$k_lo_try4, k_prior$k_hi_try4)} # Widen K [1-100] and widen r (resilience)
  
  # r/K/ID combos
  ##############################
  
  # Build r/k pairs to evaluate
  rk_pairs <- cbind(r=rs, k=ks, viable=rep(NA, npairs_do))
  
  # Build ID/r/k combos to evaluate
  id_rk_combos <- as.data.frame(do.call("rbind",
                                        lapply(ids, function(x) cbind(id=rep(x, nrow(rk_pairs)), rk_pairs))))
  
  # Remove combos with r greater than threshold
  r_threshhold <- 1.25
  id_rk_combos <- filter(id_rk_combos, r <= r_threshhold)
  
  # Simulate trajectories
  ##############################
  
  # Loop through ID/r/k combos to see if they produce viable biomass trajectories
  p <- 0.2 # Pella-Tomlinson shape parameter (max growth @ 40% of carrying capacity)
  sigmaP <- 0.0 # process error on productivity term
  b_mat <- matrix(data=NA, nrow=nyrs, ncol=nrow(id_rk_combos), dimnames=list(yrs, 1:nrow(id_rk_combos)))
  for(j in 1:nrow(id_rk_combos)){
    id <- id_rk_combos$id[j]
    r <- id_rk_combos$r[j]
    k <- id_rk_combos$k[j]
    b_mat[1,j] <- k * id
    for(yr in 2:nyrs){
      b_mat[yr,j] <- b_mat[yr-1,j] +  r*b_mat[yr-1,j]/p*(1-(b_mat[yr-1,j]/k)^p)*exp(rnorm(1,0,sigmaP)) - c_vec[yr-1]
    }
  }
  
  # Create saturation matrix
  s_mat <- t(t(b_mat) / id_rk_combos$k)
  
  # Reduce to viable r/k pairs and trajectories
  check0 <- apply(b_mat, 2, function(x) sum(x<0, na.rm=T)==0) # check B doesn't go below 0
  s2_lo <- s2_prior$s2_lo
  s2_hi <- s2_prior$s2_hi
  s2_vec <- s_mat[nrow(s_mat),]
  checkS <- s2_vec > s2_lo & s2_vec < s2_hi & !is.na(s2_vec) # check final yr saturation inside prior
  viable <- check0 & checkS # merge positive biomass and final saturation checks
  id_rk_combos[,"viable"] <- viable
  nviable <- sum(viable)
  b_mat_viable <- b_mat[,viable]
  id_rk_viable <- id_rk_combos[viable,]
  
  # Output
  output <- list(try=try,
                 nviable=nviable, 
                 b_mat_viable=b_mat_viable,
                 id_rk_viable=id_rk_viable)
  return(output)
  
}


################################################################################
# Fit RH-cMSY
################################################################################

# Robin-Hood cMSY
# data <- sdata; key <- key; npairs <- 10000
rh_cmsy <- function(data, key, npairs=10000){
  
  # Get info
  ###################################################
  
  # Get info
  stock <- unique(data$stock)
  yrs <- data$year
  nyrs <- length(yrs)
  c_vec <- data$catch
  
  # Check data inputs
  ###################################################
  
  # Data must contain the following columns: stock, year, catch
  if( any(!c("stock", "year", "catch")%in%colnames(data))){
    stop("The 'data' object must include the following column names: stock, year, catch")}
  
  # Key must contain the following columns: stock, family, resilience, id_fixed
  if( any(!c("species", "family", "resilience", "id_fixed")%in%colnames(key))){
    stop("The 'key' object must include the following column names: species, family, resilience, id_fixed")}
  
  # Check resilience values
  if(any(!key$resilience%in%c("Very low", "Low", "Medium", "High", NA))){
    stop("Resilience values must be one of the following: Very low, Low, Medium, High, NA")}
  
  # Catch time series must be continuous (i.e., no NAs between 1st and last year)
  cstats <- data %>% 
    summarize(yr1=min(year), 
              yr2=max(year),
              nyr=n(),
              nyr_check=length(yr1:yr2),
              pass=nyr==nyr_check)
  if(cstats$pass==F){
    stop("The stock has a non-continuous catch time series (i.e, NAs in catch data).")
  }
  
  # Calculate priors
  ###################################################
  
  # Calculate r and k priors (including values for 2nd, 3rd, and 4th attempts)
  r_prior <- calc_r_priors(key)
  k_prior <- calc_k_priors(data)
  
  # Calculate saturation priors
  s1_prior <- calc_sat1_priors(data)
  s2_prior <- calc_sat2_priors(data)
  
  # Simulate biomass trajectories
  ###################################################
  
  # Number of viable trajectories required
  n_viable_req <- 10 
  
  # Simulate biomass trajectories
  trajs <- sim_trajs(key, c_vec, nyrs, yrs, r_prior, k_prior, s1_prior, s2_prior, npairs, try=1)
  if(trajs$nviable < n_viable_req){trajs <- sim_trajs(key, c_vec, nyrs, yrs, r_prior, k_prior, s1_prior, s2_prior, npairs, try=2)}
  if(trajs$nviable < n_viable_req){trajs <- sim_trajs(key, c_vec, nyrs, yrs, r_prior, k_prior, s1_prior, s2_prior, npairs, try=3)}
  if(trajs$nviable < n_viable_req){trajs <- sim_trajs(key, c_vec, nyrs, yrs, r_prior, k_prior, s1_prior, s2_prior, npairs, try=4)}
  if(trajs$nviable < n_viable_req){trajs <- sim_trajs(key, c_vec, nyrs, yrs, r_prior, k_prior, s1_prior, s2_prior, npairs, try=5)}
  if(trajs$nviable < n_viable_req){trajs <- sim_trajs(key, c_vec, nyrs, yrs, r_prior, k_prior, s1_prior, s2_prior, npairs, try=6)}
  if(trajs$nviable < n_viable_req){trajs <- sim_trajs(key, c_vec, nyrs, yrs, r_prior, k_prior, s1_prior, s2_prior, npairs, try=7)}
  if(trajs$nviable < n_viable_req){trajs <- sim_trajs(key, c_vec, nyrs, yrs, r_prior, k_prior, s1_prior, s2_prior, npairs, try=8)}
  
  # Extract results and check number of viables
  try <- trajs$try
  nviable <- trajs$nviable
  id_rk_viable <- trajs$id_rk_viable
  b_mat_viable <- trajs$b_mat_viable
  if(nviable < n_viable_req){stop(paste("Fewer than", n_viable_req, "viable trajectories found."))}
  
  # Point estimate (estimates r/k)
  #############################################
  
  # Calculate density of r/K values
  dens <- MASS::kde2d(x=log(id_rk_viable$r),
                      y=log(id_rk_viable$k), n=c(100,100))
  
  # r/K combos with maximum density
  rk_log_max <- which(dens$z==max(dens$z), arr.ind = TRUE)
  r_est <- as.numeric(exp(dens$x[rk_log_max[1]]))
  k_est <- as.numeric(exp(dens$y[rk_log_max[2]]))
  id_rk_ests <- data.frame(id=1, r=r_est, k=k_est)
  
  # Plot density of r/K values
  # image(x=exp(dens$x), y=exp(dens$y)/1000, z=dens$z,
  #       xlab="Intrinsic growth rate, r", ylab="Carrying capacity, K")
  # points(x=r_est, y=k_est/1000, pch=15, cex=1.2)

  # Loop through ID/r/k combos to see if they produce viable biomass trajectories
  p <- 0.2
  b_est <- rep(NA, length=nyrs)
  b_est[1] <- k_est
  for(i in 2:nyrs){
    b_est[i] <- b_est[i-1] +  (r_est / p) * b_est[i-1] * (1 - (b_est[i-1]/k_est)^p) - c_vec[i-1]
  }
  
  # Check biomass estimates
  # plot(b_est/1000 ~ yrs, xlab="", ylab="Biomass", bty="n", type="l", las=1)
  # lines(x=yrs, y=rep(k_est/1000, nyrs), lty=2)
  
  # Derive B/BMSY
  bmsy_est <- k_est * (1 / (p+1))^(1/p)
  bbmsy_est <- b_est / bmsy_est
  
  # Derive U and U/UMSY
  u_est <- c_vec / b_est
  umsy_est <- r_est / p * (1-1/(p+1))
  uumsy_est <- u_est / umsy_est
  
  # Merge predictions
  preds_pt <- data.frame(stock=stock, year=yrs, catch=c_vec,
                         b=b_est, 
                         er=u_est, 
                         bbmsy=bbmsy_est, 
                         uumsy=uumsy_est, stringsAsFactors=F)

  
  # Global estimate (doesn't estimate r/k)
  #############################################
  
  # Derive B/BMSY
  bmsy <- id_rk_viable[,"k"] * (1 / (p+1))^(1/p)
  bbmsy_mat_viable <- t(t(b_mat_viable) / bmsy)
  
  # Calculate exploitation rate
  er_mat_viable <- c_vec / b_mat_viable
  
  # Derive U/UMSY
  umsy <- id_rk_viable[,"r"] / p * (1-1/(p+1))
  uumsy_mat_viable <- t(t(er_mat_viable) / umsy)
  
  # Diagnostic plots
  # plot_rk(id_rk_viable, r_est, k_est)
  # plot_b(b_mat_viable, yrs)
  # plot_bbmsy(bbmsy_mat_viable, bbmsy_est, yrs)
  # plot_er(er_mat_viable, yrs)
  # plot_uumsy(uumsy_mat_viable, yrs)
  
  # Calculate final predictions
  b <- t(apply(b_mat_viable, 1, function(x) quantile(x, probs=c(0.5, 0.025, 0.975))))
  er <- t(apply(er_mat_viable, 1, function(x) quantile(x, probs=c(0.5, 0.025, 0.975))))
  bbmsy <- t(apply(bbmsy_mat_viable, 1, function(x) quantile(x, probs=c(0.5, 0.025, 0.975))))
  uumsy <- t(apply(uumsy_mat_viable, 1, function(x) quantile(x, probs=c(0.5, 0.025, 0.975))))
    
  # Assign column names to final predictions
  colnames(b) <- c("b", "b_lo", "b_hi")
  colnames(er) <- c("er", "er_lo", "er_hi")
  colnames(bbmsy) <- c("bbmsy", "bbmsy_lo", "bbmsy_hi")
  colnames(uumsy) <- c("uumsy", "uumsy_lo", "uumsy_hi")

  # Combine predictions
  preds <- data.frame(stock=stock, year=yrs, catch=c_vec,
                        b, er, bbmsy, uumsy, stringsAsFactors=F)
  
  # Things to return
  out <- list(key=key,
              data=data,
              # Predictions
              preds=preds,
              preds_pt=preds_pt,
              id_rk_ests=id_rk_ests,
              # Priors
              r_prior=r_prior,
              k_prior=k_prior, 
              s1_prior=s1_prior,
              s2_prior=s2_prior,
              # Viable trajectories
              id_rk_v=id_rk_viable,
              b_v=b_mat_viable, 
              er_v=er_mat_viable,
              bbmsy_v=bbmsy_mat_viable,
              uumsy_v=uumsy_mat_viable)
  
  # Return
  return(out)
  
}


################################################################################
# Plot RH-cMSY
################################################################################

# Plot RH-cMSY results
# output <- out
plot_rh_cmsy <- function(output, true, savedir){
  
  # Extract info
  key <- output$key
  preds <- output$preds
  preds_pt <- output$preds_pt
  id_rk_ests <- output$id_rk_ests
  stock <- unique(output$preds$stock)
  
  # If savedir is provided, save figure
  if(!missing(savedir)){
    figname <- paste0(stock, ".png")
    png(file.path(savedir, figname), width=6.5, height=4.5, units="in", res=600)
  }
  
  # Loop through species
  par(mfrow=c(2,3), mar=c(3,2,2,1), oma=c(0,2,0,0), mgp=c(2,0.7,0), xpd=NA)
    
  # Year info
  #########################################
  
  # Year info
  yrs <- preds$year
  yr1 <- floor(min(yrs) / 10) * 10
  yr2 <- ceiling(max(yrs) / 10) * 10
  
  # Plot catch
  #########################################
  
  # Plot catch
  cmax <- max(preds$catch)
  cmax_yr <- min(preds$year[preds$catch==cmax])
  preds$catch_sd <- preds$catch/cmax
  plot(catch_sd ~ year, preds, type="l", las=2, bty="n", xaxt="n", cex.axis=0.9,
       xlim=c(yr1, yr2), ylim=c(0, 1), xlab="", ylab="Catch (scaled)", main="")
  points(x=cmax_yr, y=1, pch=16, cex=1.2)
  axis(side=1, at=seq(yr1, yr2, 10), las=2, cex.axis=0.9)
  title(stock, adj=0.05)
  
  # Plot r/k pairs
  #########################################
  
  # Extract viable r/k pairs
  id_rk_v <- output$id_rk_v
  # r_prior <- unlist(select(filter(output$r_priors, stock==stock_i), r_lo, r_hi))
  # k_prior <- unlist(select(filter(output$k_priors, stock==stock_i), k_lo, k_hi))
  
  # Plot viable r/k pairs
  # Potentially reduce this to unique r/k pairs
  # There could be redundancy when evaluating multiple IDs
  rmin <- pmax(0.01, floor(min(id_rk_v$r) / 0.1) * 0.1)
  rmax <- ceiling(max(id_rk_v$r) / 0.1) * 0.1
  kmin <- min(id_rk_v$k)/cmax 
  kmax <- max(id_rk_v$k)/cmax
  plot(k/cmax ~ r, id_rk_v, bty="n", las=1, pch=15, col="gray70", log="xy", 
       xlim=c(rmin, rmax), ylim=c(2,25), cex.axis=0.9, yaxt="n",
       xlab="Intrinsic growth rate, r", ylab="Carrying capacity, K (scaled)")
  points(x=id_rk_ests$r, y=id_rk_ests$k/cmax, pch=15, col="red", cex=1.2)
  axis(2, at=c(2,5,10,25), las=1, cex.axis=0.9)
  
  # Add number of viable pairs
  nviable <- nrow(id_rk_v)
  text(x=rmin, y=2.1, pos=4, labels=paste0(nviable, " viable"), cex=0.9)
  
  # Add truth if available
  if(!missing(true)){
    points(x=true$rk$r, y=true$rk$k/cmax, col="red", pch=15, cex=1.2)
  }
  
  # Plot r prior
  #########################################
  
  # Plot r prior
  r_prior_use <- output$r_prior$use
  r_threshhold <- 1.25
  # Plot FishLife r prior
  if(r_prior_use=="FishLife"){
    ln_r_mu <- output$r_prior$ln_r_mu_spp
    ln_r_sd <- output$r_prior$ln_r_sd_spp
    r_lo_plot <- qlnorm(0.001, meanlog=ln_r_mu, sdlog=ln_r_sd)
    r_hi_plot <- pmax(r_threshhold, qlnorm(0.999, meanlog=ln_r_mu, sdlog=ln_r_sd))
    x <- seq(r_lo_plot, r_hi_plot, length.out=1000)
    hx <- dlnorm(x, meanlog=ln_r_mu, sdlog=ln_r_sd)
    plot(x=x, y=hx, type="n", yaxt="n", frame=F, 
         xlim=c(0, r_threshhold), ylim=c(0,max(hx)), cex.axis=0.9,
         xlab="Prior for intrinsic growth rate, r", ylab="")
    polygon(x=c(x, rev(x)), y=c(rep(0,length(x)), rev(hx)), col="grey80", border=F)
    lines(x=rep(id_rk_ests$r,2), y=c(0, max(hx)), col="red", lty=3)
    text(x=r_threshhold, y=max(hx), pos=2, label="FishLife", cex=0.9)
  }
  # Plot family r prior
  if(r_prior_use=="family"){
    freeR::emptyplot()
  }
  # Plot resilience r prior
  if(r_prior_use=="resilience"){
    res <- as.character(output$key$resilience)
    r_lo <- output$r_prior$r_lo
    r_hi <- output$r_prior$r_hi
    plot(x=c(r_lo, r_hi), y=c(0,0), type="l", bty="n", 
         lwd=1.5,
         xlim=c(0, r_threshhold), ylim=c(0, 10), yaxt="n", 
         xlab="Prior for intrinsic growth rate, r", ylab="")
    text(x=r_threshhold, y=10, pos=2, label=paste(res, "resilience"), cex=0.9)
  }
  
    # Plot B/BMSY trajectories
  #########################################
  
  # Extract B/BMSY trajectories
  bbmsy_v <- output$bbmsy_v

  # Plot B/BMSY trajectories
  if(!missing(true)){
    ymax <- ceiling(max(bbmsy_v, true$ts$bbmsy, na.rm=T)/0.5) * 0.5
  }else{
    ymax <- ceiling(max(bbmsy_v, na.rm=T)/0.5) * 0.5
  }
  plot(bbmsy_v[,1] ~ yrs, type="n", bty="n", las=2, xaxt="n", cex.axis=0.9,
       xlim=c(yr1, yr2), ylim=c(0, ymax), xlab="", ylab=expression("B/B"["MSY"]))
  axis(side=1, at=seq(yr1, yr2, 10), las=2, cex.axis=0.9)
  # Viable trajectories
  for(k in 1:ncol(bbmsy_v)){lines(x=yrs, y=bbmsy_v[,k], col="grey80")}
  # RH-cMSY trajectory
  lines(x=preds$year, y=preds$bbmsy, lwd=1.2, col="black")
  lines(x=preds_pt$year, y=preds_pt$bbmsy, lwd=1.2, col="red")
  # Overfishing line
  lines(x=c(yr1, yr2), y=c(0.5, 0.5), lty=3)
  lines(x=c(yr1, yr2), y=c(1, 1), lty=2)
  
  # Add truth if available
  if(!missing(true)){
    lines(x=true$ts$year, y=true$ts$bbmsy, col="red", lwd=1.3)
  }
  
  # Plot U/UMSY trajectories
  #########################################
  
  # Extract U/UMSY trajectories
  uumsy_v <- output$uumsy_v

  # Plot U/UMSY trajectories
  if(!missing(true)){
    ymax <- ceiling(max(uumsy_v, true$ts$uumsy, true$ts$ffmsy, na.rm=T)/0.5) * 0.5
  }else{
    ymax <- ceiling(max(uumsy_v, na.rm=T)/0.5) * 0.5
  }
  plot(uumsy_v[,1] ~ yrs, type="n", bty="n", las=2, xaxt="n", cex.axis=0.9,
       xlim=c(yr1, yr2), ylim=c(0, ymax), xlab="", ylab=expression("U/U"["MSY"]))
  axis(side=1, at=seq(yr1, yr2, 10), las=2, cex.axis=0.9)
  # Viable trajectories
  for(k in 1:ncol(uumsy_v)){lines(x=yrs, y=uumsy_v[,k], col="grey80")}
  # RH-cMSY trajectory
  lines(x=preds$year, y=preds$uumsy, lwd=1.2, col="black")
  lines(x=preds_pt$year, y=preds_pt$uumsy, lwd=1.2, col="red")
  
  
  # Add truth if available
  if(!missing(true)){
    lines(x=true$ts$year, y=true$ts$uumsy, col="red", lwd=1.3)
    lines(x=true$ts$year, y=true$ts$ffmsy, col="red", lwd=1.3, lty=3)
  }
  
  # Plot Kobe plot
  #########################################
  
  # Setup plot
  xmax <- pmax(1.5, ceiling(max(preds$bbmsy) / 0.5) * 0.5)
  ymax <- pmax(1.5, ceiling(max(preds$uumsy) / 0.5) * 0.5)
  plot(uumsy ~ bbmsy, preds, bty="n", type="l", las=1,
       xlim=c(0,xmax), ylim=c(0,ymax), main="", cex.axis=0.9,
       xlab=expression("B/B"["MSY"]), ylab=expression("U/U"["MSY"]))
  lines(x=c(0, xmax), y=c(1,1), lty=2)
  lines(x=c(1, 1), y=c(0,ymax), lty=2)
  # Add start/end points
  points(x=preds$bbmsy[1], y=preds$uumsy[1], pch=22, cex=1.4, bg="white")
  points(x=preds$bbmsy[nrow(preds)], y=preds$uumsy[nrow(preds)], pch=22, cex=1.4, bg="grey50")
  # Add points legend
  legend("topright", bty="n", legend=c(min(yrs), max(yrs)), pch=22, pt.bg=c("white", "grey50"), pt.cex=1.4)
  
  # Off
  #########################################
  
  # If savedir is provided, save figure
  if(!missing(savedir)){dev.off()}
    
}
