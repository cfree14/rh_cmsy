

# Build key for all FB/SLB taxa

# FishBase
taxa_key_fb <- rfishbase::load_taxa(server="https://fishbase.ropensci.org") %>%
  mutate(type="fish") %>%
  select(type, everything()) %>%
  setNames(tolower(colnames(.))) %>%
  rename(sciname=species) %>%
  mutate(species=stringr::word(sciname, start=2, end=sapply(strsplit(sciname, " "), length)))

# SeaLifeBase
taxa_key_slb <- rfishbase::sealifebase %>%
  mutate(type="invert") %>%
  select(type, everything()) %>%
  setNames(tolower(colnames(.))) %>%
  mutate(sciname=paste(genus, species))

# Merged
taxa_key <-  taxa_key_fb %>%
  bind_rows(taxa_key_slb) %>%
  setNames(tolower(names(.))) %>%
  select(type, class, order, family, genus, species, sciname) %>%
  unique()

# Look up resilience values
fb_slb_res <- datalimited2::resilience(taxa_key$sciname)

# Add resilience to taxa
fb_spp_res <- taxa_key %>% 
  left_join(fb_slb_res, by=c("sciname"="species"))

# Identify mode by family
fb_fam_res <- fb_spp_res %>% 
  filter(!is.na(resilience)) %>% 
  group_by(type, class, order, family, resilience) %>% 
  summarize(n=n()) %>% 
  spread(key="resilience", value="n")
