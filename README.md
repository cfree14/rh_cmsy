# Robin Hood cMSY: using catch data and borrowed information to assess data-poor fish stocks

## Introduction

This GitHub repository documents the development and testing of the Robin Hood cMSY (RH-cMSY) catch-only stock assessment method and provides functions for implementing this method on other stocks.

Robin-Hood cMSY (RH-cMSY) is based on the cMSY (Froese et al. 2017) stock reduction analysis but uses information from data-rich stocks to set priors for data-poor stock parameters. In general, both methods reconstruct historical abundance and exploitation rates by simulating biomass trajectories that could feasibly produce the observed catch time series given assumptions about initial and final year depletion and population parameters such as carrying capacity, K and intrinsic growth rate, r. RH-cMSY primarily differs from cMSY in that it uses meta-analyses of data-rich stocks in the RAM Legacy Stock Assessment Database (Ricard et al. 2012) to set priors for all four values. Furthermore, it uses a Pella-Tomlinson rather than a Schaefer surplus production model to account for asymmetry in production (Thorson et al. 2012) and does not use the “tip of the triangle” assumption employed in cMSY.

## Priors used by RH-cMSY

We developed priors for the intrinsic growth rate, r, using the following hierarchy of best available information:

1. **All finfish species** – Lognormally distributed r priors were derived from FishLife 2.0 (Thorson in review), which uses a multivariate model fit to FishBase (Froese and Pauly 2018) and the original RAM stock-recruit database (Myers et al. 1995) to predict life history traits and productivity parameters, including r, for >32,000 fish species. We specify the prior using predictions for either the target species or the lowest available phylogenetic group (i.e., genus, family, order, or class of the target species).
2. **Spiny lobsters (Palinuridae)** – Lognormally distributed r priors were derived using the meta-analysis of Neubauer et al. (2013), which estimated family-level r means and uncertainties for species in the RAM Legacy Database.
3. **All other invertebrate species** – Uniformly distributed r priors were derived from resilience – the ability for populations to recover following perturbance – on the premise that higher resilience implies higher r and lower resilience implies lower r. These priors were specified after comparing FishBase resilience estimates (i.e., very low, low, medium, or high) for RAM Legacy Database stocks with estimates of r from surplus production model fits to the same stocks (Free et al. in review b). 

We developed priors for other model parameters based on the dynamics of data-rich stocks in the RAM Legacy Database:

1.	**Carrying capacity, K, priors** are based on the maximum catch on the premise that K must be greater than the maximum catch and is likely to represent a reasonable fraction of carrying capacity in a developed fishery.
2.	**Initial saturation priors** are based on the length of the reported catch time series on the premise that longer time series begin in a state of higher saturation (lower depletion) and shorter time series begin in a state of lower saturation (higher depletion).
3.	**Final saturation priors** are based on the ratio of the catch in the final year to the maximum catch on the premise that a low ratio is indicative of an overexploited fishery (low saturation) and a high ratio is indicative of a developing or fully developed fishery (higher saturation).


##  Citation

Free, C.M., Jensen, O.P. (2020) Robin Hood cMSY: using catch data and borrowed information to assess data-poor fish stocks. Available at: https://github.com/cfree14/rh_cmsy/


## Key references


Free, C.M., Thorson, J.T., Pinsky, M.L., Oken, K.L., Wiedenmann, J., Jensen, O.P. (2019) Impacts of historical warming on marine fisheries production. Science 363(6430): 979-983.

Froese, R., Demirel, N., Coro, G., Kleisner, K.M., Winker, H. (2017) Estimating fisheries reference points from catch and resilience. Fish and Fisheries 18(3): 506–526. 

Froese, R., Pauly, D. (2018) FishBase. Available at: www.fishbase.org.

Martell, S., Froese, R. (2013) A simple method for estimating MSY from catch and resilience. Fish and Fisheries 14: 504–514.

Myers, R. A., Bridson, J., Barrowman, N. J. (1995) Summary of worldwide spawner and recruitment data. 2024. St. Johns, Newfoundland: Department of Fisheries and Oceans Canada, Northwest Atlantic Fisheries Centre.

Neubauer, P., Jensen, O.P., Hutchings, J.A., Baum, J.K. (2013) Resilience and recovery of overexploited marine populations. Science 340(6130: 347-349.

Ricard, D., C. Minto, O.P. Jensen, Baum, J.K. (2012) Examining the knowledge base and status of commercially exploited marine species with the RAM Legacy Stock Assessment Database. Fish and Fisheries 13(4): 380–398.

Thorson, J.T. (2019) Predicting recruitment density dependence and intrinsic growth rate for all fishes worldwide using a data‐integrated life‐history model. Fish and Fisheries.

Thorson, J.T., Munch, S.B., Cope, J.M., Gao, J. (2017) Predicting life history parameters for all fishes worldwide. Ecological Applications 27(8): 2262-2276.
