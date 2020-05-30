# Robin Hood cMSY: using catch data and borrowed information to assess data-poor fish stocks

This GitHub repository documents the development and testing of the Robin Hood cMSY (RH-cMSY) catch-only stock assessment method and provides functions for implementing this method on other stocks.

Robin-Hood cMSY (RH-cMSY) is based on the cMSY (Froese et al. 2017) stock reduction analysis but uses information from data-rich stocks to set priors for data-poor stock parameters. In general, both methods reconstruct historical abundance and exploitation rates by simulating biomass trajectories that could feasibly produce the observed catch time series given assumptions about initial and final year depletion and population parameters such as carrying capacity, K and intrinsic growth rate, r. RH-cMSY primarily differs from cMSY in that it uses meta-analyses of data-rich stocks in the RAM Legacy Stock Assessment Database (Ricard et al. 2012) to set priors for all four values. Furthermore, it uses a Pella-Tomlinson rather than a Schaefer surplus production model to account for asymmetry in production (Thorson et al. 2012) and does not use the “tip of the triangle” assumption employed in cMSY.

We developed priors for the intrinsic growth rate, r, using the following hierarchy of best available information:

1. **All finfish species** – Lognormally distributed r priors were derived from FishLife 2.0 (Thorson in review), which uses a multivariate model fit to FishBase (Froese and Pauly 2018) and the original RAM stock-recruit database (Myers et al. 1995) to predict life history traits and productivity parameters, including r, for >32,000 fish species. We specify the prior using predictions for either the target species or the lowest available phylogenetic group (i.e., genus, family, order, or class of the target species).
2. **Spiny lobsters (Palinuridae)** – Lognormally distributed r priors were derived using the meta-analysis of Neubauer et al. (2013), which estimated family-level r means and uncertainties for species in the RAM Legacy Database (Figure S1).
3. **All other invertebrate species** – Uniformly distributed r priors were derived from resilience – the ability for populations to recover following perturbance – on the premise that higher resilience implies higher r and lower resilience implies lower r. These priors (Figure 2) were specified after comparing FishBase resilience estimates (i.e., very low, low, medium, or high) for RAM Legacy Database stocks with estimates of r from surplus production model fits to the same stocks (Free et al. in review b). 

We developed priors for other model parameters based on the dynamics of data-rich stocks in the RAM Legacy Database (Figure 2):

1.	**Carrying capacity, K, priors** are based on the maximum catch on the premise that K must be greater than the maximum catch and is likely to represent a reasonable fraction of carrying capacity in a developed fishery.
2.	**Initial saturation priors** are based on the length of the reported catch time series on the premise that longer time series begin in a state of higher saturation (lower depletion) and shorter time series begin in a state of lower saturation (higher depletion).
3.	**Final saturation priors** are based on the ratio of the catch in the final year to the maximum catch on the premise that a low ratio is indicative of an overexploited fishery (low saturation) and a high ratio is indicative of a developing or fully developed fishery (higher saturation).


# Citation

Free, C.M. (2020) Robin Hood cMSY: using catch data and borrowed information to assess data-poor fish stocks. Available at: https://github.com/cfree14/rh_cmsy/


# References





