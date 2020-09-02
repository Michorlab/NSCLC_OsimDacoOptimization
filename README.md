# Osimertinib and Dacomitinib Combination Schedule Optimization in EGFR-m NSCLC

Updated: 09/01/20

## Description of repository

### Code
All essential code for manuscript will be found in this directory. `Cannon/` has the scripts that were sent to the cluster Odyssey (now called Cannon) to run the *in silico* clinical trials. Each R script contains a description at the top.

#### Estimation of growth rates
Data from cell growth assays (drug combo of osimertinib + dacomitinib) used to populate tumor growth/evolution model can be found in the folders:

	- `Data from Aaron/`: (from Hata lab): 3-day assays for PC9 parental/derivative cell lines, including C797S which is not available in Pfizer. 
	- `Data from Scott/`: Pfizer; 3-day assays for PC9 parental/derivative cell lines and HCC827 parental and MET-amp cell lines. The popPK model of dacomitinib can be found in this folder under `Dacomitinib`.

The scripts `Code/predictGrowthRates_*.R` estimate the growth rates for each cell type using the data listed above. Scripts with the subscript `*_AH.R` use Aaron Hata's cell lines. The file `Code/saveAllFitsAndRates.R` runs all `predictGrowthRates_*.R` scripts and saves estimated paremeters and fitted models in an RData format. 

##### HC827 cell line

#### Drug simulations

#### *In silico* clinical trials

#####

#####

#### Validation experiments

### Results
Results of *in silico* trials can be found in directories named `Results_*`. The folder `Results_102719` is a 2 cycle treatment trial (with different drug correlation PK parameters). The folder `Results_200520` corresponds to the same trial but for substantially heavier patients. Lastly, the folder `Results_121119` contains *in silico* one-year trials (with no treatment breaks).

### Drug Concentrations
The drug concentrations were simulated using [Ubiquity](https://link.springer.com/article/10.1007/s10928-014-9352-6). The code for the simulations is in `Code/osimertinib_dacomitinib`. The output files exceed 100 Mb and, hence, are not compatible with GitHub. Please contact me (kpoels@g.harvard.edu) so I can share them via another program. 