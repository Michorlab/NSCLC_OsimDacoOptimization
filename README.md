# Osimertinib and Dacomitinib Combination Schedule Optimization in EGFR-m NSCLC

Updated: 09/01/20

## Description of repository

### Code
All essential code for manuscript can be found in this directory. `Cannon/` has the scripts that were sent to the cluster Odyssey (now called Cannon) to run the *in silico* clinical trials. Each R script contains a description of its function at the top.

#### Estimation of growth rates
Data from cell growth assays (drug combo of osimertinib + dacomitinib) used to populate tumor growth/evolution model can be found in the folders:

- "Data from Aaron": (from Hata lab): 3-day assays for PC9 parental/derivative cell lines, including C797S which is not available in Pfizer. 
- "Data from Scott": Pfizer; 3-day assays for PC9 parental/derivative cell lines and HCC827 parental and MET-amp cell lines. The popPK model of dacomitinib can be found in this folder under `Dacomitinib`.

The scripts `Code/predictGrowthRates_*.R` estimate the growth rates for each cell type using the data listed above. Scripts with the subscript `*_AH.R` use Aaron Hata's cell lines. The file `Code/saveAllFitsAndRates.R` runs all `predictGrowthRates_*.R` scripts and saves estimated parameters and fitted models in an RData format. The output is in `newFitsAndRates200229.RData`.

##### HCC827 cell line
The analysis from the HCC827 cell lines (parental and MET-amp) was not included in the main manuscript. However, the code used to estimate growth parameters at different drug concentrations can be found in `Code/HCC827growths/`. Cell line labeled HCC827R corresponds to the METamp cell line. `Code/convertHCC2PC9_met.R` "converts" HCC227 growth rates to a comparative value to that of PC9 cell lines by standardizing estimated rates. 

#### Drug concentration simulations
The drug concentrations were simulated using [Ubiquity](https://link.springer.com/article/10.1007/s10928-014-9352-6). In `Code/osimertinib_dacomitinib/`, I simulated drug concentrations (using a two compartment model for each drug) from published popPK models. 

Instructions to obtain drug concentrations over time:
1. Plug-in parameters in system.txt
2. Open analysis_multiple.r to set doses, frequency of doses, sample size, (in lines 37-60) and execute.
3. Save drug concentrations of two compartments and individual PK parameters.

Speed of simulations depends on sample size and frequency of observations per simulated patient. I simulated 1000 patients up to 8 weeks or 1 year under many different drug-combo schedules. For the results, see [below](### Drug concentrations).

#### *In silico* clinical trials

I uploaded `Code/Cannon/Code` to Cannon (at the time called Odyssey) along with the simulated drug concentrations. For each population of patients, we simulated a total of 7 dosing regimens (we actually simulated many other schedules but these 7 are presented in the manuscrip). The traditional regimens are titled `run_group1.*.R`, and the proposed are `run_group2.*.R`. The script `predictCount_ubiquity.R` is called in the other scripts to estimate the expected number of tumors cells at the end of treatment; code is written to read Ubiquity output. Shell script `batchArray.sh` runs all scripts in Odyssey using an [array mode](https://docs.rc.fas.harvard.edu/kb/running-jobs/#Job_arrays). [Results](### Results) presented in manuscript are in repository.

In `estimateRelativeImprovement.R`, we estimate the relative improvement of proposed schedule over traditional schedule using the same metric as in Chakrabarti and Michor, *Cancer Research* 2017. We use a random forest to estimate importance of PK profiles and pre-existence of resistant clones in respect to improvement, `improvement_randomForest.R`. 


#### Validation experiments

`Code/calculateSteadyState.R`

### Results
Results of *in silico* trials can be found in directories named `Results_*`. The folder `Results_102719` is a 2 cycle treatment trial (with different drug correlation PK parameters). The folder `Results_200520` corresponds to the same trial but for substantially heavier patients. Lastly, the folder `Results_121119` contains *in silico* one-year trials (with no treatment breaks).

### Drug concentrations
As mentioned above, the drug concentrations were simulated using [Ubiquity](https://link.springer.com/article/10.1007/s10928-014-9352-6). The output files exceed 100 Mb and are not compatible with GitHub. Please contact me so I can share them via another program. 