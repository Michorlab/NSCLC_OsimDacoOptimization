# Osimertinib and Dacomitinib Combination Schedule Optimization in EGFR-m NSCLC

## Description of repository

### Code
All essential code for manuscript will be found in this directory. `Cannon/` has the scripts that were sent to the cluster Odyssey (now called Cannon) to run the *in silico* clinical trials.

### Results
Results of *in silico* trials can be found in directories named `Results_*`. The folder `Results_102719` is a 2 cycle treatment trial (with different drug correlation PK parameters). The folder `Results_200520` corresponds to the same trial but for substantially heavier patients. Lastly, the folder `Results_121119` contains *in silico* one-year trials (with no treatment breaks).

### Drug Concentrations
The drug concentrations were simulated using [Ubiquity](https://link.springer.com/article/10.1007/s10928-014-9352-6). The code for the simulations is in `Code/osimertinib_dacomitinib`. These vectorized files are large and I am not able to upload them to GitHub, so please contact me (kpoels@g.harvard.edu) so I can share them separately. 
 