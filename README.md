# Workflow
This work corresponds to in prep for Hearing Research  "Inter-specific scaling of ear morphology of birds and its implications for hearing performance".

**To reproduce the the scaling pgls analyses:**

Start with "Set up data_scl.R". This loads the data and phylogeny. This also runs the phylogenetic regressions (phylogenetic generalized least squares regressions, or PGLS) between ear measures and between each ear measure and head mass. This script also exports the summary statistics and tables to csv or word files.

**To run the pgls between ear structures and audiometric measurements:**  

First,  "Set up data_scl.R" also has to be run to get the data prepared (minus the pgls from those scripts). Next, the scripts in "Audiograms linked to anatomy.R" can be run. This creates a 'limits' dataframe with the audiogram metrics, which will be joined with the anatomy dataframes for analysis. Once the limits df is created the "pgls_resids re headmass.R" can be used to run the pgls analyses with head mass-corrected values.

**Scripts for plots are in the plots folder.**  
For scatterplots, the associated analysis files must be run before plotting.  






## Data files
|File|Description|
|-----|-----|
|["databmadded.csv"](https://github.com/jzeyl/Scaling_2021/blob/main/databmadded.csv)|anatomical data|
|["Column name descriptions.csv"](https://github.com/jzeyl/Scaling_2021/blob/main/Column%20name%20descriptions.csv)|a file describing the column names in data files|
|["audiograms.csv"](https://github.com/jzeyl/Scaling_2021/blob/main/audiograms.csv)|threshold data from behavioural audiograms collected from literature|
|["JZ tree Prum merged hackett.tree"](https://github.com/jzeyl/Scaling_2021/blob/main/JZ%20tree%20Prum%20merged%20hackett.tree)|phylogeny file|


## Scaling analyses

|File|Description|
|-----|-----|
|["Set up data_scl.R"](https://github.com/jzeyl/Scaling_2021/blob/main/Set%20up%20data_scl.R)|This is the main analysis script, which imports the data analysis and runs pgls regressions, calling formulas make in other R scripts. Outputs are tables with statistical results (hm, intra, bm) exported to csv or word.|
|"pgls_intraear.R"|pgls models run between auditory structures|
|"pgls_bm.R"|pgls models run against body mass|
|"pgls_HM.R"|pgls models run against head mass|

## Audiometry analyses

|File|Description|
|-----|-----|
|"Audiograms linked to anatomy.R"|This is the main analysis script for analyses between anatomy and audiogram metrics. This script runs the pgls regressions, calling formulas make in other R scripts. Outputs are tables with statistical results exported to csv or word.|
|["pgls_audiogram_bs.R"](https://github.com/jzeyl/Scaling_2021/blob/main/pgls_audiogram_bs.R)|pgls models run against best sensitivity|
|["pgls_audiogram_hf.R"](https://github.com/jzeyl/Scaling_2021/blob/main/pgls_audiogram_hf.R)|pgls models run against high frequency limit|
|["pgls_audiogram_lf.R"]((https://github.com/jzeyl/Scaling_2021/blob/main/pgls_audiogram_hf.R))|pgls models run against low frequency limit|
|["pgls_audiogram_bh.R"](https://github.com/jzeyl/Scaling_2021/blob/main/pgls_audiogram_hf.R)|pgls models run against best frequency| 


                      



