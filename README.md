
# Data files
There area 4 data files, which contain data on anatomical data (["databmadded.csv"](https://github.com/jzeyl/Scaling_2021/blob/main/databmadded.csv)), a file describing the files in the data file (["Column name descriptions.csv"](https://github.com/jzeyl/Scaling_2021/blob/main/Column%20name%20descriptions.csv)),
audiogramsbehavioural audiograms from literature (["audiograms.csv"](https://github.com/jzeyl/Scaling_2021/blob/main/audiograms.csv)), and the phylogeny file (["JZ tree Prum merged hackett.tree"](https://github.com/jzeyl/Scaling_2021/blob/main/JZ%20tree%20Prum%20merged%20hackett.tree))


# Analysis scripts
The main R file is ["Set up data_scl.R"](https://github.com/jzeyl/Scaling_2021/blob/main/Set%20up%20data_scl.R), which sets up the data for analysis and runs pgls regressions for scaling relationships. Relationships between anatomy and audiograms is "Audiograms linked to anatomy.R" , which links structures to best sensitivity["pgls_audiogram_bs.R"](https://github.com/jzeyl/Scaling_2021/blob/main/pgls_audiogram_bs.R)  , high frequency limit ["pgls_audiogram_hf.R"](https://github.com/jzeyl/Scaling_2021/blob/main/pgls_audiogram_hf.R)  , low frequency limit["pgls_audiogram_lf.R"]((https://github.com/jzeyl/Scaling_2021/blob/main/pgls_audiogram_hf.R))  , and best frequency ["pgls_audiogram_bh.R"](https://github.com/jzeyl/Scaling_2021/blob/main/pgls_audiogram_hf.R).
                      

# Plotting

Plotting of ear measure relative to each other "[2 plot_intra_april 14.R"](https://github.com/jzeyl/Scaling_2021/blob/main/plots/2%20plot_intra_april%2014.R)

Plotting of ear measures relative to head mass and body mass 1 plot ["scatterplots_scl_hm.R"](https://github.com/jzeyl/Scaling_2021/blob/main/plots/1%20plot%20scatterplots_scl_hm.R)

Plotting of ear measures and audiogram data - "3 plot link to anaomy Apr 13.R"(https://github.com/jzeyl/Scaling_2021/blob/main/plots/3%20plot%20link%20to%20anaomy%20Apr%2013.R)


![alt text](superorder.png)
![alt text](audiogrammeasures.png)
