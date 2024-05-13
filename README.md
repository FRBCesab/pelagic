# pelagic

[![DOI](https://zenodo.org/badge/234053806.svg)](https://zenodo.org/doi/10.5281/zenodo.11183845)

Code and data used in the paper **The socioeconomic and environmental niche of protected areas reveals global conservation gaps and opportunities** (Mouillot et al. 2024) published in **Nature Communications**.


## General

This repository is structured as follow:

- :file_folder: &nbsp;`data/`: contains all data required to run PCA/ENFA and reproduce figures;
- :file_folder: &nbsp;`R/`: contains R functions developed for this project;
- :file_folder: &nbsp;`man/`: contains R functions documentation;
- :file_folder: &nbsp;`analyses/`: contains R scripts to setup project, run analyses, and make figures.
- :file_folder: &nbsp;`figures/`: contains figures of the paper 


## Notes

- All required packages will be installed (if necessary) and loaded.
- Figures will be stored in `figures/`



## Usage

Clone the repository and run this command in R/RStudio:

```r
source("_make.R")
```

Cheers!
