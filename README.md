<p align="center">
    <img height="150" src="https://uni-muenster.sciebo.de/s/RwS9PA1dj1Nmivn/download">
</p>

# clevRvis

clevRvis provides an extensive set of visualization techniques for clonal evolution. Three types of plots are available: 1) shark plots (basic trees, showing the phylogeny and optionally the cancer cell fraction CCF); 2) dolphin plots (advanced visualization, showing the phylogeny and the development of CCFs over time); 3) plaice plots (novel visualization, showing the phylogeny, the development of CCFs and the development of remaining healthy alleles, influenced by bi-allelic events, over time). Moreover, the tool provides algorithms for fully automatic interpolation of time points and estimation of therapy effect to approximate a tumor's development in the presence of few measured time points, as well as exploring alternative trees.

## Requirements
To run clevRvis, you need R Version 4.1.0 or higher.

##  Installation
clevRvis is available at Bioconductor. The latest version can be installed via:

```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("clevRvis")
```


The latest version available at github can be installed via:

```
if (!requireNamespace("devtools", quietly=TRUE))
  install.packages("devtools")
devtools::install_github("sandmanns/clevRvis")
```

##  Citation
Sandmann S, Inserte C, Varghese J. clevRvis: visualization techniques for clonal evolution. Gigascience. 2022;12:giad020. 
https://doi.org/10.1093/gigascience/giad020
