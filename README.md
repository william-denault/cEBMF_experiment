# cEBMF experiments



## Table of Contents

1. [Overview](#overview)
2. [Usage](#usage)
3. [Repository Structure](#repository-structure)
4. [Structure of the data folder](#structure-of-the-data-folder)
5. [Code Description](#code-description)
6. [Requirements](#requirements)
7. [License](#license)
8. [Contact Information](#contact-information)

## Overview

This repository contains the code associated with the manuscript "Covariate-moderated Empirical Bayes Matrix Factorization". This repository contains the code and instruction to reproduce the results presented in the manuscript. This README file provides an overview of the repository structure and instructions for usage. 

## Usage 
The spatial transcriptomics data are too large to be included in this repository. 
However, they can be downloaded  [here](https://bioconductor.org/packages/release/data/experiment/html/spatialLIBD.html).
Please place this data in the data/DLPFC folder. You can then run the code in the generate_spatial_pca folder to generate the results for the spatial PCA analysis. 
The results from the spatial PCA analysis contain the normalized counts that are used for the NMF, EBNMF, and cEBNMF analysis. It is, therefore, critical to first
run these analyses before running the NMF, EBNMF, and cEBNMF analyses.


## Repository-structure

```plaintext
├── data
│   ├── DLPFC
│   ├── res_cebmf
│   ├── res_ebmf
│   ├── res_nmf
│   ├── res_spatial_PCA
├── Fig1
├── Fig2
├── job
├── plot
├── script
│   ├── cov_sparsity
│   ├── deep_learning_simulation
│   ├── generate_spatial_pca
│   ├── result_generation
│   ├── scaling_experiement
│   ├── spaRNA
│   ├── tiling
├── sim
├── README.md
├── LICENSE
└── CITATION.cff
```


## Structure of the data folder

The data folder contains the following subfolders:
```plaintext
│   ├── DLPFC
│   ├── res_cebmf
│   ├── res_ebmf
│   ├── res_nmf
│   ├── res_spatial_PCA
```
The DLPFC folder should contain the gene expression data for the DLPFC region of the brain. The res_cebmf, res_ebmf, res_nmf, and res_spatial_PCA folders are there to contain the results of the cEBMF, EBMF, NMF, and spatial PCA methods, respectively.  


## Code-description

The Fig1 folder contains the code to generate the figure 1 in the manuscript. The job folder contains the scripts to run the experiments (simulations and spatial transcriptomics analysis). These jobs are expected to be run on an HPC using the sbatch command. The plot folder contains the plots in the manuscript. The script folder contains
the following subfolders

```
│   ├── cov_sparsity
│   ├── deep_learning_simulation
│   ├── generate_spatial_pca
│   ├── result_generation
│   ├── scaling_experiement
│   ├── spaRNA
│   ├── tiling
```

The cov_sparsity folder contains the code to generate the results for the covariate sparsity experiment.
The generate_spatial_pca folder contains the code to generate the results for the spatial PCA experiment.
The deep_learning_simulation folder contains the code to generate the results for VAE, cVAE and NCF experiments
The scaling_experiement folder contains the code to generate the results for different matrix factorization run time.

NB: running the other spaRNA experiment requires that you have run the script within
generate_spatial_pca and that the results are stored in the res_spatial_PCA folder (which should happen given the script structure in the generate_spatial_pca folder)

The result_generation folder contains the code to generate the results (mostly the plots that will be written in the plot folder). The spaRNA folder contains the code to generate the results for the spaRNA analysis. The tiling folder contains
the code to generate the results. The sim folder contains the code to run the simulation experiments.


## Requirements 

- **sbatch**  
 

- **python (>= 3.79.0)**  

- **R (>= 3.5.0)**  
 
- **PyTorch (>= 2.6.0)**  
 

- **TensorFlow (>= 2.0.0)**  
 

- **Keras (>= 2.3.0)**  
 

- **comoR (>= 1.0.0)**  
 
 
- 10 CPU cores and 15GB of RAM are recommended for the spatial transcriptomics analysis.
 

##  License

MIT License
 
