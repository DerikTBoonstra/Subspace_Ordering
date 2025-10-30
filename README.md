# Subspace Ordering

## Overview

This repository accompanies the research paper:

**"Subspace Ordering for Maximum Response Preservation in Sufficient Dimension Reduction"** 

*Authors:* Derik T. Boonstra, Rakheon Kim, and Dean M. Young

## Repository Structure

- `datasets/`: Contains datasets used for real data applications.
- `mc_sims/`: R Scripts for Monte Carlo simulations.
- `real_data_applications/`: Scripts for real data applications. 
- `saved_results/`: Saved results from simulations and applications.
- `Requirements.R`: R script listing required packages. Run before any computations. 
- `Subspace_Ordering.Rproj`: RStudio project file. To efficiently run code, open project first. 
- `figures.R`: Script for generating figures from the paper.
- `wrapper_fns.R`: Wrapper functions used in the analysis.
  
Note: Analysis is primarily in R. Matlab is used for the ENDS methods only.

If you find this work useful, please cite our paper:

```{.tex}
@article{boonstra2025,
      title={Subspace Ordering for Maximum Response Preservation in Sufficient Dimension Reduction}, 
      author={Derik T. Boonstra and Rakheon Kim and Dean M. Young},
      year={2025}
}
```
