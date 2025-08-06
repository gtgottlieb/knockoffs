Derandomised Knockoff Ensembles -- Thesis Code

This repository contains the code to reproduce every simulation and figure for
“Derandomised knockoff ensembles: robust feature selection via Bayesian stacking.”

Directories:
├── Ensemble/ 			# Extensions
│ ├── simulations/ 		
│ └── utils/ 				# Helper functions - author: Ren and Barber (2024)
├── Lasso-only/ 			# Replication of Ren and Barber (2024) - all credit attributed to authors
│ ├── simulations/		
│ └── utils/
├── scripts/ 				# Figure generation

Lasso-only/simulations directory contains the code for the replicated elements of Ren and Barber (2024). All credit attributed to the authors. 

Ensemble/simulations directory contains the code for all the extension simulations: the full ENCD statistics, and the ensemble models. 

The "scripts" folder contains the scripts used to create all figures depicted in the paper. 



How to replicate results:
Run the following bash commands. Loop over all desired seeds and amplitudes.
For the linear setting amp values are in (40, 50, 60, 70, 80) and (150, 200, 250, 300, 350) for the logistic setting. 10 seedA values were used in this paper (1:10).
Ensure each simulation is configured with the desired parameters such as M (number of individual knockoffs to be derandomized). 

# Linear model, lasso-only ensemble
Rscript Lasso-only/simulations/simulation_linear.R  <seedA>  <amplitude>

# Logistic model, lasso-only ensemble
Rscript Lasso-only/simulations/simulation_binom.R  <seedA>  <amplitude>

# Linear model, elastic net & ensembles
Rscript Ensemble/simulations/simulation_linear_elasticnet.R  <seedA>  <amplitude>

# Logistic model, elastic net & ensembles
Rscript Ensemble/simulations/simulation_linear_binm.R  <seedA>  <amplitude>


Results will be written to (by default):

Ensemble/results/
Lasso-only/results/


To replicate the figures:

Run the following for the desired configuration and desired content in figures

get_figures_extension.R
or 
get_figures_replication.R

Running line-by-line is recommended as the workflow is not automatic.