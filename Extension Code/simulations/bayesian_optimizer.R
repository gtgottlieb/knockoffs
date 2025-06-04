
#!/usr/bin/env Rscript

## Start of problem independent section
if (interactive()) {
  library(rstudioapi)
  this_path <- dirname(getActiveDocumentContext()$path)
  setwd(this_path)
  seedA <- 1; amp <- 30
} else {
  args <- commandArgs(trailingOnly = TRUE)
  seedA<- as.integer(args[1])
  amp <- as.integer(args[2])
  if(is.na(seedA)) seedA <- 1
  if(is.na(amp)) amp <- 30
}
suppressPackageStartupMessages(library(glmnet))
suppressPackageStartupMessages(library(knockoff))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(xgboost))
library(ParBayesianOptimization)
source("../utils/utils.R")

## ----------- load or generate data -------------------------
# For a *real* run, replace this block by:
#   X  <- readRDS("X.rds")           #  n x p  matrix     (original features)
#   Xk <- readRDS("Xk.rds")          #  n x p  one knockoff copy
#   y  <- readRDS("y.rds")           #  numeric response  (length n)
seedA <- 2; amp <- 30
n <- 1000
p <- 800
k <- 80
alpha <- 0.1
rho <- 0.5
M <- 50
mu <- rep(0,p)
Sigma <- toeplitz(rho^(0:(p-1)))
nonzero <- seq(10, p, by = 10)
sign_loc <- seq(20, p, by = 20)
beta_true <- rep(0, p)
beta_true[nonzero] <- rnorm(k, amp / 10, 1) / sqrt(n)
beta_true[sign_loc] <- -beta_true[sign_loc] 
 
y.lin <- function(X) X %*% beta_true + rnorm(n)   # linear

y.nonlin <- function(X){               # mildly non-linear
  g <- X[,nonzero]%*%rep(0.8,k) +
    rowSums((X[,nonzero]**2)/2) +
    rnorm(n)
  drop(g)
}
set <- data.frame(vkn = rep(0,p), mkn = rep(0,p), truth = beta_true)
diags <- knockoff::create.solve_asdp(Sigma)

## Generating data
set.seed(seedA)
X <- matrix(rnorm(n * p),n) %*% chol(Sigma)
Y <- y.lin(X)             # <--------- CHOOSE MODEL OF Y!!

Xk <- create.gaussian(X, mu, Sigma, diag_s = diags)

Xall   <- cbind(X, Xk)

## ----------- build combined matrix -------------------------
Xall <- cbind(X, Xk)
colnames(Xall) <- c(sprintf("X%03d", 1:p), sprintf("Xk%03d", 1:p))
D <- xgb.DMatrix(data = Xall, label = Y)

## ----------- search space ----------------------------------
bounds <- list(
  max_depth        = c(1L, 10L),
  min_child_weight = c(5, 15),
  subsample        = c(0.4, 1),
  colsample_bytree = c(0.4, 1),
  eta              = c(0.02, 0.3),
  gamma            = c(0, 10),
  nrounds          = c(200L, 1500L)      # total trees; early-stop inside cv
)

## ----------- objective: 5-fold CV negative RMSE -------------
score_fun <- function(max_depth, min_child_weight,
                      subsample, colsample_bytree,
                      eta, gamma, nrounds) {
  
  cv <- xgb.cv(
    params = list(
      objective        = "reg:squarederror",
      max_depth        = as.integer(max_depth),
      min_child_weight = min_child_weight,
      subsample        = subsample,
      colsample_bytree = colsample_bytree,
      eta              = eta,
      gamma            = gamma,
      nthread          = 12,              # adjust to your CPU
      verbosity        = 0
    ),
    data      = D,
    nfold     = 5,
    metrics   = "rmse",
    showsd    = FALSE,
    early_stopping_rounds = 25,
    nrounds   = as.integer(nrounds),
    verbose   = 0
  )
  
  best_rmse <- min(cv$evaluation_log$test_rmse_mean)
  list(Score = -best_rmse)              # ParBayesOpt maximises
}

## ----------- run Bayesian optimisation ----------------------
opt <- bayesOpt(
  FUN         = score_fun,
  bounds      = bounds,
  initPoints  = 8,      # random starts
  iters.n     = 22,     # guided
  verbose     = 0,
  parallel    = FALSE   
)

best <- getBestPars(opt)
print(best)

## ----------- save for later use -----------------------------
saveRDS(best, file = "best_xgb_params.rds")
cat("Best parameters saved to best_xgb_params.rds\n")

