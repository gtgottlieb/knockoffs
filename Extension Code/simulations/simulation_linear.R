#!/usr/bin/env Rscript
## Start of problem independent section
if (interactive()) {
  library(rstudioapi)
  this_path <- dirname(getActiveDocumentContext()$path)
  setwd(this_path)
  seedA <- 1; amp <- 200
} else {
  args <- commandArgs(trailingOnly = TRUE)
  seedA<- as.integer(args[1])
  amp <- as.integer(args[2])
  if(is.na(seedA)) seedA <- 1
  if(is.na(amp)) amp <- 200
}
suppressPackageStartupMessages(library(glmnet))
suppressPackageStartupMessages(library(knockoff))
suppressPackageStartupMessages(library(tidyverse))
source("../utils/utils.R")

## The directory to save the results
save_dir <- sprintf("../results/simulation_linear")
if(!dir.exists(save_dir)){
  dir.create(save_dir)
}

## Parameters
set.seed(24601)
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

y.sample <- function(X) X %*% beta_true + rnorm(n)
set <- data.frame(vkn = rep(0,p), mkn = rep(0,p), truth = beta_true)
diags <- knockoff::create.solve_asdp(Sigma)
nrep <- 20
all_res <- data.frame()

## ------------------------------------------------------------
##  Gradient-boosting antisymmetric statistic
## ------------------------------------------------------------
stat.xgb_gain_diff <- function(X, Xk, y,
                               family  = c("gaussian", "binomial"),
                               nrounds = 1460) {
  family <- match.arg(family)
  p      <- ncol(X)
  Xall   <- cbind(X, Xk)
  colnames(Xall) <- c(sprintf("X%03d",1:p), sprintf("Xk%03d",1:p))
  
  D <- xgb.DMatrix(data = Xall, label = y)
  params <- list(
    objective = if (family == "gaussian") "reg:squarederror"
    else "binary:logistic",
    max_depth = 10, eta = 0.02,
    subsample = 0.4, colsample_bytree = 0.4,
    nthread   = 12, verbosity = 0, min_child_weight = 13.92, gamma = 2.45)
  set.seed(1)
  bst <- xgb.train(params, D, nrounds = nrounds, verbose = 0)
  
  imp  <- xgb.importance(feature_names = colnames(Xall),  # <- Add names
                         model = bst)
  gain <- setNames(rep(0, 2*p), colnames(Xall))
  gain[imp$Feature] <- imp$Gain
  
  gain[1:p] - gain[(p+1):(2*p)]
}

## Generating data
set.seed(seedA)
X <- matrix(rnorm(n * p),n) %*% chol(Sigma)
Y <- y.sample(X)

seedB <- 1
## Run the procedures for multiple runs
for(seedB in 1:nrep){
  cat(sprintf("Running the %d-th rep.\n", seedB)) 
  xk_seed <- 100 + (seedA - 1) * nrep + seedB
  set.seed(xk_seed)

  ## Vanilla  knockoff
  Xk <- create.gaussian(X, mu, Sigma, diag_s = diags)
  W <- stat.glmnet_coefdiff(X, Xk, Y)
  tau <- knockoff.threshold(W, fdr = alpha, offset = 1)
  rej <- which(W >= tau)
  fdp <- sum(beta_true[rej]==0) / max(length(rej), 1)
  power <- sum(beta_true[rej]!=0) / k
  all_res <- rbind(all_res,
    data.frame(method = "vanilla", power = power, fdp = fdp, seedB = seedB))
  set$vkn[rej] <- set$vkn[rej] + 1

  ## Compute knockoff e-values 
  res <- ekn(X, Y, M, alpha / 2, mu, Sigma, diags, "gaussian", offset = 1)
  E <- res$E
  rej <- ebh(E, alpha)$rej
  fdp <- sum(beta_true[rej]==0) / max(length(rej), 1)
  power <- sum(beta_true[rej]!=0) / k
  all_res <- rbind(all_res,
    data.frame(method = "multiple", power = power, fdp = fdp, seedB = seedB))
  set$mkn[rej] <- set$mkn[rej] + 1
  
  ## 3. --- vanilla XGBoost knockoff -------------------------
  W  <- stat.xgb_gain_diff(X, Xk, Y)
  tau <- knockoff.threshold(W, fdr = alpha, offset = 1) 
  rej   <- which(W >= tau)
  fdp <- sum(beta_true[rej]==0) / max(length(rej), 1)
  power <- sum(beta_true[rej]!=0) / k
  all_res <- rbind(all_res,
                   data.frame(method = "vanilla_xgb", power = power, fdp = fdp, seedB = seedB))
  
  set$vxgb[rej] <- set$vxgb[rej] + 1
}

out_dir <- sprintf("%s/res_amp_%d_seedA_%d.csv", save_dir, amp, seedA)
write_csv(all_res, out_dir)

out_dir <- sprintf("%s/res_amp_%d_seedA_%d_set.csv", save_dir, amp, seedA)
write_csv(set, out_dir)

