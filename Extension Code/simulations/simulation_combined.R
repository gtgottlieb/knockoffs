#!/usr/bin/env Rscript

if (interactive()) {
  library(rstudioapi)
  this_path <- dirname(getActiveDocumentContext()$path)
  setwd(this_path)
  seedA    <- 1
  amp      <- 200
  sim_type <- "nonlinear"  # "linear" or "nonlinear"
} else {
  args <- commandArgs(trailingOnly = TRUE)
  seedA    <- as.integer(args[1]); if (is.na(seedA)) seedA <- 1
  amp      <- as.integer(args[2]); if (is.na(amp))   amp   <- 40
  sim_type <- ifelse(length(args) >= 3, args[3], "linear")
  sim_type <- match.arg(sim_type, c("linear", "nonlinear"))
}

suppressPackageStartupMessages({
  library(glmnet)
  library(xgboost)
  library(knockoff)
  library(tidyverse)
})

source("../utils/utils.R", local = TRUE)

save_dir <- sprintf("../results/simulation_meta_%s", sim_type)
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

set.seed(24601)
n      <- 1000
p      <- 800
k      <- 80
alpha  <- 0.10
rho    <- 0.5
M      <- 50                 
mu     <- rep(0, p)
Sigma  <- toeplitz(rho^(0:(p-1)))
nonzero   <- seq(10, p, by = 10)
sign_loc  <- seq(20, p, by = 20)
beta_true <- rep(0, p)

beta_true[nonzero] <- rnorm(k, amp/10, 1) / sqrt(n)
beta_true[sign_loc] <- -beta_true[sign_loc]

y.sample <- switch(sim_type,
                   linear    = function(X) drop(X %*% beta_true + rnorm(n)),
                   nonlinear = function(X) {
                     g <- X %*% beta_true + rowSums((X[, nonzero]^2) / 2) + rnorm(n)
                     drop(g)
                   })

# Model-based cross validated MSE:
cv_mse_glmnet <- function(X, y, nfolds = 5) {
  cv <- cv.glmnet(X, y, alpha = 1, nfolds = nfolds, family = "gaussian")
  min(cv$cvm)
}

cv_mse_xgb <- function(X, y, nrounds = 600, nfolds = 5) {
  d <- xgb.DMatrix(data = X, label = y)
  params <- list(objective = "reg:squarederror", max_depth = 6, eta = 0.05,
                 subsample = 0.7, colsample_bytree = 0.7, nthread = 12,
                 min_child_weight = 1, gamma = 0)
  cv <- xgb.cv(params, d, nrounds = nrounds, nfold = nfolds,
               verbose = 0, early_stopping_rounds = 25)
  tail(cv$evaluation_log$test_rmse_mean, 1)^2
}

stat.xgb_gain_diff <- function(X, Xk, y, nrounds = 1500) {
  p <- ncol(X)
  Xall <- cbind(X, Xk)
  colnames(Xall) <- c(sprintf("X%03d", 1:p), sprintf("Xk%03d", 1:p))
  d <- xgb.DMatrix(data = Xall, label = y)
  params <- list(objective = "reg:squarederror", max_depth = 3, eta = 0.1,
                 subsample = 0.8, colsample_bytree = 0.8, nthread = 12,
                 min_child_weight = 5, gamma = 1)
  set.seed(1)
  bst <- xgb.train(params, d, nrounds = nrounds, verbose = 0)
  imp <- xgb.importance(feature_names = colnames(Xall), model = bst)
  gain <- setNames(rep(0, 2*p), colnames(Xall))
  gain[imp$Feature] <- imp$Gain
  gain[1:p] - gain[(p+1):(2*p)]
}

stat.meta_comb <- function(X, Xk, y) {
  # base statistics
  W_glm <- stat.glmnet_coefdiff(X, Xk, y)
  W_xgb <- stat.xgb_gain_diff(X, Xk, y)
  # weights: inverse MSE
  mse_glm <- cv_mse_glmnet(X, y)
  mse_xgb <- cv_mse_xgb(X, y, nrounds = 600)
  w_glm   <- 1 / (mse_glm + 1e-8)
  w_xgb   <- 1 / (mse_xgb + 1e-8)
  # combined statistic
  (w_glm * W_glm + w_xgb * W_xgb) / (w_glm + w_xgb)
}

# Simulate data
set.seed(seedA)
X <- matrix(rnorm(n * p), n) %*% chol(Sigma)
Y <- y.sample(X)

diags <- knockoff::create.solve_asdp(Sigma)
set   <- data.frame(vkn = rep(0, p), meta = rep(0, p), truth = beta_true)
all_res <- data.frame()

nrep <- 20               # inner repetitions (seedB)

# Main loop
seedB <- 1
for (seedB in seq_len(nrep)) {
  cat(sprintf("[meta] Running rep %d/%d\n", seedB, nrep))
  set.seed(100 + (seedA - 1) * nrep + seedB)
  
  Xk <- create.gaussian(X, mu, Sigma, diag_s = diags)
  
  # 1. Vanilla Lasso
  W_glm <- stat.glmnet_coefdiff(X, Xk, Y)
  tau   <- knockoff.threshold(W_glm, fdr = alpha, offset = 1)
  rej   <- which(W_glm >= tau)
  fdp   <- sum(beta_true[rej] == 0) / max(1, length(rej))
  power <- sum(beta_true[rej] != 0) / k
  all_res <- rbind(all_res, data.frame(method = "lasso", power, fdp, seedB))
  
  # 2. Vanilla XGB
  W_xgb <- stat.xgb_gain_diff(X, Xk, Y)
  tau   <- knockoff.threshold(W_xgb, fdr = alpha, offset = 1)
  rej   <- which(W_xgb >= tau)
  fdp   <- sum(beta_true[rej] == 0) / max(1, length(rej))
  power <- sum(beta_true[rej] != 0) / k
  all_res <- rbind(all_res, data.frame(method = "xgb", power, fdp, seedB))
  
  # 3. Vanilla Meta Learner.
  W_meta <- stat.meta_comb(X, Xk, Y)
  tau    <- knockoff.threshold(W_meta, fdr = alpha, offset = 1)
  rej    <- which(W_meta >= tau)
  fdp    <- sum(beta_true[rej] == 0) / max(1, length(rej))
  power  <- sum(beta_true[rej] != 0) / k
  all_res <- rbind(all_res, data.frame(method = "meta", power, fdp, seedB))
  set$meta[rej] <- set$meta[rej] + 1
}

out_csv <- sprintf("%s/res_amp_%d_seedA_%d.csv", save_dir, amp, seedA)
write_csv(all_res, out_csv)

set_csv <- sprintf("%s/res_amp_%d_seedA_%d_set.csv", save_dir, amp, seedA)
write_csv(set, set_csv)

cat("\n[meta] Done.  Results written to:")
cat(sprintf("\n  %s\n  %s\n", out_csv, set_csv))
