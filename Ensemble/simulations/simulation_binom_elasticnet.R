#!/usr/bin/env Rscript

if (interactive()) {
  library(rstudioapi)
  this_path <- dirname(getActiveDocumentContext()$path)
  setwd(this_path)
  seedA <- 1; amp <- 150
} else {
  args  <- commandArgs(trailingOnly = TRUE)
  seedA <- as.integer(args[1]); if (is.na(seedA)) seedA <- 1
  amp   <- as.integer(args[2]); if (is.na(amp  )) amp   <- 150
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(glmnet)
  library(knockoff)
})
source("../utils/utils.R")

save_dir <- "../results/simulation_binomial_elasticnet"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

set.seed(24601)
n <- 1000
p <- 600
k <- 50
alpha <- 0.1
rho <- 0.5
M <- 5
Sigma <- toeplitz(rho^(0:(p-1)))
mu <- rep(0,p)

nonzero <- seq(12, p, by = 12)
sign_loc <- seq(24, p, by = 24)
beta_true <- rep(0, p)
beta_true[nonzero] <- rnorm(k, amp / 10, 1) / sqrt(n)
beta_true[sign_loc] <- -beta_true[sign_loc] 

y.sample <- function(X) rbinom(1, 1, exp(X %*% beta_true) / (1 + exp(X %*% beta_true)))
diags <- knockoff::create.solve_asdp(Sigma)
nrep <- 5
all_res <- data.frame()

alpha_grid   <- c(0, 0.25, 0.5, 0.75, 1)      # l1–penalties for glmnet
model_names  <- c("RIDGE", "EN25", "EN50", "EN75", "LASSO")
ens_names    <- c("ENS_EQUAL", "ENS_POWER", "ENS_STACK")

## full list for the “set” data
vkn_cols <- c(paste0("vkn_", model_names),
              paste0("vkn_", ens_names))
mkn_cols <- c(paste0("mkn_", model_names),
              paste0("mkn_", ens_names))
set <- matrix(0, nrow = p, ncol = length(vkn_cols) + length(mkn_cols),
              dimnames = list(NULL, c(vkn_cols, mkn_cols))) %>%
  as.data.frame()
set$truth <- beta_true

all_res <- data.frame()                                 # run-level output
weight_tbl <- data.frame()
nrep    <- 5                                            # nr. repetitions


## Helper functions:

get_E <- function(Xmat, Yvec, stat_fun, l1_penalty = 1) {
  if (identical(stat_fun, stat.glmnet_coefdiff)) {
    ekn(Xmat, Yvec, M, alpha / 2, mu, Sigma, diags,
        family = "binomial", offset = 1, l1_penalty = l1_penalty)$E
  } else {
    warning("No paralelization. Ensure glmnet is used")
    sm <- kn_stat(Xmat, Yvec, M, mu, Sigma, diags,
                  stat_method = stat_fun, family = "binomial")
    kn_evals(sm, gamma = alpha / 2, offset = 1)
  }
}
  
stack_weights <- function(E_list, tol = 1e-8) {
  E_mat <- do.call(cbind, E_list)
  K     <- ncol(E_mat)
  softmax <- function(theta) {
    z <- c(theta, 0)
    expz <- exp(z - max(z))
    expz / sum(expz)
  }
  obj <- function(theta) {
    w <- softmax(theta)
    -sum(log1p(E_mat %*% w))
  }
  theta0 <- rep(0, K - 1)
  opt    <- optim(theta0, obj, method = "BFGS",
                  control = list(reltol = tol))
  w_opt  <- softmax(opt$par)
  names(w_opt) <- names(E_list)
  w_opt
}

## Generate data
set.seed(seedA)
X <- matrix(rnorm(n * p),n) %*% chol(Sigma)
Y <- apply(X, 1, y.sample)

## Main loop 
for (seedB in seq_len(nrep)) {
  
  message(sprintf("Repetition %d / %d", seedB, nrep))
  set.seed(100 + (seedA - 1) * nrep + seedB)
  
  Xk <- create.gaussian(X, mu, Sigma, diag_s = diags) # Used for vanilla knockoff
  
  E_train_list <- list()
  E_test_list  <- list()
  power_train  <- numeric(length(alpha_grid))
  
  ## Individual ENCD models
  for (m in seq_along(alpha_grid)) {
    
    a     <- alpha_grid[m]
    mname <- model_names[m]
    
    # Vanilla Knockoff
    W   <- stat.glmnet_coefdiff(X, Xk, Y, family = "binomial",
                                alpha = a, cores = 6, nlambda = 500)
    tau <- knockoff.threshold(W, fdr = alpha, offset = 1)
    rej <- which(W >= tau)
    
    fdp <- sum(beta_true[rej] == 0) / max(1, length(rej))
    pwr <- sum(beta_true[rej] != 0) / k
    all_res <- rbind(all_res,
                     data.frame(method = paste0("vanilla_", mname),
                                power  = pwr,
                                fdp    = fdp,
                                seedB  = seedB))
    set[[paste0("vkn_", mname)]][rej] <-
      set[[paste0("vkn_", mname)]][rej] + 1
    
    # Derandomized Knockoff (train data)
    E_train <- get_E(X, Y,
                     stat_fun   = stat.glmnet_coefdiff,
                     l1_penalty = a)
    E_train_list[[mname]] <- E_train
    
    rej <- ebh(E_train, alpha)$rej
    fdp <- sum(beta_true[rej] == 0) / max(1, length(rej))
    pwr <- sum(beta_true[rej] != 0) / k
    power_train[m] <- pwr
    all_res <- rbind(all_res,
                     data.frame(method = paste0("multiple_", mname),
                                power  = pwr,
                                fdp    = fdp,
                                seedB  = seedB))
    set[[paste0("mkn_", mname)]][rej] <-
      set[[paste0("mkn_", mname)]][rej] + 1
  }
  
  ## Hold-out test data (e-values)
  set.seed(2000 + seedB)
  X_te <- matrix(rnorm(n * p), n) %*% chol(Sigma)
  Y_te <- apply(X_te, 1, y.sample)
  
  for (m in seq_along(alpha_grid)) {
    a     <- alpha_grid[m]
    mname <- model_names[m]
    E_te  <- get_E(X_te, Y_te,
                   stat_fun   = stat.glmnet_coefdiff,
                   l1_penalty = a)
    E_test_list[[mname]] <- E_te
  }
  
  ## Ensemble methods  
  
  #Equal weights
  E_equal <- Reduce(`+`, E_test_list) / length(E_test_list)
  rej     <- ebh(E_equal, alpha)$rej
  fdp     <- sum(beta_true[rej] == 0) / max(1, length(rej))
  pwr     <- sum(beta_true[rej] != 0) / k
  all_res <- rbind(all_res,
                   data.frame(method = "ensemble_equal",
                              power  = pwr, fdp = fdp, seedB = seedB))
  set$mkn_ENS_EQUAL[rej] <- set$mkn_ENS_EQUAL[rej] + 1
  
  #Performance weighted (power)
  w_pw <- exp(power_train - max(power_train))
  if (sum(w_pw) == 0) w_pw <- rep(1, length(w_pw))
  w_pw <- w_pw / sum(w_pw)
  E_pw <- rep(0, p)
  for (m in seq_along(model_names))
    E_pw <- E_pw + w_pw[m] * E_test_list[[model_names[m]]]
  rej  <- ebh(E_pw, alpha)$rej
  fdp  <- sum(beta_true[rej] == 0) / max(1, length(rej))
  pwr  <- sum(beta_true[rej] != 0) / k
  all_res <- rbind(all_res,
                   data.frame(method = "ensemble_power",
                              power  = pwr, fdp = fdp, seedB = seedB))
  set$mkn_ENS_POWER[rej] <- set$mkn_ENS_POWER[rej] + 1
  
  # Bayesian Stacking
  w_bs <- stack_weights(E_train_list)
  E_bs <- rep(0, p)
  for (m in seq_along(model_names))
    E_bs <- E_bs + w_bs[model_names[m]] * E_test_list[[model_names[m]]]
  rej  <- ebh(E_bs, alpha)$rej
  fdp  <- sum(beta_true[rej] == 0) / max(1, length(rej))
  pwr  <- sum(beta_true[rej] != 0) / k
  all_res <- rbind(all_res,
                   data.frame(method = "ensemble_stack",
                              power  = pwr, fdp = fdp, seedB = seedB))
  set$mkn_ENS_STACK[rej] <- set$mkn_ENS_STACK[rej] + 1
  
  ## Store the weights
  weight_tbl <- bind_rows(
    weight_tbl,
    tibble(seedB = seedB,
           stat   = model_names,
           method = "EQUAL",
           weight = rep(1 / length(model_names), length(model_names))),
    tibble(seedB = seedB,
           stat   = model_names,
           method = "POWER",
           weight = w_pw),
    tibble(seedB = seedB,
           stat   = model_names,
           method = "STACK",
           weight = as.numeric(w_bs[model_names]))
  )
}

## Save & finish!
out_csv    <- sprintf("%s/res_amp_%d_seedA_%d.csv",     save_dir, amp, seedA)
set_csv    <- sprintf("%s/res_amp_%d_seedA_%d_set.csv", save_dir, amp, seedA)
wgt_csv    <- sprintf("%s/weights_amp_%d_seedA_%d.csv", save_dir, amp, seedA)

write_csv(all_res, out_csv)   
write_csv(set,     set_csv)   
write_csv(weight_tbl, wgt_csv)

cat("Finished — results saved to\n  ",
    out_csv, "\n  ",
    set_csv, "\n  ",
    wgt_csv, "\n")

