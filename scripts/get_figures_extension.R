suppressPackageStartupMessages({
  library(ggplot2)          
  library(dplyr)            
  library(tidyr)
  library(stringr)
  library(rstudioapi)
})
this_path <- dirname(getActiveDocumentContext()$path)
setwd(this_path)

# Include vanilla in graphs?
include_vanilla <- FALSE
is_binomial <- FALSE

base_dir <- {if (is_binomial) "../results/simulation_binomial_elasticnet" else "../results/simulation_linear_elasticnet"}
fig_dir  <- "../figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir)

amps      <- if (is_binomial) { c(150, 200, 250, 300, 350)
  } else { c(40, 50, 60, 70, 80) }
seeds     <- 1:10
K_rep     <- 5
p_tot     <- 800
SHOW_CONDITIONAL <- FALSE

## method names 
#base_mods <- c("RIDGE", "EN25", "EN50", "EN75", "LASSO")
base_mods <- c("LASSO")
full_base_mods <- c("RIDGE", "EN25", "EN50", "EN75", "LASSO")  # For CMSV 
ens_mods  <- c("ENS_EQUAL", "ENS_POWER", "ENS_STACK")


vanilla_nm <- paste0("vanilla_",  base_mods)
multi_nm   <- paste0("multiple_", base_mods)
ens_nm     <- paste0("ensemble_", c("equal", "power", "stack"))

methods_keep <- if (include_vanilla)  { c(vanilla_nm, multi_nm, ens_nm)
  } else                              { c(multi_nm, ens_nm) } 

legend_labels <- setNames(
  c(base_mods,              
    c("EW ensemble",        
      "PW ensemble",        
      "BS ensemble"),       
    "CMSV"),
  c(paste0("multiple_", base_mods),
    "ensemble_equal",
    "ensemble_power",
    "ensemble_stack",
    "CMSV")
)


if (include_vanilla) {
  
  vanilla_labels <- setNames(base_mods, paste0("vanilla_", base_mods))
  legend_labels <- c(vanilla_labels, legend_labels)
} else {
  
}

## colours 
user_colours <- c(
  multiple_RIDGE   = "#d9c502",
  multiple_EN25    = "#7570b3", 
  multiple_EN50    = "#d95f02",
  multiple_EN75    = "#e7298a",
  multiple_LASSO   = "#d90202",
  ensemble_equal   = "#666666",
  ensemble_power   = "#02d995",
  ensemble_stack   = "#0257d9",
  CMSV             = "black"        
)

default_colours <- setNames(rainbow(length(methods_keep)), methods_keep)

colour_map <- default_colours               
idx         <- intersect(names(user_colours), methods_keep)
colour_map[idx] <- user_colours[idx]        

##############################
##  Power & FDP vs amp
##############################
run_all <- data.frame()

for (A in amps) for (s in seeds) {
  f <- sprintf("%s/res_amp_%d_seedA_%d.csv", base_dir, A, s)
  if (!file.exists(f)) next
  tmp <- read.csv(f, stringsAsFactors = FALSE)
  tmp$amp   <- A
  tmp$seedA <- s
  run_all   <- rbind(run_all, tmp)
}

sum_stats <- run_all %>%
  filter(method %in% methods_keep) %>%
  group_by(amp, method) %>%
  summarise(
    power_mean = mean(power),
    fdp_mean   = mean(fdp),
    power_se   = sd(power) / sqrt(n()),
    fdp_se     = sd(fdp)   / sqrt(n()),
    .groups = "drop"
  )


plot_line <- function(y, lab, fname, se_col, is_fdp = FALSE) {
  ggplot(sum_stats,
         aes(x = amp, y = .data[[y]],
             colour = method, group = method)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = .data[[y]] - .data[[se_col]],
                      ymax = .data[[y]] + .data[[se_col]]),
                  width = 0) +
    scale_colour_manual(values = colour_map, labels = legend_labels) +
    labs(x = "Signal amplitude", y = lab, colour = NULL) +
    {if (is_fdp) scale_y_continuous(limits = c(0, 0.5)) else scale_y_continuous(limits = c(0,1))} +
    {if (is_fdp) geom_hline(yintercept = 0.10, lty = 2) else NULL} +
    theme_bw(base_size = 11) +
    {if (is_fdp) theme(legend.position = "inside", legend.position.inside = c(0.8,0.8))
      else theme(legend.position = "inside", legend.position.inside = c(0.8,0.2)) } -> p
  ggsave(sprintf("%s/%s.png", fig_dir, fname),
         p, width = 5, height = 5, dpi = 300)
}

file_name_power <- if (include_vanilla & is_binomial) {
  "binomial_en_power_vs_amp"
} else if (include_vanilla) {
  "en_power_vs_amp"
} else if (is_binomial) {
  "binomial_en_power_vs_amp_mult"
} else {
  "en_power_vs_amp_mult"
}

file_name_fdp <- if (include_vanilla & is_binomial) {
  "binomial_en_fdp_vs_amp"
} else if (include_vanilla) {
  "en_fdp_vs_amp"
} else if (is_binomial) {
  "binomial_en_fdp_vs_amp_mult"
} else {
  "en_fdp_vs_amp_mult"
}

plot_line("power_mean", "Empirical power", file_name_power, "power_se")
  
plot_line("fdp_mean", "False discovery proportion", file_name_fdp,
            "fdp_se", is_fdp = TRUE)





###################################
##  Selection variability: MV & CV
###################################
mv_cv_all <- data.frame()

for (A in amps) {
  
  set_files <- list.files(
    base_dir,
    pattern = sprintf("res_amp_%d_seedA_\\d+_set.csv", A),
    full.names = TRUE)
  
  D <- length(set_files)
  if (D == 0) next
  
  lst <- lapply(set_files, read.csv, stringsAsFactors = FALSE)
  
  for (mname in c(base_mods, ens_mods)) {
    
    pref_v <- paste0("vkn_", mname)          # absent for ensembles
    pref_m <- paste0("mkn_", mname)
    
    ## aggregate marginal probs.
    pm <- Reduce(`+`, lapply(lst, `[[`, pref_m)) / (K_rep * D)
    
    s_hat_m <- sum(pm)
    mv_m    <- sum(pm * (1 - pm)) /
      (p_tot * (s_hat_m / p_tot) * (1 - s_hat_m / p_tot))
    
    # Store marginal probs.
    meth_mv <- if (mname %in% base_mods) {
      paste0("multiple_", mname)          # e.g. multiple_EN50
    } else {
      # Rename
      paste0("ensemble_", tolower(sub("ENS_", "", mname)))
    }
    
    mv_cv_all <- rbind(mv_cv_all,
                       data.frame(amp    = A,
                                  method = meth_mv,
                                  type   = "MV",
                                  value  = mv_m))
    
    ## Vanilla MV if requested
    if (include_vanilla &&
        (mname %in% base_mods) &&
        pref_v %in% names(lst[[1]])) {
      
      pv <- Reduce(`+`, lapply(lst, `[[`, pref_v)) / (K_rep * D)
      s_hat_v <- sum(pv)
      mv_v    <- sum(pv * (1 - pv)) /
        (p_tot * (s_hat_v / p_tot) * (1 - s_hat_v / p_tot))
      
      mv_cv_all <- rbind(mv_cv_all,
                         data.frame(amp    = A,
                                    method = paste0("vanilla_", mname),
                                    type   = "MV",
                                    value  = mv_v))
    }
    
    ## CV
    for (d in seq_len(D)) {
      
      pm_d <- lst[[d]][[pref_m]] / K_rep
      s_d_m <- sum(pm_d)
      cv_m  <- sum(pm_d * (1 - pm_d)) /
        (p_tot * (s_d_m / p_tot) * (1 - s_d_m / p_tot))
      
      
      meth_cv <- meth_mv                        # same naming rule
      mv_cv_all <- rbind(mv_cv_all,
                         data.frame(amp    = A,
                                    method = meth_cv,
                                    type   = "CV",
                                    value  = cv_m))
      
      
      if (include_vanilla &&
          (mname %in% base_mods) &&
          pref_v %in% names(lst[[d]])) {
        
        pv_d <- lst[[d]][[pref_v]] / K_rep
        s_d_v <- sum(pv_d)
        cv_v  <- sum(pv_d * (1 - pv_d)) /
          (p_tot * (s_d_v / p_tot) * (1 - s_d_v / p_tot))
        
        mv_cv_all <- rbind(mv_cv_all,
                           data.frame(amp    = A,
                                      method = paste0("vanilla_", mname),
                                      type   = "CV",
                                      value  = cv_v))
      }
    } 
  }   
}     


## CMSV - cross model selection variability
comb_rows <- data.frame()

for (A in amps) {
  
  set_files <- list.files(
    base_dir,
    pattern = sprintf("res_amp_%d_seedA_\\d+_set.csv", A),
    full.names = TRUE)
  
  D <- length(set_files)
  if (D == 0) next
  
  lst <- lapply(set_files, read.csv, stringsAsFactors = FALSE)
  
  ## Marginal - across all datasets
  avail <- intersect(full_base_mods,
                     sub("^mkn_", "", names(lst[[1]])[grepl("^mkn_", names(lst[[1]]))]))
  pm_mat <- sapply(avail, function(m)
    Reduce(`+`, lapply(lst, `[[`, paste0("mkn_", m))) / (K_rep * D))
  pm_comb <- rowMeans(pm_mat)
  
  s_hat   <- sum(pm_comb)
  mv_comb <- sum(pm_comb * (1 - pm_comb)) /
    (p_tot * (s_hat / p_tot) * (1 - s_hat / p_tot))
  
  comb_rows <- rbind(comb_rows,
                     data.frame(amp    = A,
                                method = "CMSV",
                                type   = "MV",
                                value  = mv_comb))
  
  ## Conditional - dataset-by-dataset
  for (d in seq_len(D)) {
    pm_mat_d <- sapply(avail, function(m)
      lst[[d]][[paste0("mkn_", m)]]) / K_rep
    pm_comb_d <- rowMeans(pm_mat_d)
    
    s_d   <- sum(pm_comb_d)
    cv_d  <- sum(pm_comb_d * (1 - pm_comb_d)) /
      (p_tot * (s_d / p_tot) * (1 - s_d / p_tot))
    
    comb_rows <- rbind(comb_rows,
                       data.frame(amp    = A,
                                  method = "CMSV",
                                  type   = "CV",
                                  value  = cv_d))
  }
}

## add to big table
mv_cv_all <- rbind(mv_cv_all, comb_rows)

## give CMSV a colour so ggplot won’t complain
colour_map["CMSV"] <- "black"


## average over data sets and plot
mv_cv_plot <- mv_cv_all %>%
  group_by(amp, method, type) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  filter(method %in% c(methods_keep, "CMSV")) %>% 
  { if (SHOW_CONDITIONAL) . else dplyr::filter(., type == "MV") }

if (SHOW_CONDITIONAL) {
  line_type <- c(MV = "dotted", CV = "solid")
  
} else {
  line_type <- c(MV = "solid")
}

ggplot(mv_cv_plot,
       aes(amp, value, colour = method,
           linetype = type,
           group = interaction(method, type))) +
  geom_line()+ geom_point() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_colour_manual(values = colour_map, labels = legend_labels) +
  scale_linetype_manual(values = line_type) +
  labs(x = "Signal amplitude", y = "Selection variability",
       colour = NULL, linetype = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "inside", legend.position.inside = c(0.8,0.75)) -> p_sv

file_name_selvar <- if (include_vanilla & is_binomial) {
  "binomial_en_selvar_vs_amp"
} else if (include_vanilla) {
  "en_selvar_vs_amp"
} else if (is_binomial) {
  "binomial_en_selvar_vs_amp_mult"
} else {
  "en_selvar_vs_amp_mult"
}

ggsave(sprintf("%s/%s.png", fig_dir, file_name_selvar),
       p_sv, width = 5, height = 5, dpi = 300)


## Selection probabilities scatter plots
if (include_vanilla) {
  target_seed <- 1
  for (A in amps) {
    f <- sprintf("%s/res_amp_%d_seedA_%d_set.csv",
                 base_dir, A, target_seed)
    if (!file.exists(f)) next
    df <- read.csv(f, stringsAsFactors = FALSE)
    for (mname in base_mods) {
      pv <- df[[paste0("vkn_", mname)]] / K_rep
      pm <- df[[paste0("mkn_", mname)]] / K_rep
      truth <- df$truth
      type  <- ifelse(truth == 0, "null", "signal")
      ggplot(data.frame(p_multi = pm, p_van = pv, type = type),
             aes(p_multi, p_van, colour = type)) +
        geom_abline(slope = 1, intercept = 0, lty = 2) +
        geom_point(alpha = .5) +
        scale_colour_manual(values = c(null = "red", signal = "blue"), labels = legend_labels) +
        labs(x = "Derandomised selection probability",
             y = "Vanilla selection probability",
             colour = NULL,
             title = sprintf("Amplitude %d  –  %s", A, mname)) +
        theme_bw(base_size = 11) +
        theme(plot.title = element_text(hjust = 0.5))          -> p_sc
        ggsave(sprintf("%s/scatter_amp%d_%s.png", fig_dir, A, mname),
               p_sc, width = 5, height = 5, dpi = 300)
    }
  }
}

cat("Finished.  Figures saved in ", fig_dir, "\n")
  
