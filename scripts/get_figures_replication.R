library(rstudioapi)
this_path <- dirname(getActiveDocumentContext()$path)
setwd(this_path)

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})

amps <- c(150, 200, 250, 300, 350)
# Helper:
read_one <- function(path) {
  meta <- str_match(basename(path),
                    "res_amp_(\\d+)_seedA_(\\d+)(?:_n_(\\d+))?.csv")
  amp   <- as.numeric(meta[,2])
  seedA <- as.numeric(meta[,3])
  n_ex  <- as.numeric(meta[,4])        
  read_csv(path, show_col_types = FALSE) %>%
    mutate(amp = amp, seedA = seedA, n_ex = n_ex)
}

runs  <- readRDS("../data_raw/simulation_binom.rds")

runs_sum <- runs %>%
  filter(!is.na(method), amp >= amps[1]) %>%                 # drop NA rows
  mutate(method = recode(method,
                         multiple = "Derandomised",
                         vanilla  = "Vanilla")) %>%   # rename
  group_by(amp, method) %>%
  summarise(se_power  = sd(power)/sqrt(100),
            se_fdp    = sd(fdp)/sqrt(100),
            power     = mean(power),
            fdp       = mean(fdp),
            .groups   = "drop") %>%
  mutate(method = factor(method,
                         levels = c("Vanilla", "Derandomised")))


## Power vs amp
p_power <- ggplot(runs_sum,
                  aes(x = amp, y = power,
                      colour = method)) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = power - 2*se_power,
                    ymax = power + 2*se_power),
                width = 0, alpha = 1) +
  scale_colour_manual(values = c("darkorange", "forestgreen")) +

  labs(x = "Signal amplitude (amp)",
       y = "Empirical power") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw(base_size = 12) +
  theme(legend.position = "top",
        legend.title    = element_blank())

ggsave("../figures/binom_power.png",
       p_power, width = 5, height = 5, dpi = 300)

#windows(width = 6, height = 4)
#show(p_power)

## FDP vs amp
p_fdp <- ggplot(runs_sum,
                aes(x = amp, y = fdp,
                    colour = method)) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = fdp - se_fdp,
                    ymax = fdp + se_fdp),
                width = 0, alpha = .4) +
  geom_hline(yintercept = .10, lty = 2, colour = "grey40") +
  scale_colour_manual(values = c("darkorange", "forestgreen")) +
  scale_linetype_manual(values = c(MV = "dotted",
                                   CV = "solid")) +
  labs(x = "Signal amplitude (amp)",
       y = "False discovery proportion") +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_bw(base_size = 12) +
  theme(legend.position = "top",
        legend.title    = element_blank())

ggsave("../figures/binom_fdp.png",
       p_fdp, width = 5, height = 5, dpi = 300)

#windows(width = 6, height = 4)
#show(p_fdp)

## Selection variability vs amp
library(vroom)
library(tidyverse)

p_tot <- 800       # number of features
K      <- 10       # knock-off repetitions

amps_keep <- c(150, 200, 250, 300, 350)

big <- vroom(
  list.files("../data_new/simulation_binom",
             pattern = "_set\\.csv$", full.names = TRUE),
  id        = "file",
  col_types = cols(.default = col_integer())
) %>%
  group_by(file) %>%                     
  mutate(feature = row_number()) %>%    
  ungroup() %>%                         
  mutate(                              
    amp     = as.integer(str_extract(file, "(?<=res_amp_)\\d+")),
    seedA   = as.integer(str_extract(file, "(?<=seedA_)\\d+"))
  ) %>%
  filter(amp %in% amps_keep) %>%
  transmute(
    amp, seedA, feature,
    p_vkn = vkn / K,                     
    p_mkn = mkn / K                      
  ) %>%
  pivot_longer(c(p_vkn, p_mkn),
               names_to  = "method",
               values_to = "p") %>%
  mutate(method = recode(method,
                         p_vkn = "Vanilla",
                         p_mkn = "Derandomised"))


# Conditional Variability
cv <- big %>%
  group_by(amp, method, seedA) %>%              # keep each dataset!
  summarise(
    num_cv = sum(p * (1 - p)),
    s_d    = sum(p),
    den_cv = p_tot * (s_d / p_tot) *
      (1 - s_d / p_tot),
    .groups = "drop"
  ) %>%
  group_by(amp, method) %>%
  summarise(CV = sum(num_cv) / sum(den_cv),
            .groups = "drop")

# Marginal variability
mv <- big %>%
  group_by(amp, method, feature) %>%
  summarise(p_j = mean(p), .groups = "drop") %>%
  group_by(amp, method) %>%
  summarise(
    s_hat = sum(p_j),
    MV = sum(p_j * (1 - p_j)) /
      (p_tot * (s_hat / p_tot) *
         (1 - s_hat / p_tot)),
    .groups = "drop")

# Combine
selvar_long <- bind_rows(
  mv %>%                  # MV rows
    transmute(amp, method,
              type   = "MV",
              selvar = MV),
  cv %>%                  # CV rows
    transmute(amp, method,
              type   = "CV",
              selvar = CV)
)

## Plot
p_selvar <- ggplot(selvar_long,
                   aes(x = amp, y = selvar,
                       colour   = method,
                       linetype = type,
                       group    = interaction(method, type))) +
  geom_line(size = 1) +
  geom_point() +
  scale_colour_manual(values = c("darkorange", "forestgreen")) +
  scale_linetype_manual(values = c(MV = "dotted",
                                   CV = "solid")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Signal amplitude A",
       y = "Selection variability") +
  theme_bw(base_size = 12) +
  theme(legend.position = "top",
        legend.title    = element_blank())

ggsave("../figures/binom_selvar.png",
       p_selvar, width = 5, height = 5, dpi = 300)



#windows(width = 5, height = 5)
#show(p_selvar)

## Selection probability scatter plots
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})

base_scatter <- function(d, x, y, xlab, ylab, title = NULL) {
  ggplot(d, aes({{x}}, {{y}}, colour = type)) +
    geom_abline(slope = 1, intercept = 0,
                lty = 2, colour = "grey50") +
    geom_point(alpha = .4, size = 2) +
    scale_colour_manual(values = c(null = "red", signal = "blue")) +
    labs(x = xlab, y = ylab, title = title) +   
    theme_bw(base_size = 11) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5)  
    )
  
}

amps        <- c(150, 200, 250, 300, 350)   # or (40, 50, 60, 70, 80) for linear
target_seed <- 1                       

for (A in amps) {
  
  # Conditional
  cond_file <- sprintf(
    "../data_new/simulation_binom/res_amp_%d_seedA_%d_set.csv",
    A, target_seed)
  
  cond_dat <- read_csv(cond_file, show_col_types = FALSE) %>%
    mutate(feature     = row_number(),
           p_vkn_cond = vkn/20,
           p_mkn_cond = mkn/20,
           type       = if_else(truth == 0, "null", "signal")) %>%
    select(feature, type, p_vkn_cond, p_mkn_cond)
  
  # Marginal
  marg_files <- list.files(
    "../data_new/simulation_binom",
    pattern = sprintf("res_amp_%d_seedA_\\d+_set.csv", A),
    full.names = TRUE)
  n_datasets <- length(marg_files)
  marg_list <- lapply(
    marg_files,
    \(f) read_csv(f, show_col_types = FALSE) %>%
      mutate(feature = row_number())         
  )
  marg_dat <- bind_rows(marg_list) %>%
    group_by(feature) %>%
    summarise(
      is_signal  = any(truth != 0),
      vkn_total  = sum(vkn),
      mkn_total  = sum(mkn),
      .groups    = "drop"
    ) %>%
    mutate(
      p_vkn_marg = vkn_total / (n_datasets * K),
      p_mkn_marg = mkn_total / (n_datasets * K),
      type       = if_else(is_signal, "signal", "null")
    ) %>%
    select(feature, type, p_vkn_marg, p_mkn_marg)
  
  
  ## Build scatter plots
  p_marg <- base_scatter(marg_dat,
                         p_mkn_marg, p_vkn_marg,
                         "Derandomised knockoff (marginal)",
                         "Vanilla knockoff (marginal)",
                         title = sprintf("Amplitude = %d", A))
  
  p_cond <- base_scatter(cond_dat,
                         p_mkn_cond, p_vkn_cond,
                         "Derandomised knockoff (conditional)",
                         "Vanilla knockoff (conditional)",
                         title = sprintf("Amplitude = %d", A))
  
  ## save and finish!
  ggsave(sprintf("../figures/binom_selection_marginal_amp%d.png", A),
         p_marg, width = 5, height = 5, dpi = 300)
  ggsave(sprintf("../figures/binom_selection_conditional_amp%d.png", A),
         p_cond, width = 5, height = 5, dpi = 300)
  
  message("saved scatter panels for amp = ", A)
}

