suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  
})

## Get weight files 
w_dir  <- "../results/simulation_binomial_elasticnet" 
figdir <- "figures"
if (!dir.exists(figdir)) dir.create(figdir, recursive = TRUE)

weight_files <- list.files(
  w_dir,
  pattern = "^weights_amp_\\d+_seedA_\\d+\\.csv$",
  full.names = TRUE
)

if (length(weight_files) == 0) stop("No weight files found.")

weights_raw <- map_dfr(weight_files, function(f){
  
  df <- read_csv(f, show_col_types = FALSE)
  
  # get amp & seedA from the name of file
  m <- str_match(basename(f),
                 "^weights_amp_(\\d+)_seedA_(\\d+)\\.csv$")
  df$amp   <- as.integer(m[ , 2])
  df$seedA <- as.integer(m[ , 3])
  
  df
})

## Only for the PW and BS
weights_long <- weights_raw %>%
  filter(method %in% c("POWER", "STACK"))

## Average over seedA and seedB
avg_w <- weights_long %>%
  group_by(amp, method, stat) %>%
  summarise(weight = mean(weight, na.rm = TRUE), .groups = "drop")

stat_levels <- c("RIDGE", "EN25", "EN50", "EN75", "LASSO")
avg_w$stat  <- factor(avg_w$stat, levels = stat_levels)

## Plot heat map
p <- ggplot(avg_w, aes(x = stat,
                       y = factor(amp,                     
                                  levels = sort(unique(amp))), 
                       fill = weight)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred") +
  facet_grid(method ~ ., scales = "free_y") +
  labs(x = NULL,
       y = "signal amplitude",
       fill = "mean\nweight") +
  theme_bw(base_size = 11) +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(0.4, "cm"))

out_png <- file.path(figdir, "binomial_weight_heatmap_pw_bs.png")
ggsave(out_png, p, width = 6, height = 4, dpi = 300)

message("Heat-map saved to: ", out_png)

