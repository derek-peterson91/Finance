
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
})

factor_levels <- c("mkt_rf","smb","hml","rmw","cma","mom","idiosyncratic")
factor_labels <- c("Market","Size (SMB)","Value (HML)",
                   "Profitability (RMW)","Investment (CMA)",
                   "Momentum (MOM)","Idiosyncratic")

# Ensure reports dir exists
ensure_reports_dir <- function() {
  dir.create(reports_dir(), showWarnings = FALSE, recursive = TRUE)
}

# 1) Stacked area of factor variance share over time
plot_factor_share <- function(decomp_ts,
                              out_file = file.path(reports_dir(), "factor_variance_share_area.png")) {
  ensure_reports_dir()
  df <- decomp_ts %>%
    filter(factor != "TOTAL") %>%
    mutate(factor = factor(factor,
                           levels = factor_levels,
                           labels = factor_labels)) %>%
    group_by(date) %>%
    mutate(share = pmax(share, 0)) %>%
    mutate(share = share / sum(share, na.rm = TRUE)) %>%
    ungroup()
  
  p <- ggplot(df, aes(x = date, y = share, fill = factor)) +
    geom_area() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_viridis_d(option = "inferno") +
    labs(title = "Portfolio variance share by component",
         x = NULL, y = "Share of total variance",
         fill = "Component") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.background  = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          legend.text = element_text(color = 'gray35'),
          legend.title = element_blank(),
          plot.title = element_text(color = 'gray35', hjust = 0.5),
          axis.text.x = element_text(color = 'gray35', size = 12),
          axis.title.y = element_text(color = 'gray35'),
          axis.text.y = element_text(color = 'gray35', size = 12)
    )
  
  ggsave(out_file, p, width = 16, height = 9, dpi = 300)
  message("Saved: ", out_file)
  invisible(p)
}

# 2) Latest-date table (CSV) and optional pretty PNG via ggplot
export_latest_table <- function(decomp_ts,
                                out_csv = file.path(reports_dir(), "latest_decomposition.csv"),
                                out_png = file.path(reports_dir(), "latest_decomposition_bar.png"),
                                annualize = TRUE) {
  ensure_reports_dir()
  
  latest_date <- max(decomp_ts$date, na.rm = TRUE)
  latest <- decomp_ts %>%
    filter(date == latest_date, factor != "TOTAL") %>%
    mutate(factor = factor(factor,
                           levels = factor_levels,
                           labels = factor_labels)) %>%
    group_by(date) %>%
    mutate(share = pmax(share, 0),
           share = share / sum(share, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(share))
  
  # Optional: compute annualized vol
  if (annualize) {
    latest <- latest %>%
      mutate(sd_annual = sd * sqrt(252))
  }
  
  # Write CSV
  readr::write_csv(latest, out_csv)
  message("Saved: ", out_csv)
  
  # Bar chart of variance share
  p <- ggplot(latest, aes(x = reorder(factor, share), y = share, fill = factor)) +
    geom_col() +
    scale_fill_viridis_d(option = "inferno", guide = "none") +
    coord_flip() +
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, .05))) +
    labs(
      title = paste0("Latest variance share (", latest_date, ")"),
      x = NULL,
      y = "Share of total variance"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.title = element_text(color = 'gray35', hjust = 0.5),
      axis.title.x = element_text(color = 'gray35', size = 12),
      axis.text.x = element_text(color = 'gray35', size = 12),
      axis.title.y = element_text(color = 'gray35'),
      axis.text.y = element_text(color = 'gray35', size = 12)
    )
  
  ggsave(out_png, p, width = 16, height = 9, dpi = 300)
  message("Saved: ", out_png)
  
  latest
}

# 3) Latest-date volatility contributions (per component)
plot_vol_contrib_latest <- function(
    decomp_ts,
    out_png = file.path(reports_dir(), "latest_vol_contrib_bar_annual.png"),
    out_csv = file.path(reports_dir(), "latest_vol_contrib_annual.csv")
) {
  ensure_reports_dir()
  
  # assumes factor_levels / factor_labels defined in this file
  latest_date <- max(decomp_ts$date, na.rm = TRUE)
  
  latest <- decomp_ts %>%
    filter(date == latest_date, factor != "TOTAL") %>%
    mutate(factor = factor(factor,
                                  levels = factor_levels,
                                  labels = factor_labels)) %>%
    # annualize daily volatility contributions
    mutate(vol_contrib = sd * sqrt(252)) %>%
    arrange(vol_contrib)
  
  # total portfolio σ (annualized) for the title
  tot_annual <- decomp_ts %>%
    filter(date == latest_date, factor == "TOTAL") %>%
    summarise(sigma_annual = sd[1] * sqrt(252)) %>%
    pull(sigma_annual)
  
  # write tidy CSV of contributions
  readr::write_csv(
    latest %>% select(factor, vol_contrib) %>%
      mutate(vol_contrib_pct = vol_contrib),  # keep as proportion for clarity
    out_csv
  )
  message("Saved: ", out_csv)
  
  p <- ggplot(
    latest,
    aes(x = reorder(factor, vol_contrib), y = vol_contrib, fill = factor)
  ) +
    geom_col() +
    scale_fill_viridis_d(option = "inferno", guide = "none") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title = paste0(
        "Latest volatility contribution (annualized, ", latest_date,
        ", total σ ≈ ", scales::percent(tot_annual, accuracy = 0.1), ")"
      ),
      x = NULL,
      y = "Volatility contribution (annualized)"
    ) +
   theme_minimal(base_size = 12) +
   theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.title = element_text(color = "gray35", hjust = 0.5),
      axis.title.x = element_text(color = "gray35", size = 12),
      axis.text.x = element_text(color = "gray35", size = 12),
      axis.title.y = element_text(color = "gray35"),
      axis.text.y = element_text(color = "gray35", size = 12)
    )
  
  ggsave(out_png, p, width = 16, height = 9, dpi = 300, bg = "white")
  message("Saved: ", out_png)
  invisible(p)
}

# Pie chart
plot_vol_contrib_pie <- function(
    decomp_ts,
    out_png = file.path(reports_dir(), "latest_vol_contrib_pie.png"),
    palette_option = "inferno"
) {
  ensure_reports_dir()
  
  latest_date <- max(decomp_ts$date, na.rm = TRUE)
  
  # Use same factor ordering
  factor_levels <- c("mkt_rf","smb","hml","rmw","cma","mom","idiosyncratic")
  factor_labels <- c("Market","Size (SMB)","Value (HML)","Profitability (RMW)",
                     "Investment (CMA)","Momentum (MOM)","Idiosyncratic")
  
  latest <- decomp_ts %>%
    filter(date == latest_date, factor != "TOTAL") %>%
    mutate(factor = factor(factor, levels = factor_levels, labels = factor_labels)) %>%
    mutate(vol_contrib = sd * sqrt(252)) %>%
    mutate(share = vol_contrib / sum(vol_contrib, na.rm = TRUE)) %>%
    arrange(desc(share))
  
  p <- ggplot(latest,
                       aes(x = "", y = share, fill = factor)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    scale_fill_viridis_d(option = palette_option) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = paste0("Volatility contribution breakdown (annualized, ", latest_date, ")"),
      fill = "Component"
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.title = element_text(color = "gray35", hjust = 0.5),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
  
  ggsave(out_png, p, width = 7, height = 7, dpi = 300, bg = "white")
  message("Saved: ", out_png)
  
  invisible(p)
}