# Analysis: WVS values vs Claude AI usage
# Requires prep_wvs_ai_usage.R to be run immediately before.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(ggrepel)
  library(svglite)
})

# ---- Load prep ----
prep_path <- "outputs/wvs_ai_prep.rds"
if (!file.exists(prep_path)) {
  stop("Missing prep file: ", prep_path, "\nRun: Rscript drafts/world-values-ai-usage/prep_wvs_ai_usage.R")
}

prep <- readRDS(prep_path)
merged <- prep$merged
# usage_y <- prep$usage_y
# usage_label <- prep$usage_label
plot_vars <- prep$plot_vars
labels_df <- prep$labels

# usage_y <- if (any(!is.na(merged$log_usage_per_capita))) "log_usage_per_capita" else "log_usage_pct"
# usage_label <- if (usage_y == "log_usage_per_capita") "log1p(usage per capita)" else "log1p(usage pct)"

usage_y <- "log_usage_per_capita"
usage_label <- "log1p(usage per capita)"

# usage_y <- "log_usage_pct"
# usage_label <- "log1p(usage pct)"

# usage_y <- "usage_pct_2025_11"
# usage_label <- "usage pct"

# ---- Correlations (raw and GDP-partial) ----
vars_for_corr <- c(
  "tech_optimism", "tech_faith_tension", "secular_rational",
  "schwartz_openness", "schwartz_conservation", "econ_statism"
)

partial_corr <- function(y, x, gdp) {
  cc <- complete.cases(y, x, gdp)
  if (sum(cc) < 3) return(NA_real_)
  ry <- resid(lm(y[cc] ~ gdp[cc]))
  rx <- resid(lm(x[cc] ~ gdp[cc]))
  cor(ry, rx, use = "pairwise.complete.obs")
}

corrs <- tibble(
  variable = vars_for_corr,
  cor_usage = sapply(vars_for_corr, function(v) {
    cor(merged[[v]], merged[[usage_y]], use = "pairwise.complete.obs")
  }),
  cor_usage_partial_gdp = sapply(vars_for_corr, function(v) {
    partial_corr(merged[[usage_y]], merged[[v]], merged$log_gdp)
  })
)

# ---- Models ----
models <- list(
  m1 = lm(as.formula(paste(usage_y, "~ tech_optimism + log_gdp")), data = merged),
  m2 = lm(as.formula(paste(usage_y, "~ secular_rational + log_gdp")), data = merged),
  m3 = lm(as.formula(paste(usage_y, "~ tech_optimism + secular_rational + econ_statism + log_gdp")), data = merged),
  m4 = lm(as.formula(paste(usage_y, "~ schwartz_openness + schwartz_conservation + log_gdp")), data = merged)
)

# ---- Plots ----
plot_dir <- "figures"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

`%||%` <- function(a, b) if (!is.null(a)) a else b
plot_caption_base <- "Data: Anthropic Economic Index (2025), World Values Survey (2017-22)\nAuthor: Soubhik Barari"
plot_caption <- function(caption = NULL) {
  if (is.null(caption) || caption == "") {
    plot_caption_base
  } else {
    paste0(caption, "\n", plot_caption_base)
  }
}

save_plot <- function(plot, filename_base, width = 7, height = 5) {
  ggsave(filename = file.path(plot_dir, paste0(filename_base, ".png")),
         plot = plot, width = width, height = height, dpi = 160)
  if (requireNamespace("svglite", quietly = TRUE)) {
    ggsave(filename = file.path(plot_dir, paste0(filename_base, ".svg")),
           plot = plot, width = width, height = height, device = svglite::svglite)
  } else {
    message("svglite not installed; skipping svg for ", filename_base)
  }
}

plot_scatter <- function(df, xvar, yvar, fname, xlab = NULL, ylab = NULL, caption = NULL) {
  p <- ggplot(df, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(alpha = 0.7, size = 2) +
    ggrepel::geom_text_repel(aes(label = geo_name), size = 2.6, max.overlaps = 25) +
    geom_smooth(method = "lm", se = TRUE, color = "#2C3E50") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold")) +
    labs(
      x = xlab %||% xvar,
      y = ylab %||% yvar,
      title = paste("AI usage vs", xvar),
      caption = plot_caption(caption)
    )
  print(p)
  save_plot(p, fname, width = 7, height = 5)
}

plot_scatter(merged, "tech_optimism", usage_y, "ai_usage_vs_tech_optimism",
             xlab = "Tech optimism (z)", ylab = usage_label)
plot_scatter(merged, "secular_rational", usage_y, "ai_usage_vs_secular_rational",
             xlab = "Secular-rational (z)", ylab = usage_label,
             caption = "Secular-rational index = mean of Welzel disbelief items: inverse importance of religion (Y012A), inverse religious person (Y012B), inverse religious practice (Y012C).")
plot_scatter(merged, "econ_statism", usage_y, "ai_usage_vs_econ_statism",
             xlab = "Economic statism (z)", ylab = usage_label)

merged <- merged %>% mutate(atheism = F034_mean)
plot_scatter(merged, "atheism", usage_y, "ai_usage_vs_religiosity",
             xlab = "Religious person (higher = more atheistic)", ylab = usage_label)

plot_scatter(merged, "log_gdp", usage_y, "ai_usage_vs_gdp",
             xlab = "Log GDP per capita", ylab = usage_label)

# ---- Histogram: AI usage by WVS missingness ----
wvs_mean_cols <- paste0(plot_vars, "_mean")
wvs_mean_cols <- wvs_mean_cols[wvs_mean_cols %in% names(merged)]
if (length(wvs_mean_cols) > 0) {
  hist_df <- merged %>%
    mutate(
      wvs_missing = if_else(
        if_all(all_of(wvs_mean_cols), ~ is.na(.x)),
        "WVS values missing",
        "WVS values present"
      )
    ) %>%
    filter(!is.na(.data[[usage_y]]))

  hist_plot <- ggplot(hist_df, aes(x = .data[[usage_y]], fill = wvs_missing)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 24) +
    scale_fill_manual(values = c("WVS values present" = "#2C3E50", "WVS values missing" = "#E67E22")) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom") +
    labs(
      title = "AI usage distribution by WVS coverage",
      x = usage_label,
      y = "Count",
      fill = NULL,
      caption = plot_caption()
    )
  print(hist_plot)
  save_plot(hist_plot, "ai_usage_hist_wvs_missingness", width = 7, height = 5)
} else {
  message("No WVS mean columns found; skipping WVS missingness histogram.")
}

violin_plot <- hist_df %>%
  ggplot(aes(x=wvs_missing, y=.data[[usage_y]])) +
  geom_violin() +
  geom_jitter(width=0.2) +
  ggrepel::geom_text_repel(aes(label=geo_name), size = 2.6, max.overlaps = 25) +
  labs(x="", y=usage_label) +
  theme_bw()
print(violin_plot)
save_plot(violin_plot, "ai_usage_violin_wvs_missingness", width = 7, height = 5)

# Optional: alternative outcome (if available in merged)
# plot_scatter(merged, "F034_mean", "usage_per_capita_index_2025_08", "ai_usage_vs_religiosity_index",
#              xlab = "Religious person (higher = more atheistic)", ylab = "usage per capita (index)")


# ---- Coefficient pointranges ----
coef_df <- function(model, model_name) {
  cf <- summary(model)$coefficients
  est <- cf[, 1]
  se <- cf[, 2]
  pval <- cf[, 4]
  terms <- rownames(cf)

  ci <- tryCatch(confint(model), error = function(e) NULL)
  if (is.null(ci)) {
    conf_low <- est - 1.96 * se
    conf_high <- est + 1.96 * se
  } else {
    conf_low <- ci[, 1]
    conf_high <- ci[, 2]
  }

  tibble(
    model = model_name,
    term = terms,
    estimate = est,
    conf_low = conf_low,
    conf_high = conf_high,
    p_value = pval
  ) %>%
    filter(term != "(Intercept)")
}

# Note:
# "Religious person" is coded such that 3 corresponds to more atheistic

coef_plot_df <- bind_rows(lapply(names(models), function(nm) coef_df(models[[nm]], nm)))

coef_plot <- ggplot(coef_plot_df, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, color = "#999999", linewidth = 0.3) +
  geom_pointrange(aes(xmin = conf_low, xmax = conf_high), color = "#2C3E50") +
  facet_wrap(~ model, scales = "free_y") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    title = "Model coefficients (95% CI)",
    x = paste("Effect on", usage_label),
    y = NULL,
    caption = plot_caption()
  )
print(coef_plot)
save_plot(coef_plot, "coefficients_pointrange", width = 9, height = 6)

coef_plot_sig_df <- coef_plot_df %>% filter(p_value < 0.05)
if (nrow(coef_plot_sig_df) > 0) {
  coef_plot_sig <- ggplot(coef_plot_sig_df, aes(x = estimate, y = term)) +
    geom_vline(xintercept = 0, color = "#999999", linewidth = 0.3) +
    geom_pointrange(aes(xmin = conf_low, xmax = conf_high), color = "#2C3E50") +
    facet_wrap(~ model, scales = "free_y") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold")) +
    labs(
      title = "Significant coefficients (95% CI)",
      x = paste("Effect on", usage_label),
      y = NULL,
      caption = plot_caption()
    )
  print(coef_plot_sig)
  save_plot(coef_plot_sig, "coefficients_pointrange_significant", width = 9, height = 6)
} else {
  message("No significant coefficients to plot at p < 0.05")
}

# ---- Save outputs ----
output_dir <- "outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write_csv(corrs, file.path(output_dir, "wvs_ai_correlations.csv"))

# ---- Partial correlations plot (GDP-controlled) ----
wrap_label <- function(x, width = 40) {
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"), character(1))
}

partial_corr_stats <- function(y, x, gdp) {
  cc <- complete.cases(y, x, gdp)
  n <- sum(cc)
  if (n < 4) {
    return(list(r = NA_real_, n = n, lo = NA_real_, hi = NA_real_))
  }
  ry <- resid(lm(y[cc] ~ gdp[cc]))
  rx <- resid(lm(x[cc] ~ gdp[cc]))
  r <- cor(ry, rx, use = "pairwise.complete.obs")
  z <- atanh(r)
  se <- 1 / sqrt(n - 3)
  lo <- tanh(z - 1.96 * se)
  hi <- tanh(z + 1.96 * se)
  list(r = r, n = n, lo = lo, hi = hi)
}

plot_vars_df <- tibble(column = plot_vars) %>%
  left_join(labels_df, by = c("column" = "column")) %>%
  mutate(label = ifelse(is.na(label) | label == "", column, label))

label_gpt <- ellmer::chat_openai(
  paste0(
    "Make some nice visualization-friendly labels (<40 characters) for these raw question labels.",
    "They come from the World Values Survey.",
    "Do not output any commentary, just output the label."
  )
)
plot_vars_df$nicer_label <- NA
for (i in 1:nrow(plot_vars_df)) {
  plot_vars_df$nicer_label[i] <- 
    label_gpt$chat(paste0(plot_vars_df$column[i],": ", plot_vars_df$label[i]))
}

partial_corr_df <- bind_rows(lapply(plot_vars, function(v) {
  var_mean <- paste0(v, "_mean")
  if (!var_mean %in% names(merged)) return(NULL)
  s <- partial_corr_stats(merged[[usage_y]], merged[[var_mean]], merged$log_gdp)
  tibble(
    column = v,
    var_mean = var_mean,
    r = s$r,
    n = s$n,
    conf_low = s$lo,
    conf_high = s$hi
  )
})) %>%
  left_join(plot_vars_df, by = "column") %>%
  mutate(label_wrap = paste0(stringr::str_to_sentence(nicer_label))) %>%
  #mutate(label_wrap = wrap_label(label, width = 60)) %>%
  arrange(r)

write_csv(partial_corr_df, file.path(output_dir, "wvs_ai_partial_correlations.csv"))

pc_plot_df <- partial_corr_df %>% filter(!is.na(r))

pc_plot <- ggplot(pc_plot_df, aes(x = r, y = reorder(label_wrap, r))) +
  geom_vline(xintercept = 0, color = "#999999", linewidth = 0.3) +
  geom_pointrange(aes(xmin = conf_low, xmax = conf_high), color = "#2C3E50") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    title = "Partial correlations with AI usage",
    subtitle = "Controlling for GDP per capita",
    x = "Partial correlation (95% CI)",
    y = NULL,
    caption = plot_caption()
  )
print(pc_plot)
save_plot(pc_plot, "partial_correlations_wvs_ai_usage", width = 9, height = 11)

pc_plot2 <- pc_plot_df %>%
  # filter(!(conf_low < 0 & conf_high > 0)) %>%
  mutate(color = case_when(
    conf_low < 0 & conf_high > 0 ~ "grey",
    TRUE ~ "black"
  )) %>%
  ggplot(aes(x = r, y = reorder(label_wrap, r))) +
  geom_vline(xintercept = 0, color = "#999999", linewidth = 0.3) +
  geom_pointrange(aes(xmin = conf_low, xmax = conf_high, color = color, alpha = color)) +
  scale_color_identity() +
  scale_alpha_manual(values=c(1, 0.5)) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    title = "Controlling for GDP, Secular-Rationalism More Significantly Correlates\nwith AI Usage Than Other Values",
    subtitle = "Includes traditional Schwartz basic human value variables, attitudes about tech, economy, and faith",
    x = "Partial correlation (95% CI)",
    y = NULL,
    caption = plot_caption()
  ) + 
  theme(legend.position = "none")
print(pc_plot2)
save_plot(pc_plot2, "partial_sig_correlations_wvs_ai_usage", width = 9, height = 7)


model_txt <- file.path(output_dir, "model_summaries.txt")
model_lines <- unlist(lapply(names(models), function(nm) {
  c(
    paste0("=== ", nm, " ==="),
    capture.output(summary(models[[nm]])),
    ""
  )
}))
writeLines(model_lines, model_txt)

message("Done. Plots in ", plot_dir, " and summaries in ", output_dir)
