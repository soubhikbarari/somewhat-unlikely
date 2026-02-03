# Prep: WVS values + Claude AI usage (country-level)
# Run this script before analysis_wvs_ai_usage.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

setwd(".")

# ---- Paths ----
# Prefer enriched file for GDP; raw file for later updates if needed.
# NOTE: README lists an output path for the 2026 release; in this repo the raw file lives in data/intermediate.

aei_enriched_path <- "data/anthropic-economic-index/release_2025_09_15/data/output/aei_enriched_claude_ai_2025-08-04_to_2025-08-11.csv"
aei_raw_path      <- "data/anthropic-economic-index/release_2026_01_15/data/intermediate/aei_raw_claude_ai_2025-11-13_to_2025-11-20.csv"

wvs_path <- "data/wvs/Trends_VS_1981_2022_rds_v4_0.rds"
wvs_labels_path <- "data/wvs/Trends_VS_1981_2022_labels_questions.csv"

if (!file.exists(aei_enriched_path)) {
  stop("Missing AEI enriched file: ", aei_enriched_path)
}
if (!file.exists(wvs_path)) {
  stop("Missing WVS file: ", wvs_path)
}
if (!file.exists(wvs_labels_path)) {
  stop("Missing WVS labels file: ", wvs_labels_path)
}

# ---- Load AEI (country-level usage + GDP) ----
print(aei_enriched_path)
aei <- read_csv(aei_enriched_path, show_col_types = FALSE)

ai_vars <- c("usage_per_capita", "usage_per_capita_index", "usage_pct", "usage_count")

aei_country <- aei %>%
  filter(geography == "country", variable %in% c("gdp_per_working_age_capita", ai_vars)) %>%
  select(geo_id, geo_name, date_end, variable, value) %>%
  group_by(geo_id, geo_name, variable) %>%
  summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename_at(ai_vars, ~paste0(.x, "_2025_08"))

aei_country$usage_per_capita_2025_08
aei_country$usage_per_capita_index_2025_08

# Optional: use the newer raw file for usage, then join GDP from the enriched file.
library(countrycode)

if (file.exists(aei_raw_path)) {
  print(aei_raw_path)
  aei_raw <- read_csv(aei_raw_path, show_col_types = FALSE)

  aei_raw_country <- aei_raw %>%
    filter(geography == "country", variable %in% c("usage_count", "usage_pct")) %>%
    select(geo_id, date_end, variable, value) %>%
    group_by(geo_id, variable) %>%
    summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = variable, values_from = value)

  aei_raw_country$iso3_code <- countrycode(aei_raw_country$geo_id, origin = 'iso2c', destination = 'iso3c')
  aei_raw_country$country_name <- countrycode(aei_raw_country$geo_id, origin = 'iso2c', destination = 'country.name.en')
  
  aei_country <- aei_raw_country %>%
    select(iso3_code, usage_count_2025_11 = usage_count, usage_pct_2025_11 = usage_pct) %>%
    left_join(aei_country %>%
      select(geo_id, geo_name, gdp_per_working_age_capita, all_of(paste0(ai_vars,"_2025_08"))), 
    by = c("iso3_code" = "geo_id"))
}

colnames(aei_country)


# ---- Load WVS and build country-level values (latest year per country) ----
wvs <- readRDS(wvs_path)
length(unique(wvs$COUNTRY_ALPHA))

wvs_vars <- c(
  # Tech/science and faith
  "E217", "E218", "E234", "E220",
  # Schwartz values
  "A189", "A190", "A191", "A192", "A193", "A194", "A195", "A196", "A197", "A198",
  # Economic values
  "E032", "E035", "E037",
  # Additional controls / related values
  "A165", "G006", "Y012A", "Y012B", "Y012C"
)

# Extra WVS items for partial-correlation plot
plot_vars <- c(
  "E217", "E218", "E234", "E220",
  "A165", "G007_36_B",
  "E235", "E123", "E110", "E114", "E116", "E119",
  "B001", "B003", "B008",
  "C001", "C002",
  "A124_06", "A124_09", "A124_12",
  "F063", "F050", "F034",
  "D059", "D078", "D063",
  "F118", "F120", "F121",
  "E035", "E032", "E037",
  "G006"
)

wvs_vars <- unique(c(wvs_vars, plot_vars))

clean_num <- function(x) {
  x <- as.numeric(as.character(x))
  x[x < 0] <- NA
  x
}

wvs_sel <- wvs %>%
  select(COUNTRY_ALPHA, S020, pwgt, all_of(wvs_vars)) %>%
  transmute(
    country_iso3 = as.character(COUNTRY_ALPHA),
    year = as.numeric(as.character(labelled::unlabelled(S020))),
    weight = ifelse(is.na(as.numeric(pwgt)), 1, as.numeric(pwgt)),
    across(all_of(wvs_vars), clean_num)
  )

wvs_country_year <- wvs_sel %>%
  group_by(country_iso3, year) %>%
  summarize(
    across(all_of(wvs_vars), ~ weighted.mean(.x, weight, na.rm = TRUE), .names = "{.col}_mean"),
    n = sum(!is.na(A165)),
    .groups = "drop"
  )

wvs_latest <- wvs_country_year %>%
  group_by(country_iso3) %>%
  slice_max(year, with_ties = FALSE) %>%
  ungroup()

# ---- Indices (z-scored across countries) ----
z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

wvs_latest <- wvs_latest %>%
  mutate(
    tech_optimism = rowMeans(cbind(z(E217_mean), z(E218_mean), z(E234_mean)), na.rm = TRUE),
    tech_faith_tension = -z(E220_mean),
    secular_rational = rowMeans(cbind(z(Y012A_mean), z(Y012B_mean), z(Y012C_mean)), na.rm = TRUE),
    schwartz_openness = rowMeans(cbind(z(A189_mean), z(A192_mean), z(A195_mean)), na.rm = TRUE),
    schwartz_conservation = rowMeans(cbind(z(A191_mean), z(A196_mean), z(A198_mean)), na.rm = TRUE),
    econ_statism = rowMeans(cbind(z(E032_mean), z(E035_mean), z(E037_mean)), na.rm = TRUE)
  )

# ---- Merge (keep all AEI countries) ----
print(names(aei_country))
has_usage_per_capita <- "usage_per_capita_2025_08" %in% names(aei_country)
has_usage_pct <- "usage_pct_2025_08" %in% names(aei_country)
has_usage_count <- "usage_count_2025_08" %in% names(aei_country)

aei_country %>%
  anti_join(wvs_latest, by = c("iso3_code" = "country_iso3")) %>%
  mutate(name = countrycode(iso3_code, origin = "iso3c", destination = "country.name.en")) %>%
  select(iso3_code, name) %>%
  as.data.frame()

merged <- aei_country %>%
  left_join(wvs_latest, by = c("iso3_code" = "country_iso3")) %>%
  mutate(
    log_usage_per_capita = if (has_usage_per_capita) log1p(usage_per_capita_2025_08) else NA_real_,
    log_usage_pct = if (has_usage_pct) log1p(usage_pct_2025_08) else NA_real_,
    log_usage_count = if (has_usage_count) log1p(usage_count_2025_08) else NA_real_,
    log_gdp = log1p(gdp_per_working_age_capita)
  )

stopifnot(any(!is.na(merged$tech_optimism)))
stopifnot(any(!is.na(merged$gdp_per_working_age_capita)))

# ---- Coverage outputs ----
output_dir <- "outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

coverage <- merged %>%
  mutate(has_wvs = !is.na(tech_optimism)) %>%
  summarize(
    n_aei_countries = n_distinct(iso3_code),
    n_with_wvs = sum(has_wvs, na.rm = TRUE),
    share_with_wvs = n_with_wvs / n_aei_countries
  )

coverage_by_country <- merged %>%
  select(iso3_code, geo_name, has_wvs = tech_optimism) %>%
  mutate(has_wvs = !is.na(has_wvs))

write_csv(coverage, file.path(output_dir, "aei_wvs_coverage_summary.csv"))
write_csv(coverage_by_country, file.path(output_dir, "aei_wvs_coverage_by_country.csv"))

# ---- Save prep data ----
labels_df <- read_csv(wvs_labels_path, show_col_types = FALSE) %>%
  select(column, label = column_label) %>%
  distinct()

prep_path <- file.path(output_dir, "wvs_ai_prep.rds")
saveRDS(
  list(
    aei_country = aei_country,
    merged = merged,
    # usage_y = usage_y,
    # usage_label = usage_label,
    plot_vars = plot_vars,
    labels = labels_df
  ),
  prep_path
)

message("Prep done. Saved ", prep_path)
