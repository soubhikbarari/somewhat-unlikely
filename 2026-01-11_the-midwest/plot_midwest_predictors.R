library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(broom)
library(forcats)
library(margins)
library(labelled)

source("utils.R")

midwest_svy <- readRDS("data/midwest_svy.rds")
midwest_responses_q <- read_excel(("data/middle-west-review-qs.xlsx")) %>%
  janitor::clean_names()

# --- STEP 1: Variable setup ---
message("setup")
discrete_vars <- c("age", "race", "gender", "education", "ideology", "partyregaff", "otehistory")
continuous_vars <- c("born")
control_vars <- c(discrete_vars, continuous_vars)
outcomes <- c("midwest", "midwesterner")

# --- STEP 2: Prepare dataset ---
message("data")
midwest_model_df <- midwest_responses_q %>%
  mutate(
    midwest = ifelse(midwest == 1, 1, 0),
    midwesterner = ifelse(midwesterner == 1, 1, 0)
  )

# --- STEP 3: Fit univariate models ---
message("models")
univariate_models <- expand.grid(outcome = outcomes, predictor = control_vars, stringsAsFactors = FALSE) %>%
  mutate(
    formula = purrr::map2(outcome, predictor, ~ {
      pred_term <- if (.y %in% discrete_vars) paste0("factor(", .y, ")") else .y
      as.formula(paste0(.x, " ~ ", pred_term))
    }),
    model = purrr::map(formula, ~ glm(.x, data = midwest_model_df, weights = weight, family = binomial))
  )

# --- STEP 4: Compute marginal effects (AMEs) ---
message("effects")
univariate_models <- univariate_models %>%
  mutate(
    margins = purrr::map(model, .progress = T, ~ suppressWarnings(as_tibble(margins::margins(.x, type = "response")))),
    tidy = purrr::map(margins, .progress = T, ~ {
      mdf <- .x
      # keep only dydx columns and reshape
      long <- mdf %>%
        select(starts_with("dydx_")) %>%
        pivot_longer(
          cols = everything(),
          names_to = "term",
          names_prefix = "dydx_",
          values_to = "dydx"
        )
      # also get CIs if available from the variance columns
      if (any(grepl("^Var_dydx_", names(mdf)))) {
        var_long <- mdf %>%
          select(starts_with("Var_dydx_")) %>%
          pivot_longer(
            cols = everything(),
            names_to = "term",
            names_prefix = "Var_dydx_",
            values_to = "variance"
          )
        long <- left_join(long, var_long, by = "term") %>%
          mutate(
            se = sqrt(variance),
            conf.low = dydx - 1.96 * se,
            conf.high = dydx + 1.96 * se
          )
      } else {
        long <- long %>%
          mutate(conf.low = NA, conf.high = NA)
      }
      return(long)
    })
  ) %>%
  unnest(tidy)

# --- STEP 5: Replace term with value labels ---
message("label")
get_label <- function(var, code) {
  lbls <- val_labels(midwest_responses_q[[var]], attr.only = TRUE)
  if (!is.null(lbls)) {
    name <- names(lbls)[lbls == code]
    if (!is.na(name) && length(name) == 1) return(name)
  }
  return(as.character(code))
}

univariate_models <- univariate_models %>%
  mutate(
    varname = gsub("factor\\((.*?)\\).*", "\\1", as.character(formula)),
    level = gsub(".*\\)", "", term),
    label = purrr::map2_chr(varname, level, ~ get_label(.x, as.numeric(.y))),
    outcome = recode(outcome,
                     midwest = "Identifies as living in Midwest",
                     midwesterner = "Identifies as Midwesterner")
  )

# --- STEP 6: Plot ---
message("plot")
ggplot(univariate_models, aes(x = dydx, y = fct_reorder(label, dydx))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(color = "black") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "black") +
  facet_wrap(~outcome, scales = "free_y") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Average Marginal Effect on Probability",
    y = NULL,
    title = "Univariate Predictors of Midwestern Identity",
    subtitle = "Estimated marginal effects from weighted logistic regressions",
    caption = "Estimates show the change in predicted probability associated with each category.\nContinuous variables show the change per unit."
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )
