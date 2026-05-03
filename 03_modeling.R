library(lme4)
library(lmerTest)
library(broom.mixed)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(car)
library(modelsummary)

# Run 01_data_cleaning.R before this script

# ── VIF check for multicollinearity ───────────────────────────────────────────
vif_model <- lm(
  SWIM_TIME_Z ~ Depth..in. + Altitude..ft. + Lanes + Age..years. + Indoor +
    STROKE + DISTANCE,
  data = df_clean
)
vif(vif_model)

# ── Primary model ─────────────────────────────────────────────────────────────
model_scaled <- lmer(
  SWIM_TIME_Z ~
    Depth_Z + Altitude_Z + Lanes_Z + Age_Z + Indoor +
    STROKE + DISTANCE +
    (1 | TEAM) + (1 | PLAYER_SEASON),
  data = df_clean,
  REML = TRUE
)
summary(model_scaled)

# ── Pool fixed effects model (for AIC comparison and pool rankings) ────────────
model_pool_fe <- lmer(
  SWIM_TIME_Z ~
    STROKE + DISTANCE +
    LOCATION +
    (1 | TEAM) + (1 | PLAYER_SEASON),
  data = df_clean,
  REML = FALSE
)

model_scaled_ml <- lmer(
  SWIM_TIME_Z ~
    Depth_Z + Altitude_Z + Lanes_Z + Age_Z + Indoor +
    STROKE + DISTANCE +
    (1 | TEAM) + (1 | PLAYER_SEASON),
  data = df_clean,
  REML = FALSE
)

AIC(model_scaled_ml, model_pool_fe)

# ── Robustness: log time and percentile specifications ────────────────────────
model_log <- lmer(
  LOG_TIME ~
    Depth_Z + Altitude_Z + Lanes_Z + Age_Z + Indoor +
    STROKE + DISTANCE +
    (1 | TEAM) + (1 | PLAYER_SEASON),
  data = df_clean,
  REML = TRUE
)

df_clean <- df_clean %>%
  group_by(EVENT) %>%
  mutate(PCT_TIME = percent_rank(SWIM.TIME_SEC)) %>%
  ungroup()

model_pct <- lmer(
  PCT_TIME ~
    Depth_Z + Altitude_Z + Lanes_Z + Age_Z + Indoor +
    STROKE + DISTANCE +
    (1 | TEAM) + (1 | PLAYER_SEASON),
  data = df_clean,
  REML = TRUE
)

modelsummary(
  list("Z-score" = model_scaled,
       "Log Time" = model_log,
       "Percentile" = model_pct),
  stars     = TRUE,
  coef_omit = "STROKE|DISTANCE",
  title     = "Robustness: Alternative Outcome Specifications"
)

# ── Figure 1: Coefficient plot ─────────────────────────────────────────────────
coef_df <- tidy(model_scaled, effects = "fixed", conf.int = TRUE) %>%
  filter(term %in% c("Depth_Z", "Altitude_Z", "Lanes_Z", "Age_Z", "Indoor")) %>%
  mutate(
    term_clean = case_when(
      term == "Depth_Z"    ~ "Pool Depth",
      term == "Altitude_Z" ~ "Altitude",
      term == "Lanes_Z"    ~ "Number of Lanes",
      term == "Age_Z"      ~ "Facility Age",
      term == "Indoor"     ~ "Indoor Pool",
      TRUE                 ~ term
    ),
    significant = !(conf.low < 0 & conf.high > 0)
  )

x_range <- range(c(coef_df$conf.low, coef_df$conf.high), na.rm = TRUE)

ggplot(coef_df, aes(x = estimate, y = reorder(term_clean, estimate),
                    color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50",
             linewidth = 0.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 1.2, alpha = 0.4) +
  geom_point(size = 5) +
  annotate("text", x = x_range[1], y = 0.4, label = "\u2190 Faster",
           color = "steelblue", size = 4, fontface = "italic", hjust = 0) +
  annotate("text", x = x_range[2], y = 0.4, label = "Slower \u2192",
           color = "tomato", size = 4, fontface = "italic", hjust = 1) +
  scale_color_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "gray70"),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "Not significant")
  ) +
  labs(
    title    = "Effect of Facility Characteristics on Swim Performance",
    subtitle = "Gray = effect not statistically significant. Bars show 95% confidence interval.",
    x        = "Effect on Standardized Swim Time",
    y        = NULL,
    color    = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position        = "bottom",
    panel.grid.minor       = element_blank(),
    panel.grid.major.y     = element_blank()
  )

ggsave("figures/coef_plot.png", width = 8, height = 5, dpi = 300)

# ── Extract pool fixed effects for rankings and app ───────────────────────────
pool_effects <- fixef(model_pool_fe)

pool_df <- data.frame(
  LOCATION = names(pool_effects)[grepl("LOCATION", names(pool_effects))],
  effect   = pool_effects[grepl("LOCATION", names(pool_effects))]
) %>%
  mutate(LOCATION = str_remove(LOCATION, "LOCATION")) %>%
  arrange(effect)

pool_df <- pool_df %>%
  left_join(
    df_clean %>%
      select(LOCATION, Depth_in = Depth..in., Altitude_ft = Altitude..ft.,
             Age_years = Age..years., Lanes, Indoor) %>%
      distinct(),
    by = "LOCATION"
  )

# ── Figure 2: Christiansburg contribution plot ────────────────────────────────
fastest_pool <- pool_df %>% slice(1) %>% pull(LOCATION)

fastest_data <- df_clean %>%
  filter(LOCATION == fastest_pool) %>%
  select(Depth_Z, Altitude_Z, Lanes_Z, Age_Z, Indoor) %>%
  distinct() %>%
  slice(1)

coefs <- fixef(model_scaled)[c("Depth_Z", "Altitude_Z", "Lanes_Z",
                                "Age_Z", "Indoor")]

contrib_df <- data.frame(
  term     = names(coefs),
  coef     = as.numeric(coefs),
  pool_val = as.numeric(fastest_data[names(coefs)])
) %>%
  mutate(
    contribution = coef * pool_val,
    term_clean   = case_when(
      term == "Depth_Z"    ~ "Pool Depth",
      term == "Altitude_Z" ~ "Altitude",
      term == "Lanes_Z"    ~ "Number of Lanes",
      term == "Age_Z"      ~ "Facility Age",
      term == "Indoor"     ~ "Indoor Pool",
      TRUE                 ~ term
    ),
    direction = ifelse(contribution < 0, "Faster", "Slower")
  ) %>%
  arrange(contribution)

ggplot(contrib_df, aes(x = contribution,
                       y = reorder(term_clean, -contribution),
                       fill = direction)) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_text(aes(label = round(contribution, 3),
                hjust = ifelse(contribution < 0, 1.2, -0.2)),
            size = 4, color = "gray20") +
  scale_fill_manual(values = c("Faster" = "steelblue", "Slower" = "tomato")) +
  labs(
    title    = paste("Why is", fastest_pool, "the Fastest Pool?"),
    subtitle = "Each bar = that characteristic's contribution to speed (coefficient \u00d7 pool value)",
    x        = "Contribution to Swim Time (SD units)",
    y        = NULL,
    fill     = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position        = "bottom",
    panel.grid.minor       = element_blank(),
    panel.grid.major.y     = element_blank()
  )

ggsave("figures/contribution_plot.png", width = 8, height = 5, dpi = 300)

# ── Team random effects ────────────────────────────────────────────────────────
team_re <- ranef(model_scaled)$TEAM %>%
  tibble::rownames_to_column("TEAM") %>%
  rename(team_effect = `(Intercept)`) %>%
  arrange(team_effect)

ggplot(team_re, aes(x = team_effect, y = reorder(TEAM, team_effect))) +
  geom_point(color = "steelblue", size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title    = "Team Random Effects",
    subtitle = "Negative = faster program after controlling for pool",
    x        = "Random Intercept (Z-score units)",
    y        = ""
  ) +
  theme_minimal(base_size = 13)

# ── Variance decomposition ─────────────────────────────────────────────────────
as.data.frame(VarCorr(model_scaled)) %>%
  select(grp, vcov) %>%
  mutate(pct_variance = round(vcov / sum(vcov) * 100, 1))

# ── Save pool_df for Shiny app ────────────────────────────────────────────────
saveRDS(pool_df, "app/pool_df.rds")
