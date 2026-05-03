library(ggplot2)
library(dplyr)
library(tidyr)

# Run 01_data_cleaning.R before this script

# ── Distribution of standardized times by event ───────────────────────────────
ggplot(df_clean, aes(x = STD_TIME)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~EVENT) +
  labs(title = "Distribution of Standardized Times by Event")

# ── Unadjusted performance variation across facilities ────────────────────────
df_clean %>%
  group_by(LOCATION) %>%
  summarise(
    mean_z = mean(STD_TIME, na.rm = TRUE),
    se     = sd(STD_TIME, na.rm = TRUE) / sqrt(n()),
    n      = n()
  ) %>%
  ggplot(aes(x = mean_z, y = reorder(LOCATION, mean_z))) +
  geom_point(aes(size = n), color = "steelblue") +
  geom_errorbarh(aes(xmin = mean_z - 1.96 * se,
                     xmax = mean_z + 1.96 * se), height = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title    = "Unadjusted Pool Effect on Standardized Swim Times",
    subtitle = "Negative z-score = faster than average. No adjustment for team strength yet.",
    x        = "Mean Z-score",
    y        = "",
    size     = "# of Swims"
  ) +
  theme_minimal()

# ── Performance variation by meet type ────────────────────────────────────────
ggplot(df_clean, aes(x = LOCATION, y = STD_TIME)) +
  geom_boxplot(fill = "lightgreen") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Performance Variation Across Facilities")
