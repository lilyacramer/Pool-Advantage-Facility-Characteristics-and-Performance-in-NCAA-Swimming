library(dplyr)
library(stringr)
library(lubridate)

# ── Load raw data ──────────────────────────────────────────────────────────────
Time_Data     <- read.csv("~/Downloads/Time Data - Sheet1.csv")
Facility_Data <- read.csv("~/Desktop/Facility Data - Sheet1.csv")
Meet_Location <- read.csv("~/Desktop/Meet Location - Sheet1-2.csv")

# ── Drop championship meets ────────────────────────────────────────────────────
Time_Data <- Time_Data %>%
  filter(!grepl("NCAA|Championships|Champs|Cha|Championship", MEET, ignore.case = TRUE))

# ── Merge facility and location data ──────────────────────────────────────────
Location_Data <- merge(Facility_Data, Meet_Location, by = "LOCATION")

facilities_clean <- Location_Data %>%
  mutate(MEET = str_trim(MEET))

swims_clean <- Time_Data %>%
  mutate(MEET = str_trim(MEET))

final_df <- swims_clean %>%
  left_join(facilities_clean, by = "MEET") %>%
  filter(!is.na(LOCATION))

# ── Convert swim times to seconds ─────────────────────────────────────────────
final_df$SWIM.TIME <- trimws(as.character(final_df$SWIM.TIME))

valid_rows <- grepl("^\\d+(:\\d+)?(\\.\\d+)?$", final_df$SWIM.TIME)
df_clean   <- final_df[valid_rows, ]

df_clean$SWIM.TIME_SEC <- sapply(df_clean$SWIM.TIME, function(x) {
  parts <- as.numeric(unlist(strsplit(x, ":")))
  if (length(parts) == 2) parts[1] * 60 + parts[2]
  else if (length(parts) == 1) parts[1]
  else NA
})

# ── Feature engineering ───────────────────────────────────────────────────────
df_clean <- df_clean %>%
  mutate(
    DISTANCE      = as.numeric(str_extract(EVENT, "^[0-9]+")),
    STROKE        = str_extract(EVENT, "(?<=\\s).*(?=\\sSCY)"),
    SWIM_DATE     = as.Date(SWIM.DATE, format = "%m/%d/%Y"),
    SEASON        = case_when(
      month(SWIM_DATE) >= 9 ~ year(SWIM_DATE),
      TRUE                  ~ year(SWIM_DATE) - 1
    ),
    PLAYER_SEASON = paste(NAME, SEASON, sep = "_"),
    LOG_TIME      = log(SWIM.TIME_SEC)
  )

# ── Standardize swim times within each event ──────────────────────────────────
df_clean <- df_clean %>%
  group_by(EVENT) %>%
  mutate(
    SWIM_TIME_Z = as.numeric(scale(SWIM.TIME_SEC)),
    STD_TIME    = (SWIM.TIME_SEC - mean(SWIM.TIME_SEC, na.rm = TRUE)) /
                   sd(SWIM.TIME_SEC, na.rm = TRUE)
  ) %>%
  ungroup()

# ── Scale pool characteristics for comparability ──────────────────────────────
df_clean <- df_clean %>%
  mutate(
    Depth_Z    = as.numeric(scale(Depth..in.)),
    Altitude_Z = as.numeric(scale(Altitude..ft.)),
    Lanes_Z    = as.numeric(scale(Lanes)),
    Age_Z      = as.numeric(scale(Age..years.))
  )
