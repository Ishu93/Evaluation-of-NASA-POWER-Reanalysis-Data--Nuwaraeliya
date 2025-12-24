cat("\f")
rm(list = ls())

# --- Load libraries ---
library(ggplot2)
library(dplyr)
library(tidyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# --- Load data ---
file_path <- file.choose()   # Select your CSV file
df <- read.csv(file_path)

# Ensure proper column names
# Expected: "Month", "Year", "Day", "Obs", "NASA"
str(df)

# --- Define seasons ---
seasons <- list(
  FIM = c(3, 4),          # First Inter-monsoon
  SWM = 5:9,              # Southwest Monsoon
  SIM = c(10, 11),        # Second Inter-monsoon
  NEM = c(12, 1, 2)       # Northeast Monsoon
)

# --- Assign Season for each row ---
df <- df %>%
  mutate(Season = case_when(
    Month %in% seasons$FIM ~ "FIM",
    Month %in% seasons$SWM ~ "SWM",
    Month %in% seasons$SIM ~ "SIM",
    Month %in% seasons$NEM ~ "NEM",
    TRUE ~ NA_character_
  ))

# --- Reshape for plotting ---
df_long <- df %>%
  pivot_longer(
    cols = c("Obs", "NASA"),
    names_to = "Dataset",
    values_to = "PP"
  )

# --- 1. Monthly Boxplot ---
p1 <- ggplot(df_long, aes(x = factor(Month), y = PP, fill = Dataset)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  scale_x_discrete(labels = month.name[1:12]) +
  scale_fill_manual(
    values = c("Obs" = "#1f77b4", "NASA" = "#ff7f0e"),
    labels = c("Obs" = "Observed Precipitation (mm)",
               "NASA" = "NASA POWER Precipitation (mm)")
  ) +
  labs(x = "Month", y = "Daily Precipitation (mm)", fill = "") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

ggsave("Boxplot_Monthly_PP.png", p1, width = 7, height = 4.5, dpi = 300)


# --- Case 1: Obs > 0 (Wet days only) ---
df_wet <- df %>% filter(Obs > 0)

df_wet_long <- df_wet %>%
  pivot_longer(cols = c("Obs", "NASA"),
               names_to = "Dataset",
               values_to = "PP")

p_wet <- ggplot(df_wet_long, aes(x = factor(Month), y = PP, fill = Dataset)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  scale_x_discrete(labels = month.name[1:12]) +
  scale_fill_manual(values = c("Obs" = "#1f77b4", "NASA" = "#ff7f0e")) +
  labs(title = "Boxplot of Wet Days (Obs > 0)",
       x = "Month", y = "Daily Precipitation (mm)", fill = "") +
  theme_bw(base_size = 16) +   # bigger base font
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  )

ggsave("Boxplot_WetDays.png", p_wet, width = 7, height = 4.5, dpi = 600)


# --- Case 2: Obs == 0 (Dry days) ---
df_dry <- df %>% filter(Obs == 0) %>%
  select(Month, NASA) %>%
  mutate(Dataset = "NASA_when_Obs0")

p_dry <- ggplot(df_dry, aes(x = factor(Month), y = NASA, fill = Dataset)) +
  geom_boxplot(width = 0.6) +
  scale_x_discrete(labels = month.name[1:12]) +
  scale_fill_manual(values = c("NASA_when_Obs0" = "#ff7f0e")) +
  labs(title = "Boxplot of NASA Precipitation on Dry Days (Obs = 0)",
       x = "Month", y = "Daily Precipitation (mm)", fill = "") +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  )

ggsave("Boxplot_DryDays.png", p_dry, width = 7, height = 4.5, dpi = 600)




# --- 2. Seasonal Boxplot ---
p2 <- ggplot(df_long, aes(x = Season, y = PP, fill = Dataset)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  scale_fill_manual(
    values = c("Obs" = "#1f77b4", "NASA" = "#ff7f0e"),
    labels = c("Obs" = "Observed Precipitation (mm)",
               "NASA" = "NASA POWER Precipitation (mm)")
  ) +
  labs(x = "Season", y = "Daily Precipitation (mm)", fill = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

ggsave("Boxplot_Seasonal_PP.png", p2, width = 7, height = 4.5, dpi = 300)

# --- Helper function for PBIAS ---
pbias <- function(obs, sim) {
  return(100 * sum(sim - obs, na.rm = TRUE) / sum(obs, na.rm = TRUE))
}

# --- 3. Monthly statistics ---
monthly_stats <- df %>%
  group_by(Month) %>%
  summarise(
    ME = mean(Obs - NASA, na.rm = TRUE),
    RMSE = sqrt(mean((Obs - NASA)^2, na.rm = TRUE)),
    R = cor(Obs, NASA, use = "complete.obs"),
    PBIAS = pbias(Obs, NASA),
    Obs_mean = mean(Obs, na.rm = TRUE),
    NASA_mean = mean(NASA, na.rm = TRUE),
    .groups = "drop"
  )

print("Monthly Statistics:")
print(monthly_stats)

# --- 4. Seasonal statistics ---
seasonal_stats <- df %>%
  group_by(Season) %>%
  summarise(
    ME = mean(Obs - NASA, na.rm = TRUE),
    RMSE = sqrt(mean((Obs - NASA)^2, na.rm = TRUE)),
    R = cor(Obs, NASA, use = "complete.obs"),
    PBIAS = pbias(Obs, NASA),
    Obs_mean = mean(Obs, na.rm = TRUE),
    NASA_mean = mean(NASA, na.rm = TRUE),
    .groups = "drop"
  )

print("Seasonal Statistics:")
print(seasonal_stats)

# --- 5. Overall statistics ---
overall_stats <- df %>%
  summarise(
    ME = mean(Obs - NASA, na.rm = TRUE),
    RMSE = sqrt(mean((Obs - NASA)^2, na.rm = TRUE)),
    R = cor(Obs, NASA, use = "complete.obs"),
    PBIAS = pbias(Obs, NASA),
    Obs_mean = mean(Obs, na.rm = TRUE),
    NASA_mean = mean(NASA, na.rm = TRUE)
  )

print("Overall Statistics:")
print(overall_stats)

# --- 6. Export results ---
write.csv(monthly_stats, "Monthly_PP_Stats.csv", row.names = FALSE)
write.csv(seasonal_stats, "Seasonal_PP_Stats.csv", row.names = FALSE)
write.csv(overall_stats, "Overall_PP_Stats.csv", row.names = FALSE)



###################################

# --- Load libraries ---
library(ggplot2)
library(dplyr)
library(tidyr)

# --- Load data ---
file_path <- file.choose()   # Select your CSV file
df <- read.csv(file_path)

# Columns: "Month", "Year", "Day", "Obs", "NASA"

# --- Define seasons ---
seasons <- list(
  FIM = c(3, 4),          # First Inter-monsoon
  SWM = 5:9,              # Southwest Monsoon
  SIM = c(10, 11),        # Second Inter-monsoon
  NEM = c(12, 1, 2)       # Northeast Monsoon
)

# Assign Season
df <- df %>%
  mutate(Season = case_when(
    Month %in% seasons$FIM ~ "FIM",
    Month %in% seasons$SWM ~ "SWM",
    Month %in% seasons$SIM ~ "SIM",
    Month %in% seasons$NEM ~ "NEM",
    TRUE ~ NA_character_
  ))

# Reshape for plotting
df_long <- df %>%
  pivot_longer(
    cols = c("Obs", "NASA"),
    names_to = "Dataset",
    values_to = "PP"
  )

# ==========================================================
# 1. Histogram / Density plots (distribution of daily rainfall)
# ==========================================================
p_hist <- ggplot(df_long, aes(x = PP, fill = Dataset, color = Dataset)) +
  geom_histogram(aes(y = ..density..), bins = 40, alpha = 0.4, position = "identity") +
  geom_density(alpha = 0.6, size = 1) +
  xlim(0, quantile(df_long$PP, 0.99, na.rm = TRUE)) + # cut off extreme outliers
  scale_fill_manual(values = c("Obs" = "#1f77b4", "NASA" = "#ff7f0e")) +
  scale_color_manual(values = c("Obs" = "#1f77b4", "NASA" = "#ff7f0e")) +
  labs(title = "Distribution of Daily Rainfall",
       x = "Daily Rainfall (mm)", y = "Density") +
  theme_bw(base_size = 14)

ggsave("Histogram_Density_Rainfall.png", p_hist, width = 7, height = 5, dpi = 300)

# ==========================================================
# 2. ECDF plot (cumulative distribution function)
# ==========================================================
p_ecdf <- ggplot(df_long, aes(x = PP, color = Dataset)) +
  stat_ecdf(size = 1) +
  xlim(0, quantile(df_long$PP, 0.99, na.rm = TRUE)) +
  scale_color_manual(values = c("Obs" = "#1f77b4", "NASA" = "#ff7f0e")) +
  labs(title = "Empirical CDF of Daily Rainfall",
       x = "Daily Rainfall (mm)", y = "Cumulative Probability") +
  theme_bw(base_size = 14)

ggsave("ECDF_Rainfall.png", p_ecdf, width = 7, height = 5, dpi = 300)

# ==========================================================
# 3. Rainfall occurrence probability (wet vs dry days)
# ==========================================================
occurrence <- df_long %>%
  mutate(WetDay = ifelse(PP >= 1, "Wet (≥1 mm)", "Dry (<1 mm)")) %>%
  group_by(Dataset, WetDay) %>%
  summarise(Days = n(), .groups = "drop") %>%
  group_by(Dataset) %>%
  mutate(Percent = 100 * Days / sum(Days))

p_occ <- ggplot(occurrence, aes(x = Dataset, y = Percent, fill = WetDay)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  scale_fill_manual(values = c("Dry (<1 mm)" = "grey70", "Wet (≥1 mm)" = "steelblue")) +
  labs(title = "Rainfall Occurrence Probability",
       x = "", y = "Percentage of Days (%)", fill = "Day Type") +
  theme_bw(base_size = 14)

ggsave("Rainfall_Occurrence.png", p_occ, width = 6, height = 5, dpi = 300)

# ==========================================================
# Export occurrence data
# ==========================================================
write.csv(occurrence, "Rainfall_Occurrence_Stats.csv", row.names = FALSE)

########### Seosonal analysis############
# ==========================================================
# 1. Histograms / Density plots (Season-wise)
# ==========================================================
p_hist_season <- ggplot(df_long, aes(x = PP, fill = Dataset, color = Dataset)) +
  geom_histogram(aes(y = ..density..), bins = 40, alpha = 0.3, position = "identity") +
  geom_density(alpha = 0.6, size = 1) +
  facet_wrap(~Season, scales = "free_y") +
  xlim(0, quantile(df_long$PP, 0.99, na.rm = TRUE)) +
  scale_fill_manual(values = c("Obs" = "#1f77b4", "NASA" = "#ff7f0e")) +
  scale_color_manual(values = c("Obs" = "#1f77b4", "NASA" = "#ff7f0e")) +
  labs(title = "Seasonal Distribution of Daily Rainfall",
       x = "Daily Rainfall (mm)", y = "Density") +
  theme_bw(base_size = 14)

ggsave("Histogram_Density_Rainfall_Seasonal.png", p_hist_season, width = 10, height = 7, dpi = 300)

# ==========================================================
# 2. ECDF plots (Season-wise)
# ==========================================================
p_ecdf_season <- ggplot(df_long, aes(x = PP, color = Dataset)) +
  stat_ecdf(size = 1) +
  facet_wrap(~Season, scales = "free_y") +
  xlim(0, quantile(df_long$PP, 0.99, na.rm = TRUE)) +
  scale_color_manual(values = c("Obs" = "#1f77b4", "NASA" = "#ff7f0e")) +
  labs(title = "Seasonal Empirical CDF of Daily Rainfall",
       x = "Daily Rainfall (mm)", y = "Cumulative Probability") +
  theme_bw(base_size = 14)

ggsave("ECDF_Rainfall_Seasonal.png", p_ecdf_season, width = 10, height = 7, dpi = 300)

# ==========================================================
# 3. Rainfall occurrence probability (Season-wise)
# ==========================================================
occurrence_season <- df_long %>%
  mutate(WetDay = ifelse(PP >= 1, "Wet (≥1 mm)", "Dry (<1 mm)")) %>%
  group_by(Season, Dataset, WetDay) %>%
  summarise(Days = n(), .groups = "drop") %>%
  group_by(Season, Dataset) %>%
  mutate(Percent = 100 * Days / sum(Days))

p_occ_season <- ggplot(occurrence_season, aes(x = Dataset, y = Percent, fill = WetDay)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  facet_wrap(~Season) +
  scale_fill_manual(values = c("Dry (<1 mm)" = "grey70", "Wet (≥1 mm)" = "steelblue")) +
  labs(title = "Seasonal Rainfall Occurrence Probability",
       x = "", y = "Percentage of Days (%)", fill = "Day Type") +
  theme_bw(base_size = 14)

ggsave("Rainfall_Occurrence_Seasonal.png", p_occ_season, width = 10, height = 7, dpi = 300)

# ==========================================================
# Export occurrence probability table
# ==========================================================
write.csv(occurrence_season, "Rainfall_Occurrence_Seasonal_Stats.csv", row.names = FALSE)


# --- 3a. Monthly statistics for Wet Days (Obs > 0) ---
monthly_stats_wet <- df %>%
  filter(Obs > 0) %>%
  group_by(Month) %>%
  summarise(
    ME = mean(abs(Obs - NASA), na.rm = TRUE),                      # Mean Error
    RMSE = sqrt(mean((Obs - NASA)^2, na.rm = TRUE)),               # Root Mean Square Error
    R = cor(Obs, NASA, use = "complete.obs"),                      # Correlation
    Obs_mean = mean(Obs, na.rm = TRUE),
    NASA_mean = mean(NASA, na.rm = TRUE),
    Obs_median = median(Obs, na.rm = TRUE),
    NASA_median = median(NASA, na.rm = TRUE),
    Obs_sd = sd(Obs, na.rm = TRUE),
    NASA_sd = sd(NASA, na.rm = TRUE),
    Count_WetDays = n(),
    .groups = "drop"
  )

print(monthly_stats_wet)

write.csv(monthly_stats_wet, "wet days stats.csv", row.names = FALSE)
# --- 3b. Monthly statistics for Dry Days (Obs == 0) ---
monthly_stats_dry <- df %>%
  filter(Obs == 0) %>%
  group_by(Month) %>%
  summarise(
    NASA_mean = mean(NASA, na.rm = TRUE),
    NASA_median = median(NASA, na.rm = TRUE),
    NASA_sd = sd(NASA, na.rm = TRUE),
    Count_DryDays = n(),
    .groups = "drop"
  )

print(monthly_stats_dry)

# --- Seasonal cumulative rainfall ---
seasonal_cum <- df %>%
  group_by(Season) %>%
  summarise(
    Obs_cum = sum(Obs, na.rm = TRUE),      # Observed cumulative rainfall
    NASA_cum = sum(NASA, na.rm = TRUE),    # NASA cumulative rainfall
    Count_days = n(),                       # Number of days in season
    .groups = "drop"
  )

print(seasonal_cum)

write.csv(seasonal_cum, "seaonal total rainfall.csv", row.names = FALSE)
