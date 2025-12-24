cat("\f")
rm(list = ls())

# --- Load libraries ---
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)
library(openxlsx)   # for Excel export

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# --- Load data ---
file_path <- file.choose()   # Opens file dialog
df <- read.csv(file_path)

# --- Reshape data for plotting ---
df_long <- df %>%
  pivot_longer(
    cols = c("Obs", "NASA"),
    names_to = "Dataset",
    values_to = "Temp"
  )

# --- 1. Boxplot by month ---
p1 <- ggplot(df_long, aes(x = factor(Month), y = Temp, fill = Dataset)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  scale_x_discrete(labels = month.name[1:12]) +
  scale_fill_manual(
    values = c("Obs" = "#1f77b4", "NASA" = "#ff7f0e"),
    labels = c("Obs" = "Observed Temp.",
               "NASA" = "NASA POWER Temp.")
  ) +
  labs(
    x = "Month",
    y = "Temperature (°C)",
    fill = ""
  ) +
  theme_bw(base_size = 16) +   # bigger base font (like p_wet)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))  # legend in one row

# Save high-resolution figure
ggsave("Boxplot_Temp_Comparison.png", p1, width = 7, height = 4.5, dpi = 600)

# --- 2. Monthly statistics (ME, RMSE, R) ---
monthly_stats <- df %>%
  group_by(Month) %>%
  summarise(
    ME = mean(abs(Obs - NASA), na.rm = TRUE),
    RMSE = sqrt(mean((Obs - NASA)^2, na.rm = TRUE)),
    R = cor(Obs, NASA, use = "complete.obs"),
    
    # Additional descriptive stats
    Obs_mean = mean(Obs, na.rm = TRUE),
    Obs_sd = sd(Obs, na.rm = TRUE),
    Obs_p25 = quantile(Obs, 0.25, na.rm = TRUE),
    Obs_p50 = quantile(Obs, 0.50, na.rm = TRUE),  # median
    Obs_p75 = quantile(Obs, 0.75, na.rm = TRUE),
    
    NASA_mean = mean(NASA, na.rm = TRUE),
    NASA_sd = sd(NASA, na.rm = TRUE),
    NASA_p25 = quantile(NASA, 0.25, na.rm = TRUE),
    NASA_p50 = quantile(NASA, 0.50, na.rm = TRUE),
    NASA_p75 = quantile(NASA, 0.75, na.rm = TRUE)
  )


print("Monthly Statistics (ME, RMSE, R):")
print(monthly_stats)

# --- 3. Overall statistics ---
overall_stats <- df %>%
  summarise(
    ME = mean(abs(Obs - NASA), na.rm = TRUE),
    RMSE = sqrt(mean((Obs - NASA)^2, na.rm = TRUE)),
    R = cor(Obs, NASA, use = "complete.obs"),
    
    # Additional descriptive stats
    Obs_mean = mean(Obs, na.rm = TRUE),
    Obs_sd = sd(Obs, na.rm = TRUE),
    Obs_p25 = quantile(Obs, 0.25, na.rm = TRUE),
    Obs_p50 = quantile(Obs, 0.50, na.rm = TRUE),  # median
    Obs_p75 = quantile(Obs, 0.75, na.rm = TRUE),
    
    NASA_mean = mean(NASA, na.rm = TRUE),
    NASA_sd = sd(NASA, na.rm = TRUE),
    NASA_p25 = quantile(NASA, 0.25, na.rm = TRUE),
    NASA_p50 = quantile(NASA, 0.50, na.rm = TRUE),
    NASA_p75 = quantile(NASA, 0.75, na.rm = TRUE)
  )

print("Overall Statistics (ME, RMSE, R):")
print(overall_stats)

# --- 4. Export to CSV and Excel ---
write.csv(monthly_stats, "Monthly_Stats.csv", row.names = FALSE)
write.csv(overall_stats, "Overall_Stats.csv", row.names = FALSE)



#########Bias Correction###############

# --- Apply monthly ME bias correction ---
df <- df %>%
  left_join(monthly_stats %>% select(Month, ME), by = "Month") %>%
  mutate(NASA_corrected = NASA - ME)

# --- Reshape for plotting ---
df_long <- df %>%
  select(Month, Year, Day, Obs, NASA_corrected) %>%
  pivot_longer(cols = c("Obs", "NASA_corrected"),
               names_to = "Dataset", values_to = "Temp")

# --- Updated boxplot with corrected NASA ---
p1 <- ggplot(df_long, aes(x = factor(Month), y = Temp, fill = Dataset)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  scale_x_discrete(labels = month.name[1:12]) +
  scale_fill_manual(
    values = c("Obs" = "#1f77b4", "NASA_corrected" = "#ff7f0e"),
    labels = c("Obs" = "Observed Temp.",
               "NASA_corrected" = "Bias-corrected NASA POWER Temp.")
  ) +
  labs(x = "Month", y = "Temperature (°C)", fill = "") +
  theme_bw(base_size = 16) +  # bigger base font
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))  # legend in 2 rows

ggsave("Boxplot_Temp_Comparison_BiasCorrected.png", p1, width = 7, height = 4.5, dpi = 600)

# --- Recalculate monthly stats with corrected NASA ---
monthly_stats_corrected <- df %>%
  group_by(Month) %>%
  summarise(
    ME = mean(Obs - NASA_corrected, na.rm = TRUE),
    RMSE = sqrt(mean((Obs - NASA_corrected)^2, na.rm = TRUE)),
    R = cor(Obs, NASA_corrected, use = "complete.obs"),
    
    # Additional descriptive stats for Obs
    Obs_mean = mean(Obs, na.rm = TRUE),
    Obs_sd = sd(Obs, na.rm = TRUE),
    Obs_p25 = quantile(Obs, 0.25, na.rm = TRUE),
    Obs_p50 = quantile(Obs, 0.50, na.rm = TRUE),
    Obs_p75 = quantile(Obs, 0.75, na.rm = TRUE),
    
    # Additional descriptive stats for NASA_corrected
    NASA_corrected_mean = mean(NASA_corrected, na.rm = TRUE),
    NASA_corrected_sd = sd(NASA_corrected, na.rm = TRUE),
    NASA_corrected_p25 = quantile(NASA_corrected, 0.25, na.rm = TRUE),
    NASA_corrected_p50 = quantile(NASA_corrected, 0.50, na.rm = TRUE),
    NASA_corrected_p75 = quantile(NASA_corrected, 0.75, na.rm = TRUE)
  )

print("Monthly Statistics (ME, RMSE, R):")
print(monthly_stats_corrected)

overall_stats_corrected <- df %>%
  summarise(
    ME = mean(Obs - NASA_corrected, na.rm = TRUE),
    RMSE = sqrt(mean((Obs - NASA_corrected)^2, na.rm = TRUE)),
    R = cor(Obs, NASA_corrected, use = "complete.obs"),
    
    # Additional descriptive stats for Obs
    Obs_mean = mean(Obs, na.rm = TRUE),
    Obs_sd = sd(Obs, na.rm = TRUE),
    Obs_p25 = quantile(Obs, 0.25, na.rm = TRUE),
    Obs_p50 = quantile(Obs, 0.50, na.rm = TRUE),
    Obs_p75 = quantile(Obs, 0.75, na.rm = TRUE),
    
    # Additional descriptive stats for NASA_corrected
    NASA_corrected_mean = mean(NASA_corrected, na.rm = TRUE),
    NASA_corrected_sd = sd(NASA_corrected, na.rm = TRUE),
    NASA_corrected_p25 = quantile(NASA_corrected, 0.25, na.rm = TRUE),
    NASA_corrected_p50 = quantile(NASA_corrected, 0.50, na.rm = TRUE),
    NASA_corrected_p75 = quantile(NASA_corrected, 0.75, na.rm = TRUE)
  )

print("Overall Statistics (ME, RMSE, R):")
print(overall_stats_corrected)

write.csv(monthly_stats_corrected, "Monthly_Stats_Corrected.csv", row.names = FALSE)
write.csv(overall_stats_corrected, "Overall_Stats_Correceted.csv", row.names = FALSE)
