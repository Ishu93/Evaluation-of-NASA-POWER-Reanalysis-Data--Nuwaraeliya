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
    values_to = "RelHum"
  )

# --- 1. Boxplot by month ---
p1 <- ggplot(df_long, aes(x = factor(Month), y = RelHum, fill = Dataset)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  scale_x_discrete(labels = month.name[1:12]) +
  scale_fill_manual(
    values = c("Obs" = "#1f77b4", "NASA" = "#ff7f0e"),
    labels = c("Obs" = "Observed RH",
               "NASA" = "NASA POWER RH")
  ) +
  labs(
    x = "Month",
    y = "Relative Humidity (%)",
    fill = ""
  ) +
  theme_bw(base_size = 16) +   # match temperature plot style
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
ggsave("Boxplot_RelHum_Comparison.png", p1, width = 7, height = 4.5, dpi = 600)

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

print("Monthly Statistics (ME, RMSE, R) for Relative Humidity:")
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

print("Overall Statistics (ME, RMSE, R) for Relative Humidity:")
print(overall_stats)

# --- 4. Export to CSV and Excel ---
write.csv(monthly_stats, "Monthly_RelHum_Stats.csv", row.names = FALSE)
write.csv(overall_stats, "Overall_RelHum_Stats.csv", row.names = FALSE)


