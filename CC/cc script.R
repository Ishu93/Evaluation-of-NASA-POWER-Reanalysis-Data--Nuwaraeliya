cat("\f")
rm(list = ls())

# --- Load libraries ---
library(ggplot2)
library(dplyr)
library(openxlsx)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# --- Load data ---
file_path <- file.choose()
df <- read.csv(file_path)

# --- 1. Time series plot (Observed vs NASA, monthly averages in oktas) ---
df$Date <- as.Date(paste(df$Year, df$Month, "01", sep = "-"))

p1 <- ggplot(df, aes(x = Date)) +
  geom_line(aes(y = Obs, color = "Observed"), size = 1) +
  geom_line(aes(y = NASA, color = "NASA POWER"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Observed" = "#1f77b4", "NASA POWER" = "#ff7f0e")) +
  labs(x = "Year", y = "Cloud Cover (oktas)", color = "") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    legend.position = "bottom"
  )

ggsave("Monthly_CloudCover_Timeseries.png", p1, width = 8, height = 4.5, dpi = 300)

# --- 2. Monthly climatology (average across years) ---
monthly_stats <- df %>%
  group_by(Month) %>%
  summarise(
    ME = mean(abs(Obs - NASA), na.rm = TRUE),
    RMSE = sqrt(mean((Obs - NASA)^2, na.rm = TRUE)),
    R = cor(Obs, NASA, use = "complete.obs"),
    Obs_mean = mean(Obs, na.rm = TRUE),
    NASA_mean = mean(NASA, na.rm = TRUE),
    .groups = "drop"
  )

print("Monthly Statistics (ME, RMSE, R) for Cloud Cover (oktas):")
print(monthly_stats)

# --- 3. Overall statistics ---
overall_stats <- df %>%
  summarise(
    ME = mean(abs(Obs - NASA), na.rm = TRUE),
    RMSE = sqrt(mean((Obs - NASA)^2, na.rm = TRUE)),
    R = cor(Obs, NASA, use = "complete.obs"),
    Obs_mean = mean(Obs, na.rm = TRUE),
    NASA_mean = mean(NASA, na.rm = TRUE)
  )

print("Overall Statistics (ME, RMSE, R) for Cloud Cover (oktas):")
print(overall_stats)

# --- 4. Scatter plot (NASA vs Obs) ---
p2 <- ggplot(df, aes(x = Obs, y = NASA)) +
  geom_point(color = "#2ca02c", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Observed Cloud Cover (oktas)",
       y = "NASA POWER Cloud Cover (oktas)") +
  theme_bw(base_size = 14)

ggsave("Scatter_CloudCover_Obs_vs_NASA.png", p2, width = 5, height = 4.5, dpi = 300)

# --- 5. Export stats ---
write.csv(monthly_stats, "Monthly_CloudCover_Stats.csv", row.names = FALSE)
write.csv(overall_stats, "Overall_CloudCover_Stats.csv", row.names = FALSE)
