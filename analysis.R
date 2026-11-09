
# =====================================
# ENVIRONMENTAL DATA ANALYSIS
# =====================================

# 1. Load packages and data
library(tidyverse)

raw <- as_tibble(airquality)

# 2. Inspect the dataset
head(raw)
glimpse(raw)
summary(raw)

# 3. Count missing values
colSums(is.na(raw))

# 4. Prepare data for monthly summaries
ozone_data <- raw |>
  drop_na(Ozone)

monthly <- ozone_data |>
  group_by(Month) |>
  summarise(
    average_ozone = mean(Ozone),
    observations = n(),
    .groups = "drop"
  )

print(monthly)

# 5. Prepare complete cases for modelling
model_data <- raw |>
  drop_na(Ozone, Temp, Wind)

# Check how many observations remain
nrow(raw)
nrow(model_data)

# 6. Create a scatterplot
plot1 <- ggplot(
  model_data,
  aes(x = Temp, y = Ozone)
) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(
    title = "Temperature vs Ozone",
    x = "Temperature (F)",
    y = "Ozone (ppb)"
  ) +
  theme_minimal()

print(plot1)

# 7. Calculate correlation
cor(model_data$Temp, model_data$Ozone)

# 8. Fit a regression model
model <- lm(
  Ozone ~ Temp + Wind,
  data = model_data
)

summary(model)

# 9. Predict under three scenarios
scenarios <- tibble(
  Temp = c(70, 80, 90),
  Wind = rep(median(model_data$Wind), 3)
)

scenarios$predicted_ozone <- predict(
  model,
  newdata = scenarios
)

print(scenarios)

# 10. Save results
dir.create("outputs", showWarnings = FALSE)

ggsave(
  "outputs/temperature_ozone.png",
  plot = plot1,
  width = 7,
  height = 5
)

write_csv(
  monthly,
  "outputs/monthly_ozone.csv"
)

