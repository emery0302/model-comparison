# Load required packages
install.packages("lme4")
install.packages("ggplot2")
library(lme4)
library(ggplot2)

# Simulate ecological dataset
set.seed(42)
n <- 1000

df <- data.frame(
  Location = factor(sample(LETTERS[1:5], n, replace = TRUE)),
  Year = factor(sample(2000:2005, n, replace = TRUE)),
  Temperature = rnorm(n, mean = 20, sd = 5),
  Precipitation = rnorm(n, mean = 100, sd = 20),
  HabitatQuality = runif(n, min = 0, max = 1),
  BirdAbundance = round(rnorm(n, mean = 50, sd = 10))
)

# Fit different linear mixed models
lmm_model1 <- lmer(BirdAbundance ~ Temperature + Precipitation + HabitatQuality + (1 | Location) + (1 | Year), data = df)
lmm_model2 <- lmer(BirdAbundance ~ Temperature + Precipitation + (1 | Location) + (1 | Year), data = df)
lmm_model3 <- lmer(BirdAbundance ~ Temperature + HabitatQuality + (1 | Location) + (1 | Year), data = df)
lmm_model4 <- lmer(BirdAbundance ~ Precipitation + HabitatQuality + (1 | Location) + (1 | Year), data = df)

# Calculate AIC for each model
AIC_model1 <- AIC(lmm_model1)
AIC_model2 <- AIC(lmm_model2)
AIC_model3 <- AIC(lmm_model3)
AIC_model4 <- AIC(lmm_model4)

# Display the AIC values
AIC_values <- data.frame(Model = c("Model 1", "Model 2", "Model 3", "Model 4"),
                         AIC = c(AIC_model1, AIC_model2, AIC_model3, AIC_model4))
AIC_values

# Choose the best model based on the lowest AIC
best_model <- AIC_values[which.min(AIC_values$AIC), ]
best_model
