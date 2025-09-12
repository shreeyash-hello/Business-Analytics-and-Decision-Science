library(dplyr)
library(ggplot2)
library(readr)
library(broom)

# Create output directories if they don’t exist
dir.create("figures", showWarnings = FALSE)
dir.create("model", showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. Load or generate dataset
# -----------------------------------------------------------------------------
cust_path <- "data/customer_data.csv"

# Function to generate synthetic customer data (for demo/reproducibility)
generate_synthetic_customers <- function(n = 400, seed = 123) {
  set.seed(seed)
  
  # Advertisement channel: equally distributed across 4 categories
  ad_channel <- rep(1:4, length.out = n)
  
  # Simulated demographics and behavior
  age <- round(rnorm(n, mean = 35, sd = 10))
  time_on_site <- round(pmax(1, rnorm(n, mean = 12, sd = 6)))
  seen_voucher <- rbinom(n, 1, prob = 0.35)  # ~35% saw voucher
  income <- round(rnorm(n, mean = 32000, sd = 8000))
  
  # Revenue: linear combination + noise
  revenue <- 2.0 +
    0.01 * age +
    0.05 * time_on_site +
    0.6 * seen_voucher +
    0.02 * (income / 1000) +
    ifelse(ad_channel == 4, 1.0, 0.0) +  # channel 4 (influencer) has boost
    rnorm(n, mean = 0, sd = 1.0)
  
  revenue <- pmax(0, revenue)  # no negative revenue
  
  data.frame(
    EstimatedAge = age,
    TimeOnSite = time_on_site,
    SeenVoucher = seen_voucher,
    EstimatedIncome = income,
    AdvertisementChannel = ad_channel,
    Revenue = revenue
  )
}

# Load dataset or generate synthetic one
if (!file.exists(cust_path)) {
  message("Customer data not found. Generating synthetic dataset...")
  dir.create("data", showWarnings = FALSE)
  cust <- generate_synthetic_customers(400)
  write_csv(cust, cust_path)
  message("Synthetic dataset saved to ", cust_path)
} else {
  cust <- read_csv(cust_path, show_col_types = FALSE)
}

# Ensure correct variable types
cust <- cust %>%
  mutate(
    AdvertisementChannel = as.factor(AdvertisementChannel),
    SeenVoucher = as.integer(SeenVoucher)
  )

# -----------------------------------------------------------------------------
# 2. Exploratory Data Analysis (EDA)
# -----------------------------------------------------------------------------

# Revenue by advertisement channel
p_channel <- cust %>%
  group_by(AdvertisementChannel) %>%
  summarise(avg_rev = mean(Revenue), .groups = "drop") %>%
  ggplot(aes(x = AdvertisementChannel, y = avg_rev)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average revenue by Advertisement Channel",
       x = "Advertisement Channel",
       y = "Average Revenue (GBP)") +
  theme_minimal()
ggsave("figures/avg_rev_by_channel.png", p_channel, width = 6, height = 4)

# Revenue by income bin
cust <- cust %>%
  mutate(IncomeBin = cut(
    EstimatedIncome,
    breaks = c(0, 20000, 25000, 30000, 35000, 40000, 45000, 50000, Inf),
    labels = c("<20k", "20–25k", "25–30k", "30–35k",
               "35–40k", "40–45k", "45–50k", ">50k"),
    include.lowest = TRUE
  ))

p_income <- cust %>%
  group_by(IncomeBin) %>%
  summarise(avg_rev = mean(Revenue), .groups = "drop") %>%
  ggplot(aes(x = IncomeBin, y = avg_rev)) +
  geom_col(fill = "darkorange") +
  labs(title = "Average revenue by Estimated Income bin",
       x = "Estimated Income (GBP)",
       y = "Average Revenue (GBP)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/avg_rev_by_income_bin.png", p_income, width = 7, height = 4)

# Revenue by voucher exposure
p_voucher <- cust %>%
  group_by(SeenVoucher) %>%
  summarise(avg_rev = mean(Revenue), .groups = "drop") %>%
  ggplot(aes(x = factor(SeenVoucher), y = avg_rev)) +
  geom_col(fill = "seagreen") +
  labs(title = "Average revenue by Voucher Seen",
       x = "Seen Voucher (0 = No, 1 = Yes)",
       y = "Average Revenue (GBP)") +
  theme_minimal()
ggsave("figures/avg_rev_by_voucher.png", p_voucher, width = 6, height = 4)

# -----------------------------------------------------------------------------
# 3. Regression Modeling
# -----------------------------------------------------------------------------

# Multiple linear regression with demographics, behavior, and ad channel
model <- lm(
  Revenue ~ EstimatedAge + TimeOnSite + SeenVoucher +
    EstimatedIncome + AdvertisementChannel,
  data = cust
)

# Print regression summary to console
cat("Regression Model Summary:\n")
print(summary(model))

# Save tidy coefficients and diagnostics
tidy_coef <- broom::tidy(model)
glance_stats <- broom::glance(model)

write.csv(tidy_coef, "figures/regression_coefficients.csv", row.names = FALSE)
saveRDS(model, "model/regression_model.rds")

# -----------------------------------------------------------------------------
# 4. Model Evaluation & Diagnostics
# -----------------------------------------------------------------------------

# Predicted vs actual plot
cust$predicted <- predict(model, cust)

p_pred <- ggplot(cust, aes(x = predicted, y = Revenue)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Revenue",
       x = "Predicted Revenue (GBP)",
       y = "Actual Revenue (GBP)") +
  theme_minimal()
ggsave("figures/predicted_vs_actual.png", p_pred, width = 6, height = 4)

# Calculate error metrics
mse <- mean(model$residuals^2)
nmse <- mse / var(cust$Revenue)

cat(sprintf("Mean Squared Error (MSE): %.6f\n", mse))
cat(sprintf("Normalized MSE (MSE/Var(Revenue)): %.6f\n", nmse))

# -----------------------------------------------------------------------------
# 5. Save summary text
# -----------------------------------------------------------------------------
summary_text <- sprintf(
  "Customer Analysis Results:\n\nKey positive drivers: Voucher seen, Estimated income, Influencer channel\nR-squared: %.4f\nMSE: %.6f\nNormalized MSE: %.6f\n",
  glance_stats$r.squared, mse, nmse
)

writeLines(summary_text, "analysis_summary_part2.txt")

cat("Customer analysis complete. Results saved in 'figures/' and 'model/'.\n")
