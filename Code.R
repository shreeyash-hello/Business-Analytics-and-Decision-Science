# analysis_all.R
# -----------------------------------------------------------------------------
# Single-file reproducible analysis:
#  - TOPSIS ranking for candidate robot prototypes
#  - Integer allocation of robots to store types via LP
#  - Multiple linear regression on customer data
#
# Usage:
#  - Put customer CSV at data/customer_data.csv with columns:
#      EstimatedAge, TimeOnSite, SeenVoucher (0/1), EstimatedIncome,
#      AdvertisementChannel (1-4), Revenue
#  - Run: Rscript analysis_all.R
#
# Required packages:
#  install.packages(c("dplyr","ggplot2","readr","broom","lpSolve"))
# -----------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(readr)
library(broom)
library(lpSolve)

# Ensure output directories exist
dir.create("figures", showWarnings = FALSE)
dir.create("model", showWarnings = FALSE)

# ----------------------------
# Part 0: Helper utilities
# ----------------------------
# Small helper to save ggplot objects safely
save_plot_safe <- function(plot_obj, filename, width = 7, height = 4) {
  tryCatch({
    ggsave(filename, plot = plot_obj, width = width, height = height)
  }, error = function(e) {
    message("Failed to save plot: ", filename, " — ", e$message)
  })
}

# ----------------------------
# Part 1: TOPSIS ranking
# ----------------------------
# Define prototype specs (one row per prototype)
# Columns: Carrying capacity (litres), Battery hours, Avg speed (km/h),
# Cost (GBP), Reliability (hours between failures)
robots <- data.frame(
  name = c("Archer", "Bowler", "Corner", "Deviant"),
  carrying_l = c(45, 50, 60, 40),
  battery_h = c(18, 18, 12, 24),
  speed_kmph = c(6, 4, 4, 10),
  cost_gbp = c(5210, 6250, 4500, 7100),
  reliability_h = c(22, 24, 24, 32),
  stringsAsFactors = FALSE
)

# Weights for criteria (sum does not need to be 1 but using given proportions)
weights <- c(
  carrying_l = 0.067,
  battery_h = 0.200,
  speed_kmph = 0.133,
  cost_gbp = 0.267,
  reliability_h = 0.333
)

# For each criterion indicate whether higher is better (TRUE) or lower is better (FALSE)
# cost_gbp is a cost criterion (lower is better)
benefit_flags <- c(TRUE, TRUE, TRUE, FALSE, TRUE)

# General TOPSIS implementation (returns scores and intermediate matrices)
topsis <- function(df, value_cols, weights, benefit) {
  M <- as.matrix(df[, value_cols])
  # Step 1: normalize columns (Euclidean norm)
  col_norms <- sqrt(colSums(M^2))
  M_norm <- sweep(M, 2, col_norms, "/")
  # Step 2: apply weights
  WN <- sweep(M_norm, 2, weights, "*")
  # Step 3: determine PIS (ideal positive) and NIS (ideal negative)
  PIS <- numeric(ncol(WN))
  NIS <- numeric(ncol(WN))
  for (j in seq_len(ncol(WN))) {
    if (benefit[j]) {
      PIS[j] <- max(WN[, j])
      NIS[j] <- min(WN[, j])
    } else {
      # for cost criteria the ideal (PIS) is the minimum value
      PIS[j] <- min(WN[, j])
      NIS[j] <- max(WN[, j])
    }
  }
  # Step 4: compute Euclidean distances to PIS and NIS
  dist_to_PIS <- apply(WN, 1, function(x) sqrt(sum((x - PIS)^2)))
  dist_to_NIS <- apply(WN, 1, function(x) sqrt(sum((x - NIS)^2)))
  # Step 5: relative closeness (higher is better)
  score <- dist_to_NIS / (dist_to_PIS + dist_to_NIS)
  result_df <- df %>%
    mutate(dist_to_NIS = dist_to_NIS,
           dist_to_PIS = dist_to_PIS,
           topsis_score = score) %>%
    arrange(desc(topsis_score))
  list(result = result_df, WN = WN, PIS = PIS, NIS = NIS)
}

# Run TOPSIS
value_cols <- c("carrying_l", "battery_h", "speed_kmph", "cost_gbp", "reliability_h")
topsis_out <- topsis(robots, value_cols, weights[value_cols], benefit_flags)

# Print ranking to console
cat("TOPSIS ranking (highest score first):\n")
print(topsis_out$result[, c("name", "topsis_score")])

# Bar chart of TOPSIS scores
p_topsis <- ggplot(topsis_out$result, aes(x = reorder(name, topsis_score), y = topsis_score)) +
  geom_col() + coord_flip() +
  labs(title = "TOPSIS scores for robot prototypes",
       x = "Robot", y = "TOPSIS score") +
  theme_minimal()
save_plot_safe(p_topsis, "figures/topsis_scores.png", width = 7, height = 4)

# ----------------------------
# Part 2: Integer allocation via linear programming
# ----------------------------
# Decision variables:
#  x1 = number of robots assigned to Grocery stores
#  x2 = number of robots assigned to Clothing stores
#  x3 = number of robots assigned to Sports stores
#
# Constraints encoded below:
#  - orders constraint (equality)
#  - budget constraint (equality)
#  - technician hours constraint (upper bound)
#  - lower bounds on each decision var (>= 5)
#
# These numeric coefficients are the problem inputs.

# Coefficients for constraints
# Orders contributed per robot by store type
orders_coeff <- c(9, 6, 4)         # 9*x1 + 6*x2 + 4*x3 = 215
orders_rhs   <- 215                # target orders

# Per-robot total cost (base + extras)
cost_per_robot <- c(7100 + 1600,   # grocery
                    7100 + 1000,   # clothing
                    7100 + 600)    # sports
budget_rhs <- 250000               # total budget in GBP (equality)

# Technician hours per robot (weekly)
tech_coeff <- c(10, 7, 5)
tech_rhs <- 250                     # technician hours upper bound (<=)

# Lower bounds (minimum robots per category)
lower_bounds <- c(5, 5, 5)

# Build LP matrices
# lpSolve expects constraints as matrix A, directions, rhs
A <- rbind(orders_coeff, cost_per_robot, tech_coeff)
dir <- c("=", "=", "<=")
rhs <- c(orders_rhs, budget_rhs, tech_rhs)

# Objective: we just need a feasible integer solution, so set trivial objective
obj <- c(0, 0, 0)

# Call integer LP solver with variable lower bounds
lp_solution <- lp(direction = "min",
                  objective.in = obj,
                  const.mat = A,
                  const.dir = dir,
                  const.rhs = rhs,
                  all.int = TRUE,
                  lower = lower_bounds)

if (lp_solution$status == 0) {
  sol <- lp_solution$solution
  names(sol) <- c("x1_grocery", "x2_clothing", "x3_sports")
  cat("Integer feasible allocation found:\n")
  print(sol)
  # compute LHS checks
  orders_lhs <- sum(orders_coeff * sol)
  budget_lhs <- sum(cost_per_robot * sol)
  tech_lhs <- sum(tech_coeff * sol)
  cat(sprintf("Check: Orders LHS = %d (RHS %d)\n", orders_lhs, orders_rhs))
  cat(sprintf("Check: Budget LHS = %d (RHS %d)\n", budget_lhs, budget_rhs))
  cat(sprintf("Check: Tech LHS = %d (<= %d)\n", tech_lhs, tech_rhs))
  # Save allocation pie chart
  alloc_df <- data.frame(store = c("Grocery", "Clothing", "Sports"),
                         robots = sol)
  p_alloc <- ggplot(alloc_df, aes(x = "", y = robots, fill = store)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    labs(title = "Robot allocation across store types") +
    theme_void()
  save_plot_safe(p_alloc, "figures/allocation_pie.png", width = 6, height = 4)
} else {
  warning("No integer feasible solution found with the current inputs. LP status: ", lp_solution$status)
  # If desired: relax equalities or solve a small least-squares / integer approx here
}

# ----------------------------
# Part 3: Multiple linear regression on customer data
# ----------------------------
# Expected columns in customer data:
#  - EstimatedAge (numeric)
#  - TimeOnSite (numeric)
#  - SeenVoucher (0/1)
#  - EstimatedIncome (numeric)
#  - AdvertisementChannel (1..4)
#  - Revenue (numeric)  <- dependent variable
#
# If data file is missing, create a synthetic dataset for demonstration.

cust_path <- "data/customer_data.csv"

generate_synthetic_customers <- function(n = 400, seed = 42) {
  set.seed(seed)
  # Simulate advertisement channel distribution roughly equal
  ad_channel <- rep(1:4, length.out = n)
  # Simulate demographics
  age <- round(rnorm(n, mean = 35, sd = 10))
  time_on_site <- round(pmax(1, rnorm(n, mean = 12, sd = 6)))
  # Binary voucher seen indicator
  seen_voucher <- rbinom(n, 1, prob = 0.35)
  income <- round(rnorm(n, mean = 32000, sd = 8000))
  # Create revenue as linear function + noise (for demo)
  # revenue = base + beta_age*age + beta_time*time_on_site + beta_voucher*seen_voucher + beta_income*(income/1000) + ad effects + noise
  base <- 2.0
  rev <- base +
    0.01 * age +
    0.05 * time_on_site +
    0.6 * seen_voucher +
    0.02 * (income / 1000) +
    c(rep(0.0, n))[ad_channel] + # placeholder: channels will be handled by factor in lm
    rnorm(n, mean = 0, sd = 1.0)
  rev <- pmax(0, rev)  # no negative revenue
  synthetic <- data.frame(
    EstimatedAge = age,
    TimeOnSite = time_on_site,
    SeenVoucher = seen_voucher,
    EstimatedIncome = income,
    AdvertisementChannel = ad_channel,
    Revenue = rev
  )
  return(synthetic)
}

if (!file.exists(cust_path)) {
  message("Customer data not found at '", cust_path, "'. Generating synthetic dataset for demonstration.")
  cust <- generate_synthetic_customers(400)
  dir.create("data", showWarnings = FALSE)
  write_csv(cust, cust_path)
  message("Synthetic customer data written to: ", cust_path)
} else {
  cust <- read_csv(cust_path, show_col_types = FALSE)
}

# Ensure types are correct
cust <- cust %>%
  mutate(AdvertisementChannel = as.factor(AdvertisementChannel),
         SeenVoucher = as.integer(SeenVoucher))

# Exploratory visuals saved to figures/
p_avg_by_channel <- cust %>%
  group_by(AdvertisementChannel) %>%
  summarize(avg_rev = mean(Revenue, na.rm = TRUE)) %>%
  ggplot(aes(x = AdvertisementChannel, y = avg_rev)) +
  geom_col() + labs(title = "Average revenue by advertisement channel", x = "Ad channel", y = "Avg revenue") +
  theme_minimal()
save_plot_safe(p_avg_by_channel, "figures/avg_rev_by_channel.png", width = 6, height = 4)

# Income bins & plot
cust <- cust %>%
  mutate(IncomeBin = cut(EstimatedIncome,
                        breaks = c(0, 20000, 25000, 30000, 35000, 40000, 45000, 50000, Inf),
                        labels = c("<20k","20-25k","25-30k","30-35k","35-40k","40-45k","45-50k",">50k"),
                        include.lowest = TRUE))
p_income <- cust %>%
  group_by(IncomeBin) %>%
  summarize(avg_rev = mean(Revenue, na.rm = TRUE)) %>%
  ggplot(aes(x = IncomeBin, y = avg_rev)) +
  geom_col() + labs(title = "Average revenue by income bin", x = "Income bin", y = "Avg revenue") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot_safe(p_income, "figures/avg_rev_by_income_bin.png", width = 7, height = 4)

# Voucher effect plot
p_voucher <- cust %>%
  group_by(SeenVoucher) %>%
  summarize(avg_rev = mean(Revenue, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(SeenVoucher), y = avg_rev)) +
  geom_col() + labs(title = "Average revenue by voucher seen (0/1)", x = "SeenVoucher", y = "Avg revenue") +
  theme_minimal()
save_plot_safe(p_voucher, "figures/avg_rev_by_voucher.png", width = 6, height = 4)

# Fit the regression model
# Model formula: Revenue ~ EstimatedAge + TimeOnSite + SeenVoucher + EstimatedIncome + AdvertisementChannel
model <- lm(Revenue ~ EstimatedAge + TimeOnSite + SeenVoucher + EstimatedIncome + AdvertisementChannel, data = cust)
cat("Regression summary:\n")
print(summary(model))

# Save tidy coefficients and model diagnostics
tidy_coef <- broom::tidy(model)
glance_stats <- broom::glance(model)
write.csv(tidy_coef, "figures/regression_coefficients.csv", row.names = FALSE)
saveRDS(model, file = "model/regression_model.rds")

# Predicted vs actual plot
cust$predicted <- predict(model, cust)
p_pred_vs_actual <- ggplot(cust, aes(x = predicted, y = Revenue)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Predicted vs Actual Revenue", x = "Predicted", y = "Actual") +
  theme_minimal()
save_plot_safe(p_pred_vs_actual, "figures/predicted_vs_actual.png", width = 6, height = 4)

# Compute MSE and normalized MSE
mse <- mean(model$residuals^2)
nmse <- mse / var(cust$Revenue)
cat(sprintf("Mean Squared Error (MSE): %.6f\n", mse))
cat(sprintf("Normalized MSE (MSE/Var(Revenue)): %.6f\n", nmse))

# Save a short textual summary of key outputs
summary_text <- sprintf(
  "TOPSIS winner: %s\nAllocation (if found): %s\nRegression R-squared: %.4f\nMSE: %.6f\n",
  topsis_out$result$name[1],
  if (lp_solution$status == 0) paste0("Grocery=", sol[1], ", Clothing=", sol[2], ", Sports=", sol[3]) else "No feasible allocation found",
  glance_stats$r.squared,
  mse
)
writeLines(summary_text, con = "analysis_summary.txt")

cat("Script completed. Key figures in 'figures/', model in 'model/', and summary in 'analysis_summary.txt'\n")
