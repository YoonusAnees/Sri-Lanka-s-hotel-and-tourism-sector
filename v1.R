req_pkgs <- c(
  "readr","dplyr","tidyr","ggplot2","psych","nortest",
  "car","lmtest","corrplot","MASS"
)

installed <- rownames(installed.packages())
for (p in req_pkgs) {
  if (!p %in% installed) install.packages(p)
  library(p, character.only = TRUE)
}
library(tidyverse)


# -----------------------------
# 1) Import dataset
#-----------------------------
hotels <- readr::read_csv("HOTELS_2025.csv", show_col_types = FALSE)

# Quick checks
print(names(hotels))
print(dim(hotels))
print(head(hotels, 10))
print(summary(hotels))
str(hotels)

# Load data set
df <- read_csv("HOTELS_2025.csv")

# -----------------------------
#  Top 5 Revenue  making Hotels
# -----------------------------
df <- df %>%
  mutate(HotelID = paste0("Hotel_", row_number()))

df %>%
  arrange(desc(Revenue)) %>%
  mutate(Rank = row_number()) %>%
  slice_head(n = 5) %>%
  dplyr::select(
    Rank,
    HotelID,
    RoomsAvailable,
    OccupancyRate,
    ADR,
    Revenue,
    HotelQualityRank
  )

# -----------------------------
#  Top 5 Biggest Hotels by Size (Room Available )
# -----------------------------


df %>%
  arrange(desc(RoomsAvailable)) %>%
  mutate(Rank = row_number()) %>%
  slice_head(n = 5) %>%
  dplyr::select(
    Rank,
    HotelID,
    RoomsAvailable,
    OccupancyRate,
    ADR,
    Revenue,
    HotelQualityRank
  )


# -----------------------------
#  Top 5 Highest Occupancy Hotels (most demand)
# ----

df %>%
  arrange(desc(OccupancyRate)) %>%
  mutate(Rank = row_number()) %>%
  slice_head(n = 5) %>%
  dplyr::select(
    Rank,
    HotelID,
    RoomsAvailable,
    OccupancyRate,
    ADR,
    Revenue,
    HotelQualityRank
  )

# -----------------------------
#  Top 5 Premium-Priced Hotels (highest ADR)
# ----

df %>%
  arrange(desc(ADR)) %>%
  mutate(Rank = row_number()) %>%
  slice_head(n = 5) %>%
  dplyr::select(
    Rank,
    HotelID,
    RoomsAvailable,
    OccupancyRate,
    ADR,
    Revenue,
    HotelQualityRank
  )

# -----------------------------
# Top 5 Hotels with the Largest Workforce
# ----

df %>%
  arrange(desc(StaffCount)) %>%
  mutate(Rank = row_number()) %>%
  slice_head(n = 5) %>%
  dplyr::select(
    Rank,
    HotelID,
    StaffCount,
    RoomsAvailable,
    Revenue,
    HotelQualityRank
  )

# -----------------------------
# Top 5  Hotels with the Highest Guest Satisfaction Scores
# ----

df %>%
  arrange(desc(GuestSatisfactionScore)) %>%
  mutate(Rank = row_number()) %>%
  slice_head(n = 5) %>%
  dplyr::select(
    Rank,
    HotelID,
    GuestSatisfactionScore,
    OccupancyRate,
    ADR,
    Revenue,
    HotelQualityRank
  )

# -----------------------------
# Top 5 Hotels with Most Loyal Customers
# ----

df %>%
  arrange(desc(LoyaltyMembers)) %>%
  mutate(Rank = row_number()) %>%
  slice_head(n = 5) %>%
  dplyr::select(
    Rank,
    HotelID,
    LoyaltyMembers,
    GuestSatisfactionScore,
    Revenue,
    HotelQualityRank
  )

# -----------------------------
# 3) Summary statistics (nice table)
# -----------------------------
num_vars <- c(
  "RoomsAvailable","OccupancyRate","ADR","MarketingSpend","StaffCount",
  "GuestSatisfactionScore","LoyaltyMembers","Revenue"
)
num_vars <- num_vars[num_vars %in% names(hotels)]

desc_stats <- psych::describe(hotels %>% dplyr::select(dplyr::all_of(num_vars)))
print(desc_stats)

# Category counts for rank (if available)
if ("HotelQualityRank" %in% names(hotels)) {
  print(table(hotels$HotelQualityRank))
}

# -----------------------------
# 4) Normality tests (AD / Lillie / Shapiro)
# -----------------------------
# Note: Shapiro is sensitive with large n; still include for rubric.

normality_tests <- function(x, var_name = "variable") {
  x <- x[is.finite(x)]
  cat("\n----------------------------------\n")
  cat("Normality tests for:", var_name, "\n")
  cat("----------------------------------\n")
  print(nortest::ad.test(x))
  print(nortest::lillie.test(x))
  # Shapiro max n=5000; dataset likely < 5000. If larger, sample.
  if (length(x) > 5000) {
    set.seed(1)
    x_s <- sample(x, 5000)
    cat("Shapiro on random sample of 5000 (dataset too large)\n")
    print(shapiro.test(x_s))
  } else {
    print(shapiro.test(x))
  }
}

if ("Revenue" %in% names(hotels)) normality_tests(hotels$Revenue, "Revenue")
if ("RoomsAvailable" %in% names(hotels)) normality_tests(hotels$RoomsAvailable, "RoomsAvailable")
if ("ADR" %in% names(hotels)) normality_tests(hotels$ADR, "ADR")
if ("OccupancyRate" %in% names(hotels)) normality_tests(hotels$OccupancyRate, "OccupancyRate")
if ("MarketingSpend" %in% names(hotels)) normality_tests(hotels$MarketingSpend, "MarketingSpend")

# -----------------------------
# 5) Graphical analysis: hist + QQ + scatter + boxplots
# -----------------------------
# Histogram + QQ for Revenue
if ("Revenue" %in% names(hotels)) {
  hist(hotels$Revenue, breaks = 30, main = "Revenue Distribution", xlab = "Revenue")
  qqnorm(hotels$Revenue); qqline(hotels$Revenue)
}

# Histogram + QQ for RoomsAvailable
if ("RoomsAvailable" %in% names(hotels)) {
  hist(hotels$RoomsAvailable, breaks = 30, main = "RoomsAvailable Distribution", xlab = "RoomsAvailable")
  qqnorm(hotels$RoomsAvailable); qqline(hotels$RoomsAvailable)
}

# Scatterplots with regression line (Revenue vs key predictors)
scatter_lm <- function(xvar, yvar = "Revenue") {
  if (all(c(xvar, yvar) %in% names(hotels))) {
    ggplot(hotels, aes_string(x = xvar, y = yvar)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) +
      labs(title = paste(yvar, "vs", xvar), x = xvar, y = yvar) %>%
      print()
  }
}

scatter_lm("ADR")
scatter_lm("OccupancyRate")
scatter_lm("MarketingSpend")
scatter_lm("RoomsAvailable")
scatter_lm("GuestSatisfactionScore")
scatter_lm("LoyaltyMembers")
scatter_lm("StaffCount")

# Boxplot: Revenue by HotelQualityRank
if (all(c("Revenue","HotelQualityRank") %in% names(hotels))) {
  ggplot(hotels, aes(x = HotelQualityRank, y = Revenue)) +
    geom_boxplot() +
    labs(title = "Revenue by Hotel Quality Rank", x = "HotelQualityRank", y = "Revenue") %>%
    print()
}

# -----------------------------
# 6) Variance tests: Revenue by HotelQualityRank
# -----------------------------
if (all(c("Revenue","HotelQualityRank") %in% names(hotels))) {
  cat("\n----------------------------------\n")
  cat("Variance tests: Revenue ~ HotelQualityRank\n")
  cat("----------------------------------\n")
  print(bartlett.test(Revenue ~ HotelQualityRank, data = hotels))
  print(car::leveneTest(Revenue ~ HotelQualityRank, data = hotels, center = "median"))
}

# -----------------------------
# 7) Mean testing: One-way ANOVA (Revenue by rank)
# -----------------------------
if (all(c("Revenue","HotelQualityRank") %in% names(hotels))) {
  cat("\n----------------------------------\n")
  cat("One-way ANOVA: Revenue ~ HotelQualityRank\n")
  cat("----------------------------------\n")
  anova_rev <- aov(Revenue ~ HotelQualityRank, data = hotels)
  print(summary(anova_rev))
  
  cat("\nGroup means:\n")
  print(tapply(hotels$Revenue, hotels$HotelQualityRank, mean))
  cat("\nGroup SD:\n")
  print(tapply(hotels$Revenue, hotels$HotelQualityRank, sd))
}

# -----------------------------
# 8) Correlation analysis (Revenue vs predictors)
# -----------------------------
corr_test <- function(xvar, yvar = "Revenue") {
  if (all(c(xvar, yvar) %in% names(hotels))) {
    cat("\n----------------------------------\n")
    cat("Pearson correlation:", yvar, "vs", xvar, "\n")
    cat("----------------------------------\n")
    print(cor.test(hotels[[yvar]], hotels[[xvar]], method = "pearson"))
  }
}

corr_test("RoomsAvailable")
corr_test("OccupancyRate")
corr_test("ADR")
corr_test("MarketingSpend")
corr_test("StaffCount")
corr_test("GuestSatisfactionScore")
corr_test("LoyaltyMembers")

# Correlation matrix (numeric only)
num_for_corr <- intersect(
  c("RoomsAvailable","OccupancyRate","ADR","MarketingSpend","StaffCount",
    "GuestSatisfactionScore","LoyaltyMembers","Revenue"),
  names(hotels)
)
if (length(num_for_corr) >= 3) {
  cor_mat <- cor(hotels %>% dplyr::select(dplyr::all_of(num_for_corr)), use = "complete.obs")
  print(cor_mat)
  corrplot::corrplot(cor_mat, method = "number")
}

# -----------------------------
# 9) Regression models
# -----------------------------
# 9.1 Simple linear regressions (Revenue vs each predictor)
simple_models <- list()
for (xvar in setdiff(num_for_corr, "Revenue")) {
  f <- as.formula(paste("Revenue ~", xvar))
  m <- lm(f, data = hotels)
  simple_models[[xvar]] <- m
  cat("\n==================================\n")
  cat("Simple Linear Regression:", deparse(f), "\n")
  cat("==================================\n")
  print(summary(m))
}

# 9.2 Multiple linear regression (core model)
# Start with key business predictors (edit if needed)
multi_vars <- intersect(c("RoomsAvailable","ADR","OccupancyRate","MarketingSpend",
                          "GuestSatisfactionScore","LoyaltyMembers","StaffCount"),
                        names(hotels))

if (all(c("Revenue", multi_vars) %in% names(hotels)) && length(multi_vars) >= 2) {
  f_multi <- as.formula(paste("Revenue ~", paste(multi_vars, collapse = " + ")))
  m_full <- lm(f_multi, data = hotels)
  
  cat("\n==================================\n")
  cat("Multiple Linear Regression (FULL):\n")
  cat("==================================\n")
  print(summary(m_full))
  
  # Stepwise selection to find a compact best model
  m_step <- MASS::stepAIC(m_full, direction = "both", trace = FALSE)
  
  cat("\n==================================\n")
  cat("Multiple Linear Regression (STEP/AIC Selected):\n")
  cat("==================================\n")
  print(summary(m_step))
  
  # VIF (multicollinearity)
  cat("\nVIF (for step model):\n")
  print(car::vif(m_step))
  
  # Diagnostics plots
  par(mfrow = c(2,2))
  plot(m_step)
  par(mfrow = c(1,1))
  
  # Residual normality test
  cat("\nResidual normality (AD test) for step model:\n")
  print(nortest::ad.test(residuals(m_step)))
  
  # Heteroscedasticity test
  cat("\nBreusch-Pagan test for step model:\n")
  print(lmtest::bptest(m_step))
  
  # Save chosen model object
  best_model <- m_step
} else {
  message("Not enough predictors found for multiple regression. Check column names.")
}

# -----------------------------
# 10) Optional: Log transform model (if Revenue is skewed)
# -----------------------------
if ("Revenue" %in% names(hotels)) {
  hotels <- hotels %>% mutate(logRevenue = log(Revenue))
  
  # Normality of logRevenue
  normality_tests(hotels$logRevenue, "logRevenue")
  
  # Simple log model example
  if ("ADR" %in% names(hotels)) {
    m_log <- lm(logRevenue ~ ADR, data = hotels)
    cat("\n==================================\n")
    cat("Log model: log(Revenue) ~ ADR\n")
    cat("==================================\n")
    print(summary(m_log))
  }
  
  # Multiple log model with same predictors (if available)
  if (exists("multi_vars") && length(multi_vars) >= 2) {
    f_log_multi <- as.formula(paste("logRevenue ~", paste(multi_vars, collapse = " + ")))
    m_log_full <- lm(f_log_multi, data = hotels)
    m_log_step <- MASS::stepAIC(m_log_full, direction = "both", trace = FALSE)
    
    cat("\n==================================\n")
    cat("Log Multiple Regression (STEP/AIC Selected):\n")
    cat("==================================\n")
    print(summary(m_log_step))
  }
}

# -----------------------------
# 11) Save outputs (optional)
# -----------------------------
# If you want to save key tables to CSV for appendix
write.csv(desc_stats, "hotels_descriptive_stats.csv", row.names = TRUE)

# Save a correlation matrix file
if (exists("cor_mat")) write.csv(cor_mat, "hotels_correlation_matrix.csv", row.names = TRUE)

cat("\nDONE ✅\n")
cat("Next: Take screenshots of key outputs (summary, normality tests, ANOVA, correlation, regression summaries, diagnostic plots).\n")
