CIS6008 – Analytics & Business Intelligence
Task (a): Statistical Analysis of Hotel Revenue (DOWNLOADABLE README)
This README explains only Task (a) of the CIS6008 WRIT1 assessment.
It shows how to run the statistical analysis in R / RStudio and how to prepare Appendix A for submission.

1. Purpose of Task (a)
Task (a) aims to identify factors influencing hotel revenue using statistical and business analytics techniques.
The analysis supports evidence-based decision-making for Sri Lanka’s hotel and tourism sector.

Core outcomes:

Identify key revenue drivers
Build statistical models
Support policy and managerial decisions
2. Dataset Used
File: HOTELS_2025.csv
Records: 500 hotels
Variables: 9 (8 numerical, 1 categorical)

Key variables:

Revenue (dependent variable)
ADR, OccupancyRate, MarketingSpend, StaffCount, GuestSatisfactionScore, LoyaltyMembers (explanatory)
3. Software & Packages
Required:

R (latest version)
RStudio
Required R packages:

install.packages("tidyverse")
Optional:

install.packages("nortest")
4. Task (a) Analysis Workflow
Step 1: Load Data
df <- read.csv("HOTELS_2025.csv")
summary(df)
Step 2: Descriptive Statistics
summary(df$Revenue)
Step 3: Normality Testing (Revenue)
shapiro.test(df$Revenue)
hist(df$Revenue, prob = TRUE)
qqnorm(df$Revenue); qqline(df$Revenue)
Step 4: Correlation Analysis
cor.test(df$Revenue, df$ADR)
cor.test(df$Revenue, df$OccupancyRate)
Step 5: Simple Linear Regression
model1 <- lm(Revenue ~ ADR, data = df)
summary(model1)
Step 6: Multiple Linear Regression
model2 <- lm(
  Revenue ~ ADR + OccupancyRate + MarketingSpend + StaffCount,
  data = df
)
summary(model2)
5. Top 5 Revenue-Generating Hotels (Supporting Analysis)
df <- df %>%
  mutate(HotelID = paste0("Hotel_", row_number()))

df %>%
  arrange(desc(Revenue)) %>%
  mutate(Rank = row_number()) %>%
  slice_head(n = 5)
This table is illustrative only and supports statistical findings.
