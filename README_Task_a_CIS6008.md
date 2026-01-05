# CIS6008 – Analytics & Business Intelligence
## Task (a): Statistical Analysis of Hotel Revenue (DOWNLOADABLE README)

This README explains **only Task (a)** of the CIS6008 WRIT1 assessment.  
It shows how to run the statistical analysis in **R / RStudio** and how to prepare **Appendix A** for submission.

---

## 1. Purpose of Task (a)

Task (a) aims to identify **factors influencing hotel revenue** using statistical and business analytics techniques.  
The analysis supports **evidence-based decision-making** for Sri Lanka’s hotel and tourism sector.

Core outcomes:
- Identify key revenue drivers
- Build statistical models
- Support policy and managerial decisions

---

## 2. Dataset Used

**File:** `HOTELS_2025.csv`  
**Records:** 500 hotels  
**Variables:** 9 (8 numerical, 1 categorical)

Key variables:
- Revenue (dependent variable)
- ADR, OccupancyRate, MarketingSpend, StaffCount, GuestSatisfactionScore, LoyaltyMembers (explanatory)

---

## 3. Software & Packages

Required:
- R (latest version)
- RStudio

Required R packages:
```r
install.packages("tidyverse")
```
Optional:
```r
install.packages("nortest")
```

---

## 4. Task (a) Analysis Workflow

### Step 1: Load Data
```r
df <- read.csv("HOTELS_2025.csv")
summary(df)
```

### Step 2: Descriptive Statistics
```r
summary(df$Revenue)
```

### Step 3: Normality Testing (Revenue)
```r
shapiro.test(df$Revenue)
hist(df$Revenue, prob = TRUE)
qqnorm(df$Revenue); qqline(df$Revenue)
```

### Step 4: Correlation Analysis
```r
cor.test(df$Revenue, df$ADR)
cor.test(df$Revenue, df$OccupancyRate)
```

### Step 5: Simple Linear Regression
```r
model1 <- lm(Revenue ~ ADR, data = df)
summary(model1)
```

### Step 6: Multiple Linear Regression
```r
model2 <- lm(
  Revenue ~ ADR + OccupancyRate + MarketingSpend + StaffCount,
  data = df
)
summary(model2)
```

---

## 5. Top 5 Revenue-Generating Hotels (Supporting Analysis)

```r
df <- df %>%
  mutate(HotelID = paste0("Hotel_", row_number()))

df %>%
  arrange(desc(Revenue)) %>%
  mutate(Rank = row_number()) %>%
  slice_head(n = 5)
```

This table is **illustrative only** and supports statistical findings.

---

## 6. Appendix A – What to Include

Appendix A must contain:
- R scripts used
- Console outputs (screenshots)
- Plots (histogram, QQ plot, scatterplots)
- Regression summaries

Do NOT include:
- Raw datasets
- Written discussion

Example heading:
> Appendix A – Statistical Analysis of Hotel Revenue (Task a)

---

## 7. Reporting Requirement

In the main report, include this sentence:
> All R scripts, statistical outputs, and graphical evidence supporting this analysis are provided in Appendix A.

---

## 8. Submission Notes

- Main report: **PDF**
- Appendix A: included after references
- Referencing style: **Harvard**
- CRS / GIS tools are NOT required for Task (a)

---

## 9. Academic Integrity

- Do not fabricate results
- Do not copy code without understanding
- Screenshots must match reported outputs

---

End of README – Task (a)
