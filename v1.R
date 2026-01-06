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


# Load data set
df <- read_csv("HOTELS_2025.csv")

summary(hotels)


# Revenue descriptive statistics
min_rev <- min(hotels$Revenue, na.rm = TRUE)
max_rev <- max(hotels$Revenue, na.rm = TRUE)
mean_rev <- mean(hotels$Revenue, na.rm = TRUE)
median_rev <- median(hotels$Revenue, na.rm = TRUE)
mode_rev <- as.numeric(names(which.max(table(hotels$Revenue, useNA = "no"))))

cat("Revenue - Minimum:", min_rev, "\n")
cat("Revenue - Maximum:", max_rev, "\n")
cat("Revenue - Mean   :", mean_rev, "\n")
cat("Revenue - Median :", median_rev, "\n")
cat("Revenue - Mode   :", mode_rev, "\n\n")

summary(hotels$Revenue)


install.packages("Rcmdr")
library(Rcmdr)


# -----------------------------
#  Histogram with Normal Curve (Revenue)
#-----------------------------



hist(hotels$Revenue,main = "Revenue Distribution ",prob = TRUE,
     xlab = "Revenue ", ylab = "Density")
curve(dnorm(x,sd=sd(hotels$Revenue,na.rm=TRUE),
      mean=mean(hotels$Revenue,na.rm = TRUE)) ,add = TRUE)
# -----------------------------
# normality test
#-----------------------------

install.packages("nortest")
library(nortest)

# -----------------------------
#  Anderson-Darling normality test
#-----------------------------

ad.test(hotels$Revenue)


# -----------------------------
#  Shapiro-Wilk normality test
#-----------------------------

shapiro.test(hotels$Revenue)

# -----------------------------
#  Lilliefors  normality test
#-----------------------------


lillie.test(hotels$Revenue)



# -----------------------------
# Scatter Plot for Revenue with ADR
#-----------------------------

scatterplot(hotels$Revenue~hotels$ADR, regLine=FALSE, smooth=FALSE,
            boxplots=FALSE,
            xlab="ADR",
            ylab="Revenue",
            main = "Revenue Vs ADR",
            data=hotels)

Revenue_ADR.Model<-lm(hotels$Revenue~hotels$ADR,na.exclude=TRUE)
plot(hotels$Revenue ~ hotels$ADR,
     xlab = "ADR",
     ylab = "Revenue",
     main = "Revenue vs ADR")
abline(Revenue_ADR.Model, col = "black")









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















