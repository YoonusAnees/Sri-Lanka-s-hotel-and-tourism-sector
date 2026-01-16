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
# Scatter Plot for Revenue with Occupancy Rate
#-----------------------------

scatterplot(hotels$Revenue~hotels$OccupancyRate, regLine=FALSE, smooth=FALSE,
            boxplots=FALSE,
            xlab="Occupancy Rate",
            ylab="Revenue",
            main = "Revenue Vs Occupancy Rate",
            data=hotels)

Revenue_OccupancyRate.Model<-lm(hotels$Revenue~hotels$OccupancyRate,na.exclude=TRUE)
plot(hotels$Revenue ~ hotels$OccupancyRate,
     xlab = "Occupancy Rate",
     ylab = "Revenue",
     main = "Revenue vs Occupancy Rate")
abline(Revenue_OccupancyRate.Model, col = "black")


Revenue_RoomsAvailable.Model<-lm(hotels$Revenue~hotels$RoomsAvailable,na.exclude=TRUE)
plot(hotels$Revenue ~ hotels$RoomsAvailable,
     xlab = "Rooms Available",
     ylab = "Revenue",
     main = "Revenue vs Rooms Available")
abline(Revenue_RoomsAvailable.Model, col = "black")

scatterplotMatrix(
  ~ADR + OccupancyRate + RoomsAvailable + Revenue,
  regLine = FALSE,
  smooth = FALSE,
  diagonal = list(method = "density"),
  data = hotels
)

par(mfrow = c(2, 2))
plot(Mul.Model)







Revenue_MarketingSpend.Model <- lm(Revenue ~ MarketingSpend, data = hotels, na.action = na.exclude)
scatterplot(Revenue ~ MarketingSpend, regLine = FALSE, smooth = FALSE,
            boxplots = FALSE, data = hotels)
abline(Revenue_MarketingSpend.Model, col = "black")


summary(Revenue_MarketingSpend.Model)


# Fit the model
model_marketing <- lm(Revenue ~ MarketingSpend, data = hotels, na.action = na.exclude)

# Residuals vs Fitted plot
plot(
  fitted(model_marketing),
  resid(model_marketing),
  xlab = "Fitted Revenue",
  ylab = "Residuals",
  main = "Residuals vs Fitted Values: Revenue ~ Marketing Spend"
)

abline(h = 0, col = "green")



#Revenue with HotelQualityRank



#Convert To Number 

hotels$HotelQualityRank_num <- dplyr::case_when(
  hotels$HotelQualityRank == "Low"    ~ 1,
  hotels$HotelQualityRank == "Medium" ~ 2,
  hotels$HotelQualityRank == "High"   ~ 3
)

# Check
table(hotels$HotelQualityRank, hotels$HotelQualityRank_num)









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



#-----------------------
#Regression Model
#-----------------------

lm(formula = Revenue ~ ADR, data = hotels)

lm(formula = Revenue ~ OccupancyRate, data = hotels)

lm(formula = Revenue ~ RoomsAvailable, data = hotels)






#--------------------
#Model Compersion
#---------------------

plot(hotels$Revenue ~ hotels$ADR,
     xlab = "ADR",
     ylab = "Revenue",
     main = "Revenue vs ADR")
abline(Revenue_ADR.Model, col = "black")

plot(hotels$Revenue ~ hotels$OccupancyRate,
     xlab = "OccupancyRate",
     ylab = "Revenue",
     main = "Revenue vs OccupancyRate")
abline(Revenue_OccupancyRate.Model, col = "black")

#---------------
#Residual 
#---------------

ADR<-resid(Revenue_ADR.Model)
plot(fitted(Revenue_ADR.Model),ADR)
abline(0,0,col="Green")

OR<resid(Revenue_OccupancyRate.Model)
plot(fitted(Revenue_OccupancyRate.Model),OR)
abline(0,0,col="Green")



# fit model safely
model_occ <- lm(Revenue ~ OccupancyRate, data = hotels)
roomsAvailable.Model <- lm(Revenue~RoomsAvailable, data=hotels)
summary(roomsAvailable.Model)

# residual plot
plot(
  fitted(model_occ),
  resid(model_occ),
  xlab = "Fitted Revenue",
  ylab = "Residuals",
  main = "Residuals vs Fitted Values: Revenue ~ Occupancy Rate"
)

abline(h = 0, col = "green")

# residual plot
plot(
  fitted(roomsAvailable.Model),
  resid(roomsAvailable.Model),
  xlab = "Fitted Revenue",
  ylab = "Residuals",
  main = "Residuals vs Fitted Values: Revenue ~ RoomsAvailable"
)

abline(h = 0, col = "green")


plot(
  fitted(Revenue_MarketingSpend.Model),
  resid(Revenue_MarketingSpend.Model),
  xlab = "Fitted Revenue",
  ylab = "Residuals",
  main = "Residuals vs Fitted Values: Revenue ~ RoomsAvailable"
)

abline(h = 0, col = "green")




---------------
-  AIC
--------------
  
  # Simple regression models
AIC(lm(Revenue ~ ADR, data = hotels))
AIC(lm(Revenue ~ OccupancyRate, data = hotels))
AIC(lm(Revenue ~ RoomsAvailable, data = hotels))

# Multiple regression model
AIC(RegModel.9)







