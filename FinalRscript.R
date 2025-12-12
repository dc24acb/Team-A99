# PART 1 — CLEANING + SUMMARY (UH STYLE)

# Load libraries
library(readr)
library(tidyverse)

# 1. LOAD DATA

df <- read_csv("Amazon.csv")
View(df)

# 2. READ COLUMNS AND ROWS

colnames(df)
nrow(df)
head(df, 10)

# 3. CLEANING AND PREPARING VARIABLES

# Create shorter variable names
df$Category <- df$category
df$Rating   <- df$rating

# FIX: create Price from discounted_price
df$Price <- df$discounted_price

# Convert Rating and Price to numeric
df$Rating <- as.numeric(df$Rating)
df$Price  <- as.numeric(gsub("[^0-9\\.]", "", df$Price))

# Replace missing values
df$Category[is.na(df$Category)] <- "Unknown"

# Remove rows with missing or invalid ratings
df <- subset(df, !is.na(Rating))
df <- subset(df, Rating > 0 & Rating <= 5)

# Remove rows with missing or invalid prices
df <- subset(df, !is.na(Price))
df <- subset(df, Price > 0)

# Convert Category to factor
df$Category <- as.factor(df$Category)

# 4. SUMMARY STATISTIC

summary(df$Rating)
sd(df$Rating)
mean(df$Rating)
median(df$Rating)

summary(df$Price)
sd(df$Price)
mean(df$Price)
median(df$Price)

# Mean and median rating by category
aggregate(Rating ~ Category, data = df, FUN = mean)
aggregate(Rating ~ Category, data = df, FUN = median)

# 5. SAVE CLEANED DATA

write.csv(df, "Amazon_clean.csv", row.names = FALSE)

# 6. BOXPLOT (MAIN VISUALISATION)

boxplot(Rating ~ Category,
        data = df,
        main = "Product Rating by Category",
        xlab = "Product Category",
        ylab = "Rating",
        col  = "lightblue",
        las = 2)

# 7. HISTOGRAM (SUPPLEMENTARY VISUALISATION)

hist(df$Rating,
     prob = TRUE,
     breaks = 20,
     xlab = "Rating",
     ylab = "Relative Frequency",
     main = "Distribution of Amazon Product Ratings",
     col = "lightblue")

# Normal curve
x <- seq(min(df$Rating), max(df$Rating), length = 40)
f <- dnorm(x, mean = mean(df$Rating), sd = sd(df$Rating))
lines(x, f, col = "red", lwd = 2)

# 8. STATISTICAL TEST — KRUSKAL–WALLIS

kw_result <- kruskal.test(Rating ~ Category, data = df)
kw_result
kw_result$p.value

