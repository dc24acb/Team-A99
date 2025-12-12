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

