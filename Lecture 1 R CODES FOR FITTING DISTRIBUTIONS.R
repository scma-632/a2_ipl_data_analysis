# Load required libraries
library(fitdistrplus)
library(ggplot2)

# Ensure no NA or negative values
kholi_scores <- kholi_scores[!is.na(kholi_scores)]

# Optional: remove 0s if needed for certain distributions
positive_scores <- kholi_scores[kholi_scores > 0]

# Fit multiple distributions
fits <- list()

# Always fit normal
fits$norm <- fitdist(kholi_scores, "norm")

# Fit lognormal, gamma, weibull, exponential only if scores are strictly positive
if (all(positive_scores > 0)) {
  fits$lnorm <- tryCatch(fitdist(positive_scores, "lnorm"), error = function(e) NULL)
  fits$gamma <- tryCatch(fitdist(positive_scores, "gamma", start = list(shape = 1, rate = 0.1)), error = function(e) NULL)
  fits$weibull <- tryCatch(fitdist(positive_scores, "weibull"), error = function(e) NULL)
  fits$exp <- tryCatch(fitdist(positive_scores, "exp"), error = function(e) NULL)
}

# Remove any failed fits
fits <- Filter(Negate(is.null), fits)

# Compare AIC values
aic_values <- sapply(fits, function(f) f$aic)
print(aic_values)

# Get the best fit
best_name <- names(which.min(aic_values))
best_fit <- fits[[best_name]]

cat("\n✅ Best fitting distribution:", best_name, "\n")
cat("📌 Parameters:\n")
print(best_fit$estimate)

# Plot density comparison
denscomp(fits, legendtext = names(fits), main = "Virat Kohli's Runs - Fitted Distributions")

# Load libraries
library(fitdistrplus)
library(ggplot2)

# Step 1: Clean the data
kholi_scores <- kholi_scores[!is.na(kholi_scores)]
positive_scores <- kholi_scores[kholi_scores > 0]  # Weibull requires > 0

# Step 2: Fit Weibull distribution
fit_weibull <- fitdist(positive_scores, "weibull")

# Step 3: Display estimated parameters
cat("✅ Weibull Fit Parameters:\n")
print(fit_weibull$estimate)

# Step 4: Plot histogram + fitted Weibull density
hist(positive_scores, breaks = 15, probability = TRUE,
     main = "Histogram with Fitted Weibull Density - Kohli's Scores",
     xlab = "Runs Scored", col = "lightblue", border = "white")

# Add Weibull density curve
x_vals <- seq(min(positive_scores), max(positive_scores), length.out = 100)
y_vals <- dweibull(x_vals, shape = fit_weibull$estimate["shape"], scale = fit_weibull$estimate["scale"])
lines(x_vals, y_vals, col = "red", lwd = 2)

# Optional: add a legend
legend("topright", legend = "Fitted Weibull", col = "red", lwd = 2)


# Load libraries
library(fitdistrplus)

# Ensure clean, positive data for Weibull
kholi_scores <- kholi_scores[!is.na(kholi_scores)]
positive_scores <- kholi_scores[kholi_scores > 0]

# Fit Weibull distribution
fit_weibull <- fitdist(positive_scores, "weibull")

# Goodness-of-fit statistics
gof_result <- gofstat(fit_weibull)

# View the full result
print(gof_result)

pweibull(0.90, fit_weibull)

1.346271 69.900627 

# PDF at score = 50
dweibull(35, shape = 1.346271, scale = 69.900627)

# CDF at score = 50
pweibull(35, shape = 1.346271, scale = 69.900627)

# 95th percentile score
qweibull(0.3, shape = 1.346271, scale = 69.900627)


=====================================================================
library(dplyr)
library(fitdistrplus)
library(ggplot2)

# List of batters
batters <- c("Shubman Gill", "KL Rahul", "F du Plessis", "V Kohli", "RD Gaikwad",
             "S Dhawan", "JC Buttler", "SV Samson", "SA Yadav", "Ishan Kishan",
             "DA Warner", "Q de Kock", "RG Sharma", "D Padikkal", "N Rana",
             "RR Pant", "HH Pandya", "YBK Jaiswal", "S Dube", "N Pooran")

# Loop through each batter
for (batter in batters) {
  cat("\n==============================\n")
  cat("🎯 Player:", batter, "\n")
  
  # Filter player data
  batter_scores <- df_latest1 %>%
    filter(striker == batter) %>%
    pull(runs_scored) %>%
    na.omit()

  if (length(batter_scores) < 10) {
    cat("⚠️ Not enough data to fit distributions. Skipping.\n")
    next
  }
  
  # Positive scores for certain distributions
  positive_scores <- batter_scores[batter_scores > 0]
  
  # Fit distributions
  fits <- list()
  fits$norm <- fitdist(batter_scores, "norm")
  
  if (all(positive_scores > 0)) {
    fits$lnorm <- tryCatch(fitdist(positive_scores, "lnorm"), error = function(e) NULL)
    fits$gamma <- tryCatch(fitdist(positive_scores, "gamma", start = list(shape = 1, rate = 0.1)), error = function(e) NULL)
    fits$weibull <- tryCatch(fitdist(positive_scores, "weibull"), error = function(e) NULL)
    fits$exp <- tryCatch(fitdist(positive_scores, "exp"), error = function(e) NULL)
  }

  # Remove failed fits
  fits <- Filter(Negate(is.null), fits)
  
  # Compare AIC
  aic_values <- sapply(fits, function(f) f$aic)
  print(aic_values)
  
  best_name <- names(which.min(aic_values))
  best_fit <- fits[[best_name]]
  
  cat("✅ Best fitting distribution:", best_name, "\n")
  cat("📌 Parameters:\n")
  print(best_fit$estimate)
  
  # === Plot 1: Density comparison of all fitted distributions ===
  denscomp(fits, legendtext = names(fits),
           main = paste(batter, "- Fitted Distributions"))
  
  # === Plot 2: Histogram + Weibull if it exists ===
  if ("weibull" %in% names(fits)) {
    fit_weibull <- fits$weibull
    hist(positive_scores, breaks = 5, probability = TRUE,
         main = paste(batter, "- Weibull Fit"),
         xlab = "Runs Scored", col = "lightblue", border = "white")
    
    x_vals <- seq(min(positive_scores), max(positive_scores), length.out = 100)
    y_vals <- dweibull(x_vals, shape = fit_weibull$estimate["shape"],
                                 scale = fit_weibull$estimate["scale"])
    
    lines(x_vals, y_vals, col = "red", lwd = 2)
    legend("topright", legend = "Fitted Weibull", col = "red", lwd = 2)
  }
}

0.4652821 0.6416783 

# PDF at score = 50
dlnorm(35, meanlog= 1.346271, sdlog= 0.6416783)

# CDF at score = 50
plnorm(35, meanlog= 1.346271, sdlog= 0.6416783)

# 95th percentile score
qlnorm(0.3, meanlog= 1.346271, sdlog= 0.6416783)

sg = rlnorm(1000,meanlog= 1.346271, sdlog= 0.6416783)
plot(density(sg))
lines(hist(Shubman Gill)

