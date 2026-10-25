# ECO346 Assignment 1
# Variant 5: Khyber Pakhtunkhwa Urban
# Question 2: Weighted Earnings Distribution

# Load the person-level and household-level datasets
person <- read.csv("data/pslm_person_subset.csv")
household <- read.csv("data/pslm_hh_subset.csv")

# Join household weights and location information to the person data
combined <- merge(
  person,
  household[, c("hhid", "hhweight", "prov_code", "urban_rural")],
  by = "hhid"
)

# Select KP urban observations
kp_urban <- subset(
  combined,
  prov_code == 1 & urban_rural == 2
)

# Keep earners aged 15–64 with positive annual earnings
earners <- subset(
  kp_urban,
  age >= 15 & age <= 64 & earn_annual > 0
)

# Check the number of qualifying earners
nrow(earners)
# Calculate weighted earnings percentiles
weighted_quantile <- function(x, w, probs) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  
  cumulative_weight <- cumsum(w) / sum(w)
  
  sapply(probs, function(p) {
    x[which(cumulative_weight >= p)[1]]
  })
}

# Weighted 10th, 50th, and 90th percentiles
weighted_results <- weighted_quantile(
  x = earners$earn_annual,
  w = earners$hhweight,
  probs = c(0.10, 0.50, 0.90)
)

names(weighted_results) <- c("P10", "Median", "P90")

# Display the weighted percentiles in whole rupees
round(weighted_results)

# Calculate the unweighted median
unweighted_median <- median(
  earners$earn_annual,
  na.rm = TRUE
)

cat("Unweighted median:", round(unweighted_median), "\n")

# Calculate the P90-to-median ratio
ratio <- weighted_results["P90"] / weighted_results["Median"]

cat("P90-to-median ratio:", round(ratio, 2), "\n")

# Count observations excluded from the earnings analysis
excluded <- nrow(kp_urban) - nrow(earners)

cat("KP urban observations:", nrow(kp_urban), "\n")
cat("Qualifying earners:", nrow(earners), "\n")
cat("Excluded observations:", excluded, "\n")
# Create the weighted earnings distribution

# Convert annual earnings to log10 values
log_earnings <- log10(earners$earn_annual)

# Define bins from log10(earnings) = 4 to 7
breaks <- seq(4, 7, by = 0.1)

# Keep observations within the specified bin range
in_range <- log_earnings >= 4 & log_earnings <= 7

plot_data <- log_earnings[in_range]
plot_weights <- earners$hhweight[in_range]

# Calculate weighted density for each bin
bin_index <- findInterval(plot_data, breaks, rightmost.closed = TRUE)

# Put observations at the upper boundary into the last bin
bin_index[bin_index == length(breaks)] <- length(breaks) - 1

weighted_bin_counts <- tapply(
  plot_weights,
  bin_index,
  sum
)

weighted_density <- rep(0, length(breaks) - 1)

weighted_density[as.integer(names(weighted_bin_counts))] <-
  weighted_bin_counts / sum(earners$hhweight) / 0.1

# Save the graph as an SVG file
svglite::svglite(
  "figures/q2_earnings_distribution.svg",
  width = 9,
  height = 6
)

plot(
  breaks[-length(breaks)] + 0.05,
  weighted_density,
  type = "n",
  xlim = c(4, 7),
  ylim = c(0, max(weighted_density) * 1.15),
  xlab = "Annual earnings (Pakistani rupees, log10 scale)",
  ylab = "Weighted density per log10 unit",
  xaxt = "n",
  main = "Annual Earnings Distribution: KP Urban"
)
axis(
  1,
  at = c(4, 5, 6, 7),
  labels = c("Rs 10,000", "Rs 100,000", "Rs 1,000,000", "Rs 10,000,000")
)

rect(
  breaks[-length(breaks)],
  0,
  breaks[-1],
  weighted_density,
  col = "grey80",
  border = "white"
)

abline(v = log10(weighted_results["Median"]), col = "blue", lwd = 2)
abline(v = log10(weighted_results["P90"]), col = "red", lwd = 2)

legend(
  "topright",
  legend = c("Weighted median", "Weighted P90"),
  col = c("blue", "red"),
  lwd = 2,
  bty = "n"
)

dev.off()

# Report how many observations fall outside the bin range
cat("Observations outside the plotted range:", sum(!in_range), "\n")