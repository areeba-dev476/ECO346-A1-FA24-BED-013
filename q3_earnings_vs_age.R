# ECO346 Assignment 1
# Question 3: Earnings versus Age

# Load the datasets
person <- read.csv("data/pslm_person_subset.csv")
household <- read.csv("data/pslm_hh_subset.csv")

# Join household weights and location information
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

# Create five-year age bands
earners$age_band <- cut(
  earners$age,
  breaks = seq(15, 65, by = 5),
  right = FALSE,
  labels = c(
    "15-19", "20-24", "25-29", "30-34", "35-39",
    "40-44", "45-49", "50-54", "55-59", "60-64"
  )
)

# Calculate weighted mean earnings for each age band
band_means <- aggregate(
  cbind(weighted_earnings = earners$earn_annual * earners$hhweight,
        weight = earners$hhweight),
  by = list(age_band = earners$age_band),
  FUN = sum
)

band_means$weighted_mean <- (
  band_means$weighted_earnings / band_means$weight
)

# Count unweighted observations in each band
band_counts <- aggregate(
  earn_annual ~ age_band,
  data = earners,
  FUN = length
)

names(band_counts)[2] <- "n"

# Combine means and counts
band_table <- merge(
  band_means[, c("age_band", "weighted_mean")],
  band_counts,
  by = "age_band",
  all.x = TRUE
)

# Sort the bands in age order
band_table <- band_table[
  match(levels(earners$age_band), band_table$age_band),
]

# Display the results
band_table$weighted_mean <- round(band_table$weighted_mean)
print(band_table)
# Add age-band midpoints for plotting
band_table$age_midpoint <- seq(17, 62, by = 5)

# Display the updated table
print(band_table)
# Create the Question 3 SVG graph
svglite::svglite(
  "figures/q3_earnings_vs_age.svg",
  width = 9,
  height = 6
)

# Start with an empty graph using a logarithmic earnings axis
plot(
  earners$age,
  earners$earn_annual,
  log = "y",
  xlim = c(15, 65),
  ylim = c(10000, 10000000),
  type = "n",
  xlab = "Age (years)",
  ylab = "Annual earnings (Rs, log scale)",
  main = "Annual Earnings versus Age: KP Urban"
)

# Add individual earners as lightly jittered points
set.seed(346)
points(
  jitter(earners$age, amount = 0.35),
  earners$earn_annual,
  pch = 16,
  cex = 0.55,
  col = adjustcolor("grey35", alpha.f = 0.35)
)

# Add weighted mean earnings for each age band
# Use hollow circles when a band has fewer than 30 observations
for (i in seq_len(nrow(band_table))) {
  point_symbol <- if (band_table$n[i] < 30) 1 else 19
  
  points(
    band_table$age_midpoint[i],
    band_table$weighted_mean[i],
    pch = point_symbol,
    cex = 1.5,
    lwd = 2
  )
}

# Add a legend
legend(
  "topleft",
  legend = c(
    "Individual earners",
    "Weighted mean (n >= 30)",
    "Weighted mean (n < 30)"
  ),
  pch = c(16, 19, 1),
  col = c("grey35", "black", "black"),
  bty = "n",
  cex = 0.8
)

dev.off()