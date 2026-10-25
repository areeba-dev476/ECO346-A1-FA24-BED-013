
# ECO346 Assignment 1
# Question 4: Default Graph

# Load the person dataset
person <- read.csv("data/pslm_person_subset.csv")

# Select KP urban earners aged 15–64
household <- read.csv("data/pslm_hh_subset.csv")

combined <- merge(
  person,
  household[, c("hhid", "hhweight", "prov_code", "urban_rural")],
  by = "hhid"
)

earners <- subset(
  combined,
  prov_code == 1 &
    urban_rural == 2 &
    age >= 15 &
    age <= 64 &
    earn_annual > 0
)

# Create the default scatter plot
svg("figures/default.svg", width = 9, height = 6)

plot(
  earners$age,
  earners$earn_annual,
  xlab = "Age",
  ylab = "Annual earnings",
  main = "Default Graph: Earnings versus Age",
  pch = 16
)

dev.off()
