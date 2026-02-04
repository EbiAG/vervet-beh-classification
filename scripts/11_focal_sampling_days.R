# In this we will identify the months where most focal sampling occurs. 
# We will use this to compare with the accelerometer classifier
# We use the following rule:
# 1. Identify the months for each focal individual where most sampling occurred
# 2. Obtain the top 3 months which have the most sampling for the most individuals
# 3. Choose 7 random days per month for each individual sampled in the month

library(tidyverse)

# Load data ----
# Monthly sampling data
focal_months <- read.csv(
    "./data/raw/focal_sampling/prop_behaviour_monthly_focal.csv"
) %>%
    rename(
        "focalID" = "idindividual1",
    ) %>%
    mutate(
        year_month = ymd(year_month)
    )
str(focal_months)

# Days of sampling
focal_days <- read.csv(
    "./data/raw/focal_sampling/focal_sampled_days.csv"
) %>%
    rename(
        "focalID" = "idindividual1",
    ) %>%
    mutate(
        date = ymd(date)
    )
str(focal_days)

ind <- focal_months %>%
    distinct(focalID) %>%
    pull(focalID)
# Babelas and Heilweiss is missing from the focal_months data
# Babelas is from a group that was not followed
# Heilweiss needs to be checked

# Summary information ----
# Check how many months per individual
focal_months %>%
    group_by(focalID) %>%
    summarise(
        n_months = n_distinct(year_month),
        .groups = "drop"
    ) %>%
    arrange(desc(n_months)) %>%
    group_by(n_months) %>%
    summarise(
        n_individuals = n(),
        .groups = "drop"
    ) %>%
    ungroup() %>%
    arrange(desc(n_months)) %>%
    mutate(
        cprop = cumsum(n_individuals) / sum(n_individuals) * 100
    )

focal_months %>%
    summarise(
        n_individuals = n_distinct(focalID),
        n_months = n_distinct(year_month)
    )
# Total of 35 individuals and 18 months of observation
# Highest sampling is 18 months for 1 individual
# For 60% of individuals we have at least 10 months
# For 4 individuals we have fewer than 6 months.

# Identify the months with most sampling ----
focal_months_sel <- focal_months %>%
    distinct(focalID, year_month, n_scans, n_behaviours) %>%
    group_by(focalID) %>%
    # Select top 3 months with most sampling
    slice_max(n_scans, n = 3, with_ties = FALSE) %>%
    ungroup() %>%
    group_by(year_month) %>%
    summarise(
        n_individuals = n_distinct(focalID),
        total_scans = sum(n_scans),
        total_behaviours = sum(n_behaviours),
        avg_scans = mean(n_scans),
        avg_behaviours = mean(n_behaviours),
        .groups = "drop"
    ) %>%
    arrange(desc(total_scans))

# Based on this, the best months for focal sampling are:
final_months <- focal_months_sel %>%
    slice_max(n_individuals, n = 3, with_ties = FALSE) %>%
    pull(year_month)
final_months
# July 2022, June 2023, September 2022

# Select individuals sampled in these months ----
focal_months_final <- focal_months %>%
    filter(year_month %in% final_months) %>%
    distinct(focalID, year_month, n_scans, n_behaviours)

# Total individuals
focal_months_final %>%
    summarise(
        n_individuals = n_distinct(focalID)
    )
# 32 out of 35 are included in the final months

focal_month_summary <- focal_months_final %>%
    group_by(year_month) %>%
    summarise(
        n_individuals = n_distinct(focalID),
        total_scans = sum(n_scans),
        total_behaviours = sum(n_behaviours),
        avg_scans = mean(n_scans),
        avg_behaviours = mean(n_behaviours),
        .groups = "drop"
    ) %>%
    arrange(desc(total_scans))
# 27 individuals in July and Sept 2022, 23 in June 2023
# On average, about 5 scans per individual and 7 behaviours
write.csv(
    focal_month_summary,
    "./data/temp/focal_sampling/focal_month_summary.csv",
    row.names = FALSE
)

# Final sampled days ----
focal_months_final <- focal_months_final %>%
    select(focalID, year_month) %>%
    distinct()
head(focal_months_final)

## Randomly sample same 7 days across individuals ----
# Sample 7 random days within each month
set.seed(123) # for reproducibility

sampled_days_df <- tibble(year_month = final_months) %>%
    rowwise() %>%
    mutate(
        # Get all days in the month
        days_in_month = list(seq.Date(
            from = year_month,
            to = ceiling_date(year_month, "month") - days(1),
            by = "day"
        )),
        # Sample 7 random days from the month
        sampled_days = list(sample(days_in_month, 7))
    ) %>%
    unnest(sampled_days) %>%
    select(year_month, sampled_day = sampled_days)

# Join with focal individuals
focal_months_final_days <- focal_months_final %>%
    left_join(
        sampled_days_df,
        by = "year_month", relationship = "many-to-many"
    ) %>%
    arrange(focalID, year_month, sampled_day)

# Output CSV ----
write.csv(
    focal_months_final_days,
    "./data/temp/focal_sampling/focal_sampled_days_random.csv",
    row.names = FALSE
)

# This csv is used to subset specific (individual, days) accelerometer data from the full dataset.
# The subset data is found in the /data/raw/focal_sampling folder