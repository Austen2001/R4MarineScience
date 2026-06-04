# Workshop 2: Advanced data wrangling
# Extracting ecological signals from noisy systems

# Housekeeping
rm(list = ls())

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(palmerpenguins)
library(here)

# Section 2.2: Understanding tidy data

# Inspect a tidy example dataset
table1

# Calculate disease case rate per 10,000 people
table1_rate <- table1 |>
  mutate(rate = cases / population * 10000)

print(table1_rate)

# Summarise total cases recorded in each year
cases_per_year <- table1 |>
  count(year, wt = cases)

print(cases_per_year)

# Visualise changes in cases through time for each country
ggplot(table1, aes(x = year, y = cases)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country)) +
  labs(
    title = "Reported Cases Through Time",
    x = "Year",
    y = "Number of cases",
    colour = "Country"
  ) +
  theme_minimal()
# Section 2.4: Lengthening datasets with pivot_longer()

# Inspect the original wide billboard dataset
billboard

# Convert weekly ranking columns into long format
billboard_long <- billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank"
  )

# Inspect the lengthened dataset
glimpse(billboard_long)

# Remove weeks where songs were not ranked
billboard_ranked <- billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )

# Inspect the dataset after removing structural missing values
glimpse(billboard_ranked)

# Simple example of pivoting a dataset longer
bp_wide <- tribble(
  ~id, ~bp1, ~bp2,
  "A", 100, 120,
  "B", 140, 115,
  "C", 120, 125
)

bp_long <- bp_wide |>
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  )

print(bp_long)

# Section 2.5: Widening datasets with pivot_wider()

# Inspect the patient experience dataset
cms_patient_experience

# View the different measurement codes
patient_measures <- cms_patient_experience |>
  distinct(measure_cd, measure_title)

print(patient_measures)

# First attempt to widen the dataset
patient_wide_attempt <- cms_patient_experience |>
  pivot_wider(
    names_from = measure_cd,
    values_from = prf_rate
  )

glimpse(patient_wide_attempt)

# Correctly widen the dataset so each organisation occupies one row
patient_wide <- cms_patient_experience |>
  pivot_wider(
    id_cols = starts_with("org"),
    names_from = measure_cd,
    values_from = prf_rate
  )

# Inspect the corrected wide dataset
glimpse(patient_wide)

# Simple example of pivoting a dataset wider
bp_longer <- tribble(
  ~id, ~measurement, ~value,
  "A", "bp1", 100,
  "B", "bp1", 140,
  "B", "bp2", 115,
  "A", "bp2", 120,
  "A", "bp3", 105
)

bp_wider <- bp_longer |>
  pivot_wider(
    names_from = measurement,
    values_from = value
  )

print(bp_wider)

# Section 2.6: Pivoting exercises using Palmer Penguins

# Reshape four morphometric measurements into long format
penguins_long <- penguins |>
  pivot_longer(
    cols = c(
      bill_length_mm,
      bill_depth_mm,
      flipper_length_mm,
      body_mass_g
    ),
    names_to = "measurement_type",
    values_to = "value"
  )

# Inspect the long-format dataset
head(penguins_long)
glimpse(penguins_long)

# Visualise distributions of the four morphometric measurements
penguin_measurement_plot <- penguins_long |>
  drop_na(value) |>
  ggplot(aes(x = value, fill = species)) +
  geom_histogram(
    bins = 30,
    alpha = 0.7,
    colour = "black"
  ) +
  facet_wrap(
    ~ measurement_type,
    scales = "free_x"
  ) +
  theme_minimal() +
  labs(
    title = "Morphometric distributions across penguin species",
    x = "Measurement value",
    y = "Frequency",
    fill = "Species"
  )

penguin_measurement_plot

# Summarise mean body mass for each species on each island
mass_summary <- penguins |>
  drop_na(body_mass_g) |>
  group_by(species, island) |>
  summarise(
    mean_mass = mean(body_mass_g),
    .groups = "drop"
  )

print(mass_summary)

# Widen the summary table so islands form the columns
mass_matrix <- mass_summary |>
  pivot_wider(
    names_from = island,
    values_from = mean_mass
  )

print(mass_matrix)
# Section 2.7: Separating and uniting data tables

# Inspect a dataset where two variables are combined in one column
table3

# Separate the rate column into cases and population
table3_separated <- table3 |>
  separate(
    rate,
    into = c("cases", "population"),
    sep = "/",
    convert = TRUE
  )

print(table3_separated)

# Separate the year into century and final two digits
table3_year_split <- table3 |>
  separate(
    year,
    into = c("century", "year_short"),
    sep = 2
  )

print(table3_year_split)

# Reunite century and year into a complete year column
table3_year_reunited <- table3_year_split |>
  unite(
    year,
    century,
    year_short,
    sep = ""
  )

print(table3_year_reunited)

# Section 2.8: Wrangling strings and dates

# Create a messy dataset of field site names
messy_sites <- tibble(
  site_id = c(
    " Nelly Bay",
    "nelly_bay",
    "NELLY BAY",
    " Geoffrey_Bay ",
    "geoffrey bay"
  )
)

# Standardise site names so matching sites use identical text
clean_sites <- messy_sites |>
  mutate(
    site_clean = str_trim(site_id),
    site_clean = str_to_lower(site_clean),
    site_clean = str_replace_all(site_clean, pattern = "\\s+", replacement = "_")
  )

print(clean_sites)

# Parse dates written in different formats
date_1 <- dmy("25/12/2026")
date_2 <- ymd("2026-12-25")

# Confirm that both text formats represent the same date
date_1 == date_2

# Create raw sensor data containing character timestamps
sensor_data <- tibble(
  raw_time = c(
    "14-05-2026 08:30:00",
    "14-05-2026 08:45:00",
    "14-05-2026 09:00:00"
  ),
  temperature = c(24.5, 24.6, 24.4)
)

# Convert raw timestamps into date time objects
sensor_clean <- sensor_data |>
  mutate(
    true_time = dmy_hms(raw_time)
  )

print(sensor_clean)
glimpse(sensor_clean)

# Section 2.9: Relational data and joining tables

# Biological observation data
observations <- tibble(
  site_code = c("NB", "GB", "MI", "NB"),
  species = c("Trout", "Snapper", "Trout", "Cod"),
  count = c(5, 2, 1, 3)
)

# Spatial metadata for each site
site_metadata <- tibble(
  site_code = c("NB", "GB", "MI", "RP"),
  zone = c(
    "Marine National Park",
    "Conservation Park",
    "Habitat Protection",
    "General Use"
  ),
  lat = c(-19.16, -19.15, -19.14, -19.12)
)

# Keep every biological observation and attach matching site metadata
joined_data <- observations |>
  left_join(
    site_metadata,
    by = join_by(site_code)
  )

print(joined_data)

# Keep only observations with matching site metadata
matched_data <- observations |>
  inner_join(
    site_metadata,
    by = join_by(site_code)
  )

print(matched_data)

# Check whether any observations lack matching metadata
missing_context <- observations |>
  anti_join(
    site_metadata,
    by = join_by(site_code)
  )

print(missing_context)

# Section 2.10: Handling missing values

# Simulate raw temperature logger data containing a sensor error code
logger_data <- tibble(
  depth_m = c(10, 20, 30, 40),
  temp_c = c(24.5, 24.1, -999, 23.5)
)

# Convert the known sensor failure code into a true missing value
fixed_logger <- logger_data |>
  mutate(
    temp_c = na_if(temp_c, -999)
  )

print(fixed_logger)

# Simulate shark survey counts where a blank value means zero observed
shark_counts <- tibble(
  site = c("Reef_A", "Reef_B", "Reef_C"),
  shark_count = c(3, NA, 5)
)

# Replace missing shark counts with zero because no individuals were observed
shark_fixed <- shark_counts |>
  mutate(
    shark_count = coalesce(shark_count, 0)
  )

print(shark_fixed)

# Demonstrate a mathematically impossible CPUE calculation
cpue_data <- tibble(
  site = c("Bay_1", "Bay_2"),
  catch = c(10, 0),
  effort_hours = c(2, 0)
)

# Calculate catch per unit effort
cpue_calc <- cpue_data |>
  mutate(
    cpue = catch / effort_hours
  )

print(cpue_calc)

# Identify rows containing NaN values
cpue_errors <- cpue_calc |>
  filter(is.nan(cpue))

print(cpue_errors)

# Raw catch data only records species that were observed
raw_catch <- tibble(
  site = c("Reef_1", "Reef_1", "Reef_2"),
  species = c("Pmaculatus", "Pleopardus", "Pmaculatus"),
  count = c(5, 2, 8)
)

# Add missing site and species combinations as explicit zero catches
full_catch_matrix <- raw_catch |>
  complete(
    site,
    species,
    fill = list(count = 0)
  )

print(full_catch_matrix)

# Remove sensor rows where the response variable is missing
sensor_log <- tibble(
  day = 1:4,
  salinity = c(35.2, 35.1, NA, 35.3)
)

clean_log <- sensor_log |>
  drop_na(salinity)

print(clean_log)

# Section 2.11: Practical exercises with penguin metadata

# Create the messy metadata provided by a colleague
island_metadata <- tibble(
  island_name = c(" biscoe", "Dream ", "Torgersen"),
  station_install = c("15/01/2003", "22-03-2004", "05/11/2001"),
  latitude = c(-64.81, -64.73, -64.76)
)

print(island_metadata)

# Clean island names and convert installation dates into Date objects
clean_metadata <- island_metadata |>
  mutate(
    island_name = str_trim(island_name),
    island_name = str_to_title(island_name),
    station_install = dmy(station_install)
  )

print(clean_metadata)
glimpse(clean_metadata)

# Join cleaned island metadata to the biological penguin observations
penguins_spatial <- penguins |>
  left_join(
    clean_metadata,
    by = join_by(island == island_name)
  )

# Check that latitude and station installation date were added
head(penguins_spatial)
glimpse(penguins_spatial)

# Check whether any penguin island failed to match the metadata table
unmatched_islands <- penguins |>
  distinct(island) |>
  anti_join(
    clean_metadata,
    by = join_by(island == island_name)
  )

print(unmatched_islands)

# Create a wide summary table of maximum body mass by species and island
penguin_max_mass <- penguins_spatial |>
  drop_na(body_mass_g) |>
  group_by(species, island) |>
  summarise(
    max_body_mass_g = max(body_mass_g),
    .groups = "drop"
  )

print(penguin_max_mass)

# Pivot island names into columns for a presentation-ready matrix
penguin_max_mass_matrix <- penguin_max_mass |>
  pivot_wider(
    names_from = island,
    values_from = max_body_mass_g
  )

print(penguin_max_mass_matrix)

# Section 2.12: Estuary Fish Survey Data Rescue Mission
# AI assistance used to help structure the reproducible import workflow.
# I checked each step before running it.

# Store the folder path containing the Workshop 2 datasets
estuary_path <- here("data", "workshop2")

# Check which site tabs are stored inside the catch log workbook
catch_sheets <- excel_sheets(
  here("data", "workshop2", "estuary_catch_log.xlsx")
)

print(catch_sheets)

# Import the three CSV datasets
estuary_metadata <- read_csv(
  here("data", "workshop2", "estuary_metadata.csv")
)

estuary_sonde <- read_csv(
  here("data", "workshop2", "estuary_sonde_data.csv")
)

species_dictionary <- read_csv(
  here("data", "workshop2", "species_dictionary.csv")
)

# Inspect the structure of each imported CSV file
glimpse(estuary_metadata)
glimpse(estuary_sonde)
glimpse(species_dictionary)

# Phase 1: Ingest the multi-sheet fish catch log

# Read every Excel sheet and combine all site records into one data frame
catch_raw <- catch_sheets |>
  set_names() |>
  map_dfr(
    ~ read_excel(
      here("data", "workshop2", "estuary_catch_log.xlsx"),
      sheet = .x
    ),
    .id = "source_sheet"
  )

# Inspect the combined raw catch log before cleaning
glimpse(catch_raw)
print(catch_raw)

# Standardise site and species names in the catch log
catch_clean <- catch_raw |>
  mutate(
    site = site |>
      str_trim() |>
      str_to_lower() |>
      str_replace_all("\\s+", "_"),
    species = species |>
      str_trim() |>
      str_to_lower() |>
      str_replace_all("\\s+", "_"),
    sampling_date = as.Date(date)
  ) |>
  select(site, sampling_date, species, count)

# Standardise site names in the spatial metadata table
metadata_clean <- estuary_metadata |>
  mutate(
    site = site_name |>
      str_trim() |>
      str_to_lower() |>
      str_replace_all("\\s+", "_"),
    zone = factor(
      zone,
      levels = c("Upstream", "Middle", "Downstream", "Marine")
    )
  ) |>
  select(site, lat, lon, zone)

# Standardise common names in the species dictionary
dictionary_clean <- species_dictionary |>
  mutate(
    common_name = common_name |>
      str_trim() |>
      str_to_lower() |>
      str_replace_all("\\s+", "_")
  )

# Check the cleaned identifiers
distinct(catch_clean, site)
distinct(catch_clean, species)
metadata_clean
dictionary_clean

# Clean high frequency water quality sensor data
sonde_clean <- estuary_sonde |>
  mutate(
    # Standardise site names so they match the catch and metadata tables
    site = site |>
      str_trim() |>
      str_to_lower() |>
      str_replace_all("\\s+", "_"),
    
    # Convert character timestamps into formal date time objects
    datetime = dmy_hm(timestamp),
    
    # Extract the daily sampling date for matching with fish catches
    sampling_date = as.Date(datetime),
    
    # Convert the known turbidity sensor failure code into NA
    turbidity = na_if(turbidity, -999.0)
  ) |>
  select(site, datetime, sampling_date, temperature, salinity, turbidity)

# Check that site names and timestamps were cleaned correctly
glimpse(sonde_clean)
distinct(sonde_clean, site)

# Count how many turbidity failures were converted into NA
sonde_clean |>
  summarise(
    missing_turbidity_values = sum(is.na(turbidity))
  )

# Phase 2: Summarise water quality data to match daily fish surveys

# Calculate daily water quality means for each estuary site
sonde_daily <- sonde_clean |>
  group_by(site, sampling_date) |>
  summarise(
    mean_temperature = mean(temperature, na.rm = TRUE),
    mean_salinity = mean(salinity, na.rm = TRUE),
    mean_turbidity = mean(turbidity, na.rm = TRUE),
    .groups = "drop"
  )

# Inspect the daily water quality summary
glimpse(sonde_daily)
print(sonde_daily)

# Check the number of daily records available for each site
sonde_daily |>
  count(site)
# Attach accepted scientific names to each fish catch record
catch_taxonomy <- catch_clean |>
  left_join(
    dictionary_clean,
    by = join_by(species == common_name)
  ) |>
  select(site, sampling_date, scientific_name, count)

# Inspect the translated fish catch records
glimpse(catch_taxonomy)
print(catch_taxonomy)

# Confirm that every common name successfully matched a scientific name
missing_taxonomy <- catch_taxonomy |>
  filter(is.na(scientific_name))

print(missing_taxonomy)
# Join biological, spatial and daily water quality information
master_observed <- catch_taxonomy |>
  left_join(
    metadata_clean,
    by = join_by(site)
  ) |>
  left_join(
    sonde_daily,
    by = join_by(site, sampling_date)
  )

# Inspect the combined observed catch dataset
glimpse(master_observed)
print(master_observed)

# Audit records that failed to receive spatial or water quality context
missing_join_context <- master_observed |>
  filter(
    is.na(zone) |
      is.na(mean_salinity) |
      is.na(mean_temperature)
  )

print(missing_join_context)


# Phase 3: Build the complete zero catch framework

# Add missing species records for every surveyed site and sampling date
catch_complete <- catch_taxonomy |>
  complete(
    site,
    sampling_date,
    scientific_name
  ) |>
  mutate(
    # Replace newly created missing catches with explicit zero counts
    count = coalesce(count, 0)
  )

# Reattach spatial and daily water quality information to the completed catch matrix
master_data <- catch_complete |>
  left_join(
    metadata_clean,
    by = join_by(site)
  ) |>
  left_join(
    sonde_daily,
    by = join_by(site, sampling_date)
  )

# Inspect the completed master dataset
glimpse(master_data)

# Count observed catches and explicit zero catch records
master_data |>
  summarise(
    total_rows = n(),
    zero_catch_rows = sum(count == 0),
    positive_catch_rows = sum(count > 0)
  )

# Confirm that no join context is missing after completing the dataset
master_missing_context <- master_data |>
  filter(
    is.na(scientific_name) |
      is.na(zone) |
      is.na(mean_salinity) |
      is.na(mean_temperature)
  )

print(master_missing_context)

# Confirm that the legacy turbidity error value is absent
master_data |>
  summarise(
    legacy_turbidity_errors = sum(mean_turbidity == -999, na.rm = TRUE)
  )


# Phase 4: Extract ecological summary statistics

# Summarise fish abundance and daily salinity by scientific species and estuary zone
estuary_summary <- master_data |>
  group_by(scientific_name, zone) |>
  summarise(
    sample_size = n(),
    
    mean_count = mean(count, na.rm = TRUE),
    sd_count = sd(count, na.rm = TRUE),
    se_count = sd_count / sqrt(sample_size),
    
    mean_daily_salinity = mean(mean_salinity, na.rm = TRUE),
    sd_daily_salinity = sd(mean_salinity, na.rm = TRUE),
    se_daily_salinity = sd_daily_salinity / sqrt(sample_size),
    
    .groups = "drop"
  )

# Create a rounded version for presentation in the ePortfolio report
estuary_summary_display <- estuary_summary |>
  mutate(
    across(
      c(
        mean_count,
        sd_count,
        se_count,
        mean_daily_salinity,
        sd_daily_salinity,
        se_daily_salinity
      ),
      ~ round(.x, 2)
    )
  )

print(estuary_summary_display)


# Phase 5: Visualise fish abundance along the estuary salinity gradient

# Create italicised scientific name labels for the plot facets
plot_data <- master_data |>
  mutate(
    species_label = paste0("italic('", scientific_name, "')")
  )

# Plot fish abundance against daily mean salinity for each species
salinity_abundance_plot <- ggplot(
  plot_data,
  aes(x = mean_salinity, y = count)
) +
  geom_point(
    aes(colour = zone),
    alpha = 0.65,
    position = position_jitter(width = 0.12, height = 0.08)
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = "black",
    linewidth = 0.7
  ) +
  facet_wrap(
    ~ species_label,
    labeller = label_parsed,
    scales = "free_y"
  ) +
  labs(
    title = "Fish Abundance Along the Ross River Salinity Gradient",
    subtitle = "Counts include explicit zero catches across all surveyed sites and days",
    x = "Daily mean salinity",
    y = "Fish abundance (count)",
    colour = "Estuary zone"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10),
    plot.title = element_text(face = "bold")
  )

salinity_abundance_plot


# Save final cleaned outputs for the ePortfolio report

# Create the outputs folder if it does not already exist
dir.create(
  here("outputs"),
  showWarnings = FALSE,
  recursive = TRUE
)

# Export the completed master dataset
write_csv(
  master_data,
  here("outputs", "workshop2_estuary_master_data.csv")
)

# Export the corrected statistical summary table
write_csv(
  estuary_summary_display,
  here("outputs", "workshop2_estuary_summary_table.csv")
)

# Export the final publication style figure
ggsave(
  filename = here("outputs", "workshop2_salinity_abundance_plot.png"),
  plot = salinity_abundance_plot,
  width = 12,
  height = 9,
  dpi = 300
)


# Final quality checks

final_checks <- master_data |>
  summarise(
    total_rows = n(),
    zero_catch_rows = sum(count == 0),
    missing_scientific_names = sum(is.na(scientific_name)),
    missing_zones = sum(is.na(zone)),
    missing_salinity = sum(is.na(mean_salinity)),
    legacy_turbidity_errors = sum(mean_turbidity == -999, na.rm = TRUE)
  )

print(final_checks)

