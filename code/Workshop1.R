# Housekeeping
objects()
rm(list = ls())
objects()

library(tidyverse)
library(readxl)
library(here)

# Import standard CSV dataset
benthic_cover <- read_csv(here("data", "reef_cover_log.csv"))

# Import tab-separated telemetry dataset
acoustic_stream <- read_tsv(here("data", "acoustic_telemetry_stream.txt"))

# Import Excel fisheries dataset
fisheries_annual <- read_excel(
  here("data", "fish_catch_data.xlsx"),
  sheet = "Commercial_2026"
)


# Correct import for messy mangrove survey dataset
mangrove_data <- read_csv(
  here("data", "mangrove_survey_raw.csv"),
  skip = 5,
  na = c(".", "NA", "9999", "ND", "blank")
)
# Compare a tibble with a base R data frame
benthic_cover_df <- as.data.frame(benthic_cover)

print(benthic_cover_df)
print(benthic_cover)

# Load the Palmer Penguins dataset
library(palmerpenguins)
data("penguins")

# Inspect the structure of the dataset
glimpse(penguins)
str(penguins)

# Generate an exploratory summary
summary(penguins)


# Select key morphological variables
morphology_metrics <- select(
  penguins,
  species,
  bill_length_mm,
  bill_depth_mm,
  body_mass_g
)

glimpse(morphology_metrics)

# Retain a continuous block of columns
spatial_block <- select(penguins, species:island)

# Remove the year column
clean_scientific_fields <- select(penguins, -year)

# Filter rows for specific ecological groups

# Keep only Adelie penguins
adelie_cohort <- filter(penguins, species == "Adelie")

# Keep penguins weighing more than 4500 g
heavy_penguins <- filter(penguins, body_mass_g > 4500)

# Keep Gentoo penguins from Biscoe Island
biscoe_gentoo <- filter(
  penguins,
  species == "Gentoo" & island == "Biscoe"
)

# Keep penguins recorded on Dream or Torgersen islands
sub_islands <- filter(
  penguins,
  island %in% c("Dream", "Torgersen")
)

# Order observations by body size and species

# Sort penguins from smallest to largest body mass
lightest_first <- arrange(penguins, body_mass_g)

# Sort penguins from largest to smallest body mass
heaviest_first <- arrange(penguins, desc(body_mass_g))

# Sort by species, then by largest bill length within each species
stratified_morphology <- arrange(
  penguins,
  species,
  desc(bill_length_mm)
)

# Compute new morphological variables
penguin_ratios <- penguins |>
  mutate(
    body_mass_kg = body_mass_g / 1000,
    bill_ratio = bill_length_mm / bill_depth_mm
  )

# Inspect the newly created variables
glimpse(penguin_ratios)

# Group penguins by species
grouped_penguins <- group_by(penguins, species)

# Calculate mean body mass for each species
species_mass_summary <- summarise(
  grouped_penguins,
  mean_mass_g = mean(body_mass_g)
)

print(species_mass_summary)

# Calculate body mass summaries while removing missing values
biological_signal <- penguins %>%
  group_by(species, sex) %>%
  summarise(
    sample_size = n(),
    mean_mass_g = mean(body_mass_g, na.rm = TRUE),
    sd_mass_g = sd(body_mass_g, na.rm = TRUE)
  )

print(biological_signal)

# Calculate body mass summaries while removing missing values
biological_signal <- penguins %>%
  group_by(species, sex) %>%
  summarise(
    sample_size = n(),
    mean_mass_g = mean(body_mass_g, na.rm = TRUE),
    sd_mass_g = sd(body_mass_g, na.rm = TRUE)
  )

print(biological_signal)

# Calculate body mass summaries while removing missing values
biological_signal <- penguins %>%
  group_by(species, sex) %>%
  summarise(
    sample_size = n(),
    mean_mass_g = mean(body_mass_g, na.rm = TRUE),
    sd_mass_g = sd(body_mass_g, na.rm = TRUE),
    .groups = "drop"
  )

print(biological_signal)
