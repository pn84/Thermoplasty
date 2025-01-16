library(tidyverse)
library(rlist)

# Read Coreline AVIEW output
page1 = read_csv('data/exp_20250106_134911_pages.csv')
page2 = read_csv('data/exp_20250106_151300_pages.csv')
# Correct data types
page2$DB_FilterType = as.character(page2$DB_FilterType)
# Correct a wrong StudyDescription
correction = page1 %>%
  filter(
    DB_PatientName == 'T010008',
    DB_StudyDescription == 'CT Chest high resolution_BASELINE') %>%
  mutate('DB_StudyDescription' = 'CT Chest high resolution_FOLLOWUP')
page1 = page1 %>%
  filter(
    !(DB_PatientName == 'T010008' & DB_StudyDescription == 'CT Chest high resolution_BASELINE')
  ) %>%
  bind_rows(correction)

# Select relevant columns
cols = c(
  # headers
  "DB_PatientId",
  "DB_ImageType",
  "DB_PatientName",
  "DB_PatientSex",
  "DB_StudyDescription",
  "DB_SeriesDescription",
  # airway fields
  "FWHM_Awt-Pi10(mm)_LevelWhole_WholeLung",
  "FWHM_WallArea%Mean(%)_LevelWhole_WholeLung",
  # density fields
  "Mean(HU)_WholeLung",
  "LAA_Volume(%)_WholeLung"
)

df = page1 %>%
  select(cols) %>%
  # Join the coreline outputs
  full_join(select(page2, cols)) %>%
  # Get timepoint from StudyDescription
  mutate('timepoint' = sapply(str_split(.$DB_StudyDescription, '_'), tail, 1)) %>%
  group_by(DB_PatientName) %>%
  # Get the number of timepoints for each patient
  mutate(ntimepoints = n_distinct(timepoint)) %>%
  filter(
    # Drop DERIVED data
    !str_detect(DB_ImageType, 'DERIVED'),
    # Only include inspiration,
    !str_detect(DB_SeriesDescription, 'EXPIRATION'),
    # Only include patients who have both baseline and followup data
    ntimepoints == 2,
    # Drop one extra row without measurements
    DB_PatientId != 'T040002'
  )

# TODO Download and clean clinical data