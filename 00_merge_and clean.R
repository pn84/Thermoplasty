library(tidyverse)
library(rlist)

df1 = read_csv('data/exp_20250106_134911_pages.csv')
df2 = read_csv('data/exp_20250106_151300_pages.csv')
df2$DB_FilterType = as.character(df2$DB_FilterType)
thermoplasty_df = full_join(df1, df2)

# select relevant columns
cols = c(
  # headers
  "DB_PatientId",
  "DB_PatientName",
  "DB_PatientSex",
  "DB_StudyDescription",
  "DB_SeriesDescription",
  # airways
  "FWHM_Awt-Pi10(mm)_LevelWhole_WholeLung",
  "FWHM_WallArea%Mean(%)_LevelWhole_WholeLung",
  # density
  "Mean(HU)_WholeLung",
  "LAA_Volume(%)_WholeLung"
  # TODO other columns
)


# filter keep only inspiratory row
# filter only patients with both baseline and followup
