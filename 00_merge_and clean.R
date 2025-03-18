library(tidyverse)
library(rlist)
library(readxl)
library(ggpubr)
library(rempsyc)

# Select relevant qct columns
qct_cols = c(
  # headers
  "DB_PatientName",
  "DB_ImageType",
  "DB_StudyInstanceUid",
  "DB_StudyDateTime",
  "DB_StudyDescription",
  # "DB_SeriesDateTime",
  "DB_SeriesDescription",
  # density fields
  "Mean(HU)_WholeLung",
  "LAA_Volume(%)_WholeLung",
  "Volume(cc)_WholeLung"
)

# Read Coreline AVIEW output
page1 = read_csv('data/thermoplasty_20250219_125929_corrected.csv')
page2 = read_csv('data/thermoplasty_20250219_125930_corrected.csv') %>%
  mutate(
    DB_StudyDateTime = parse_datetime(DB_StudyDateTime, format = '%d/%m/%Y  %H:%M')
    # DB_SeriesDateTime = parse_datetime(DB_SeriesDateTime, format = '%d/%m/%Y  %H:%M')
    )
page3 = read_csv('data/thermoplasty_20250219_181925_corrected.csv') %>%
  mutate(
    DB_StudyDateTime = parse_datetime(DB_StudyDateTime, format = '%d/%m/%Y  %H:%M')
    # DB_SeriesDateTime = parse_datetime(DB_SeriesDateTime, format = '%d/%m/%Y  %H:%M')
  )
page4 = read_csv('data/thermoplasty_20250311_165300_corrected.csv') %>%
  mutate(DB_StudyDateTime = parse_datetime(DB_StudyDateTime, format = '%d/%m/%Y  %H:%M'))

# airway fields
airway_cols = c()
for(level in c("Whole", "5", "6", "7", "8")) {
  for(region in c("WholeLung", "RtUpper", "RtMiddle", "RtLower", "LtUpper", "LtLower")){
    pi10 = paste("FWHM_Awt-Pi10(mm)_Level", level, "_", region, sep = "")
    airway_cols = append(airway_cols, pi10)
    wa = paste("FWHM_WallArea%Mean(%)_Level", level, "_", region, sep = "")
    airway_cols = append(airway_cols, wa)
    la = paste("FWHM_LumenAreaMean(mm2)_Level", level, "_", region, sep = "")
    airway_cols = append(airway_cols, la)
  }
}
qct_cols = append(qct_cols, airway_cols)
qct_cols = append(qct_cols, names(select(page1, contains("AirTrap"))))
airtrap_cols = qct_cols[grep("AirTrap", qct_cols)]
page1 = page1 %>% select(qct_cols)
page2 = page2 %>% select(qct_cols)
page4 = page4 %>% select(qct_cols)
# Correct a wrong StudyDescription
# correction = page1 %>%
#   filter(
#     DB_PatientName == 'T010008',
#     DB_StudyDescription == 'CT Chest high resolution_BASELINE') %>%
#   mutate(DB_StudyDescription = 'CT Chest high resolution_FOLLOWUP')
# page1 = page1 %>%
#   filter(
#     !(DB_PatientName == 'T010008' & DB_StudyDescription == 'CT Chest high resolution_BASELINE')
#   ) %>%
#   bind_rows(correction)


# fit lm of air trapping ~ volumes
# qct = bind_rows(page1, page2, page3, page4) %>%
#   select(DB_PatientName, DB_ImageType, DB_StudyInstanceUid, DB_StudyDescription, DB_SeriesDescription, `Volume(cc)_WholeLung`, `AdvAirTrap_Ovr_fAT(%)_WholeLung`) %>%
#   # select(DB_PatientName, DB_StudyInstanceUid, DB_SeriesDescription, qct_cols) %>%
#   filter(
#     # Drop DERIVED data
#     !str_detect(DB_ImageType, 'DERIVED')
#   ) %>%
#   filter(!is.na(`Volume(cc)_WholeLung`)) %>%
#   mutate(series_type = str_split_i(DB_SeriesDescription, "_", -1)) %>%
#   pivot_wider(
#     names_from = series_type,
#     values_from = `Volume(cc)_WholeLung`
#   ) %>%
#   group_by(DB_PatientName, DB_StudyInstanceUid) %>%
#   fill(everything(), .direction = "downup") %>%
#   distinct() %>%
#   mutate(diff = INSPIRATION - EXPIRATION)
# m = lm(qct$`AdvAirTrap_Ovr_fAT(%)_WholeLung` ~ qct$diff)
# summary(m)

# merge qct pages
qct = bind_rows(page1, page2, page3, page4) %>%
  select(DB_PatientName, DB_StudyInstanceUid, DB_SeriesDescription, qct_cols) %>%
  filter(
    # Drop DERIVED data
    !str_detect(DB_ImageType, 'DERIVED')
  ) %>%
  filter(
    # Only include inspiration,
    str_detect(DB_SeriesDescription, 'INSPIRATION')) %>%
  group_by(DB_PatientName, DB_StudyInstanceUid) %>%
  fill(qct_cols, .direction = 'downup') %>%
  rename(PatientID = DB_PatientName)

qct_verification = read_csv('data/qct_verification.csv') %>%
  rename(PatientID = DB_PatientName)

qct_cols[1] = "PatientID"

# filter qct on data that was verified and patients with 2 timepoints
qct = qct %>%
  right_join(qct_verification, by = join_by(PatientID, DB_SeriesDescription, DB_ImageType, DB_StudyDescription)) %>%
  select(PatientID, qct_cols) %>%
  distinct()

# define clinical data fields
clinical_cols = c(
  "PatientID",
  "visit",
  "date",
  "age",
  "sex",
  "BMI",
  "GINA Classification",
  "Post BD FEV1 (% pred)",
  "Post BD FVC (% pred)",
  "Post BD FEV1/FVC Ratio (%)",
  "Blood_eos (x109/L)",
  "ACQ6",
  "ACQ7",
  "AQLQ_TOTAL SCORE"
)

# read and clean clinical data
clinical_data = 'data/BT full data 22 01 2019.xlsx'
demographics = read_excel(clinical_data, 'Demographics') %>%
  mutate(visit = 'visit 1') %>%
  rename('date' = 'Visit 1 date')
prebt1 = read_excel(clinical_data, 'Baseline Pre-BT1') %>%
  mutate(visit = 'pre-BT') %>%
  rename('date' = 'Pre BT1 Visit date (DD/MM/YYYY)')
week6 = read_excel(clinical_data, '6weeks FU') %>%
  mutate(visit = '6-week FU') %>%
  rename('date' = '6 Weeks follow up date (DD/MM/YYYY)')
month6 = read_excel(clinical_data, '6months FU') %>%
  mutate(visit = '6-month FU') %>%
  rename('date' = '6 Months Follow Up Date (DD/MM/YYYY)')
# combine clinical data and select relevant columns
clinical_data = bind_rows(demographics, prebt1, week6, month6) %>%
  mutate(PatientID = str_replace(`Patient No.`, '/', '')) %>%
  rename(
    age = "Age (Yrs)",
    sex = "Sex",
    BMI = "BMI (kg/m2)"
  ) %>%
  group_by(PatientID) %>%
  fill(age, sex, BMI, .direction = 'downup') %>%
  ungroup() %>%
  mutate(
    visit = parse_factor(visit),
    sex = parse_factor(sex)) %>%
  drop_na(PatientID) %>%
  select(all_of(clinical_cols)) %>%
  arrange(PatientID, visit)

df = qct %>%
  left_join(clinical_data, relationship = 'many-to-many') %>%
  mutate(
    # Get timepoint from StudyDescription
    timepoint = str_split_i(DB_StudyDescription, '_', -1),
    # get ct to visit time diff
    ct_to_visit = abs(DB_StudyDateTime - date))

baselines = df %>%
  filter(timepoint == 'BASELINE') %>%
  group_by(PatientID) %>%
  filter(visit %in% c('visit 1', 'pre-BT')) %>%
  fill(`GINA Classification`, .direction = 'downup') %>%
  filter(visit == 'pre-BT')

followups = df %>%
  filter(timepoint == 'FOLLOWUP') %>%
  group_by(PatientID) %>%
  mutate(mindatediff = min(ct_to_visit)) %>%
  filter(visit == '6-week FU')

df = bind_rows(baselines, followups) %>%
  rename_with(function(col) gsub('FWHM_Awt-Pi10\\(mm\\)', 'Pi10', col)) %>%
  rename_with(function(col) gsub('FWHM_WallArea%Mean\\(%\\)', 'WAperc', col)) %>%
  rename_with(function(col) gsub('FWHM_LumenAreaMean\\(mm2\\)', 'la', col)) %>%
  rename_with(function(col) gsub('AirTrapVolume\\(%\\)', 'ATperc', col)) %>%
  ungroup()
airway_cols = gsub('FWHM_Awt-Pi10\\(mm\\)', 'Pi10', airway_cols)
airway_cols = gsub('FWHM_WallArea%Mean\\(%\\)', 'WAperc', airway_cols)
airway_cols = gsub('FWHM_LumenAreaMean\\(mm2\\)', 'la', airway_cols)
airtrap_cols = gsub('AirTrapVolume\\(%\\)', 'ATperc', airtrap_cols)

df = df %>%
  mutate(
    site = str_sub(PatientID, 1, 3)
  )

t = df %>%
  group_by(timepoint) %>%
  summarise(
    "Pi10 (mm)" = paste(round(mean(Pi10_LevelWhole_WholeLung), 2), " ± ", round(sd(Pi10_LevelWhole_WholeLung), 2), sep = ""),
    "WA (%)" = paste(round(mean(WAperc_LevelWhole_WholeLung), 2), " ± ", round(sd(WAperc_LevelWhole_WholeLung), 2), sep = ""),
    "LA (mm^2)" = paste(round(mean(la_LevelWhole_WholeLung), 2), " ± ", round(sd(la_LevelWhole_WholeLung), 2), sep = ""),
    "MLD (HU)" = paste(round(mean(`Mean(HU)_WholeLung`), 2), " ± ", round(sd(`Mean(HU)_WholeLung`), 2), sep = ""),
    "Volume (cc)" = paste(round(mean(`Mean(HU)_WholeLung`), 2), " ± ", round(sd(`Mean(HU)_WholeLung`), 2), sep = ""),
    "Air trapping (%)" = paste(round(mean(`AdvAirTrap_Ovr_fAT(%)_WholeLung`, na.rm = TRUE), 2), " ± ", round(sd(`AdvAirTrap_Ovr_fAT(%)_WholeLung`, na.rm = TRUE), 2), sep = ""),
    "ACQ6" = paste(round(mean(`ACQ6`, na.rm = TRUE), 2), " ± ", round(sd(`ACQ6`, na.rm = TRUE), 2), sep = ""),
    "ACQ7" = paste(round(mean(`ACQ7`, na.rm = TRUE), 2), " ± ", round(sd(`ACQ7`, na.rm = TRUE), 2), sep = ""),
    "AQLQ" = paste(round(mean(`AQLQ_TOTAL SCORE`, na.rm = TRUE), 2), " ± ", round(sd(`AQLQ_TOTAL SCORE`, na.rm = TRUE), 2), sep = ""),
    "FEV1 (% pred)" = paste(round(mean(`Post BD FEV1 (% pred)`, na.rm = TRUE), 2), " ± ", round(sd(`Post BD FEV1 (% pred)`, na.rm = TRUE), 2), sep = ""),
    "FVC (% pred)" = paste(round(mean(`Post BD FVC (% pred)`, na.rm = TRUE), 2), " ± ", round(sd(`Post BD FVC (% pred)`, na.rm = TRUE), 2), sep = ""),
    "FEV1/FVC (% pred)" = paste(round(mean(`Post BD FEV1/FVC Ratio (%)`, na.rm = TRUE), 2), " ± ", round(sd(`Post BD FEV1/FVC Ratio (%)`, na.rm = TRUE), 2), sep = "")
  )

my_table = as.data.frame(t(t))

# write_csv(my_table, "my_table.csv")


#####################             ANALYSIS             #########################

# for(var in airway_cols) {
#   formula = as.formula(paste(var, '~ timepoint'))
#   print(formula)
#   print(t.test(formula,
#          data = df,
#          alternative = 'two.sided',
#          paired = TRUE))
  # print(paste("Plotting:", var))
  # p = ggboxplot(df, x = "timepoint", y = var,
  #           color = "timepoint", palette = c("#00AFBB", "#E7B800"),
  #           ylab = var, xlab = "timepoint")
#   print(p)
# }
airtrap_cols = names(df)[grep("AdvAirTrap.+%", names(df))]
airtrap_df = df %>% select(PatientID, site, timepoint, airtrap_cols) # %>%
  # pivot_wider(
  #   names_from = timepoint,
  #   values_from = starts_with('AdvAirTrap')
  # ) %>%
  # na.omit() %>%
  # pivot_longer(
  #   cols = starts_with("AdvAirTrap"),
  #   names_to = c(".value", "timepoint"),
  #   names_pattern = "(.+_.+)_(.+)"
  # )
# qct_cols = qct_cols[grep("FWHM", qct_cols)]
# qct_cols = c("la_LevelWhole_WholeLung")



df = df %>%
  select(PatientID, timepoint, site, contains("la_"))

longer_df = df %>%
  pivot_longer(
    contains("la_"),
      names_to = c("level", "region"),
      names_pattern = "la_Level(.+)_(.+)",
    values_to = "Luminal area"
  )

longer_df = longer_df %>%
  filter(
    level %in% c("5", "6"),
    region != "Whole"
    ) %>%
  mutate(`Luminal area` = na_if(`Luminal area`, 0.0)) %>%
  filter(!is.na(`Luminal area`))

l = ggplot(filter(longer_df, level == "5"),
           aes(
             x = timepoint,
             y = `Luminal area`,
             color = PatientID,
             group = PatientID
             )) +
  geom_line() +
  facet_wrap(vars(region)) +
  labs(
    title = "Luminal area of generation 5 airways in different lobes"
  ) +
  theme(legend.position="none") +
  ylab("Luminal area (mm^2)")
print(l)
ggsave("spaghetti_la_level5.png")

for(var in qct_cols) {
# formula = as.formula(paste(var, '~ timepoint'))
# print(formula)
# print(t.test(formula,
#              data = airtrap_df,
#              alternative = 'two.sided',
#              paired = TRUE))
print(paste("Plotting:", var))
# p = ggboxplot(df, x = "timepoint", y = var,
#               color = "timepoint", palette = c("#00AFBB", "#E7B800"),
#               ylab = var, xlab = "timepoint")
print(p)
# d = ggplot(df , aes(x = .data[[var]], col = timepoint)) +
#   geom_density() +
#   theme_bw()
# print(d)
l = ggplot(df,
           aes(
             x = timepoint,
             y = {{ var }},
             group = PatientID,
             color = PatientID)) +
  geom_line() +
  facet_wrap(vars(site))
print(l)
}


l = ggplot(df,
           aes(
             x = timepoint,
             y = {{ var }},
             group = PatientID,
             color = PatientID)) +
  geom_line() +
  facet_wrap(vars(site))
print(l)
ggsave("spaghetti_la_.png")

# diffs = airtrap_df %>%
#   group_by(PatientID) %>%
# summarise(diff = diff(`AdvAirTrap_Ovr_Normal(%)_WholeLung`))

wide_data <- df %>%
  select(PatientID, timepoint, `ACQ7`) %>%
  pivot_wider(
    names_from = timepoint,
    values_from = `ACQ7`)

# Remove rows where either A or B is NA
paired_data <-
  na.omit(wide_data) %>%
  mutate(
    diff = BASELINE - FOLLOWUP
  )

# Perform paired t-test
t.test(paired_data$BASELINE, paired_data$FOLLOWUP, paired = TRUE)

paired_data %>%
  ggplot(mapping = aes(x = diff)) +
  geom_density()
