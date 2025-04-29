library(tidyverse)
library(rlist)
library(readxl)
library(ggpubr)
library(rempsyc)
library(finalfit)
library(flextable)

# TODO write a normality test function that returns variables that are not normally-distributed

# Select relevant qct columns
qct_cols = c(
  # headers
  "DB_PatientName",
  "DB_ImageType",
  "DB_StudyInstanceUid",
  "DB_StudyDateTime",
  "DB_StudyDescription",
  "DB_SeriesInstanceUid",
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

# merge qct pages
qct = bind_rows(page1, page2, page3, page4) %>%
  select(DB_PatientName, DB_StudyInstanceUid, DB_StudyDescription, DB_SeriesDescription, qct_cols) %>%
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
  "BT1_date",
  "BT2_date",
  "BT3_date",
  "Age",
  "Sex",
  "BMI",
  "Weight (kg)",
  "Height (m)",
  "Smoking status",
  "OCS_maintenance_dose",
  "OCS_inhaled_BDP_eq",
  "GINA Classification",
  "Post BD FEV1 (% pred)",
  "Post BD FVC (% pred)",
  "Post BD FEV1/FVC Ratio (%)",
  "Post BD FVC (L)",
  "Blood_eos (x109/L)",
  "ACQ6",
  "ACQ7",
  "AQLQ_TOTAL SCORE"
)

# read and clean clinical data
clinical_data = 'data/BT full data 22 01 2019.xlsx'
demographics = read_excel(clinical_data, 'Demographics') %>%
  mutate(visit = 'visit 1') %>%
  rename(
    'date' = 'Visit 1 date',
    'OCS_maintenance_dose' = "Oral corticosteriod dose (mg)",
    'OCS_inhaled_BDP_eq' = "Inhaled steroid BDP equivalent (mcg)")
prebt1 = read_excel(clinical_data, 'Baseline Pre-BT1') %>%
  mutate(visit = 'pre-BT1') %>%
  rename(
    'date' = 'Pre BT1 Visit date (DD/MM/YYYY)',
    "BT1_date" = "Treatment date (DD/MM/YYYY)"
    )
prebt2 = read_excel(clinical_data, 'Pre-BT2') %>%
  mutate(visit = 'pre-BT2') %>%
  rename(
    'date' = 'Pre BT 2 Visit Date (DD/MM/YYYY)',
    "BT2_date" = "Treatment date (DD/MM/YYYY)"
  )
prebt3 = read_excel(clinical_data, 'Pre-BT3') %>%
  mutate(visit = 'pre-BT3') %>%
  rename(
    'date' = 'Pre BT 3 Visit date (DD/MM/YYYY)',
    "BT3_date" = "Treatment date (DD/MM/YYYY)"
  )
week6 = read_excel(clinical_data, '6weeks FU') %>%
  mutate(visit = '6-week FU') %>%
  rename('date' = '6 Weeks follow up date (DD/MM/YYYY)')
month6 = read_excel(clinical_data, '6months FU') %>%
  mutate(visit = '6-month FU') %>%
  rename('date' = '6 Months Follow Up Date (DD/MM/YYYY)')
# combine clinical data and select relevant columns
clinical_data = bind_rows(demographics, prebt1, prebt2, prebt3, week6, month6) %>%
  mutate(PatientID = str_replace(`Patient No.`, '/', '')) %>%
  rename(
    Age = "Age (Yrs)",
    Sex = "Sex",
    BMI = "BMI (kg/m2)"
  ) %>%
  group_by(PatientID) %>%
  fill(Age, Sex, BMI, `Weight (kg)`, `Height (m)`, `Smoking status`, .direction = 'downup') %>%
  ungroup() %>%
  mutate(
    BSA=0.007184*(`Height (m)`*100)^0.725*`Weight (kg)`^0.425,
    visit = parse_factor(visit),
    Sex = parse_factor(Sex)) %>%
  drop_na(PatientID) %>%
  select(all_of(clinical_cols), BSA) %>%
  arrange(PatientID, visit)

df = qct %>%
  # filter out outlier patient who had an exacerbation during CT
  filter(PatientID != "T030007") %>%
  left_join(clinical_data, relationship = 'many-to-many') %>%
  mutate(
    # Get timepoint from StudyDescription
    timepoint = str_split_i(DB_StudyDescription, '_', -1),
    # get ct to visit time diff
    ct_to_visit = abs(DB_StudyDateTime - date))

baselines = df %>%
  filter(timepoint == 'BASELINE') %>%
  group_by(PatientID) %>%
  filter(visit %in% c('visit 1', 'pre-BT1')) %>%
  fill(`GINA Classification`, `OCS_maintenance_dose`, `OCS_inhaled_BDP_eq`, .direction = 'downup') %>%
  filter(visit == 'pre-BT1')

followups = df %>%
  filter(timepoint == 'FOLLOWUP') %>%
  group_by(PatientID) %>%
  mutate(mindatediff = min(ct_to_visit)) %>%
  filter(
    visit == '6-week FU',
    DB_SeriesInstanceUid != "1.3.12.2.1107.5.1.4.54592.30000015060907333854600015021")

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
  ) %>%
  arrange(PatientID, DB_SeriesInstanceUid)

# Read mucus plugging scores
mucus_plugging = read_csv('data/mucus-plugging.csv') %>%
  filter(PatientID != "T030007") %>%
  select(-PatientID, -comments) %>%
  bind_cols(select(df, PatientID, timepoint))
mucus_plugging[is.na(mucus_plugging)] = 0
rul = c("RUL apical", "RUL posterior", "R anterior")
rml = c("ML lateral", "ML medial")
rll = c("RLL superior", "RLL medial", "RLL anterior", "RLL lateral", "RLL posterior")
lul = c("LUL apicopost", "LUL anterior", "Sup Ling", "Inf Ling" )
lll = c("LLL Superior", "LLL anteromedial", "LLL Lateral", "LLL posterior")

mucus_plugging = mucus_plugging %>%
  pivot_longer(
    cols = c(-PatientID, -timepoint),
    names_to = "region",
    values_to = "mucus_score"
  ) %>%
  mutate(
    lobe = case_when(
      region %in% rul ~ "RtUpper",
      region %in% rml ~ "RtMiddle",
      region %in% rll ~ "RtLower",
      region %in% lul ~ "LtUpper",
      region %in% lll ~ "LtLower",
      region == "Total" ~ "WholeLung"
    ),
    lobe = parse_factor(lobe, levels = c("RtLower", "RtMiddle", "LtLower", "RtUpper", "LtUpper", "WholeLung"))
  ) %>%
  group_by(PatientID, timepoint, lobe) %>%
  mutate(lobe_score = sum(mucus_score))

df = df %>%
  mutate(
    `Smoking status` = ff_label(`Smoking status`, "Smoking status"),
    `Pi10_LevelWhole_WholeLung` = ff_label(`Pi10_LevelWhole_WholeLung`, "Pi10 (mm)"),
    `WAperc_LevelWhole_WholeLung` = ff_label(`WAperc_LevelWhole_WholeLung`, "Airway wall area (%)"),
    `la_LevelWhole_WholeLung` = ff_label(`la_LevelWhole_WholeLung`, "Airway luminal area (mm^2)"),
    `Mean(HU)_WholeLung` = ff_label(`Mean(HU)_WholeLung`, "Mean lung density (HU)"),
    `Volume(cc)_WholeLung` = ff_label(`Volume(cc)_WholeLung`, "Lung volume (ml)"),
    `ACQ6` = ff_label(`ACQ6`, "ACQ6"),
    `AQLQ_TOTAL SCORE` = ff_label(`AQLQ_TOTAL SCORE`, "AQLQ"),
    `Post BD FEV1 (% pred)` = ff_label(`Post BD FEV1 (% pred)`, "FEV1 (%pred)"),
    `Post BD FVC (% pred)` = ff_label(`Post BD FVC (% pred)`, "FVC (%pred)"),
    `Post BD FEV1/FVC Ratio (%)` = ff_label(`Post BD FEV1/FVC Ratio (%)`, "FEV1/FVC (%)")
  )

#####################             ANALYSIS             #########################

#TODO check normality for age, BMI
explanatory_vars = c(
  'Age',
  'Sex',
  'BMI',
  'Smoking.status')
nonpara_vars = c()
for(timepoint in c("BASELINE", "FOLLOWUP")) {
  temp = df %>% filter(timepoint == timepoint)
  for (i in seq_along(explanatory_vars)) {
    if (i %in% c(1, 3)){
    p = shapiro.test(temp[[explanatory_vars[i]]])$p.value
    
      if(p < 0.05) {
        nonpara_vars = c(nonpara_vars, i)
      }
    }
  }
}
nonpara_vars = unique(nonpara_vars)
# create valid column names
colnames(df) <- make.names(colnames(df))
# table 1 - demographics
table1 = df %>%
  filter(timepoint == "BASELINE") %>%
  summary_factorlist(
    dependent = NULL,
    explanatory = explanatory_vars,
    p = FALSE,
    cont = 'mean',
    cont_nonpara = nonpara_vars,
    column = FALSE
  )

save_as_html(flextable(table1), path = 'table1.html')

mucus_totals = mucus_plugging %>%
  ungroup() %>%
  filter(region == "Total") %>%
  select(PatientID, timepoint, mucus_score)
# table 2 - clinical and qct variables
# TODO Add air trapping
explanatory_vars = c(
  "Pi10_LevelWhole_WholeLung",
  "la_LevelWhole_WholeLung",
  "WAperc_LevelWhole_WholeLung",
  "Mean.HU._WholeLung",
  "Volume.cc._WholeLung",
  "ACQ6",
  "AQLQ_TOTAL.SCORE",
  "Post.BD.FEV1....pred.",
  "Post.BD.FVC....pred.",
  "Post.BD.FEV1.FVC.Ratio...."
  # "mucus_score"
)
# test for normality
nonpara_vars = c()
for(timepoint in c("BASELINE", "FOLLOWUP")) {
  temp = df %>% filter(timepoint == timepoint) %>% left_join(mucus_totals)
  for (i in seq_along(explanatory_vars)) {
    p = shapiro.test(temp[[explanatory_vars[i]]])$p.value 
    if(p < 0.05) {
      nonpara_vars = c(nonpara_vars, i)
    }
  }
}
nonpara_vars = unique(nonpara_vars)
table2 = df %>%
  summary_factorlist(
    dependent = "timepoint",
    explanatory = explanatory_vars,
    p = TRUE,
    cont = "mean",
    cont_nonpara = nonpara_vars,
    p_cont_para = "t.test",
    column = TRUE
  )
save_as_html(flextable(table2), path = 'table2.html')

# Luminal Area plots
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

ylims <- c(125, 75)
generations <- c("5", "6")
for (i in seq_along(ylims)) {
  ylim <- ylims[i]
  generation <- generations[i]
  plot_df = filter(longer_df, level == {{ generation }})
  l = ggplot() +
    geom_line(
      data = plot_df,
      mapping = aes(
        x = timepoint,
        y = `Luminal area`,
        color = PatientID,
        group = PatientID)
      ) +
  geom_boxplot(
    data = filter(plot_df, timepoint == 'BASELINE'),
    mapping = aes(y = `Luminal area`),
    position = position_nudge(x = 0.8),
    width = 0.15) +
  geom_boxplot(
    data = filter(plot_df, timepoint == 'FOLLOWUP'),
    aes(y = `Luminal area`),
    position = position_nudge(x = 2.2),
    width = 0.15) +
  facet_wrap(vars(region)) +
  labs(
    title = paste("Luminal area of generation ", generation, " airways in different lobes", sep = "")
  ) +
  ylim(c(0, ylim)) +
  theme(legend.position="none") +
  ylab("Luminal area (mm^2)") +
    xlab("Timepoint")
print(l)
ggsave(paste("spaghetti_la_level", generation, ".png", sep = ""))
}

# timeseries analysis
xlsx = "data/BT full data 22 01 2019.xlsx"
sheets <- excel_sheets(xlsx)
sheets = list()
for (sheet in sheets) {
  xl = read_excel(xslx, sheet = sheet) %>%
    select(c("Patient No."))
}

# clinical visit dates
clinical_visits = clinical_data %>% select(PatientID, visit, date)
timeseries = clinical_data %>%
  select(PatientID, BT1_date, BT2_date, BT3_date) %>%
  pivot_longer(cols = 2:4, names_to = "visit", values_to = "date") %>%
  mutate(visit = str_split_i(visit, pattern = "_", 1)) %>%
  filter(!is.na(date)) %>%
  bind_rows(clinical_visits) %>%
  inner_join(select(baselines, PatientID, CTDate_base = DB_StudyDateTime)) %>%
  inner_join(select(followups, PatientID, CTDate_fup = DB_StudyDateTime)) %>%
  mutate(
    date = as.Date(date),
    CTDate_fup = as.Date(CTDate_fup),
    CTDate_base = as.Date(CTDate_base),
    daydiff_fup = CTDate_fup - date,
    daydiff_base = CTDate_base - date,
    weekdiff_fup = round(difftime(CTDate_fup, date, units = "weeks")),
    weekdiff_base = round(difftime(CTDate_base, date, units = "weeks")),
    visit = parse_factor(visit, levels = c(
      "visit 1",
      "pre-BT1",
      "BT1",
      "pre-BT2",
      "BT2",
      "pre-BT3",
      "BT3",
      "6-week FU",
      "6-month FU"
      ))
    ) %>%
  pivot_longer(
    cols = contains('_'),
    names_pattern = "(.+)_(fup|base)",
    names_to = c(".value", "timepoint")
    ) %>%
  mutate(
    daydiff_cat = case_when(
      daydiff < 0 ~ "CT before visit date",
      daydiff == 0 ~ "CT on visit date",
      daydiff > 0 ~ "CT after visit date"
    )
  ) %>%
  arrange(PatientID, visit)

# timecourse plot
timeseries %>%
  filter(timepoint == "fup") %>%
  ggplot() + 
  geom_boxplot(aes(
    x = visit,
    y = as.numeric(weekdiff),
    fill = visit)
  ) +
  ylab("Time from visit to CT (weeks)") +
  xlab("Visit") +
  theme(legend.position="none") +
  labs(title = "Timecourse")
ggsave('timecourse.png')


timeseries_categories = timeseries %>%
  filter(
    timepoint == "base",
    visit == "pre-BT1"
    ) %>%
  select(PatientID, daydiff_cat)

blood_eos = df %>%
  filter(visit == "pre-BT1") %>%
  mutate(
    Blood_eos = ff_label(Blood_eos..x109.L. * 1000, "Blood eosinophils / ul"),
    Blood_eos_cat = case_when(
      Blood_eos >= 300 ~ "high",
      Blood_eos < 300 ~ "low"
    )
  ) %>%
  filter(!is.na(Blood_eos)) %>%
  select(PatientID, Blood_eos, Blood_eos_cat)

# Mucus plug plot by lobe
mucus_plugging %>%
  left_join(timeseries_categories) %>%
  inner_join(blood_eos) %>%
  select(-region, -mucus_score) %>%
  filter(lobe != "WholeLung") %>%
  arrange(PatientID, lobe) %>%
  distinct() %>%
  ggplot() +
  geom_line(aes(x = timepoint, y = lobe_score, group = PatientID, colour = Blood_eos_cat)) +
  geom_boxplot(
    data = filter(mucus_plugging, timepoint == 'BASELINE', lobe != "WholeLung"),
    aes(y = lobe_score),
    position = position_nudge(x = 0.8),
    width = 0.15) +
  geom_boxplot(
    data = filter(mucus_plugging, timepoint == 'FOLLOWUP', lobe != "WholeLung"),
    aes(y = lobe_score),
    position = position_nudge(x = 2.2),
    width = 0.15) +
  facet_wrap(~ lobe) +
  # theme(legend.position="none") + 
  ylab("Mucus score") +
  labs(
    title = "Mucus score by lobe over time",
    colour = "Blood eosinophils") +
  xlab("Timepoint")
ggsave('mucus_plug_eos_spaghetti_by_lobe.png')

# Total mucus score over time by blood_eos_cat
mucus_plugging %>%
  left_join(timeseries_categories) %>%
  inner_join(blood_eos) %>%
  filter(lobe == "WholeLung") %>%
  arrange(PatientID) %>%
  ggplot() +
  geom_line(aes(x = timepoint, y = mucus_score, group = PatientID, colour = Blood_eos_cat)) + 
  geom_hline(yintercept = 4, alpha = 0.8, linetype = "dashed") +
  geom_boxplot(
    data = filter(mucus_plugging, timepoint == 'BASELINE', lobe == "WholeLung"),
    aes(y = mucus_score),
    position = position_nudge(x = 0.8),
    width = 0.15) +
  geom_boxplot(
    data = filter(mucus_plugging, timepoint == 'FOLLOWUP', lobe == "WholeLung"),
    aes(y = mucus_score),
    position = position_nudge(x = 2.2),
    width = 0.15) +
  # theme(legend.position="none") + 
  ylab("Mucus score") +
  labs(
    title = "Total mucus score over time",
    colour = "Blood eosinophils") +
  xlab("Timepoint")
ggsave('mucus_plug_eos_spaghetti_total.png')

# Total mucus score over time by daydiff_cat
mucus_plugging %>%
  left_join(timeseries_categories) %>%
  inner_join(blood_eos) %>%
  mutate(
    daydiff_cat = factor(daydiff_cat,
                         levels = c("CT on visit date", "CT before visit date", "CT after visit date"),
                         labels = c("CT on pre-BT1 visit", "CT before pre-BT1 visit", "CT on BT1 visit"))
  ) %>%
  filter(lobe == "WholeLung") %>%
  arrange(PatientID) %>%
  ggplot() +
  geom_line(aes(x = timepoint, y = mucus_score, group = PatientID, colour = daydiff_cat)) + 
  geom_hline(yintercept = 4, alpha = 0.8, linetype = "dashed") +
  geom_boxplot(
    data = filter(mucus_plugging, timepoint == 'BASELINE', lobe == "WholeLung"),
    aes(y = mucus_score),
    position = position_nudge(x = 0.8),
    width = 0.15) +
  geom_boxplot(
    data = filter(mucus_plugging, timepoint == 'FOLLOWUP', lobe == "WholeLung"),
    aes(y = mucus_score),
    position = position_nudge(x = 2.2),
    width = 0.15) +
  # theme(legend.position="none") + 
  ylab("Mucus score") +
  labs(
    title = "Total mucus score over time",
    colour = "") +
  xlab("Timepoint")
ggsave('mucus_plug_daydiff_spaghetti_total.png')

# Spirometry plots
clinical_data %>%
  filter(visit != "visit 1") %>%
  arrange(PatientID, visit) %>%
  ggplot() + 
  geom_boxplot(aes(
    x = visit,
    y = `Post BD FEV1 (% pred)`,
    fill = visit
  )) + 
  ylab("FEV1 (% pred)") +
  theme(legend.position="none") + 
  labs(title = "FEV1 over time") +
  xlab("Visit")
ggsave('FEV1 over time.png')

clinical_data %>%
  filter(visit != "visit 1") %>%
  arrange(PatientID, visit) %>%
  ggplot() + 
  geom_boxplot(aes(
    x = visit,
    y = `Post BD FEV1/FVC Ratio (%)`,
    fill = visit
  )) + 
  ylab("FEV1/FVC (%)") +
  theme(legend.position="none") + 
  labs(title = "FEV1/FVC over time") +
  xlab("Visit")
ggsave('FEV1_FVC over time.png')

# Steroid plots
plot_df = mucus_plugging %>%
  filter(region == "Total") %>%
  left_join(df) %>%
  mutate(
    OCS_maintenance_cat = case_when(
      OCS_maintenance_dose <= 0 ~ "no",
      OCS_maintenance_dose > 0 ~ "yes"
    ),
    OCS_maintenance_cat = ff_label(OCS_maintenance_cat, "maintenance OCS") 
  ) %>%
  group_by(PatientID) %>%
  fill(starts_with("OCS"), .direction = "downup") %>%
  ungroup()

# Scatterplot of Mucus score over OCS maintenance dose by timepoint
plot_df %>%
  ggplot(mapping = aes(x = OCS_maintenance_dose, y = mucus_score, colour = timepoint)) +
  geom_jitter(width = 0.3, height = 0.2) + 
  theme_bw() + 
  facet_wrap(~ timepoint) +
  theme(legend.position="none") +
  xlab("OCS maintenance dose (mg)") +
  ylab("Mucus score") + 
  labs(title = "Mucus score over OCS maintenance dose by timepoint")
ggsave('scatterplot_Mucus_OCS_maintenance.png')

# plot_df %>%
#   ggplot(mapping = aes(x = OCS_inhaled_BDP_eq, y = mucus_score, colour = timepoint)) +
#   geom_jitter(width = 0.3, height = 0.3) + 
#   theme_bw() + 
#   facet_wrap(~ timepoint) +
#   xlim(c(0,4000)) +
#   theme(legend.position="none")

plot_df %>%
  ggplot(mapping = aes(x = timepoint, y = mucus_score)) +
  geom_line(aes(group = PatientID, colour = OCS_maintenance_cat)) + 
  geom_boxplot(
    data = filter(plot_df, timepoint == 'BASELINE', lobe == "WholeLung"),
    aes(y = mucus_score),
    position = position_nudge(x = -0.2),
    width = 0.15) +
  geom_boxplot(
    data = filter(plot_df, timepoint == 'FOLLOWUP', lobe == "WholeLung"),
    aes(y = mucus_score),
    position = position_nudge(x = 0.2),
    width = 0.15) +
  theme(legend.position="none") +
  ylab("Mucus score") +
  labs(title = "Total mucus score with and without maintenance OCS") +
  facet_wrap( ~ OCS_maintenance_cat) +
  theme(legend.position="none") +
  xlab("Timepoint")
ggsave("spaghetti_Mucus_score_over_time_by_maintenance_OCS.png")
  
plot_df %>%
  group_by(OCS_maintenance_cat, timepoint) %>%
  summarise(
    mean_mucus_score = mean(mucus_score),
    median_mucus_score = median(mucus_score)
    )


# plot_df %>%
#   filter(OCS_maintenance_cat == "yes") %>%
#   select(PatientID, timepoint, mucus_score) %>%
#   View()
# FVC / Volume scatterplot and histogram
# df %>% ggplot(aes(x = `Post BD FVC (L)`, y = `Volume(cc)_WholeLung` / 1000, colour = timepoint)) +
#   geom_point()
# df %>% ggplot(aes(x=`Volume(cc)_WholeLung`/1000 - `Post BD FVC (L)`)) +
#   geom_histogram()
# df$`Volume(cc)_WholeLung`/1000 - df$`Post BD FVC (L)`


# for(var in qct_cols) {
# formula = as.formula(paste(var, '~ timepoint'))
# print(formula)
# print(t.test(formula,
#              data = airtrap_df,
#              alternative = 'two.sided',
#              paired = TRUE))
# print(paste("Plotting:", var))
# p = ggboxplot(df, x = "timepoint", y = var,
#               color = "timepoint", palette = c("#00AFBB", "#E7B800"),
#               ylab = var, xlab = "timepoint")
# print(p)
# d = ggplot(df , aes(x = .data[[var]], col = timepoint)) +
#   geom_density() +
#   theme_bw()
# print(d)
# l = ggplot(df,
#            aes(
#              x = timepoint,
#              y = {{ var }},
#              group = PatientID,
#              color = PatientID)) +
#   geom_line() +
#   facet_wrap(vars(site))
# print(l)
# }


# l = ggplot(df,
#            aes(
#              x = timepoint,
#              y = {{ var }},
#              group = PatientID,
#              color = PatientID)) +
#   geom_line() +
#   facet_wrap(vars(site))
# print(l)
# ggsave("spaghetti_la_.png")

# diffs = airtrap_df %>%
#   group_by(PatientID) %>%
# summarise(diff = diff(`AdvAirTrap_Ovr_Normal(%)_WholeLung`))

# wide_data <- df %>%
#   select(PatientID, timepoint, `ACQ7`) %>%
#   pivot_wider(
#     names_from = timepoint,
#     values_from = `ACQ7`)

# Remove rows where either A or B is NA
# paired_data <-
#   na.omit(wide_data) %>%
#   mutate(
#     diff = BASELINE - FOLLOWUP
#   )

# Perform paired t-test
# t.test(paired_data$BASELINE, paired_data$FOLLOWUP, paired = TRUE)
# 
# paired_data %>%
#   ggplot(mapping = aes(x = diff)) +
#   geom_density()

# df <- df %>%
#   arrange(PatientID, DB_SeriesInstanceUid) %>%
#   bind_cols(mucus_plugging)




# df %>%
#   ggplot(
#     mapping = aes(
#       x = timepoint,
#       y = Total,
#       group = PatientID,
#       colour = PatientID)) +
#   geom_line() +
#   geom_hline(yintercept = 4) +
#   ylab("Mucus plugging score") +
#   theme(legend.position="none") +
#   labs(title = "Mucus plugging score over time")
# 
# df %>%
#   filter(Total != 0) %>%
#   ggplot(
#     mapping = aes(
#       x = Total,
#       y = `Mean(HU)_WholeLung`,
#       colour = timepoint
#     )
#   ) + 
#   geom_point() +
#   geom_smooth(se = FALSE)
# 
# df %>%
#   ggplot(mapping = aes(
#     x = timepoint,
#     y = `Mean(HU)_WholeLung`,
#     colour = PatientID,
#     group = PatientID
#   )) + 
#   geom_line() +
#   theme(legend.position="none")

# time difference between visit and CT

# mucus_df = df %>%
#   arrange(PatientID, timepoint) %>%
#   group_by(PatientID) %>%
#   filter(!is.na(Total), !str_starts(PatientID, "T04")) %>%
#   mutate(mucus_diff = diff(Total)) %>%
#   select(PatientID, mucus_diff) %>%
#   distinct() %>%
#   mutate(
#     mucus_cat = as.factor(case_when(
#     mucus_diff > 0 ~ "higher",
#     mucus_diff == 0 ~ "no change",
#     mucus_diff < 0 ~ "lower"
#   )))

# table(mucus_df$mucus_cat)
# mucus_df %>%
#   left_join(df) %>%
#   select(PatientID, timepoint, mucus_score=Total, mucus_diff, mucus_cat) %>%
#   write_csv('thermoplasty_mucus_scores.csv')


# 
# t = df %>%
#   group_by(timepoint) %>%
#   summarise(
#     "Pi10 (mm)" = paste(round(mean(Pi10_LevelWhole_WholeLung), 2), " ± ", round(sd(Pi10_LevelWhole_WholeLung), 2), sep = ""),
#     "WA (%)" = paste(round(mean(WAperc_LevelWhole_WholeLung), 2), " ± ", round(sd(WAperc_LevelWhole_WholeLung), 2), sep = ""),
#     "LA (mm^2)" = paste(round(mean(la_LevelWhole_WholeLung), 2), " ± ", round(sd(la_LevelWhole_WholeLung), 2), sep = ""),
#     "MLD (HU)" = paste(round(mean(`Mean(HU)_WholeLung`), 2), " ± ", round(sd(`Mean(HU)_WholeLung`), 2), sep = ""),
#     "Volume (cc)" = paste(round(mean(`Volume(cc)_WholeLung`), 2), " ± ", round(sd(`Mean(HU)_WholeLung`), 2), sep = ""),
#     "Air trapping (%)" = paste(round(mean(`AdvAirTrap_Ovr_fAT(%)_WholeLung`, na.rm = TRUE), 2), " ± ", round(sd(`AdvAirTrap_Ovr_fAT(%)_WholeLung`, na.rm = TRUE), 2), sep = ""),
#     "ACQ6" = paste(round(mean(`ACQ6`, na.rm = TRUE), 2), " ± ", round(sd(`ACQ6`, na.rm = TRUE), 2), sep = ""),
#     "ACQ7" = paste(round(mean(`ACQ7`, na.rm = TRUE), 2), " ± ", round(sd(`ACQ7`, na.rm = TRUE), 2), sep = ""),
#     "AQLQ" = paste(round(mean(`AQLQ_TOTAL SCORE`, na.rm = TRUE), 2), " ± ", round(sd(`AQLQ_TOTAL SCORE`, na.rm = TRUE), 2), sep = ""),
#     "FEV1 (% pred)" = paste(round(mean(`Post BD FEV1 (% pred)`, na.rm = TRUE), 2), " ± ", round(sd(`Post BD FEV1 (% pred)`, na.rm = TRUE), 2), sep = ""),
#     "FVC (% pred)" = paste(round(mean(`Post BD FVC (% pred)`, na.rm = TRUE), 2), " ± ", round(sd(`Post BD FVC (% pred)`, na.rm = TRUE), 2), sep = ""),
#     "FEV1/FVC (% pred)" = paste(round(mean(`Post BD FEV1/FVC Ratio (%)`, na.rm = TRUE), 2), " ± ", round(sd(`Post BD FEV1/FVC Ratio (%)`, na.rm = TRUE), 2), sep = "")
#   )
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
# airtrap_cols = names(df)[grep("AdvAirTrap.+%", names(df))]
# airtrap_df = df %>% select(PatientID, site, timepoint, airtrap_cols) # %>%
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
# df = df %>%
#   select(PatientID, timepoint, site, DB_SeriesInstanceUid, DB_StudyDescription, contains("la_"))