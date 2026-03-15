library(tidyverse)
library(readxl)
library(stringr)
# library(naniar)

file_path <- "C:/Users/Nagler/ownCloud/ecmo_hemodynamics/RDA/backup/12.03.2026_20-45-26_HemodynamECMOnitoring-VA_Export_anonymised.xlsx"

# 1. Get the names of all sheets in the Excel file
sheet_names <- excel_sheets(file_path)

# 2. Define the cleaning function for a single sheet
import_and_clean_sheet <- function(sheet) {
  # Read the specific sheet
  df <- read_excel(path = file_path, sheet = sheet)
  
  # Clean the column names (strip the EDC suffixes like "-12418-4")
  df <- df %>%
    rename_with(~ str_remove_all(., "-\\d+-\\d+$")) %>% 
    rename_with(~ str_replace_all(., "-", "_")) # Replace any remaining hyphens with underscores
  
  # Add a column to track which sheet the data came from
  df <- df %>% mutate(source_sheet = sheet)
  
  return(df)
}

# 3. Apply the function to all sheets
# This creates a "list" of dataframes, where each dataframe is a cleaned sheet
all_sheets_list <- map(sheet_names, import_and_clean_sheet)

# Name the elements in the list so you can easily call them by sheet name
names(all_sheets_list) <- sheet_names

# Extract your specific timepoint sheets into their own dataframes
tp01 <- all_sheets_list[["Timepoint_01"]]
tp02 <- all_sheets_list[["Timepoint_02"]]

# Merge them into a single "long" format dataframe for analysis
# bind_rows handles dataframes with different numbers of columns by filling missing ones with NA
df_long <- bind_rows(tp01, tp02)


#### ADD Static Baseline Data to df_long:

# Define the administrative columns you want to banish entirely
cols_to_remove <- c("Dokument_ID", "Zentrum/Klinik/Abteilung_ID", "Zentrum/Klinik/Abteilung")

# 1. Clean up the longitudinal dataset (remove the admin columns if they exist)
df_long_clean <- df_long %>%
  select(-any_of(cols_to_remove))

# 2. Clean up the static Patient dataset 
# (Drop the admin columns AND source_sheet so it doesn't duplicate)
patient_data_clean <- all_sheets_list[["Patient"]] %>%
  rename(Patient_ID = ID) %>%
  select(-any_of(c(cols_to_remove, "source_sheet")))

# 3. Clean up the static Baseline dataset
# (Drop the admin columns AND source_sheet)
baseline_data_clean <- all_sheets_list[["ECMO_baseline"]] %>%
  select(-any_of(c(cols_to_remove, "source_sheet"))) %>%
  mutate(across(
    .cols = c("weight", "height"),
    .fns = ~ as.numeric(str_replace_all(as.character(.), ",", "."))
    ))

# 4. Define your common demographic columns again
common_cols <- c("Patient_ID", "Geschlecht", "Geburtsdatum", "Sterbedatum", "Alter")

# 5. Execute the clean merge!
df_merged <- df_long_clean %>%
  left_join(patient_data_clean, by = common_cols) %>%
  left_join(baseline_data_clean, by = common_cols) %>%
  relocate(any_of(common_cols), .after = Patient_ID) %>%
  relocate(source_sheet, .after = last_col())

# Check your final merged dataframe
# glimpse(df_merged)



#### CLEANUP:

# Step 3: Automated Unit Standardization & Data Integrity Checks
df_cleaned <- df_merged %>%
  mutate(
    across(
      .cols = c(contains("lvotdiam"), contains("sv"), contains("co"), contains("etco2"), contains("vti"), contains("hr"), contains("tte_tr")),
      .fns = ~ {
        val <- as.character(.x)
        val <- if_else(str_detect(val, "[A-Za-z]"), NA_character_, val)
        as.numeric(str_replace_all(val, ",", "."))
        }
    )
  ) %>%
 

   mutate(
    # -----------------------------------------------------------------
    # 1. LVOT Diameter Correction (mm to cm)
    # -----------------------------------------------------------------
    # Normal LVOT diameter is ~1.8 to 2.5 cm. 
    # If a user entered > 10, they likely inputted millimeters (e.g., 20 instead of 2.0).
    across(
      .cols = contains("lvotdiam"), 
      .fns = ~ case_when(
        . > 10 ~ . / 10,  # Convert mm to cm by dividing by 10
        TRUE ~ .          # Keep correct cm values as they are
      )
    ),
    
    # -----------------------------------------------------------------
    # 2. Stroke Volume (SV) Correction (Liters to mL)
    # -----------------------------------------------------------------
    # SV is typically 40-100 mL. If a value is < 5 (e.g., 0.06), it was likely entered in Liters.
    # We explicitly exclude "svv" (Stroke Volume Variation) and "svr" (Systemic Vascular Resistance).
    across(
      .cols = contains("sv") & !contains("svv") & !contains("svr"), 
      .fns = ~ case_when(
        . < 5  ~ . * 1000, # Convert L to mL by multiplying by 1000
        TRUE ~ .
      )
    ),
    
    # -----------------------------------------------------------------
    # 3. Cardiac Output (CO) Correction (mL/min to L/min)
    # -----------------------------------------------------------------
    # CO is typically 3-8 L/min. If a value is > 100 (e.g., 5000), it was likely entered in mL/min.
    # We must exclude etCO2 and blood gas CO2 variables from this check!
    across(
      .cols = contains("co") & !contains("etco2") & !contains("pco2") & !contains("hco3"), 
      .fns = ~ case_when(
        . > 100 ~ . / 1000, # Convert mL/min to L/min
        TRUE ~ .
      )
    ),
    
    ### additional correction: convert to numeric values:
    across(
      .cols = c(
        contains("eiot_co"),   # Catches all EIOT before/after for baseline and fluid
        contains("eeot_co"),   # Catches all EEOT before/after for baseline and fluid
        contains("vti"),       # Catches baseline, PLR, RTB, and fluid TTE VTI
        contains("pca_sv"),    # Catches baseline, PLR, RTB, and fluid PCA SV
        contains("etco2"),     # Catches baseline, PLR, RTB, and fluid etCO2
        contains("ppv")        # Catches baseline, PLR, RTB, and fluid PPV
      ),
      .fns = ~ as.numeric(str_replace_all(as.character(.), ",", "."))
    ),
    
    # -----------------------------------------------------------------
    # 4. etCO2 Correction (kPa or percentage to mmHg)
    # -----------------------------------------------------------------
    # etCO2 is typically monitored in mmHg (normal ~ 35-45). 
    # If the value is very low (e.g., < 10), the user might have entered kPa or %.
    # Note: 1 kPa = 7.5 mmHg. (Adjust this if your monitors use standard %).
    across(
      .cols = contains("etco2"),
      .fns = ~ case_when(
        . < 10 ~ . * 7.5, # Approximate conversion from kPa to mmHg
        TRUE ~ .
      )
    )
  )

# -----------------------------------------------------------------
# Step 4: Out-of-bounds Flagging (Sanity Check)
# -----------------------------------------------------------------
# After standardizing units, let's filter for rows that STILL have impossible physiological 
# values, which might indicate severe typos (e.g., HR of 800 instead of 80).
suspicious_entries <- df_cleaned %>%
  filter(
    if_any(contains("lvotdiam"), ~ . < 1.0 | . > 3.5) |    # LVOT should be ~1-3.5 cm
      if_any(contains("vti"), ~ . < 2 | . > 50) |            # VTI should be ~10-25 cm
      if_any(contains("hr"), ~ . < 20 | . > 250)             # HR should be ~20-250 bpm
  ) %>%
  select(Patient_ID, source_sheet, contains("lvotdiam"), contains("vti"), contains("hr"))

# Print a warning if any suspicious entries are found
if (nrow(suspicious_entries) > 0) {
  warning(paste(nrow(suspicious_entries), "rows have potentially extreme typos. Please review 'suspicious_entries'."))
  print(head(suspicious_entries))
} else {
  print("All targeted numeric variables appear to be within safe physiological bounds after cleaning!")
}

#### Boxplots:
# Example: Visualize the distribution of ALL Heart Rate measurements
df_cleaned %>%
  select(Patient_ID, contains("hr")) %>%
  # Reshape data for plotting
  pivot_longer(
    cols = contains("hr"), 
    names_to = "maneuver", 
    values_to = "heart_rate"
  ) %>%
  drop_na(heart_rate) %>%
  
  # Create the boxplot
  ggplot(aes(x = maneuver, y = heart_rate)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "darkred") + # Show individual data points
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Distribution of Heart Rate Across All Maneuvers",
    x = "Maneuver / Measurement Phase",
    y = "Heart Rate (bpm)"
  )


# Boxplot all VTI measurements Timepoint_01
df_cleaned %>%
  filter(source_sheet == "Timepoint_01") %>%
  select(Patient_ID, contains("vti")) %>%
  # Reshape data for plotting
  pivot_longer(
    cols = contains("vti"), 
    names_to = "maneuver", 
    values_to = "vti"
  ) %>%
  drop_na(vti) %>%
  
  # Create the boxplot
  ggplot(aes(x = maneuver, y = vti)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "darkred") + # Show individual data points
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Distribution of LVOT VTI Across All Maneuvers",
    x = "Maneuver / Measurement Phase",
    y = "LVOT VTI (cm)"
  )

# Boxplot all PCA CO measurements Timepoint_01
df_cleaned %>%
  filter(source_sheet == "Timepoint_01") %>%
  select(Patient_ID, contains("pca_co")) %>%
  # Reshape data for plotting
  pivot_longer(
    cols = contains("pca_co"), 
    names_to = "maneuver", 
    values_to = "pca_co"
  ) %>%
  drop_na(pca_co) %>%
  
  # Create the boxplot
  ggplot(aes(x = maneuver, y = pca_co)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "darkred") + # Show individual data points
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Distribution of Pulse Contour Analysis CO Across All Maneuvers",
    x = "Maneuver / Measurement Phase",
    y = "CO (L/min)"
  )

# Boxplot all etCo2 measurements Timepoint_01
df_cleaned %>%
  filter(source_sheet == "Timepoint_01") %>%
  select(Patient_ID, contains("etco2")) %>%
  # Reshape data for plotting
  pivot_longer(
    cols = contains("etco2"), 
    names_to = "maneuver", 
    values_to = "etco2"
  ) %>%
  drop_na(etco2) %>%
  
  # Create the boxplot
  ggplot(aes(x = maneuver, y = etco2)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "darkred") + # Show individual data points
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Distribution of EtCO2 Across All Maneuvers",
    x = "Maneuver / Measurement Phase",
    y = "EtCO2 (mmHg)"
  )


## MISSING VALUES ASSESSMENT

# # Visualize missingness for TTE measurements specifically
# df_cleaned %>%
#   select(contains("tte")) %>%
#   vis_miss(sort_miss = TRUE) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(title = "Missing Data Pattern in Echocardiography Variables")



#### CALCULATE DERIVED VARIABLES (BSA, CO, SV, CI)
# Calculate derived hemodynamic variables across all ECMO flow and positioning maneuvers
df_calculated <- df_cleaned %>%
  mutate(
    # -----------------------------------------------------------------
    # 0. Calculate Body Surface Area (BSA)
    # -----------------------------------------------------------------
    bsa = 0.007184 * (weight ^ 0.425) * (height ^ 0.725),
    
    # -----------------------------------------------------------------
    # 1. BASELINE 1
    # -----------------------------------------------------------------
    b_tte_sv = (pi * (b_tte_lvotdiam / 2)^2) * b_tte_vti,
    b_tte_co = (b_tte_sv * b_hr) / 1000,
    b_tte_ci = b_tte_co / bsa,

    # -----------------------------------------------------------------
    # 2. BLOOD FLOW INCREASE (+1 L/min)
    # -----------------------------------------------------------------
    flow_plus1_lvotdiam_used = coalesce(flow_plus1_tte_lvotdiam, b_tte_lvotdiam),
    flow_plus1_tte_sv = (pi * (flow_plus1_lvotdiam_used / 2)^2) * flow_plus1_tte_vti,
    flow_plus1_tte_co = (flow_plus1_tte_sv * flow_plus1_hr) / 1000,
    flow_plus1_tte_ci = flow_plus1_tte_co / bsa,

    # -----------------------------------------------------------------
    # 3. BASELINE BLOOD FLOW 2
    # -----------------------------------------------------------------
    flow_baseline2_lvotdiam_used = coalesce(flow_baseline2_tte_lvotdiam, b_tte_lvotdiam),
    flow_baseline2_tte_sv = (pi * (flow_baseline2_lvotdiam_used / 2)^2) * flow_baseline2_tte_vti,
    flow_baseline2_tte_co = (flow_baseline2_tte_sv * flow_baseline2_hr) / 1000,
    flow_baseline2_tte_ci = flow_baseline2_tte_co / bsa,

    # -----------------------------------------------------------------
    # 4. BLOOD FLOW DECREASE (-1 L/min)
    # -----------------------------------------------------------------
    flow_minus1_lvotdiam_used = coalesce(flow_minus1_tte_lvotdiam, b_tte_lvotdiam),
    flow_minus1_tte_sv = (pi * (flow_minus1_lvotdiam_used / 2)^2) * flow_minus1_tte_vti,
    flow_minus1_tte_co = (flow_minus1_tte_sv * flow_minus1_hr) / 1000,
    flow_minus1_tte_ci = flow_minus1_tte_co / bsa,

    # -----------------------------------------------------------------
    # 5. BASELINE BLOOD FLOW 3
    # -----------------------------------------------------------------
    flow_baseline3_lvotdiam_used = coalesce(flow_baseline3_tte_lvotdiam, b_tte_lvotdiam),
    flow_baseline3_tte_sv = (pi * (flow_baseline3_lvotdiam_used / 2)^2) * flow_baseline3_tte_vti,
    flow_baseline3_tte_co = (flow_baseline3_tte_sv * flow_baseline3_hr) / 1000,
    flow_baseline3_tte_ci = flow_baseline3_tte_co / bsa,

    # -----------------------------------------------------------------
    # 6. PASSIVE LEG RAISE (PLR)
    # -----------------------------------------------------------------
    plr_lvotdiam_used = coalesce(plr_tte_lvotdiam, b_tte_lvotdiam),
    plr_tte_sv = (pi * (plr_lvotdiam_used / 2)^2) * plr_tte_vti,
    plr_tte_co = (plr_tte_sv * plr_hr) / 1000,
    plr_tte_ci = plr_tte_co / bsa,

    # -----------------------------------------------------------------
    # 7. RETURN TO BASELINE (RTB)
    # -----------------------------------------------------------------
    rtb_lvotdiam_used = coalesce(rtb_tte_lvotdiam, b_tte_lvotdiam),
    rtb_tte_sv = (pi * (rtb_lvotdiam_used / 2)^2) * rtb_tte_vti,
    rtb_tte_co = (rtb_tte_sv * rtb_hr) / 1000,
    rtb_tte_ci = rtb_tte_co / bsa,

    # -----------------------------------------------------------------
    # 8. FLUID BOLUS
    # -----------------------------------------------------------------
    fluid_lvotdiam_used = coalesce(fluid_tte_lvotdiam, b_tte_lvotdiam),
    
    fluid_tte_sv = (pi * (fluid_lvotdiam_used / 2)^2) * fluid_tte_vti,
    fluid_tte_co = (fluid_tte_sv * fluid_hr) / 1000,
    fluid_tte_ci = fluid_tte_co / bsa,
  ) %>%
  # Clean up the intermediate calculation columns to keep the dataframe tidy
  select(-ends_with("_calc"), -ends_with("_used"))

# Calculate and cross-fill derived hemodynamic variables for Pulse Contour Analysis (PCA)
df_calculated_pca <- df_calculated %>% # Assuming you are chaining this after the TTE calculations
  mutate(
    # -----------------------------------------------------------------
    # 1. BASELINE 1
    # -----------------------------------------------------------------
    # Cross-calculate CO and SV depending on which one was entered
    b_pca_co_calc = (b_pca_sv * b_hr) / 1000,
    b_pca_co = coalesce(b_pca_co, b_pca_co_calc),
    b_pca_sv_calc = (b_pca_co * 1000) / b_hr,
    b_pca_sv = coalesce(b_pca_sv, b_pca_sv_calc),
    # Calculate Cardiac Index (CI)
    b_pca_ci = b_pca_co / bsa,

    # -----------------------------------------------------------------
    # 2. BLOOD FLOW INCREASE (+1 L/min)
    # -----------------------------------------------------------------
    flow_plus1_pca_co_calc = (flow_plus1_pca_sv * flow_plus1_hr) / 1000,
    flow_plus1_pca_co = coalesce(flow_plus1_pca_co, flow_plus1_pca_co_calc),
    flow_plus1_pca_sv_calc = (flow_plus1_pca_co * 1000) / flow_plus1_hr,
    flow_plus1_pca_sv = coalesce(flow_plus1_pca_sv, flow_plus1_pca_sv_calc),
    flow_plus1_pca_ci_ = flow_plus1_pca_co / bsa,

    # -----------------------------------------------------------------
    # 3. BASELINE BLOOD FLOW 2
    # -----------------------------------------------------------------
    flow_baseline2_pca_co_calc = (flow_baseline2_pca_sv * flow_baseline2_hr) / 1000,
    flow_baseline2_pca_co = coalesce(flow_baseline2_pca_co, flow_baseline2_pca_co_calc),
    flow_baseline2_pca_sv_calc = (flow_baseline2_pca_co * 1000) / flow_baseline2_hr,
    flow_baseline2_pca_sv = coalesce(flow_baseline2_pca_sv, flow_baseline2_pca_sv_calc),
    flow_baseline2_pca_ci = flow_baseline2_pca_co / bsa,

    # -----------------------------------------------------------------
    # 4. BLOOD FLOW DECREASE (-1 L/min)
    # -----------------------------------------------------------------
    flow_minus1_pca_co_calc = (flow_minus1_pca_sv * flow_minus1_hr) / 1000,
    flow_minus1_pca_co = coalesce(flow_minus1_pca_co, flow_minus1_pca_co_calc),
    flow_minus1_pca_sv_calc = (flow_minus1_pca_co * 1000) / flow_minus1_hr,
    flow_minus1_pca_sv = coalesce(flow_minus1_pca_sv, flow_minus1_pca_sv_calc),
    flow_minus1_pca_ci = flow_minus1_pca_co / bsa,

    # -----------------------------------------------------------------
    # 5. BASELINE BLOOD FLOW 3
    # -----------------------------------------------------------------
    flow_baseline3_pca_co_calc = (flow_baseline3_pca_sv * flow_baseline3_hr) / 1000,
    flow_baseline3_pca_co = coalesce(flow_baseline3_pca_co, flow_baseline3_pca_co_calc),
    flow_baseline3_pca_sv_calc = (flow_baseline3_pca_co * 1000) / flow_baseline3_hr,
    flow_baseline3_pca_sv = coalesce(flow_baseline3_pca_sv, flow_baseline3_pca_sv_calc),
    flow_baseline3_pca_ci = flow_baseline3_pca_co / bsa,

    # -----------------------------------------------------------------
    # 6. PASSIVE LEG RAISE (PLR)
    # -----------------------------------------------------------------
    plr_pca_co_calc = (plr_pca_sv * plr_hr) / 1000,
    plr_pca_co = coalesce(plr_pca_co, plr_pca_co_calc),
    plr_pca_sv_calc = (plr_pca_co * 1000) / plr_hr,
    plr_pca_sv = coalesce(plr_pca_sv, plr_pca_sv_calc),
    plr_pca_ci = plr_pca_co / bsa,

    # -----------------------------------------------------------------
    # 7. RETURN TO BASELINE (RTB)
    # -----------------------------------------------------------------
    rtb_pca_co_calc = (rtb_pca_sv * rtb_hr) / 1000,
    rtb_pca_co = coalesce(rtb_pca_co, rtb_pca_co_calc),
    rtb_pca_sv_calc = (rtb_pca_co * 1000) / rtb_hr,
    rtb_pca_sv = coalesce(rtb_pca_sv, rtb_pca_sv_calc),
    rtb_pca_ci = rtb_pca_co / bsa,

    # -----------------------------------------------------------------
    # 8. FLUID BOLUS
    # -----------------------------------------------------------------
    fluid_pca_co_calc = (fluid_pca_sv * fluid_hr) / 1000,
    fluid_pca_co = coalesce(fluid_pca_co, fluid_pca_co_calc),
    fluid_pca_sv_calc = (fluid_pca_co * 1000) / fluid_hr,
    fluid_pca_sv = coalesce(fluid_pca_sv, fluid_pca_sv_calc),
    fluid_pca_ci = fluid_pca_co / bsa,
  ) %>%
  # Clean up the intermediate calculation columns
  select(-ends_with("_calc"))


##### Fluid Responsiveness Tests
df_frresults <- df_calculated_pca %>%
  mutate(
    # EIOT / EEOT at Baseline
    change_b_eiot_pca_perc = b_eiot_co_after/b_eiot_co_before,
    change_b_eiot_pca_abs = b_eiot_co_after - b_eiot_co_before,
    responsive_b_eiot_pca = change_b_eiot_pca_perc <= 0.95,
    change_b_eeot_pca_perc = b_eeot_co_after/b_eeot_co_before,
    change_b_eeot_pca_abs = b_eeot_co_after - b_eeot_co_before,
    responsive_b_eeot_pca = change_b_eeot_pca_perc >= 1.05,

    # EIOT / EEOT after Fluid
    change_fluid_eiot_pca_perc = fluid_eiot_co_after/fluid_eiot_co_before,
    change_fluid_eiot_pca_abs = fluid_eiot_co_after - fluid_eiot_co_before,
    responsive_fluid_eiot_pca = change_fluid_eiot_pca_perc <= 0.95,
    change_fluid_eeot_pca_perc = fluid_eeot_co_after/fluid_eeot_co_before,
    change_fluid_eeot_pca_abs = fluid_eeot_co_after - fluid_eeot_co_before,
    responsive_fluid_eeot_pca = change_fluid_eeot_pca_perc >= 1.05,
    
    # PLR
    plr_baseline_vti = coalesce(flow_baseline3_tte_vti, b_tte_vti),
    plr_baseline_pca_sv = coalesce(flow_baseline3_pca_sv, b_pca_sv),
    change_plr_vti_perc = plr_tte_vti/plr_baseline_vti,
    change_plr_vti_abs = plr_tte_vti - plr_baseline_vti,
    responsive_plr_vti = change_plr_vti_perc >= 1.1,
    change_plr_pca_sv_perc = plr_pca_sv/plr_baseline_pca_sv,
    change_plr_pca_sv_abs = plr_pca_sv - plr_baseline_pca_sv,
    responsive_pca_sv = change_plr_pca_sv_perc >= 1.1,
    plr_baseline_etco2 = coalesce(flow_baseline3_etco2, b_etco2),
    plr_baseline_ppv = coalesce(flow_baseline3_pca_ppv, b_pca_ppv),
    change_plr_etco2_perc = plr_etco2/plr_baseline_etco2,
    change_plr_etco2_abs = plr_etco2 - plr_baseline_etco2,
    responsive_plr_etco2_abs = change_plr_etco2_abs >= 2,
    change_plr_ppv_perc = plr_pca_ppv/plr_baseline_ppv,
    change_plr_ppv_abs = plr_pca_ppv - plr_baseline_ppv,
    responsive_plr_ppv_abs = change_plr_ppv_abs <=-2,
  
    # FLUID BOLUS
    fluid_baseline_vti = coalesce(rtb_tte_vti, b_tte_vti),
    fluid_baseline_pca_sv = coalesce(rtb_pca_sv, b_pca_sv),
    fluid_baseline_etco2 = coalesce(rtb_etco2, b_etco2),
    fluid_baseline_ppv = coalesce(rtb_pca_ppv, b_pca_ppv),
    change_fluid_vti_perc = fluid_tte_vti/fluid_baseline_vti,
    change_fluid_vti_abs = fluid_tte_vti - fluid_baseline_vti,
    responsive_fluid_vti = change_fluid_vti_perc >= 1.1,
    change_fluid_pca_sv_perc = fluid_pca_sv/fluid_baseline_pca_sv,
    change_fluid_pca_sv_abs = fluid_pca_sv - fluid_baseline_pca_sv,
    responsive_fluid_pca_sv = change_fluid_pca_sv_perc >= 1.1,
    change_fluid_etco2_perc = fluid_etco2/fluid_baseline_etco2,
    change_fluid_etco2_abs = fluid_etco2 - fluid_baseline_etco2, 
    responsive_fluid_etco2_abs = change_fluid_etco2_abs >= 2,
    change_fluid_ppv_perc = fluid_pca_ppv/fluid_baseline_ppv,
    change_fluid_ppv_abs = fluid_pca_ppv - fluid_baseline_ppv,
    responsive_fluid_ppv_abs = change_fluid_ppv_abs <= -2,
    rdaid = Patient_ID
  ) %>%
  relocate(rdaid, source_sheet)

# quick check:
# df_frresults %>% select(responsive_plr_vti, responsive_fluid_vti) %>% view()

####### FURTHER DATA CLEANING:
df_ready <- df_frresults %>%
  mutate(across(.cols = c(Alter, height, weight, bmi, ibw, sapsiii, sofa_admission), .fns = as.numeric)) %>%
  mutate(across(where(is.numeric), ~ if_else(is.finite(.), ., NA_real_)))
