library(tidyverse)
library(readxl)
library(stringr)

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
  select(-any_of(c(cols_to_remove, "source_sheet")))

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