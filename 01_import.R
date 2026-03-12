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

# 1. Extract the static datasets from your list
# We rename 'ID' to 'Patient_ID' in the Patient sheet so the keys match perfectly
patient_data <- all_sheets_list[["Patient"]] %>%
  rename(Patient_ID = ID) 

baseline_data <- all_sheets_list[["ECMO_baseline"]]

# 2. Identify common demographic columns that might be duplicated
# Columns like 'Geschlecht', 'Geburtsdatum', 'Sterbedatum', 'Alter' 
# exist in almost all sheets. We will use them as additional join keys 
# so R merges them cleanly instead of creating .x and .y duplicates.
common_cols <- c("Patient_ID", "Geschlecht", "Geburtsdatum", "Sterbedatum", "Alter")

# 3. Merge the baseline data into your longitudinal df_long dataset
df_merged <- df_long %>%
  # Join Patient demographic data
  left_join(patient_data, by = common_cols) %>%
  
  # Join ECMO Baseline data 
  # (Assuming ECMO_baseline also shares the common demographic columns)
  left_join(baseline_data, by = common_cols)

# Optional: Relocate demographic and baseline columns to the front for easier viewing
df_merged <- df_merged %>%
  relocate(any_of(common_cols), .after = Patient_ID) %>%
  relocate(source_sheet, .after = last_col()) 

# Check your final merged dataframe
# glimpse(df_merged)