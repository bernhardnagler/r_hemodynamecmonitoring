# labels_dictionary.R

apply_study_labels <- function(data) {
  require(labelled)
  
  data <- data %>%
    set_variable_labels(
      # Demographics
      Alter = "Age (years)",
      Geschlecht = "Sex",
      age = "Age (years)",
      sex = "Sex",
      height = "Height (cm)",
      weight = "Weight (kg)",
      bmi = "Body Mass Index (kg/m²)",
      bsa = "Body Surface Area (m²)",
      
      # Clinical Scores & Status
      sapsiii = "SAPS III Score",
      sofa_admission = "SOFA Score at Admission",
      icu_diagnosis_cat = "Primary ICU Diagnosis",
      shock_criteria_hypotension = "Shock Criteria: Hypotension",
      shock_criteria_lactate = "Shock Criteria: Lactate > 2 mmol/l",
      
      # ECMO Configuration
      cannula_drain_site = "Drainage Cannula Site",
      cannula_return_site = "Return Cannula Site",
      base_ecmo_flow = "Baseline ECMO Flow (L/min)",
      
      # Study Manuevers - PCA (Pulse Contour Analysis)
      b_pca_co = "Baseline PCA Cardiac Output (L/min)",
      plr_pca_co = "PLR PCA Cardiac Output (L/min)",
      change_plr_pca_sv_perc = "PLR-induced change in PCA SV (%)",
      
      # Study Maneuvers - TTE (Transthoracic Echocardiography)
      b_tte_vti = "Baseline TTE LVOT VTI (cm)",
      plr_tte_vti = "PLR TTE LVOT VTI (cm)",
      change_plr_vti_perc = "PLR-induced change in TTE VTI (%)"
    )
  
  return(data)
}