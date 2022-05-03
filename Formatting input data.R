## NCD-RisC
## v1 (20210514)
## Formatting input data for cleaning and summarising

library(foreign)  # also check other packages: haven, sas7bdat, readstata13, readxl

### READ DATA ###
data <- read.dta("raw_data.dta")    # other formats: read.csv, read.spss, read.dta13, read.sas7bdat, read_xpt, read_excel
names(data) <- tolower(names(data)) # convert all variable names to lower case

### FORMATTING ###
# if a risk factor was not available, remove the related variables or set their values to NA
# e.g., if glucose was not available, remove rows with ...glu... variables, or make their values NA
# e.g., if cholesterol was available but ldl and triglycerides were not, remove ldl/unit_ldl/trg/unit_trg, or make their values NA, but keep others
d.info <- data.frame(
            id_study = "",          # use a unique string as study ID, we recommend 'ISO_mid-year_acronym', e.g., USA_2000_NHANES
            urban_rural = "both",   # urban/rural/both
            
            # age ranges for different risk factors and by gender (F = female, M = male)
            age_min_anthro_F = 5, age_max_anthro_F = 200,    # use 200 if no upper limit in age range
            age_min_anthro_M = 5, age_max_anthro_M = 200,
            age_min_chol_F   = 5, age_max_chol_F   = 200,
            age_min_chol_M   = 5, age_max_chol_M   = 200,
            age_min_glu_F    = 5, age_max_glu_F    = 200,
            age_min_glu_M    = 5, age_max_glu_M    = 200,
            age_min_bp_F     = 5, age_max_bp_F     = 200,
            age_min_bp_M     = 5, age_max_bp_M     = 200)

attach(data)
d <- data.frame(id = unique_id,
				age = age, sex = sex,
				
				psu = psu, stratum = strata,
				samplewt_anthro = sample_weight, samplewt_bp = sample_weight,     # use different sample weights for different variables if available
				samplewt_chol = sample_weight, samplewt_glu = sample_weight,
				is_urban = urban_or_rural_resident,
				
				# please make sure complete the unit according to the data
				
				# For these data, please make sure units are in cm or kg, NOT inch or pound
				height = height, unit_height = "cm",  # height in metres/inches should be converted to cm
                weight = weight, unit_weight = "kg",  # weight in pounds should be converted to kg
                waist = waist,   unit_waist = "cm",   # waist circumference in inches should be converted to cm
				hip = hip,       unit_hip = "cm",     # hip circumference in inches should be converted to cm
				
				# For blood measures, units can be mmol/L or mg/dL, but not any other unit
				# Please complete the empty string as appropriate
				# For HbA1c, please make sure units are in % (NGSP)
				fgl = fasting_glucose, unit_gl = "",  # unit can be either "mmol/L" or "mg/dL"
				is_fasting = fasted,                  # if fasting time is available, consider 6-24 hour fasting as yes, otherwise no (<6 or >24 hour)
				is_plasma = NA,                       # if fasting glucose is reported as whole-blood based values using a portable device
				                                      # please code as is_plasma = 0, otherwise code as is_plasma = 1 (either measured in a lab
				                                      # or the device has already converted to plasma equivalent values)
				ppg = ogtt,    unit_ppg = "",         # unit can be either "mmol/L" or "mg/dL"
				hba1c = hba1c, unit_hba1c = "%",      # please convert to NGSP if data are in IFCC (mmol/mol), using NGSP = 0.0915*IFCC + 2.15
				self_diab = history_of_dm,
				drug_diab = treated_for_dm,           # please consider either taking insulin or oral agent as being treated
				
				tc = total_chol,   unit_tc = "",      # unit can be either "mmol/L" or "mg/dL"
				hdl = hdl, unit_hdl = "",             # unit can be either "mmol/L" or "mg/dL"
				ldl = ldl, unit_ldl = "",             # unit can be either "mmol/L" or "mg/dL"
				trg = triglycerides, unit_trg = "",   # unit can be either "mmol/L" or "mg/dL"
				
				sbp1 = first_sbp, sbp2 = second_sbp, sbp3 = third_sbp,   # please provide individual BP measurements depending on how many were taken
				dbp1 = first_dbp, dbp2 = second_dbp, dbp3 = third_dbp,   # add more (sbp4/dbp4 etc) if needed
				sbp_avg = NA, dbp_avg = NA,           # averaged BP from individual measurements: only use if individual measurements are unavailable
				self_hyper = history_of_hypertension,
				drug_hyper = treated_for_hypertension)
detach(data)

### RECODE DATA ###
# please recode sex to 1 = male, 2 = female
# e.g. to recode sex = 'male'/'female', use:
# d$sex <- ifelse(d$sex == 'male', 1, ifelse(d$sex == 'female', 2, NA))

# please recode is_urban to 1 = urban, 0 = rural

# please code is_fasting as 1 = yes, 0 = no
# please recode self_xxx and drug_xxx variables to 1 = yes, 0 = no
# e.g. to recode original data that use 1/2 for yes/no, use:
# d$self_hyper[which(d$self_hyper == 2)] <- 0

# If some missing data is caused by structure in the questionnaire, e.g.
# Question B is not asked because of the answer of Question A
# Typically seen in medication question following a disease history question (for DM or hypertension)
# Please correct this missing pattern using: (hypertension for example)
# d$drug_hyper[which(is.na(d$drug_hyper) & d$self_hyper == 0)] <- 0

# Check the extracted data
# 1. Please pay special attention to the type of each variable
#    All measurement data should be numeric type, i.e. they cannot be characters or factors
#    to convert factor/character to numeric, use (do not omit 'as.character' function):
#    numeric_data <- as.numeric(as.character(character_or_factor_data))
# 2. Please make sure that any missing data are genuinely missing
#    Please contact us (ncdrisc@imperial.ac.uk) if FPG/OGTT(PPG) was not measured among people with known diabetes
summary(d)

### OUTPUT ###
d <- data.frame(d.info, d)
write.csv(d, "input_data.csv", row.names = FALSE)
