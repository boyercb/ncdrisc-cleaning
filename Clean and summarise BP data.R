## NCD-RisC
## v1 (20210514)
## cleaning and summarising script for BP

library(survey)
library(plyr)

date <- format(Sys.Date(),"%Y%m%d")
source("utilities/shared_functions.R")

################################################################################
############################# DATA #############################################
################################################################################

# read data
data <- read.csv("input_data.csv")

################################################################################
########################## DATA CLEANING #######################################
################################################################################

# set up log file
sink(paste0("log_BPCleaning_", date, ".txt"))
data$id_study <- as.factor(data$id_study)

# set up cleaning indicators
data$dropped <- ""

# 1. Clean age for 0-120, set the rest to missing  ==============================================
# 2. Clean sex for 1/2, set the rest to missing. Label the variable 1=male, 2=female
data$age[which(data$age>120|data$age<0)] <- NA
data$sex[which(data$sex!=1&data$sex!=2&!is.na(data$sex))] <- NA

# Mark Dropped
dropList <- which(is.na(data$sex)|is.na(data$age))
data$dropped[dropList] <- paste(data$dropped[dropList],"AgeSex")

# 3. Table the missing percentages for sex and age by study =====================================
clnList <- which(is.na(data$age))
age.na <- table(data[clnList,]$id_study)/table(data$id_study)*100
print("Percentage of NAs in Age (%)")
print(sort(round(age.na[which(age.na>0)],2), decreasing=TRUE))

clnList <- which(is.na(data$sex))
sex.na <- table(data[clnList,]$id_study)/table(data$id_study)*100
print("Percentage of NAs in Sex (%)")
print(sort(round(sex.na[which(sex.na>0)],2), decreasing=TRUE))

# Clean for ages outside the design range -------------------------------------------------------
# truncate age
data$age <- floor(data$age)
# Mark Dropped
clnList <-
  with(data, 
      which((sex==1 & ((is.na(age_max_bp_M)|age>age_max_bp_M)|(is.na(age_min_bp_M)|age<age_min_bp_M))) |
            (sex==2 & ((is.na(age_max_bp_F)|age>age_max_bp_F)|(is.na(age_min_bp_F)|age<age_min_bp_F))) ) )
if (length(clnList)>0) data$dropped[clnList] <- paste(data$dropped[clnList], "DesignAge")

# print out
age.out <- table(data[clnList,]$id_study)/table(data$id_study)*100
print("Percentage of Implausible Values (Outside Designed Range) in Age (%)")
print(sort(round(age.out[which(age.out>0)],2), decreasing=TRUE))

# 4. Clean data in bp ======================================================================
### a. sbp ------------------------------------------------------------------
max_sbp <- 270
min_sbp <- 70

## depending on how many BP measurements were done
for (v in c("sbp1", "sbp2", "sbp3")) {
  v_clean <- paste0(v,"_f")
  data[v_clean] <- data[v]
  clnList <- which(data[v_clean]>max_sbp | data[v_clean]<min_sbp)
  print_cleaned(v_clean)
  if (length(clnList)>0) data[clnList,v_clean] <- NA
}

### b.dbp ------------------------------------------------------------------
max_dbp <- 150
min_dbp <- 30

## depending on how many BP measurements were done
for (v in c("dbp1", "dbp2", "dbp3")) {
  v_clean <- paste0(v,"_f")
  data[v_clean] <- data[v]
  clnList <- which(data[v_clean]>max_dbp | data[v_clean]<min_dbp)
  print_cleaned(v_clean)
  if (length(clnList)>0) data[clnList,v_clean] <- NA
}

# Mark Dropped: no BP data (modify the list of variables as appropriate)
dropList <- which(is.na(data$sbp1_f) & is.na(data$sbp2_f) & is.na(data$sbp3_f) &
                  is.na(data$dbp1_f) & is.na(data$dbp2_f) & is.na(data$dbp3_f) )
data$dropped[dropList] <- paste(data$dropped[dropList], "NoData")

### g-j. antihypertensive drug use and history of hypertension --------------
clnList <- which(data$self_hyper!=0 & data$self_hyper!=1)
print_cleaned("self_hyper")
if (length(clnList)>0) data$self_hyper[clnList] <- NA

clnList <- which(data$drug_hyper!=0 & data$drug_hyper!=1)
print_cleaned("drug_hyper")
if (length(clnList)>0) data$drug_hyper[clnList] <- NA

### m.q. samplewt_bp, psu, stratum -----------------------------------------
data$samplewt_bp[which(data$samplewt_bp<0)] <- NA
data$psu[which(data$psu<0)] <- NA
data$stratum[which(data$stratum<0)] <- NA

### p. is_urban, is_pregnant  ----------------------------------------------
clnList <- which(data$is_urban!=0 & data$is_urban!=1)
print_cleaned("is_urban")
if (length(clnList)>0) data$is_urban[clnList] <- NA

## is_pregnant
clnList <- which(data$is_pregnant!=0 & data$is_pregnant!=1)
print_cleaned("is_pregnant")
if (length(clnList)>0) data$is_pregnant[clnList] <- NA
# clean for age/sex
clnList <- which(data$is_pregnant==1 & (data$sex==1|data$age>=50|data$age<10) )
print_cleaned("is_pregnant")
if (length(clnList)>0) data$is_pregnant[clnList] <- 0
print("Cleaned pregnant males and pregnant females aged over 50 or under 10")

# Mark Dropped: pregnant women (keep if pregnacy information is missing)
dropList <- which(data$is_pregnant==1)
data$dropped[dropList] <- paste(data$dropped[dropList], "Preg")

# 6. Please check patterns of missingness and agreement between self_hyper and drug_hyper ---------------
print("Please check patterns of missingness and agreement between self_hyper and drug_hyper")
print("Any substantial missing pattern or disagreement between self_hyper and drug_hyper should be checked and potentially corrected before proceeding")
print(table(data$self_hyper, data$drug_hyper, data$id_study, exclude = NULL))

# recode is_urban to 'urban'/'rural'/'NA'
data$is_urban[data$urban_rural %in% c('rural','urban')] <- NA
table(data$is_urban, data$urban_rural, exclude=NULL)
data$is_urban <- ifelse(data$is_urban==0, 'rural', ifelse(data$is_urban==1, 'urban', NA))

sink()

# Substitute empty string in indicator "dropped"
data$dropped[which(data$dropped=="")] <- " Keep"
data$dropped <- factor(data$dropped)
levels(data$dropped) <- substr(levels(data$dropped), 2, nchar(levels(data$dropped)) )


################################################################################
########################## CREATE SUMMARIES  ###################################
################################################################################

# switch for sets of metrics
CW <- TRUE
DIST <- TRUE
UHC <- TRUE
UHC_test <- TRUE
UHC_allcat <- TRUE

# Clean dataset ====
# 18+ only
data <- data[which(data$age>=18),]
data$age_min_bp_M[which(data$age_min_bp_M<18)] <- 18
data$age_min_bp_F[which(data$age_min_bp_F<18)] <- 18

# drop records cleaned during data cleaning
data <- data[which(data$dropped=="Keep"),]

# Categorize age groups ====
data$age_min  <- with(data, ifelse(sex==1, age_min_bp_M, age_min_bp_F))
data$age_max  <- with(data, ifelse(sex==1, age_max_bp_M, age_max_bp_F))
tmp <- make_age_groups(data$age,data$age_min, data$age_max)
data$age_group <- tmp$age_group
data$age_mean  <- tmp$age_mean


### Calculate average BP ###
# drop 1st available measurement and average the rest
# only use bp_avg when individual measurements are unavailable
calc_avg <- function(x){
  avg <- x[1]; x <- x[-1]
  pos <- which(!is.na(x))
  if (length(pos)==0) res <- avg else
    if (length(pos)==1) res <- x[pos] else
      if (length(pos)==2) res <- x[pos[2]] else
        res <- mean(x[pos[2:length(pos)]])
  return (res)
}

if (!"sbp_avg_f" %in% names(data)) data$sbp_avg_f <- NA
if (!"dbp_avg_f" %in% names(data)) data$dbp_avg_f <- NA

data$sbp_final <- apply(data[,c("sbp_avg_f","sbp1_f","sbp2_f","sbp3_f")],1,calc_avg)
data$dbp_final <- apply(data[,c("dbp_avg_f","dbp1_f","dbp2_f","dbp3_f")],1,calc_avg)

## Calculating indicators for different categories of BP ====
# 0. Diagnosis:
data$diagnosis <- data$self_hyper
# 0. New_med: cleaned drug_hyper
# if new_med=1 but diagnosis=0, recode diagnosis to 1
data$new_med <- data$drug_hyper
data$diagnosis[which(data$diagnosis==0 & data$new_med==1)] <- 1
# calculation
core_list <- c("BP_140","BP_140_90","HTN_140_90_med","HTN_160_95_med","ctrlHTN_140_90_med","HTN_160_100_med")
# 1. Raised BP defind as: SBP >= 140
data$BP_140 <- 0
data$BP_140[which(data$sbp_final>=140)] <- 1
data$BP_140[which(is.na(data$sbp_final))] <- NA
# 2. Raised BP defind as: SBP >= 140 OR DBP >= 90
data$BP_140_90 <- 0
data$BP_140_90[which(data$sbp_final>=140|data$dbp_final>=90)] <- 1
data$BP_140_90[which(is.na(data$sbp_final)|is.na(data$dbp_final))] <- NA
# 3. HTN defind as: SBP >= 140 OR DBP >= 90 OR medication
data$HTN_140_90_med <- 0
data$HTN_140_90_med[which(data$sbp_final>=140|data$dbp_final>=90|data$new_med==1)] <- 1
data$HTN_140_90_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
# 4. HTN defind as: SBP >= 160 OR DBP >= 95 OR medication
data$HTN_160_95_med <- 0
data$HTN_160_95_med[which(data$sbp_final>=160|data$dbp_final>=95|data$new_med==1)] <- 1
data$HTN_160_95_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
# 5. Controlled HTN defind as: medication AND SBP < 140 AND DBP < 90
data$ctrlHTN_140_90_med <- 0
data$ctrlHTN_140_90_med[which(data$sbp_final<140&data$dbp_final<90&data$new_med==1)] <- 1
data$ctrlHTN_140_90_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
# 15. New diagnosis for CW: 160_100_med
data$HTN_160_100_med <- 0
data$HTN_160_100_med[which(data$sbp_final>=160|data$dbp_final>=100|data$new_med==1)] <- 1
data$HTN_160_100_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA

CW_list <- c()
if (CW) {
  CW_list <- c("BP_160_90","BP_160_95","HTN_130_80_med","HTN_135_85_med","BP_160","BP_150_90","BP_130_85","HTN_130_85_med","HTN_140_and_90_med","dbp_95","BP_135_85")
  # 6. New for CW: 160_90
  data$BP_160_90 <- 0
  data$BP_160_90[which(data$sbp_final>=160|data$dbp_final>=90)] <- 1
  data$BP_160_90[which(is.na(data$sbp_final)|is.na(data$dbp_final))] <- NA
  # 7. New for CW: 160_95
  data$BP_160_95 <- 0
  data$BP_160_95[which(data$sbp_final>=160|data$dbp_final>=95)] <- 1
  data$BP_160_95[which(is.na(data$sbp_final)|is.na(data$dbp_final))] <- NA
  # 8. New for CW: 130_80_med
  data$HTN_130_80_med <- 0
  data$HTN_130_80_med[which(data$sbp_final>=130|data$dbp_final>=80|data$new_med==1)] <- 1
  data$HTN_130_80_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
  # 9. New for CW: 135_85_med
  data$HTN_135_85_med <- 0
  data$HTN_135_85_med[which(data$sbp_final>=135|data$dbp_final>=85|data$new_med==1)] <- 1
  data$HTN_135_85_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
  # 11. New for CW: 160
  data$BP_160 <- 0
  data$BP_160[which(data$sbp_final>=160)] <- 1
  data$BP_160[which(is.na(data$sbp_final))] <- NA
  # 12. New for CW: 150_90
  data$BP_150_90 <- 0
  data$BP_150_90[which(data$sbp_final>=150|data$dbp_final>=90)] <- 1
  data$BP_150_90[which(is.na(data$sbp_final)|is.na(data$dbp_final))] <- NA
  # 13. New for CW: 130_85
  data$BP_130_85 <- 0
  data$BP_130_85[which(data$sbp_final>=130|data$dbp_final>=85)] <- 1
  data$BP_130_85[which(is.na(data$sbp_final)|is.na(data$dbp_final))] <- NA
  # 14. New for CW: 130_85_med
  data$HTN_130_85_med <- 0
  data$HTN_130_85_med[which(data$sbp_final>=130|data$dbp_final>=85|data$new_med==1)] <- 1
  data$HTN_130_85_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
  # 16. HTN defind as: SBP >= 140 AND DBP >= 90 OR medication
  data$HTN_140_and_90_med <- 0
  data$HTN_140_and_90_med[which((data$sbp_final>=140&data$dbp_final>=90)|data$new_med==1)] <- 1
  data$HTN_140_and_90_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
  # 17. New for CW: dbp_95
  data$dbp_95 <- 0
  data$dbp_95[which(data$dbp_final>=95)] <- 1
  data$dbp_95[which(is.na(data$dbp_final))] <- NA
  # 18. New for CW: 135_85
  data$BP_135_85 <- 0
  data$BP_135_85[which(data$sbp_final>=135|data$dbp_final>=85)] <- 1
  data$BP_135_85[which(is.na(data$sbp_final)|is.na(data$dbp_final))] <- NA
  
}

UHC_list <- c()
if (UHC) {
  UHC_list <- 
    c("ratio_treat_over_diag","HTN_140_90_med_wdiag","HTN_160_100_med_wdiag",
      "ratio_diag_HTN_140_90_med_wdiag","ratio_diag_HTN_160_100_med_wdiag",
      "ratio_treat_HTN_140_90_med_wdiag","ratio_treat_HTN_160_100_med_wdiag",
      "ratio_ctrl_HTN_140_90_med_wdiag","ratio_ctrl_HTN_160_100_med_wdiag",
      "ratio_ctrl_130_80_HTN_140_90_med_wdiag","ratio_ctrl_135_85_HTN_140_90_med_wdiag",
      "ratio_130_80_over_not_HTN_140_90_med_wdiag","ratio_135_85_over_not_HTN_140_90_med_wdiag",
      "ratio_treat_DxHTN_140_90_med_wdiag","ratio_treat_DxHTN_160_100_med_wdiag",
      "ratio_ctrl_TxHTN_140_90_med_wdiag","ratio_ctrl_TxHTN_160_100_med_wdiag",
      "HTN_160_100_wmed_wdiag","ratio_diag_HTN_160_100_wmed_wdiag","ratio_treat_HTN_160_100_wmed_wdiag",
      "HTN_160_100_wmed","ratio_treat_HTN_160_100_wmed",
      "BP_160_100")
  # ----------------------------------
  # 26. Percentage of treated over diagnosed
  data$ratio_treat_over_diag <- NA
  data$ratio_treat_over_diag[which(data$diagnosis==1 & !is.na(data$new_med))] <- 0
  data$ratio_treat_over_diag[which(data$diagnosis==1 & data$new_med==1)] <- 1
  # ----------------------------------
  # 34. HTN defined as: SBP >= 140 OR DBP >= 90 OR medicaiton, AND diagnosis available
  data$HTN_140_90_med_wdiag <- data$HTN_140_90_med
  data$HTN_140_90_med_wdiag[is.na(data$diagnosis)] <- NA
  # 35. HTN defined as: SBP >= 160 OR DBP >= 100 OR medicaiton, AND diagnosis available
  data$HTN_160_100_med_wdiag <- data$HTN_160_100_med
  data$HTN_160_100_med_wdiag[is.na(data$diagnosis)] <- NA
  # ----------------------------------
  # 36. Percentage of diagnosed over HTN_140_90_med_wdiag
  data$ratio_diag_HTN_140_90_med_wdiag <- NA
  data$ratio_diag_HTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==1)] <- 0
  data$ratio_diag_HTN_140_90_med_wdiag[which(data$diagnosis==1 & (data$HTN_140_90_med_wdiag==1))] <- 1
  # 37. Percentage of diagnosed over HTN_160_100_med_wdiag
  data$ratio_diag_HTN_160_100_med_wdiag <- NA
  data$ratio_diag_HTN_160_100_med_wdiag[which(data$HTN_160_100_med_wdiag==1)] <- 0
  data$ratio_diag_HTN_160_100_med_wdiag[which(data$diagnosis==1 & (data$HTN_160_100_med_wdiag==1))] <- 1
  # ----------------------------------
  # 38. Percentage of treated over HTN_140_90_med_wdiag
  data$ratio_treat_HTN_140_90_med_wdiag <- NA
  data$ratio_treat_HTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==1)] <- 0
  data$ratio_treat_HTN_140_90_med_wdiag[which(data$new_med==1 & (data$HTN_140_90_med_wdiag==1))] <- 1
  # 39. Percentage of treated over HTN_160_100_med_wdiag
  data$ratio_treat_HTN_160_100_med_wdiag <- NA
  data$ratio_treat_HTN_160_100_med_wdiag[which(data$HTN_160_100_med_wdiag==1)] <- 0
  data$ratio_treat_HTN_160_100_med_wdiag[which(data$new_med==1 & (data$HTN_160_100_med_wdiag==1))] <- 1
  # ----------------------------------
  # 40. Percentage of controlled over HTN 140_90_med_wdiag
  data$ratio_ctrl_HTN_140_90_med_wdiag <- NA
  data$ratio_ctrl_HTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==1)] <- 0
  data$ratio_ctrl_HTN_140_90_med_wdiag[which(data$new_med==1 & (data$sbp_final<140 & data$dbp_final<90) & (data$HTN_140_90_med_wdiag==1))] <- 1
  # 41. Percentage of controlled over HTN 160_100_med_wdiag
  data$ratio_ctrl_HTN_160_100_med_wdiag <- NA
  data$ratio_ctrl_HTN_160_100_med_wdiag[which(data$HTN_160_100_med_wdiag==1)] <- 0
  data$ratio_ctrl_HTN_160_100_med_wdiag[which(data$new_med==1 & (data$sbp_final<160 & data$dbp_final<100) & (data$HTN_160_100_med_wdiag==1))] <- 1
  # ----------------------------------
  # 45. Percentage of controlled to 130/80 over HTN 140_90_med_wdiag
  data$ratio_ctrl_130_80_HTN_140_90_med_wdiag <- NA
  data$ratio_ctrl_130_80_HTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==1)] <- 0
  data$ratio_ctrl_130_80_HTN_140_90_med_wdiag[which(data$new_med==1 & (data$sbp_final<130 & data$dbp_final<80) & (data$HTN_140_90_med_wdiag==1))] <- 1
  # 46. Percentage of controlled to 135/85 over HTN 140_90_med_wdiag
  data$ratio_ctrl_135_85_HTN_140_90_med_wdiag <- NA
  data$ratio_ctrl_135_85_HTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==1)] <- 0
  data$ratio_ctrl_135_85_HTN_140_90_med_wdiag[which(data$new_med==1 & (data$sbp_final<135 & data$dbp_final<85) & (data$HTN_140_90_med_wdiag==1))] <- 1
  # ----------------------------------
  # 47. Prevalence of 130/80 and not on medication
  data$ratio_130_80_over_not_HTN_140_90_med_wdiag <- NA
  data$ratio_130_80_over_not_HTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==0)] <- 0
  data$ratio_130_80_over_not_HTN_140_90_med_wdiag[which(data$HTN_140_90_med==0 & (data$sbp_final<130 & data$dbp_final<80))] <- 1
  # 48. Percentage of 135/85 over all normal BP (140/90)
  data$ratio_135_85_over_not_HTN_140_90_med_wdiag <- NA
  data$ratio_135_85_over_not_HTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==0)] <- 0
  data$ratio_135_85_over_not_HTN_140_90_med_wdiag[which(data$HTN_140_90_med==0 & (data$sbp_final<135 & data$dbp_final<85))] <- 1
  # ----------------------------------
  # 49. Percentage of treated over diagnosed HTN (140/90)
  data$ratio_treat_DxHTN_140_90_med_wdiag <- NA
  data$ratio_treat_DxHTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==1 & data$diagnosis==1)] <- 0
  data$ratio_treat_DxHTN_140_90_med_wdiag[which(data$new_med==1 & data$HTN_140_90_med_wdiag==1 & data$diagnosis==1)] <- 1
  # 50. Percentage of treated over diagnosed HTN (160/100)
  data$ratio_treat_DxHTN_160_100_med_wdiag <- NA
  data$ratio_treat_DxHTN_160_100_med_wdiag[which(data$HTN_160_100_med_wdiag==1 & data$diagnosis==1)] <- 0
  data$ratio_treat_DxHTN_160_100_med_wdiag[which(data$new_med==1 & data$HTN_160_100_med_wdiag==1 & data$diagnosis==1)] <- 1
  # ----------------------------------
  # 51. Percentage of controlled over treated HTN (140/90)
  data$ratio_ctrl_TxHTN_140_90_med_wdiag <- NA
  data$ratio_ctrl_TxHTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==1 & data$new_med==1)] <- 0
  data$ratio_ctrl_TxHTN_140_90_med_wdiag[which(data$new_med==1 & (data$sbp_final<140 & data$dbp_final<90) & (data$HTN_140_90_med_wdiag==1))] <- 1
  # 52. Percentage of controlled over treated HTN (160/100)
  data$ratio_ctrl_TxHTN_160_100_med_wdiag <- NA
  data$ratio_ctrl_TxHTN_160_100_med_wdiag[which(data$HTN_160_100_med_wdiag==1 & data$new_med==1)] <- 0
  data$ratio_ctrl_TxHTN_160_100_med_wdiag[which(data$new_med==1 & (data$sbp_final<160 & data$dbp_final<100) & (data$HTN_160_100_med_wdiag==1))] <- 1
  # ----------------------------------
  # 53. HTN defined as: SBP >=160 OR DBP >=100, AND medication and diagnosis available
  data$HTN_160_100_wmed_wdiag <- 0
  data$HTN_160_100_wmed_wdiag[which(data$sbp_final>=160|data$dbp_final>=100)] <- 1
  data$HTN_160_100_wmed_wdiag[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$diagnosis)|is.na(data$new_med))] <- NA
  # 54. Percentage of diagnosed over HTN_160_100_wmed_wdiag
  data$ratio_diag_HTN_160_100_wmed_wdiag <- NA
  data$ratio_diag_HTN_160_100_wmed_wdiag[which(data$HTN_160_100_wmed_wdiag==1)] <- 0
  data$ratio_diag_HTN_160_100_wmed_wdiag[which(data$diagnosis==1 & (data$HTN_160_100_wmed_wdiag==1))] <- 1
  # 55. Percentage of treated over HTN_160_100_wmed_wdiag
  data$ratio_treat_HTN_160_100_wmed_wdiag <- NA
  data$ratio_treat_HTN_160_100_wmed_wdiag[which(data$HTN_160_100_wmed_wdiag==1)] <- 0
  data$ratio_treat_HTN_160_100_wmed_wdiag[which(data$new_med==1 & (data$HTN_160_100_wmed_wdiag==1))] <- 1
  # ----------------------------------
  # 56. HTN defined as: SBP >=160 OR DBP >=100, AND medication available
  data$HTN_160_100_wmed <- 0
  data$HTN_160_100_wmed[which(data$sbp_final>=160|data$dbp_final>=100)] <- 1
  data$HTN_160_100_wmed[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
  # 57. Percentage of treated over HTN_160_100_wmed
  data$ratio_treat_HTN_160_100_wmed <- NA
  data$ratio_treat_HTN_160_100_wmed[which(data$HTN_160_100_wmed==1)] <- 0
  data$ratio_treat_HTN_160_100_wmed[which(data$new_med==1 & (data$HTN_160_100_wmed==1))] <- 1
  # 58. 160_100
  data$BP_160_100 <- 0
  data$BP_160_100[which(data$sbp_final>=160|data$dbp_final>=100)] <- 1
  data$BP_160_100[which(is.na(data$sbp_final)|is.na(data$dbp_final))] <- NA
}

# Prevalence of BP in all categories
UHC_allcat_list <- c()
if (UHC_allcat){
  UHC_allcat_list <- 
    c("HTN_160_100_Tx_wdiag", "HTN_160_100_Dx_uTx", "HTN_160_100_uDx_wmed",
      "HTN_140_90_160_100_Tx_wdiag", "HTN_140_90_160_100_Dx_uTx", "HTN_140_90_160_100_uDx_wmed", "ctrlHTN_140_90_med_wdiag", "L_140_90_Dx_uTx","L_140_90_uDx_wmed",
      "HTN_135_85_140_90_Tx_wdiag", "HTN_135_85_140_90_Dx_uTx", "HTN_135_85_140_90_uDx_wmed", "ctrlHTN_135_85_med_wdiag", "L_135_85_Dx_uTx", "L_135_85_uDx_wmed",
      "HTN_130_80_140_90_Tx_wdiag", "HTN_130_80_140_90_Dx_uTx", "HTN_130_80_140_90_uDx_wmed", "ctrlHTN_130_80_med_wdiag", "L_130_80_Dx_uTx", "L_130_80_uDx_wmed")
  # ----------------------------------
  # 101. Prevalence of BP>=160/100 and treated (with diagnosis info available)
  data$HTN_160_100_Tx_wdiag <- 0
  data$HTN_160_100_Tx_wdiag[which((data$sbp_final>=160 | data$dbp_final>=100) & (data$new_med==1))] <- 1
  data$HTN_160_100_Tx_wdiag[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 102. Prevalence of BP>=160/100 and diagnosed but not treated
  data$HTN_160_100_Dx_uTx <- 0
  data$HTN_160_100_Dx_uTx[which((data$sbp_final>=160 | data$dbp_final>=100) & (data$diagnosis==1) & (data$new_med==0))] <- 1
  data$HTN_160_100_Dx_uTx[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 103. Prevalence of BP>=160/100 and undiagnosed (with treatment info available)
  data$HTN_160_100_uDx_wmed <- 0
  data$HTN_160_100_uDx_wmed[which((data$sbp_final>=160 | data$dbp_final>=100) & (data$diagnosis==0))] <- 1
  data$HTN_160_100_uDx_wmed[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # ----------------------------------
  # 104. Prevalence of BP in 140/90 to 160/100 and treated (with diagnosis info available)
  data$HTN_140_90_160_100_Tx_wdiag <- 0
  data$HTN_140_90_160_100_Tx_wdiag[which((data$sbp_final<160 & data$dbp_final<100) & (data$sbp_final>=140 | data$dbp_final>=90) & (data$new_med==1))] <- 1
  data$HTN_140_90_160_100_Tx_wdiag[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 105. Prevalence of BP in 140/90 to 160/100 and diagnosed but not treated
  data$HTN_140_90_160_100_Dx_uTx <- 0
  data$HTN_140_90_160_100_Dx_uTx[which((data$sbp_final<160 & data$dbp_final<100) & (data$sbp_final>=140 | data$dbp_final>=90) & (data$diagnosis==1) & (data$new_med==0))] <- 1
  data$HTN_140_90_160_100_Dx_uTx[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 106. Prevalence of BP in 140/90 to 160/100 and undiagnosed (with treatment info available)
  data$HTN_140_90_160_100_uDx_wmed <- 0
  data$HTN_140_90_160_100_uDx_wmed[which((data$sbp_final<160 & data$dbp_final<100) & (data$sbp_final>=140 | data$dbp_final>=90) & (data$diagnosis==0))] <- 1
  data$HTN_140_90_160_100_uDx_wmed[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 107. Prevalence of BP <140/90 and treated (with diagnosis info available)
  data$ctrlHTN_140_90_med_wdiag <- data$ctrlHTN_140_90_med
  data$ctrlHTN_140_90_med_wdiag[is.na(data$diagnosis)] <- NA
  # 108. Prevalence of BP <140/90, diagnosed but not treated
  data$L_140_90_Dx_uTx <- 0
  data$L_140_90_Dx_uTx[which(data$sbp_final<140 & data$dbp_final<90 & data$diagnosis==1 & data$new_med==0)] <- 1
  data$L_140_90_Dx_uTx[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 109. Prevalence of BP <140/90 and undiagnosed (with treatment info available)
  data$L_140_90_uDx_wmed <- 0
  data$L_140_90_uDx_wmed[which(data$sbp_final<140 & data$dbp_final<90 & data$diagnosis==0)] <- 1
  data$L_140_90_uDx_wmed[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # ----------------------------------
  # 110. Prevalence of BP in 135/85 to 140/90 and treated (with diagnosis info available)
  data$HTN_135_85_140_90_Tx_wdiag <- 0
  data$HTN_135_85_140_90_Tx_wdiag[which((data$sbp_final<140 & data$dbp_final<90) & (data$sbp_final>=135 | data$dbp_final>=85) & (data$new_med==1))] <- 1
  data$HTN_135_85_140_90_Tx_wdiag[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 111. Prevalence of BP in 135/85 to 140/90 and diagnosed but not treated
  data$HTN_135_85_140_90_Dx_uTx <- 0
  data$HTN_135_85_140_90_Dx_uTx[which((data$sbp_final<140 & data$dbp_final<90) & (data$sbp_final>=135 | data$dbp_final>=85) & (data$diagnosis==1) & (data$new_med==0))] <- 1
  data$HTN_135_85_140_90_Dx_uTx[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 112. Prevalence of BP in 135/85 to 140/90 and undiagnosed (with treatment info available)
  data$HTN_135_85_140_90_uDx_wmed <- 0
  data$HTN_135_85_140_90_uDx_wmed[which((data$sbp_final<140 & data$dbp_final<90) & (data$sbp_final>=135 | data$dbp_final>=85) & (data$diagnosis==0))] <- 1
  data$HTN_135_85_140_90_uDx_wmed[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 113. Prevalence of BP <135/85 and treated (with diagnosis info available)
  data$ctrlHTN_135_85_med_wdiag <- 0
  data$ctrlHTN_135_85_med_wdiag[which(data$sbp_final<135 & data$dbp_final<85 & data$new_med==1)] <- 1
  data$ctrlHTN_135_85_med_wdiag[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 114. Prevalence of BP <135/85, diagnosed but not treated
  data$L_135_85_Dx_uTx <- 0
  data$L_135_85_Dx_uTx[which(data$sbp_final<135 & data$dbp_final<85 & data$diagnosis==1 & data$new_med==0)] <- 1
  data$L_135_85_Dx_uTx[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 115. Prevalence of BP <135/85 and undiagnosed (with treatment info available)
  data$L_135_85_uDx_wmed <- 0
  data$L_135_85_uDx_wmed[which(data$sbp_final<135 & data$dbp_final<85 & data$diagnosis==0)] <- 1
  data$L_135_85_uDx_wmed[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # ----------------------------------
  # 116. Prevalence of BP in 130/80 to 140/90 and treated (with diagnosis info available)
  data$HTN_130_80_140_90_Tx_wdiag <- 0
  data$HTN_130_80_140_90_Tx_wdiag[which((data$sbp_final<140 & data$dbp_final<90) & (data$sbp_final>=130 | data$dbp_final>=80) & (data$new_med==1))] <- 1
  data$HTN_130_80_140_90_Tx_wdiag[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 117. Prevalence of BP in 130/80 to 140/90 and diagnosed but not treated
  data$HTN_130_80_140_90_Dx_uTx <- 0
  data$HTN_130_80_140_90_Dx_uTx[which((data$sbp_final<140 & data$dbp_final<90) & (data$sbp_final>=130 | data$dbp_final>=80) & (data$diagnosis==1) & (data$new_med==0))] <- 1
  data$HTN_130_80_140_90_Dx_uTx[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 118. Prevalence of BP in 130/80 to 140/90 and undiagnosed (with treatment info available)
  data$HTN_130_80_140_90_uDx_wmed <- 0
  data$HTN_130_80_140_90_uDx_wmed[which((data$sbp_final<140 & data$dbp_final<90) & (data$sbp_final>=130 | data$dbp_final>=80) & (data$diagnosis==0))] <- 1
  data$HTN_130_80_140_90_uDx_wmed[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 119. Prevalence of BP <130/80 and treated (with diagnosis info available)
  data$ctrlHTN_130_80_med_wdiag <- 0
  data$ctrlHTN_130_80_med_wdiag[which(data$sbp_final<130 & data$dbp_final<80 & data$new_med==1)] <- 1
  data$ctrlHTN_130_80_med_wdiag[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 120. Prevalence of BP <130/80, diagnosed but not treated
  data$L_130_80_Dx_uTx <- 0
  data$L_130_80_Dx_uTx[which(data$sbp_final<130 & data$dbp_final<80 & data$diagnosis==1 & data$new_med==0)] <- 1
  data$L_130_80_Dx_uTx[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
  # 121. Prevalence of BP <130/80 and undiagnosed
  data$L_130_80_uDx_wmed <- 0
  data$L_130_80_uDx_wmed[which(data$sbp_final<130 & data$dbp_final<80 & data$diagnosis==0)] <- 1
  data$L_130_80_uDx_wmed[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
}

UHC_test_list <- c()
if (UHC_test) {
  UHC_test_list <- c("ctrlHTN_160_100_med","ratio_diag_HTN_140_90_med","ratio_diag_HTN_160_100_med","ratio_treat_HTN_140_90_med","ratio_treat_HTN_160_100_med","ratio_ctrl_HTN_140_90_med","ratio_ctrl_HTN_160_100_med")
  # ---------------------------------
  # 19. Controlled HTN defind as: medication AND SBP < 160 AND DBP < 100
  data$ctrlHTN_160_100_med <- 0
  data$ctrlHTN_160_100_med[which(data$sbp_final<160&data$dbp_final<100&data$new_med==1)] <- 1
  data$ctrlHTN_160_100_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
  # ----------------------------------
  # 22. Percentage of diagnosed over HTN 140_90_med
  data$ratio_diag_HTN_140_90_med <- NA
  data$ratio_diag_HTN_140_90_med[which(data$HTN_140_90_med==1 & !is.na(data$diagnosis))] <- 0
  data$ratio_diag_HTN_140_90_med[which(data$diagnosis==1 & data$HTN_140_90_med==1)] <- 1
  # 23. Percentage of diagnosed over HTN 160_100_med
  data$ratio_diag_HTN_160_100_med <- NA
  data$ratio_diag_HTN_160_100_med[which(data$HTN_160_100_med==1 & !is.na(data$diagnosis))] <- 0
  data$ratio_diag_HTN_160_100_med[which(data$diagnosis==1 & data$HTN_160_100_med==1)] <- 1
  # ----------------------------------
  # 27. Percentage of treated over HTN_140_90_med
  data$ratio_treat_HTN_140_90_med <- NA
  data$ratio_treat_HTN_140_90_med[which(data$HTN_140_90_med==1)] <- 0
  data$ratio_treat_HTN_140_90_med[which(data$HTN_140_90_med==1 & data$new_med==1)] <- 1
  # 28. Percentage of treated over HTN_160_100_med
  data$ratio_treat_HTN_160_100_med <- NA
  data$ratio_treat_HTN_160_100_med[which(data$HTN_160_100_med==1)] <- 0
  data$ratio_treat_HTN_160_100_med[which(data$HTN_160_100_med==1 & data$new_med==1)] <- 1
  # ----------------------------------
  # 10. Percentage of controlled over HTN 140_90_med
  data$ratio_ctrl_HTN_140_90_med <- NA
  data$ratio_ctrl_HTN_140_90_med[which(data$HTN_140_90_med==1)] <- 0
  data$ratio_ctrl_HTN_140_90_med[which(data$ctrlHTN_140_90_med==1)] <- 1
  # 31. Percentage of controlled over HTN 160_100_med
  data$ratio_ctrl_HTN_160_100_med <- NA
  data$ratio_ctrl_HTN_160_100_med[which(data$HTN_160_100_med==1)] <- 0
  data$ratio_ctrl_HTN_160_100_med[which(data$ctrlHTN_160_100_med==1)] <- 1
}

lists <- c(core_list, UHC_list, UHC_allcat_list, CW_list, UHC_test_list)
nums  <- 1:length(lists)


### Sample design ###  
countNA <- function(x)sum(!is.na(x))

# if PSU or stratum only has 1 value within a survey, remove it
p1 <- tapply(data$psu,data$id_study,function(...)length(unique(...)))
p1.list <- names(p1)[which(p1==1)]
p2 <- tapply(data$stratum,data$id_study,function(...)length(unique(...)))
p2.list <- names(p2)[which(p2==1)]

# Sample design specification ====
x<-tapply(data$psu,data$id_study,countNA);             psu_list <- names(x)[which(x>0)]
x<-tapply(data$stratum,data$id_study,countNA);         strat_list <- names(x)[which(x>0)]
psu_list <- setdiff(psu_list, p1.list)
strat_list <- setdiff(strat_list, p2.list)

x<-tapply(data$samplewt_bp,data$id_study,countNA);     wt_list <- names(x)[which(x>0)]

### CHECK SAMPLE DESIGN STATUS ####
surveys <- unique(data$id_study)
all_3 <- surveys[which(surveys%in%psu_list&surveys%in%strat_list&surveys%in%wt_list)]
psu_only <- surveys[which(surveys%in%psu_list&!surveys%in%strat_list&!surveys%in%wt_list)]
strat_only <- surveys[which(surveys%in%strat_list&!surveys%in%psu_list&!surveys%in%wt_list)]
wt_only <- surveys[which(surveys%in%wt_list&!surveys%in%psu_list&!surveys%in%strat_list)]
psu_strat <- surveys[which(surveys%in%psu_list&surveys%in%strat_list&!surveys%in%wt_list)]
psu_wt <- surveys[which(surveys%in%psu_list&!surveys%in%strat_list&surveys%in%wt_list)]
strat_wt <- surveys[which(!surveys%in%psu_list&surveys%in%strat_list&surveys%in%wt_list)]
none <- surveys[which(!surveys%in%psu_list&!surveys%in%strat_list&!surveys%in%wt_list)]

# assign 1 to sample weights in surveys without sample weights
data$samplewt_bp[ which( !data$id_study%in%wt_list )] <- 1

# drop missing psu in psu_list
data <- data[which(!is.na(data$psu)|!data$id_study%in%psu_list), ]
# drop missing stratum in strat_list
data <- data[which(!is.na(data$stratum)|!data$id_study%in%strat_list), ]
# drop missing sample weights in wt_list
data <- data[which(!is.na(data$samplewt_bp)| !data$id_study%in%wt_list), ]


## Calculating summaries ====
num.valid <- function(x) sum(!is.na(x)&x!=-1)
null <- t(c(NA,NA))
sumd <- function(tmp) {
  options(survey.lonely.psu = "adjust",survey.adjust.domain.lonely=TRUE)
  n <- nrow(tmp)
  if (n > 1) {
    if (tmp$id_study[1]%in%all_3) {
      dsub <- svydesign(id=~psu, strata=~stratum, weights=~samplewt_bp, data=tmp, nest=TRUE)
    } else if (tmp$id_study[1]%in%psu_only) {
      dsub <- dsub_int <- svydesign(id=~psu, strata=NULL, weights=NULL, data=tmp, nest=TRUE)
    } else if (tmp$id_study[1]%in%strat_only) {
      dsub <- dsub_int <- svydesign(id=~1, strata=~stratum, weights=NULL, data=tmp, nest=TRUE)
    } else if (tmp$id_study[1]%in%wt_only) {
      dsub <- svydesign(id=~1, strata=NULL, weights=~samplewt_bp, data=tmp, nest=TRUE)
    } else if (tmp$id_study[1]%in%psu_strat) {
      dsub <- dsub_int <- svydesign(id=~psu, strata=~stratum, weights=NULL, data=tmp, nest=TRUE)
    } else if (tmp$id_study[1]%in%psu_wt) {
      dsub <- svydesign(id=~psu, strata=NULL, weights=~samplewt_bp, data=tmp, nest=TRUE)
    } else if (tmp$id_study[1]%in%strat_wt) {
      dsub <- svydesign(id=~1, strata=~stratum, weights=~samplewt_bp, data=tmp, nest=TRUE)
    } else if (tmp$id_study[1]%in%none) {
      dsub <- dsub_int <- svydesign(id=~1, strata=NULL, weights=NULL, data=tmp)
    }
  }
  
  n_sbp <- num.valid(tmp$sbp_final); n_dbp <- num.valid(tmp$dbp_final)
  if (n_sbp>1) { 
    sbp_m  <- svymean(~sbp_final, dsub, na.rm=TRUE)
    sbp_sd <- sqrt(coef(svyvar(~sbp_final, dsub, na.rm=TRUE)))
  } else {
    sbp_m  <- null
    sbp_sd <- NA
  }
  if (n_dbp>1) { 
    dbp_m <- svymean(~dbp_final, dsub, na.rm=TRUE)
    dbp_sd <- sqrt(coef(svyvar(~dbp_final, dsub, na.rm=TRUE)))
  } else {
    dbp_m  <- null
    dbp_sd <- NA
  }
  
  n_med <- num.valid(tmp$new_med); n_diag <- num.valid(tmp$diagnosis)
  if (n_med>1) { med_m <- svymean(~new_med, dsub, na.rm=TRUE)
  } else med_m <- null
  if (n_diag>1) { diag_m <- svymean(~diagnosis, dsub, na.rm=TRUE)
  } else diag_m <- null
  
  res <- data.frame(n, n_sbp, sbp_m, sbp_sd, n_dbp, dbp_m, dbp_sd, n_med, med_m, n_diag, diag_m)
  names(res) <- c("n", "n_sbp", "mean_sbp", "se_sbp", "sd_sbp", "n_dbp", "mean_dbp", "se_dbp", "sd_dbp", "n_med", "prev_med", "se_med","n_diag", "prev_diag", "se_diag")
  
  for (i in 1:length(lists)) {
    num <- nums[i]
    var <- lists[i]
    eval(parse(text = paste0("n",num," <- num.valid(tmp$",var,")")))   # n1 <- num.valid(tmp$variable1)
    
    if (get(paste0("n",num)) > 1) {
      assign(paste0("prev_", num), eval(parse(text = paste0("svymean(~",var,", dsub, na.rm=TRUE)")))) # prev_1 <- svymean(~tmp$variable1, dsub, na.rm=T)
    } else assign(paste0("prev_", num), null)                          # else prev_1 <- null
  }
  
  res_tmp <- eval(parse(text = paste0("data.frame(", paste(paste0(c("n","prev_"), rep(nums,each=2)), collapse=", "),")")))
  names(res_tmp) <- paste(c("n","prev","se"), rep(lists, each=3), sep="_")
  res <- data.frame(res, res_tmp)  
  
  if (DIST) {
    if (n_sbp>2) {
      tt <- svyquantile(~sbp_final, dsub, c(0.2,0.5,0.8), se=TRUE, na.rm=TRUE, ties="rounded")
      sbp_quant <- t(c(rep(n_sbp,3),tt$quantiles,SE(tt))[c(1,4,7,2,5,8,3,6,9)])
    } else sbp_quant <- t(rep(NA,9))
    if (n_dbp>2) {
      tt <- svyquantile(~dbp_final, dsub, c(0.2,0.5,0.8), se=TRUE, na.rm=TRUE, ties="rounded")
      dbp_quant <- t(c(rep(n_dbp,3),tt$quantiles,SE(tt))[c(1,4,7,2,5,8,3,6,9)])
    } else dbp_quant <- t(rep(NA,9))
    
    res_dist <- data.frame(sbp_quant, dbp_quant)
    names(res_dist) <- c("n_sbp_q20", "mean_sbp_q20", "se_sbp_q20", "n_sbp_q50", "mean_sbp_q50", "se_sbp_q50", "n_sbp_q80", "mean_sbp_q80", "se_sbp_q80", "n_dbp_q20", "mean_dbp_q20", "se_dbp_q20", "n_dbp_q50", "mean_dbp_q50", "se_dbp_q50", "n_dbp_q80", "mean_dbp_q80", "se_dbp_q80")
    
    res <- data.frame(res, res_dist)
  }
  
  return(res)
}

# age_group and age_mean should be matched uniquely/interchangeablly
data.clean <- data[,c("id_study","is_urban","sex","age_mean","age_group","psu","stratum","samplewt_bp","samplewt_smoke","sbp_final","dbp_final","new_med","diagnosis",core_list, UHC_list, UHC_allcat_list, CW_list, UHC_test_list)]

# clean single PSU
data.clean 				    <- clean_single_psu_ssa(data.clean)
data.clean$id_study 	<- droplevels(data.clean$id_study)
data.clean$stratum 		<- as.numeric(as.factor(data.clean$stratum))


## FOR URBAN AND RURAL COMBINED ##
# summarising
summary0 <- ddply(data.clean, .(id_study, sex, age_mean), .fun=sumd)
summary <- summary0[with(summary0, order(id_study,sex,age_mean)), ]

# Link with age group
link <- unique(data.clean[,c("id_study","sex","age_mean","age_group")])
summary <- merge(summary,link)
summary <- summary[with(summary,which(n_sbp>0|n_dbp>0)),]


## FOR URBAN AND RURAL SEPARATELY ##
# summarising
summary0 <- ddply(data.clean, .(id_study, is_urban, sex, age_mean), .fun=sumd)
summary_ur <- summary0[with(summary0, order(id_study,is_urban,sex,age_mean)), ]

# Link with age group
link_ur <- unique(data.clean[,c("id_study","is_urban","sex","age_mean","age_group")])
summary_ur <- merge(summary_ur,link_ur)
summary_ur <- summary_ur[with(summary_ur,which(n_sbp>0|n_dbp>0)),]


################################################################################
########################## SAVE DATASETS  ######################################
################################################################################

write.csv(summary,file=paste0("BP_summary_ALL_adultonly_",date,".csv"),row.names=FALSE)
write.csv(summary_ur,file=paste0("BP_summary_ALL_adultonly_URstratified_",date,".csv"),row.names=FALSE)