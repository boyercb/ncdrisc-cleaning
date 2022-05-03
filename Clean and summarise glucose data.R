## NCD-RisC
## v1 (20210514)
## cleaning and summarising script for glucose

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
sink(paste("log_GluCleaning_",date, ".txt", sep=""))
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
# floor age
data$age <- floor(data$age)
# Mark Dropped
clnList <- 
  with(data, 
    which((sex==1&((is.na(age_max_glu_M)|age>age_max_glu_M)|(is.na(age_min_glu_M)|age<age_min_glu_M))) |
          (sex==2&((is.na(age_max_glu_F)|age>age_max_glu_F)|(is.na(age_min_glu_F)|age<age_min_glu_F))) ) )
data$dropped[clnList] <- paste(data$dropped[clnList], "DesignAge")

# print out
age.out <- table(data[clnList,]$id_study)/table(data$id_study)*100
print("Percentage of Implausible Values (Outside Designed Range) in Age (%)")
print(sort(round(age.out[which(age.out>0)],2), decreasing=TRUE))

# 4. Clean data in glucose ======================================================================
##   convert mg/dl and mg% to mmol/L # convert blood to plasma # clean for plausibility range #

### f. is_plasma ------------------------------------------------------------
clnList <- which(data$is_plasma!=0&data$is_plasma!=1)
print_cleaned("is_plasma")
if (length(clnList)>0) data$is_plasma[clnList] <- NA

### a. fgl ------------------------------------------------------------------
data$fgl_f <- data$fgl
fglunitList <- which(data$unit_gl %in% c("mg/dl","mg/dL","mg%"))
data$fgl_f[fglunitList] <- data$fgl_f[fglunitList] * 0.0556
data$fgl_f[which(data$is_plasma==0)] <- data$fgl_f[which(data$is_plasma==0)] * 1.11
max_fgl <- 30
min_fgl <- 2.5
clnList <- which(data$fgl_f>max_fgl | data$fgl_f<min_fgl)
print_cleaned("fgl_f")
if (length(clnList)>0) data$fgl_f[clnList] <- NA

### b. ppg ------------------------------------------------------------------
if (!"ppg"%in% names(data)) data$ppg <- NA
data$ppg_f <- data$ppg
ppgunitList <- which(data$unit_ppg %in% c("mg/dl","mg/dL","mg%"))
data$ppg_f[ppgunitList] <- data$ppg_f[ppgunitList] * 0.0556
data$ppg_f[which(data$is_plasma_ppg==0)] <- data$ppg_f[which(data$is_plasma_ppg==0)] * 1.11
max_ppg <- 30
min_ppg <- 1.5
clnList <- which(data$ppg_f>max_ppg|data$ppg_f<min_ppg)
print_cleaned("ppg_f")
if (length(clnList)>0) data$ppg_f[clnList] <- NA

### c. hba1c -----------------------------------------------------------------
if (!"hba1c"%in% names(data)) data$hba1c <- NA
data$hba1c_f <- data$hba1c
max_hba1c <- 18
min_hba1c <- 3
clnList <- which(data$hba1c_f>max_hba1c|data$hba1c_f<min_hba1c)
print_cleaned("hba1c_f")
if (length(clnList)>0) data$hba1c_f[clnList] <- NA

### d. is_fasting ### e. fasting_time ---------------------------------------
clnList <- which(data$is_fasting!=0&data$is_fasting!=1)
print_cleaned("is_fasting")
if (length(clnList)>0) data$is_fasting[clnList] <- NA

if (!"fasting_time"%in%names(data)) data$fasting_time <- NA
clnList <- which(data$fasting_time<0|data$fasting_time>48)
print_cleaned("fasting_time")
if (length(clnList)>0) data$fasting_time[clnList] <- NA

### g-j. diabetes diagnosis and drug use ------------------------------------
clnList <- which(data$self_diab!=0&data$self_diab!=1)
print_cleaned("self_diab")
if (length(clnList)>0) data$self_diab[clnList] <- NA

clnList <- which(data$drug_diab!=0&data$drug_diab!=1)
print_cleaned("drug_diab")
if (length(clnList)>0) data$drug_diab[clnList] <- NA

### m.q. samplewt_glu, psu, stratum -----------------------------------------
data$samplewt_glu[which(data$samplewt_glu<0)] <- NA
data$psu[which(data$psu<0)] <- NA
data$stratum[which(data$stratum<0)] <- NA

### p. is_urban, is_pregnant, is_pregnant_exam ------------------------------
clnList <- which(data$is_urban!=0&data$is_urban!=1)
print_cleaned("is_urban")
if (length(clnList)>0) data$is_urban[clnList] <- NA

## is_pregnant
clnList <- which(data$is_pregnant!=0&data$is_pregnant!=1)
print_cleaned("is_pregnant")
if (length(clnList)>0) data$is_pregnant[clnList] <- NA
# clean for age/sex
clnList <- which(data$is_pregnant==1&(data$sex==1|data$age>=50|data$age<10))
print_cleaned("is_pregnant")
if (length(clnList)>0) data$is_pregnant[clnList] <- 0
print("Cleaned pregnant males and pregnant females aged over 50 or under 10")

# Mark Dropped
dropList <- which(data$is_pregnant_exam==1|(is.na(data$is_pregnant_exam)&data$is_pregnant==1))
data$dropped[dropList] <- paste(data$dropped[dropList], "Preg")


# 5. Generate drug use (drug_diab_final) from pill and insulin use ==============================
data$drug_diab_final <- data$drug_diab

# 7. Generate "acceptable fasting time" =========================================================
data$fasting_accept <- NA
data$fasting_accept[which(data$fasting_time<=24 & data$fasting_time>=6)] <- 1
data$fasting_accept[which(data$fasting_time>24 | data$fasting_time<6)]   <- 0
data$fasting_accept[which(is.na(data$fasting_time) & data$is_fasting==1)] <- 1
data$fasting_accept[which(is.na(data$fasting_time) & data$is_fasting==0)] <- 0
data$fasting_accept[which(is.na(data$fasting_time) & is.na(data$is_fasting))] <- NA

clnList <- which(data$fasting_accept==0)
if (length(data$fgl_f[clnList])>0) data$fgl_f[clnList] <- NA
if (length(data$ppg_f[clnList])>0) data$ppg_f[clnList] <- NA


# Mark Dropped
dropList <- which(is.na(data$fgl_f)&is.na(data$ppg_f)&is.na(data$hba1c_f))
data$dropped[dropList] <- paste(data$dropped[dropList], "NoData")

# 8. create new_med    ==========================================================================
# 0. new_med combines self and med variables
data$new_med <- NA
data$new_med[which(data$self_diab==0&data$drug_diab_final==0)] <- 0
data$new_med[which(data$self_diab==1|data$drug_diab_final==1)] <- 1
data$new_med[which(data$self_diab==0&is.na(data$drug_diab_final))] <- 0  # already taken care of by Item 6, but kept to be safe

### if self_diab or drug_diab is not available at survey level
### use the other variable as new_med directly
library(data.table)
dt <- data.table(data)
sm.count <- dt[,list(N_med=sum(!is.na(drug_diab_final)),N_self=sum(!is.na(self_diab))),by=list(id_study)]
only.drug.list <- sm.count$id_study[sm.count$N_self==0&sm.count$N_med>0]
only.self.list <- sm.count$id_study[sm.count$N_med==0&sm.count$N_self>0]
data$new_med[data$id_study%in%only.drug.list] <- data$drug_diab_final[data$id_study%in%only.drug.list]
data$new_med[data$id_study%in%only.self.list] <- data$self_diab[data$id_study%in%only.self.list]  # already taken care of by Item 6, but kept to be safe

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


# only keep those over 18 years old   # to be changed when moving to children/adolescents as well
data <- data[which(data$age>=18),]
data$age_min_glu_M[which(data$age_min_glu_M<18)] <- 18
data$age_min_glu_F[which(data$age_min_glu_F<18)] <- 18

# drop records cleaned during data cleaning
data <- data[which(data$dropped=="Keep"),]

# Categorize age groups ====
data$age_min  <- with(data, ifelse(sex==1, age_min_glu_M, age_min_glu_F))
data$age_max  <- with(data, ifelse(sex==1, age_max_glu_M, age_max_glu_F))
tmp <- make_age_groups(data$age,data$age_min, data$age_max)
data$age_group <- tmp$age_group
data$age_mean  <- tmp$age_mean

#### Calculating indicators for different categories of DM ####
# 1. DM defind as: Fasting glucose>=7.0 mmol/L
data$DM_fgl_70 <-NA
data$DM_fgl_70[which(data$fgl_f>=7)] <- 1
data$DM_fgl_70[which(data$fgl_f<7)] <- 0

# 2. DM defind as: Fasting glucose>=7.0 mmol/L OR diagnosis OR medication
data$DM_fgl_70_self_med <- NA
data$DM_fgl_70_self_med[with(data,which(fgl_f>=7|new_med==1))] <- 1
data$DM_fgl_70_self_med[with(data,which(fgl_f<7&new_med==0))] <- 0
data$DM_fgl_70_self_med[with(data,which(is.na(fgl_f) | is.na(new_med)))] <- NA

# 3. DM defind as: Fasting glucose>=7.8 mmol/L OR diagnosis OR medication
data$DM_fgl_78_self_med <- NA
data$DM_fgl_78_self_med[with(data,which(fgl_f>=7.8|new_med==1))] <- 1
data$DM_fgl_78_self_med[with(data,which(fgl_f<7.8&new_med==0))] <- 0
data$DM_fgl_78_self_med[with(data,which(is.na(fgl_f) | is.na(new_med)))] <- NA

# 4. DM defined as: HbA1c>=6.5% OR diagnosis OR medication
data$DM_hba1c_65_self_med <- NA
data$DM_hba1c_65_self_med[with(data,which(hba1c_f>=6.5|new_med==1))] <- 1
data$DM_hba1c_65_self_med[with(data,which(hba1c_f<6.5&new_med==0))] <-0
data$DM_hba1c_65_self_med[with(data,which(is.na(hba1c_f)|is.na(new_med)))] <- NA

# 5. DM defined as: Postprandial glucose>=11.1 mmol/L OR diagnosis OR medication
data$DM_ppg_111_self_med <- NA
data$DM_ppg_111_self_med[with(data,which(ppg_f>=11.1|new_med==1))] <- 1
data$DM_ppg_111_self_med[with(data,which(ppg_f<11.1&new_med==0))] <- 0
data$DM_ppg_111_self_med[with(data,which(is.na(ppg_f)|is.na(new_med)))] <- NA

# 6. DM defined as: Fasting glucose>=7.0 mmol/L OR Postprandial glucose>=11.1 mmol/L OR diagnosis OR medication
data$DM_fgl_70_ppg_111_self_med <- NA
data$DM_fgl_70_ppg_111_self_med[with(data,which(fgl_f>=7|ppg_f>=11.1|new_med==1))] <- 1
data$DM_fgl_70_ppg_111_self_med[with(data,which((fgl_f<7&ppg_f<11.1&new_med==0)))] <- 0
data$DM_fgl_70_ppg_111_self_med[with(data,which(is.na(fgl_f) | is.na(ppg_f) | is.na(new_med)))] <- NA

# 7. Impaired fasting glucose defined as: NOT diagnosis NOT medication AND 5.6=<Fasting glucose<7.0  mmol/L
data$IFG_fgl_56_70_self_med <- NA
data$IFG_fgl_56_70_self_med[with(data,which(fgl_f>=5.6&fgl_f<=7&new_med==0))] <- 1
data$IFG_fgl_56_70_self_med[with(data,which((fgl_f<5.6|fgl_f>7)|new_med==1))] <- 0
data$IFG_fgl_56_70_self_med[with(data,which(is.na(fgl_f) | is.na(new_med)))] <- NA

# 8. Impaired glucose tolerance defined as: NOT diagnosis NOT medication AND 7.8=<Postprandial glucose<11.1 mmol/L
data$IGT_ppg_78_111_self_med <- NA
data$IGT_ppg_78_111_self_med[with(data,which(ppg_f>=7.8&ppg_f<=11.1&new_med==0))] <- 1
data$IGT_ppg_78_111_self_med[with(data,which((ppg_f<7.8|ppg_f>11.1)|new_med==1))] <- 0
data$IGT_ppg_78_111_self_med[with(data,which(is.na(ppg_f)|is.na(new_med)))] <- NA

# 9. Prevalence based on: FPG>=7.0mmol/L, HbA1c>=6.5%
data$DM_fgl_70_hba1c_65_self_med <- NA
data$DM_fgl_70_hba1c_65_self_med[with(data,which(fgl_f>=7|hba1c_f>=6.5|new_med==1))] <- 1
data$DM_fgl_70_hba1c_65_self_med[with(data,which((fgl_f<7&hba1c_f<6.5&new_med==0)))] <- 0
data$DM_fgl_70_hba1c_65_self_med[with(data,which(is.na(fgl_f) | is.na(hba1c_f) | is.na(new_med)))] <- NA

# 10. Prevalence based on all three metrics: FPG>=7.0mmol/L, PPG>=11.1mmol/L, HbA1c>=6.5%
data$DM_ALL_self_med <- NA
data$DM_ALL_self_med[with(data,which(fgl_f>=7|ppg_f>=11.1|hba1c_f>=6.5|new_med==1))] <- 1
data$DM_ALL_self_med[with(data,which((fgl_f<7&ppg_f<11.1&hba1c_f<6.5&new_med==0)))] <- 0
data$DM_ALL_self_med[with(data,which(is.na(fgl_f)|is.na(ppg_f)|is.na(hba1c_f)|is.na(new_med) ))] <- NA

# 11. DM defind as: Fasting glucose>=6.1 mmol/L
data$DM_fgl_61 <- 0
data$DM_fgl_61[which(is.na(data$fgl_f))] <- NA
data$DM_fgl_61[which(data$fgl_f>=6.1)] <- 1

# 12. DM defind as: Fasting glucose>=6.7 mmol/L
data$DM_fgl_67 <- 0
data$DM_fgl_67[which(is.na(data$fgl_f))] <- NA
data$DM_fgl_67[which(data$fgl_f>=6.7)] <- 1

# 13. DM defind as: Fasting glucose>=6.8 mmol/L
data$DM_fgl_68 <- 0
data$DM_fgl_68[which(is.na(data$fgl_f))] <- NA
data$DM_fgl_68[which(data$fgl_f>=6.8)] <- 1

# 14. DM defind as: Fasting glucose>=7.8 mmol/L
data$DM_fgl_78 <- 0
data$DM_fgl_78[which(is.na(data$fgl_f))] <- NA
data$DM_fgl_78[which(data$fgl_f>=7.8)] <- 1

# 15. DM defind as: Fasting glucose>=5.6 mmol/L OR diagnosis OR medication
data$DM_fgl_56_self_med <- NA
data$DM_fgl_56_self_med[with(data,which(fgl_f>=5.6|new_med==1))] <- 1
data$DM_fgl_56_self_med[with(data,which(fgl_f<5.6&new_med==0))] <- 0
data$DM_fgl_56_self_med[with(data,which(is.na(fgl_f) | is.na(new_med)))] <- NA

# 16. DM defind as: Fasting glucose>=6.1 mmol/L OR diagnosis OR medication
data$DM_fgl_61_self_med <- NA
data$DM_fgl_61_self_med[with(data,which(fgl_f>=6.1|new_med==1))] <- 1
data$DM_fgl_61_self_med[with(data,which(fgl_f<6.1&new_med==0))] <- 0
data$DM_fgl_61_self_med[with(data,which(is.na(fgl_f) | is.na(new_med)))] <- NA

# 17. DM defind as: Fasting glucose>=6.8 mmol/L OR diagnosis OR medication
data$DM_fgl_68_self_med <- NA
data$DM_fgl_68_self_med[with(data,which(fgl_f>=6.8|new_med==1))] <- 1
data$DM_fgl_68_self_med[with(data,which(fgl_f<6.8&new_med==0))] <- 0
data$DM_fgl_68_self_med[with(data,which(is.na(fgl_f) | is.na(new_med)))] <- NA

# 18. DM defined as: Fasting glucose>=7.0 mmol/L OR Postprandial glucose>=11.1 mmol/L 
data$DM_fgl_70_ppg_111 <- NA
data$DM_fgl_70_ppg_111[with(data,which(fgl_f>=7|ppg_f>=11.1))] <- 1
data$DM_fgl_70_ppg_111[with(data,which((fgl_f<7&ppg_f<11.1)))] <- 0
data$DM_fgl_70_ppg_111[with(data,which(is.na(fgl_f)|is.na(ppg_f)))] <- NA

# 19. DM defined as: Fasting glucose>=7.8 mmol/L OR Postprandial glucose>=11.1 mmol/L 
data$DM_fgl_78_ppg_111 <- NA
data$DM_fgl_78_ppg_111[with(data,which(fgl_f>=7.8|ppg_f>=11.1))] <- 1
data$DM_fgl_78_ppg_111[with(data,which((fgl_f<7.8&ppg_f<11.1)))] <- 0
data$DM_fgl_78_ppg_111[with(data,which(is.na(fgl_f)|is.na(ppg_f)))] <- NA

# 20. DM defined as: Fasting glucose>=8.6 mmol/L OR Postprandial glucose>=12.3 mmol/L 
data$DM_fgl_86_ppg_123 <- NA
data$DM_fgl_86_ppg_123[with(data,which(fgl_f>=8.6|ppg_f>=12.3))] <- 1
data$DM_fgl_86_ppg_123[with(data,which((fgl_f<8.6&ppg_f<12.3)))] <- 0
data$DM_fgl_86_ppg_123[with(data,which(is.na(fgl_f)|is.na(ppg_f)))] <- NA

# 21. DM defined as: Fasting glucose>=7.8 mmol/L OR Postprandial glucose>=11.1 mmol/L OR diagnosis OR medication
data$DM_fgl_78_ppg_111_self_med <- NA
data$DM_fgl_78_ppg_111_self_med[with(data,which(fgl_f>=7.8|ppg_f>=11.1|new_med==1))] <- 1
data$DM_fgl_78_ppg_111_self_med[with(data,which((fgl_f<7.8&ppg_f<11.1&new_med==0)))] <- 0
data$DM_fgl_78_ppg_111_self_med[with(data,which(is.na(fgl_f) | is.na(ppg_f) | is.na(new_med)))] <- NA

# 22. DM defined as: Postprandial glucose>=11.1 mmol/L 
data$DM_ppg_111 <- NA
data$DM_ppg_111[with(data,which(ppg_f>=11.1))] <- 1
data$DM_ppg_111[with(data,which(ppg_f<11.1))] <- 0

# 23. DM defined as: HbA1c>=10% OR diagnosis OR medicationn
data$DM_hba1c_10_self_med <- NA
data$DM_hba1c_10_self_med[with(data,which(hba1c_f>=10|new_med==1))] <- 1
data$DM_hba1c_10_self_med[with(data,which(hba1c_f<10&new_med==0))] <-0
data$DM_hba1c_10_self_med[with(data,which(is.na(hba1c_f)|is.na(new_med)))] <- NA

# 24. DM defind as: Fasting glucose>=5.5 mmol/L OR diagnosis OR medication
data$DM_fgl_55_self_med <- NA
data$DM_fgl_55_self_med[with(data,which(fgl_f>=5.5|new_med==1))] <- 1
data$DM_fgl_55_self_med[with(data,which(fgl_f<5.5&new_med==0))] <- 0
data$DM_fgl_55_self_med[with(data,which(is.na(fgl_f) | is.na(new_med)))] <- NA

# 25. prevalence defined as: Fasting glucose >=6.1 and <7.0 mmol/L
data$DM_fgl_61_70 <- NA
data$DM_fgl_61_70[with(data,which(fgl_f>=6.1&fgl_f<7))] <- 1
data$DM_fgl_61_70[with(data,which(fgl_f<5.6|fgl_f>=7))] <- 0

# 26. DM defind as: Fasting glucose>=6.0 mmol/L
data$DM_fgl_60 <- 0
data$DM_fgl_60[which(is.na(data$fgl_f))] <- NA
data$DM_fgl_60[which(data$fgl_f>=6.0)] <- 1

# 27. Percentage of diagnosed over all DM_70_self_med
data$ratio_diag_DM_70_self_med <- NA
data$ratio_diag_DM_70_self_med[which(data$DM_fgl_70_self_med == 1)] <- 0
data$ratio_diag_DM_70_self_med[which(data$DM_fgl_70_self_med == 1 & data$self_diab == 1)] <- 1

# 28. Percentage of treated over diagnosed
data$ratio_treat_over_diag <- NA
data$ratio_treat_over_diag[which(data$self_diab == 1 & !is.na(data$drug_diab_final))] <- 0
data$ratio_treat_over_diag[which(data$self_diab == 1 & data$drug_diab_final == 1)] <- 1

# 29. Percentage of treated over all DM_70_self_med
data$ratio_treat_DM_70_self_med <- NA
data$ratio_treat_DM_70_self_med[which(data$DM_fgl_70_self_med == 1)] <- 0
data$ratio_treat_DM_70_self_med[which(data$DM_fgl_70_self_med == 1 & data$drug_diab_final == 1)] <- 1

# 30. Percentage of treated over all DM_70_self_med with diagnsis
# equivalent to treated over diagnosed and with FPG available
data$ratio_treat_DM_70_self_med_diag <- NA
data$ratio_treat_DM_70_self_med_diag[which(data$DM_fgl_70_self_med == 1 & data$self_diab == 1)] <- 0
data$ratio_treat_DM_70_self_med_diag[which(data$DM_fgl_70_self_med == 1 & data$self_diab == 1 & data$drug_diab_final == 1)] <- 1

lists <- c("DM_fgl_70","DM_fgl_70_self_med","DM_fgl_78_self_med","DM_hba1c_65_self_med","DM_ppg_111_self_med","DM_fgl_70_ppg_111_self_med","IFG_fgl_56_70_self_med","IGT_ppg_78_111_self_med","DM_fgl_70_hba1c_65_self_med","DM_ALL_self_med","DM_fgl_61","DM_fgl_67","DM_fgl_68","DM_fgl_78","DM_fgl_56_self_med","DM_fgl_61_self_med","DM_fgl_68_self_med","DM_fgl_70_ppg_111","DM_fgl_78_ppg_111","DM_fgl_86_ppg_123","DM_fgl_78_ppg_111_self_med","DM_ppg_111","DM_hba1c_10_self_med","DM_fgl_55_self_med","DM_fgl_61_70","DM_fgl_60","ratio_diag_DM_70_self_med","ratio_treat_over_diag","ratio_treat_DM_70_self_med","ratio_treat_DM_70_self_med_diag")
nums <- 1:length(lists)

### sample design ####
countNA <- function(x)sum(!is.na(x))

# correction 22 May 2015
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

x<-tapply(data$samplewt_glu,data$id_study,countNA);    wt_list <- names(x)[which(x>0)]

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
data$samplewt_glu[ which(!data$id_study%in%wt_list )] <- 1

# drop missing psu in psu_list
data <- data[which(!is.na(data$psu)|!data$id_study%in%psu_list), ]
# drop missing stratum in strat_list
data <- data[which(!is.na(data$stratum)|!data$id_study%in%strat_list), ]
# drop missing sample weights in wt_list
data <- data[which(!is.na(data$samplewt_glu)| !data$id_study%in%wt_list), ]

## Calculating summaries ====
num.valid <- function(x) sum(!is.na(x)&x!=-1)
null <- t(c(NA,NA))

sumd <- function(tmp) {
  options(survey.lonely.psu = "adjust",survey.adjust.domain.lonely=TRUE)
  n <- nrow(tmp)
  n_fgl <- num.valid(tmp$fgl_f); n_ppg <- num.valid(tmp$ppg_f); n_hba1c <- num.valid(tmp$hba1c_f)
  
  if (n > 1) {
    if (tmp$id_study[1]%in%all_3) {
      dsub <- svydesign(id=~psu, strata=~stratum, weights=~samplewt_glu, data=tmp, nest=T)
    } else if (tmp$id_study[1]%in%psu_only) {
      dsub <- svydesign(id=~psu, strata=NULL, weights=NULL, data=tmp, nest=T)
    } else if (tmp$id_study[1]%in%strat_only) {
      dsub <- svydesign(id=~1, strata=NULL, weights=NULL, data=tmp, nest=T)
    } else if (tmp$id_study[1]%in%wt_only) {
      dsub <- svydesign(id=~1, strata=NULL, weights=~samplewt_glu, data=tmp, nest=T)
    } else if (tmp$id_study[1]%in%psu_strat) {
      dsub <- svydesign(id=~psu, strata=~stratum, weights=NULL, data=tmp, nest=T)
    } else if (tmp$id_study[1]%in%psu_wt) {
      dsub <- svydesign(id=~psu, strata=NULL, weights=~samplewt_glu, data=tmp, nest=T)
    } else if (tmp$id_study[1]%in%strat_wt) {
      dsub <- svydesign(id=~1, strata=~stratum, weights=~samplewt_glu, data=tmp, nest=T)
    } else if (tmp$id_study[1]%in%none) {
      dsub <- svydesign(id=~1, strata=NULL, weights=NULL, data=tmp)
    }
  }
  
  if (n_fgl>1) { fgl_m <- svymean(~tmp$fgl_f, dsub, na.rm=T)
  sd_fgl <- sqrt(coef(svyvar(~tmp$fgl_f, dsub, na.rm=T)))
  } else {fgl_m <- null
  sd_fgl <- as.numeric(NA)
  }
  if (n_ppg>1) { ppg_m <- svymean(~tmp$ppg_f, dsub, na.rm=T)
  sd_ppg <- sqrt(coef(svyvar(~tmp$ppg_f, dsub, na.rm=T)))
  } else {ppg_m <- null
  sd_ppg <- as.numeric(NA)
  }
  if (n_hba1c>1) { hba1c_m <- svymean(~tmp$hba1c_f, dsub, na.rm=T)
  sd_hba1c <- sqrt(coef(svyvar(~tmp$hba1c_f, dsub, na.rm=T)))
  } else {hba1c_m <- null
  sd_hba1c <- as.numeric(NA)
  }

  res <- data.frame(n, n_fgl, fgl_m, sd_fgl, n_ppg, ppg_m, sd_ppg, n_hba1c, hba1c_m, sd_hba1c)
  names(res) <- c("N", "n_fgl", "mean_fgl", "se_fgl", "sd_fgl", "n_ppg", "mean_ppg", "se_ppg", "sd_ppg", "n_hba1c", "mean_hba1c", "se_hba1c", "sd_hba1c")
  
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
  
  return(res)
}

# clean single PSU
data <- clean_single_psu_ssa(data)

## FOR URBAN AND RURAL COMBINED ##
# summarising
summary0 <- ddply(data, .(id_study, sex, age_mean), .fun=sumd)
summary <- summary0[with(summary0, order(id_study,sex,age_mean)), ]

# Link with age group
link <- unique(data[,c("id_study","sex","age_mean","age_group")])
summary <- merge(summary,link)
summary <- summary[with(summary,which(n_fgl>0|n_ppg>0|n_hba1c>0)),]

## FOR URBAN AND RURAL SEPARATELY ##
# summarising
summary0 <- ddply(data, .(id_study, is_urban, sex, age_mean), .fun=sumd)
summary_ur <- summary0[with(summary0, order(id_study,is_urban,sex,age_mean)), ]

# Link with age group
link <- unique(data[,c("id_study","is_urban","sex","age_mean","age_group")])
summary_ur <- merge(summary_ur,link)
summary_ur <- summary_ur[with(summary_ur,which(n_fgl>0|n_ppg>0|n_hba1c>0)),]


################################################################################
########################## SAVE DATASETS  ######################################
################################################################################

write.csv(summary,file=paste0("Glucose_summary_ALL_adultonly_",date,".csv"), row.names = FALSE)
write.csv(summary_ur,file=paste0("Glucose_summary_ALL_adultonly_URstratified_",date,".csv"), row.names = FALSE)