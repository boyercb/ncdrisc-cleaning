## NCD-RisC
## v1 (20210514)
## cleaning and summarising script for cholesterol

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
sink(paste0("log_CHOLCleaning_",date, ".txt"))
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
print(round(age.na[which(age.na>0)],2))

clnList <- which(is.na(data$sex))
sex.na <- table(data[clnList,]$id_study)/table(data$id_study)*100
print("Percentage of NAs in Sex (%)")
print(round(sex.na[which(sex.na>0)],2))

# Clean for ages outside the design range -------------------------------------------------------
# floor age
data$age <- floor(data$age)
# Mark Dropped
clnList <-
  with(data, 
       which((sex==1&((is.na(age_max_chol_M)|age>age_max_chol_M)|(is.na(age_min_chol_M)|age<age_min_chol_M))) |
             (sex==2&((is.na(age_max_chol_F)|age>age_max_chol_F)|(is.na(age_min_chol_F)|age<age_min_chol_F))) ) )
data$dropped[clnList] <- paste(data$dropped[clnList], "DesignAge")

# print out
age.out <- table(data[clnList,]$id_study)/table(data$id_study)*100
print("Percentage of Implausible Values (Outside Designed Range) in Age (%)")
print(round(age.out[which(age.out>0)],2))

# 4. Clean data in cholesterol ======================================================================
##   set negatives to missing #
##   convert mg/dl and mg% to mmol/L # clean for plausibility range #

### a. tc ------------------------------------------------------------------
data$tc_f <- data$tc
tcunitList <- which(data$unit_tc %in% c("mg/dl","mg/dL","mg%"))
data$tc_f[tcunitList] <- data$tc_f[tcunitList] * 0.0259
max_tc <- 20
min_tc <- 1.75
clnList <- which(data$tc_f>max_tc | data$tc_f<min_tc)
print_cleaned("tc_f")
if (length(clnList)>0) data$tc_f[clnList] <- NA

### b. ldl ------------------------------------------------------------------
data$ldl_f <- data$ldl
ldlunitList <- which(data$unit_ldl %in% c("mg/dl","mg/dL","mg%"))
data$ldl_f[ldlunitList] <- data$ldl_f[ldlunitList] * 0.0259
max_ldl <- 10
min_ldl <- 0.5
clnList <- which(data$ldl_f>max_ldl | data$ldl_f<min_ldl)
print_cleaned("ldl_f")
if (length(clnList)>0) data$ldl_f[clnList] <- NA

### c. hdl ------------------------------------------------------------------
data$hdl_f <- data$hdl
hdlunitList <- which(data$unit_hdl %in% c("mg/dl","mg/dL","mg%"))
data$hdl_f[hdlunitList] <- data$hdl_f[hdlunitList] * 0.0259
max_hdl <- 5
min_hdl <- 0.4
clnList <- which(data$hdl_f>max_hdl|data$hdl_f<min_hdl)
print_cleaned("hdl_f")
if (length(clnList)>0) data$hdl_f[clnList] <- NA

### d. trg ------------------------------------------------------------------
data$trg_f <- data$trg
trgunitList <- which(data$unit_trg %in% c("mg/dl","mg/dL","mg%"))
data$trg_f[trgunitList] <- data$trg_f[trgunitList] * 0.0113
max_trg <- 20
min_trg <- 0.2
clnList <- which(data$trg_f>max_trg|data$trg_f<min_trg)
print_cleaned("trg_f")
if (length(clnList)>0) data$trg_f[clnList] <- NA

### d. drop any participant with implausible values
# TC < HDL
clnList <- which(!is.na(data$tc_f)&!is.na(data$hdl_f)&data$tc_f<data$hdl_f)
if (length(clnList)>0) {
  data$tc_f[clnList] <- NA
  data$hdl_f[clnList] <-NA
}
# TC < LDL
clnList <- which(!is.na(data$tc_f)&!is.na(data$ldl_f)&data$tc_f<data$ldl_f)
if (length(clnList)>0) {
  data$tc_f[clnList] <- NA
  data$ldl_f[clnList] <- NA
}
# TC < HDL + HDL + margin
margin <- ifelse(data$tc_f<4.040, -0.7125, ifelse(data$tc_f<4.714, -0.8821, ifelse(data$tc_f<5.465, -0.97, -1.752)))
clnList <- which(!is.na(data$tc_f)&!is.na(data$hdl_f)&!is.na(data$ldl_f) & (data$tc_f - data$hdl_f - data$ldl_f < margin))
if (length(clnList)>0) {
  data$tc_f[clnList] <- NA
  data$hdl_f[clnList] <-NA
  data$ldl_f[clnList] <-NA
}

# Mark Dropped
dropList <- which(is.na(data$tc_f)&is.na(data$hdl_f))
data$dropped[dropList] <- paste(data$dropped[dropList], "NoData")

### i-k. samplewt_chol, psu, stratum -----------------------------------------
data$samplewt_chol[which(data$samplewt_chol<0)] <- NA
data$psu[which(data$psu<0)] <- NA
data$stratum[which(data$stratum<0)] <- NA

### l-n. is_urban, is_pregnant ------------------------------
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
dropList <- which(data$is_pregnant==1)
data$dropped[dropList] <- paste(data$dropped[dropList], "Preg")

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

# Clean dataset ====
# 18+ only
data <- data[which(data$age>=18),]
data$age_min_chol_M[which(data$age_min_chol_M<18)] <- 18
data$age_min_chol_F[which(data$age_min_chol_F<18)] <- 18

# drop records cleaned during data cleaning
data <- data[which(data$dropped=="Keep"),]

# Categorize age groups ====
data$age_min  <- with(data, ifelse(sex==1, age_min_chol_M, age_min_chol_F))
data$age_max  <- with(data, ifelse(sex==1, age_max_chol_M, age_max_chol_F))
tmp <- make_age_groups(data$age,data$age_min, data$age_max)
data$age_group <- tmp$age_group
data$age_mean  <- tmp$age_mean


## Calculating non-HDL  and TC-to-HDL ratio ====
# 1. gen non-HDL (calculated as TC-HDL) ---------------------------------- 
data$non_hdl <- NA
non_hdl_list <- which(!is.na(data$tc_f)&!is.na(data$hdl_f))
data$non_hdl[non_hdl_list] <- data$tc_f[non_hdl_list] - data$hdl_f[non_hdl_list]

# 2. gen TC-to-HDL ratio (calculated as TC/HDL) --------------------------
##tc_hdl_r
data$tc_hdl_r <- NA
data$tc_hdl_r[non_hdl_list] <- data$tc_f[non_hdl_list]/data$hdl_f[non_hdl_list]

# 3. gen LDL when TC/HDL/TRG available using Friedewald equation ---------
ldl_comp_list <- unique(data$id_study[which(!is.na(data$tc_f)&!is.na(data$hdl_f)&!is.na(data$trg_f))])
ldl_list      <- unique(data$id_study[which(!is.na(data$ldl_f))])
calc_ldl_list <- which(data$id_study %in% setdiff(ldl_comp_list, ldl_list) & data$trg_f <= 4.52)
data$ldl_f[calc_ldl_list] <- data$tc_f[calc_ldl_list] - data$hdl_f[calc_ldl_list] - data$trg_f[calc_ldl_list]*0.45
message(paste("Calculated LDL using Friedewald equation for", paste(setdiff(ldl_comp_list, ldl_list), collapse=" ")))
# clean calculated LDL
clnList <- which(data$ldl_f>max_ldl | data$ldl_f<min_ldl)
print_cleaned("ldl_f")
if (length(clnList)>0) data$ldl_f[clnList] <- NA

## Calculating prevalence ====

# 1. Raised TC defind as: TC>=5 mmol/L
data$chol_tc_5 <- 0
data$chol_tc_5[which(data$tc_f>=5)] <- 1
data$chol_tc_5[which(is.na(data$tc_f))] <- NA

countNA <- function(x)sum(!is.na(x))
# Sample design specification ====
x<-tapply(data$psu,data$id_study,countNA);              psu_list <- names(x)[which(x>0)]
x<-tapply(data$stratum,data$id_study,countNA);          strat_list <- names(x)[which(x>0)]
x<-tapply(data$samplewt_chol,data$id_study,countNA);    chol_wt_list <- names(x)[which(x>0)]

### CHECK SAMPLE DESIGN STATUS ####
surveys <- unique(data$id_study)
all_3_chol <- surveys[which(surveys%in%psu_list&surveys%in%strat_list&surveys%in%chol_wt_list)]
psu_only_chol <- surveys[which(surveys%in%psu_list&!surveys%in%strat_list&!surveys%in%chol_wt_list)]
strat_only_chol <- surveys[which(surveys%in%strat_list&!surveys%in%psu_list&!surveys%in%chol_wt_list)]
wt_only_chol <- surveys[which(surveys%in%chol_wt_list&!surveys%in%psu_list&!surveys%in%strat_list)]
psu_strat_chol <- surveys[which(surveys%in%psu_list&surveys%in%strat_list&!surveys%in%chol_wt_list)]
psu_wt_chol <- surveys[which(surveys%in%psu_list&!surveys%in%strat_list&surveys%in%chol_wt_list)]
strat_wt_chol <- surveys[which(!surveys%in%psu_list&surveys%in%strat_list&surveys%in%chol_wt_list)]
none_chol <- surveys[which(!surveys%in%psu_list&!surveys%in%strat_list&!surveys%in%chol_wt_list)]

# assign 0 to missing sample weights in surveys with sample weights and in surveys with nothing
data$samplewt_chol[ which( is.na(data$samplewt_chol) & data$id_study%in%chol_wt_list )] <- 0
# assign 1 to sample weights in surveys without sample weights
data$samplewt_chol[ which( !data$id_study%in%chol_wt_list )] <- 1

# drop missing psu in psu_list
data <- data[which(!is.na(data$psu)|!data$id_study%in%psu_list), ]
# drop missing stratum in strat_list
data <- data[which(!is.na(data$stratum)|!data$id_study%in%strat_list), ]

## Calculating summaries ====
num.valid <- function(x) {
  return(sum(!is.na(x)&x!=-1))
}

null <- t(c(NA,NA))
sumd <- function(tmp) {
  n <- nrow(tmp)
  n_tc <- num.valid(tmp$tc_f); n_ldl <- num.valid(tmp$ldl_f); n_hdl <- num.valid(tmp$hdl_f); n_trg <- num.valid(tmp$trg_f)
  n_non_hdl <- num.valid(tmp$non_hdl); n_tc_hdl_r <- num.valid(tmp$tc_hdl_r)
  n1 <- num.valid(tmp$chol_tc_5)
  
  if (n > 1) {
    if (tmp$id_study[1]%in%all_3_chol) {
      dsub_chol <- svydesign(id=~psu, strata=~stratum, weights=~samplewt_chol, data=tmp, nest=T)
    } else if (tmp$id_study[1]%in%psu_only_chol) {
      dsub_chol <- svydesign(id=~psu, strata=NULL, weights=NULL, data=tmp, nest=T)
    } else if (tmp$id_study[1]%in%strat_only_chol) {
      dsub_chol <- svydesign(id=~1, strata=~stratum, weights=NULL, data=tmp, nest=T)
    } else if (tmp$id_study[1]%in%wt_only_chol) {
      dsub_chol <- svydesign(id=~1, strata=NULL, weights=~samplewt_chol, data=tmp, nest=T)
    } else if (tmp$id_study[1]%in%psu_strat_chol) {
      dsub_chol <- svydesign(id=~psu, strata=~stratum, weights=NULL, data=tmp, nest=T)
    } else if (tmp$id_study[1]%in%psu_wt_chol) {
      dsub_chol <- svydesign(id=~psu, strata=NULL, weights=~samplewt_chol, data=tmp, nest=T)
    } else if (tmp$id_study[1]%in%strat_wt_chol) {
      dsub_chol <- svydesign(id=~1, strata=~stratum, weights=~samplewt_chol, data=tmp, nest=T)
    } else if (tmp$id_study[1]%in%none_chol) {
      dsub_chol <- svydesign(id=~1, strata=NULL, weights=NULL, data=tmp)
    }
  }
  
  if (n_tc>1) { 
    tc_m  <- svymean(~tmp$tc_f, dsub_chol, na.rm=T)
    tc_sd <- sqrt(coef(svyvar(~tmp$tc_f, dsub_chol, na.rm=T)))
  } else {
    tc_m  <- null
    tc_sd <- NA
  }
  
  if (n_ldl>1) { 
    ldl_m <- svymean(~tmp$ldl_f, dsub_chol, na.rm=T)
    ldl_sd <- sqrt(coef(svyvar(~tmp$ldl_f, dsub_chol, na.rm=T)))
  } else {
    ldl_m  <- null
    ldl_sd <- NA
  }
  if (n_hdl>1) { 
    hdl_m <- svymean(~tmp$hdl_f, dsub_chol, na.rm=T)
    hdl_sd <- sqrt(coef(svyvar(~tmp$hdl_f, dsub_chol, na.rm=T)))
  } else {
    hdl_m  <- null
    hdl_sd <- NA
  }
  if (n_trg>1) { 
    trg_m <- svymean(~tmp$trg_f, dsub_chol, na.rm=T)
    trg_sd <- sqrt(coef(svyvar(~tmp$trg_f, dsub_chol, na.rm=T)))
  } else {
    trg_m  <- null
    trg_sd <- NA
  }
  if (n_non_hdl>1) { 
    non_hdl_m <- svymean(~tmp$non_hdl, dsub_chol, na.rm=T)
    non_hdl_sd <- sqrt(coef(svyvar(~tmp$non_hdl, dsub_chol, na.rm=T)))
  } else {
    non_hdl_m  <- null
    non_hdl_sd <- NA
  }
  if (n_tc_hdl_r>1) { 
    tc_hdl_r_m <- svymean(~tmp$tc_hdl_r, dsub_chol, na.rm=T)
    tc_hdl_r_sd <- sqrt(coef(svyvar(~tmp$tc_hdl_r, dsub_chol, na.rm=T)))
  } else {
    tc_hdl_r_m  <- null
    tc_hdl_r_sd <- NA
  }
  if (n1>1) { prev_1 <- svymean(~tmp$chol_tc_5, dsub_chol, na.rm=T)
  } else prev_1 <- null
  
  res <- data.frame(n, n_tc, tc_m, tc_sd, n_ldl, ldl_m, ldl_sd,
                    n_hdl, hdl_m, hdl_sd, n_trg, trg_m, trg_sd,
                    n_non_hdl, non_hdl_m, non_hdl_sd,
                    n_tc_hdl_r, tc_hdl_r_m, tc_hdl_r_sd,
                    n1, prev_1)
  names(res) <- c("n", "n_tc", "mean_tc", "se_tc", "sd_tc",
                  "n_ldl", "mean_ldl", "se_ldl", "sd_ldl",
                  "n_hdl", "mean_hdl", "se_hdl", "sd_hdl",
                  "n_trg", "mean_trg", "se_trg", "sd_trg",
                  "n_non_hdl", "mean_non_hdl", "se_non_hdl", "sd_non_hdl",
                  "n_tc_hdl_r", "mean_tc_hdl_r", "se_tc_hdl_r",  "sd_tc_hdl_r",
                  "n_chol_tc_5", "prev_chol_tc_5", "se_chol_tc_5")
  return(res)
}

options(survey.lonely.psu = "adjust",survey.adjust.domain.lonely=TRUE)


## FOR URBAN AND RURAL COMBINED ##
# summarising
summary0 <- ddply(data, .(id_study, sex, age_mean), .fun=sumd)
summary <- summary0[with(summary0, order(id_study,sex,age_mean)), ]

# Link with age group
link <- unique(data[,c("id_study","sex","age_mean","age_group")])
summary <- merge(summary,link)
summary <- summary[with(summary,which(n_tc>0|n_hdl>0)),]

## FOR URBAN AND RURAL SEPARATELY ##
# summarising
summary0_ur <- ddply(data, .(id_study, is_urban, sex, age_mean), .fun=sumd)
summary_ur <- summary0_ur[with(summary0_ur, order(id_study,is_urban,sex,age_mean)), ]

# Link with age group
link_ur <- unique(data[,c("id_study","is_urban","sex","age_mean","age_group")])
summary_ur <- merge(summary_ur,link_ur)
summary_ur <- summary_ur[with(summary_ur,which(n_tc>0|n_hdl>0)),]

################################################################################
########################## SAVE DATASETS  ######################################
################################################################################
write.csv(summary,file=paste0("CHOL_summary_ALL_adultonly_",date,".csv"), row.names = FALSE)
write.csv(summary_ur,file=paste0("CHOL_summary_ALL_adultonly_URstratified_",date,".csv"), row.names = FALSE)
