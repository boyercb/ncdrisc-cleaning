## NCD-RisC
## v1 (20210514)
## cleaning and summarising script for anthropometrics

library(survey)
library(plyr)

date <- format(Sys.Date(),"%Y%m%d")
source("utilities/anthro_functions.R")

################################################################################
############################# DATA #############################################
################################################################################

# read data
data <- read.csv("input_data.csv")

################################################################################
########################## DATA CLEANING #######################################
################################################################################

## GENERAL CLEANING ##

## Sex check ##
sex.na <- unique(as.character(data$id_study[is.na(data$sex)])) # studies containing NA values
table(data$sex, exclude = NULL) # possible values for sex variable
sex.n <- ddply(data ,.(id_study), function(tmp) length(unique(tmp$sex))) # counts the different sex values in each study
sex.check <- ddply(data ,.(id_study), function(tmp) all(is.na(tmp$sex))) # returns TRUE when all participants for a study have sex = NA
unique(sex.check[, 2]) # If FALSE, in all studies at least some participants have sex reported

## Drop from the dataset subjects <5 years old
data <- subset(data, data$age >= 5)
data$age <- trunc(data$age) # keeps only integers

## Clean age and gender variables
data <- clean_anthro(data, var = "age")
data <- clean_anthro(data, var = "sex")

## Keep data for which no missing values for age_clean, sex (clean) and only population >5 years
data <- subset(data, data$age_clean >= 5 & !is.na(data$age_clean) & !is.na(data$sex))

## Drop columns that are not needed ##
anthro_var <- c('id_study', 'sex', 'age','age_mean', 'agemin', 'agemax', 'age_group',
                'psu','stratum', 'samplewt_anthro',
                'urban_rural', 'is_urban',
                'age_min_anthro_F', 'age_min_anthro_M', 'age_max_anthro_F', 'age_max_anthro_M', 'age_clean',
                'bmi', 'height', 'weight', 'waist', 'hip', 'whr', 'whtr', 'is_pregnant')
data <- data[, intersect(anthro_var, names(data))]

## Update bmi, waist-to-hip ratio (whr), and waist-to-height ratio (whtr) variables (re-calculate them when missing, eg. bmi from measured weight and height)
if (!"bmi" %in% names(data)) data$bmi <- NA
data$bmi <- ifelse(is.na(data$bmi), data$weight/((data$height/100)^2), data$bmi)
if (!"whr" %in% names(data)) data$whr <- NA
data$whr <- ifelse(is.na(data$whr), data$waist/data$hip, data$whr)
if (!"whtr" %in% names(data)) data$whtr <- NA
data$whtr <- ifelse(is.na(data$whtr), data$waist/data$height, data$whtr)

for (var in c("pregnant", "weight", "height", "waist", "hip", "whr", "whtr", "bmi")) { # make sure pregnant goes first, and BMI last!
    if (var != "pregnant" & !var %in% names(data)) data[var] <- NA
    data <- clean_anthro(data, var)     # creating var_clean variables
}
## Check summaries ##
summary(data$bmi);    summary(data$bmi_clean)
summary(data$height); summary(data$height_clean)
summary(data$weight); summary(data$weight_clean)
summary(data$hip);    summary(data$hip_clean)
summary(data$waist);  summary(data$waist_clean)
summary(data$whr);    summary(data$whr_clean)
summary(data$whtr);   summary(data$whtr_clean)

## Keep data for which anthro data
data <- subset(data, !is.na(data$bmi_clean) | !is.na(data$weight_clean) | !is.na(data$height_clean) |
                     !is.na(data$hip_clean) | !is.na(data$waist_clean)  | !is.na(data$whr_clean)    | !is.na(data$whtr_clean))

## Create age groups
data <- create_age_groups(data)

## Remove subjects with NA values in problematic studies (see function information)
data <- clean_svydesign(data) # Do not forget to add rownames to data (see above)

## URBAN_RURAL CLEANING ##

## Clean urban_rural variable
data$urban_rural <- as.character(data$urban_rural)
data$is_urban <- as.character(data$is_urban)
if(any(!data$urban_rural %in% c('both','urban','rural'))) {
    stop ('Recode variable urban_rural: there are values other than urban/rural/both')
}
if(any(!(data$is_urban%in%c(0,1, NA)))) {
    stop ('Recode variable is_urban: there are values other than 0/1/NA')
}

# ## Urban only / rural only studies - set is_urban to corresponding values
data$is_urban[data$urban_rural == "urban"]  <- 1
data$is_urban[data$urban_rural == "rural"]  <- 0


################################################################################
########################## CREATE SUMMARIES  ###################################
################################################################################

################################# BMI #################################
bmi_data <- subset(data, !is.na(data$bmi_clean)) # remove subjects without BMI data
bmi_data <- get_bmi_prev(bmi_data)
bmi_data <- bmi_data[ ,c("id_study", "sex", "age_mean", "age_group", "is_urban", "psu", "stratum",
                         "samplewt_anthro", "bmi_clean", "weight_clean" ,
                         "prev_bmi_1sd", "prev_bmi_2sd", "prev_bmi_neg1sd",
                         "prev_bmi_neg2sd", "prev_bmi12", "prev_bmi12_15",
                         "prev_bmi15_185","prev_bmi185_20","prev_bmi20_25",
                         "prev_bmi25_30", "prev_bmi30_35", "prev_bmi35_40",
                         "prev_bmi40", "prev_bmi25", "prev_bmi30", "prev_bmi22",
                         "prev_bmi23", "prev_bmi24","prev_bmi27", "prev_bmi275",
                         "prev_bmi28", "prev_bmi_185_25", "prev_bmi_l185",
                         "prev_bmi_l16", "prev_bmi_16_17", "prev_bmi_l17",
                         "prev_bmi_l25", "prev_bmi_30_40", "prev_bmi_17_185")]

bmi_data$stratum <- as.numeric(as.factor(bmi_data$stratum))

age_group_link    <- unique(bmi_data[,c("id_study","sex","age_mean","age_group")])
age_group_link_ur <- unique(bmi_data[,c("id_study","is_urban","sex","age_mean","age_group")])

## Get summaries
summary0 <- ddply(bmi_data, .(id_study, sex, age_mean), .fun=get_summary)
summary_bmi <- summary0[with(summary0, order(id_study,sex,age_mean)), ]
# Link with age group
summary_bmi <- merge(summary_bmi, age_group_link)

# Urban_rural stratified summaries
summary0 <- ddply(bmi_data, .(id_study, is_urban, sex, age_mean), .fun=get_summary)
summary_bmi_ur <- summary0[with(summary0, order(id_study,is_urban,sex,age_mean)), ]
# Link with age group
summary_bmi_ur <- merge(summary_bmi_ur, age_group_link_ur)


################################# HEIGHT #################################
height_data <- subset(data, !is.na(data$height_clean)) # remove NA values
rm(list = c("bmi_data")) # free up memory

height_data <- get_height_prev(height_data)

prev_by5 <- seq(140, 200, 5) # for colnames (see below)
prev_names <- c("prev_height120", "prev_height120_140", "prev_height200",
               paste("prev_height", paste(prev_by5[1:length(prev_by5) - 1],
                                          prev_by5[2:length(prev_by5)], sep = "_"), sep = ""))
height_data <- height_data[ ,c("id_study","sex","age_mean", "age_group", "is_urban", "psu","stratum",
                              "samplewt_anthro", "height_clean", sort(prev_names))]
height_data$stratum <- as.numeric(as.factor(height_data$stratum))

age_group_link    <- unique(height_data[,c("id_study","sex","age_mean","age_group")])
age_group_link_ur <- unique(height_data[,c("id_study","is_urban","sex","age_mean","age_group")])

## Get summaries
summary0 <- ddply(height_data, .(id_study, sex, age_mean), .fun=get_summary)
summary_height <- summary0[with(summary0, order(id_study,sex,age_mean)), ]
# Link with age group
summary_height <- merge(summary_height, age_group_link)

# Urban_rural stratified summaries
summary0 <- ddply(height_data, .(id_study, is_urban, sex, age_mean), .fun=get_summary)
summary_height_ur <- summary0[with(summary0, order(id_study,is_urban,sex,age_mean)), ]
# Link with age group
summary_height_ur <- merge(summary_height_ur, age_group_link_ur)


################################# Waist/Hip #################################
waist_hip_data <- subset(data, !is.na(data$waist_clean) | !is.na(data$hip_clean) | !is.na(data$whr_clean) | !is.na(data$whtr_clean)) # remove NA values
rm(list = c("height_data")) # free up memory

waist_hip_data <- waist_hip_data[ ,c("id_study","sex","age_mean", "age_group", "is_urban", "psu","stratum",
                                     "samplewt_anthro", "waist_clean", "hip_clean", "whr_clean", "whtr_clean")]
waist_hip_data$stratum <- as.numeric(as.factor(waist_hip_data$stratum))

age_group_link    <- unique(waist_hip_data[,c("id_study","sex","age_mean","age_group")])
age_group_link_ur <- unique(waist_hip_data[,c("id_study","is_urban","sex","age_mean","age_group")])

## Get summaries
summary0 <- ddply(waist_hip_data, .(id_study, sex, age_mean), .fun=get_summary)
summary_waist_hip <- summary0[with(summary0, order(id_study,sex,age_mean)), ]
# Link with age group
summary_waist_hip <- merge(summary_waist_hip, age_group_link)

# Urban_rural stratified summaries
summary0 <- ddply(waist_hip_data, .(id_study, is_urban, sex, age_mean), .fun=get_summary)
summary_waist_hip_ur <- summary0[with(summary0, order(id_study,is_urban,sex,age_mean)), ]
# Link with age group
summary_waist_hip_ur <- merge(summary_waist_hip_ur, age_group_link_ur)


################################################################################
########################## SAVE DATASETS  ######################################
################################################################################

write.csv(summary_height,file=paste0("Height_summary_ALL_",date,".csv"),row.names=FALSE)
write.csv(summary_bmi,file=paste0("BMI_summary_ALL_",date,".csv"),row.names=FALSE)
write.csv(summary_waist_hip,file=paste0("Waist_hip_summary_ALL_",date,".csv"),row.names=FALSE)

write.csv(summary_height_ur,file=paste0("Height_summary_ALL_URstratified_",date,".csv"),row.names=FALSE)
write.csv(summary_bmi_ur,file=paste0("BMI_summary_ALL_URstratified_",date,".csv"),row.names=FALSE)
write.csv(summary_waist_hip_ur,file=paste0("Waist_hip_summary_ALL_URstratified_",date,".csv"),row.names=FALSE)
