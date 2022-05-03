## NCD-RisC
## v1 (20210514)
## Functions for cleaning and summarising anthro data

## clean_anthro ##
## Cleans variables ("var") according to the criteria specified within the function
## Anthroprometric variables are cleaned using age-specific plausible ranges
clean_anthro <- function(data, var, bmi_only_surveyids = NULL) {

    if (var == "sex") {
        # If sex is not 1 or 2, set it as NA (preexisting NA values, remain NA)
        message("Cleaning sex variable")
        print(paste("Number of subjects recoded as NA:", length(which(!data$sex %in% c(1, 2)))))
        data$sex[which(!data$sex %in% c(1, 2))] = NA
    }
    if (var == "age") {
        # If age is not included within the range [age_min_anthro, age_max_anthro], set
        # age to NA
        message("Cleaning age variable")
        data$age_clean <- ifelse((data$age < data$age_min_anthro_F | data$age > data$age_max_anthro_F) &
            data$sex == 2, NA, ifelse((data$age < data$age_min_anthro_M | data$age >
            data$age_max_anthro_M) & data$sex == 1, NA, data$age))
        print(paste("Number of subjects recoded as NA:", sum(is.na(data$age_clean)) - sum(is.na(data$age))))

    }
    if (var == "pregnant") {
        if (!"is_pregnant" %in% names(data)) {
          message("No pregnancy variable")
        } else {
          message("Cleaning pregnant")
          # Pregnant for men (=1) spread across surveys --> all men & preg are considered as men
          preg_old <- data$is_pregnant
          data$is_pregnant <- ifelse(data$is_pregnant == 1 & data$sex == 2 & (data$age_clean >= 10 & data$age_clean <= 49),
                                     1, 0) # Only 1 if female, and pregnant, and age [10-49] # Males, or too young/too old females are set to zero.
          data$is_pregnant <- ifelse(is.na(data$is_pregnant), 0, data$is_pregnant) # if it's NA, set to 0. This was updated Dec-18
          print(paste("Number of subjects recoded as 0:", sum(data$is_pregnant == 0) - length(which(preg_old == 0))))
        }
    }
    if (var == "height") {
        message("Cleaning height variable")
        # Clean height according to age group #
        data$height_clean <- data$height
        data[which((data$age_clean >= 5 & data$age_clean < 10) & (data$height < 60 | data$height > 180)), "height_clean"] <- NA
        data[which((data$age_clean >= 10 & data$age_clean < 15) & (data$height < 80 | data$height > 200)), "height_clean"] <- NA
        data[which((data$age_clean >= 15) & (data$height < 100 | data$height > 250)), "height_clean"] <- NA
        print(paste("Number of subjects recoded as NA:", sum(is.na(data$height_clean)) - sum(is.na(data$height))))

    }
    if (var == "weight") {
        message("Cleaning weight variable")
        # Clean weight according to age group #
        data$weight_clean <- data$weight
        data[which((data$age_clean >= 5 & data$age_clean < 10) & (data$weight < 5 | data$weight > 90)), "weight_clean"] <- NA
        data[which((data$age_clean >= 10 & data$age_clean < 15) & (data$weight < 8 | data$weight > 150)), "weight_clean"] <- NA
        data[which((data$age_clean >= 15) & (data$weight < 12 | data$weight > 300)), "weight_clean"] <- NA
        data$weight_clean[which(data$is_pregnant == 1)] <- NA # clean for pregnant
        print(paste("Number of subjects recoded as NA:", sum(is.na(data$weight_clean)) - sum(is.na(data$weight))))
    }
    if (var == "waist") {
        message("Cleaning waist variable")
        data$waist_clean <- data$waist
        data[which((data$age_clean >= 5 & data$age_clean < 10) & (data$waist < 20 | data$waist > 150)), "waist_clean"] <- NA
        data[which((data$age_clean >= 10 & data$age_clean < 15) & (data$waist < 20 | data$waist > 200)), "waist_clean"] <- NA
        data[which((data$age_clean >= 15) & (data$waist < 30 | data$waist > 300)), "waist_clean"] <- NA
        data$waist_clean[which(data$is_pregnant == 1)] <- NA  # clean for pregnant
        print(paste("Number of subjects recoded as NA:", sum(is.na(data$waist_clean)) - sum(is.na(data$waist))))
    }
    if (var == "hip") {
        message("Cleaning hip variable")
        data$hip_clean <- data$hip
        data[which((data$age_clean >= 5 & data$age_clean < 10) & (data$hip < 30 | data$hip > 180)), "hip_clean"] <- NA
        data[which((data$age_clean >= 10 & data$age_clean < 15) & (data$hip < 30 | data$hip > 200)), "hip_clean"] <- NA
        data[which((data$age_clean >= 15) & (data$hip < 40 | data$hip > 300)), "hip_clean"] <- NA
        data$hip_clean[which(data$is_pregnant == 1)] <- NA  # clean for pregnant
        print(paste("Number of subjects recoded as NA:", sum(is.na(data$hip_clean)) - sum(is.na(data$hip))))
    }
    if (var == "whr") {
        message("Cleaning waist-hip-ratio variable")
        data$whr_clean <- ifelse(is.na(data$waist_clean) | is.na(data$hip_clean), NA, data$whr)
        data[which((data$age_clean >= 5 & data$age_clean < 15) & (data$whr_clean < 0.4 | data$whr_clean > 1.8)), "whr_clean"] <- NA
        data[which((data$age_clean >= 15) & (data$whr_clean < 0.4 | data$whr_clean > 2.0)), "whr_clean"] <- NA
        data$whr_clean[which(data$is_pregnant == 1)] <- NA  # clean for pregnant
        print(paste("Number of subjects recoded as NA:", sum(is.na(data$whr_clean)) - sum(is.na(data$whr))))
    }
    if (var == "whtr") {   # Bin added WHtR
      message("Cleaning waist-to-height-ratio variable")
      data$whtr_clean <- ifelse(is.na(data$waist_clean) | is.na(data$height_clean), NA, data$whtr)
      data[which((data$age_clean >= 5 & data$age_clean < 15) & (data$whtr_clean < 0.2 | data$whtr_clean > 1.5)), "whtr_clean"] <- NA
      data[which((data$age_clean >= 15) & (data$whtr_clean < 0.2 | data$whtr_clean > 2.0)), "whtr_clean"] <- NA
      data$whtr_clean[which(data$is_pregnant == 1)] <- NA  # clean for pregnant
      print(paste("Number of subjects recoded as NA:", sum(is.na(data$whtr_clean)) - sum(is.na(data$whtr))))
    }
    if (var == "bmi") {
        # Clean BMI according to age group #
        message("Cleaning BMI variable")
        data$bmi_clean <- data$bmi
        data[which((data$age_clean >= 5 & data$age_clean < 10) & (data$bmi < 6 | data$bmi > 40)), "bmi_clean"] <- NA
        data[which((data$age_clean >= 10 & data$age_clean < 15) & (data$bmi < 8 | data$bmi > 60)), "bmi_clean"] <- NA
        data[which((data$age_clean >= 15) & (data$bmi < 10 | data$bmi > 80)), "bmi_clean"] <- NA
        data$bmi_clean[which(data$is_pregnant == 1)] <- NA  # clean for pregnant
        ## Reclean for height and weight
        if (length(setdiff(c("height_clean", "weight_clean"), names(data)) > 0)) { #
            stop("Variables height_clean or weight_clean are missing")
        }
        if (!is.null(bmi_only_surveyids)) {
            data$bmi_clean[data$id_study %in% bmi_only_surveyids] <- data$bmi[data$id_study %in% bmi_only_surveyids]
            data$bmi_clean <- ifelse((is.na(data$height_clean) | is.na(data$weight_clean)) &
                                         (!data$id_study %in% bmi_only_surveyids), NA, data$bmi_clean)
        } else {
            data$bmi_clean <- ifelse(is.na(data$height_clean) | is.na(data$weight_clean), NA, data$bmi_clean)
        }
        print(paste("Number of subjects recoded as NA:", sum(is.na(data$bmi_clean)) - sum(is.na(data$bmi))))
    }

    return(data)
}
################################################################################

## create_age_groups ##
# agemin: if age_min_anthro and age_max_anthro are within a 10-year band agemin = age_min_anthro, else agemin = floor(age_clean).
# Exceptions: if age_clean < 20, agemin = age_clean; if age_clean > 80, agemin = 80
# agemax: if age_min_anthro and age_max_anthro are within a 10-year band agemin = age_max_anthro, else agemax = floor(age_clean) + 9.
# Exceptions: if age_clean < 20, agemax = age_clean
create_age_groups = function(data) {

    data$agemin <- ifelse(data$age_clean < 20, data$age_clean, ifelse(data$age_clean >=
        20 & data$age_clean < 30 & data$age_min_anthro_F >= 20 & data$age_min_anthro_F <
        30 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age_clean >= 30 &
        data$age_clean < 40 & data$age_min_anthro_F >= 30 & data$age_min_anthro_F <
        40 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age_clean >= 40 &
        data$age_clean < 50 & data$age_min_anthro_F >= 40 & data$age_min_anthro_F <
        50 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age_clean >= 50 &
        data$age_clean < 60 & data$age_min_anthro_F >= 50 & data$age_min_anthro_F <
        60 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age_clean >= 60 &
        data$age_clean < 70 & data$age_min_anthro_F >= 60 & data$age_min_anthro_F <
        70 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age_clean >= 70 &
        data$age_clean < 80 & data$age_min_anthro_F >= 70 & data$age_min_anthro_F <
        80 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age_clean >= 80 &
        data$sex == 2, 80, ifelse(data$age_clean >= 20 & data$age_clean < 30 & data$age_min_anthro_M >=
        20 & data$age_min_anthro_M < 30 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age_clean >=
        30 & data$age_clean < 40 & data$age_min_anthro_M >= 30 & data$age_min_anthro_M <
        40 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age_clean >= 40 &
        data$age_clean < 50 & data$age_min_anthro_M >= 40 & data$age_min_anthro_M <
        50 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age_clean >= 50 &
        data$age_clean < 60 & data$age_min_anthro_M >= 50 & data$age_min_anthro_M <
        60 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age_clean >= 60 &
        data$age_clean < 70 & data$age_min_anthro_M >= 60 & data$age_min_anthro_M <
        70 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age_clean >= 70 &
        data$age_clean < 80 & data$age_min_anthro_M >= 70 & data$age_min_anthro_M <
        80 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age_clean >= 80 &
        data$sex == 1, 80, floor(data$age_clean/10) * 10)))))))))))))))

    data$agemax <- ifelse(data$age_clean < 20, data$age_clean, ifelse(data$age_clean >=
        20 & data$age_clean < 30 & data$age_max_anthro_F >= 20 & data$age_max_anthro_F <
        30 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age_clean >= 30 &
        data$age_clean < 40 & data$age_max_anthro_F >= 30 & data$age_max_anthro_F <
        40 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age_clean >= 40 &
        data$age_clean < 50 & data$age_max_anthro_F >= 40 & data$age_max_anthro_F <
        50 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age_clean >= 50 &
        data$age_clean < 60 & data$age_max_anthro_F >= 50 & data$age_max_anthro_F <
        60 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age_clean >= 60 &
        data$age_clean < 70 & data$age_max_anthro_F >= 60 & data$age_max_anthro_F <
        70 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age_clean >= 70 &
        data$age_clean < 80 & data$age_max_anthro_F >= 70 & data$age_max_anthro_F <
        80 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age_clean >= 80 &
        data$sex == 2, data$age_max_anthro_F, ifelse(data$age_clean >= 20 & data$age_clean <
        30 & data$age_max_anthro_M >= 20 & data$age_max_anthro_M < 30 & data$sex ==
        1, data$age_max_anthro_M, ifelse(data$age_clean >= 30 & data$age_clean <
        40 & data$age_max_anthro_M >= 30 & data$age_max_anthro_M < 40 & data$sex ==
        1, data$age_max_anthro_M, ifelse(data$age_clean >= 40 & data$age_clean <
        50 & data$age_max_anthro_M >= 40 & data$age_max_anthro_M < 50 & data$sex ==
        1, data$age_max_anthro_M, ifelse(data$age_clean >= 50 & data$age_clean <
        60 & data$age_max_anthro_M >= 50 & data$age_max_anthro_M < 60 & data$sex ==
        1, data$age_max_anthro_M, ifelse(data$age_clean >= 60 & data$age_clean <
        70 & data$age_max_anthro_M >= 60 & data$age_max_anthro_M < 70 & data$sex ==
        1, data$age_max_anthro_M, ifelse(data$age_clean >= 70 & data$age_clean <
        80 & data$age_max_anthro_M >= 70 & data$age_max_anthro_M < 80 & data$sex ==
        1, data$age_max_anthro_M, ifelse(data$age_clean >= 80 & data$sex == 1, data$age_max_anthro_M,
        9 + floor(data$age_clean/10) * 10)))))))))))))))

    data$age_group <- paste(data$agemin, "-", data$agemax, sep = "")

    data$age_mean <- ifelse(data$agemax - data$agemin == 0, data$agemax, ifelse(data$agemax ==
        200, 84.91, data$agemin + (data$agemax + 1 - data$agemin)/2))

    return(data)

}
################################################################################

## svy_check ##
## Checks whether in tmp[, "var"] there is a mixed of subjects with reported values
# and subjects with missing values for "var"
svy_check <- function(tmp, var = "samplewt_anthro") {

    if (any(is.na(tmp[, var])) & !all(is.na(tmp[, var]))) {

        if (is.numeric(tmp[, var]) & var == "samplewt_anthro") {
            ans <- c(sum(is.na(tmp[, var])),
                     round(100*sum(is.na(tmp[, var]))/nrow(tmp), 2),
                     range(na.omit(tmp[, var])),
                     paste(tmp$names[(is.na(tmp[, var]))], collapse = "//"))
            names(ans) <- c("N_NA","%_NA", "min_sw", "max_sw", "NA_idx")
            return(ans)

        } else {
            ans <- c(sum(is.na(tmp[, var])),
                     round(100*sum(is.na(tmp[, var]))/nrow(tmp), 2),
                     paste(tmp$names[(is.na(tmp[, var]))], collapse = "//"))
            names(ans) <- c("N_NA", "%_NA", "NA_idx")
            return(ans)
        }
    } else {
        return(NULL)
    }
}
################################################################################

## clean_svydesign ##
## This function stratifies the data based in: id_study, or id_study//age_mean//sex, or id_study//age_mean//sex//is_urban;
# It then checks whether within a given study there is a mix of subjects with & without (NA) values
# for any survey design variable: psu, stratum, sample weight (each variable is explored separately).
# These studies where there is a mixed of subjects with & without (i.e. NA) values for
# the survey design variables are problematic because the svydesign() function can't deal
# with NA values. Therefore, subjects with NA values within these "problematic studies" must be removed.
clean_svydesign <- function (data, by = "survey") {

    if(grepl("S", rownames(data))[1] == FALSE) { # Make sure we have rownames
        rownames(data) <- paste("S", 1:nrow(data), sep = "")
        data$names <- rownames(data)
    }

    if (by == "age_gender") {
        sw_prob <- ddply(data,.(id_study, age_mean, sex), function(tmp) svy_check(tmp))
        psu_prob <- ddply(data,.(id_study, age_mean, sex), function(tmp) svy_check(tmp, var = "psu"))
        stratum_prob <- ddply(data,.(id_study, age_mean, sex), function(tmp) svy_check(tmp, var = "stratum"))

    } else if (by == "urban"){
        sw_prob <- ddply(data,.(id_study, age_mean, sex, is_urban), function(tmp) svy_check(tmp))
        psu_prob <- ddply(data,.(id_study, age_mean, sex, is_urban), function(tmp) svy_check(tmp, var = "psu"))
        stratum_prob <- ddply(data,.(id_study, age_mean, sex, is_urban), function(tmp) svy_check(tmp, var = "stratum"))

    } else { # Do not stratifiy
        sw_prob <- ddply(data,.(id_study), function(tmp) svy_check(tmp))
        psu_prob <- ddply(data,.(id_study), function(tmp) svy_check(tmp, var = "psu"))
        stratum_prob <- ddply(data,.(id_study), function(tmp) svy_check(tmp, var = "stratum"))
    }

    if (ncol(sw_prob) > 4) {
        sw_prob <- sw_prob[, setdiff(colnames(sw_prob), c("min_sw", "max_sw"))]
    }

    all_prob <- rbind(sw_prob, psu_prob, stratum_prob)

    if (nrow(all_prob) == 0) {
        message("None of the subjects was dropped due to survey design")
        return(data)
    }
    indx <- unlist(strsplit(all_prob$NA_idx, "//"))
    if (sum(as.numeric(all_prob$N_NA)) != length(indx)) {
        stop ("Problem in code")
    }
    indx_wanted <- setdiff(rownames(data), indx)

    to_print <- paste("Number of subjects removed due to survey design:", length(indx))
    data <- data[indx_wanted, ]

    return(data)

}
################################################################################

## bmi_adol ##
# Gets prevalence of bmi for a given bmi category (bmi_cat), and for a given age_sex group (bmi_line)
bmi_adol <- function(bmi_line, bmi_cat, bmi_data) {
    if (grepl("neg", bmi_cat)) {
        res <- which(bmi_data$age_mean == bmi_line$age & bmi_data$sex == bmi_line$sex &
                         bmi_data$bmi_clean < bmi_line[, bmi_cat])
    } else {
        res <- which(bmi_data$age_mean == bmi_line$age & bmi_data$sex == bmi_line$sex &
                         bmi_data$bmi_clean > bmi_line[, bmi_cat])
    }
    return(res)
}

## get bmi_prev ##
# Gives 1 if an individual falls in a given BMI category, else 0 (subjects with NA values in height_data were removed)
get_bmi_prev = function(bmi_data) {

    # prevalences
    bmi_data$prev_bmi12 <- ifelse(bmi_data$bmi_clean < 12, 1,0)
    bmi_data$prev_bmi12_15 <- ifelse(bmi_data$bmi_clean >= 12 & bmi_data$bmi_clean < 15, 1, 0)
    bmi_data$prev_bmi15_185 <- ifelse(bmi_data$bmi_clean >= 15 & bmi_data$bmi_clean < 18.5, 1, 0)
    bmi_data$prev_bmi185_20 <- ifelse(bmi_data$bmi_clean >= 18.5 & bmi_data$bmi_clean < 20, 1, 0)
    bmi_data$prev_bmi20_25 <- ifelse(bmi_data$bmi_clean >= 20 & bmi_data$bmi_clean < 25, 1, 0)
    bmi_data$prev_bmi25_30 <- ifelse(bmi_data$bmi_clean >= 25 & bmi_data$bmi_clean < 30, 1, 0)
    bmi_data$prev_bmi30_35 <- ifelse(bmi_data$bmi_clean >= 30 & bmi_data$bmi_clean < 35, 1, 0)
    bmi_data$prev_bmi35_40 <- ifelse(bmi_data$bmi_clean >= 35 & bmi_data$bmi_clean < 40, 1, 0)
    bmi_data$prev_bmi40 <- ifelse(bmi_data$bmi_clean >= 40, 1, 0)
    bmi_data$prev_bmi25 <- ifelse(bmi_data$bmi_clean >= 25, 1, 0)
    bmi_data$prev_bmi30 <- ifelse(bmi_data$bmi_clean >= 30, 1, 0)
    bmi_data$prev_bmi22 <- ifelse(bmi_data$bmi_clean >= 22, 1, 0)
    bmi_data$prev_bmi23 <- ifelse(bmi_data$bmi_clean >= 23, 1, 0)
    bmi_data$prev_bmi24 <- ifelse(bmi_data$bmi_clean >= 24, 1, 0)
    bmi_data$prev_bmi27 <- ifelse(bmi_data$bmi_clean >= 27, 1, 0)
    bmi_data$prev_bmi275 <- ifelse(bmi_data$bmi_clean >= 27.5, 1, 0)
    bmi_data$prev_bmi28 <- ifelse(bmi_data$bmi_clean >= 28, 1, 0)
    bmi_data$prev_bmi_185_25 <- ifelse(bmi_data$bmi_clean < 25 & bmi_data$bmi_clean >= 18.5, 1, 0)
    bmi_data$prev_bmi_l185 <- ifelse(bmi_data$bmi_clean < 18.5, 1, 0)
    bmi_data$prev_bmi_l16 <- ifelse(bmi_data$bmi_clean < 16, 1, 0)
    bmi_data$prev_bmi_16_17 <- ifelse(bmi_data$bmi_clean < 17 & bmi_data$bmi_clean >= 16, 1, 0)
    bmi_data$prev_bmi_l17 <- ifelse(bmi_data$bmi_clean < 17, 1, 0)
    bmi_data$prev_bmi_l25 <- ifelse(bmi_data$bmi_clean < 25, 1, 0)
    bmi_data$prev_bmi_30_40 <- ifelse(bmi_data$bmi_clean < 40 & bmi_data$bmi_clean >= 30, 1, 0)
    bmi_data$prev_bmi_17_185 <- ifelse(bmi_data$bmi_clean < 18.5 & bmi_data$bmi_clean >= 17, 1,0)

    ## Adolescent cut-offs
    bmi_cutoff <- read.csv("utilities/child_adolescent_bmi_cutoffs.csv")
    bmi_rows <- split(bmi_cutoff, f = seq(nrow(bmi_cutoff)))

    idx_neg2sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_neg2sd", bmi_data = bmi_data))
    idx_neg1sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_neg1sd", bmi_data = bmi_data))
    idx_sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_sd", bmi_data = bmi_data))
    idx_2sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_2sd", bmi_data = bmi_data))

    bmi_data$prev_bmi_neg2sd <- 0
    bmi_data$prev_bmi_neg2sd[idx_neg2sd] <- 1 # Works even in idx_neg2sd = integer(0)

    bmi_data$prev_bmi_neg1sd <- 0
    bmi_data$prev_bmi_neg1sd[idx_neg1sd] <- 1

    bmi_data$prev_bmi_1sd <- 0
    bmi_data$prev_bmi_1sd[idx_sd] <- 1

    bmi_data$prev_bmi_2sd <- 0
    bmi_data$prev_bmi_2sd[idx_2sd] <- 1

    return(bmi_data)
}
################################################################################

## get_height_prev ##
# Gives 1 if an individual falls in a given height category, else 0 (subjects with NA values in height_data were removed)
get_height_prev = function(height_data) {

    height_data$prev_height120 <- ifelse(height_data$height_clean < 120, 1, 0)
    height_data$prev_height120_140 <- ifelse(height_data$height_clean >= 120 & height_data$height_clean < 140, 1, 0)
    height_data$prev_height140_145 <- ifelse(height_data$height_clean >= 140 & height_data$height_clean < 145, 1, 0)
    height_data$prev_height145_150 <- ifelse(height_data$height_clean >= 145 & height_data$height_clean < 150, 1, 0)
    height_data$prev_height150_155 <- ifelse(height_data$height_clean >= 150 & height_data$height_clean < 155, 1, 0)
    height_data$prev_height155_160 <- ifelse(height_data$height_clean >= 155 & height_data$height_clean < 160, 1, 0)
    height_data$prev_height160_165 <- ifelse(height_data$height_clean >= 160 & height_data$height_clean < 165, 1, 0)
    height_data$prev_height165_170 <- ifelse(height_data$height_clean >= 165 & height_data$height_clean < 170, 1, 0)
    height_data$prev_height170_175 <- ifelse(height_data$height_clean >= 170 & height_data$height_clean < 175, 1, 0)
    height_data$prev_height175_180 <- ifelse(height_data$height_clean >= 175 & height_data$height_clean < 180, 1, 0)
    height_data$prev_height180_185 <- ifelse(height_data$height_clean >= 180 & height_data$height_clean < 185, 1, 0)
    height_data$prev_height185_190 <- ifelse(height_data$height_clean >= 185 & height_data$height_clean < 190, 1, 0)
    height_data$prev_height190_195 <- ifelse(height_data$height_clean >= 190 & height_data$height_clean < 195, 1, 0)
    height_data$prev_height195_200 <- ifelse(height_data$height_clean >= 195 & height_data$height_clean < 200, 1, 0)
    height_data$prev_height200 <- ifelse(height_data$height_clean >= 200, 1, 0)

    return(height_data)
}
################################################################################

## get_summary_bmi ##
# For each study (id_study//sex//age) it calculates the mean of bmi (as continuous variable)
# and the mean of each prevalence group, across all subjects included within that
# study. This means that the SE for prevalences, are calcualted as for means

get_summary <- function(tmp, study = NULL) { #

    if (any(grepl("prev_bmi", colnames(tmp)))) {
        vars <- "bmi"
    } else if (any(grepl("height", colnames(tmp)))) {
        vars <- "height"
    } else if (any(grepl("weight", colnames(tmp)))) {
        vars <- "weight"
    } else {
        vars <- c("waist", "hip", "whr", "whtr")   # Bin added WHtR
    }
    # print(paste(tmp$id_study[1], tmp$sex[1], tmp$age_mean[1]))

    ## Set options ##
    options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)

    ## Check survey design variables ##
    if (any(is.na(tmp$samplewt_anthro))) { # If sample weights are missing set them to 1 (NA values not allowed in svydesign())
        if (all(is.na(tmp$samplewt_anthro)) == FALSE) { # Sample weights should be all NA, or all not NA
            stop ("Sample weights are missing ONLY in some subjects")
        }
        tmp$samplewt_anthro <- 1
        res_wt <- TRUE #
    }
    if (any(is.na(tmp$psu))) {
        if (all(is.na(tmp$psu)) == FALSE) { # psu should be all NA, or all not NA
            stop ("psu values are missing ONLY in some subjects")
        }
        res_psu <- FALSE # psu missing
    } else {
        res_psu <- TRUE # psu available
    }
    if (any(is.na(tmp$stratum))) {
        if (all(is.na(tmp$stratum)) == FALSE) { # stratum should be all NA, or all not NA
            stop ("stratum values are missing ONLY in some subjects")
        }
        res_stratum <- FALSE # stratum missing
    } else {
        res_stratum <- TRUE # stratum available
    }

    ## Clean here for single_psu_ssa
    # Number of different psu (excluding NAs). If the study has only one psu and
    # stratum is not reported, it should be dropped. Else the function svydesign gives an error
    if (length(unique(na.omit(tmp$psu))) == 1) {
        res_psu <- FALSE # psu not available
        tmp$psu <- NA
        message("Study psu updated to NA because of single psu")
    }
    num.valid <- function(x) sum(!is.na(x) & x != -1) # sums values which are different from NA or -1 (valid subjects)

    results <- NULL
    for (var in vars) {
      var_clean <- paste(var, "clean", sep = "_")
      n_var <- num.valid(tmp[, var_clean])

      if (n_var > 1) {

          if (sum(res_psu, res_stratum) == 0) { # psu and stratum missing (if sw are not available, they're all 1)
              dsub <- svydesign(id = ~1, strata = NULL, weights = ~samplewt_anthro, data = tmp)
              # print("id = ~1, strata = NULL, weights = ~samplewt_anthro")
          } else if ((sum(res_psu, res_stratum) == 2)) { # we have psu and stratum
              dsub <- svydesign(id = ~psu, strata = ~stratum, weights = ~samplewt_anthro, data = tmp, nest = TRUE)
              # print("id = ~psu, strata = ~stratum, weights = ~samplewt_anthro")
          } else if (res_psu & !res_stratum) {# we have psu but not stratum
              dsub <- svydesign(id = ~psu, strata = NULL, weights = ~samplewt_anthro, data = tmp, nest = TRUE)
              # print("id = ~psu, strata = ~NULL, weights = ~samplewt_anthro")
          } else if (!res_psu & res_stratum) {
              dsub <- svydesign(id = ~1, strata = ~stratum, weights = ~samplewt_anthro, data = tmp, nest = TRUE)
              # print("id = ~1, strata = ~stratum, weights = ~samplewt_anthro")
          }

          var_m <- data.frame(svymean(~tmp[, var_clean], dsub, na.rm = TRUE))  # gives mean and standard error
          var_sd <- as.numeric(sqrt(coef(svyvar(~tmp[, var_clean], dsub, na.rm = TRUE)))) # gives only one value
          var_cols <- paste(c("N", "mean", "se", "sd"), var, sep = "_")

          # BMI and height also have prevalence
          if (var %in% c("bmi", "height")) {
            colnames(tmp) <- gsub(paste0(var, "_"), var, colnames(tmp))
            colnames(tmp) <- gsub(var, paste0(var, "_"), colnames(tmp))

            prev_names <- grep("prev_", colnames(tmp), value = TRUE)
            res <- as.data.frame(matrix(ncol = (length(prev_names)*2 + 4), nrow = 1)) # empty df
            prev_cols <- unlist(lapply(prev_names, function(x) c(paste("se", x, sep = "_"), x)))
            colnames(res) <- c(var_cols, prev_cols)

            res[, var_cols] <- c(n_var, var_m[1, 1], var_m[1, 2], var_sd)

            for (prev in prev_names) {
                ans <- data.frame(svymean(~tmp[, prev], dsub, na.rm = TRUE))
                res[, prev] <- ans[1, 1]
                res[, paste("se", prev, sep = "_")] <- ans[1, 2]
            }
          } else {
          # Waist does not have prevalence
            res <- as.data.frame(cbind(n_var, var_m[1, 1], var_m[1, 2], var_sd))
            colnames(res) <- var_cols
          }

      } else {
          # message(paste("Study excluded for", var, "as n <= 1"))
          res <- NULL
      }
      if (!is.null(res))
          if (is.null(results)) {
            results <- res
          } else {
            results <- data.frame(results, res)
          }
    }
    return(results)
}
################################################################################
