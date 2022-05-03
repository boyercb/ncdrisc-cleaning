## NCD-RisC
## v1 (20210514)
## Functions for cleaning and summarising BP, cholesterol, glucose data

mean_age_open_age_group <-
    data.frame(min_age  = 65:100,
               mean_age = c(73.81, 74.52, 75.23, 75.95, 76.67,
                            77.40, 78.14, 78.88, 79.62, 80.37, 81.11, 81.86, 82.61, 83.37, 84.14,
                            84.91, 85.69, 86.48, 87.27, 88.07, 88.88, 89.69, 90.50, 91.33, 92.15,
                            92.98, 93.81, 94.64, 95.47, 96.30, 97.12, 97.94, 98.75, 99.52, 100.15, 100.50))

make_age_groups <- function(age, age_design_min, age_design_max) {
    # naive grouping
    age_group0 <- cut(age, c(18,seq(20,80,by=10),200), right = FALSE)
    age_min   <- c(18, seq(20,80,by=10))[as.numeric(age_group0)]
    age_max   <- c(seq(20,80,by=10),200)[as.numeric(age_group0)]
    
    # update the first and last age group according to age range
    age_min[which(age_min<age_design_min)] <- age_design_min[which(age_min<age_design_min)]
    age_max[which(age_max>age_design_max)] <- age_design_max[which(age_max>age_design_max)] + 1
    
    # single year age groups: use nominal age rather than xx.5 as mid-age
    age_max[which(age_max - age_min == 1)] <- age_min[which(age_max - age_min == 1)]
    
    # generate age group and mean age
    age_group <- paste0(age_min, "-", age_max)
    age_group <- gsub("-200", "\\+", age_group)
    age_mean  <- rowMeans(cbind(age_min, age_max))
    # use pre-determined mean age for for open-ended age groups
    age_mean[which(age_max==200)] <- mean_age_open_age_group$mean_age[match(age_min[which(age_max==200)], mean_age_open_age_group$min_age)]
    
    return(data.frame(age_mean, age_group))
}

clean_single_psu_ssa <- function(tmp){ # ssa = study-sex-age group
    psu.count 	<- ddply(tmp[!is.na(tmp$psu),],.( id_study, age_mean, sex),function(tmp)length(unique(tmp$psu)))
    single.psu <- psu.count[psu.count$V1==1 & psu.count$id_study%in%c(as.character(psu_only),as.character(psu_wt)),] # single stratum, single psu 
    drop.singlepsu <- which(paste(tmp$id_study, tmp$age_mean, tmp$sex, sep="_")%in%paste(single.psu$id_study, single.psu$age_mean, single.psu$sex, sep="_"))
    print(length(drop.singlepsu))
    print(single.psu)
    
    if(length(drop.singlepsu)>0) {
        tmp <- tmp[-drop.singlepsu,]
    }
    return(tmp)
}

print_cleaned <- function(var) {
    print(paste("Percentage Cleaned (No. of cleaned/No. of non-NAs):",var,"(%)"))
    if (length(clnList)==0) {
        print("No records cleaned")
    } else {
        cln.table <- table(data[clnList,]$id_study)/table(data[!is.na(data[var]),]$id_study)*100
        print(sort(round(cln.table[which(cln.table!=Inf&cln.table>0)],2), decreasing=TRUE))
    }
}

make_age_group_anthro <- function(age_clean, sex, age_min_anthro_F, age_max_anthro_F, age_min_anthro_M, age_max_anthro_M) {
    agemin <-
        ifelse(age_clean<20,age_clean,
        ifelse(age_clean>=20 & age_clean<30 & age_min_anthro_F>=20 & age_min_anthro_F<30 & sex==2, age_min_anthro_F,
        ifelse(age_clean>=30 & age_clean<40 & age_min_anthro_F>=30 & age_min_anthro_F<40 & sex==2, age_min_anthro_F,
        ifelse(age_clean>=40 & age_clean<50 & age_min_anthro_F>=40 & age_min_anthro_F<50 & sex==2, age_min_anthro_F,
        ifelse(age_clean>=50 & age_clean<60 & age_min_anthro_F>=50 & age_min_anthro_F<60 & sex==2, age_min_anthro_F,
        ifelse(age_clean>=60 & age_clean<70 & age_min_anthro_F>=60 & age_min_anthro_F<70 & sex==2, age_min_anthro_F,
        ifelse(age_clean>=70 & age_clean<80 & age_min_anthro_F>=70 & age_min_anthro_F<80 & sex==2, age_min_anthro_F,
        ifelse(age_clean>=80 & sex==2, 80,
        ifelse(age_clean>=20 & age_clean<30 & age_min_anthro_M>=20 & age_min_anthro_M<30 & sex==1, age_min_anthro_M,
        ifelse(age_clean>=30 & age_clean<40 & age_min_anthro_M>=30 & age_min_anthro_M<40 & sex==1, age_min_anthro_M,
        ifelse(age_clean>=40 & age_clean<50 & age_min_anthro_M>=40 & age_min_anthro_M<50 & sex==1, age_min_anthro_M,
        ifelse(age_clean>=50 & age_clean<60 & age_min_anthro_M>=50 & age_min_anthro_M<60 & sex==1, age_min_anthro_M,
        ifelse(age_clean>=60 & age_clean<70 & age_min_anthro_M>=60 & age_min_anthro_M<70 & sex==1, age_min_anthro_M,
        ifelse(age_clean>=70 & age_clean<80 & age_min_anthro_M>=70 & age_min_anthro_M<80 & sex==1, age_min_anthro_M,
        ifelse(age_clean>=80 & sex==1, 80, floor(age_clean/10)*10)
        ))))))))))))))
    agemax <-
        ifelse(age_clean<20,age_clean,
        ifelse(age_clean>=20 & age_clean<30 & age_max_anthro_F>=20 & age_max_anthro_F<30 & sex==2, age_max_anthro_F,
        ifelse(age_clean>=30 & age_clean<40 & age_max_anthro_F>=30 & age_max_anthro_F<40 & sex==2, age_max_anthro_F,
        ifelse(age_clean>=40 & age_clean<50 & age_max_anthro_F>=40 & age_max_anthro_F<50 & sex==2, age_max_anthro_F,
        ifelse(age_clean>=50 & age_clean<60 & age_max_anthro_F>=50 & age_max_anthro_F<60 & sex==2, age_max_anthro_F,
        ifelse(age_clean>=60 & age_clean<70 & age_max_anthro_F>=60 & age_max_anthro_F<70 & sex==2, age_max_anthro_F,
        ifelse(age_clean>=70 & age_clean<80 & age_max_anthro_F>=70 & age_max_anthro_F<80 & sex==2, age_max_anthro_F,
        ifelse(age_clean>=80 & sex==2, age_max_anthro_F,   
        ifelse(age_clean>=20 & age_clean<30 & age_max_anthro_M>=20 & age_max_anthro_M<30 & sex==1, age_max_anthro_M,
        ifelse(age_clean>=30 & age_clean<40 & age_max_anthro_M>=30 & age_max_anthro_M<40 & sex==1, age_max_anthro_M,
        ifelse(age_clean>=40 & age_clean<50 & age_max_anthro_M>=40 & age_max_anthro_M<50 & sex==1, age_max_anthro_M,
        ifelse(age_clean>=50 & age_clean<60 & age_max_anthro_M>=50 & age_max_anthro_M<60 & sex==1, age_max_anthro_M,
        ifelse(age_clean>=60 & age_clean<70 & age_max_anthro_M>=60 & age_max_anthro_M<70 & sex==1, age_max_anthro_M,
        ifelse(age_clean>=70 & age_clean<80 & age_max_anthro_M>=70 & age_max_anthro_M<80 & sex==1, age_max_anthro_M,
        ifelse(age_clean>=80 & sex==1, age_max_anthro_M,9+floor(age_clean/10)*10)
        ))))))))))))))
    age_group <- paste(agemin, '-', agemax, sep="")
    age_mean  <- ifelse(agemax-agemin==0, agemax, ifelse(agemax==200, 84.91,  agemin+(agemax+1-agemin)/2))
    return(data.frame(age_mean, age_group))
}