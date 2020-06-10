library(data.table)
library(lubridate)
library(xlsx)
library(zoo)

#########################################################################
# set parameters
#########################################################################

t2_lower_thresh = 25 # set BMI lower threshold for T2          - for manual review
t2_optimise_HbA1c_thresh = 80 # set HbA1c for optimal T2       - for auto discharge/retain
t2_optimise_BMI_thresh = 40 # set BMI threshold for optimal T2 - for auto discharge/retain
t2_optimise_BMI_thresh_ins = 35 # set BMI thresh if on insulin - for auto discharge/retain
t2_max_time_from_last_review = 365.25 * 3 #                    - for auto discharge/retain
t2_n_med_thresh = 3 # number of meds to retain                 - for auto discharge/retain
t2_recent_diagnosis_threshold <- 2 # years                     - for auto discharge/retain

t1_duration_threshold = 1 # set min duration threshold for T1 (below for IR + CA)
t1_upper_thresh = 35 # set BMI upper limit for T1
t1_upper_hba1c_thresh = 110 # set HbA1c upper limit for T1
index_date <- "2020-07-01"

returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}

#########################################################################
# load/setup data
#########################################################################

# export files from flex query
ggh_1 <- read.csv("~/projects/caseLoad/ggh_export_1.csv", stringsAsFactors = FALSE, header = TRUE)
ggh_2 <- read.csv("~/projects/caseLoad/ggh_export_2.csv", stringsAsFactors = FALSE, header = TRUE)

ggh <- rbind(ggh_1, ggh_2)
# ggh = data.table(ggh)

inputFile_name <- "caseLoad_export.csv"
# inputFile_name <- "dm_caseLoad.csv"
inputFile_path <- paste0("~/projects/caseLoad/", inputFile_name)

## load and setup tables
cs_pt <- read.csv(inputFile_path, stringsAsFactors = FALSE, header = TRUE)
cs_pt <- data.table(cs_pt)
cs_pt <- cs_pt[Surname != ""]
cs_pt <- data.frame(cs_pt)

x = merge(cs_pt, ggh, by.x = "CHI", by.y = "Patient.ID.CHI", all.x = T)
x = data.table(x)
x = unique(x)
x$autocomment <- ""
x$comments <- ""

## fill date of last review entry
x$Date.of.last.annual.review..eg.Nov19. <- x$Last.consultant.led.clinic.attended.Date

## set flag reporting vector
x$flag_review_diagosis <- 0

# flag T1 with ? wrong diagn0sis
t1_sub <- x[x$Diabetes.Type == "Type 1 Diabetes Mellitus"]
t2_sub <- x[x$Diabetes.Type == "Type 2 Diabetes Mellitus"]

#########################################################################
# T1: flag
#########################################################################

# not listed as insulin
    t1_sub$containsInsulin <- 0
    t1_sub$containsInsulin[grep("insulin", t1_sub$Treatment.Type, ignore.case = TRUE)] <- 1
    
    x$flag_review_diagosis <- ifelse(x$CHI %in% t1_sub[containsInsulin == 0]$CHI == TRUE, 1, x$flag_review_diagosis)
    x$autocomment <- ifelse(x$CHI %in% t1_sub[containsInsulin == 0]$CHI == TRUE, "T1DM diagnosis, no insulin listed in therapy", x$autocomment)
    x$t1_no_insulin <- ifelse(x$CHI %in% t1_sub[containsInsulin == 0]$CHI == TRUE, 1, 0)
    
# BMI > threshold
    t1_sub$BMI_35 <- ifelse(t1_sub$BMI > t1_upper_thresh, 1, 0)
    x$flag_review_diagosis <- ifelse(x$CHI %in% t1_sub[BMI_35 == 1]$CHI == TRUE, 1, x$flag_review_diagosis)
    x$autocomment <- ifelse(x$CHI %in% t1_sub[BMI_35 == 1]$CHI == TRUE, paste0("T1DM diagnosis, BMI above threshold (", t1_upper_thresh, "kg/m^2)"), x$autocomment)
    x$t1_high_BMI <- ifelse(x$CHI %in% t1_sub[BMI_35 == 1]$CHI == TRUE, 1, 0)
    
# on DPP4, SGLT2
    t1_sub$suspect_meds_glip <- 0
    t1_sub$suspect_meds_sglt2 <- 0
    t1_sub$suspect_meds_glip[grep("Gliptin", t1_sub$Treatment.Type, ignore.case = TRUE)] <- 1
    t1_sub$suspect_meds_sglt2[grep("sglt2", t1_sub$Treatment.Type, ignore.case = TRUE)] <- 1
    t1_sub$suspect_meds <- ifelse(t1_sub$suspect_meds_glip == 1 | t1_sub$suspect_meds_sglt2 == 1, 1, 0)
    
    x$flag_review_diagosis <- ifelse(x$CHI %in% t1_sub[suspect_meds == 1]$CHI == TRUE, 1, x$flag_review_diagosis)
    x$autocomment <- ifelse(x$CHI %in% t1_sub[suspect_meds == 1]$CHI == TRUE, paste0("T1DM diagnosis, therapy listed includes DPP4, SGLT2"), x$autocomment)
    x$suspect_meds <- ifelse(x$CHI %in% t1_sub[suspect_meds == 1]$CHI == TRUE, 1, 0)

#########################################################################
# T2: flag
#########################################################################

# flag thin t2 BMI < threshold on insulin or SU
    t2_sub$thin <- ifelse(t2_sub$BMI < t2_lower_thresh, 1, 0)
    t2_sub$ins <- 0
    t2_sub$su <- 0
    t2_sub$ins[grep("insulin", t2_sub$Treatment.Type, ignore.case = TRUE)] <- 1
    t2_sub$su[grep("Sulphonylurea ", t2_sub$Treatment.Type, ignore.case = TRUE)] <- 1
    t2_sub$thin_plus_meds <- ifelse(t2_sub$thin == 1 & (t2_sub$ins == 1 | t2_sub$su == 1), 1, 0)
    
    x$flag_review_diagosis <- ifelse(x$CHI %in% t2_sub[thin_plus_meds == 1]$CHI == TRUE, 1, x$flag_review_diagosis)
    x$autocomment <- ifelse(x$CHI %in% t2_sub[thin_plus_meds == 1]$CHI == TRUE, paste0("T2DM diagnosis, BMI below threshold (", t2_lower_thresh, "kg/m^2)"), x$autocomment)
    x$t2_thin_plus_ins <- ifelse(x$CHI %in% t2_sub[thin_plus_meds == 1]$CHI == TRUE, 1, 0)
    
# autofill fields
    x$date_last_review = dmy(x$Last.consultant.led.clinic.attended.Date)
    x$diagnosis_date <- dmy(x$Date.of.Diagnosis)
    duration <- returnUnixDateTime(index_date) - returnUnixDateTime(x$diagnosis_date)
    duration_y <- duration / (60*60*24*365.25)
    duration_1y <- ifelse(duration_y < t1_duration_threshold, 1, 0)
    
# extract diagnosis
    x$diag_type <- ifelse((x$flag_review_diagosis == 0) & (x$Diabetes.Type == "Type 1 Diabetes Mellitus"), 1, 
                          ifelse((x$flag_review_diagosis == 0) & (x$Diabetes.Type == "Type 2 Diabetes Mellitus"), 2, 
                                 ifelse((x$flag_review_diagosis == 0) & (x$Diabetes.Type == "Other"), "non-1/2", 
                                        ifelse((x$flag_review_diagosis == 0) & (x$Diabetes.Type == "Diabetes in Remission"), "non-1/2", 0))))
    x$diag_type <- ifelse((x$flag_review_diagosis == 1), "", x$diag_type)
    
    x$flag_review_diagosis <- ifelse(is.na(x$diag_type) == TRUE, 1, x$flag_review_diagosis)
    x$autocomment <- ifelse(is.na(x$diag_type) == TRUE, paste0("missing diagnostic information"), x$autocomment)
    x$diag_type[is.na(x$diag_type)] <- ""
    
#########################################################################
# T1: calculate followup type
#########################################################################

# t1 followup
x$t1_followup <- ifelse(x$diag_type == 1 & duration_1y == 0, "CA", "")
x$t1_followup <- ifelse(x$diag_type == 1 & duration_1y == 1, "IR + CA", x$t1_followup)
x$t1_followup <- ifelse(x$diag_type == 1 & x$HbA1c > t1_upper_hba1c_thresh , "IR + CA", x$t1_followup)

#########################################################################
# T2: determine discharge / retain
#########################################################################

# set default to retain
x$t2_optimised <- "no"

# t2. discharge if HbA1c/bmi within threshold
hba1c_bmi_test <- x$diag_type == 2 &
                  x$HbA1c < t2_optimise_HbA1c_thresh &
                  x$BMI < t2_optimise_BMI_thresh

x$t2_optimised <- ifelse(hba1c_bmi_test == TRUE, "yes", x$t2_optimised)

x$autocomment <- ifelse(hba1c_bmi_test == TRUE,
                        paste0("T2DM. discharge as conditions met: HbA1c < ", t2_optimise_HbA1c_thresh, "mmol/mol & BMI < ", t2_optimise_BMI_thresh, "kg/m^2"), x$autocomment)

# retain and comment if high BMI on insulin
hiBMI_onIns_test <- x$diag_type == 2 &
  x$BMI    > t2_optimise_BMI_thresh_ins  &
  x$CHI %in% t2_sub[ins == 1]$CHI == TRUE &
  x$CHI %in% t2_sub[su == 1]$CHI == TRUE

x$t2_optimised <- ifelse(hiBMI_onIns_test == TRUE, "no", x$t2_optimised)

x$autocomment <- ifelse(hiBMI_onIns_test == TRUE,
                        paste0("T2DM. BMI above ", t2_optimise_BMI_thresh, "kg/m^2. on insulin or SU"), x$autocomment)

# identify multiple treatment IDs
therapy_ids <- c("metformin", "insulin", "sglt2", "Other Injectable Agent", "Gliptin", "glitazone")
therapy_frame <- as.data.frame(matrix(nrow = nrow(x), ncol = length(therapy_ids)))
colnames(therapy_frame) <- therapy_ids

for (i in 1:length(therapy_ids)) {
  vec = rep(0, nrow(x))
  vec[grep(therapy_ids[i], x$Treatment.Type, ignore.case = TRUE)] <- 1
  therapy_frame[, i] <- vec
} 

therapy_frame$total_agents <- rowSums(therapy_frame)

# retain if T2 on >= 3 agents
agent_n_test <- x$diag_type == 2 & therapy_frame$total_agents > t2_n_med_thresh

x$t2_optimised <- ifelse(agent_n_test == TRUE, "no", x$t2_optimised)

x$autocomment <- ifelse(agent_n_test == TRUE,
                        paste0("T2DM. On more than ", t2_n_med_thresh, " agents"), x$autocomment)

# retain if T2 & recent diagnosis (2y)
time_from_diagnosis <- as.numeric(ymd(index_date) - x$diagnosis_date)

x$t2_optimised <- ifelse(x$diag_type == 2 & time_from_diagnosis < (t2_recent_diagnosis_threshold * 365.25), "no", x$t2_optimised)

x$autocomment <- ifelse(x$diag_type == 2 & time_from_diagnosis < (t2_recent_diagnosis_threshold * 365.25), paste0("T2DM. Diagnosis in last ", t2_recent_diagnosis_threshold, " years"), x$autocomment)

# only retain optimised decision if dmtype = 2
x$t2_optimised <- ifelse(x$diag_type != 2, "", x$t2_optimised)

# urgent
x$urgent = "n"

x[, c("missing_data") := ifelse(flag_review_diagosis == 1 &
                                  t1_no_insulin == 0 &
                                  t1_high_BMI == 0 &
                                  suspect_meds == 0 &
                                  t2_thin_plus_ins ==0, 1, 0) , by=.(CHI)]

# time since last review
x$time_to_index <- as.numeric(ymd(index_date) - x$date_last_review)

# discharge if time since last review > threshold eg 3 years
x$t2_optimised <- ifelse(as.numeric(x$time_to_index) > t2_max_time_from_last_review, "yes", x$t2_optimised)

## add postcode
x$short_postcode <- substr(x$Patient.Postcode, 1, 4)
x$short_postcode <- ifelse(substr(x$short_postcode, 3, 3) == " ", substr(x$short_postcode, 1, 2), x$short_postcode)

#########################################################################
# merge with manually annotated data
#########################################################################
annotated = read.csv("~/projects/caseLoad/annotated.csv", stringsAsFactors = FALSE, header = TRUE)

for (j in 1:nrow(annotated)) {
  
  x[CHI == annotated$CHI[j]]$short_postcode = annotated$Postcode[j]
  x[CHI == annotated$CHI[j]]$diag_type = annotated$diag_type[j]
  x[CHI == annotated$CHI[j]]$date_last_review = dmy(annotated$date_last_review[j])
  x[CHI == annotated$CHI[j]]$t1_followup  = annotated$t1_followup [j]
  x[CHI == annotated$CHI[j]]$comments  = annotated$comments [j]
  x[CHI == annotated$CHI[j]]$t2_optimised  = annotated$t2_optimised [j]
  
}

output <- data.frame(x$CHI, x$Surname, x$Forename, x$short_postcode, x$diag_type, x$t1_followup, x$date_last_review, x$urgent, x$t2_optimised, x$flag_review_diagosis, x$t1_no_insulin, x$t1_high_BMI, x$suspect_meds, x$t2_thin_plus_ins, x$missing_data, x$autocomment, x$comments)
colnames(output) <- c("CHI", "Surname", "Forename", "Postcode", "diag_type", "t1_followup", "date_last_review", "urgent", "t2_optimised", "flag_review_diagosis", "t1_no_insulin", "t1_high_BMI", "t1_suspect_meds", "t2_thin_plus_ins", "missing_data", "autocomments", "comments")

print("total n:"); print(nrow(output))
print("total n T1DM:"); print(nrow(x[diag_type == 1]))
print("total n T2DM:"); print(nrow(x[diag_type == 2]))
print("total n T2DM - discharge:"); print(nrow(x[diag_type == 2 & t2_optimised == "yes"]))
print("prop T2 discharged"); print(nrow(x[diag_type == 2 & t2_optimised == "yes"]) / nrow(x[diag_type == 2]))
print("n for review"); print(nrow(x[flag_review_diagosis == 1]))

#########################################################################
# generate SC compliant output
#########################################################################

sc <- x[match(cs_pt$CHI, x$CHI)]
sc$Postcode..first.part. <- sc$short_postcode
sc$Diagnosis..1..2..or.non.1.2. <- sc$diag_type
sc$Pathway.Code.if.T1..eg.CA..IR.CA...PIE.CA. <- sc$t1_followup
sc$Date.of.last.annual.review..eg.Nov19. <- as.yearmon(sc$date_last_review)
sc$Urgent..Needs.input.pre.August <- "n"
sc$If.T2..optimised...Y..N. <- sc$t2_optimised
sc$Free.text <- sc$autocomment

comments = sc$comments

sc[, 15:ncol(sc)] <- NULL

sc$manual_comments <- comments

#########################################################################
# save out output
#########################################################################
write.table(output, file = paste0("~/projects/caseLoad/output_", inputFile_name), sep = ",", row.names = FALSE)

write.xlsx(sc, file = "./cs_dm_export.xlsx", sheetName = "export", col.names = TRUE, row.names = FALSE, append = FALSE)
