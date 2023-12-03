#--------------------------------------------------------#
# data_manipulation.R
#
# Initial data import and preparation
# Author: Flo Goemans
#--------------------------------------------------------#

#--------------------------------------------------------#
# Bring in data
#--------------------------------------------------------#

phosp_data <- readRDS(paste0(rawpath, #redacted))
phosp_tc <- readRDS(paste0(rawpath, #redacted))
phosp_exclude <- read.table(paste0(excludepath, #redacted), header=TRUE, sep=",")

#--------------------------------------------------------#
# Exclude IDs from exclusion file
#--------------------------------------------------------#

phosp_data_exc <- subset(phops_data, !study_id %in% phosp_exclude$study_id)

#--------------------------------------------------------#
# Check and attach phosp_id to all record (currently only present on first record per study_id)
#--------------------------------------------------------#

#get list of unique study_id/phosp_id combinations
unique_IDs <- subset(phosp_data_exc[ , c("study_id", "phosp_id")], !is.na(phosp_data_exc$phosp_id))

#find unique study IDs in case of missing phosp_IDs
unique_studyID <- as.data.frame(unique(phosp_data_exc$study_id))

#merge unique phosp with unique study
all_IDs <- merge(unique_IDs, unique_studyID, by.x="study_id", by.y="unique(phosp_data_exc$study_id)", all=TRUE)

#check for subjects with a studyid and no phosp_id or vice versa (were present early on)
ID_mismatch <- subset(all_IDs, (is.na(all_IDs$phosp_id)|is.na(all_IDs$study_id)))
ID_mismatch

#merge phosp_id to all records in main dataset
phosp_data_id <- merge(all_IDs, subset(phosp_data_exc, select= -c(phosp_id)), by"study_id", all=TRUE)

#--------------------------------------------------------#
# Split data into 1 dataframe per CRF/ timepoint only keeping columns with any data in each
#--------------------------------------------------------#

#identify unique CRFs present in the data
CRF_list <- as.list(unique(phosp_data_id$redcap_repeat_instrument))
CRF_list <- CRF_list[-2]

event_list <- unique(phosp_data_id$redcap_event_name)

#loop through each CRF name/event name
#extract records that belong with that CRF
#keep only columns with ANY data

#initialise list for results + counting index
i <- 1
data_CRF <- list()

#per CRF
for (CRF in CRF_list){

print(CRF)
data_CRF_temp <- subset(phosp_data_id, redcap_repeat_instrument==CRF_list[[i]] )
empty_columns <- colSums(is.na(data_CRF_temp) == nrow(data_CRF_temp)
data_CRF[[i]] <- data_CRF_temp[ , !empty_columns]

i <- i + 1

}

#per timepoint (where CRF name is missing)
i <- 1
data_event <- list()

for (event in event_list){

print(event)
data_event_temp <- subset(phosp_data_id[is.na(phosp_data_id$redcap_repeat_instrument) , ], redcap_event_name==event_list[[i]] )
empty_columns <- colSums(is.na(data_event_temp) == nrow(data_event_temp)
data_event[[i]] <- data_event_temp[ , !empty_columns]

i <- i + 1

}


#--------------------------------------------------------#
# Tidy R environment
#--------------------------------------------------------#

rm(list=setdiff(ls(), c("filepath", "rawpath", "datapath", "analpath",
			"packages", "vis3lower", "vis3upper", "vis12lower", "vis12upper",
			"knitr_fromR", "phosp_data", "phosp_data_id",
			"CRF_list", "data_CRF", "event_list", "data_event")

## End of Program

