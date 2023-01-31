# a module for `buildEDDBob()`
options(warn=-1)
getEDDActivities <- function(results_list, habitat_marc2021, habitat_marc2022, bob_2021_macroinvert, bob_2021_water_chem, addBob){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            #----- load project functions
            source("scripts/edd/getMarcHabActivities.R")
            source("scripts/edd/getNCRNChemActivities.R")
            source("scripts/edd/getNCRNHabActivities.R")
            source("scripts/bob/edd/getNCRNBenthicHabActivities.R")
            source("scripts/bob/edd/getNCRNMacroinvertActivities.R")
            source("scripts/bob/edd/getBobMacroinvertActivities.R")
            source("scripts/bob/edd/getBobChemActivities.R")
            
            # load example data
            example <- readxl::read_excel("data/NCRN_BSS_EDD_20230105_1300.xlsx", sheet = "Activities") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNDataManagement/Shared%20Documents/General/Standards/Data-Standards/EQuIS-WQX-EDD/NCRN_BSS_EDD_20230105_1300.xlsx?d=w8c283fde9cbd4af480945c8c8bd94ff6&csf=1&web=1&e=7Y9W1M
            
            #----- call NCRN project functions
            ncrn_chem_activities <- getNCRNChemActivities(results_list, example)
            ncrn_hab_activities <- getNCRNHabActivities(results_list, example)
            ncrn_macroinvert_activities <- getNCRNMacroinvertActivities(results_list, example)
            ncrn_benthic_hab_activities <- getNCRNBenthicHabActivities(results_list, example)
            marc_habitat_activities <- getMarcHabActivities(habitat_marc2022, habitat_marc2021, example, results_list)
            real <- rbind(ncrn_chem_activities, ncrn_hab_activities, ncrn_macroinvert_activities, ncrn_benthic_hab_activities, marc_habitat_activities) # row bind project function returns
            
            #----- if TRUE, call Marc project functions
            if(addBob==TRUE){
              bob_2021_macroinvert_activities <- getBob2021MacroinvertsActivities(results_list, bob_2021_water_chem, example)# bob's water chem and macroinvert samples are the same locations
              bob_2021_chem_activities <- getBob2021ChemActivities(results_list, bob_2021_water_chem, example) # just use `water_chem` twice because it's already grouped by site
              # combine
              real <- rbind(real, bob_2021_macroinvert_activities, bob_2021_chem_activities)
            }
            
            #----- keep only unique Activities
            real <- real %>% distinct(Activity_ID, .keep_all = TRUE)
            
            # error-checking:
            check_df <- tibble::tibble(data.frame(matrix(ncol=3, nrow=ncol(real))))
            colnames(check_df) <- c("real", "example", "result")
            check_df$real <- colnames(real)
            check_df$example <- colnames(example)
            for(i in 1:nrow(check_df)){
                if(check_df$real[i] == check_df$example[i]){
                    check_df$result[i] <- "MATCH"
                } else {
                    check_df$result[i] <- "MISMATCH"
                }
            }
            
            message(
                if(length(check_df$result == "MATCH") == nrow(check_df)){
                    "`getEDDActivities()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`real.", check_df$real[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            return(real)
        }
    )
}