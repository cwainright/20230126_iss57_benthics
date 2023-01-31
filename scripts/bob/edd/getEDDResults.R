# a module for `buildEDD()`
options(warn=-1)
getEDDResults <- function(results_list, habitat_marc2021, habitat_marc2022, bob_2021_macroinvert, bob_2021_water_chem, addBob){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            #----- load project functions
            source("scripts/edd/getMarcHabResults.R")
            source("scripts/bob/edd/getNCRNMacroinvertResults.R")
            source("scripts/bob/edd/getNCRNChemResults.R")
            source("scripts/bob/edd/getNCRNHabResults.R")
            source("scripts/bob/edd/getBobMacroinvertResults.R")
            source("scripts/bob/edd/getBobChemResults.R")
            
            # load example data
            example <- readxl::read_excel("data/NCRN_BSS_EDD_20230105_1300.xlsx", sheet = "Results") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNDataManagement/Shared%20Documents/General/Standards/Data-Standards/EQuIS-WQX-EDD/NCRN_BSS_EDD_20230105_1300.xlsx?d=w8c283fde9cbd4af480945c8c8bd94ff6&csf=1&web=1&e=7Y9W1M
            
            #----- call NCRN project functions
            ncrn_fish_results <- getNCRNFishResults(results_list, example)
            ncrn_chem_results <- getNCRNChemResults(results_list, example)
            ncrn_hab_results <- getNCRNHabResults(results_list, example)
            marc_habitat_results <- getMarcHabResults(habitat_marc2022, habitat_marc2021, example, results_list)
            real <- rbind(ncrn_fish_results, ncrn_chem_results, ncrn_hab_results, marc_habitat_results) # row bind project function returns
            
            #----- if TRUE, call Bob functions
            if(addBob==TRUE){
                # run bob results modules
                bob_2021_macroinvert_results <- getBob2021MacroinvertsResults()
                bob_2021_chem_results <- getBob2021ChemResults()
                # combine
                real <- rbind(real, bob_2021_macroinvert_results, bob_2021_chem_results)
            }
            
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
                    "`getEDDResults()` executed successfully..."
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