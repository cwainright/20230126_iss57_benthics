# a module for `buildEDDBob()`
options(warn=-1)
getEDDLocations <- function(results_list, habitat_marc2021, habitat_marc2022, bob_2021_macroinvert, bob_2021_water_chem, bob_2022_wq, bob_2022_hab, bob_2022_macroinvert, addBob){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            #----- load project functions
            source("scripts/edd/getMarcHabLocations.R")
            source("scripts/edd/getNCRNChemLocations.R")
            source("scripts/bob/edd/getNCRNBenthicHabLocations.R")
            source("scripts/bob/edd/getNCRNMacroinvertLocations.R")
            source("scripts/edd/getNCRNHabLocations.R")
            source("scripts/bob/edd/getBobMacroinvertLocations.R")
            source("scripts/bob/edd/getBobChemLocations.R")
            source("scripts/bob/edd/getBob2022ChemLocations.R")
            source("scripts/bob/edd/getBob2022HabLocations.R") # write me
            source("scripts/bob/edd/getBob2022MacroinvertLocations.R")
            
            # load example data
            example <- readxl::read_excel("data/NCRN_BSS_EDD_20230105_1300.xlsx", sheet = "Locations") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNDataManagement/Shared%20Documents/General/Standards/Data-Standards/EQuIS-WQX-EDD/NCRN_BSS_EDD_20230105_1300.xlsx?d=w8c283fde9cbd4af480945c8c8bd94ff6&csf=1&web=1&e=7Y9W1M
            
            #----- call NCRN project functions
            marc_habitat_locations <- getMarcHabLocations(habitat_marc2022, habitat_marc2021, example, results_list) # updated to NCRN_Site_ID
            ncrn_chem_locations <- getNCRNChemLocations(results_list, example) # updated to NCRN_Site_ID
            ncrn_macroinvert_locations <- getNCRNMacroinvertLocations(results_list, example) # updated to NCRN_Site_ID
            ncrn_benth_hab_locations <- getNCRNBenthicHabLocations(results_list, example) # NCRN_Site_ID
            ncrn_hab_locations <- getNCRNHabLocations(results_list, example) # updated to NCRN_Site_ID
            
            real <- rbind(ncrn_macroinvert_locations, ncrn_benth_hab_locations, ncrn_chem_locations, ncrn_hab_locations, marc_habitat_locations) # row bind project function returns
            
            #----- if TRUE add Bob's 2022 data to dataframe `real` NCRN db data
            if(addBob==TRUE){
              bob_2021_macroinvert_locations <- getBob2021MacroinvertLocations(results_list, bob_2021_water_chem, example) # updated to NCRN_Site_ID
              bob_2021_chem_locations <- getBob2021ChemLocations(results_list, bob_2021_water_chem, example) # updated to NCRN_Site_ID
              bob_2022_chem_locations <- getBob2022ChemLocations(results_list, bob_2022_wq, example) # updated to NCRN_Site_ID
              bob_2022_hab_locations <- getBob2022HabLocations(results_list, bob_2022_hab, example) # updated to NCRN_Site_ID
              bob_2022_macroinvert_locations <- getBob2022MacroinvertLocations(results_list, bob_2022_macroinvert, example) # updated to NCRN_Site_ID
              # combine
              real <- rbind(real, bob_2021_macroinvert_locations, bob_2021_chem_locations, bob_2022_chem_locations, bob_2022_hab_locations, bob_2022_macroinvert_locations)
            }
            
            #----- keep only unique Locations
            real <- real[!duplicated(real[c('Latitude', 'Longitude')]),] # since naming conventions differed according to who did the sampling, we keep unique lat/lon pairs
            
            #----- error-checking:
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
                    "`getEDDLocations()` executed successfully..."
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