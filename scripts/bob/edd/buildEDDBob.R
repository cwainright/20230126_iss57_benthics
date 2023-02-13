# build electronic data deliverable (EDD)
# EDD is a data format https://doimspp.sharepoint.com/sites/NCRNBiologicalStreamSampling/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=oKeaBg&cid=46a9b284%2D304d%2D4b63%2D9d30%2D64b97cdd0314&RootFolder=%2Fsites%2FNCRNBiologicalStreamSampling%2FShared%20Documents%2FGeneral%2FAnnual%2DData%2DPackages%2F2022%2FExamples&FolderCTID=0x0120002FA57F4A20C6EC439BFEBEF4FA9D88E0
# database is from: https://doimspp.sharepoint.com/:u:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb?csf=1&web=1&e=jjeJIg

# build example
options(warn=-1)
buildEDDBob <- function(connection, write, addBob){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(openxlsx)))
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            
            #----- load project functions
            source("scripts/bob/edd/getEDDLocations.R")
            source("scripts/bob/edd/getEDDActivities.R")
            source("scripts/bob/edd/getEDDResults.R") # equivalent to python "from x import function"
            source("scripts/bob/getQueryResults.R") # equivalent to python "from x import function"
            
            #----- read and pre-process static assets
            # https://doimspp.sharepoint.com/:f:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template?csf=1&web=1&e=beb0mc
            habitat_marc2021 <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_Stream_Habitat_2021_Marc.xlsx", sheet = "Summer_Habitat_Data_2021") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_Stream_Habitat_2021_Marc.xlsx?d=w621453e1ce9f48a6a36d48850938f9cf&csf=1&web=1&e=B3sOOD
            habitat_marc2022 <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_Stream_Habitat_2022_Marc.xlsx", sheet = "Summer Habitat Data Sheet") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_Stream_Habitat_2022_Marc.xlsx?d=web62dc84b1204861bb8fff2754a34c88&csf=1&web=1&e=5Um3sm
            bob_2021_macroinvert <- readxl::read_excel("data/2021 PRWI sites.xlsx", sheet = "raw benthic data")
            bob_2021_water_chem <- readxl::read_excel("data/2021 PRWI sites.xlsx", sheet = "water chem")
            
            #-----  Query db
            db_objs <- RODBC::sqlTables(con) # test db connection
            tbl_names <- db_objs %>% # choose which tables you want to query
              subset(
                TABLE_NAME %in% c(
                  "tbl_Events",
                  "tlu_Macroinverts",
                  "tbl_Protocol",
                  "tbl_Chemistry_Data",
                  "tbl_Locations",
                  "tbl_Meta_Events",
                  "tbl_Benthic_Data",
                  "tlu_Basin_Code",
                  "tbl_Summer_PHI",
                  "tbl_Spring_PHI",
                  "tbl_Chemistry_Data",
                  "tbl_Benthic_Habitat",
                  "tbl_Summer_PHI",
                  "tbl_Spring_PHI"
                )
              ) %>%
              select(TABLE_NAME)
            
            # make list of queries so we can extract a few rows from each table
            qry_list <- vector(mode="list", length=nrow(tbl_names))
            names(qry_list) <- tbl_names$TABLE_NAME
            for (i in 1:length(qry_list)){
                qry_list[[i]] <- paste("SELECT * FROM", names(qry_list)[i])
            }
            
            results_list <- getQueryResults(qry_list = qry_list, connection = con)
            RODBC::odbcCloseAll() # close db connection
            
            # tidy up
            rm(db_objs)
            rm(tbl_names)
            rm(qry_list)
            
            #----- call functions that build data for EDD tabs
            activities <- getEDDActivities(results_list, habitat_marc2021, habitat_marc2022, bob_2021_macroinvert, bob_2021_water_chem, addBob)
            locations <- getEDDLocations(results_list, habitat_marc2021, habitat_marc2022, bob_2021_macroinvert, bob_2021_water_chem, addBob)
            results <- getEDDResults(results_list, habitat_marc2021, habitat_marc2022, bob_2021_macroinvert, bob_2021_water_chem, addBob)
            
            #----- compile data for EDD tabs into a list
            list_of_datasets <- list("Locations" = locations, "Activities" = activities, "Results" = results)
            if(length(list_of_datasets)==3){
                if(nrow(list_of_datasets[[1]]>0) & nrow(list_of_datasets[[2]]>0) & nrow(list_of_datasets[[3]]>0)){
                    assign("EDD", list_of_datasets, envir = globalenv()) # save final product to global environment
                    message("\n\n`buildEDDBob()` successfully produced data views.\nOutput saved as `EDD` in global environment.\n\n")
                }
            } else {
                message("An error occurred when compiling results.")
                break
            }
            
            #----- write list to xlsx if `write` flag is TRUE
            if(write == TRUE){
                openxlsx::write.xlsx(list_of_datasets, file = file.choose())
            }
        }
    )
}