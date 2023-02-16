# a module for `buildEDD()`
options(warn=-1)
getBob2022HabResultsUnitLookup <- function(df, bob_2022_hab){
  tryCatch(
    expr = {
      #----- load external libraries
      suppressWarnings(suppressMessages(library(data.table)))
      suppressWarnings(suppressMessages(library(tidyverse)))
      suppressWarnings(suppressMessages(library(dplyr)))
      suppressWarnings(suppressMessages(library(readxl)))
      
      # make a lookup table for units
      unit_lookup <- data.frame(variable=unique(df$variable))

      int_df <- bob_2022_hab %>% select(unit_lookup$variable)
      for(i in 1:nrow(unit_lookup)){
        unit_lookup$choices[i] <- paste0(unique(int_df[,i]), collapse = ",")
        # unit_lookup$choices[i] <- dput(as.character(unique(int_df[,i])))
        # unit_lookup$choices[i] <- paste(shQuote(unique(int_df[,i])), collapse=", ")
        if(stringr::str_detect(unit_lookup$choices[i], "\\(")){
          unit_lookup$choices[i] <- stringr::str_extract(unit_lookup$choices[i], "(?<=\\().+?(?=\\))")
        }
      }
      unit_lookup <- as.data.frame(lapply(unit_lookup, function(y) gsub("\\<N\\>", "N", y)))
      unit_lookup <- as.data.frame(lapply(unit_lookup, function(y) gsub("\\<Y\\>", "Y", y)))
      for(i in 1:nrow(unit_lookup)){ # extract units from inside parens if they are present
        unit_lookup$unit[i] <- stringr::str_extract(unit_lookup$variable[i], "(?<=\\().+?(?=\\))") # https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
        if(stringr::str_detect(unit_lookup$variable[i], regex('velocity', ignore_case = T))) {
          unit_lookup$unit[i] <- "m/s"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('depth', ignore_case = T))) {
          unit_lookup$unit[i] <- "m"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('depth', ignore_case = T))) {
          unit_lookup$unit[i] <- "m"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('benthos', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates stream benthic macroinvertebrates were collected"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('habitat assessment', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates habitat inventory data were collected"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('water quality', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates stream water chemistry data were collected"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('vernal pool', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates this sample is from a vernal pool"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('vernal pool present', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates presence of a vernal pool"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('stream gradient', ignore_case = T))) {
          unit_lookup$unit[i] <- "m/km; drop in elevation (m) per unit horizontal distance (km)"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('land use', ignore_case = T))) {
          unit_lookup$unit[i] <- "Description of the land-use category in the immediate vicinity of the sampling location"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('left bank buffer breaks', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates there are breaks in the riparian buffer on the left descending creek bank"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('right bank buffer breaks', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates there are breaks in the riparian buffer on the right descending creek bank"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('culvert', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates is a culvert in the sampling reach"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('sampleable', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates the sampling reach was sampleable"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('left bank concrete', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates the left descending bank is concrete"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('left bank gabion', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates the left descending bank is gabion"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('left bank rip-rap', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates the left descending bank is rip-rap"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('Left bank Earthen berm', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates the left descending bank is an earthen berm"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('Left bank Dredge spoil', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates the left descending bank is made of dredge spoil"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('Left bank Pipe culvert', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates the left descending bank has one or more culverts"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('Right bank Concrete', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates the right descending bank is concrete"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('Right bank Concrete', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates the right descending bank is concrete"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('left bank gabion', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates the right descending bank is gabion"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('right bank rip-rap', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates the right descending bank is rip-rap"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('right bank Earthen berm', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates the right descending bank is an earthen berm"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('right bank Dredge spoil', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates the right descending bank is made of dredge spoil"
        }
        if(stringr::str_detect(unit_lookup$variable[i], regex('right bank Pipe culvert', ignore_case = T))) {
          unit_lookup$unit[i] <- "Y/N; Y indicates the right descending bank has one or more culverts"
        }
      }
      # openxlsx::write.xlsx(unit_lookup, "data/unit_lookup.xlsx")
      
      
      message("`getBob2022HabResultsUnitLookup()` executed successfully...")
      return(unit_lookup)
    }
  )
}