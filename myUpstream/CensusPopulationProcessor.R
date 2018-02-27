# =====================================================================================
# "acsCensusCurrent.R" file                                                           |
#            designate folder locations and load packages                             |
#            load packages                                                            |
#            get census tract population data                                         |
#            link tracts to "community IDs" and to county names                       |
#            save data set                                                            |
#                                                                                     |   
# =====================================================================================

#-- Set Locations Etc----------------------------------------------------------------------

myDrive  <- "E:"  
myPlace  <- paste0(myDrive,"/0.CBD/myCBD")  
upPlace  <- paste0(myDrive,"/0.CBD/myUpstream")  

#-- Load Packages ---------------------------------------------------------------------------------------------------

library(tidycensus)  # gets census and ACS data - get_acs function
library(tigris)      # gets "shape" files  # requires census_api_key("MYKEY") to be run once
library(tidyverse)   # data processing

options(tigris_use_cache = TRUE)

# -------------------------------------------------------------------------------------------------------------------

popCensusCom   <- get_acs(geography = "tract", variables = "B01003_001", state = "CA",endyear=2015)  # GEOID is 11 digit character string #2011-2015
#8057
cbdLinkCA      <- read.csv(paste0(myPlace,"/myInfo/cbdLinkCA.csv"),colClasses = "character")           # file linking census tracts to MSSAs and counties 
#8036
popCensusCom   <- mutate(popCensusCom,
                           comID  = cbdLinkCA[match(popCensusCom$GEOID,cbdLinkCA[,"GEOID"]),"comID"],
                           county = cbdLinkCA[match(popCensusCom$GEOID,cbdLinkCA[,"GEOID"]),"county"],
                           popTot = estimate              )
popCensusCom$NAME  <- popCensusCom$variable <- popCensusCom$estimate <- NULL                                       # remove unnecessary "variables"

# NOTE 22 tracts from get_acs do not match to cbdLinkCA  (and 21 based on endyer 2010!) - tracts with NO population
supTracts <- popCensusCom[is.na(popCensusCom$county),]
write.csv(supTracts,(paste0(upPlace,"/tempOutput/subTracts.csv")))

#popCensusCom <- popCensusCom[!is.na(popCensusCom$county),]

save(popCensusCom, file=paste0(upPlace,"/upData/popCensusCom.R"))


# END ================================================================================================================


