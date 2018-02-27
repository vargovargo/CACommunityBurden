# ====================================================================================================
# "StatePopulationDataProcessor.R" file                                                              |
#                                                                                                    |
#            Reads in age-race-sex-1980-2015 text file provided by CDPH-CID-DCDC-STDCB               |
#            Subsets and processes data, and saves as County by year total population file           |
#            Adds stratification by age group, and saves another file (for age adjustement)          |
#            Generates total 2015 CA pop by age to use as "Standard Population" for age adjustment   |
#                                                                                                    |
#                                                                                                    |   
# ====================================================================================================

# -- Set locations and load packages ---------------------------------------------------------------------------------------------------

myDrive    <- "E:"
myPlace    <- paste0(myDrive,"/0.CBD/myCBD")
upPlace    <- paste0(myDrive,"/0.CBD/myUpstream")

library(dplyr)
library(readxl)

source(paste0(myPlace,"/myFunctions/helperFunctions/capwords.R"))



# special file location becuase file is too big for GitHub
# tDat        <- read.delim(file=paste0(myDrive,"/0.CBD/Resources/populationData/1980_2025.txt"),sep="\t", header = TRUE, stringsAsFactors = FALSE)
# tDat        <- filter(tDat, YEAR >=2000 & YEAR <= 2020)
# save(tDat, file= paste0(upPlace,"/upData/tDat_2000_2020.R"))
load(file= paste0(upPlace,"/upData/tDat_2000_2020.R"))



names(tDat) <- c("county","year","sex","age","raceE","OrigPop","pop" )
tDat        <- filter(tDat, year %in% 2000:2015 & !(county %in% c("Alameda HD","Berkeley","Pasadena","Long Beach","Los Angeles HD")))


# could use DOF data instead, downloaded from their Open Data Site
# does not contain race/ethnicity which will be needed soon
tDatDOF        <- read.csv(file=paste0(upPlace,"/upData/dof_dru_pop_1970_2050_csya_wide.csv"),as.is=TRUE)
tDatDOF        <- filter(tDatDOF, year %in% 2000:2015)
tDatDOF$county <- capwords(tDatDOF$county,strict=TRUE)


# Trying here to access DOF data directly from their Open Data site
# this looks hopeful:
# https://onepager.togaware.com/CkanO.pdf


# Trid this, but no luck....
# http://data.library.virginia.edu/using-data-gov-apis-in-r/
# install.packages("httr")
# library(httr)
# dofURL  <- "https://data.ca.gov/api/action/datastore/search.json?resource_id=b62d688b-89e1-45f6-8cf6-50ca72d712e0"
# dofInfo <- GET(url = dofURL)
# dofDat  <- content(dofInfo)
# names(dofDat)
# names(dofDat$result[[1]])


# -- Make and save file with County (and California Total) Pop by year 2000-2015 -----------------------------------------------------------

popCounty <- tDat %>% group_by(year, county) %>% summarize(pop  = sum(pop))
save(popCounty, file= paste0(upPlace,"/upData/popCounty.R"))


# -- Construct age groups, stratify pop file by age group and save ---------------------------------------------------------------------------

ageMap  <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/acsCensus.Map.xlsx"),sheet = "ageList"))
ageMap  <- ageMap[!is.na(ageMap$inAgeG),c("lAge","uAge")]
aL      <-      ageMap$lAge
aU      <- c(-1,ageMap$uAge)

aMark       <- findInterval(tDat$age,aU,left.open = TRUE)
aLabs       <- paste(aL,"-",aU[-1])
tDat$ageG   <- aLabs[aMark]


popCountyAgeG <- tDat %>% group_by(year, county,ageG) %>%  summarize(pop  = sum(pop))
popCountyAgeG <- popCountyAgeG[popCountyAgeG$county != "California",]
save(popCountyAgeG, file= paste0(upPlace,"/upData/popCountyAgeG.R"))


# -- Construct and save file with total state popuation by age groups to use as "Standard Population"-------------------------------------------

popT          <- filter(tDat,year == 2015, county == "California")
popStandard   <- popT %>% group_by(ageG) %>%  summarize(popStandard  = sum(pop))
save(popStandard, file= paste0(upPlace,"/upData/popStandard.R"))

