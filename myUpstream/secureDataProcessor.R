# =====================================================================================
# "secureDataProcessor.R" file                                                        |
#            designate folder locations                                               |
#            load packages                                                            |
#            read in key variable mapping file                                        |
#            process2005-2015 files                                                   |
#            process 2000-2004 files                                                  |
#            merge and save 2000-2004 and 2005-2015                                   |
#            generate sample/distorted file                                           |
#                                                                                     |   
# =====================================================================================

#-- Set Locations Etc----------------------------------------------------------------------

# myDriveSec <- "G:/CCB" 
mySecure <- "I:/0.Secure.Data"    

myDrive    <- "E:"
myPlace    <- paste0(myDrive,"/0.CBD/myCBD")
upPlace    <- paste0(myDrive,"/0.CBD/myUpstream")

#-- Load Packages --------------------------------------------------------------------------

library(stringr)
library(readxl)
library(dplyr)
library(readr)

#-- Read in variable mapping file -----------------------------------------------------------

varInfo <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/death.File.Vars.xlsx"), sheet="variableNames"))   

# === Process 2005 - 2015 files ==============================================================

vInfo <- filter(varInfo,file1 == 1)  # select input variables and variable names for 2005-2015 files

 ca15    <- read.csv(paste0(mySecure,"/rawDeathData/Samuel_2015.csv"), colClasses = "character") 
 ca14    <- read.csv(paste0(mySecure,"/rawDeathData/Samuel_2014.csv"), colClasses = "character") 
 ca13    <- read.csv(paste0(mySecure,"/rawDeathData/Samuel_2013.csv"), colClasses = "character")
 ca12    <- read.csv(paste0(mySecure,"/rawDeathData/Samuel_2012.csv"), colClasses = "character")
 ca11    <- read.csv(paste0(mySecure,"/rawDeathData/Samuel_2011.csv"), colClasses = "character")
 ca10    <- read.csv(paste0(mySecure,"/rawDeathData/Samuel_2010.csv"), colClasses = "character")
 ca09    <- read.csv(paste0(mySecure,"/rawDeathData/Samuel_2009.csv"), colClasses = "character")
 ca08    <- read.csv(paste0(mySecure,"/rawDeathData/Samuel_2008.csv"), colClasses = "character")
 ca07    <- read.csv(paste0(mySecure,"/rawDeathData/Samuel_2007.csv"), colClasses = "character")
 ca06    <- read.csv(paste0(mySecure,"/rawDeathData/Samuel_2006.csv"), colClasses = "character")
 ca05    <- read.csv(paste0(mySecure,"/rawDeathData/Samuel_2005.csv"), colClasses = "character")
 
cbdDatA        <- rbind(ca15,ca14,ca13,ca12,ca11,ca10,ca09,ca08,ca07,ca06,ca05)
cbdDatA        <- cbdDatA[vInfo$seqID1]   # select only needed columns of 2005-2015 data!
names(cbdDatA) <- vInfo$varName           # name columns based on varName!

cbdDatA$year               <- as.numeric(cbdDatA$year)
cbdDatA$multiraceStatus    <- as.numeric(cbdDatA$multiraceStatus)

# AGE -----
# HARMONISE with CHSI:
cbdDatA$age      <- as.numeric(cbdDatA$age) # missing values generated from non-numeric values # consider fixes
cbdDatA$age[!cbdDatA$age %in% 0:120] <- NA

# STATE -----
# select Califonia cases only
# HARMONIZE with CHSI -- "state" here based on "F71" only -- 10 blank, 4593 XX (not US or Canada), 6685 ZZ (unknown) -- use additional variables?
# note: large shift of previously missing to "unknown" code since last data processing -- seems good
cbdDatA <- subset(cbdDatA, state=="CA")     # note: subset function excludes NAs 
cbdDatA$stateFIPS  <-"06" 

# 2005-2013 & 2015 data DOES NOT include reallocates (deaths of California residents that occured out-of-state)
# 2014             data DOES     include reallocates
# what about 2001-2004?
# is this correct?
# can we fix it?

# COUNTY -----
# County name based strictly on (mapping to) FIPS code (F62)
# HARMONISE with CHSI:
fipsCounty          <- as.data.frame(read_excel(paste0(myPlace,"/myinfo/countyCodes.Map.xlsx"))) 
cbdDatA$county      <- fipsCounty$countyName[match(cbdDatA$countyFIPS,fipsCounty$FIPSCounty)]        

# code no longer needed since FIPS code is read in as character, but valuable "snipit" for similar purposes:
# cbdDatA$countyFIPS  <- str_pad(cbdDatA$countyFIPS,3,pad="0")  

# -----------------------------------------------------------------------------------------------------------------------
# several prior issues with the geoid variable (now using F192) have been resolved by CHSI, making code
#  below unnecessary, but keeping some for now becuase potentially useful for other/related purposes
#  F192 is now an 11 character string for between 95% and 97% for all years 2005-2015, and is blank otherwise -- good/clean

# geoid checking 1
# cbdDatA$geoLength <- nchar(cbdDatA$geoid,type="chars")
# write.csv(table(cbdDatA$year, cbdDatA$geoLength), file= paste0(upPlace,"/tempOutput/geoLengthExplore.csv"))

# geoid processing
# cbdDatA$geoid[cbdDatA$year %in% c(2013,2014,2015)] <- paste0("0",cbdDatA$geoid[cbdDatA$year %in% c(2013,2014,2015)])
# cbdDatA$geoid[cbdDatA$geoid == "0NA" | cbdDatA$geoid == "0" | cbdDatA$geoid == "" ]  <- NA
# cbdDatA$geoid[cbdDatA$geoLength != 11 | !(cbdDatA$year %in% 2011:2015) ]             <- NA
# cbdDatA$geoid <- str_pad(cbdDatA,side="left",pad="0")

# geoid checking 2
# cbdDatA$geoLength <- nchar(cbdDatA$geoid,type="chars")
# write.csv(table(cbdDatA$year, cbdDatA$geoLength), file= paste0(upPlace,"/tempOutput/geoidWidth2.csv"))

# cbdDatA$geoLength  <- NULL    # drops geoLength "variable"
# -----------------------------------------------------------------------------------------------------------------------

# RACE/ETHNICITY -----

cbdDatA$raceCode                              <-"-missing"

cbdDatA$raceCode[cbdDatA$multiraceStatus==1]  <-"White-NH"
cbdDatA$raceCode[cbdDatA$multiraceStatus==2]  <-"Black-NH"
cbdDatA$raceCode[cbdDatA$multiraceStatus==3]  <-"AIAN-NH"
cbdDatA$raceCode[cbdDatA$multiraceStatus==4]  <-"Asian-NH"
cbdDatA$raceCode[cbdDatA$multiraceStatus==5]  <-"NHPI-NH"
cbdDatA$raceCode[cbdDatA$multiraceStatus==6]  <-"Other-NH"
cbdDatA$raceCode[cbdDatA$multiraceStatus==7]  <-"Multi-NH"
cbdDatA$raceCode[cbdDatA$multiraceStatus==9]  <-"Unk-NH"

cbdDatA$raceCode[cbdDatA$multiraceStatus==8]  <-"Hisp"
cbdDatA$raceCode[cbdDatA$hispanicOrigin=="Y"] <-"Hisp"

cbdDatA$hispanicOrigin   <- NULL
cbdDatA$multiraceStatus  <- NULL

# === Process 2000 - 2014 files ==============================================================

# note: 2000-2004 files are "flat" ASCII files not .csv so need to be processed differently

vInfo <- filter(varInfo,file3 == 1)    # select input variables and variable names for 2000-2004 files
vInfo <- vInfo[order(vInfo$mStart),]   # columns need to be read in order with read_fwf function !!  

f0 <- paste0(mySecure,"/rawDeathData/Death2000.txt")
f1 <- paste0(mySecure,"/rawDeathData/Death2001.txt")
f2 <- paste0(mySecure,"/rawDeathData/Death2002.txt")
f3 <- paste0(mySecure,"/rawDeathData/Death2003.txt")
f4 <- paste0(mySecure,"/rawDeathData/Death2004.txt")

# reading from flat files based on start and end positions and names as defined in vInfo  
ca00 <- read_fwf(file=f0,col_positions=fwf_positions(start=vInfo$mStart, end=vInfo$mEnd, col_names = vInfo$varName),skip=0)
ca01 <- read_fwf(file=f1,col_positions=fwf_positions(start=vInfo$mStart, end=vInfo$mEnd, col_names = vInfo$varName),skip=0)
ca02 <- read_fwf(file=f2,col_positions=fwf_positions(start=vInfo$mStart, end=vInfo$mEnd, col_names = vInfo$varName),skip=0)
ca03 <- read_fwf(file=f3,col_positions=fwf_positions(start=vInfo$mStart, end=vInfo$mEnd, col_names = vInfo$varName),skip=0)
ca04 <- read_fwf(file=f4,col_positions=fwf_positions(start=vInfo$mStart, end=vInfo$mEnd, col_names = vInfo$varName),skip=0)

cbdDatB <- rbind(ca00,ca01,ca02,ca03,ca04)

# AGE -----
# HARMONISE with CHSI:
cbdDatB$age                              <- as.numeric(cbdDatB$age)              # some non-numeric values become NA
cbdDatB$age[cbdDatB$ageUnit==0]          <- cbdDatB$age[cbdDatB$ageUnit==0]+100  # ageUnit = 0 are > 99 years of age 
cbdDatB$age[cbdDatB$ageUnit %in% c(2:5)] <- 0                                    # ageUnit = 2-5 are < 1 (months, days or hours) 
                                                                                 # ageUnit = 9 is unknown

# STATE -----
# HARMONISE with CHSI -- "State" based on stateCode=05 only (98.7%)
cbdDatB           <- subset(cbdDatB, stateCode=="05") # restricts data to CA state of residence code
cbdDatB$stateFIPS <-"06"                              # create stateFIPS variable with standard 06 CA code
cbdDatB$stateCode <- NULL                             # remove stateCode variable

# COUNTY -----
# note: same process as 2005-2015, but using diffent code standard from death files, so different mapping column
cbdDatB$county     <- fipsCounty$countyName[match(cbdDatB$countyCode,fipsCounty$cdphcaCountyTxt)]  
cbdDatB$countyCode <- NULL

# SEX -----
cbdDatB$sex[cbdDatB$sex==1] <- "M"  # could use mutate here and elsewhere, but this whole file uses basic R indexing approach
cbdDatB$sex[cbdDatB$sex==2] <- "F"

# GEOID ----
# no census tract data currently used for 2000-2004 data
# 4 character tractCode variable is availabe, but unclear if this can reliably be combined with county and state to generate geoid??
cbdDatB$tractCode        <- NULL   # for now remove this variable


# RACE/ETHNICITY -----

cbdDatB$raceCode                                             <-"-missing"

cbdDatB$raceCode[cbdDatB$multiraceStatus==1]                 <-"White-NH"
cbdDatB$raceCode[cbdDatB$multiraceStatus==2]                 <-"Black-NH"
cbdDatB$raceCode[cbdDatB$multiraceStatus==3]                 <-"AIAN-NH"
cbdDatB$raceCode[cbdDatB$multiraceStatus==4]                 <-"Asian-NH"
cbdDatB$raceCode[cbdDatB$multiraceStatus==5]                 <-"NHPI-NH"
cbdDatB$raceCode[cbdDatB$multiraceStatus==6]                 <-"Other-NH"
cbdDatB$raceCode[cbdDatB$multiraceStatus==7]                 <-"Multi-NH"
cbdDatB$raceCode[cbdDatB$multiraceStatus==9]                 <-"Unk-NH"

cbdDatB$raceCode[cbdDatB$hispanicOrigin %in% c(2,3,4,5,6,8)] <-"Hisp"

cbdDatB$hispanicOrigin   <- NULL 
cbdDatB$multiraceStatus  <- NULL

# === Combine 2001-2004 and 2005-2015 files ============================================================================

cbdDat0FULL  <- bind_rows(cbdDatA,cbdDatB)  # "When row-binding using bind_rows, columns are matched by name,
                                            #   and any values that don't match will be filled with NA."

save(cbdDat0FULL, file= paste0(mySecure,"/myData/cbdDat0FULL.R"))


# ========================================================================================================================
# === Create Random Data Set =============================================================================================
  
# CAUTION
load(paste0(mySecure,"/myData/cbdDat0FULL.R"))

#cbdDat0FULL$ageUnit  <- NULL
work <- cbdDat0FULL
work <- work[,c("year","state","county","zip","GEOID","countyFIPS","stateFIPS","age","sex","raceCode","ICD10")]

sampN1 <- 200000  
half1  <- sample_n(work,sampN1)  # sample function from dplyr

sampN2       <- 300000
p1           <- sample_n(work[,1:7],  sampN2)
p2           <- sample_n(work[,8:10], sampN2)
p3           <- sample_n(work[,10:11], sampN2)
p3$raceCode  <- NULL
half2        <- cbind(p1,p2,p3)

cbdDat0SAMP <- rbind(half1,half2)

save(cbdDat0SAMP, file= paste0(upPlace,"/upData/cbdDat0SAMP.R"))


