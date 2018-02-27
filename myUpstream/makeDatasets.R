# =====================================================================================
# "makeDatasets.R" file                                                               |
#            designate folder locations and load packages                             |
#            load packages                                                            |
#                                                                                     |   
# =====================================================================================

# -- Designate locations and load packages-------------------------------------------------

myDrive <- "E:"  
myPlace <- paste0(myDrive,"/0.CBD/myCBD") 
upPlace <- paste0(myDrive,"/0.CBD/myUpstream") 

library(tidyverse)
library(epitools)
library(sqldf)
library(readxl)

#-- LOAD MAIN DATA SET, AND "INFO FILES", BEGIN KEY TRANSFORMATIONS------------------------

# CAUTION --- if using REAL DATA INCLUDE these two lines below and edit the first one with your secure location
#  load("F:/0.Secure.Data/myData/cbdDat0FULL.R")      # Secure location "G:/ccb" 
#  cbdDat0 <- cbdDat0FULL    

# Load FAKE Data --- COMMENT OUT these two lines if using REAL DATA
   load(paste0(upPlace,"/upData/cbdDat0SAMP.R"))      
   cbdDat0 <- cbdDat0SAMP

# source(paste0(myPlace,"/myFunctions/icdToGroup.R"))

# this "as.data.frame" below and elsewhere is really annoying.... but at least icdToGroup function below does not work otherwise;
# becuase the "tibble" is double precision or for some other reason this messes up; 
# and get error "Error: Can't use matrix or array for column indexing"

gbdMap0    <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/gbd.ICD.Map.xlsx"), sheet="main"))   # also have e.g. range="A1:J167"
leMap      <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/le.Map.xlsx"), sheet="LifeExpLink", range = cell_cols("A:B")))
yearMap    <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/year.Map.xlsx")))
geoMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/countycodes.Map.xlsx")))
cbdLinkCA  <- read.csv(paste0(myPlace,"/myInfo/cbdLinkCA.csv"),colClasses = "character")  # file linking MSSAs to census 
comName    <- unique(cbdLinkCA[,c("comID","comName")])                                    # dataframe linking comID and comName

cbdDat0       <- mutate(cbdDat0,
                         age    = as.numeric(age),                                                  # redundant...
                         ICD10  = as.character(ICD10),                                              # redundant...
                         comID  = cbdLinkCA[match(cbdDat0$GEOID,cbdLinkCA[,"GEOID"]),"comID"],   
                         yll    = leMap[match(cbdDat0$age,leMap[,"Age"]),"LE"],
                         yearG  = yearMap[match(year,yearMap[,"year"]),"yGroup1"]           )


# function to map ICD-10 codes to GBD conditions
icdToGroup <- function(myIn,myInMap) {
  Cause   <- rep(NA,length(myIn))
  for (i in 1:nrow(myInMap)) {Cause[grepl(myInMap[i,2],myIn)] <- myInMap[i,1] } 
  Cause}

cbdDat0$gbd36   <- icdToGroup(myIn=cbdDat0$ICD10,gbdMap0[!is.na(gbdMap0$list36),c("gbdCode","regEx10")])      
cbdDat0$gbd3    <- icdToGroup(myIn=cbdDat0$ICD10,gbdMap0[is.na(gbdMap0$L2)     ,c("gbdCode","regEx10")])      
cbdDat0$gbdSpec <- icdToGroup(myIn=cbdDat0$ICD10,gbdMap0[ (gbdMap0$L2 %in% c(2,6,8,11,13,14,15,16,21,22) & !is.na(gbdMap0$L3) & is.na(gbdMap0$L4) & is.na(gbdMap0$list36) )    ,c("gbdCode","regEx10")])      

load(paste0(upPlace,"/upData/PopCensusCom.R"))   # Read ACS (Denominator) Data # GEOID character 11 


# DATA CLEANING ISSUES --------------------------------------------------------------------------------

# CENSUS TRACTS
# current from acsWork0     has 8057 tracts 
# current cbdLinkCA         has 8036 (2010 data)
# current cbddat0           has 8603! bad geocodes?        
# something ??              has 8035 ... check...

# these records have a GEOID but not comID suggesting the GEOID is "bad"
# junk <- filter(cbdDat0,is.na(comID) & GEOID != ""  & year > 2004)  
# 651 records
# length(unique(junk$GEOID))
# 590 unique GEOID not in California (based on current link file)
#  write.csv(table(junk$GEOID,junk$year),(paste0(upPlace,"/tempOutput/junk Tracts.csv")))

# county missing from 3797 records     
# junk <- filter(cbdDat0,is.na(county))   
# 3797 records
# countyFIPS blank=2145 and 999=1652 (but State="CA; based on "F71" only)
#  write.csv(table(junk$year,junk$countyFIPS),(paste0(upPlace,"/tempOutput/missing County FIPS.csv")))

# MAJOR cleaning issue!!!
# junk <- filter(cbdDat0,is.na(gbd36))   
# 82775 records where ICD10 does not map to gbd36 -- errors in info file!
#  write.csv(table(junk$year,junk$countyFIPS),(paste0(upPlace,"/tempOutput/no ICD10 to gbd36.csv")))


# --- CONSTANTS ETC. ------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
yF   <- 1*100000  # yearFactor x 100,000
pop5 <- 5


#-- AGE ADJUSTED ("AA") RATES ---make function?   move lower in code? ------------------------------

# OLD
# cbdDat0$ageG[cbdDat0$age >=  0 & cbdDat0$age  <= 4]   <-   "0 - 4"
# cbdDat0$ageG[cbdDat0$age >=  5 & cbdDat0$age <= 14]   <-  "5 - 14"
# cbdDat0$ageG[cbdDat0$age >= 15 & cbdDat0$age <= 24]   <- "15 - 24"
# cbdDat0$ageG[cbdDat0$age >= 25 & cbdDat0$age <= 34]   <- "25 - 34"
# cbdDat0$ageG[cbdDat0$age >= 35 & cbdDat0$age <= 44]   <- "35 - 44"
# cbdDat0$ageG[cbdDat0$age >= 45 & cbdDat0$age <= 54]   <- "45 - 54"
# cbdDat0$ageG[cbdDat0$age >= 55 & cbdDat0$age <= 64]   <- "55 - 64"
# cbdDat0$ageG[cbdDat0$age >= 65 & cbdDat0$age <= 74]   <- "65 - 74"
# cbdDat0$ageG[cbdDat0$age >= 75 & cbdDat0$age <= 84]   <- "75 - 84"
# cbdDat0$ageG[cbdDat0$age >= 85 & cbdDat0$age <= 999]  <- "85 - 999"

# NEW
# aL            <- c(   0, 5,15,25,35,45,55,65,75,85)
# aU            <- c(-1,4,14,24,34,44,54,64,74,84,999)

# NEWER

# make "ageMap" from info file to make standard age groups  # CHANGE NAME OF THIS FILE...
ageMap  <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/acsCensus.Map.xlsx"),sheet = "ageList"))
# subset ageMap to relevant rows and columns
ageMap  <- ageMap[!is.na(ageMap$inAgeG),c("lAge","uAge")]
#ageMap  <- read.csv(paste0(myPlace,"/myInfo/ageGroupMap2.csv"))  #simplier, but want to use same age info source for all files...
aL      <-      ageMap$lAge   # lower age ranges
aU      <- c(-1,ageMap$uAge)  # upper age ranges, plus inital value of "-1" to make all functions work properly

aMark         <- findInterval(cbdDat0$age,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
aLabs         <- paste(aL,"-",aU[-1])                           # make label for ranges
cbdDat0$ageG  <- aLabs[aMark]                                   # make new "ageG" variable based on two objects above 


# make dataframe of all possible combinations of county, year, CAUSE, and ageG for ----------------

county <- data.frame(county = geoMap$countyName)           # these "vectors" need to be dataframes for the sq merge below to work
year   <- data.frame(year   = 2000:2015)
CAUSE  <- data.frame(CAUSE  = c(0,sort(unique(cbdDat0$gbd36))))  # "0" is for "all cause"
ageG   <- data.frame(ageG   = sort(unique(cbdDat0$ageG)))

fullMat <- sqldf(" select * 
                    from  county
                    cross join year
                    cross join CAUSE
                    cross join ageG")

fullMat <- mutate(fullMat,
                   county = as.character(county),
                   ageG   = as.character(ageG),
                   tester = 0)

# other very cool approach from Adam:
# fullMatX <- Reduce(function(...) merge(..., all = TRUE), list(county, year, gbd36, agecat))

# -------------------------------------------------------------------------------------------------

# number of deaths by county, year, *AGE GROUP*, and CAUSE
datAA1 <- cbdDat0 %>% group_by(county,year,ageG,gbd36) %>% 
                       summarize(Ndeaths = n() ) 
names(datAA1)[names(datAA1)=="gbd36"] <- "CAUSE"

# number of ALL CAUSE deaths by county, year, and AGE GROUP  
datAA2 <- cbdDat0 %>% group_by(county,year,ageG) %>% 
  summarize(Ndeaths = n() ) 
datAA2$CAUSE <- 0

datAA1 <- bind_rows(datAA1,datAA2)   # merge cause-specific and all-cause  # 140,192 records

# DATA CLEANING ISSUES as above
datAA1 <- filter(datAA1,!is.na(ageG))   # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
datAA1 <- filter(datAA1,!is.na(CAUSE))  # remove 6955 records with missing CAUSE
datAA1 <- filter(datAA1,!is.na(county))  # remove 758 records with missing county

ageCounty <- merge(fullMat,datAA1,by = c("county","year","ageG","CAUSE"),all=TRUE)   # merge with "fullMat"

load(file= paste0(upPlace,"/upData/popCountyAgeG.R"))
ageCounty <- merge(ageCounty,popCountyAgeG,by = c("county","year","ageG"),all=TRUE)   # merge with county age populations

load(file= paste0(upPlace,"/upData/popStandard.R"))
ageCounty   <- merge(ageCounty,popStandard,by = c("ageG"),all=TRUE)  # merge with "Standard" population

ageCounty$Ndeaths[is.na(ageCounty$Ndeaths)] <- 0    # if NA deaths in strata change to "0"

#calculate number of expected deaths in strata among standard population
ageCounty$deathsE <- (ageCounty$Ndeaths/ageCounty$pop)*ageCounty$popStandard


# NOTE: oDeaths etc  != total deaths in other files because of missings removed
ageAdjWork <- ageCounty %>% group_by(county,year,CAUSE) %>% 
  summarize(oDeaths = sum(Ndeaths),                               # na.rm=TRUE not needed becuase of cleaning above
            oPop    = sum(pop),
            cRate   = 100000*oDeaths/oPop,
            eDeaths = sum(deathsE),
            ePop    = sum(popStandard),
            aRate   = 100000*eDeaths/ePop)

ageAdj <- ageAdjWork[!(ageAdjWork$oDeaths==0),c("county","year","CAUSE","aRate")]  # remove strata with no deaths and select columns


# confidence intervals for (directly) age-adjusted rates
# ageadjust.direct from epitools package
tempDat <- filter(ageCounty,county=="Alameda" & CAUSE ==3 & year == 2015)
tempAdj <- ageadjust.direct(count=tempDat$Ndeaths, pop=tempDat$pop, rate = NULL, stdpop=tempDat$popStandard, conf.level = 0.95)*100000
# https://www.cdc.gov/nchs/data/nvsr/nvsr47/nvs47_03.pdf


# MEASURES FUNCTION =====================================================================================

myMeasures <- function(group_vars,levLab,myTotal=TRUE){
  
   dat <- cbdDat0 %>% group_by_(.dots = group_vars) %>% 
    summarize(Ndeaths = n() , 
              YLL     = sum(yll,   na.rm = TRUE), 
              m.YLL   = mean(yll,  na.rm = TRUE)
              #med.age = median(age,na.rm = TRUE)
              ) %>%  ungroup
   names(dat)[grep("gbd", names(dat))] <- "CAUSE"     # CHANGE "gbd" here to iGRP and add that in front of any groupings
 
   
   if (myTotal) {
   temp <- length(group_vars) 
   dat2 <- cbdDat0 %>% group_by_(.dots = group_vars[-temp]) %>% 
     summarize(Ndeaths = n() , 
               YLL     = sum(yll,   na.rm = TRUE), 
               m.YLL   = mean(yll,  na.rm = TRUE)
               #med.age = median(age,na.rm = TRUE)
               ) %>% ungroup
   dat2$CAUSE <- 0
   dat <- bind_rows(dat,dat2)
   }

   dat       <- filter(dat,!is.na(CAUSE))  # "HARD FIX" that should be assessed carefully
   dat$Level <- levLab
   dat %>% data.frame
}

#  Examples
#  grp <- c("county","year","gbd3")
#  df  <- myMeasures(grp,grp[length(grp)])
#  df  <- myMeasures(grp,grp[length(grp)],myTotal=FALSE)
  

# =====================================================================================================================
# TRACT - level file

grp      <- c("county","GEOID","yearG","gbd36")
datTract <- myMeasures(grp,grp[length(grp)])

popDat1  <- as.data.frame(popCensusCom  %>% group_by(county,GEOID) %>% summarize(popTot = sum(popTot,na.rm = TRUE) ))
datTract <- merge(datTract,popDat1,by.x = c("county","GEOID"), by.y = c("county","GEOID"))                     

datTract <- transform(datTract, 
                      YLLper     = yF*YLL/(pop5*popTot),
                      cDeathRate = yF*Ndeaths/(pop5*popTot))

# =====================================================================================================================
# COMMUNITY - level file  

grp36   <- c("county","comID","yearG","gbd36")
grp3    <- c("county","comID","yearG","gbd3")

datComm <- rbind(myMeasures(grp36,grp36[length(grp36)]),
                 myMeasures( grp3, grp3[ length(grp3)],myTotal=FALSE))

# POPULATION file for community level
popDat1 <- as.data.frame(popCensusCom  %>% group_by(county,comID)  %>% summarize(popTot = sum(popTot,na.rm = TRUE) ))
# MERGE DEATH and POPULATION file
datComm     <- merge(datComm,popDat1,by = c("county","comID"))

# calculate new measures/rates 
datComm     <- transform(datComm, 
                         YLLper     = yF*YLL/(pop5*popTot),
                         cDeathRate = yF*Ndeaths/(pop5*popTot) ) 

temp <- yF*pois.approx(datComm$Ndeaths,pop5*datComm$popTot, conf.level = 0.95)
datComm$rateLCI <- temp$lower
datComm$rateUCI <- temp$upper

datComm  <- merge(datComm, comName, by.x="comID", by.y = "comID",all=TRUE)


# =====================================================================================================================
# STATE-level file  

grp36    <- c("year","gbd36")
datState <- myMeasures(grp36, grp36[length(grp36)])

grpSpec  <- c("year","gbdSpec")
spec     <- myMeasures(grpSpec, grpSpec[length(grpSpec)],myTotal=FALSE)

datState <- rbind(datState,spec) 

load(file= paste0(upPlace,"/upData/popCounty.R"))
statePop  <- popCounty[popCounty$county=="California" ,c("year","pop")]    # CA population by year
datState  <- merge(datState, statePop, by="year",all=TRUE)

datState$cDeathRate <- yF*datState$Ndeaths / datState$pop
datState$YLLper     <- yF*datState$YLL     / datState$pop

datState$causeName <- gbdMap0[match(datState$CAUSE,gbdMap0[,1]),"nameOnly"]   # needed? 
datState$county    <- "CALIFORNIA STATE"

# ---------------------------------------------------------------------------------------
# COUNTY - Level Data Set Generation 

grp       <- c("county","year","gbd36")
datCounty <- myMeasures(grp,grp[length(grp)])

datCounty$causeName <- gbdMap0[match(datCounty$CAUSE,gbdMap0[,1]),"nameOnly"]  #  "needed?"

datCounty <- merge(datCounty,popCounty,by = c("year","county"))

datCounty <- transform(datCounty,
                       YLLper     = yF*YLL/pop,
                       cDeathRate = yF*Ndeaths/pop)

# 95% CI for death rates
temp <- yF*pois.approx(datCounty$Ndeaths,datCounty$pop, conf.level = 0.95)
datCounty$rateLCI <- temp$lower
datCounty$rateUCI <- temp$upper

# SMR
tState               <- datState
tState$stateRate     <- tState$cDeathRate
tState               <- tState[,c("year","Level","CAUSE","stateRate")]

# for LOCAL installation of application EXCLUDE save line and INCLUDE load line
save(tState, file= paste0(upPlace,"/upData/tState.R"))
#load(file= paste0(upPlace,"/upData/tState.R"))

datCounty            <- merge(datCounty,tState,by = c("year","Level","CAUSE"))
datCounty$SMR        <- datCounty$cDeathRate / datCounty$stateRate

# Bring in age-adjusted rates
datCounty <- merge(datCounty,ageAdj ,by = c("county","year","CAUSE"),all=TRUE)

# Bring State file into County file 

datCounty <- bind_rows(datState,datCounty)

# "SMALL CELL and "RISKY CAUSE" supression --------------------------------------------------------

#should be <- NA, but then shows up as "highest on ranking..."  fix at some point
datCounty$SMR[datCounty$Ndeaths < 6]                  <- 0
datCounty$SMR[datCounty$county == "CALIFORNIA STATE"] <- 0

xCause0 <- c(14,41,50,139,4,49,192)
xCause1 <- c(xCause0,10)

datTract  <- filter(datTract, !(CAUSE %in% xCause1))
datComm   <- filter(datComm,  !(CAUSE %in% xCause1))
datCounty <- filter(datCounty,!(CAUSE %in% xCause0))

# Output Files ------------------------------------------------------------------------------------

write.csv(datTract,(paste0(upPlace,"/tempOutput/Tract CCB Work.csv")))
write.csv(datComm,(paste0(upPlace,"/tempOutput/Community CCB Work.csv")))
write.csv(datCounty,(paste0(upPlace,"/tempOutput/County CCB Work.csv")))
write.csv(datState,(paste0(upPlace,"/tempOutput/State CCB Work.csv")))

save(datTract,  file= paste0(myPlace,"/myData/datTract.R"))
save(datComm,   file= paste0(myPlace,"/myData/datComm.R"))
save(datCounty, file= paste0(myPlace,"/myData/datCounty.R"))
save(datState,  file= paste0(myPlace,"/myData/datState.R"))

# END
