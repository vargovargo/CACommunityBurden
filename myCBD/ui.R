n1 <- "The goal of the California Community Burden of Disease and Cost Engine is to provide systematic scientific insight for allocation of Public Health resources, evaluation of Public Health interventions, and other Public Health actions. This initial version of the application displays multiple death-related measures (e.g. Years of Life Lost per 100,000 population, crude and age-adjusted death rate, standard mortality ratios) in interactive rankings charts, maps, and trend lines, for California counties, communities (Medical Service Study Areas), and census tracts for 2001 to 2015.  Cause of death groupings are based on the Global Burden of Disease Study.  At the county level, data are displayed separately for each year, 2001 to 2015.  In this release, data at the community or census-tract level are displayed only for 2011 to 2015 combined.  Data for some conditions with very few deaths and/or with other sensitivity considerations are suppressed in this release."

#Real
n2 <- "This app deployment is for preliminary internal CDPH review. Do not share these data with external partners.  A very wide range of enchantments and are being considered for this application. Any/all comments regarding errors, enhancements, or any other ideas about this version are most welcome. Please email michael.samuel@cdph.ca.gov."

#Fake
#n2 <- "NOTE: THIS VERSION OF THE ENGINE IS FOR DEMONSTRATION PURPOSES ONLY - THE DATA ARE NOT REAL - THEY ARE A RANDOM SUBSET OF RANDOMLY DISTORTED DATA" 

fC <- function(vec) {
  tRep <- length(vec)-1
  paste("input.ID == ",vec,    c(rep("|",tRep),""), collapse="")
}

#library(shinythemes)
shinyUI(fluidPage(#theme = "bootstrap.css",
                 # shinythemes::themeSelector(),

tags$h3(mTitle),
  
# CDC Whiskers app
# wellPanel
# navBarPanel 

sidebarPanel( 
 conditionalPanel(condition = fC(c(1,3,6,8)),     selectInput(  "myCAUSE",   "Cause",           choices=causeNum36,selected=104)),
 conditionalPanel(condition = fC(c(1,2,4,5,8)),   selectInput(  "myLHJ",     "County",          choices=lList,selected=1)),
 conditionalPanel(condition = fC(c(1,3,5,6)),     numericInput( "myYear",    "Year",            value=2015,min=2001,max=2015)),
 conditionalPanel(condition = fC(c(2,4,5,7)),     numericInput( "myN",       "How many",        value=10,min=1,max=50)),
 conditionalPanel(condition = fC(c(1,3,5,6,7,8)), selectInput(  "myMeasure", "What Measure",    choices=lMeasures,selected="YLLper")),
 conditionalPanel(condition = fC(c(1,3)),         selectInput(  "myGeo",     "Geo Level",       choices=c("County","Community","Census Tract"))),
 conditionalPanel(condition = fC(c(3)),           checkboxInput("cZoom",     "Zoom to County",  value=FALSE)),
 conditionalPanel(condition = fC(c(6)),           checkboxInput("gZoom",     "Comunity level",  value=FALSE)),
 conditionalPanel(condition = fC(c(6)),           checkboxInput("myCI",      "95% CIs?",        value=TRUE)),

 conditionalPanel(condition = paste("(input.cZoom | input.gZoom) && ", fC(c(3,6))), selectInput(   "myLHJX",    "County?",               choices=lList,selected=1)),
 conditionalPanel(condition = fC(c(3)),                                             checkboxInput( "myLabName", "Geo Names",             value=FALSE)),
 conditionalPanel(condition = fC(c(1,3)),                                           checkboxInput(  "myCon",    "State-based cutpoints", value=TRUE)),

 hr(), 

 helpText(n1), br(),
 helpText(n2,style="color:red"),br(),
 helpText("LINKS",tags$a(href="https://www.cdph.ca.gov/Programs/CHSI/Pages/Data-and-Statistics-.aspx", h6("California Death Data")),
             tags$a(href="https://data.chhs.ca.gov/",
                    h6("California Health and Human Service Agency Open Data Portal")),
             tags$a(href="https://www.census.gov/programs-surveys/acs/",
                    h6("American Community Survey")),  style="color:blue"),
   br(),
   helpText("DEFINITIONS",
     h6("YLL:  Years of Life Lost"),
     h6("SMR:  Standard Mortality Ratio (Local Rate/State Rate)"),
     h6("Community: Medical Service Study Areas (MSSA)"),
     tags$a(href="https://oshpd.ca.gov/HWDD/MSSA.html",h6("MSSA Info")),
     style="color:blue"),
   br(),
   HTML('<center><img src="cdph2.gif" height="85" width="100"></center>')
   #img(src='cdph2.gif',width = "100px", height = "85px", align = "center")  # , align = "center"
 
 ),
 
mainPanel(
  hr(), 
  tabsetPanel(type = "tabs",
   tabPanel("Map (static)",      plotOutput("cbdMap1",  width=700,height=700), value =3),
   tabPanel("Map (interactive)", htmlOutput("map_title"),leafletOutput("cbdMap0",height=700), value =1),
   tabPanel("Rank Causes",       plotOutput("rankCause",width=700,height=700), value =5),
   tabPanel("Rank Geographies",  plotOutput("rankGeo",  width=700,height=700), value =6),
   tabPanel("Trend",             plotOutput("trend",    width=700,height=700), value =8),
   
   id="ID")       ) 
 
))

