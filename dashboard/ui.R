# Depowa secretariat complex 092904342, 08164189053
library(DT)
library(shiny)
library(highcharter)
library(magrittr)
library(httr)
library(jsonlite)
library("data.table")
library(googlesheets)
library(dplyr,warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2)
library(formattable)
library(knitr)
library(shinydashboard)
library(lubridate)
library(readr)

install.packages(c("magrittr","httr","jsonlite","data.table","googlesheets","dplyr","ggplot2","formattable","knitr","shinydashboard","lubridate","readr"))
server_version <- "yes" #Enter "yes" or "no"

if(server_version == "yes") {
  setwd("/srv/shiny-server/e4e-apps/MNH_QoC_Dashboard")
} else if (server_version == "no") {
  setwd("C:/Users/totus tuus/Documents/R_projects/MNH_QoC_Dashboard/")
}
p_csv_import <- read.csv(file = "p_csv.csv", row.names = NULL, as.is = TRUE)
s_csv_import <- read.csv(file = "s_csv.csv", row.names = NULL, as.is = TRUE)


sec_metadata_import <- read.csv(file = "dashboard/data/sec_metadata_import.csv", as.is = TRUE)
prim_metadata_import <- read.csv(file = "dashboard/data/prim_metadata_import.csv", as.is = TRUE)

OrgUnitsExport <- read.csv("dashboard/data/MNH_OrgUnitsExport.csv", stringsAsFactors = TRUE)

primDataTemp1 <- left_join(x = p_csv_import, y = OrgUnitsExport[,c("FacilityName","FacilityLabel")], by = c("name_of_facility" = "FacilityName"))
primDataTemp2 <- left_join(x = primDataTemp1, y = OrgUnitsExport[,c("StateName","StateLabel")], by = c("state" = "StateName"))
primDataTemp3 <- left_join(x = primDataTemp2, y = OrgUnitsExport[,c("LGAName","LGALabel")], by = c("local_government_area_lga" = "LGAName"))
primDataTemp4 <- left_join(x = primDataTemp3, y = OrgUnitsExport[,c("SenatorialDistName","SenatorialDistLabel")], by = c("senatorial_district" = "SenatorialDistName"))

secDataTemp1 <- left_join(x = s_csv_import, y = OrgUnitsExport[,c("FacilityName","FacilityLabel")], by = c("FacilitName" = "FacilityName"))
secDataTemp2 <- left_join(x = secDataTemp1, y = OrgUnitsExport[,c("StateName","StateLabel")], by = c("s_StateName00" = "StateName"))
# Skipped secDataTemp3 since there is no senatorial district
secDataTemp4 <- left_join(x = secDataTemp2, y = OrgUnitsExport[,c("LGAName","LGALabel")], by = c("s_LGAName0000" = "LGAName"))

# no longer needed. to be deleted soon.----
# primData_org_units_Names <- names(primDataTemp4)[(ncol(primDataTemp4) - 3):ncol(primDataTemp4)]
# prim_org_units_metadata <- data.frame(Col_Name = primData_org_units_Names, Data_Type = "Factor", Computed.question.name = c("Facility Label", "State Label","LGA Label", "Senatorial District Label"))
# prim_metadata <- rbind(prim_metadata_import, prim_org_units_metadata)
# prim_metadata <- prim_metadata_import

# Specifying the class of each columns in Primary data ----
# Try to refactor the function in this sapply so that there is just a function that is called
primDataTemp5_List <- sapply(X = 1:ncol(primDataTemp4),
                             FUN = function(j) {
                               x <- primDataTemp4[,j]
                               y <- names(primDataTemp4)[j]
                               column_class <- prim_metadata_import[prim_metadata_import$Col_Name == y,2][1]
                               if(column_class == "character") {
                                 as.character(x)
                               } else if(column_class == "logical") {
                                 as.logical.factor(x)
                               } else if(column_class == "factor") {
                                 as.factor(x)
                               } else if(column_class == "numeric") {
                                 as.numeric(x)
                               } else if(column_class == "integer") {
                                 as.integer(x)
                               } else if(column_class == "Date-time") {
                                 ymd_hms(x, quiet = TRUE)
                               } else if(column_class == "Image") {
                                 as.character(x)
                               } else if(column_class == "geo-coordinates") {
                                 as.character(x)
                               } else if(column_class == "Time") {
                                 hm(x, quiet = TRUE)
                               } else if(column_class == "Date") {
                                 ymd(x)
                               } else if(column_class == "CharNum") {
                                 as.numeric(as.character(x))
                               }
                             }, simplify = FALSE, USE.NAMES = TRUE)
# Specifying the class of each columns in Secondary data ----
secDataTemp5_List <- sapply(X = 1:ncol(secDataTemp4),
                            FUN = function(j) {
                              x <- secDataTemp4[,j]
                              y <- names(secDataTemp4)[j]
                              column_class <- sec_metadata_import[sec_metadata_import$Col_Name == y,2][1]
                              if(column_class == "character") {
                                as.character(x)
                              } else if(column_class == "logical") {
                                as.logical.factor(x)
                              } else if(column_class == "factor") {
                                as.factor(x)
                              } else if(column_class == "numeric") {
                                as.numeric(x)
                              } else if(column_class == "integer") {
                                as.integer(x)
                              } else if(column_class == "Date-time") {
                                ymd_hms(x, quiet = TRUE)
                              } else if(column_class == "Image") {
                                as.character(x)
                              } else if(column_class == "geo-coordinates") {
                                as.character(x)
                              } else if(column_class == "Time") {
                                hm(x, quiet = TRUE)
                              } else if(column_class == "Date") {
                                ymd(x)
                              } else if(column_class == "CharNum") {
                                as.numeric(as.character(x))
                              }
                            }, simplify = FALSE, USE.NAMES = TRUE)

# Can be deleted later ----
# ll <- sapply(primDataTemp5_List,length)
# cols_that_didnot_convert <- names(primDataTemp4)[ll == 0]

# converting list versions to data frames ----
primDataTemp5_df <- as.data.frame(primDataTemp5_List, row.names = NULL,stringsAsFactors = FALSE, col.names = names(primDataTemp4))
secDataTemp5_df <- as.data.frame(secDataTemp5_List, row.names = NULL,stringsAsFactors = FALSE, col.names = names(secDataTemp4))

# generating columns from primary data with different data types ----
primData <- primDataTemp5_df
prim_Data_Types <- sapply(primData, class, simplify = TRUE, USE.NAMES = FALSE)
prim_numeric_cols <- prim_metadata_import[prim_Data_Types == "numeric"|prim_Data_Types == "integer",]
prim_factor_cols <- prim_metadata_import[prim_Data_Types == "factor",]
prim_logical_cols <- prim_metadata_import[prim_Data_Types == "logical",]
prim_character_cols <- prim_metadata_import[prim_Data_Types == "character",]
prim_date_cols <- prim_metadata_import[prim_Data_Types == "Date",]
prim_period_cols <- prim_metadata_import[prim_Data_Types == "Period",]
prim_posixct_cols <- prim_metadata_import[prim_Data_Types == "POSIXct",]

# generating columns from secondary data with different data types ----
secData <- secDataTemp5_df
sec_Data_Types <- sapply(secData, class, simplify = TRUE, USE.NAMES = FALSE)
sec_numeric_cols <- sec_metadata_import[sec_Data_Types == "numeric"|sec_Data_Types == "integer",]
sec_factor_cols <- sec_metadata_import[sec_Data_Types == "factor",]
sec_logical_cols <- sec_metadata_import[sec_Data_Types == "logical",]
sec_character_cols <- sec_metadata_import[sec_Data_Types == "character",]
sec_date_cols <- sec_metadata_import[sec_Data_Types == "Date",]
sec_period_cols <- sec_metadata_import[sec_Data_Types == "Period",]
sec_posixct_cols <- sec_metadata_import[sec_Data_Types == "POSIXct",]

# Generating labels from primary data with different data types ----
prim_all_label <- prim_metadata_import$Col_Name
names(prim_all_label) <- prim_metadata_import$Computed.question.name
prim_numeric_label <- prim_numeric_cols$Col_Name
names(prim_numeric_label) <- prim_numeric_cols$Computed.question.name
prim_factor_label <- prim_factor_cols$Col_Name
names(prim_factor_label) <- prim_factor_cols$Computed.question.name
prim_logical_label <- prim_logical_cols$Col_Name
names(prim_logical_label) <- prim_logical_cols$Computed.question.name
prim_character_label <- prim_character_cols$Col_Name
names(prim_character_label) <- prim_character_cols$Computed.question.name
prim_date_label <- prim_date_cols$Col_Name
names(prim_date_label) <- prim_date_cols$Computed.question.name
prim_period_label <- prim_period_cols$Col_Name
names(prim_period_label) <- prim_period_cols$Computed.question.name
prim_posixct_label <- prim_posixct_cols$Col_Name
names(prim_posixct_label) <- prim_posixct_cols$Computed.question.name

# Generating labels from secondary data with different data types ----
# sec_all_label <- sec_metadata_import$Col_Name
# names(sec_all_label) <- sec_metadata_import$Computed.question.name
# sec_numeric_label <- sec_numeric_cols$Col_Name
# names(sec_numeric_label) <- sec_numeric_cols$Computed.question.name
# sec_factor_label <- sec_factor_cols$Col_Name
# names(sec_factor_label) <- sec_factor_cols$Computed.question.name
# sec_logical_label <- sec_logical_cols$Col_Name
# names(sec_logical_label) <- sec_logical_cols$Computed.question.name
# sec_character_label <- sec_character_cols$Col_Name
# names(sec_character_label) <- sec_character_cols$Computed.question.name
# sec_date_label <- sec_date_cols$Col_Name
# names(sec_date_label) <- sec_date_cols$Computed.question.name
# sec_period_label <- sec_period_cols$Col_Name
# names(sec_period_label) <- sec_period_cols$Computed.question.name
# sec_posixct_label <- sec_posixct_cols$Col_Name
# names(sec_posixct_label) <- sec_posixct_cols$Computed.question.name
# Download raw data ----
downloadRawData <- parse("DownloadMNHData.R")

PreSelectedPrimInds <- c("email","username","end00000000","StateLabel","FacilityLabel")
#Start dashboard UI ----
shinyUI(
  dashboardPage(
  # tags$head(tags$script(src = "message-handler.js"), tags$style(type="text/css","body{padding-bottom: 70px;}")),
  dashboardHeader(title = "MNH QoC Dashboard"),
  dashboardSidebar(width = "400px",collapsed = FALSE,
    # Primary labels ----
    fluidRow(column(6, selectInput("prim_all_label", "1st primary variable(x-axis)",prim_all_label, prim_all_label[1])),column(6,selectInput("prim_all_label2", "2nd primary variable(y-axis)",prim_all_label, prim_all_label[2]))),
    fluidRow(column(6, selectInput("prim_numeric_label", "1st primary numeric variable(x-axis)",prim_numeric_label, prim_numeric_label[1])),column(6,selectInput("prim_numeric_label2", "2nd primary numeric variable(y-axis)",prim_numeric_label, prim_numeric_label[2]))),
    fluidRow(column(6, selectInput("prim_factor_label", "1st primary factor variable(x-axis)",prim_factor_label, prim_factor_label[1])),column(6, selectInput("prim_factor_label2", "2nd primary factor variable(y-axis)",prim_factor_label, prim_factor_label[2]))),
    fluidRow(column(6, selectInput("prim_logical_label", "1st primary logical variable(x-axis)",prim_logical_label, prim_logical_label[1])),column(6, selectInput("prim_logical_label2", "2nd primary logical variable(y-axis)",prim_logical_label, prim_logical_label[2]))),
    fluidRow(column(6, selectInput("prim_character_label", "1st primary character variable(x-axis)",prim_character_label, prim_character_label[1])),column(6, selectInput("prim_character_label2", "2nd primary character variable(y-axis)",prim_character_label, prim_character_label[2]))),
    fluidRow(column(6, selectInput("prim_date_label", "1st primary date variable(x-axis)",prim_date_label, prim_date_label[1])),column(6, selectInput("prim_date_label2", "2nd primary date variable(y-axis)",prim_date_label, prim_date_label[2]))),
    fluidRow(column(6, selectInput("prim_period_label", "1st primary period variable(x-axis)",prim_period_label, prim_period_label[1])),column(6, selectInput("prim_period_label2", "2nd primary period variable(y-axis)",prim_period_label, prim_period_label[2]))),
    fluidRow(column(6, selectInput("prim_posixct_label", "1st primary posixct variable(x-axis)",prim_posixct_label, prim_posixct_label[1])),column(6, selectInput("prim_posixct_label2", "2nd primary posixct variable(y-axis)",prim_posixct_label, prim_posixct_label[2]))),
    # Secondary labels ----
    # fluidRow(column(6, selectInput("sec_all_label", "1st primary variable(x-axis)",sec_all_label, sec_all_label[1])),column(6, selectInput("sec_all_label2", "2nd primary variable(y-axis)",sec_all_label, sec_all_label[2]))),
    # fluidRow(column(6, selectInput("sec_numeric_label", "1st primary numeric variable(x-axis)",sec_numeric_label, sec_numeric_label[1])),column(6, selectInput("sec_numeric_label2", "2nd primary numeric variable(y-axis)",sec_numeric_label, sec_numeric_label[2]))),
    # fluidRow(column(6, selectInput("sec_factor_label", "1st primary factor variable(x-axis)",sec_factor_label, sec_factor_label[1])),column(6, selectInput("sec_factor_label2", "2nd primary factor variable(y-axis)",sec_factor_label, sec_factor_label[2]))),
    # fluidRow(column(6, selectInput("sec_logical_label", "1st primary logical variable(x-axis)",sec_logical_label, sec_logical_label[1])),column(6, selectInput("sec_logical_label2", "2nd primary logical variable(y-axis)",sec_logical_label, sec_logical_label[2]))),
    # fluidRow(column(6, selectInput("sec_character_label", "1st primary character variable(x-axis)",sec_character_label, sec_character_label[1])),column(6, selectInput("sec_character_label2", "2nd primary character variable(y-axis)",sec_character_label, sec_character_label[2]))),
    # fluidRow(column(6, selectInput("sec_date_label", "1st primary date variable(x-axis)",sec_date_label, sec_date_label[1])),column(6, selectInput("sec_date_label2", "2nd primary date variable(y-axis)",sec_date_label, sec_date_label[2]))),
    # fluidRow(column(6, selectInput("sec_period_label", "1st primary period variable(x-axis)",sec_period_label, sec_period_label[1])),column(6, selectInput("sec_period_label2", "2nd primary period variable(y-axis)",sec_period_label, sec_period_label[2]))),
    # fluidRow(column(6, selectInput("sec_posixct_label", "1st primary posixct variable(x-axis)",sec_posixct_label, sec_posixct_label[1])),column(6, selectInput("sec_posixct_label2", "2nd primary posixct variable(y-axis)",sec_posixct_label, sec_posixct_label[2]))),
    
    # Other input widgets
    fluidRow(
      column(6, selectInput("type", "Type", c("line","column","bar","spline"))),
      column(6, selectInput("sortData","Sort Data", choices = c("Ascending","Descending","Default"), selected = "Descending"))
    ),
    fluidRow(
      column(6, selectInput("aggr_fn","Aggregate function", choices = c("sum","Count" = "Length", "No. of values" = "NROW", "Distinct Count" = "n_distinct","mean","median","max","min","var","sd","IQR"), selected = "sum"))
    ),
    fluidRow(
      column(6, selectInput("theme","Theme for charts", c(FALSE, "fivethirtyeight", "economist","darkunica","gridlight","sandsignika","null","handdrwran","chalk")))
    )
    ,
    fluidRow(
      column(12, selectizeInput("all_primary_indicators", "Select multiple indicators",c("Select multiple" = "",prim_all_label), multiple = TRUE, selected = PreSelectedPrimInds))
    ),
    
    # Dashboard side bars ----
    sidebarMenu(
      menuItem(text = "Dashboard", tabName = "dashboard", icon = icon("dashboard"),badgeLabel = "new",badgeColor = "blue", selected = TRUE, expandedName = "DashboardExpandedName", startExpanded = FALSE),
        menuSubItem("Dashboard Scatter_Chart", tabName = "Scatter_Chart"),
        menuSubItem("Dashboard Correlation_Matrix", tabName = "Correlation_Matrix"),
        menuSubItem("Dashboard Manual_Table_Maker", tabName = "Manual_Table_Maker"),
        menuSubItem("Dashboard Formatted_table", tabName = "Formatted_table"),
        menuSubItem("Dashboard BaseR_barchart", tabName = "BaseR_barchart"),
      menuItem("Settings"),
      menuItem("Update and Export Data")
    )
  ),
  # Dashboard body ----
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                
              )
            ),
      tabItem(tabName = "Scatter_Chart",
              fluidRow(
                highchartOutput(outputId = "scatterChart")
              ),
              fluidRow(
                highchartOutput(outputId = "hcontainer")
              )
              ),
      tabItem(tabName = "Correlation_Matrix"),
      tabItem(tabName = "Manual_Table_Maker",
              fluidRow(
                formattableOutput("formattedTable")
              )),
      tabItem(tabName = "Formatted_table"),
      tabItem(tabName = "BaseR_barchart")
  ))))
