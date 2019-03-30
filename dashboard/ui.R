# Depowa secretariat complex 092904342, 08164189053
# Load the libraries ----
library(DT)
library(shiny)
library(highcharter)
library(magrittr)
library(httr)
library(jsonlite)
library("data.table")
library(googlesheets)
library(tidyr)
library(dplyr,warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2)
library(formattable)
library(knitr)
library(shinydashboard)
library(lubridate)
library(readr)

# later use yaml to specify the path ----
config <- config::get()
server_version <- config$ServerVersion
# server_version <- "no" #Enter "yes" or "no"
# Set path variable ----
if(server_version == "yes") {
  setwd("/srv/shiny-server/e4e-apps/MNH_QoC_Dashboard")
} else if (server_version == "no") {
  setwd("C:/Users/totus tuus/Documents/R_projects/MNH_QoC_Dashboard/")}
p_csv_import <- read.csv(file = "p_csv.csv", row.names = NULL, as.is = TRUE)
s_csv_import <- read.csv(file = "s_csv.csv", row.names = NULL, as.is = TRUE)
source("dashboard/hchart_cor.R")

sec_metadata_import <- read.csv(file = "dashboard/data/sec_metadata_import.csv", as.is = TRUE)
prim_metadata_import <- read.csv(file = "dashboard/data/prim_metadata_import.csv", as.is = TRUE)

OrgUnitsExport <- read.csv("dashboard/data/MNH_OrgUnitsExport.csv", stringsAsFactors = TRUE)
# Add orgUnits to data ----
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
sec_all_label <- sec_metadata_import$Col_Name
names(sec_all_label) <- sec_metadata_import$Computed.question.name
sec_numeric_label <- sec_numeric_cols$Col_Name
names(sec_numeric_label) <- sec_numeric_cols$Computed.question.name
sec_factor_label <- sec_factor_cols$Col_Name
names(sec_factor_label) <- sec_factor_cols$Computed.question.name
sec_logical_label <- sec_logical_cols$Col_Name
names(sec_logical_label) <- sec_logical_cols$Computed.question.name
sec_character_label <- sec_character_cols$Col_Name
names(sec_character_label) <- sec_character_cols$Computed.question.name
sec_date_label <- sec_date_cols$Col_Name
names(sec_date_label) <- sec_date_cols$Computed.question.name
sec_period_label <- sec_period_cols$Col_Name
names(sec_period_label) <- sec_period_cols$Computed.question.name
sec_posixct_label <- sec_posixct_cols$Col_Name
names(sec_posixct_label) <- sec_posixct_cols$Computed.question.name
# Download raw data ----
# downloadRawData <- parse("DownloadMNHData.R")


allObjs <- c("DashboardExpandedName","colours","sidebarMenu","theme","expandBookMarkOpts","bookmarkType","objBookmarks","includeExcluedBk",
             "type","sortData","aggr_fn","prim_all_label","prim_numeric_label","prim_factor_label","prim_logical_label","prim_character_label",
             "prim_date_label","prim_period_label","prim_posixct_label","prim_numeric_label2","prim_correlation_variables","correlation_use",
             "correlation_method","aggr_data_quest","prim_group_by_var","summarize_aggregate_prim","all_primary_indicators")

PreSelectedPrimInds <- c("StateLabel","FacilityLabel","how_many_people_live_in_the_cat")
PreSelectedSecInds <- c("StateLabel","FacilityLabel","how_many_people_live_in_the_cat")

# UIside custom scripts ----
colours <- c("blue","red","black","orange","yellow","purple","brown", "pink")



#Start dashboard UI ----
ui <- function(request) {shinyUI(
  # tags$head(tags$script(src = "dashboard/www/countries/us/custom/us-all/us-all.js")),  
  dashboardPage(
  # tags$head(tags$script(src = "message-handler.js"), tags$style(type="text/css","body{padding-bottom: 70px;}")),
    # tags$script(src = "https://code.highcharts.com/mapdata/custom/world-palestine-highres.js"),
    header = dashboardHeader(title = span(
    tags$img(src = "Coat_of_arms_of_Nigeria.png", width = "50px", height = "50px"),
   "MNH QoC Dashboard", style = "font-family: Roboto; font-weight: bold"
  ), titleWidth = "400px",
  # Drop-down menus ----
  dropdownMenu(
    type = "messages",
    badgeStatus = "primary",
    icon = icon("comments"),
    messageItem(from = "Admin", message = "Development still in progress")
  ),
  dropdownMenu(
    type = "tasks",
    badgeStatus = "success",
    icon = icon("check-square"),
    taskItem(text = "Set-up correlation analysis", value = 100, color = "green"),
    taskItem(text = "Set-up table maker and summariser", value = 100, color = "green"),
    taskItem(text = "Set-up Map view", value = 50, color = "yellow"),
    taskItem(text = "Develop grading system to grade issues", value = 15, color = "red")
  )
  ),
  # sidebar menu ----
  sidebar = dashboardSidebar(width = "400px",collapsed = FALSE,
      fluidPage(column(12,sidebarSearchForm("textId", "buttonId", label = "Search MNH...",
                                            icon = shiny::icon("search")))) ,
     # Dashboard side bars ----
     sidebarMenu(id = "sidebarMenu",
       # menuItem(text = "Dashboard", tabName = "dashboard", icon = icon("dashboard"),badgeLabel = "new",badgeColor = "blue", selected = TRUE, expandedName = "DashboardExpandedName", startExpanded = FALSE),
       #   menuSubItem("Scatter Chart", tabName = "Scatter_Chart"),
       fluidRow(column(12, radioButtons("dashboardLevel",label = NULL, choiceNames = list(
         HTML("<p style='color:yellow;'>Primary</p>"),
         HTML("<p style='color:blue;'>Secondary/Tertiary</p>")
       ), choiceValues = list("Primary","Secondary"),inline = TRUE, width = "100%"))),
        menuItem(text = "Dashboard",
                menuSubItem("Scatter Chart", tabName = "Scatter_Chart", icon = icon("plus-square")),
                menuSubItem("Correlation Matrix", tabName = "Correlation_Matrix",icon = icon("check-circle", lib = "font-awesome")),
                menuSubItem("Manual Table Maker", tabName = "Manual_Table_Maker",icon = icon("table")),
                menuSubItem("Summary of MNH QoC Conducted", tabName = "Formatted_table", icon = icon("list-alt")),
                menuSubItem("Analysis Chart Primary & Secondary", tabName = "Prim_Sec_Barchart", icon = icon("bar-chart-o")),
                menuSubItem("Analysis Chart", tabName = "Barchart", icon = icon("bar-chart-o")),
                menuSubItem("Maps", tabName = "Maps", icon = icon("map"), selected = TRUE),
                tabName = "dashboard", icon = icon("dashboard"), selected = TRUE, expandedName = "DashboardExpandedName", startExpanded = FALSE),  
       hr(),
       # Other menu items ----
       menuItem("Settings",
                
                fluidRow(column(6, selectInput("colours","Select colours",colours,"blue")),
                conditionalPanel(condition = "input['sidebarMenu'] != 'Manual_Table_Maker'",
                                 column(6, selectInput("theme","Theme for charts", c(FALSE, "fivethirtyeight", "economist","darkunica","gridlight","sandsignika","null","handdrwran","chalk")))),
                
                fluidRow(column(12,checkboxInput('expandBookMarkOpts','Check to show bookmark options', value = FALSE, width = "100%"))),
                conditionalPanel(condition = "input.expandBookMarkOpts == true", 
                                 selectizeInput(inputId = "bookmarkType",label = "Type of bookmark", choices = c("server","url"),options = list(create = TRUE,placeholder = "Select type of bookmark")),
                                 selectizeInput(inputId = "objBookmarks", label = "Objects",choices = allObjs,selected = NULL, multiple = TRUE),
                                 selectInput("includeExcluedBk","Select one",choices = c("Include to bookmark","Exclude from bookmark")),
                                 bookmarkButton(title = "Save the current state of the dashboard", label = "Bookmark dashboard"),
                                 textOutput(outputId = "myprint"),
                                 br(),
                                 textOutput(outputId = "lastSaved"))
                ), icon = icon("cog", lib = "glyphicon")),
       hr(),
       menuItem("Update and Export Data",icon = icon("refresh", lib="font-awesome"),badgeLabel = "not-active yet",badgeColor = "red")
       # ,menuItem("Resources", tabName = "Resources", icon = icon("book"))
     ),hr(),h5("Chart Controls."),
     # Chart controls ----
    fluidRow(
    conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Formatted_table'",
      column(6, selectInput("type", "Type", c("line","column","bar","spline"), "bar"))),
    conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Formatted_table'",
      column(6, selectInput("sortData","Sort Data", choices = c("Ascending","Descending","Default"), selected = "Descending"))),
    conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Formatted_table'",
    column(6, selectInput("aggr_fn","Aggregate function", choices = c("sum","Count" = "Length", "No. of values" = "NROW", "Distinct Count" = "n_distinct","mean","median","max","min","var","sd","IQR"), selected = "n_distinct"))),  
    # Primary labels ----
    conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Barchart' && input['sidebarMenu'] != 'Formatted_table'",
    column(6, selectInput("prim_all_label", "Primary variable",prim_all_label, prim_all_label[1]))),
    conditionalPanel(condition = "input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Formatted_table'",
    column(6, selectInput("prim_numeric_label", "Primary numeric variable",prim_numeric_label, prim_numeric_label[8]))),
    conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Barchart'",
    column(6, selectInput("prim_factor_label", "Primary factor variable",prim_factor_label, prim_factor_label[699]))),
    conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Barchart' && input['sidebarMenu'] != 'Formatted_table'",
    column(6, selectInput("prim_logical_label", "Primary logical variable",prim_logical_label, prim_logical_label[1]))),
    conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Barchart' && input['sidebarMenu'] != 'Formatted_table'",
    column(6, selectInput("prim_character_label", "Primary character variable",prim_character_label, prim_character_label[1]))),
    conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Barchart' && input['sidebarMenu'] != 'Formatted_table'",
    column(6, selectInput("prim_date_label", "Primary date variable",prim_date_label, prim_date_label[1]))),
    conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Barchart' && input['sidebarMenu'] != 'Formatted_table'",
    column(6, selectInput("prim_period_label", "Primary period variable",prim_period_label, prim_period_label[1]))),
    conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Barchart' && input['sidebarMenu'] != 'Formatted_table'",
    column(6, selectInput("prim_posixct_label", "Primary posixct variable",prim_posixct_label, prim_posixct_label[1])))
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
    
    )
  ),
  
  
  # Dashboard body ----
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",status = "primary",
              fluidRow(h1("Welcome to the MNH Quality of Care Dashboard for analysis"))
            ),
      tabItem(tabName = "Scatter_Chart",fluidRow(
        fluidRow(column(12, selectInput("prim_numeric_label2", "2nd primary numeric variable(y-axis)",prim_numeric_label, prim_numeric_label[2],width = "100%"))),
              highchartOutput(outputId = "scatterChart"))),
      tabItem(tabName = "Correlation_Matrix",
              fluidRow(column(12, selectInput("prim_correlation_variables", "Select variables to compare",prim_numeric_label, c(prim_numeric_label[8],prim_numeric_label[9],prim_numeric_label[10]), multiple = TRUE, width = "100%"))),
              fluidRow(
                column(6, selectInput("correlation_use","Method in using NAs to compute covariance?",c("everything", "all.obs","complete.obs","na.or.complete","pairwise.complete.obs"), selected = "complete.obs")),
                column(6, selectInput("correlation_method","Correlation coefficient to compute", c("pearson","kendall","spearman")))),
              highchartOutput("correlationMatrix")),
      tabItem(tabName = "Manual_Table_Maker",
              fluidRow(
              column(4, selectInput("aggr_data_quest", "Aggregage data?", c("Yes","No"), "No")),
              conditionalPanel(condition = "input['aggr_data_quest'] == 'Yes'",
              column(4, selectInput("prim_group_by_var", "Select factor variable to group by",prim_factor_label, prim_factor_label[699])),
              column(4, selectInput("summarize_aggregate_prim", "Variable to summarize with",prim_all_label[prim_all_label %in% prim_numeric_label]))
              )),
              fluidRow(column(12, selectizeInput("all_primary_indicators", "Select multiple indicators",c("Select multiple" = "",prim_all_label), multiple = TRUE, selected = PreSelectedPrimInds, width = "100%"))),
              shiny::dataTableOutput("manualDTMaker")
              ),
      tabItem(tabName = "Formatted_table",
              formattableOutput("formattedTable")),
      tabItem(tabName = "Barchart",
              highchartOutput("columnChartPrim"),
              selectInput("prim_factor_label_analysis_chart", "Primary factor variable",prim_factor_label, prim_factor_label[699]),
              highchartOutput("IndicatorBarChart2")),
      tabItem(tabName = "Prim_Sec_Barchart", 
              fluidRow(column(6, fluidRow(column(6,
                              selectInput("temp_prim_factor_label_analysis_chart", "Primary factor variable",prim_factor_label, prim_factor_label[699])),
                              column(6, selectInput("prim_numeric_label_temp", "Primary numeric variable",prim_numeric_label, "X_duration"))),
                              highchartOutput("IndicatorBarChart2_Prim_temp"),
                              style='margin-bottom:0px;border-right:1px solid; padding: 0px;'),
              column(6, fluidRow(column(6,
                     selectInput("temp_sec_factor_label_analysis_chart", "Secondary factor variable",sec_factor_label, sec_factor_label[1022])),
                     column(6, selectInput("sec_numeric_label_temp", "Secondary numeric variable",sec_numeric_label, "X_duration"))),
                     highchartOutput("IndicatorBarChart2_Sec_temp"),style='margin-bottom:0px;padding: 0px;')))
      
      
      # ,tabItem(tabName = "Resources","pdf document",hr(), tags$iframe(style = "height:350px; width:100%;scrolling=yes", src = "Primary_ Assessment_Tool.pdf"),
      #          br(),"Video",hr()
      #          # ,tags$iframe(style = "height: 600px; width: 100%; scrolling = yes", src = "data_types.mp4")
      #          ,tags$video(src = "data_types.mp4",width = "500px",height="350px",type="video/mp4", controls = "controls"),
      #          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/NBys4XMi7QE" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
      #          )
      ,tabItem(tabName = "Maps", 
              selectInput("prim_numeric_label_maps", "Select primary numeric variable",c("Dist of MNH conducted",prim_numeric_label), selected = prim_numeric_label[6]),
              highchartOutput(outputId = "Maps"))
  )),
  title = "Analysis of MNH QoC Assessments",
  skin = "green"))
}