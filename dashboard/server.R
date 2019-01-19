

















# First set-up a way of sharing data between ui.R and server.R before migrating to yaml Later change to use yaml
# later use yaml to specify the path ----
server_version <- "no" #Enter "yes" or "no"
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





# shiny server function ----
shinyServer(function(input, output, session,data) {
  # reactiveDataSetExpr logic ----
  reactiveDataSetExpr <- reactive({
    if(input$dashboardLevel == "Primary") {
      tempData <- primData
    } else if(input$dashboardLevel == "Secondary") {
      tempData <- secData
    }
    factor2 <- as.data.frame(table(tempData[,input$prim_factor_label]))
    names(factor2) <- c(input$prim_factor_label, "Frequency")
    # factor2_arranged <-  arrange(factor2, desc(Frequency))
    data.frame(factor2[!(factor2[,1] == ""),], row.names = NULL)
  })
  # formattedTable logic ----
  output$formattedTable <- formattable::renderFormattable(expr = {
    df <- reactiveDataSetExpr()
    df <- arrange(df, desc(Frequency))
    df$Proportion <- df$Frequency/sum(df$Frequency)
    df$Icon <- df$Frequency
    formatted_table <- 
      formattable(x = df,
                  formatters = 
                    list(
                      StateLabel = 
                        formatter(
                          "span",
                          style = x ~ ifelse(
                            x == "Federal Capital Territory",
                            style(color = "green", font.weight = "bold"),NA
                          )
                        ),
                      Frequency = normalize_bar(color = "pink",0),
                      Proportion = 
                        formatter(
                          .tag = "span",
                          style = x ~ style(color = ifelse(test = rank(-x) <= 3,"green",ifelse(rank(x) <= 3, "red","orange"))),
                          x ~ sprintf("%.2f%% (rank: %02d)", x * 100, rank(-x,na.last = "keep", ties.method = "min"))
                        ),
                      Icon = 
                        formatter(
                          .tag = "span",
                          style = x ~ style(color = ifelse(x, "green","red")),
                          x ~ icontext(
                            lapply(df$Frequency, function(x) {
                              rep("star",x)
                            }),
                            df$Frequency)
                        )
                    ),
                  align = c("l","l","r","l")
                  )
    formatted_table
  })
  
  # scatterChart logic ----
  # abstract out colour blue and chart type e.g.scatter,line,bar
  output$scatterChart <- renderHighchart({
    if(input$dashboardLevel == "Primary") {
      tempData <- primData
    } else if(input$dashboardLevel == "Secondary") {
      tempData <- secData
    }
    #This looks not accurate that is miny or maxy
    miny <- min(tempData[,input$prim_numeric_label2], na.rm = TRUE)
    maxy <- max(tempData[,input$prim_numeric_label2], na.rm = TRUE)
    range = maxy - miny
    data_series_list <- lapply(c(1:nrow(tempData)), function(x){
      vect <- c(list(tempData[x,input$prim_numeric_label]), list(tempData[x,input$prim_numeric_label2]))
      vect
    })
    scatterChart_plot <- highchart() %>% 
      hc_chart(type = "scatter") %>% 
      hc_title(text = "Scatter plot") %>% 
      hc_add_series(data = data_series_list, type = "scatter", color = input$colours, name = paste0(input$prim_numeric_label,"(x) Vs ",input$prim_numeric_label2, "(y)")) %>% 
      hc_xAxis(title = list(text = input$prim_numeric_label),
               opposite = FALSE,
               plotLines = list(
                 list(label = list(text = "Plotline"),
                      color = "#FF0000",
                      width = 2,
                      value = c(10))
               )) %>% 
      hc_yAxis(title = list(text = input$prim_numeric_label2),
               opposite = TRUE,
               minorTickInterval = "auto",
               minorGridLineDashStyle = "LongDashDotDotDotDotDot",
               showFirstLabel = FALSE,
               showLastLabel = TRUE,
               plotBands = list(
                 list(from = (miny + (range/4)),to = (maxy - (range/4)), color = "rgba(100,0,0,0.1)",
                      label = list(text = "PlotBand"))
               ))
    if(input$theme != FALSE) {
      theme <- themeExpr()
      scatterChart_plot <- scatterChart_plot %>% hc_add_theme(theme)  
    }
    scatterChart_plot
  })
  
  themeExpr <- reactive({
    switch(input$theme,
           null = hc_theme_null(),
           darkunica = hc_theme_darkunica(),
           gridlight = hc_theme_gridlight(),
           sandsignika = hc_theme_sandsignika(),
           fivethirtyeight = hc_theme_538(),
           economist = hc_theme_economist(),
           chalk = hc_theme_chalk(),
           handdrwran = hc_theme_handdrawn()
    )
  })
  output$manualDTMaker <- shiny::renderDataTable(expr = {
    summarize_sym <- rlang::sym(input$prim_group_by_var)
    summarize_aggregate_sym <- rlang::sym(input$summarize_aggregate_prim)
    myAggregate <- rlang::sym("MyAggregateCol")
    selected_Inds <- input$all_primary_indicators
    if(input$dashboardLevel == "Primary") {
      mydt <- primData[,c(prim_all_label[1],selected_Inds)]
    } else if(input$dashboardLevel == "Secondary") {
      mydt <- secData[,c(sec_all_label[1],selected_Inds)]
    }
    
    if(input$aggr_data_quest == "Yes") {
      mydt <- mydt %>% group_by(!!summarize_sym)
      mydt <- summarise(mydt, Aggregate = do.call(what = paste0(input$aggr_fn), args = list(!!summarize_aggregate_sym, na.rm = TRUE)))
      if(input$sortData == "Ascending") {
        mydt <- arrange(mydt, Aggregate)
      } else if(input$sortData == "Descending") {
        mydt <- arrange(mydt, desc(Aggregate))
      }
      names(mydt) <- c(names(mydt)[1],paste0(input$aggr_fn, " of ", input$summarize_aggregate_prim, " by ", input$prim_group_by_var))
      } #use updateselectinput
    mydt
    }, searchDelay = 2000, options = list(height = 99999, pageLength = 10))
  
  observe({
    x <- input$all_primary_indicators
    # can use character(0) to remove all choices
    if(is.null(x))
      x <- character(0)
    # Can also set the label and select items
    if(input$dashboardLevel == "Primary") {
      temp_numeric_label <- prim_numeric_label
    } else if(input$dashboardLevel == "Secondary") {
      temp_numeric_label <- sec_numeric_label
    }
    y <- x[x %in% temp_numeric_label]
    updateSelectInput(session = session, inputId = "summarize_aggregate_prim",label =  "Numeric Variable to summarize with", choices = y, selected = tail(y,1))
  })
  
  # Bar chart ----
  output$IndicatorBarChart2 <- renderHighchart({
    my_sym <- rlang::sym(input$prim_numeric_label)
    my_fac_ind <- rlang::sym(input$prim_factor_label_analysis_chart)
    if(input$dashboardLevel == "Primary") {
      tempData <- primData
    } else if(input$dashboardLevel == "Secondary") {
      tempData <- secData
    }
    dtt2 <- tempData[c(input$prim_factor_label_analysis_chart, input$prim_numeric_label)]
    dtt2 <- dtt2[!is.na(dtt2[,1]),]
    dtt2 <- dtt2 %>% group_by(!!my_fac_ind)
    dtt2 <- summarise(dtt2, Aggregate = do.call(what = paste0(input$aggr_fn), args = list(!!my_sym, na.rm = TRUE)))
    if(input$sortData == "Ascending") {
      dtt2 <- arrange(dtt2, Aggregate)
    } else if(input$sortData == "Descending") {
      dtt2 <- arrange(dtt2, desc(Aggregate))
    }
    bar_chart <- highchart() %>% hc_xAxis(categories = dtt2[[1]]) %>% hc_add_series(dtt2[[2]], type = input$type,dataLabels = list(enabled = TRUE), name = paste(input$dashboardLevel, input$prim_factor_label_analysis_chart, sep = " - " )) %>% 
      hc_colors(input$colours) %>% hc_title(text = "Analysis of MNH Submissions") %>% hc_subtitle(text = input$dashboardLevel)
    if(input$theme != FALSE) {
      theme <- themeExpr()
      bar_chart <- bar_chart %>% hc_add_theme(theme)  
    }
    bar_chart
  })
  
  # Bar chart primary temp ----
  output$IndicatorBarChart2_Prim_temp <- renderHighchart({
    my_sym <- rlang::sym(input$prim_numeric_label_temp)
    my_fac_ind <- rlang::sym(input$temp_prim_factor_label_analysis_chart)
    
    dtt2 <- primData[c(input$temp_prim_factor_label_analysis_chart, input$prim_numeric_label_temp)]
    dtt2 <- dtt2[!is.na(dtt2[,1]),]
    dtt2 <- dtt2 %>% group_by(!!my_fac_ind)
    dtt2 <- summarise(dtt2, Aggregate = do.call(what = paste0(input$aggr_fn), args = list(!!my_sym, na.rm = TRUE)))
    if(input$sortData == "Ascending") {
      dtt2 <- arrange(dtt2, Aggregate)
    } else if(input$sortData == "Descending") {
      dtt2 <- arrange(dtt2, desc(Aggregate))
    }
    bar_chart <- highchart() %>% hc_xAxis(categories = dtt2[[1]]) %>% hc_add_series(dtt2[[2]], type = input$type,dataLabels = list(enabled = TRUE), name = paste("Primary", input$temp_prim_factor_label_analysis_chart, sep = " - " )) %>% 
      hc_colors(input$colours) %>% hc_title(text = "Analysis of MNH Submissions") %>% hc_subtitle(text = "Primary")
    if(input$theme != FALSE) {
      theme <- themeExpr()
      bar_chart <- bar_chart %>% hc_add_theme(theme)  
    }
    bar_chart
  })
  # Bar chart secondary temp ----
  output$IndicatorBarChart2_Sec_temp <- renderHighchart({
    my_sym <- rlang::sym(input$sec_numeric_label_temp)
    my_fac_ind <- rlang::sym(input$temp_sec_factor_label_analysis_chart)
 
    dtt2 <- secData[c(input$temp_sec_factor_label_analysis_chart, input$sec_numeric_label_temp)]
    dtt2 <- dtt2[!is.na(dtt2[,1]),]
    dtt2 <- dtt2 %>% group_by(!!my_fac_ind)
    dtt2 <- summarise(dtt2, Aggregate = do.call(what = paste0(input$aggr_fn), args = list(!!my_sym, na.rm = TRUE)))
    if(input$sortData == "Ascending") {
      dtt2 <- arrange(dtt2, Aggregate)
    } else if(input$sortData == "Descending") {
      dtt2 <- arrange(dtt2, desc(Aggregate))
    }
    bar_chart <- highchart() %>% hc_xAxis(categories = dtt2[[1]]) %>% hc_add_series(dtt2[[2]], type = input$type,dataLabels = list(enabled = TRUE), name = paste("Secondary", input$temp_sec_factor_label_analysis_chart, sep = " - " )) %>% 
      hc_colors(input$colours) %>% hc_title(text = "Analysis of MNH Submissions") %>% hc_subtitle(text = "Secondary")
    if(input$theme != FALSE) {
      theme <- themeExpr()
      bar_chart <- bar_chart %>% hc_add_theme(theme)  
    }
    bar_chart
  })
  output$columnChartPrim <- renderHighchart({
  ts <- primData[,c(input$prim_all_label,"StateLabel")]
  names(ts)[1] <- "start000000"
  ts <- ts[!is.na(ts[,1]),]
  
  ts$start000000 <- ymd(format(ymd_hms(ts$start000000), "%Y/%m/%e"))
  ts_grouped <- group_by(ts, start000000) %>% summarise(count = dplyr::n())
  names(ts_grouped)[2] <- "Number_of_submissions" 
  ts_grouped$start000000 <- as.Date(ts_grouped$start000000)
  
  highchart() %>% 
    hc_chart(type = "column") %>% 
    hc_xAxis(categories = ts_grouped$start000000) %>% 
    hc_add_series(data = ts_grouped$Number_of_submissions, dataLabels = list(enabled = TRUE), 
                  name = "Number of daily assessments in PHCs",colorByPoint = FALSE)%>% 
    hc_add_theme(hc_theme_google())
  })

  output$correlationMatrix <- renderHighchart({
    if(input$dashboardLevel == "Primary") {
      tempData <- primData
    } else if(input$dashboardLevel == "Secondary") {
      tempData <- secData
    }
    myData <- cor(tempData[,input$prim_correlation_variables], use = input$correlation_use, method = input$correlation_method)
    correlation_matrix <- highchart() %>% hc_add_series(cor(myData))
    correlation_matrix <- hchart.cor(cor(myData))
    if(input$theme != FALSE) {
      theme <- themeExpr()
      correlation_matrix <- correlation_matrix %>% hc_add_theme(theme)  
    }
    correlation_matrix
  })
  # NOTifications ----
  # output$notifications < renderMenu({
  #   dropdownMenu(
  #     type = "notifications",
  #     badgeStatus = "Warning",
  #     icon = icon("exclamation-circle"),
  #     notificationItem(
  #       text = paste("You have selected to sort in the order of"),
  #       status = "warning"
  #     )
  #   )
  # }
  # )
  
  # Map logic ----
  output$Maps <- renderHighchart({
        NigeriaStates <- data.frame(code = c("Abia", "Adamawa", 
                                         "Akwa Ibom", "Anambra", "Bauchi", "Bayelsa", "Benue", "Borno", 
                                         "Cross River", "Delta", "Ebonyi", "Edo", "Ekiti", "Enugu", "Federal Capital Territory", 
                                         "Gombe", "Imo", "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", 
                                         "Kogi", "Kwara", "Lagos", "Nassarawa", "Niger", "Ogun", "Ondo", 
                                         "Osun", "Oyo", "Plateau", "Rivers", "Sokoto", "Taraba", "Yobe", 
                                         "Zamfara"), MNHStateNames = c('Abia','Adamawa','Akwa Ibom','Anambra','Bauchi','Bayelsa','Benue','Borno','Cross River','Delta','Ebonyi','Edo','Ekiti','Enugu','Federal Capital Territory','Gombe','Imo','Jigawa','Kaduna','Kano','Katsina','Kebbi','Kogi','Kwara','Lagos','Nasarawa','Niger','Ogun','Ondo','Osun','Oyo','Plateau','Rivers','Sokoto','Taraba','Yobe','Zamfara'))
    if(input$prim_numeric_label_maps == "Dist of MNH conducted") {
      df <- reactiveDataSetExpr()
      names(df) <- c("MNHStateNames","Aggregate")
      df_map <- left_join(x = NigeriaStates, y = df, by = "MNHStateNames",copy = TRUE)
      my_map_data <- df_map
    } else {
      if(input$dashboardLevel == "Primary") {
        tempData <- primData
        temp_numeric_label <- prim_numeric_label
      } else if(input$dashboardLevel == "Secondary") {
        tempData <- secData
        temp_numeric_label <- sec_numeric_label
      }
      PrimData_Map <- left_join(x = NigeriaStates, y = tempData[,c("StateLabel","FacilityLabel","LGALabel",temp_numeric_label)], by = c("MNHStateNames" = "StateLabel"), copy = TRUE)     
      summarize_aggregate_sym <- rlang::sym(input$prim_numeric_label_maps)
      PrimData_Map_Summarised <- PrimData_Map %>% group_by(code)
      PrimData_Map_Summarised <- summarise(PrimData_Map_Summarised, Aggregate = do.call(what = paste0(input$aggr_fn), args = list(!!summarize_aggregate_sym, na.rm = TRUE)))
      
      my_map_data <- PrimData_Map_Summarised
    }
    hcmap("countries/ng/ng-all",download_map_data = FALSE, data = my_map_data, value = "Aggregate",
          joinBy = c("woe-name", "code"), name = input$prim_numeric_label_maps,
          dataLabels = list(enabled = TRUE, format = "{point.name}"),
          borderColor = "#FAFAFA", borderWidth = 0.1, 
          tooltip = list(valueDecimals = 2, valuePrefix = "", 
                         valueSuffix = ""))
  })
  
  # Update select inputs ----
  observeEvent(eventExpr = {input$dashboardLevel}, handlerExpr = {
    if(input$dashboardLevel == "Secondary") {
      all_label <- "Secondary variable"
      all_choice <- sec_all_label
      numeric_label <- "Secondary numeric variable"
      numeric_choice <- sec_numeric_label
      factor_label <- "Secondary factor variable"
      factor_choice <- sec_factor_label
      numeric_label2 <- "2nd Secondary numeric variable(y-axis)"
      numeric_choice2 <- sec_numeric_label
      correlation_label <- "Select secondary variables to compare"
      correlation_choice <- sec_numeric_label
      group_by_label <- "Select secondary factor variable to group by"
      group_by_choice <- sec_factor_label
      summarize_aggregrate_label <- "Sec Variable to summarize with"
      summarize_aggregrate_choice <- sec_all_label
      all_indicators_label <- "Select multiple secondary indicators"
      all_indicators_choice <- sec_all_label
      all_indicators_selected <- PreSelectedSecInds
      numeric_maps_label <- "Select secondary numeric variable"
      numeric_maps_choice <- sec_numeric_label
      pre_selected_factor <- 1022
      pre_selected_numeric2 <- 9
    } else if(input$dashboardLevel == "Primary") {
      all_label <- "Primary variable"
      all_choice <- prim_all_label
      numeric_label <- "Primary numeric variable"
      numeric_choice <- prim_numeric_label
      factor_label <- "Primary factor variable"
      factor_choice <- prim_factor_label
      numeric_label2 <- "2nd primary numeric variable(y-axis)"
      numeric_choice2 <- prim_numeric_label
      correlation_label <- "Select primary variables to compare"
      correlation_choice <- prim_numeric_label
      group_by_label <- "Select primary factor variable to group by"
      group_by_choice <- prim_factor_label
      summarize_aggregrate_label <- "Prim Variable to summarize with"
      summarize_aggregrate_choice <- prim_all_label
      all_indicators_label <- "Select multiple primary indicators"
      all_indicators_choice <- prim_all_label
      all_indicators_selected <- PreSelectedPrimInds
      numeric_maps_label <- "Select primary numeric variable"
      numeric_maps_choice <- prim_numeric_label
      pre_selected_factor <- 699
      pre_selected_numeric2 <- 80 #initially 2
    }
    # which(sec_factor_label == prim_factor_label[699])
    updateSelectInput(session = session, "prim_all_label", all_label,all_choice, all_choice[1])
    updateSelectInput(session = session, "prim_numeric_label", numeric_label,numeric_choice, numeric_choice[8])
    updateSelectInput(session = session, "prim_factor_label", factor_label,factor_choice, factor_choice[pre_selected_factor])
    updateSelectInput(session = session, "prim_numeric_label2", numeric_label2,numeric_choice2, numeric_choice2[pre_selected_numeric2])
    updateSelectInput(session = session, "prim_correlation_variables", correlation_label,correlation_choice, c(correlation_choice[7],correlation_choice[8],correlation_choice[9]))
    updateSelectInput(session = session, "prim_group_by_var", group_by_label,group_by_choice, group_by_choice[pre_selected_factor])
    updateSelectInput(session = session, "summarize_aggregate_prim", summarize_aggregrate_label,summarize_aggregrate_choice[summarize_aggregrate_choice %in% numeric_choice])
    updateSelectizeInput(session = session,"all_primary_indicators", all_indicators_label,c("Select multiple" = "",all_indicators_choice),  selected = all_indicators_selected)
    updateSelectInput(session = session, "prim_numeric_label_maps", numeric_maps_label,c("Dist of MNH conducted",numeric_maps_choice), selected = numeric_maps_choice[7])
    updateSelectInput(session = session, "prim_factor_label_analysis_chart", factor_label,factor_choice, factor_choice[pre_selected_factor])
  })

  
  
  
  # Bookmarking logic ----
  vals <- reactiveValues(savedTime = NULL)
  output$lastSaved <- renderText({
    if(!is.null(vals$savedTime))
      paste("Last saved at", vals$savedTime)
    else
      ""
  })
  onBookmark(function(state) {
    vals$savedTime <- Sys.time()
    state$values$time <- vals$savedTime
  })
  onRestore(function(state){
    vals$savedTime <- state$values$time
  })
  output$myprint <- renderPrint({
    print(paste("Items to bookmark are below "))
    print(setdiff(allObjs,bookmarkToOmit()))
  })
  bookmarkToOmit <- reactive({
    if(input$includeExcluedBk == "Include to bookmark") {
      setdiff(x = allObjs, y = input$objBookmarks)
    } else if(input$includeExcluedBk == "Exclude from bookmark") {
      input$objBookmarks
    }
  })

  
  observeEvent({input$bookmarkType},{
    setBookmarkExclude(names = bookmarkToOmit())
    enableBookmarking(input$bookmarkType)
  })

  
  
enableBookmarking("server")

})