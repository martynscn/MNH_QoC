# Later change to use yaml
server_version <- "no" #Enter "yes" or "no"

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

# shiny server function ----
shinyServer(function(input, output, session,data) {
  reactiveDataSetExpr <- reactive({
    factor2 <- as.data.frame(table(primData[,input$prim_factor_label2]))
    names(factor2) <- c(input$prim_factor_label2, "Frequency")
    # factor2_arranged <-  arrange(factor2, desc(Frequency))
    data.frame(factor2[!(factor2[,1] == ""),], row.names = NULL)
  })
  
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
  
  output$BaseR_barchart <- renderPlot({
    dd <- primData[,c(input$prim_factor_label, input$prim_numeric_label)] 
    dd <- dd[!(dd[,1] == ""),]
    ddaggr <- aggregate(dd[,2] ~ dd[,1], dd, sum)
    barplot(height = ddaggr[,2], names.arg = ddaggr[,1], col = "blue")
  })
  
  output$scatterChart <- renderHighchart({
    #This looks not accurate that is miny or maxy
    miny <- min(primData[,input$prim_numeric_label2], na.rm = TRUE)
    maxy <- max(primData[,input$prim_numeric_label2], na.rm = TRUE)
    range = maxy - miny
    data_series_list <- lapply(c(1:nrow(primData)), function(x){
      vect <- c(list(primData[x,input$prim_numeric_label]), list(primData[x,input$prim_numeric_label2]))
      vect
    })
    scatterChart_plot <- highchart() %>% 
      hc_chart(type = "scatter") %>% 
      hc_title(text = "Scatter plot") %>% 
      hc_add_series(data = data_series_list, type = "scatter", color = "blue", name = paste0(input$prim_numeric_label,"(x) Vs ",input$prim_numeric_label2, "(y)")) %>% 
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
    if(input$stacked != FALSE) {
      scatterChart_plot <- scatterChart_plot %>% 
        hc_plotOptions(series = list(stacking = input$stacked))
    }
    if(input$theme != FALSE) {
      theme <- switch(input$theme,
        null = hc_theme_null(),
        darkunica = hc_theme_darkunica(),
        gridlight = hc_theme_gridlight(),
        sandsignika = hc_theme_sandsignika(),
        fivethirtyeight = hc_theme_538(),
        economist = hc_theme_economist(),
        chalk = hc_theme_chalk(),
        handdrwran = hc_theme_handdrawn()
      )
      scatterChart_plot <- scatterChart_plot %>% hc_add_theme(theme)  
    }
    scatterChart_plot
  })
  
  # abstract out colour blue and chart type e.g.scatter,line,bar
  output$hcontainer <- renderHighchart({
    data_series_list <- lapply(c(1:nrow(primData)), function(x){
      vect <- c(list(primData[x,input$prim_numeric_label]), list(primData[x,input$prim_numeric_label2]))
      vect
    })
    hcontainer_chart <- highchart() %>% 
      hc_chart(type = "scatter") %>% #scatter
      hc_title(text = "Scatter chart") %>% 
      hc_add_series(data = data_series_list, type = "scatter", color = "blue", name = paste0(input$prim_numeric_label, "(x) Vs ", input$prim_numeric_label2, "(y)"))
    if(input$stacked != FALSE) {
      hcontainer_chart <- hcontainer_chart %>% 
        hc_plotOptions(series = list(stacking = input$stacked))
    }
    if (input$theme != FALSE) {
      theme <- switch(input$theme,
                      null = hc_theme_null(),
                      darkunica = hc_theme_darkunica(),
                      gridlight = hc_theme_gridlight(),
                      sandsignika = hc_theme_sandsignika(),
                      fivethirtyeight = hc_theme_538(),
                      economist = hc_theme_economist(),
                      chalk = hc_theme_chalk(),
                      handdrwran = hc_theme_handdrawn())
      hcontainer_chart <- hcontainer_chart %>% hc_add_theme(theme)
    }
    hcontainer_chart
  })
  
  output$primsummaryDataTable <- shiny::renderDataTable({
    dt <- reactiveDataSetExpr()
  }, searchDelay = 2000, options = list(pageLength = 10))
  
  output$manualDTMaker <- shiny::renderDataTable(expr = {
    summarize_sym <- rlang::sym(input$prim_factor_label)
    summarize_aggregate_sym <- rlang::sym(input$summarize_aggregate_prim)
    selected_Inds <- input$all_primary_indicators
    mydt <- primData[,c(prim_all_label[1],selected_Inds)]
    if(input$aggr_data_quest == "Yes") {
      mydt <- mydt %>% group_by(!!summarize_sym)
      mydt <- summarise(mydt, Aggregate = do.call(what = paste0(input$aggr_fn), args = list(!!summarize_aggregate_sym, na.rm = TRUE)))} #use updateselectinput
    mydt
    }, searchDelay = 2000, options = list(height = 99999, pageLength = 25, initComplet = I("function(settings, json) {alert('Table updated');}")))
  
  observe({
    x <- input$all_primary_indicators
    # can use character(0) to remove all choices
    if(is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    y <- x[x %in% prim_numeric_label]
    updateSelectInput(session,"summarize_aggregate_prim","Numeric Variable to summarize with", y,tail(y,1))
  })
  
  output$IndicatorBarChart2 <- renderHighchart({
    my_sym <- rlang::sym(input$prim_numeric_label)
    my_fac_ind <- rlang::sym(input$prim_factor_label)
    dtt2 <- primData[c(input$prim_factor_label, input$prim_numeric_label)]
    dtt2 <- dtt2[!is.na(dtt2[,1]),]
    dtt2 <- dtt2 %>% group_by(!!my_fac_ind)
    dtt2 <- summarise(dtt2, Aggregate = do.call(what = paste0(input$aggr_fn), args = list(!!my_sym)))
    if(input$sortData == "Ascending") {
      dtt2 <- arrange(dtt2, Aggr)
    } else if(input$sortData == "Descending") {
      dtt2 <- arrange(dtt2, desc(Aggr))
    }
    highchart() %>% hc_xAxis(categories = dtt2[[1]]) %>% hc_add_series(dtt2[[2]], type = input$type)
  })
})