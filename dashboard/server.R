shinyServer(function(input, output, session, data) {
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

  
  
})