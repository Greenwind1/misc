# server

shinyServer(function(input, output) {

    # plotter of micro integer features
    output$plot1 <- renderPlotly({
        rc1 <- reactive({input$column1})
        ggplot(data = train.data,
               mapping = aes_string(x = rc1(), y = 'price_doc')) + 
            geom_point(alpha = 0.2,
                       color = brocolors('crayons')['Razzmatazz'],
                       size = 0.4) +
            annotate(geom = 'text',
                     x = max(train.data[[rc1()]], na.rm = TRUE)/2,
                     y = 7e7,
                     label = paste0('Pearson Correlation : ',
                                      as.character(round(cor(train.data$price_doc,
                                              train.data[rc1()],
                                              use = 'complete.obs'),
                                              digits = 3)))
                     )
        })
  
    # plotter of micro categorical features
    output$plot2 <- renderPlotly({
        rc2 <- reactive({input$column2})
        ggplot(data = train.data,
               mapping = aes_string(x = rc2(), y = 'price_doc')) + 
            geom_boxplot(mapping = aes(),
                         color = brocolors('crayons')['Yellow Orange'],
                         outlier.fill = brocolors('crayons')['Shadow'],
                         outlier.color = brocolors('crayons')['Shadow'],
                         outlier.size = 0.1,
                         outlier.alpha = 0.2)
    })
    
    # plotter of macro features
    output$plot6 <- renderPlotly({
        rc7 <- reactive({input$column7})
        train.data.macro.median <- train.data.macro %>% 
            select(year_stamp, month_stamp, one_of(rc7()), price_doc) %>% 
            dplyr::group_by(year_stamp, month_stamp) %>% 
            summarise_each(funs = funs(median), one_of(rc7()), price_doc)
        an.x <- max(train.data.macro.median[[rc7()]], na.rm = TRUE)
        an.x <- an.x + min(train.data.macro.median[[rc7()]], na.rm = TRUE)
        ggplot(data = train.data.macro.median,
               mapping = aes_string(x = rc7(), y = 'price_doc')) + 
            geom_point(alpha = 0.6,
                       color = brocolors('crayons')['Pacific Blue'],
                       size = 0.5) +
            annotate(geom = 'text',
                     x = an.x/2 ,
                     y = 7e6,
                     label = paste0('Pearson Correlation : ',
                                    as.character(round(cor(train.data.macro.median$price_doc,
                                                           train.data.macro.median[rc7()],
                                                           use = 'complete.obs'),
                                                       digits = 3)))
            )
    })
    
    # plotter of categorical features
    output$plot3 <- renderPlotly({
        rc3 <- reactive({input$column3})
        rc4 <- reactive({input$column4})
        ggplot(data = train.data,
               mapping = aes_string(x = rc3(), y = rc4())) + 
            geom_point(mapping = aes(size = price_doc / 20),
                        color = brocolors('crayons')['Purple Heart'],
                        fill = brocolors('crayons')['Purple Heart'],
                        alpha = 0.2) + geom_jitter(width = 0.1, height = 0.1) +
            scale_x_discrete()
    })

    output$plot4 <- renderPlotly({
        rc5 <- reactive({input$column5})
        ggplot(data = train.data,
               mapping = aes_string(x = rc5())) + 
            geom_histogram(
                bins = 100,    
                color = brocolors('crayons')['Indigo'],
                fill = brocolors('crayons')['Indigo'])
    })
    
    output$plot5 <- renderPlotly({
        rc6 <- reactive({input$column6})
        ggplot(data = train.data,
               mapping = aes_string(x = rc6())) + 
            geom_bar(
                color = brocolors('crayons')['Indigo'],
                fill = brocolors('crayons')['Indigo'])
    })
    
    output$plot7 <- renderPlotly({
        rc8 <- reactive({input$column8})
        ggplot(data = macro.data.201108,
               mapping = aes_string(x = rc8())) + 
            geom_histogram(
                bins = 100,    
                color = brocolors('crayons')['Pacific Blue'],
                fill = brocolors('crayons')['Pacific Blue'])
    })
    
    output$plot8 <- renderPlotly({
        rc8 <- reactive({input$column8})
        ggplot(data = macro.data.201108,
               mapping = aes_string(x = 'timestamp', y = rc8())) + 
            geom_point(
                alpha = 0.5,
                size = 0.3,
                color = brocolors('crayons')['Pacific Blue'])
    })
})

