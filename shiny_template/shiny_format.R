# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   ui + server format
#   https://mastering-shiny.org/basic-app.html
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(shiny)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   ui: how your app should look
#   https://mastering-shiny.org/basic-ui.html#inputs
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   titlePanel; mainPanel; tabsetPanel; tabPanel; 
#   sidebarLayout; sidebarPanel; 
#   selectInput; radioButtons; checkboxGroupInput;
#   numericInput; sliderInput; 
#   textInput; passwordInput; textAreaInput; 
#   dateInput; dateRangeInput; 
#   fileInput; 
#   actionButton; actionLink; 
#   textOutput; verbatimTextOutput; 
#   tableOutput; dataTableOutput; 
#   plotOutput; plotlyOutput;
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ui <- fluidPage(
    selectInput(inputId = "dataset", 
                label = "Dataset", 
                choices = ls("package:datasets")),
    verbatimTextOutput("summary"),
    tableOutput("table")
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   server: how it should behave
#   https://mastering-shiny.org/basic-reactivity.html#the-server-function
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   renderText; renderPrint; 
#   renderTable; renderDataTable;
#   renderPlot; renderPlotly;
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

server <- function(input, output, session) {
    # reactive expression: reactive({...})
    # only runs the first time it is called and 
    # then caches its result until it needs to be updated.
    # https://mastering-shiny.org/basic-app.html#reactive-expr
    dataset <- reactive({
        get(input$dataset, "package:datasets")
    })
    
    output$summary <- renderPrint({
        summary(dataset())  # reactive returns function-like and callable object
    })
    
    output$table <- renderTable({
        dataset()  # reactive returns function-like and callable object
    })
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   ui + server
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shinyApp(ui, server)
