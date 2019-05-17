# ui

shinyUI(fluidPage(

  # Application title
  titlePanel("Kaggle Russian housing price \n feature exploaration"),
  
  sidebarLayout(
      position = 'right',
      
      
      sidebarPanel(
          
          ########################
          # price vs micro and macro features
          # Sidebar Panel1 : integer
          selectInput('column1',
                      'Choose a integer column : ',
                      choices = features.numeric,
                      selected = 'full_sq'),
          
          # Sidebar Panel2 : categorical
          selectInput('column2',
                      'Choose a categorical column : ',
                      choices = features.factor),
          
          # Macro data analysis
          selectInput('column7',
                      'Choose a macro features for macro analysis : ',
                      choices = colnames(macro.features)),
          
          ########################
          # Interaction analysis
          # Sidebar Panel3 : column3
          selectInput('column3',
                      'Choose a column1 for interaction : ',
                      choices = colnames(features)),
          # Sidebar Panel4 : column4
          selectInput('column4',
                      'Choose a column2 for interaction : ',
                      choices = colnames(features),
                      selected = 'full_sq'),
          
          ########################
          # Imputation analysis micro and macro
          # Sidebar Panel5 : column5
          selectInput('column5',
                      'Choose a numeric column for imputation analysis : ',
                      choices = features.na.numeric),
          
          # Sidebar Panel6 : column6
          selectInput('column6',
                      'Choose a categorical column for imputation analysis : ',
                      choices = features.na.factor),
          
          # Sidebar Panel7 : column8
          selectInput('column8',
                      'Choose a macro column for imputation analysis : ',
                      choices = macro.colnames.na)
          ),
      
      # Main Panel
      mainPanel(
          p("feature vs target plot", style = "font-size: 16pt"),
          tabsetPanel(type = 'tabs',
          tabPanel('plot1', plotlyOutput('plot1')),
          tabPanel('plot2', plotlyOutput('plot2')),
          tabPanel('plot6', plotlyOutput('plot6')),
          tabPanel('plot3', plotlyOutput('plot3')),
          tabPanel('plot4', plotlyOutput('plot4')),
          tabPanel('plot5', plotlyOutput('plot5')),
          tabPanel('plot7', plotlyOutput('plot7')),
          tabPanel('plot8', plotlyOutput('plot8'))))
    )
))
