# library ----
library(tidyverse)
library(sf)
library(jpndistrict)
library(leaflet)
library(shiny)
library(shinydashboard)


# load dataset ----
reg.codes.df <- read_csv("input/35region-codes.csv",
                         locale = locale(encoding = "cp932")) %>% 
    mutate(code = as.character(code)) %>%
    rename(REGION_CLASS = CLASS) %>%
    mutate(REGION_CLASS = as.factor(REGION_CLASS))

nutrition.df <- read_csv("input/Nutrition Data of Shizuoka_v2.csv")

data.df <- reg.codes.df %>% 
    left_join(nutrition.df, by = "NUTRITION_REGION_CODE")

rm(reg.codes.df, nutrition.df)

city <- jpn_pref(22) %>% mutate(CODE = as.character(city))  # 22: Shizuoka
city[str_detect(city$city, "静岡市"), "CODE"] <- "静岡市"
city[str_detect(city$city, "浜松市"), "CODE"] <- "浜松市"
city[str_detect(city$city, "郡 "), "CODE"] <- 
    str_remove(city[str_detect(city$city, "郡 "), ]$city, pattern = " ")
city <- city[!str_detect(city$city, "浜松市"), ]

# unite Polygon, cast MULTIPOLYGON to GEOMETRY
mp.shizuoka <- sf::st_union(city[city$city == "静岡市 葵区", "geometry"],
                            city[city$city == "静岡市 駿河区", "geometry"])
mp.shizuoka <- sf::st_union(mp.shizuoka,
                            city[city$city == "静岡市 清水区", "geometry"])
# plot(mp.shizuoka)

city["geometry2"] <- city["geometry"]
city[str_detect(city$city, "静岡市"), "geometry2"] <- 
    sf::st_cast(mp.shizuoka, "GEOMETRY")

# https://stackoverflow.com/questions/61001322/how-to-change-the-active-geometry-column-in-an-sf-table-to-a-different-geometr
st_geometry(city) <- "geometry2"

plot.sf <- city %>% left_join(data.df, by = c("CODE" = "city_name"))
plot.sf <- plot.sf %>% slice(1:n())
plot.sf <- plot.sf[!duplicated(plot.sf$CODE), ]
plot.df <- plot.sf %>% as.data.frame


# ui ----
ui <- fluidPage(
    dashboardPage(
        dashboardHeader(title = 'Nutrition Map'),
        dashboardSidebar(),
        dashboardBody(
            selectInput(inputId = "nut.col", 
                        label = "Type of Nutrition",
                        choices = colnames(data.df)[8:ncol(data.df)]),
            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
            textOutput("summary"),
            leafletOutput("map", width = 800, height = 600),
        ),
    )
)


# server ----
server <- function(input, output) {
    # summary
    output$summary <- renderText({
        # https://r-spatial.github.io/sf/articles/sf4.html
        summary(plot.df[input$nut.col])
    })
    
    # map
    output$map <- renderLeaflet({
        journal.col1 <- "#495170"
        journal.col2 <- "#f7be16"
        journal.col3 <- "#fa8072"
        pal <- colorNumeric(
            palette = c(journal.col1, "#FFFFFF", journal.col3),
            domain = plot.df[, input$nut.col],
            reverse = F
        )
        
        leaflet(data = plot.sf) %>%
            addProviderTiles("CartoDB.Positron",
                             options = providerTileOptions(opacity = 0.5)) %>%
            setView(lng = 138.3262, lat = 35.11148, zoom = 9) %>%
            addPolygons(
                fillColor = ~ pal(plot.df[, input$nut.col]),
                fillOpacity = 0.7,
                stroke = TRUE, color = "dimgray",
                weight = 1, dashArray = 4, opacity = 0.2,
                popup = paste(plot.df[, "city"], ": ", 
                              plot.df[, input$nut.col])
            ) %>% 
            addLegend(
                position = "topleft",
                pal = pal, values = plot.df[, input$nut.col], 
                # bins = 20, 
                opacity = 0.8,
                labFormat = labelFormat(
                    transform = function(x) sort(x, decreasing = F),
                    digits = 1
                ),
                title = ""
            )
    })
}


# shinyApp ----
shinyApp(ui = ui, server = server)


# library(rsconnect)
# rsconnect::deployApp("../Nutrition_map")